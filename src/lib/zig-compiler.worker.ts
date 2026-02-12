/* eslint-disable no-restricted-globals */

import { WASI, PreopenDirectory, OpenFile, File, type Fd, type Inode, Directory } from "@bjorn3/browser_wasi_shim";
import { untar } from "@andrewbranch/untar.js";

type TreeNode = Map<string, TreeNode | Uint8Array>;

function convertTree(node: TreeNode): Directory {
  return new Directory(
    [...node.entries()].map(([key, value]) => {
      if (value instanceof Uint8Array) {
        return [key, new File(value)] as [string, Inode];
      }
      return [key, convertTree(value)] as [string, Inode];
    }),
  );
}

let zigWasmModule: WebAssembly.Module | null = null;
let libDirectory: Directory | null = null;

self.addEventListener("message", async (e: MessageEvent) => {
  const { type } = e.data;

  if (type === "init") {
    try {
      const [wasmResponse, tarResponse] = await Promise.all([
        fetch("/zig/zig.wasm"),
        fetch("/zig/zig-stdlib.tar.gz"),
      ]);

      if (!wasmResponse.ok) {
        throw new Error(`Failed to load zig.wasm: ${wasmResponse.status}`);
      }
      if (!tarResponse.ok) {
        throw new Error(`Failed to load zig-stdlib.tar.gz: ${tarResponse.status}`);
      }

      // Compile WASM module (can be instantiated multiple times)
      zigWasmModule = await WebAssembly.compileStreaming(wasmResponse);

      // Parse the standard library tarball into a virtual directory tree
      let arrayBuffer = await tarResponse.arrayBuffer();
      const magicNumber = new Uint8Array(arrayBuffer).slice(0, 2);
      if (magicNumber[0] === 0x1f && magicNumber[1] === 0x8b) {
        const ds = new DecompressionStream("gzip");
        const decompressed = new Response(
          new Response(arrayBuffer).body!.pipeThrough(ds),
        );
        arrayBuffer = await decompressed.arrayBuffer();
      }

      const entries = untar(arrayBuffer);
      const root: TreeNode = new Map();

      for (const entry of entries) {
        if (!entry.filename.startsWith("lib/")) continue;
        const path = entry.filename.slice("lib/".length);
        const segments = path.split("/");

        let current = root;
        for (const seg of segments.slice(0, -1)) {
          if (!current.has(seg)) {
            current.set(seg, new Map());
          }
          current = current.get(seg) as TreeNode;
        }
        current.set(segments[segments.length - 1], entry.fileData);
      }

      libDirectory = convertTree(root);

      self.postMessage({ type: "ready" });
    } catch (err) {
      self.postMessage({
        type: "error",
        error: err instanceof Error ? err.message : String(err),
      });
    }
  } else if (type === "compile") {
    try {
      if (!zigWasmModule || !libDirectory) {
        throw new Error("Compiler not initialized");
      }

      const { code } = e.data;
      let compileErrors = "";

      const cwd = new PreopenDirectory(
        ".",
        new Map<string, Inode>([
          ["main.zig", new File(new TextEncoder().encode(code))],
        ]),
      );

      const stderrFd = new OpenFile(new File([]));

      const fds: Fd[] = [
        new OpenFile(new File([])), // stdin
        stderrFd,                   // stdout (zig writes errors here too)
        stderrFd,                   // stderr
        cwd,                        // preopened "."
        new PreopenDirectory("/lib", libDirectory.contents), // std lib
        new PreopenDirectory("/cache", new Map()),           // cache dir
      ];

      const args = [
        "zig.wasm",
        "build-exe",
        "main.zig",
        "-fno-llvm",
        "-fno-lld",
        "-fno-ubsan-rt",
        "-fno-entry",
      ];

      const wasi = new WASI(args, [], fds, { debug: false });

      const instance = await WebAssembly.instantiate(zigWasmModule, {
        wasi_snapshot_preview1: wasi.wasiImport,
      });

      // Capture stderr before running (for error reporting)
      const stderrCollector = { text: "" };
      const origWrite = stderrFd.fd_write;
      stderrFd.fd_write = function (data: Uint8Array) {
        stderrCollector.text += new TextDecoder().decode(data);
        return origWrite.call(this, data);
      };

      try {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        const exitCode = (wasi as any).start(instance);

        if (exitCode === 0) {
          const mainWasm = cwd.dir.contents.get("main.wasm") as File | undefined;
          if (mainWasm) {
            const wasmBinary = mainWasm.data.slice();
            self.postMessage(
              { type: "compiled", wasm: wasmBinary.buffer },
              { transfer: [wasmBinary.buffer] },
            );
          } else {
            throw new Error("Compilation produced no output");
          }
        } else {
          compileErrors = stderrCollector.text || `Compilation failed with exit code ${exitCode}`;
          throw new Error(compileErrors);
        }
      } catch (err) {
        if (err instanceof Error && err.message === compileErrors && compileErrors) {
          throw err;
        }
        const errMsg = stderrCollector.text || (err instanceof Error ? err.message : String(err));
        throw new Error(errMsg);
      }
    } catch (err) {
      self.postMessage({
        type: "error",
        error: err instanceof Error ? err.message : String(err),
      });
    }
  }
});
