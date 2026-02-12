import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let zigReady = false;
let zigLoading = false;
let zigLoadPromise: Promise<void> | null = null;

let zigWasmModule: WebAssembly.Module | null = null;
// eslint-disable-next-line @typescript-eslint/no-explicit-any
let libContents: Map<string, any> | null = null;

export function isZigReady(): boolean {
  return zigReady;
}

export function initZigRunner(): Promise<void> {
  if (zigReady) return Promise.resolve();
  if (zigLoadPromise) return zigLoadPromise;

  zigLoadPromise = (async () => {
    zigLoading = true;
    try {
      const { Directory, File: WasiFile } = await import("@bjorn3/browser_wasi_shim");
      const { untar } = await import("@andrewbranch/untar.js");

      // Load zig.wasm and stdlib in parallel
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

      zigWasmModule = await WebAssembly.compileStreaming(wasmResponse);

      // Parse stdlib tarball
      let arrayBuffer = await tarResponse.arrayBuffer();
      const magic = new Uint8Array(arrayBuffer).slice(0, 2);
      if (magic[0] === 0x1f && magic[1] === 0x8b) {
        const ds = new DecompressionStream("gzip");
        const decompressed = new Response(
          new Response(arrayBuffer).body!.pipeThrough(ds),
        );
        arrayBuffer = await decompressed.arrayBuffer();
      }

      const entries = untar(arrayBuffer);

      type TreeNode = Map<string, TreeNode | Uint8Array>;
      const root: TreeNode = new Map();

      for (const entry of entries) {
        if (!entry.filename.startsWith("lib/")) continue;
        const path = entry.filename.slice("lib/".length);
        const segments = path.split("/");

        let current = root;
        for (const seg of segments.slice(0, -1)) {
          if (!current.has(seg)) current.set(seg, new Map());
          current = current.get(seg) as TreeNode;
        }
        current.set(segments[segments.length - 1], entry.fileData);
      }

      function convertTree(node: TreeNode): InstanceType<typeof Directory> {
        return new Directory(
          [...node.entries()].map(([key, value]) => {
            if (value instanceof Uint8Array) {
              return [key, new WasiFile(value)];
            }
            return [key, convertTree(value)];
          }),
        );
      }

      const lib = convertTree(root);
      libContents = lib.contents;

      zigReady = true;
    } catch (err) {
      zigReady = false;
      zigLoadPromise = null;
      throw err;
    } finally {
      zigLoading = false;
    }
  })();

  return zigLoadPromise;
}

export function isZigLoading(): boolean {
  return zigLoading;
}

async function compileZig(code: string): Promise<Uint8Array> {
  if (!zigWasmModule || !libContents) throw new Error("Zig compiler not initialized");

  const { WASI, PreopenDirectory, OpenFile, File: WasiFile } = await import("@bjorn3/browser_wasi_shim");

  const cwd = new PreopenDirectory(
    ".",
    new Map([["main.zig", new WasiFile(new TextEncoder().encode(code))]]),
  );

  let stderrText = "";
  const decoder = new TextDecoder("utf-8", { fatal: false });

  const fds = [
    new OpenFile(new WasiFile([])),  // stdin
    new OpenFile(new WasiFile([])),  // stdout
    new OpenFile(new WasiFile([])),  // stderr
    cwd,
    new PreopenDirectory("/lib", libContents),
    new PreopenDirectory("/cache", new Map()),
  ];

  // Intercept stderr writes
  const origStdout = fds[1].fd_write;
  fds[1].fd_write = function (data: Uint8Array) {
    stderrText += decoder.decode(data, { stream: true });
    return origStdout.call(this, data);
  };
  const origStderr = fds[2].fd_write;
  fds[2].fd_write = function (data: Uint8Array) {
    stderrText += decoder.decode(data, { stream: true });
    return origStderr.call(this, data);
  };

  const wasi = new WASI(
    ["zig.wasm", "build-exe", "main.zig", "-fno-llvm", "-fno-lld", "-fno-ubsan-rt", "-fno-entry"],
    [],
    fds,
    { debug: false },
  );

  const instance = await WebAssembly.instantiate(zigWasmModule, {
    wasi_snapshot_preview1: wasi.wasiImport,
  });

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const exitCode = (wasi as any).start(instance);

  if (exitCode !== 0) {
    throw new Error(stderrText || `Compilation failed with exit code ${exitCode}`);
  }

  const mainWasm = cwd.dir.contents.get("main.wasm") as { data: Uint8Array } | undefined;
  if (!mainWasm) throw new Error(stderrText || "Compilation produced no output");

  return mainWasm.data;
}

async function executeWasm(wasmBytes: Uint8Array): Promise<{ stdout: string; stderr: string }> {
  const { WASI, PreopenDirectory, OpenFile, File: WasiFile } = await import("@bjorn3/browser_wasi_shim");

  let stdout = "";
  let stderr = "";
  const decoder = new TextDecoder("utf-8", { fatal: false });

  const stdoutFd = new OpenFile(new WasiFile([]));
  const stderrFd = new OpenFile(new WasiFile([]));

  const origStdout = stdoutFd.fd_write;
  stdoutFd.fd_write = function (data: Uint8Array) {
    stdout += decoder.decode(data, { stream: true });
    return origStdout.call(this, data);
  };
  const origStderr = stderrFd.fd_write;
  stderrFd.fd_write = function (data: Uint8Array) {
    stderr += decoder.decode(data, { stream: true });
    return origStderr.call(this, data);
  };

  const fds = [
    new OpenFile(new WasiFile([])),
    stdoutFd,
    stderrFd,
    new PreopenDirectory(".", new Map()),
  ];

  const wasi = new WASI(["main.wasm"], [], fds, { debug: false });

  const module = await WebAssembly.compile(wasmBytes as BufferSource);
  const instance = await WebAssembly.instantiate(module, {
    wasi_snapshot_preview1: wasi.wasiImport,
  });

  try {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (wasi as any).start(instance);
  } catch {
    // proc_exit(0) throws but that's normal termination
  }

  return { stdout, stderr };
}

export async function runZig(code: string): Promise<RunResult> {
  if (!zigReady) {
    return {
      stdout: "",
      stderr: "",
      error: "Zig runtime is not loaded. Please wait for it to initialize.",
    };
  }

  try {
    const wasm = await compileZig(code);
    const result = await executeWasm(wasm);
    return {
      stdout: result.stdout || result.stderr,
      stderr: "",
      error: "",
    };
  } catch (err) {
    return {
      stdout: "",
      stderr: "",
      error: err instanceof Error ? err.message : String(err),
    };
  }
}

/**
 * Extracts user-defined functions, types, and constants from Zig source.
 * Strips the std import and pub fn main.
 */
export function extractZigFunctions(code: string): string {
  let result = code;

  // Remove std import
  result = result.replace(/^\s*const\s+std\s*=\s*@import\s*\(\s*"std"\s*\)\s*;\s*\n/m, "");

  // Remove pub fn main
  const mainRegex = /pub\s+fn\s+main\s*\([^)]*\)\s*[^{]*\{/;
  const match = mainRegex.exec(result);
  if (match) {
    const startIndex = match.index;
    let braceCount = 0;
    let endIndex = startIndex;
    let foundOpen = false;

    for (let i = startIndex; i < result.length; i++) {
      if (result[i] === "{") {
        braceCount++;
        foundOpen = true;
      } else if (result[i] === "}") {
        braceCount--;
        if (foundOpen && braceCount === 0) {
          endIndex = i + 1;
          break;
        }
      }
    }

    result = result.slice(0, startIndex) + result.slice(endIndex);
  }

  return result.trim();
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
  const results: TestResult[] = [];

  for (const test of tests) {
    let codeToRun = code;

    if (test.code) {
      const funcs = extractZigFunctions(code);
      codeToRun = test.code.replace("{{FUNC}}", funcs);
    }

    const result = await runZig(codeToRun);

    const hasError = result.error !== "";
    const actual = hasError ? result.error : result.stdout;

    results.push({
      name: test.name,
      passed: !hasError && result.stdout === test.expected,
      actual,
      expected: test.expected,
    });
  }

  return results;
}
