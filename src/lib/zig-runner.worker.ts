/* eslint-disable no-restricted-globals */

import { WASI, PreopenDirectory, OpenFile, File, type Fd } from "@bjorn3/browser_wasi_shim";

self.addEventListener("message", async (e: MessageEvent) => {
  const { type } = e.data;

  if (type === "run") {
    try {
      const wasmBytes = new Uint8Array(e.data.wasm);
      let stdout = "";
      let stderr = "";

      const decoder = new TextDecoder("utf-8", { fatal: false });

      const stdoutFile = new OpenFile(new File([]));
      const stderrFile = new OpenFile(new File([]));

      // Intercept writes to capture output
      const origStdoutWrite = stdoutFile.fd_write;
      stdoutFile.fd_write = function (data: Uint8Array) {
        stdout += decoder.decode(data, { stream: true });
        return origStdoutWrite.call(this, data);
      };

      const origStderrWrite = stderrFile.fd_write;
      stderrFile.fd_write = function (data: Uint8Array) {
        stderr += decoder.decode(data, { stream: true });
        return origStderrWrite.call(this, data);
      };

      const fds: Fd[] = [
        new OpenFile(new File([])), // stdin
        stdoutFile,                  // stdout
        stderrFile,                  // stderr
        new PreopenDirectory(".", new Map()),
      ];

      const wasi = new WASI(["main.wasm"], [], fds, { debug: false });

      const module = await WebAssembly.compile(wasmBytes);
      const instance = await WebAssembly.instantiate(module, {
        wasi_snapshot_preview1: wasi.wasiImport,
      });

      try {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (wasi as any).start(instance);
      } catch (err) {
        if (err instanceof Error && err.message.startsWith("Process exited with code")) {
          throw err;
        }
        // proc_exit(0) throws but that's normal termination
      }

      self.postMessage({ type: "result", stdout, stderr });
    } catch (err) {
      self.postMessage({
        type: "error",
        error: err instanceof Error ? err.message : String(err),
      });
    }
  }
});
