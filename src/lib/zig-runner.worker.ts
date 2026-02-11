/* eslint-disable no-restricted-globals */

self.addEventListener("message", async (e: MessageEvent) => {
  const { type } = e.data;

  if (type === "run") {
    try {
      const wasmBytes = new Uint8Array(e.data.wasm);
      let stdout = "";
      let stderr = "";

      // Set up WASI-like imports for the compiled Zig program
      const decoder = new TextDecoder();

      const importObject: WebAssembly.Imports = {
        wasi_snapshot_preview1: {
          fd_write(fd: number, iovs: number, iovsLen: number, nwritten: number) {
            const memory = instance.exports.memory as WebAssembly.Memory;
            const view = new DataView(memory.buffer);
            let totalWritten = 0;

            for (let i = 0; i < iovsLen; i++) {
              const ptr = view.getUint32(iovs + i * 8, true);
              const len = view.getUint32(iovs + i * 8 + 4, true);
              const bytes = new Uint8Array(memory.buffer, ptr, len);
              const text = decoder.decode(bytes);

              if (fd === 1) {
                stdout += text;
              } else if (fd === 2) {
                stderr += text;
              }

              totalWritten += len;
            }

            view.setUint32(nwritten, totalWritten, true);
            return 0; // success
          },
          fd_close() { return 0; },
          fd_seek() { return 0; },
          fd_read() { return 0; },
          fd_prestat_get() { return 8; }, // EBADF
          fd_prestat_dir_name() { return 8; },
          fd_fdstat_get(fd: number, buf: number) {
            const memory = instance.exports.memory as WebAssembly.Memory;
            const view = new DataView(memory.buffer);
            // Set filetype to character device
            view.setUint8(buf, fd <= 2 ? 2 : 4);
            view.setUint16(buf + 2, 0, true);
            view.setBigUint64(buf + 8, BigInt(0), true);
            view.setBigUint64(buf + 16, BigInt(0), true);
            return 0;
          },
          environ_sizes_get(countPtr: number, sizePtr: number) {
            const memory = instance.exports.memory as WebAssembly.Memory;
            const view = new DataView(memory.buffer);
            view.setUint32(countPtr, 0, true);
            view.setUint32(sizePtr, 0, true);
            return 0;
          },
          environ_get() { return 0; },
          args_sizes_get(countPtr: number, sizePtr: number) {
            const memory = instance.exports.memory as WebAssembly.Memory;
            const view = new DataView(memory.buffer);
            view.setUint32(countPtr, 0, true);
            view.setUint32(sizePtr, 0, true);
            return 0;
          },
          args_get() { return 0; },
          proc_exit(code: number) {
            if (code !== 0) {
              throw new Error(`Process exited with code ${code}`);
            }
          },
          clock_time_get(_id: number, _precision: bigint, outPtr: number) {
            const memory = instance.exports.memory as WebAssembly.Memory;
            const view = new DataView(memory.buffer);
            view.setBigUint64(outPtr, BigInt(Date.now()) * BigInt(1_000_000), true);
            return 0;
          },
          random_get(buf: number, len: number) {
            const memory = instance.exports.memory as WebAssembly.Memory;
            const bytes = new Uint8Array(memory.buffer, buf, len);
            crypto.getRandomValues(bytes);
            return 0;
          },
          path_open() { return 44; }, // ENOSYS
          path_filestat_get() { return 44; },
          poll_oneoff() { return 44; },
          sched_yield() { return 0; },
        },
      };

      const module = await WebAssembly.compile(wasmBytes);
      const instance = await WebAssembly.instantiate(module, importObject);

      // Run _start (WASI entry point)
      const start = instance.exports._start as (() => void) | undefined;
      if (start) {
        try {
          start();
        } catch (err) {
          if (err instanceof Error && err.message.startsWith("Process exited with code")) {
            throw err;
          }
          // proc_exit(0) throws but that's normal termination
        }
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
