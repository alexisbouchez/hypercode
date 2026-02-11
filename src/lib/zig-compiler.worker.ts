/* eslint-disable no-restricted-globals */

let zigCompiler: WebAssembly.Instance | null = null;
let stdTar: ArrayBuffer | null = null;

interface ZigExports {
  memory: WebAssembly.Memory;
  compile: (srcPtr: number, srcLen: number) => number;
  getOutputPtr: () => number;
  getOutputLen: () => number;
  getErrorPtr: () => number;
  getErrorLen: () => number;
  alloc: (len: number) => number;
}

self.addEventListener("message", async (e: MessageEvent) => {
  const { type } = e.data;

  if (type === "init") {
    try {
      // Load zig.wasm compiler and std library archive in parallel
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

      const wasmModule = await WebAssembly.instantiateStreaming(wasmResponse, {
        env: {
          // Minimal imports the Zig WASM compiler expects
        },
      });
      zigCompiler = wasmModule.instance;
      stdTar = await tarResponse.arrayBuffer();

      self.postMessage({ type: "ready" });
    } catch (err) {
      self.postMessage({
        type: "error",
        error: err instanceof Error ? err.message : String(err),
      });
    }
  } else if (type === "compile") {
    try {
      if (!zigCompiler) throw new Error("Compiler not initialized");

      const { code } = e.data;
      const encoder = new TextEncoder();
      const decoder = new TextDecoder();
      const src = encoder.encode(code);

      const exports = zigCompiler.exports as unknown as ZigExports;

      // Allocate memory for source and copy it in
      const srcPtr = exports.alloc(src.length);
      const mem = new Uint8Array(exports.memory.buffer);
      mem.set(src, srcPtr);

      // Compile
      const result = exports.compile(srcPtr, src.length);

      if (result !== 0) {
        // Error
        const errPtr = exports.getErrorPtr();
        const errLen = exports.getErrorLen();
        const errBytes = new Uint8Array(exports.memory.buffer, errPtr, errLen);
        const errMsg = decoder.decode(errBytes);
        throw new Error(errMsg);
      }

      // Get output WASM binary
      const outPtr = exports.getOutputPtr();
      const outLen = exports.getOutputLen();
      const wasmBinary = new Uint8Array(exports.memory.buffer, outPtr, outLen).slice();

      self.postMessage(
        { type: "compiled", wasm: wasmBinary.buffer },
        { transfer: [wasmBinary.buffer] },
      );
    } catch (err) {
      self.postMessage({
        type: "error",
        error: err instanceof Error ? err.message : String(err),
      });
    }
  }
});
