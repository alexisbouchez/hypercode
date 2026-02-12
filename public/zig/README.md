# Zig runtime assets

The Zig course expects these files in this folder:

- **zig.wasm** – Zig compiler compiled to WebAssembly (used to compile user code to WASM in the browser).
- **zig-stdlib.tar.gz** – Zig standard library tarball (required by the compiler).

Without them, the app will show “Zig runtime failed to load” and the editor will not run Zig code. You need to build or obtain these assets and place them here.
