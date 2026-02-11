# Hypercode

[![Discord](https://img.shields.io/badge/Discord-Join-5865F2?style=flat&logo=discord&logoColor=white)](https://discord.gg/Fft3TE3UkY)

Interactive Go lessons in the browser. You edit Go code in the editor and run it; execution is handled by [Yaegi](https://github.com/traefik/yaegi) (the Go interpreter) compiled to WebAssembly.

## Clone and run

```bash
git clone https://github.com/alexisbouchez/hypercode.git
cd hypercode
```

Install dependencies (npm or bun):

```bash
npm install
# or
bun install
```

Start the dev server:

```bash
npm run dev
# or
bun dev
```

Open http://localhost:3000.

## Building the WASM module

The repo ships prebuilt `public/yaegi.wasm` and `public/wasm_exec.js`. To rebuild them you need Go and the project’s script:

```bash
./scripts/build-wasm.sh
```

This compiles the `wasm/` Go package (Yaegi + a small stdlib subset) to WebAssembly and copies `wasm_exec.js` from your Go installation. Optional: install [Binaryen](https://github.com/WebAssembly/binaryen) for `wasm-opt` to shrink the binary.

## License

[LICENSE](./LICENSE)
