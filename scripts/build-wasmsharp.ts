#!/usr/bin/env bun
/**
 * Prepares @wasmsharp/core for use as a static asset in public/wasmsharp/.
 *
 * @wasmsharp/core is designed for Vite (which auto-handles binary file imports
 * as URL-returning assets). Next.js/webpack cannot bundle it. This script:
 *
 * 1. Copies all WasmSharp files to public/wasmsharp/
 * 2. Patches dotnet.js: replaces static binary ESM imports with const URL strings
 * 3. Patches WasmCompiler.js: replaces the Comlink CDN import with a local path
 * 4. Copies comlink.mjs to public/wasmsharp/comlink.mjs
 *
 * After this, csharp-runner.ts uses dynamic import('/wasmsharp/index.js') to
 * load WasmSharp via native browser ES modules, bypassing webpack entirely.
 */

import { copyFileSync, mkdirSync, readFileSync, writeFileSync, readdirSync } from "fs";
import { join } from "path";

const SRC = "node_modules/@wasmsharp/core";
const DEST = "public/wasmsharp";

mkdirSync(DEST, { recursive: true });

// Copy all files from @wasmsharp/core root
const files = readdirSync(SRC, { withFileTypes: true });
for (const f of files) {
  if (f.isFile()) {
    copyFileSync(join(SRC, f.name), join(DEST, f.name));
  }
}

// Copy comlink.mjs to public/wasmsharp/comlink.mjs
copyFileSync(
  "node_modules/comlink/dist/esm/comlink.mjs",
  join(DEST, "comlink.mjs")
);

// Patch dotnet.js: replace binary ESM imports with const URL strings
// e.g.: import Foo_dll from "./Foo.dll"  ->  const Foo_dll = "/wasmsharp/Foo.dll";
let dotnetJs = readFileSync(join(DEST, "dotnet.js"), "utf8");

// Match: import <identifier> from "./<filename>.<ext>"
// where ext is dll, dat (not .js, .ts — those stay as real imports)
dotnetJs = dotnetJs.replace(
  /^import (\w+) from "\.\/([^"]+\.(dll|dat))";$/gm,
  (_match, varName, filename) => `const ${varName} = "/wasmsharp/${filename}";`
);

// Also handle the wasm import: import dotnet_native_wasm from "./dotnet.native.wasm"
dotnetJs = dotnetJs.replace(
  /^import (\w+) from "\.\/([^"]+\.wasm)";$/gm,
  (_match, varName, filename) => `const ${varName} = "/wasmsharp/${filename}";`
);

writeFileSync(join(DEST, "dotnet.js"), dotnetJs);

// Patch WasmCompiler.js: replace Comlink CDN import with local file
let wasmCompilerJs = readFileSync(join(DEST, "WasmCompiler.js"), "utf8");
wasmCompilerJs = wasmCompilerJs.replace(
  `import * as Comlink from "https://unpkg.com/comlink/dist/esm/comlink.mjs";`,
  `import * as Comlink from "/wasmsharp/comlink.mjs";`
);
writeFileSync(join(DEST, "WasmCompiler.js"), wasmCompilerJs);

console.log("WasmSharp assets built to public/wasmsharp/");
