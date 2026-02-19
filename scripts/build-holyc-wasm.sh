#!/usr/bin/env bash
set -euo pipefail

# Build Aiwnios HolyC compiler to WebAssembly using Emscripten.
#
# Requirements:
#   - git
#   - emcc / emcmake / emmake  (Emscripten SDK, sourced from emsdk_env.sh)
#   - cmake
#
# Usage:
#   ./scripts/build-holyc-wasm.sh
#
# Output:
#   public/holyc/aiwnios.js
#   public/holyc/aiwnios.wasm

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUTPUT_DIR="$PROJECT_DIR/public/holyc"
TMP_DIR="/tmp/aiwnios-build-$$"

echo "==> Checking dependencies..."
command -v git  >/dev/null || { echo "ERROR: git not found"; exit 1; }
command -v emcc >/dev/null || { echo "ERROR: emcc not found. Source the Emscripten SDK: source emsdk_env.sh"; exit 1; }
command -v cmake >/dev/null || { echo "ERROR: cmake not found"; exit 1; }

echo "==> Cloning Aiwnios..."
git clone --depth 1 https://github.com/Aiwnios/Aiwnios "$TMP_DIR"

echo "==> Configuring with Emscripten..."
cd "$TMP_DIR"
emcmake cmake -B build \
  -DUSE_BYTECODE=on \
  -DCMAKE_BUILD_TYPE=Release

echo "==> Building..."
emmake make -C build -j"$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)"

echo "==> Copying output to $OUTPUT_DIR..."
mkdir -p "$OUTPUT_DIR"
cp build/aiwnios.js  "$OUTPUT_DIR/aiwnios.js"
cp build/aiwnios.wasm "$OUTPUT_DIR/aiwnios.wasm"

echo "==> Cleaning up..."
rm -rf "$TMP_DIR"

echo ""
echo "Done! Files written to $OUTPUT_DIR:"
ls -lh "$OUTPUT_DIR"
echo ""
echo "To use: start the dev server with 'npm run dev' and navigate to /courses/holyc"
