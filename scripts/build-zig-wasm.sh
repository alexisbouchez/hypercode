#!/usr/bin/env bash
#
# Downloads or builds the Zig compiler WASM binary and standard library archive
# for browser-based Zig code execution.
#
# These files go into public/zig/ and are served statically.
#
# Requires: curl, tar
# Optional: zig (for building from source instead of downloading)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
OUTPUT_DIR="$PROJECT_DIR/public/zig"

ZIG_VERSION="0.15.1"
DOWNLOAD_BASE="https://playground.zigtools.org/assets"
ZIG_WASM_FILE="zig-yHkVAafI.wasm"
ZIG_STDLIB_FILE="zig.tar-MBl2uXhS.gz"

mkdir -p "$OUTPUT_DIR"

echo "==> Downloading Zig WASM compiler (zig.wasm)..."
if [ ! -f "$OUTPUT_DIR/zig.wasm" ]; then
  curl -fL "$DOWNLOAD_BASE/$ZIG_WASM_FILE" -o "$OUTPUT_DIR/zig.wasm"
  echo "    Downloaded zig.wasm"
else
  echo "    zig.wasm already exists, skipping"
fi

echo "==> Downloading Zig standard library archive (zig-stdlib.tar.gz)..."
if [ ! -f "$OUTPUT_DIR/zig-stdlib.tar.gz" ]; then
  curl -fL "$DOWNLOAD_BASE/$ZIG_STDLIB_FILE" -o "$OUTPUT_DIR/zig-stdlib.tar.gz"
  echo "    Downloaded zig-stdlib.tar.gz"
else
  echo "    zig-stdlib.tar.gz already exists, skipping"
fi

echo "==> Done! Zig WASM files are in $OUTPUT_DIR"
ls -lh "$OUTPUT_DIR"
