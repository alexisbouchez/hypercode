#!/usr/bin/env bash
#
# Downloads the Gleam WASM compiler and bundles stdlib assets
# for browser-based Gleam code execution.
#
# These files go into public/gleam/ and are served statically.
#
# Requires: curl, tar, gleam

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
OUTPUT_DIR="$PROJECT_DIR/public/gleam"
PRECOMPILED_DIR="$OUTPUT_DIR/precompiled"

GLEAM_VERSION="1.14.0"
TARBALL_URL="https://github.com/gleam-lang/gleam/releases/download/v${GLEAM_VERSION}/gleam-v${GLEAM_VERSION}-browser.tar.gz"

mkdir -p "$OUTPUT_DIR" "$PRECOMPILED_DIR"

# --- Step 1: Download and extract Gleam WASM compiler ---
echo "==> Downloading Gleam WASM compiler (v${GLEAM_VERSION})..."
if [ ! -f "$OUTPUT_DIR/gleam_wasm_bg.wasm" ]; then
  TMP_TAR=$(mktemp)
  curl -fL "$TARBALL_URL" -o "$TMP_TAR"
  tar xzf "$TMP_TAR" -C "$OUTPUT_DIR" gleam_wasm.js gleam_wasm_bg.wasm
  rm "$TMP_TAR"
  echo "    Extracted gleam_wasm.js and gleam_wasm_bg.wasm"
else
  echo "    gleam_wasm_bg.wasm already exists, skipping"
fi

# --- Step 2: Build stdlib sources and precompiled JS ---
echo "==> Building stdlib assets..."
TMP_PROJECT=$(mktemp -d)
trap "rm -rf $TMP_PROJECT" EXIT

cd "$TMP_PROJECT"
gleam new stdlib_builder --skip-github --skip-git > /dev/null 2>&1
cd stdlib_builder

# Build to get stdlib source and compiled JS
gleam build --target javascript > /dev/null 2>&1

# --- Step 3: Bundle stdlib Gleam source files into JSON ---
echo "==> Bundling stdlib source files..."
STDLIB_SRC="$TMP_PROJECT/stdlib_builder/build/packages/gleam_stdlib/src"

# Create a JSON map of module_name -> source_code
node -e "
const fs = require('fs');
const path = require('path');
const srcDir = '$STDLIB_SRC/gleam';
const result = {};

function walk(dir, prefix) {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    if (entry.isDirectory()) {
      walk(path.join(dir, entry.name), prefix ? prefix + '/' + entry.name : entry.name);
    } else if (entry.name.endsWith('.gleam')) {
      const modName = (prefix ? prefix + '/' : '') + entry.name.replace('.gleam', '');
      result['gleam/' + modName] = fs.readFileSync(path.join(dir, entry.name), 'utf-8');
    }
  }
}

walk(srcDir, '');
console.log(JSON.stringify(result));
" > "$OUTPUT_DIR/stdlib-sources.json"

echo "    Bundled $(node -e "console.log(Object.keys(JSON.parse(require('fs').readFileSync('$OUTPUT_DIR/stdlib-sources.json','utf-8'))).length)") modules"

# --- Step 4: Copy precompiled stdlib JS files ---
echo "==> Copying precompiled JS files..."
COMPILED_DIR="$TMP_PROJECT/stdlib_builder/build/dev/javascript"

# Copy gleam_stdlib FFI and root-level compiled modules (dict.mjs, etc.)
# NOTE: this must run BEFORE the prelude copy because gleam_stdlib/gleam.mjs
# is a re-export stub that would overwrite the real prelude content.
for f in "$COMPILED_DIR/gleam_stdlib/"*.mjs; do
  [ -f "$f" ] && cp "$f" "$PRECOMPILED_DIR/"
done

# Copy prelude — gleam_stdlib/gleam.mjs re-exports from ../prelude.mjs,
# so we need prelude.mjs at the parent level (public/gleam/) as well.
cp "$COMPILED_DIR/prelude.mjs" "$OUTPUT_DIR/prelude.mjs"

# Copy compiled stdlib modules
mkdir -p "$PRECOMPILED_DIR/gleam"
if [ -d "$COMPILED_DIR/gleam_stdlib/gleam" ]; then
  cp -r "$COMPILED_DIR/gleam_stdlib/gleam/"*.mjs "$PRECOMPILED_DIR/gleam/" 2>/dev/null || true
  # Copy subdirectories (e.g. gleam/dynamic/)
  for subdir in "$COMPILED_DIR/gleam_stdlib/gleam"/*/; do
    if [ -d "$subdir" ]; then
      dirname=$(basename "$subdir")
      mkdir -p "$PRECOMPILED_DIR/gleam/$dirname"
      cp "$subdir"*.mjs "$PRECOMPILED_DIR/gleam/$dirname/" 2>/dev/null || true
    fi
  done
fi

# Also copy the prelude as gleam_prelude.mjs (the WASM compiler references this name)
cp "$COMPILED_DIR/prelude.mjs" "$PRECOMPILED_DIR/gleam_prelude.mjs"

echo "==> Done! Gleam WASM files are in $OUTPUT_DIR"
ls -lh "$OUTPUT_DIR"
echo ""
echo "Precompiled files:"
find "$PRECOMPILED_DIR" -name "*.mjs" | head -20
