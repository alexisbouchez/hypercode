#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
WASM_DIR="$PROJECT_DIR/wasm"
PUBLIC_DIR="$PROJECT_DIR/public"

echo "Building Yaegi WASM module..."

# Initialize Go module if needed
cd "$WASM_DIR"
if [ ! -f go.mod ]; then
    go mod init hypercode/wasm
    go get github.com/traefik/yaegi@latest
fi

# Ensure dependencies are downloaded
go mod tidy

# Build WASM
echo "Compiling to WebAssembly..."
GOOS=js GOARCH=wasm go build -o "$PUBLIC_DIR/yaegi.wasm" .

# Copy wasm_exec.js from Go installation
GOROOT=$(go env GOROOT)
if [ -f "$GOROOT/lib/wasm/wasm_exec.js" ]; then
    cp "$GOROOT/lib/wasm/wasm_exec.js" "$PUBLIC_DIR/wasm_exec.js"
elif [ -f "$GOROOT/misc/wasm/wasm_exec.js" ]; then
    cp "$GOROOT/misc/wasm/wasm_exec.js" "$PUBLIC_DIR/wasm_exec.js"
else
    echo "Warning: wasm_exec.js not found in GOROOT ($GOROOT)"
    echo "You may need to copy it manually."
    exit 1
fi

echo "Build complete!"
echo "  WASM: $PUBLIC_DIR/yaegi.wasm"
echo "  JS:   $PUBLIC_DIR/wasm_exec.js"

# Print file sizes
ls -lh "$PUBLIC_DIR/yaegi.wasm" "$PUBLIC_DIR/wasm_exec.js"
