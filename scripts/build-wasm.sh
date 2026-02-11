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

# Build WASM with size optimizations:
#   -s: strip symbol table
#   -w: strip DWARF debug info
echo "Compiling to WebAssembly..."
GOOS=js GOARCH=wasm go build -ldflags="-s -w" -trimpath -o "$PUBLIC_DIR/yaegi.wasm" .

# Optimize WASM binary with wasm-opt (Binaryen) if available
if command -v wasm-opt &> /dev/null; then
    echo "Optimizing WASM with wasm-opt..."
    wasm-opt -Oz --enable-bulk-memory -o "$PUBLIC_DIR/yaegi.wasm" "$PUBLIC_DIR/yaegi.wasm"
elif npx wasm-opt --version &> /dev/null; then
    echo "Optimizing WASM with wasm-opt (via npx)..."
    npx wasm-opt -Oz --enable-bulk-memory -o "$PUBLIC_DIR/yaegi.wasm" "$PUBLIC_DIR/yaegi.wasm"
else
    echo "Note: Install binaryen (wasm-opt) for additional size reduction."
fi

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
