#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
CLIENT_DIR="$ROOT/logistics-client"
OUT_DIR="$CLIENT_DIR/client-dist"
STATIC_DIR="$CLIENT_DIR/static"

mkdir -p "$OUT_DIR"

echo "=== Development: run jsaddle dev server ==="
echo "Run: cabal run logistics-client"

cat <<'EOT'
Development usage:
 1) Build and run the client dev server:
      cabal run logistics-client
    This runs jsaddle-warp on http://localhost:3000 and serves the JSM app.

 2) Open https://localhost:3000 in your browser.

EOT

echo "=== Production: build WASM bundle (ghc-wasm-meta) ==="

echo "Note: the ghc-wasm-meta invocation below is an example. Adjust according to your ghc-wasm-meta version."

cat <<'EOT'
# Example (pseudo-command) - replace with exact ghc-wasm-meta CLI for your installation
# ghc-wasm-meta build --package logistics-client --output "$OUT_DIR" --html "$STATIC_DIR/index.html"

# After build, copy static files (client-boot.js + index.html) into output dir
# cp -r "$STATIC_DIR/"* "$OUT_DIR/"

# Serve the $OUT_DIR directory over HTTPS (see README notes for mkcert/openssl commands)
EOT

echo "Build script finished. See comments above for exact ghc-wasm-meta invocation and serving instructions."
