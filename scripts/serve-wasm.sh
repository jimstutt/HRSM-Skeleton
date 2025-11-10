#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DIST_DIR="$ROOT_DIR/logistics-client/client-dist"
CERT_DIR="$ROOT_DIR/certs"

PORT=8443

# === 1. Ensure mkcert is installed ===
if ! command -v mkcert >/dev/null 2>&1; then
  echo "❌ mkcert not found. Install with: sudo apt install mkcert"
  exit 1
fi

# === 2. Generate local CA (if not already) ===
if [ ! -d "$HOME/.local/share/mkcert" ] && [ ! -d "$HOME/Library/Application Support/mkcert" ]; then
  echo "⚙️  Installing local CA..."
  mkcert -install
fi

# === 3. Generate dev certs ===
mkdir -p "$CERT_DIR"
if [ ! -f "$CERT_DIR/localhost.pem" ] || [ ! -f "$CERT_DIR/localhost-key.pem" ]; then
  echo "🔑 Generating TLS certs for localhost..."
  mkcert -cert-file "$CERT_DIR/localhost.pem" -key-file "$CERT_DIR/localhost-key.pem" localhost 127.0.0.1 ::1
fi

# === 4. Serve the client-dist folder ===
echo "🚀 Serving $DIST_DIR over https://localhost:$PORT"
cd "$DIST_DIR"

# Python 3.7+ supports --directory for specifying docroot
exec python3 -m http.server "$PORT" --directory "$DIST_DIR" \
  --bind 127.0.0.1 \
  --certfile "$CERT_DIR/localhost.pem" \
  --keyfile "$CERT_DIR/localhost-key.pem"

