#!/usr/bin/env bash
set -euo pipefail

echo "🧩 NGOLogisticsCG project cleanup & refactor started..."

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
BACKUP="$ROOT/tmp/backup"
LOG="$ROOT/tmp/cleanup.log"

mkdir -p "$BACKUP" "$ROOT/scripts" "$ROOT/examples"

log() { echo "$@" | tee -a "$LOG"; }

# -----------------------------------------------------------------------------
# 1. Backup and remove redundant files
# -----------------------------------------------------------------------------

backup_and_remove() {
  for path in "$@"; do
    if [ -e "$ROOT/$path" ]; then
      log "📦 Backing up and removing: $path"
      mkdir -p "$BACKUP/$(dirname "$path")"
      mv "$ROOT/$path" "$BACKUP/$path"
    fi
  done
}

log "==> Backing up redundant or temporary files..."
backup_and_remove \
  "NGOLCG.nix" \
  "wasm-overlay.nix~" \
  "backend-shell/flake.nix" \
  "wasm-shell/flake.nix" \
  "NGOLogisticsCG.cabal" \
  "backend/NGOLogisticsCG.cabal~" \
  "test/Main" \
  "error.txt" \
  "sha256.txt"

# -----------------------------------------------------------------------------
# 2. Move helper scripts into scripts/
# -----------------------------------------------------------------------------

log "==> Moving scripts into scripts/"
for script in \
  build-wasm.sh run-wasm.sh serve-wasm.sh \
  setenv.sh openssl.sh prune-vendor.sh mkcerts.sh
do
  if [ -e "$ROOT/$script" ]; then
    log "📂 Moving $script → scripts/"
    mv "$ROOT/$script" "$ROOT/scripts/"
  fi
done

# -----------------------------------------------------------------------------
# 3. Move hello.hs example
# -----------------------------------------------------------------------------

if [ -f "$ROOT/hello.hs" ]; then
  log "📂 Moving hello.hs → examples/"
  mv "$ROOT/hello.hs" "$ROOT/examples/"
fi

# -----------------------------------------------------------------------------
# 4. Move logistics.sql under backend/sql/
# -----------------------------------------------------------------------------

if [ -f "$ROOT/logistics.sql" ]; then
  log "📂 Moving logistics.sql → backend/sql/"
  mkdir -p "$ROOT/backend/sql"
  mv "$ROOT/logistics.sql" "$ROOT/backend/sql/"
fi

# -----------------------------------------------------------------------------
# 5. Move wasm loader files into wasm/public/
# -----------------------------------------------------------------------------

mkdir -p "$ROOT/wasm/public"
for f in wasm-index.html wasm-loader.html wasm-loader.js; do
  if [ -f "$ROOT/$f" ]; then
    log "📂 Moving $f → wasm/public/"
    mv "$ROOT/$f" "$ROOT/wasm/public/"
  fi
done

# -----------------------------------------------------------------------------
# 6. Clean up build artifacts
# -----------------------------------------------------------------------------

log "==> Cleaning build artifacts..."
rm -rf "$ROOT"/**/client-dist || true
rm -rf "$ROOT"/dist-newstyle "$ROOT"/result || true

# -----------------------------------------------------------------------------
# 7. Create .gitignore if missing
# -----------------------------------------------------------------------------

GITIGNORE="$ROOT/.gitignore"
if [ ! -f "$GITIGNORE" ]; then
  log "📄 Creating .gitignore"
  cat > "$GITIGNORE" <<'EOF'
# Haskell build
dist-newstyle/
*.hi
*.o
*.dyn_o
*.dyn_hi

# Nix
result/
.nix-*

# Node/Vite
node_modules/
wasm/node_modules/
wasm/public/hello.wasm

# Certificates and logs
certs/*.pem
*.log
tmp/
EOF
fi

# -----------------------------------------------------------------------------
# 8. Summary
# -----------------------------------------------------------------------------

log "✅ Cleanup complete."
log "All removed/moved files are backed up under: $BACKUP"
echo
echo "✨ Done! Please review tmp/cleanup.log to confirm all operations."
