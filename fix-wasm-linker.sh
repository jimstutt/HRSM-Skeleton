#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Adding --allow-undefined to wasm linker flags..."

cat << 'SCRIPT' > "$DIR/scripts/build-wasm.sh"
#!/usr/bin/env bash
set -euo pipefail

mkdir -p dist-wasm

echo "[1/2] Compiling and Linking Haskell to WASI reactor"
wasm32-wasi-ghc \
  -O2 \
  -no-hs-main \
  -optl-mexec-model=reactor \
  -optl-Wl,--allow-undefined \
  -optl-Wl,--export=reactor_start \
  -optl-Wl,--export=reactor_stop \
  -optl-Wl,--export-all \
  -optl-Wl,--export=hs_init \
  frontend-wasm/Main.hs \
  -o dist-wasm/reactor.wasm

echo "[2/2] Done: dist-wasm/reactor.wasm"
SCRIPT
chmod +x "$DIR/scripts/build-wasm.sh"

echo "[HRSM] build-wasm.sh updated with --allow-undefined."
echo "Next step: Run 'nix build .#frontend-wasm'"
