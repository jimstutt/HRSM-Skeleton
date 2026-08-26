#!/usr/bin/env bash
set -euo pipefail
D="/home/jimstutt/Dev/HRSM-Skeleton"
cat > "$D/scripts/wasm-iserv.sh" << 'IE'
#!/usr/bin/env bash
W=$(find /nix/store -name "ghc-iserv.wasm" -path "*/lib/bin/*" 2>/dev/null | head -n 1)
[ -z "$W" ] && { echo "Error: ghc-iserv.wasm not found" >&2; exit 1; }
exec wasmtime run "$W" "$@"
IE
chmod +x "$D/scripts/wasm-iserv.sh"

cat > "$D/clean-wasm-build-fixed.sh" << 'BE'
#!/usr/bin/env bash
set -euo pipefail
nix develop --command bash -c '
D="/home/jimstutt/Dev/HRSM-Skeleton"
wasm32-wasi-ghc --version
cat > "$D/frontend-wasm/cabal.project" << PE
packages:
  .
package *
  ghc-options: -fexternal-interpreter -pgmi $D/scripts/wasm-iserv.sh
PE
mkdir -p "$D/dist-wasm"
wasm32-wasi-clang -c "$D/frontend-wasm/stubs.c" -o "$D/dist-wasm/stubs.o"
export HOME="$D" CABAL_DIR="$D/.cabal"
rm -rf "$HOME/.config/cabal" "$D/dist-newstyle/cache/plan.json" "$D/frontend-wasm/dist-newstyle"
unset GHC_PACKAGE_PATH
[ -f "$D/cabal.project" ] && mv "$D/cabal.project" "$D/cabal.project.bak"
wasm32-wasi-cabal update
cd "$D/frontend-wasm"
wasm32-wasi-cabal build frontend-wasm-exe
cd "$D"
[ -f "$D/cabal.project.bak" ] && mv "$D/cabal.project.bak" "$D/cabal.project"
O=$(find "$D/dist-newstyle" -type f -name "Main.o" | grep "frontend-wasm" | head -n 1)
[ -z "$O" ] && { echo "Error: Main.o not found"; exit 1; }
wasm32-wasi-ghc -O2 -no-hs-main -optl-mexec-model=reactor -optl-Wl,--allow-undefined -optl-Wl,--export=start_reactor -optl-Wl,--export=reactor_stop -optl-Wl,--export-all "$O" "$D/dist-wasm/stubs.o" -o "$D/dist-wasm/reactor.wasm"
echo "[HRSM] Done: $D/dist-wasm/reactor.wasm"
'
BE
chmod +x "$D/clean-wasm-build-fixed.sh"
echo "[HRSM] Ready. Run: ~/Dev/HRSM-Skeleton/clean-wasm-build-fixed.sh"
