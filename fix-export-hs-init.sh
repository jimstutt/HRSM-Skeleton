#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Exporting hs_init directly for host-side initialization..."

# 1. Simplify stubs.c (remove lazy init)
cat << 'EOF' > "$DIR/frontend-wasm/stubs.c"
#include <stdint.h>
#include <stddef.h>

// Stub implementations for GHC RTS threading primitives
int32_t forkOS_createThread(void* param) { return 0; }
void setTimerManagerControlFd(int32_t fd) {}
void setIOManagerWakeupFd(int32_t fd) {}
void blockUserSignals(void) {}
void unblockUserSignals(void) {}
void osReleaseFreeMemory(void) {}
void osFreeMBlocks(void* addr, uint32_t len) {}
EOF

# 2. Update Main.hs to export hs_init
cat << 'EOF' > "$DIR/frontend-wasm/Main.hs"
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import System.IO (hFlush, stdout)

foreign export ccall reactor_start :: IO ()
foreign export ccall reactor_stop  :: IO ()

-- hs_init is already exported by GHC RTS, we just need to make sure it's in the export list

reactor_start :: IO ()
reactor_start = do
  putStrLn "[HRSM] Reactor started"
  hFlush stdout

reactor_stop :: IO ()
reactor_stop = do
  putStrLn "[HRSM] Reactor stopped"
  hFlush stdout

main :: IO ()
main = pure ()
EOF

# 3. Update build-wasm.sh to export hs_init
cat << 'EOF' > "$DIR/scripts/build-wasm.sh"
#!/usr/bin/env bash
set -euo pipefail

mkdir -p dist-wasm

echo "[1/3] Compiling C stubs..."
wasm32-wasi-clang -c frontend-wasm/stubs.c -o dist-wasm/stubs.o

echo "[2/3] Compiling and Linking Haskell with stubs → WASI reactor"
wasm32-wasi-ghc \
  -O2 \
  -no-hs-main \
  -optl-mexec-model=reactor \
  -optl-Wl,--allow-undefined \
  -optl-Wl,--export=reactor_start \
  -optl-Wl,--export=reactor_stop \
  -optl-Wl,--export=hs_init \
  -optl-Wl,--export-all \
  frontend-wasm/Main.hs \
  dist-wasm/stubs.o \
  -o dist-wasm/reactor.wasm

echo "[3/3] Done: dist-wasm/reactor.wasm"
EOF
chmod +x "$DIR/scripts/build-wasm.sh"

echo "[HRSM] Files updated successfully."
echo "Next steps:"
echo "  1. git add frontend-wasm/stubs.c frontend-wasm/Main.hs scripts/build-wasm.sh"
echo "  2. nix build .#frontend-wasm"
echo "  3. nix shell nixpkgs#wasmtime -c wasmtime run --invoke hs_init --invoke reactor_start ./result/reactor.wasm"
