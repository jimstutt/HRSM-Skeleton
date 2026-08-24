#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing RTS initialization with valid arguments for hs_init..."

# 1. Update stubs.c to pass valid pointers to hs_init
cat << 'EOF' > "$DIR/frontend-wasm/stubs.c"
#include <stdint.h>

// Stub implementations for GHC RTS threading primitives
int32_t forkOS_createThread(void* param) { return 0; }
void setTimerManagerControlFd(int32_t fd) {}
void setIOManagerWakeupFd(int32_t fd) {}
void blockUserSignals(void) {}
void unblockUserSignals(void) {}
void osReleaseFreeMemory(void) {}
void osFreeMBlocks(void* addr, uint32_t len) {}

// RTS initialization wrapper with valid dummy arguments
extern void hs_init(int *argc, char **argv[]);
void init_rts(void) {
    int argc = 1;
    char arg0[] = "wasm-reactor";
    char *argv[] = { arg0, (char *)0 };
    hs_init(&argc, argv);
}
EOF

# 2. Ensure Main.hs calls the wrapper
cat << 'EOF' > "$DIR/frontend-wasm/Main.hs"
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import System.IO (hFlush, stdout)

foreign export ccall reactor_start :: IO ()
foreign export ccall reactor_stop  :: IO ()

foreign import ccall "init_rts" init_rts :: IO ()

reactor_start :: IO ()
reactor_start = do
  init_rts
  putStrLn "[HRSM] Reactor started"
  hFlush stdout

reactor_stop :: IO ()
reactor_stop = do
  putStrLn "[HRSM] Reactor stopped"
  hFlush stdout

main :: IO ()
main = pure ()
EOF

echo "[HRSM] stubs.c and Main.hs updated successfully."
echo "Next steps:"
echo "  1. git add frontend-wasm/stubs.c frontend-wasm/Main.hs"
echo "  2. nix build .#frontend-wasm"
echo "  3. nix shell nixpkgs#wasmtime -c wasmtime run --invoke reactor_start ./result/reactor.wasm"
