#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Implementing lazy RTS initialization (no wizer needed)..."

# 1. Update stubs.c with lazy initialization
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

// Lazy RTS initialization
extern void hs_init(int *argc, char **argv[]);

static int rts_initialized = 0;

void ensure_rts_initialized(void) {
    if (!rts_initialized) {
        static int argc = 1;
        static char arg0[] = "wasm-reactor";
        static char *argv[] = { arg0, NULL };
        hs_init(&argc, &argv);
        rts_initialized = 1;
    }
}
EOF

# 2. Update Main.hs to call ensure_rts_initialized
cat << 'EOF' > "$DIR/frontend-wasm/Main.hs"
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import System.IO (hFlush, stdout)

foreign export ccall reactor_start :: IO ()
foreign export ccall reactor_stop  :: IO ()

foreign import ccall "ensure_rts_initialized" ensure_rts_initialized :: IO ()

reactor_start :: IO ()
reactor_start = do
  ensure_rts_initialized
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
