#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Setting up wizer pre-initialization for GHC RTS..."

# 1. Update stubs.c to include wizer.initialize export
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

// RTS initialization wrapper
extern void hs_init(int *argc, char **argv[]);
void init_rts(void) {
    int argc = 1;
    char arg0[] = "wasm-reactor";
    char *argv[] = { arg0, (char *)0 };
    hs_init(&argc, argv);
}

// Wizer initialization entry point
__attribute__((export_name("wizer.initialize"))) void wizer_initialize(void) {
    init_rts();
}
EOF

# 2. Simplify Main.hs (remove manual init_rts call)
cat << 'EOF' > "$DIR/frontend-wasm/Main.hs"
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import System.IO (hFlush, stdout)

foreign export ccall reactor_start :: IO ()
foreign export ccall reactor_stop  :: IO ()

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

echo "[HRSM] stubs.c and Main.hs updated successfully."
echo "Next steps:"
echo "  1. git add frontend-wasm/stubs.c frontend-wasm/Main.hs"
echo "  2. nix build .#frontend-wasm"
echo "  3. nix shell nixpkgs#wizer -c wizer --allow-wasi ./result/reactor.wasm -o reactor-initialized.wasm"
echo "  4. nix shell nixpkgs#wasmtime -c wasmtime run --invoke reactor_start ./reactor-initialized.wasm"
