#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing hs_init argument types in C wrapper..."

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

// Forward declarations
extern void hs_init(int *argc, char **argv[]);
extern void reactor_start(void);

// Wrapper that initializes RTS and starts the reactor
void start_reactor(void) {
    static int argc = 1;
    static char arg0[] = "wasm-reactor";
    
    // Correctly structure argv for hs_init(int*, char**[])
    // hs_init expects a char***, so we need a char** that points to the array
    static char *argv_arr[] = { arg0, NULL };
    static char **argv = argv_arr;
    
    hs_init(&argc, &argv);
    reactor_start();
}
EOF

echo "[HRSM] stubs.c updated successfully."
echo "Next steps:"
echo "  1. git add frontend-wasm/stubs.c"
echo "  2. nix build .#frontend-wasm"
echo "  3. nix shell nixpkgs#wasmtime -c wasmtime run --invoke start_reactor ./result/reactor.wasm"
