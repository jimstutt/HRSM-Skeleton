#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing C stubs for hs_init signature and NULL..."

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

// RTS initialization wrapper
extern void hs_init(int *argc, char **argv[]);

static int argc_val = 1;
static char arg0[] = "wasm-reactor";
static char *argv_val[] = { arg0, NULL };

void init_rts(void) {
    // Pass &argv_val because hs_init expects char***
    hs_init(&argc_val, &argv_val);
}

__attribute__((export_name("wizer.initialize"))) void wizer_initialize(void) {
    init_rts();
}
EOF

echo "[HRSM] stubs.c updated successfully."
echo "Next steps:"
echo "  1. git add frontend-wasm/stubs.c"
echo "  2. nix build .#frontend-wasm"
echo "  3. nix shell nixpkgs#wizer -c wizer --allow-wasi --init-func wizer.initialize ./result/reactor.wasm -o reactor-initialized.wasm"
echo "  4. nix shell nixpkgs#wasmtime -c wasmtime run --invoke reactor_start ./reactor-initialized.wasm"
