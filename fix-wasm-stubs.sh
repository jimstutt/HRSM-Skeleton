#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Creating stub implementations for missing WASI threading primitives..."

# Create stubs.c with implementations for missing RTS functions
cat << 'EOF' > "$DIR/frontend-wasm/stubs.c"
#include <stdint.h>

// Stub implementations for GHC RTS threading primitives
// These are never actually called in a single-threaded WASI reactor

int32_t forkOS_createThread(void* param) {
    return 0; // Success (no-op)
}

void setTimerManagerControlFd(int32_t fd) {
    // No-op
}

void setIOManagerWakeupFd(int32_t fd) {
    // No-op
}

void blockUserSignals(void) {
    // No-op
}

void unblockUserSignals(void) {
    // No-op
}

void osReleaseFreeMemory(void) {
    // No-op
}

void osFreeMBlocks(void* addr, uint32_t len) {
    // No-op
}
EOF

# Update build-wasm.sh to compile and link the stubs
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
  -optl-Wl,--export-all \
  -optl-Wl,--export=hs_init \
  frontend-wasm/Main.hs \
  dist-wasm/stubs.o \
  -o dist-wasm/reactor.wasm

echo "[3/3] Done: dist-wasm/reactor.wasm"
EOF
chmod +x "$DIR/scripts/build-wasm.sh"

echo "[HRSM] stubs.c and build-wasm.sh updated successfully."
echo "Next steps:"
echo "  1. Rebuild: nix build .#frontend-wasm"
echo "  2. Test: nix shell nixpkgs#wasmtime -c wasmtime run --invoke reactor_start ./result/reactor.wasm"
