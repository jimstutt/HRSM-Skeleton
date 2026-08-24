#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Creating C wrapper to handle RTS initialization and reactor start..."

# 1. Update stubs.c to include the start_reactor wrapper
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
    static char *argv[] = { arg0, NULL };
    hs_init(&argc, &argv);
    reactor_start();
}
EOF

# 2. Update build-wasm.sh to export start_reactor instead of hs_init
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
  -optl-Wl,--export=start_reactor \
  -optl-Wl,--export=reactor_stop \
  -optl-Wl,--export-all \
  frontend-wasm/Main.hs \
  dist-wasm/stubs.o \
  -o dist-wasm/reactor.wasm

echo "[3/3] Done: dist-wasm/reactor.wasm"
EOF
chmod +x "$DIR/scripts/build-wasm.sh"

echo "[HRSM] stubs.c and build-wasm.sh updated successfully."
echo "Next steps:"
echo "  1. git add frontend-wasm/stubs.c scripts/build-wasm.sh"
echo "  2. nix build .#frontend-wasm"
echo "  3. nix shell nixpkgs#wasmtime -c wasmtime run --invoke start_reactor ./result/reactor.wasm"
