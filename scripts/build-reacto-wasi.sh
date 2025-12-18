#!/usr/bin/env bash
set -euo pipefail

# Build script: compile C wrapper -> object, and link GHC-produced .o files (dist-reactor/*.o)
# into a single reactor.wasm using wasm-ld from the WASI SDK / lld.

# Precondition: run `nix develop` in the project root so WASI_SYSROOT and tools are available.
: "${WASI_SYSROOT:?WASI_SYSROOT must be set (run nix develop)}"

ROOT="$(cd "$(dirname "$0")/.."; pwd -P)"
BUILD_DIR="$ROOT/build"
SRC_DIR="$ROOT/src"
DIST_REACTOR="$ROOT/dist-reactor"

mkdir -p "$BUILD_DIR"

# 1) Compile the C wrapper to a wasm object
#    - Uses the WASI sysroot headers in $WASI_SYSROOT
#    - Produces build/reactor_wrapper.o
echo "Compiling C wrapper..."
clang --target=wasm32-wasi \
      --sysroot="$WASI_SYSROOT" \
      -nostdinc -I"$WASI_SYSROOT/include" \
      -O2 -c "$SRC_DIR/reactor_wrapper.c" \
      -o "$BUILD_DIR/reactor_wrapper.o" \
      -ffunction-sections -fdata-sections

# 2) Collect GHC-produced object files (adjust pattern if GHC outputs somewhere else)
#    Your GHC-wasm toolchain must put .o files into dist-reactor/ for this to work.
echo "Collecting GHC object files from: $DIST_REACTOR"
GHC_OBJS=( "$DIST_REACTOR"/*.o )
if [ ! -e "${GHC_OBJS[0]}" ]; then
  echo "Warning: no GHC object files found at $DIST_REACTOR. Build your Haskell reactor to populate *.o files."
  echo "Expected something like: $DIST_REACTOR/ReactModule.o"
  # still proceed to produce a minimal wasm containing only the wrapper if desired:
fi

# 3) Link all object files + wrapper into single .wasm
OUT_WASM="$BUILD_DIR/reactor.wasm"
echo "Linking WASM into: $OUT_WASM"

# Example wasm-ld flags:
#  --no-entry             : there is no C main; runtime will call exported functions
#  --export-all           : export all defined functions (or use --export=<name>)
#  --allow-undefined     : allow unresolved symbols (useful if GHC runtime is provided elsewhere)
#  --lto-O3 etc. (omit for simplicity)
# You may need to refine exports or supply GHC runtime object files.

wasm-ld \
  --no-entry \
  --export-all \
  --allow-undefined \
  -o "$OUT_WASM" \
  "$BUILD_DIR/reactor_wrapper.o" \
  ${GHC_OBJS[@]:-}

echo "WASM produced at: $OUT_WASM"
echo
echo "Notes / next steps:"
echo " - If you want to export a single named symbol (eg. reactor_entry), either:"
echo "    * annotate/define that symbol in src/reactor_wrapper.c and use --export=reactor_entry instead of --export-all;"
echo "    * or use wasm-opt/wasm-objdump to inspect exports and strip what you don't need."
echo " - GHC's runtime may require additional object files or special GC/runtime linking — linking Haskell -> WASM is nontrivial."
echo " - If linking fails due to missing symbols, add GHC runtime objects or consider producing a static C shim that calls the GHC-generated wasm module."
