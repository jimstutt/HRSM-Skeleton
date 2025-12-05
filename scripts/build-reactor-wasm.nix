#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

# === CONFIG (edit if needed) ===
# Path to the GHC that targets wasm (or 'ghc --target=wasm32-wasi' if supported)
GHC_WASM=${GHC_WASM:-ghc}                  # change to your ghc-wasm binary if available
WASI_SYSROOT=${WASI_SYSROOT:-${WASI_SYSROOT:-/nix/store/.../wasi-sysroot}} # If unset, you must set it in devshell
OUTDIR=build
LINKER=${LINKER:-wasm-ld}
CLANG=${CLANG:-clang}
OPTFLAGS="-O2 -ffunction-sections -fdata-sections"

mkdir -p "$OUTDIR"

echo "WASI_SYSROOT=$WASI_SYSROOT"
echo "Using GHC_WASM=$GHC_WASM"

# === 1) Compile Haskell -> object(s)
# We expect the reactor Haskell code to expose a C-callable symbol named `reactor`:
#   foreign export ccall reactor :: IO ()
# or similar in Exports.hs (adjust symbol name if different).
#
# Two options:
#  A) If you have a wasm-enabled GHC:
if "$GHC_WASM" --version >/dev/null 2>&1; then
  echo "Attempting to compile Haskell to wasm object(s) with $GHC_WASM..."
  # compile to object files in OUTDIR
  "$GHC_WASM" --target=wasm32-wasi -c src/Main.hs -outputdir "$OUTDIR" -o "$OUTDIR/reactor.o" || true
  # try to compile other modules too
  mkdir -p dist-reactor || true
  # If your project uses Cabal/build system prefer cabal build with ghc-wasm configured.
else
  echo "ghc-wasm not found or doesn't support --target=wasm32-wasi. Skipping Haskell compilation step - expecting prebuilt objects in dist-reactor/ or build/."
fi

# If you already have object files placed by your build system, pick them up:
# pick up any .o in dist-reactor or build
for f in dist-reactor/*.o build/*.o src/*.o 2>/dev/null; do
  if [ -f "$f" ]; then
    echo "Found object: $f"
  fi
done

# === 2) Compile the C wrapper
# The wrapper must include HsFFI.h or provide a minimal prototype if HsFFI isn't available.
# The wrapper exposes an exported function "call_reactor" which calls the Haskell symbol "reactor".
"$CLANG" --target=wasm32-wasi --sysroot="$WASI_SYSROOT" $OPTFLAGS \
  -nostdinc -I"$WASI_SYSROOT/include" \
  -c src/reactor_wrapper.c -o "$OUTDIR/reactor_wrapper.o" || {
    # fallback: compile without -nostdinc if the sysroot layout differs
    "$CLANG" --target=wasm32-wasi --sysroot="$WASI_SYSROOT" $OPTFLAGS \
      -I"$WASI_SYSROOT/include" -c src/reactor_wrapper.c -o "$OUTDIR/reactor_wrapper.o"
  }

echo "Compiled wrapper -> $OUTDIR/reactor_wrapper.o"

# === 3) Collect object files (GHC-produced and wrapper)
OBJS=""
for f in "$OUTDIR"/*.o dist-reactor/*.o 2>/dev/null; do
  if [ -f "$f" ]; then
    OBJS="$OBJS $f"
  fi
done
# Ensure wrapper is included
OBJS="$OUTDIR/reactor_wrapper.o $OBJS"

echo "Objects to link: $OBJS"

# === 4) Link to wasm with wasm-ld
# We export the wrapper symbol "call_reactor" which calls into the Haskell runtime/symbol `reactor`.
# You may need --allow-undefined if Haskell runtime symbols are unresolved at link time (they'll be resolved at runtime by WASI host).
"$LINKER" --no-entry --export=call_reactor --allow-undefined \
  --strip-debug -o "$OUTDIR/reactor.wasm" $OBJS || {
    echo "Link failed; trying with --export-all (less strict)"
    "$LINKER" --no-entry --export-all --allow-undefined --strip-debug -o "$OUTDIR/reactor.wasm" $OBJS
  }

echo "Linked -> $OUTDIR/reactor.wasm"

# Optional: produce textual wat for inspection
if command -v wasm-objdump >/dev/null 2>&1; then
  wasm-objdump -x "$OUTDIR/reactor.wasm" > "$OUTDIR/reactor.wat" || true
  echo "WAT saved -> $OUTDIR/reactor.wat"
fi

echo "Build complete. Output in $OUTDIR"

