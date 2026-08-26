import os
D = "/home/jimstutt/Dev/HRSM-Skeleton"

# 1. Update cabal file to use Miso instead of Reflex
cabal = """cabal-version: 3.0
name: frontend-wasm
version: 0.1.0.0
build-type: Simple

executable frontend-wasm-exe
  main-is: Main.hs
  hs-source-dirs: .
  default-language: Haskell2010
  build-depends:
      base >= 4.14 && < 5
    , miso
    , text
  ghc-options:
    -O2
    -no-hs-main
    -optl-mexec-model=reactor
    -optl-Wl,--allow-undefined
    -optl-Wl,--export=start_reactor
    -optl-Wl,--export=reactor_stop
    -optl-Wl,--export-all
"""
with open(os.path.join(D, "frontend-wasm", "frontend-wasm.cabal"), "w") as f:
    f.write(cabal)

# 2. Update Main.hs with a minimal Miso app
main_hs = """{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Miso
import Miso.String (MisoString)
import System.IO (hFlush, stdout)

foreign export ccall start_reactor :: IO ()
foreign export ccall reactor_stop  :: IO ()

data Model = Model { count :: Int }
data Action = AddOne | SubtractOne | NoOp

start_reactor :: IO ()
start_reactor = do
  putStrLn "[HRSM] Miso Reactor initialized."
  hFlush stdout

reactor_stop :: IO ()
reactor_stop = do
  putStrLn "[HRSM] Miso Reactor stopped."
  hFlush stdout

updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = m { count = count m + 1 } <# pure NoOp
updateModel SubtractOne m = m { count = count m - 1 } <# pure NoOp
updateModel NoOp m = pure m

viewModel :: Model -> View Action
viewModel Model{..} =
  div_ []
    [ button_ [ onClick SubtractOne ] [ text "-" ]
    , text (ms (show count))
    , button_ [ onClick AddOne ] [ text "+" ]
    ]

main :: IO ()
main = pure ()
"""
with open(os.path.join(D, "frontend-wasm", "Main.hs"), "w") as f:
    f.write(main_hs)

# 3. Update cabal.project (remove external interpreter flags)
cabal_proj = """packages:
  .
"""
with open(os.path.join(D, "frontend-wasm", "cabal.project"), "w") as f:
    f.write(cabal_proj)

# 4. Update build-wasm.sh (remove external interpreter flags)
build_sh = """#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mkdir -p "$DIR/dist-wasm"

echo "[1/3] Compiling C stubs..."
wasm32-wasi-clang -c "$DIR/frontend-wasm/stubs.c" -o "$DIR/dist-wasm/stubs.o"

echo "[2/3] Building frontend-wasm with wasm32-wasi-cabal..."
export HOME="$DIR"
export CABAL_DIR="$DIR/.cabal"
rm -rf "$HOME/.config/cabal"
unset GHC_PACKAGE_PATH

[ -f "$DIR/cabal.project" ] && mv "$DIR/cabal.project" "$DIR/cabal.project.bak"
rm -rf "$DIR/dist-newstyle/cache/plan.json" "$DIR/frontend-wasm/dist-newstyle"

wasm32-wasi-cabal update

cd "$DIR/frontend-wasm"
wasm32-wasi-cabal build frontend-wasm-exe

cd "$DIR"
[ -f "$DIR/cabal.project.bak" ] && mv "$DIR/cabal.project.bak" "$DIR/cabal.project"

echo "[3/3] Linking with stubs..."
OBJ_FILE=$(find "$DIR/dist-newstyle" -type f -name "Main.o" | grep "frontend-wasm" | head -n 1)
[ -z "$OBJ_FILE" ] && { echo "Error: Main.o not found"; exit 1; }

wasm32-wasi-ghc -O2 -no-hs-main -optl-mexec-model=reactor -optl-Wl,--allow-undefined -optl-Wl,--export=start_reactor -optl-Wl,--export=reactor_stop -optl-Wl,--export-all "$OBJ_FILE" "$DIR/dist-wasm/stubs.o" -o "$DIR/dist-wasm/reactor.wasm"
echo "[HRSM] Done: $DIR/dist-wasm/reactor.wasm"
"""
with open(os.path.join(D, "scripts", "build-wasm.sh"), "w") as f:
    f.write(build_sh)
os.chmod(os.path.join(D, "scripts", "build-wasm.sh"), 0o755)

print("[HRSM] Switched frontend to Miso. Removed TH external interpreter hacks.")
