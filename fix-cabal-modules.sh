#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

cat << 'EOF' > "$DIR/backend/backend.cabal"
cabal-version:      3.0
name:               backend
version:            0.1.0.0
build-type:         Simple

library
  exposed-modules:
    Backend,
    DB
  build-depends:
    base >=4.14 && <5,
    common,
    text,
    mysql-simple,
    mtl,
    transformers,
    servant,
    servant-server
  hs-source-dirs:   src
  default-language: Haskell2010
  default-extensions:
    OverloadedStrings
    RecordWildCards

executable backend-exe
  main-is:          Main.hs
  other-modules:
  build-depends:
    base >=4.14 && <5,
    backend,
    common,
    servant,
    servant-server,
    warp,
    wai
  hs-source-dirs:   app
  default-language: Haskell2010
  default-extensions:
    OverloadedStrings
    RecordWildCards
EOF

echo "[HRSM] Fixed backend.cabal - removed modules from executable other-modules"
