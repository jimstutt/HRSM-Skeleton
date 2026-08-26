#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Rewriting common/common.cabal with valid syntax and TS generator..."

cat > "$DIR/common/common.cabal" << 'EOF'
cabal-version: 3.0
name: common
version: 0.1.0.0
build-type: Simple

library
  exposed-modules:
      Common.Api
    , Common.Types
  hs-source-dirs: src
  default-language: Haskell2010
  build-depends:
      base >= 4.14 && < 5
    , servant
    , servant-typescript
    , text
    , aeson

executable generate-ts
  main-is: GenerateTS.hs
  hs-source-dirs: app, src
  default-language: Haskell2010
  build-depends:
      base >= 4.14 && < 5
    , common
    , servant-typescript
    , text
EOF

echo "[HRSM] common.cabal fixed. Now rebuilding ts-types..."
