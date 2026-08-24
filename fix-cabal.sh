#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Overwriting common/common.cabal with a valid configuration..."

cat << 'EOF' > "$DIR/common/common.cabal"
cabal-version:      3.0
name:               common
version:            0.1.0.0
synopsis:           Shared types and API for HRSM
build-type:         Simple

library
  exposed-modules:
    Common.Api,
    Common.Types
  build-depends:
    base >=4.14 && <5,
    text,
    aeson,
    servant
  hs-source-dirs:   src
  default-language: Haskell2010
  default-extensions:
    OverloadedStrings
    RecordWildCards
    DataKinds
    TypeOperators
EOF

echo "[HRSM] common.cabal fixed successfully."
echo "Next step: Run 'nix build .#backend' to verify."
