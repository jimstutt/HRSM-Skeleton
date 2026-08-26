#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Adding servant to generate-openapi executable dependencies..."

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
    , servant-openapi3
    , openapi3
    , text
    , aeson

executable generate-openapi
  main-is: GenerateOpenAPI.hs
  other-modules:
      Common.Api
    , Common.Types
  hs-source-dirs: app, src
  default-language: Haskell2010
  build-depends:
      base >= 4.14 && < 5
    , common
    , servant
    , servant-openapi3
    , openapi3
    , aeson
    , bytestring
    , text
EOF

echo "[HRSM] common.cabal updated. Committing and rebuilding..."

cd "$DIR"
git add common/common.cabal
git commit -m "[HRSM] Add servant to generate-openapi deps" || true
nix build .#ts-types
