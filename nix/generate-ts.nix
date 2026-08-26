{ pkgs, commonPkg }:
pkgs.stdenv.mkDerivation {
  pname = "hrsm-ts-types";
  version = "0.1.0.0";
  src = ../.;
  nativeBuildInputs = [ 
    commonPkg
    pkgs.haskellPackages.ghc 
    pkgs.jq
    pkgs.quicktype
  ];
  buildPhase = ''
    mkdir -p frontend/src
    
    # Generate full OpenAPI spec
    generate-openapi --output=frontend/openapi.json
    
    # Extract only component schemas as standalone JSON Schema
    jq '.components.schemas' frontend/openapi.json > frontend/schemas.json
    
    # Generate TypeScript from extracted schemas
    quicktype --src-lang schema --lang typescript \
      --out frontend/src/api-types.ts \
      frontend/schemas.json
  '';
  installPhase = ''
    mkdir -p $out
    cp frontend/src/api-types.ts $out/
  '';
}
