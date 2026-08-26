{ pkgs, commonPkg }:
pkgs.stdenv.mkDerivation {
  pname = "hrsm-ts-types";
  version = "0.1.0.0";
  src = ../.;
  nativeBuildInputs = [ 
    commonPkg
    pkgs.haskellPackages.ghc 
    pkgs.openapi-typescript
  ];
  buildPhase = ''
    mkdir -p frontend/src
    
    # Generate OpenAPI spec
    generate-openapi --output=frontend/openapi.json
    
    # Generate TypeScript using openapi-typescript
    openapi-typescript frontend/openapi.json -o frontend/src/api-types.ts
  '';
  installPhase = ''
    mkdir -p $out
    cp frontend/src/api-types.ts $out/
  '';
}
