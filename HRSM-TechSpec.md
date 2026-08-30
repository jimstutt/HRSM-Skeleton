# HRSM-TechSpec

## Project: HRSM-Skeleton (Haskell Wasm Servant App)

### Environment Context
- **OS**: NixOS (reproducible builds via flakes)
- **Build System**: Nix Flakes + Cabal
- **Frontend**: Pure Haskell compiled to WebAssembly (via GHC's `wasm32-wasi` backend). Generates simple HTML and uses vanilla JavaScript for REST API calls.
- **Backend**: Haskell Servant
- **Database**: MariaDB

### Architecture Overview
- **Frontend**: Due to current limitations in the Haskell WASM ecosystem (specifically, the `basement` dependency required by Reflex and Miso not supporting GHC's WASM backend yet), the frontend uses a minimal Pure Haskell approach. It compiles to WASM, generates static HTML, and relies on embedded JavaScript for DOM manipulation and `fetch` calls to the backend.
- **Backend**: Haskell Servant API server.
- **Database**: MariaDB for persistence.
- **Shared**: `common/` package holds shared Servant API types and business logic.

### Critical Instructions for Qwen
- **Nix Workflow**: DO NOT suggest `npm`, `cabal install`, or `apt`. Always use `nix build`, `nix run`, or `nix shell`.
- **Build Commands**:
  - **Development Shell**: `nix develop` (Required to access the `wasm32-wasi-cabal` toolchain)
  - **Build Wasm Frontend**: `wasm32-wasi-cabal build exe:frontend` (Run from within `nix develop`)
  - **Build Backend**: `nix build .#backend`
- **Wasm Constraints**: 
  - The frontend is compiled using GHC's `wasm32-wasi` backend.
  - **Strictly avoid** libraries that do not support this backend (e.g., `basement`, Reflex, Miso, GHCJS-only libraries).
  - Keep frontend dependencies minimal to ensure successful WASM compilation.
- **MariaDB Access**: Use `nix shell nixpkgs#mariadb --run "mariadb -u root project_db"` to inspect the schema.
- **Code Style**:
  - Use explicit imports for Servant.
  - Prefer `RecordWildCards` and `OverloadedStrings`.
  - Maintain the strict separation between `common/` (shared types), `frontend/`, and `backend/`.

### Project Structure
- `common/`: Shared Servant API types and business logic.
- `frontend/`: Pure Haskell WASM frontend (generates HTML, uses JS for API calls).
- `backend/`: Servant server and MariaDB persistence.
- `nix/`: Nix derivations and overlays.
- `flake.nix`: Defines the build environment and outputs.