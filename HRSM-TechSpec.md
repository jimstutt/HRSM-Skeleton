# Project: Haskell Wasm Reflex Servant App

## Environment Context
- **OS**: NixOS (reproducible builds via flakes)
- **Build System**: Nix + Cabal
- **Frontend**: Haskell Reflex-DOM (Target: WebAssembly)
- **Backend**: Haskell Servant
- **Database**: MariaDB

## Critical Instructions for Qwen
1. **Nix Workflow**: DO NOT suggest `npm`, `cabal install`, or `apt`. Always use `nix build`, `nix run`, or `nix shell`.
2. **Build Commands**:
   - Build Wasm Frontend: `nix build .#frontend-wasm`
   - Build Backend: `nix build .#backend`
   - Development Shell: `nix develop`
3. **Wasm Constraints**: Remember that Reflex-DOM code is compiled to Wasm. Avoid GHCJS-only libraries or C-system dependencies that don't support the Wasm backend.
4. **MariaDB Access**: Use `nix shell nixpkgs#mariadb --run "mariadb -u root project_db"` to inspect the schema.
5. **Code Style**: 
   - Use explicit imports for Servant and Reflex.
   - Prefer `RecordWildCards` and `OverloadedStrings`.
   - Maintain the separation between `common/` (shared types), `frontend/`, and `backend/`.

## Project Structure
- `common/`: Shared Servant API types and business logic.
- `frontend/`: Reflex-DOM UI logic.
- `backend/`: Servant server and MariaDB persistence.
- `flake.nix`: Defines the build environment and outputs.

