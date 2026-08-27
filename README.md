# HRSM-Skeleton

A Wasm, Haskell, MariaDB, Servant, Typescript web application skeleton.

Refer to [INSTALL.md](./INSTALL.md) for installation instructions.

## Architecture Overview

- **Frontend**: TypeScript (Vite) / Haskell Reflex-DOM (Target: WebAssembly)
- **Backend**: Haskell Servant
- **Database**: MariaDB
- **Build System**: Nix Flakes + Cabal

## Project Structure

- `common/`: Shared Servant API types and business logic.
- `frontend/`: TypeScript UI logic (Vite) and Reflex-DOM Wasm targets.
- `backend/`: Servant server and MariaDB persistence.
- `nix/`: Nix derivations and overlays.
- `flake.nix`: Defines the build environment and outputs.
