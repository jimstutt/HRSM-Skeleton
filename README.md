# HRSM-Skeleton

A Wasm, Haskell, MariaDB, and Servant web application skeleton.

## Architecture Overview

- **Frontend**: Pure Haskell compiled to WebAssembly (via GHC's `wasm32-wasi` backend). Generates simple HTML and uses vanilla JavaScript for REST API calls.
- **Backend**: Haskell Servant
- **Database**: MariaDB
- **Build System**: Nix Flakes + Cabal

## Project Structure

- `common/`: Shared Servant API types and business logic.
- `frontend/`: Pure Haskell WASM frontend logic.
- `backend/`: Servant server and MariaDB persistence.
- `nix/`: Nix derivations and overlays.
- `flake.nix`: Defines the build environment and outputs.

## Frontend - Haskell WASM

This frontend is compiled from Haskell to WebAssembly using GHC's WASM backend (`wasm32-wasi`).

### Architecture

The frontend uses a minimal approach to avoid dependency conflicts with the WASM backend:
- **Pure Haskell**: Compiles to WASM without heavy framework dependencies.
- **Simple HTML output**: Generates static HTML that can be enhanced with JavaScript.
- **REST API calls**: JavaScript in the HTML makes `fetch` calls to the Haskell backend.

### Why Not Reflex/Miso?

The initial plan was to use a full Haskell FRP framework like Reflex or Miso. However, they depend on `basement` (or pull in dependencies that conflict), which does not support GHC's WASM backend yet. This minimal approach ensures successful compilation and avoids dependency hell.

### Building

Enter the development shell to get the `wasm32-wasi-cabal` toolchain:
```bash
nix develop

wasm32-wasi-cabal build exe:frontend

The WASM file will be generated at:
dist-newstyle/build/wasm32-wasi/ghc-9.10.3.20251220/frontend-0.1.0.0/x/frontend/build/frontend/frontend.wasm
Running
Make sure the backend is running on port 8080:
bash
   # From project root
   ./result/bin/backend
12
Serve the frontend HTML:
bash
   # From frontend directory
   python3 -m http.server 3000
12
Open http://localhost:3000/index.html in your browser.
Current Limitations & Future Improvements
⚠️ Limited interactivity: DOM manipulation and API calls are mostly handled by vanilla JavaScript.
⚠️ No shared types in frontend: The WASM frontend doesn't currently use the shared types from common/ directly in the browser context.
Future Improvements:
When the Haskell WASM ecosystem matures (e.g., basement gets WASM support), consider:
Migrating to a full Haskell FRP framework (Miso, Reflex, etc.).
Direct DOM manipulation from Haskell.
Sharing types between frontend and backend via the common package.
Installation & Setup
Refer to the Nix flake setup. Ensure you have Nix installed with flakes enabled.

