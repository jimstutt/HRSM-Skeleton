# Frontend - Haskell WASM

This frontend is compiled from Haskell to WebAssembly using GHC's WASM backend.

## Architecture

The frontend uses a minimal approach to avoid dependency conflicts with the WASM backend:
- **Pure Haskell**: Compiles to WASM without heavy framework dependencies
- **Simple HTML output**: Generates static HTML that can be enhanced with JavaScript
- **REST API calls**: JavaScript in the HTML makes fetch calls to the Haskell backend

## Building

Build the frontend WASM module:

```bash
nix develop
wasm32-wasi-cabal build exe:frontend
```

The WASM file will be generated at:
```
dist-newstyle/build/wasm32-wasi/ghc-9.10.3.20251220/frontend-0.1.0.0/x/frontend/build/frontend/frontend.wasm
```

## Running

1. Make sure the backend is running on port 8080:
```bash
# From project root
./result/bin/backend
```

2. Serve the frontend HTML:
```bash
# From frontend directory
python3 -m http.server 3000
```

3. Open http://localhost:3000/index.html in your browser

## Current Limitations

### Why Not Reflex/Miso?

The initial attempt used Reflex, but it depends on `basement` which doesn't support GHC's WASM backend yet. Miso also pulls in dependencies that conflict with WASM.

### Future Improvements

When the Haskell WASM ecosystem matures, consider:
- Using a full Haskell FRP framework (Miso, Reflex, etc.)
- Direct DOM manipulation from Haskell
- Shared types between frontend and backend via the `shared` package

### Current Approach

The current minimal approach:
- ✅ Successfully compiles to WASM
- ✅ No dependency conflicts
- ✅ Can communicate with backend
- ⚠️ Limited interactivity (mostly handled by JavaScript)
- ⚠️ Not using shared types yet

## Alternative: JavaScript Frontend

If you need richer frontend functionality now, consider:
- React/Vue/Svelte frontend calling the Haskell backend
- Keep the shared API types in Haskell
- Generate TypeScript types from Haskell (using servant-typescript or similar)
