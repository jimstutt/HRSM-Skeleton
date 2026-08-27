# Installation Instructions

## Prerequisites

- **Nix Package Manager**: Required for reproducible builds and dependency management.
- **MariaDB**: The strictly supported database backend.

## 1. Database Setup

Ensure MariaDB is installed and running on your system. On NixOS, enable it via your `configuration.nix`.
You can inspect or initialize the database schema using Nix:

```bash
nix shell nixpkgs#mariadb --run "mariadb -u root project_db"
```

## 2. Entering the Development Environment

Use Nix to load all required build tools (GHC, Cabal, Node.js, etc.) into your shell:

```bash
nix develop
```

## 3. Running the Backend

The backend is a Haskell Servant application. You can build and run it directly via the Nix flake:

```bash
nix run .#backend
```

This will compile the Haskell source and start the Servant server on port `8080`, connecting to your local MariaDB instance.

## 4. Running the Frontend

The frontend utilizes TypeScript and Vite. While inside the `nix develop` shell, navigate to the frontend directory to start the development server:

```bash
cd frontend
npm run dev
```

*(Note: All environment dependencies are provided by Nix. Do not use global package managers like `apt` or global `npm install`.)*

## 5. Building Artifacts

To build specific outputs defined in `flake.nix` without running them:

```bash
# Build the Servant backend
nix build .#backend

# Build the Wasm frontend target
nix build .#frontend-wasm
```
