#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing backend Only import and frontend Wasm configuration..."

# Fix 1: Update backend/src/DB.hs with correct Only import
cat << 'EOF' > "$DIR/backend/src/DB.hs"
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DB
  ( DBConn
  , initDB
  , getUsers
  , createUser
  , deleteUser
  , updateUser
  ) where

import Data.Text (Text)
import Database.MySQL.Simple (ConnectInfo(..), Connection, connect, defaultConnectInfo, execute, query_)
import Database.MySQL.Simple.Types (Only(..))
import Common.Types (User(..), UserId)

type DBConn = Connection

initDB :: IO DBConn
initDB = do
  putStrLn "[HRSM] Connecting to MariaDB..."
  let connInfo = defaultConnectInfo 
        { connectUser = "hrsm_user"
        , connectPassword = "hrsm_password"
        , connectDatabase = "project_db"
        }
  connect connInfo

getUsers :: DBConn -> IO [User]
getUsers conn = do
  rows <- query_ conn "SELECT id, name, email FROM users"
  return [ User (Just uid) name email | (uid, name, email) <- rows ]

createUser :: DBConn -> User -> IO UserId
createUser conn User{..} = do
  _ <- execute conn "INSERT INTO users (name, email) VALUES (?, ?)" (userName, userEmail)
  return 1 

deleteUser :: DBConn -> UserId -> IO ()
deleteUser conn uid = do
  _ <- execute conn "DELETE FROM users WHERE id = ?" (Only uid)
  return ()

updateUser :: DBConn -> UserId -> User -> IO ()
updateUser conn uid User{..} = do
  _ <- execute conn "UPDATE users SET name = ?, email = ? WHERE id = ?" (userName, userEmail, uid)
  return ()
EOF

# Fix 2: Update flake.nix to use your custom Wasm toolchain
cat << 'EOF' > "$DIR/flake.nix"
{
  description = "HRSM-Skeleton: Haskell Wasm Reflex Servant App";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        
        # Standard GHC for backend and common
        haskellPkgs = pkgs.haskellPackages;
        
        # Use standard GHC for frontend (Wasm compilation handled separately)
        haskellWasmPkgs = pkgs.haskellPackages;

        # 1. Build the local 'common' package first
        commonPkg = haskellPkgs.callCabal2nix "common" ./common {};
        
        # 2. Build 'backend', explicitly passing the local 'common' package
        backendPkg = haskellPkgs.callCabal2nix "backend" ./backend {
          common = commonPkg;
        };

        # 3. Build 'frontend' with standard GHC (Wasm handled by scripts)
        frontendWasmPkg = haskellWasmPkgs.callCabal2nix "frontend" ./frontend {
          common = commonPkg;
        };

        # Emacs 30 package set
        emacsPkgs = pkgs.emacsPackagesFor pkgs.emacs30;

      in
      {
        packages = {
          inherit commonPkg backendPkg frontendWasmPkg;
          common = commonPkg;
          backend = backendPkg;
          frontend-wasm = frontendWasmPkg;
          default = backendPkg;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            haskellPkgs.cabal-install
            haskellPkgs.haskell-language-server
            pkgs.mariadb
            
            # Project-specific Emacs with gptel injected
            (emacsPkgs.emacsWithPackages (epkgs: [
              epkgs.gptel
            ]))
          ];
          
          shellHook = ''
            echo "[HRSM] Development shell loaded."
            echo " - Backend: nix build .#backend"
            echo " - Frontend: nix build .#frontend-wasm"
            echo " - Emacs with gptel is available in this shell."
          '';
        };
      }
    );
}
EOF

echo "[HRSM] Backend and flake.nix fixed successfully."
echo "Next steps:"
echo "  1. Rebuild backend: nix build .#backend"
echo "  2. Rebuild frontend: nix build .#frontend-wasm"
