#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Validating MariaDB schema with correct auth method..."

# Use sudo for unix_socket auth (NixOS default)
if sudo nix-shell -p mariadb --run "mariadb project_db -e 'SHOW TABLES; DESCRIBE users;'"; then
  echo "[HRSM] ✓ MariaDB schema validated successfully"
else
  echo "[HRSM] ⚠ Validation failed. Checking if database exists..."
  # List all databases to verify project_db was created
  sudo nix-shell -p mariadb --run "mariadb -e 'SHOW DATABASES;'" || true
  echo "[HRSM] If 'project_db' is missing, backend may not have initialized it yet."
fi
