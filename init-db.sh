#!/usr/bin/env bash
set -euo pipefail

# Hardcode absolute path to prevent sudo from changing $HOME to /root
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Checking MySQL/MariaDB service status..."

if ! systemctl is-active --quiet mysql; then
    echo "[HRSM] ERROR: MySQL/MariaDB service is not running."
    echo "[HRSM] Please start it with: sudo systemctl start mysql"
    exit 1
fi

echo "[HRSM] MariaDB is running. Creating database..."
# Run nix as normal user, but pass sudo to the mariadb client for socket access
nix shell nixpkgs#mariadb -c sudo mariadb -u root -e "CREATE DATABASE IF NOT EXISTS project_db;"

echo "[HRSM] Applying schema..."
nix shell nixpkgs#mariadb -c sudo mariadb -u root project_db < "$DIR/backend/sql/schema.sql"

echo "[HRSM] Verifying tables..."
nix shell nixpkgs#mariadb -c sudo mariadb -u root project_db -e "SHOW TABLES;"

echo "[HRSM] Database setup complete."
