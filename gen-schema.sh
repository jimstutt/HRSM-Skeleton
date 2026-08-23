#!/usr/bin/env bash
set -euo pipefail

DIR="$HOME/Dev/HRSM-Skeleton"

echo "[HRSM] Creating backend/sql directory and schema file..."
mkdir -p "$DIR/backend/sql"

cat << 'EOF' > "$DIR/backend/sql/schema.sql"
-- HRSM-Skeleton MariaDB Schema
CREATE TABLE IF NOT EXISTS tasks (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    done BOOLEAN NOT NULL DEFAULT FALSE
);
EOF

echo "[HRSM] backend/sql/schema.sql generated successfully."
