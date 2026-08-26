#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Inspecting Common.Api.hs for API type definition..."
echo "--- FILE CONTENTS ---"
cat "$DIR/common/src/Common/Api.hs"
echo "--- END CONTENTS ---"
