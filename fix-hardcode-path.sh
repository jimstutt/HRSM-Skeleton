#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"
sed -i 's|^DIR=.*|DIR="/home/jimstutt/Dev/HRSM-Skeleton"|' "$DIR/fix-absolute-isolation.sh"
sed -i 's|^DIR=.*|DIR="/home/jimstutt/Dev/HRSM-Skeleton"|' "$DIR/scripts/build-wasm.sh"
bash "$DIR/fix-absolute-isolation.sh"
