#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Initializing Vite frontend via Nix (no npm)..."

cd "$DIR/frontend"

# 1. Create package.json with Vite + TypeScript deps
cat > package.json << 'EOF'
{
  "name": "hrsm-frontend",
  "private": true,
  "version": "0.1.0",
  "type": "module",
  "scripts": {
    "dev": "vite",
    "build": "tsc && vite build",
    "preview": "vite preview"
  },
  "devDependencies": {
    "typescript": "^5.5.0",
    "vite": "^5.4.0"
  }
}
EOF

# 2. Create tsconfig.json
cat > tsconfig.json << 'EOF'
{
  "compilerOptions": {
    "target": "ES2020",
    "useDefineForClassFields": true,
    "module": "ESNext",
    "lib": ["ES2020", "DOM", "DOM.Iterable"],
    "skipLibCheck": true,
    "moduleResolution": "bundler",
    "allowImportingTsExtensions": true,
    "isolatedModules": true,
    "moduleDetection": "force",
    "noEmit": true,
    "strict": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "noFallthroughCasesInSwitch": true
  },
  "include": ["src"]
}
EOF

# 3. Create index.html entry point
cat > index.html << 'EOF'
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>HRSM Skeleton</title>
  </head>
  <body>
    <div id="app"></div>
    <script type="module" src="/src/main.ts"></script>
  </body>
</html>
EOF

# 4. Add frontend outputs to flake.nix devShell
cd "$DIR"
if ! grep -q "nodejs" flake.nix; then
  sed -i '/wasmToolchain/a\            pkgs.nodejs_20' flake.nix
  echo "[HRSM] Added nodejs_20 to devShell"
fi

# 5. Commit frontend scaffolding
git add frontend/package.json frontend/tsconfig.json frontend/index.html flake.nix
git commit -m "[HRSM] Initialize Vite+TS frontend via Nix" || true

echo "[HRSM] ✓ Vite scaffolded. Enter dev shell and run:"
echo "   cd frontend && npm install && npm run dev"
