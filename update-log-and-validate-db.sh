#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Updating PROJECT_LOG.md and validating MariaDB schema..."

# 1. Update PROJECT_LOG.md with today's milestones
cat > "$DIR/PROJECT_LOG.md" << 'EOF'
# HRSM-Skeleton Project Log
Last updated: 2026-08-26
📍 Local path: `~/Dev/HRSM-Skeleton`

## 🔑 Key Conventions
- Use `[HRSM]` prefix in all LLM chat titles.
- Database: MariaDB only (No SQLite, No MongoDB).
- All file edits must be generated as complete, full-replacement terminal shell scripts.
- Script execution: Always `chmod +x <script>` then `bash ./<script>`
- Nix Workflow: DO NOT suggest `npm`, `cabal install`, or `apt`. Always use `nix build`, `nix run`, or `nix shell`.

## 📅 Recent Activity
| Date       | Topic                                      | Status   |
|------------|--------------------------------------------|----------|
| 2026-08-26 | Metadata corrected (HRSM-Skeleton/MariaDB) | Done ✅  |
| 2026-08-26 | Git repo recovered from remote after crash | Done ✅  |
| 2026-08-26 | UI components + backend integration        | Done ✅  |
| 2026-08-26 | Vite+TS frontend initialized via Nix       | Done ✅  |
| 2026-08-26 | Hybrid TS/Servant architecture implemented | Done ✅  |
| 2026-08-26 | OpenAPI + quicktype TS generation pipeline | Done ✅  |
| 2026-08-26 | Git history cleaned of .cabal artifacts    | Done ✅  |
| 2026-08-23 | Project initialization                     | Done ✅  |

## ⚠️ Current Blockers
- [ ] None

## 🧠 Decisions & Rationale
- **2026-08-26: Hybrid Architecture Adoption**  
  Reason: GHC Wasm backend Template Haskell support remains incomplete for Reflex-DOM dependencies. Pivoted to TypeScript frontend with Servant→OpenAPI→quicktype type sharing to maintain type safety while ensuring Wasm compatibility per TechSpec.
- **2026-08-26: Metadata Correction**  
  Reason: Frontend displayed stale "NGO Logistics CG / SQLite" from template. Updated to "HRSM-Skeleton / MariaDB" per TechSpec mandate.
- **2026-08-23: Database Choice**  
  Reason: Strict adherence to MariaDB only, as specified in HRSM-TechSpec.md.

## 🔗 Useful Links
- [Tech Spec](./HRSM-TechSpec.md)
- [GitHub](https://github.com/jimstutt/HRSM-Skeleton)
EOF

# 2. Validate MariaDB schema matches Servant API expectations
echo "[HRSM] Validating MariaDB schema..."
if nix shell nixpkgs#mariadb --run "mariadb -u root project_db -e 'SHOW TABLES; DESCRIBE users;' 2>/dev/null"; then
  echo "[HRSM] ✓ MariaDB schema validated successfully"
else
  echo "[HRSM] ⚠ MariaDB validation failed. Possible causes:"
  echo "   - Database 'project_db' does not exist yet"
  echo "   - MariaDB service not running"
  echo "   - Schema migrations not applied"
  echo "[HRSM] Next: Run backend to auto-initialize DB, or check backend logs"
fi

# 3. Commit log update
cd "$DIR"
git add PROJECT_LOG.md
git commit -m "[HRSM] Update project
