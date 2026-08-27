# HRSM-Skeleton Project Log
Last updated: 2026-08-27
📍 Local path: `~/Dev/HRSM-Skeleton`

## 🔑 Key Conventions
- Use `[HRSM]` prefix in all LLM chat titles.
- Database: MariaDB only (No SQLite, No MongoDB).
- All file edits must be generated as complete, full-replacement terminal shell scripts.
- Script execution: Always `chmod +x <script>` then `bash ./<script>`
- Nix Workflow: DO NOT suggest `npm`, `cabal install`, or `apt`. Always use `nix build`, `nix run`, or `nix shell`.

## 📅 Recent Activity
| Date       | Topic                                                | Status   |
|------------|------------------------------------------------------|----------|
| 2026-08-27 | Full CRUD (Add/Edit/Delete) MariaDB integration      | Done ✅  |
| 2026-08-27 | CORS & Backend connection (Servant ↔ Vite)           | Done ✅  |
| 2026-08-27 | Frontend sanitized (Eradicated NGO Logistics remnant)| Done ✅  |
| 2026-08-27 | Hybrid TS/Servant architecture & OpenAPI pipeline    | Done ✅  |
| 2026-08-26 | Metadata corrected (HRSM-Skeleton/MariaDB)           | Done ✅  |
| 2026-08-26 | Git repo recovered from remote after crash           | Done ✅  |
| 2026-08-23 | Project initialization                               | Done ✅  |

## ⚠️ Current Blockers
- [ ] None

## 🧠 Decisions & Rationale
- **2026-08-27: Full CRUD Implementation**  
  Reason: Wired Servant handlers directly to `mysql-simple` queries. Used `LAST_INSERT_ID()` for safe auto-increment retrieval. Bypassed NixOS `root` socket-auth by creating a dedicated `hrsm_user` with TCP (`127.0.0.1`) routing.
- **2026-08-27: Hybrid Architecture Adoption**  
  Reason: GHC Wasm backend Template Haskell support remains incomplete for Reflex-DOM dependencies. Pivoted to TypeScript frontend with Servant→OpenAPI→TS type sharing to maintain type safety while ensuring compatibility per TechSpec.
- **2026-08-23: Database Choice**  
  Reason: Strict adherence to MariaDB only, as specified in HRSM-TechSpec.md.

## 🔗 Useful Links
- [Tech Spec](./HRSM-TechSpec.md)
- [GitHub](https://github.com/jimstutt/HRSM-Skeleton)
