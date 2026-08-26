# HRSM-Skeleton Project Log
*Last updated: 2026-08-23*
📍 Local path: `~/Dev/HRSM-Skeleton`

## 🔑 Key Conventions
- Use `[HRSM]` prefix in all LLM chat titles.
- Database: MariaDB only (No SQLite, No MongoDB).
- All file edits must be generated as complete, full-replacement terminal shell scripts.

## 📅 Recent Activity
| Date       | Topic                         | Status     |
|------------|-------------------------------|------------|
| 2026-08-23 | Project initialization        | Done ✅    |

## ⚠️ Current Blockers
- [ ] None

## 🧠 Decisions & Rationale
### 2026-08-23: Database Choice
> Reason: Strict adherence to MariaDB only, as specified in HRSMTechSpec.md.

## 🔗 Useful Links
- [Tech Spec](./HRSM-TechSpec.md)
- [GitHub](https://github.com/jimstutt/HRSM-Skeleton)

## 2026-08-25: Backend Execution Success
**Status**: Done ✅
**Details**: 
- Successfully built and executed the backend using `nix build .#backend`.
- Backend starts on port 8080 and successfully connects to MariaDB.
- Command: `./result/bin/backend-exe`
