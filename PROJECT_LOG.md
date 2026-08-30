Last updated: 2026-08-29
📍 Local path: ~/Dev/HRSM-Skeleton
🔑 Key Conventions
Use [HRSM] prefix in all LLM chat titles.
Database: MariaDB only (No SQLite, No MongoDB).
All file edits must be generated as complete, full-replacement terminal shell scripts.
Frontend: Pure Haskell WASM (GHC wasm32-wasi backend) + vanilla JS. No Reflex/Miso due to basement dependency limitations.
📅 Recent Activity
Date
Topic
Status
2026-08-29
Documentation sync & TechSpec correction
Done ✅
2026-08-23
Project initialization
Done ✅
⚠️ Current Blockers
None
🧠 Decisions & Rationale
2026-08-29: Frontend Architecture Pivot
Reason: Initial attempts to use Reflex/Miso failed because they depend on basement, which lacks support for GHC's WASM backend.
Decision: Pivoted to a minimal Pure Haskell WASM approach that generates simple HTML and uses vanilla JavaScript for fetch calls and DOM manipulation.
2026-08-23: Database Choice
Reason: Strict adherence to MariaDB only, as specified in the core requirements.
🔗 Useful Links
Tech Spec
GitHub
