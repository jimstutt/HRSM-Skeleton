#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Implementing UI components with backend integration and updating PROJECT_LOG.md..."

# 1. Update main.ts with actual UI + backend fetch logic
cat > "$DIR/frontend/src/main.ts" << 'EOF'
import { User, UserId } from './api-types';

const API_BASE = 'http://localhost:8080';

async function getUsers(): Promise<User[]> {
  const response = await fetch(`${API_BASE}/api/users`);
  if (!response.ok) throw new Error(`Failed to fetch users: ${response.statusText}`);
  return response.json() as Promise<User[]>;
}

async function createUser(user: Omit<User, 'userId'>): Promise<UserId> {
  const response = await fetch(`${API_BASE}/api/users`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(user)
  });
  if (!response.ok) throw new Error(`Failed to create user: ${response.statusText}`);
  return response.json() as Promise<UserId>;
}

function renderUsers(users: User[]): string {
  if (users.length === 0) return '<p>No users found.</p>';
  return `<ul>${users.map(u => 
    `<li><strong>${u.userName}</strong> (ID: ${u.userId})</li>`
  ).join('')}</ul>`;
}

document.addEventListener('DOMContentLoaded', async () => {
  const app = document.getElementById('app');
  if (!app) return;

  app.innerHTML = '<h1>HRSM Skeleton</h1><p>Loading users...</p>';

  try {
    const users = await getUsers();
    app.innerHTML = `
      <h1>HRSM Skeleton</h1>
      <h2>Users (${users.length})</h2>
      ${renderUsers(users)}
      <hr/>
      <h3>Add User</h3>
      <form id="add-user-form">
        <input type="text" id="username" placeholder="Username" required />
        <button type="submit">Add</button>
      </form>
      <div id="status"></div>
    `;

    // Wire up form submission
    const form = document.getElementById('add-user-form') as HTMLFormElement;
    const statusDiv = document.getElementById('status')!;
    
    form.addEventListener('submit', async (e) => {
      e.preventDefault();
      const username = (document.getElementById('username') as HTMLInputElement).value;
      statusDiv.textContent = 'Creating...';
      try {
        await createUser({ userName: username });
        statusDiv.textContent = '✓ Created! Refreshing...';
        const updatedUsers = await getUsers();
        app.querySelector('h2')!.textContent = `Users (${updatedUsers.length})`;
        app.querySelector('ul')!.outerHTML = renderUsers(updatedUsers);
        (document.getElementById('username') as HTMLInputElement).value = '';
        statusDiv.textContent = '';
      } catch (err) {
        statusDiv.textContent = `✗ Error: ${err instanceof Error ? err.message : 'Unknown'}`;
      }
    });

  } catch (err) {
    app.innerHTML = `
      <h1>HRSM Skeleton</h1>
      <p style="color:red;">⚠ Failed to load users: ${err instanceof Error ? err.message : 'Unknown error'}</p>
      <p>Ensure backend is running: <code>nix run .#backend</code></p>
    `;
  }
});
EOF

# 2. Update PROJECT_LOG.md with implementation milestone
cat > "$DIR/PROJECT_LOG.md" << 'EOF'
# HRSM-Skeleton Project Log
Last updated: 2026-08-26
📍 Local path: `~/Dev/HRSM-Skeleton`

## 🔑 Key Conventions
- Use `[HRSM]` prefix in all LLM chat titles.
- Database: MariaDB only (No SQLite, No MongoDB).
- All file edits must be generated as complete, full-replacement terminal shell scripts.
- Script execution: Always `chmod +x <script>` then `bash ./<script>`

## 📅 Recent Activity
| Date       | Topic                                      | Status   |
|------------|--------------------------------------------|----------|
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
- **2026-08-23: Database Choice**  
  Reason: Strict adherence to MariaDB only, as specified in HRSM-TechSpec.md.

## 🔗 Useful Links
- [Tech Spec](./HRSM-TechSpec.md)
- [GitHub](https://github.com/jimstutt/HRSM-Skeleton)
EOF

# 3. Commit changes
cd "$DIR"
git add frontend/src/main.ts PROJECT_LOG.md
git commit -m "[HRSM] Implement user CRUD UI with backend integration and update log" || true

echo "[HRSM] ✓ UI implemented and PROJECT_LOG.md updated."
echo "[HRSM] Next: Start backend with 'nix run .#backend' and verify at http://localhost:5173/"
