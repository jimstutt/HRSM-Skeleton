import { User, UserId } from './api-types';

const API_BASE = 'http://localhost:8080';
const PROJECT_NAME = 'HRSM-Skeleton';
const DB_BACKEND = 'MariaDB';

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

  app.innerHTML = `<h1>${PROJECT_NAME}</h1><p>Loading users...</p>`;

  try {
    const users = await getUsers();
    app.innerHTML = `
      <h1>${PROJECT_NAME}</h1>
      <div style="margin-bottom:1rem;font-size:0.9rem;color:#666;">
        Stack: Haskell Servant + ${DB_BACKEND} | Frontend: TypeScript (Hybrid)
      </div>
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
      <h1>${PROJECT_NAME}</h1>
      <div style="margin-bottom:1rem;font-size:0.9rem;color:#666;">
        Stack: Haskell Servant + ${DB_BACKEND} | Frontend: TypeScript (Hybrid)
      </div>
      <p style="color:red;">⚠ Failed to load users: ${err instanceof Error ? err.message : 'Unknown error'}</p>
      <p>Ensure backend is running: <code>nix run .#backend</code></p>
    `;
  }
});
