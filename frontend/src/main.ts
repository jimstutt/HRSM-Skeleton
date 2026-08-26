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
    `<li><strong>${u.userName}</strong> (${u.userEmail}) - ID: ${u.userId}</li>`
  ).join('')}</ul>`;
}

document.addEventListener('DOMContentLoaded', async () => {
  const app = document.getElementById('app');
  if (!app) return;

  app.innerHTML = `
    <h1>${PROJECT_NAME}</h1>
    <div class="stack-info">
      <strong>Stack Info</strong>
      <p>Frontend: TypeScript (Vite)</p>
      <p>Backend: Servant (Haskell)</p>
      <p>Database: ${DB_BACKEND}</p>
    </div>
    <h2>Users</h2>
    <div id="user-list"><p>Loading users...</p></div>
    <hr/>
    <h3>Add User</h3>
    <form id="add-user-form">
      <input type="text" id="username" placeholder="Username" required />
      <input type="email" id="email" placeholder="Email" required />
      <button type="submit">Add</button>
    </form>
    <div id="status"></div>
  `;

  const userList = document.getElementById('user-list')!;
  const form = document.getElementById('add-user-form') as HTMLFormElement;
  const statusDiv = document.getElementById('status')!;

  async function refreshUsers() {
    try {
      const users = await getUsers();
      userList.innerHTML = renderUsers(users);
    } catch (err) {
      userList.innerHTML = `<p class="error">⚠ Failed to load users: ${err instanceof Error ? err.message : 'Unknown error'}. Is the backend running on port 8080?</p>`;
    }
  }

  await refreshUsers();

  form.addEventListener('submit', async (e) => {
    e.preventDefault();
    const username = (document.getElementById('username') as HTMLInputElement).value;
    const email = (document.getElementById('email') as HTMLInputElement).value;
    statusDiv.textContent = 'Creating...';
    try {
      await createUser({ userName: username, userEmail: email });
      statusDiv.textContent = '✓ Created!';
      (document.getElementById('username') as HTMLInputElement).value = '';
      (document.getElementById('email') as HTMLInputElement).value = '';
      await refreshUsers();
      setTimeout(() => { statusDiv.textContent = ''; }, 2000);
    } catch (err) {
      statusDiv.innerHTML = `<span class="error">✗ Error: ${err instanceof Error ? err.message : 'Unknown'}</span>`;
    }
  });
});
