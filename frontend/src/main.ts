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
  const payload = { userId: 0, ...user };
  const response = await fetch(`${API_BASE}/api/users`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload)
  });
  if (!response.ok) throw new Error(`Failed to create user: ${response.statusText}`);
  return response.json() as Promise<UserId>;
}

async function deleteUser(id: UserId): Promise<void> {
  const response = await fetch(`${API_BASE}/api/users/${id}`, { method: 'DELETE' });
  if (!response.ok) throw new Error(`Failed to delete user: ${response.statusText}`);
}

async function updateUser(id: UserId, user: Omit<User, 'userId'>): Promise<void> {
  const payload = { userId: id, ...user };
  const response = await fetch(`${API_BASE}/api/users/${id}`, {
    method: 'PUT',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload)
  });
  if (!response.ok) throw new Error(`Failed to update user: ${response.statusText}`);
}

function renderUsers(users: User[]): string {
  if (users.length === 0) return '<p>No users found.</p>';
  return `<ul>${users.map(u => 
    `<li>
      <strong>${u.userName}</strong> (${u.userEmail}) - ID: ${u.userId}
      <button class="delete-btn" data-id="${u.userId}" style="margin-left: 10px; background: #e74c3c;">Delete</button>
      <button class="edit-btn" data-id="${u.userId}" data-name="${u.userName}" data-email="${u.userEmail}" style="margin-left: 5px; background: #f39c12;">Edit</button>
    </li>`
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
    <h3>Add / Edit User</h3>
    <form id="user-form">
      <input type="hidden" id="edit-id" value="0" />
      <input type="text" id="username" placeholder="Username" required />
      <input type="email" id="email" placeholder="Email" required />
      <button type="submit" id="submit-btn">Add</button>
      <button type="button" id="cancel-btn" style="display:none; background:#95a5a6;">Cancel</button>
    </form>
    <div id="status"></div>
  `;

  const userList = document.getElementById('user-list')!;
  const form = document.getElementById('user-form') as HTMLFormElement;
  const statusDiv = document.getElementById('status')!;
  const editIdInput = document.getElementById('edit-id') as HTMLInputElement;
  const usernameInput = document.getElementById('username') as HTMLInputElement;
  const emailInput = document.getElementById('email') as HTMLInputElement;
  const submitBtn = document.getElementById('submit-btn') as HTMLButtonElement;
  const cancelBtn = document.getElementById('cancel-btn') as HTMLButtonElement;

  async function refreshUsers() {
    try {
      const users = await getUsers();
      userList.innerHTML = renderUsers(users);
      attachButtonListeners();
    } catch (err) {
      userList.innerHTML = `<p class="error">⚠ Failed to load users: ${err instanceof Error ? err.message : 'Unknown error'}.</p>`;
    }
  }

  function attachButtonListeners() {
    document.querySelectorAll('.delete-btn').forEach(btn => {
      btn.addEventListener('click', async (e) => {
        const id = Number((e.target as HTMLElement).getAttribute('data-id'));
        if (confirm('Delete this user?')) {
          try {
            await deleteUser(id);
            await refreshUsers();
          } catch (err) {
            alert('Delete failed: ' + (err as Error).message);
          }
        }
      });
    });

    document.querySelectorAll('.edit-btn').forEach(btn => {
      btn.addEventListener('click', (e) => {
        const target = e.target as HTMLElement;
        editIdInput.value = target.getAttribute('data-id') || '0';
        usernameInput.value = target.getAttribute('data-name') || '';
        emailInput.value = target.getAttribute('data-email') || '';
        submitBtn.textContent = 'Update';
        cancelBtn.style.display = 'inline-block';
      });
    });
  }

  cancelBtn.addEventListener('click', () => {
    form.reset();
    editIdInput.value = '0';
    submitBtn.textContent = 'Add';
    cancelBtn.style.display = 'none';
  });

  await refreshUsers();

  form.addEventListener('submit', async (e) => {
    e.preventDefault();
    const id = Number(editIdInput.value);
    const username = usernameInput.value;
    const email = emailInput.value;
    
    statusDiv.textContent = id === 0 ? 'Creating...' : 'Updating...';
    try {
      if (id === 0) {
        await createUser({ userName: username, userEmail: email });
      } else {
        await updateUser(id, { userName: username, userEmail: email });
      }
      statusDiv.textContent = '✓ Success!';
      form.reset();
      editIdInput.value = '0';
      submitBtn.textContent = 'Add';
      cancelBtn.style.display = 'none';
      await refreshUsers();
      setTimeout(() => { statusDiv.textContent = ''; }, 2000);
    } catch (err) {
      statusDiv.innerHTML = `<span class="error">✗ Error: ${(err as Error).message}</span>`;
    }
  });
});
