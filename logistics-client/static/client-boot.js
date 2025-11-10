// client-boot.js - plain JS to avoid relying on fragile glue growth
(() => {
  const base = 'https://localhost:8443';
  let token = null;

  document.getElementById('btnLogin').addEventListener('click', async () => {
    const username = document.getElementById('username').value;
    const password = document.getElementById('password').value;
    try {
      const r = await fetch(base + '/auth/login', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ username, password })
      });
      if (!r.ok) throw new Error(r.status + ' ' + r.statusText);
      const j = await r.json();
      token = j.token;
      document.getElementById('loginResult').innerText = 'Logged in';
    } catch (e) {
      document.getElementById('loginResult').innerText = 'Login error: ' + e;
    }
  });

  document.getElementById('btnList').addEventListener('click', async () => {
    try {
      const r = await fetch(base + '/items');
      const j = await r.json();
      document.getElementById('itemsOut').innerText = JSON.stringify(j, null, 2);
    } catch (e) {
      document.getElementById('itemsOut').innerText = 'Error: ' + e;
    }
  });

  document.getElementById('btnCreate').addEventListener('click', async () => {
    const name = document.getElementById('itemName').value;
    const qty = parseInt(document.getElementById('itemQty').value || '0', 10);
    const loc = document.getElementById('itemLoc').value || null;
    try {
      const headers = { 'Content-Type': 'application/json' };
      if (token) headers['Authorization'] = 'Bearer ' + token;
      const r = await fetch(base + '/items', { method: 'POST', headers, body: JSON.stringify({ name, quantity: qty, location: loc }) });
      if (!r.ok) throw new Error(r.status + ' ' + r.statusText);
      const j = await r.json();
      document.getElementById('itemsOut').innerText = 'Created: ' + JSON.stringify(j);
    } catch (e) {
      document.getElementById('itemsOut').innerText = 'Create error: ' + e;
    }
  });
})();
