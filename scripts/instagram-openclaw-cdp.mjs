#!/usr/bin/env node

import WebSocket from 'ws';

const command = process.argv[2];
if (!command) {
  throw new Error('Usage: instagram-openclaw-cdp.mjs <expression> | --method <CDP.method> <params-json>');
}

const configuredTimeout = Number.parseInt(process.env.TDF_INSTAGRAM_CDP_TIMEOUT_MS || '30000', 10);
const requestTimeoutMs = Number.isFinite(configuredTimeout) && configuredTimeout > 0
  ? Math.min(configuredTimeout, 30_000)
  : 30_000;
const connectionTimeoutMs = Math.min(requestTimeoutMs, 10_000);

const targets = await fetch('http://127.0.0.1:18800/json/list', {
  signal: AbortSignal.timeout(connectionTimeoutMs),
}).then((response) => {
  if (!response.ok) throw new Error(`CDP target lookup failed: HTTP ${response.status}`);
  return response.json();
});
const instagramPages = targets
  .filter((target) => target.type === 'page' && target.url.startsWith('https://www.instagram.com/'))
  .sort((left, right) => {
    const rank = (target) => {
      if (target.url.startsWith('https://www.instagram.com/direct/')) return 0;
      if (!target.url.includes('/accounts/login')) return 1;
      return 2;
    };
    return rank(left) - rank(right);
  });
const page = instagramPages[0];
if (!page) throw new Error('Instagram page target not found on CDP port 18800');

const socket = new WebSocket(page.webSocketDebuggerUrl);
await new Promise((resolve, reject) => {
  const timer = setTimeout(() => reject(new Error('CDP connection timeout')), connectionTimeoutMs);
  socket.once('open', () => {
    clearTimeout(timer);
    resolve();
  });
  socket.once('error', reject);
});

let nextId = 1;
const pending = new Map();
socket.on('message', (data) => {
  const message = JSON.parse(data.toString());
  const waiter = pending.get(message.id);
  if (!waiter) return;
  pending.delete(message.id);
  if (message.error) waiter.reject(new Error(message.error.message));
  else waiter.resolve(message.result || {});
});

const send = (method, params = {}) => new Promise((resolve, reject) => {
  const id = nextId++;
  const timer = setTimeout(() => {
    pending.delete(id);
    reject(new Error(`${method} timeout`));
  }, requestTimeoutMs);
  pending.set(id, {
    resolve: (value) => {
      clearTimeout(timer);
      resolve(value);
    },
    reject: (error) => {
      clearTimeout(timer);
      reject(error);
    },
  });
  socket.send(JSON.stringify({ id, method, params }));
});

await send('Runtime.enable');
const authenticationProbe = await send('Runtime.evaluate', {
  expression: `Boolean(
    document.cookie.split('; ').some((entry) => entry.startsWith('ds_user_id=')) ||
    document.querySelector('a[href="/tdf.records.label/"]') ||
    document.body?.innerText?.includes('tdf.records.label')
  )`,
  returnByValue: true,
  userGesture: false,
});
if (!authenticationProbe.result?.value) {
  socket.close();
  throw new Error(`Instagram target is not authenticated as tdf.records.label: ${page.url}`);
}

const result = command === '--method'
  ? await send(process.argv[3], JSON.parse(process.argv[4] || '{}'))
  : await send('Runtime.evaluate', {
      expression: command,
      awaitPromise: true,
      returnByValue: true,
      userGesture: false,
    });
socket.close();

if (command !== '--method' && result.exceptionDetails) {
  throw new Error(result.exceptionDetails.text || 'Runtime evaluation failed');
}

console.log(JSON.stringify({
  targetUrl: page.url,
  value: command === '--method' ? result : result.result?.value,
}, null, 2));
