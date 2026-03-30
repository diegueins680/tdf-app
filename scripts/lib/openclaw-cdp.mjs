import { existsSync, readFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';
import WebSocket from 'ws';

const RELAY_TOKEN_HEADER = 'x-openclaw-relay-token';
const DEFAULT_RELAY_BASE_URL = 'http://127.0.0.1:18792';

function ensureTrailingSlash(url) {
  return url.endsWith('/') ? url : `${url}/`;
}

export function resolveRelayBaseUrl() {
  return (process.env.SRI_RELAY_CDP_URL || DEFAULT_RELAY_BASE_URL).trim();
}

export function resolveGatewayToken() {
  const direct =
    process.env.OPENCLAW_GATEWAY_TOKEN?.trim() ||
    process.env.CLAWDBOT_GATEWAY_TOKEN?.trim() ||
    process.env.SRI_OPENCLAW_GATEWAY_TOKEN?.trim();
  if (direct) return direct;

  const stateDir =
    process.env.OPENCLAW_STATE_DIR?.trim() ||
    process.env.CLAWDBOT_STATE_DIR?.trim() ||
    join(homedir(), '.openclaw');
  const configPath =
    process.env.OPENCLAW_CONFIG_PATH?.trim() ||
    process.env.CLAWDBOT_CONFIG_PATH?.trim() ||
    join(stateDir, 'openclaw.json');
  if (!existsSync(configPath)) {
    return '';
  }

  const raw = readFileSync(configPath, 'utf8');
  const match = raw.match(/"token"\s*:\s*"([^"]+)"/);
  return match?.[1]?.trim() || '';
}

export async function fetchRelayJson(pathname, token, baseUrl = resolveRelayBaseUrl()) {
  const url = new URL(pathname.replace(/^\//, ''), ensureTrailingSlash(baseUrl));
  const response = await fetch(url, {
    headers: token ? { [RELAY_TOKEN_HEADER]: token } : {},
  });
  if (!response.ok) {
    const body = await response.text().catch(() => '');
    throw new Error(`Relay HTTP ${response.status}${body ? `: ${body}` : ''}`);
  }
  return response.json();
}

function normalizeWsUrl(raw) {
  if (!raw) {
    throw new Error('CDP websocket URL is missing');
  }
  if (raw.startsWith('ws://') || raw.startsWith('wss://')) {
    return raw;
  }
  if (raw.startsWith('http://')) {
    return `ws://${raw.slice('http://'.length)}`;
  }
  if (raw.startsWith('https://')) {
    return `wss://${raw.slice('https://'.length)}`;
  }
  return raw;
}

export async function createCdpClient(opts = {}) {
  const baseUrl = (opts.baseUrl || resolveRelayBaseUrl()).trim();
  const token = (opts.token || resolveGatewayToken()).trim();
  if (!token) {
    throw new Error(
      'Missing OpenClaw gateway token. Set OPENCLAW_GATEWAY_TOKEN or SRI_OPENCLAW_GATEWAY_TOKEN.',
    );
  }

  const version = await fetchRelayJson('/json/version', token, baseUrl);
  const wsUrl = normalizeWsUrl(version.webSocketDebuggerUrl);
  const ws = new WebSocket(wsUrl, {
    headers: { [RELAY_TOKEN_HEADER]: token },
    handshakeTimeout: 10_000,
  });

  let nextId = 1;
  const pending = new Map();

  function rejectPending(error) {
    for (const entry of pending.values()) {
      entry.reject(error);
    }
    pending.clear();
  }

  const openPromise = new Promise((resolve, reject) => {
    ws.once('open', resolve);
    ws.once('error', reject);
    ws.once('close', () => reject(new Error('CDP socket closed during handshake')));
  });

  ws.on('message', (buffer) => {
    let message;
    try {
      message = JSON.parse(String(buffer));
    } catch {
      return;
    }
    if (typeof message.id !== 'number') {
      return;
    }
    const entry = pending.get(message.id);
    if (!entry) {
      return;
    }
    pending.delete(message.id);
    if (message.error) {
      entry.reject(new Error(message.error.message || JSON.stringify(message.error)));
      return;
    }
    entry.resolve(message.result);
  });

  ws.on('error', (error) => rejectPending(error instanceof Error ? error : new Error(String(error))));
  ws.on('close', () => rejectPending(new Error('CDP socket closed')));

  await openPromise;

  async function send(method, params = {}, sessionId) {
    const id = nextId++;
    ws.send(JSON.stringify({ id, method, params, sessionId }));
    return new Promise((resolve, reject) => {
      pending.set(id, { resolve, reject });
    });
  }

  async function withSession(targetId, fn) {
    const { sessionId } = await send('Target.attachToTarget', { targetId, flatten: true });
    try {
      return await fn(sessionId);
    } finally {
      try {
        await send('Target.detachFromTarget', { sessionId });
      } catch {
        // The target may have navigated or detached itself; ignore cleanup failures.
      }
    }
  }

  return {
    baseUrl,
    token,
    async listTargets() {
      return fetchRelayJson('/json/list', token, baseUrl);
    },
    async createTarget(url) {
      return send('Target.createTarget', { url });
    },
    async withSession(targetId, fn) {
      return withSession(targetId, fn);
    },
    async evaluate(targetId, expression, { awaitPromise = false } = {}) {
      return withSession(targetId, async (sessionId) => {
        const result = await send(
          'Runtime.evaluate',
          { expression, returnByValue: true, awaitPromise },
          sessionId,
        );
        return result?.result?.value;
      });
    },
    async navigate(targetId, url) {
      return withSession(targetId, async (sessionId) => {
        await send('Page.enable', {}, sessionId);
        return send('Page.navigate', { url }, sessionId);
      });
    },
    async close() {
      try {
        ws.close();
      } catch {
        // ignore
      }
    },
  };
}
