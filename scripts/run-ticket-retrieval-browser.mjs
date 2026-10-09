import { createServer } from 'vite';
import { spawn } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { disposablePostgresUrl } from './lib/disposable-postgres-url.mjs';

const target = process.env.TDF_TICKET_JOURNEY_API_ORIGIN ?? '';
if (!/^http:\/\/127\.0\.0\.1:\d+$/.test(target)) throw new Error('Owned loopback API required');
disposablePostgresUrl(process.env.TDF_TICKET_JOURNEY_DSN);
const root = fileURLToPath(new URL('../', import.meta.url));
// No .env or deployment keys. Every API response comes from the real Haskell server.
const server = await createServer({
  root: `${root}tdf-hq-ui`, configFile: `${root}tdf-hq-ui/vite.config.ts`, envFile: false, envPrefix: [],
  define: {
    'import.meta.env.VITE_API_BASE': JSON.stringify(''),
    'import.meta.env.VITE_POSTHOG_KEY': JSON.stringify(''),
    'import.meta.env.VITE_GOOGLE_CLIENT_ID': JSON.stringify(''),
  },
  server: { host: '127.0.0.1', port: 0, strictPort: true, proxy: {
    '^/(public|social-events|catalogs|session|login|logout|fans|directory|exchange-rates)(/|$)': { target },
  } },
});
try {
  await server.listen();
  const address = server.httpServer.address();
  const child = spawn(`${root}node_modules/.bin/playwright`, ['test', '--config=playwright.ticket-retrieval.config.mjs'], {
    cwd: root, stdio: 'inherit', env: { ...process.env, TDF_TICKET_JOURNEY_UI_ORIGIN: `http://127.0.0.1:${address.port}` },
  });
  process.exitCode = await new Promise((resolve, reject) => {
    child.once('error', reject); child.once('exit', code => resolve(code ?? 1));
  });
} finally { await server.close(); }
