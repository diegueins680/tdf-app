import { createServer } from 'vite';
import { spawn } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const port = process.argv[2];
if (process.env.EVENT_RACI_DISPOSABLE_BROWSER_TEST !== '1'
  || !/^\d+$/.test(port ?? '') || Number(port) < 1 || Number(port) > 65535
  || !/^[a-f0-9]{64}$/.test(process.env.EVENT_RACI_TEST_CONTAINER ?? '')) {
  throw new Error('Owned disposable browser environment required');
}
const root = fileURLToPath(new URL('../', import.meta.url));
const target = `http://127.0.0.1:${port}`;
// Never load a checkout's deployment credentials or .env. Same-origin API calls only.
const server = await createServer({
  root: `${root}tdf-hq-ui`, configFile: `${root}tdf-hq-ui/vite.config.ts`, envFile: false,
  envPrefix: [],
  define: {
    'import.meta.env.VITE_API_BASE': JSON.stringify(''),
    'import.meta.env.VITE_POSTHOG_KEY': JSON.stringify(''),
    'import.meta.env.VITE_GOOGLE_CLIENT_ID': JSON.stringify(''),
  },
  server: { host: '127.0.0.1', port: 0, strictPort: true,
    proxy: { '^/session$': { target }, '^/event-operations/': { target } } },
});
try {
  await server.listen();
  const address = server.httpServer.address();
  if (!address || typeof address === 'string') throw new Error('Missing owned UI listener');
  const child = spawn(`${root}node_modules/.bin/playwright`,
    ['test', '--config=playwright.event-raci-browser.config.mjs'], {
      cwd: root, stdio: 'inherit', env: { ...process.env,
        EVENT_RACI_UI_ORIGIN: `http://127.0.0.1:${address.port}` },
    });
  process.exitCode = await new Promise((resolve, reject) => {
    child.once('error', reject); child.once('exit', code => resolve(code ?? 1));
  });
} finally { await server.close(); }
