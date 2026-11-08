import path from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { createRequire } from 'node:module';
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const ui = path.join(root, 'tdf-hq-ui');
const require = createRequire(path.join(ui, 'package.json'));
const { build } = await import(pathToFileURL(require.resolve('vite')).href);
await build({
  root: ui,
  configFile: path.join(ui, 'vite.config.ts'),
  build: {
    outDir: path.join(root, 'artifacts/torrent-audio-fixture/dist'),
    emptyOutDir: true,
    rollupOptions: { input: path.join(ui, 'browser-test/torrent-audio.html') },
  },
});
