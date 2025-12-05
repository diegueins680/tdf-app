import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import { execSync } from 'node:child_process';
import packageJson from './package.json';

function resolveGitSha() {
  const envKeys = [
    'VITE_UI_COMMIT',
    'KOYEB_GIT_SHA',
    'KOYEB_DEPLOYMENT_GIT_SHA',
    'SOURCE_COMMIT',
    'SOURCE_VERSION',
    'GIT_SHA',
  ];

  for (const key of envKeys) {
    const value = process.env[key];
    if (value && value.trim().length > 0) {
      return value.trim();
    }
  }

  try {
    return execSync('git rev-parse HEAD').toString().trim();
  } catch {
    return 'dev';
  }
}

const uiCommit = resolveGitSha();
const uiVersion = packageJson.version ?? '0.0.0';

export default defineConfig({
  plugins: [react()],
  server: { port: 5173, host: true },
  build: {
    chunkSizeWarningLimit: 900,
    rollupOptions: {
      output: {
        manualChunks(id) {
          if (!id.includes('node_modules')) return;
          if (id.includes('@mui')) return 'mui';
          if (id.includes('react-router')) return 'router';
          if (id.includes('@tanstack')) return 'tanstack';
          if (id.includes('luxon')) return 'luxon';
          if (id.includes('qrcode')) return 'qrcode';
          return 'vendor';
        },
      },
    },
  },
  define: {
    __APP_COMMIT__: JSON.stringify(uiCommit),
    __APP_VERSION__: JSON.stringify(uiVersion),
  },
});
