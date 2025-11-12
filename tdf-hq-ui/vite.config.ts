import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import { execSync } from 'node:child_process';

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

export default defineConfig({
  plugins: [react()],
  server: { port: 5173, host: true },
  define: {
    __APP_COMMIT__: JSON.stringify(uiCommit),
  },
});
