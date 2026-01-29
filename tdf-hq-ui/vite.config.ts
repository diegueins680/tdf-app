import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import { execSync } from 'node:child_process';
import { createRequire } from 'node:module';
import path from 'node:path';
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
const require = createRequire(import.meta.url);
const muiMaterialRoot = path.dirname(require.resolve('@mui/material/package.json'));
const muiSystemRoot = (() => {
  try {
    return path.dirname(
      require.resolve('@mui/system/package.json', { paths: [muiMaterialRoot] }),
    );
  } catch {
    return path.dirname(require.resolve('@mui/system/package.json'));
  }
})();
const muiMaterialSubpathRegex = /^@mui\/material\/(.*)$/;
const muiMaterialSubpathReplacement = `${muiMaterialRoot}/$1/index.js`;
const muiSystemEntry = `${muiSystemRoot}/index.js`;
const muiSystemExactRegex = /^@mui\/system$/;
const muiSystemSubpathRegex = /^@mui\/system\/(.*)$/;
const muiSystemSubpathReplacement = `${muiSystemRoot}/$1/index.js`;

export default defineConfig({
  plugins: [react()],
  server: { port: 5173, host: true },
  resolve: {
    dedupe: ['react', 'react-dom'],
    alias: [
      // Ensure Vite resolves MUI deep imports to their ESM entry points.
      { find: muiMaterialSubpathRegex, replacement: muiMaterialSubpathReplacement },
      { find: muiSystemExactRegex, replacement: muiSystemEntry },
      { find: muiSystemSubpathRegex, replacement: muiSystemSubpathReplacement },
    ],
  },
  build: {
    chunkSizeWarningLimit: 900,
    rollupOptions: {
      output: {
        manualChunks(id) {
          if (!id.includes('node_modules')) {
            if (id.includes('/src/pages/')) {
              const parts = id.split('/src/pages/')[1]?.split('/');
              const page = parts?.[0];
              if (page) return `page-${page}`;
            }
            return;
          }
          if (id.includes('react-router')) return 'router';
          if (id.includes('@tanstack')) return 'tanstack';
          if (id.includes('@fullcalendar/')) return 'fullcalendar';
          if (id.includes('@mui/icons-material')) return 'mui-icons';
          if (id.includes('@mui/x-date-pickers')) return 'mui-x';
          if (id.includes('@mui/')) return 'mui';
          if (id.includes('@emotion/')) return 'emotion';
          if (id.includes('@hello-pangea/dnd')) return 'dnd';
          if (id.includes('luxon')) return 'luxon';
          if (id.includes('qrcode')) return 'qrcode';
          return 'vendor';
        },
      },
    },
  },
  optimizeDeps: {
    include: ['react', 'react-dom'],
  },
  define: {
    __APP_COMMIT__: JSON.stringify(uiCommit),
    __APP_VERSION__: JSON.stringify(uiVersion),
  },
});
