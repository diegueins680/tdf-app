import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import { execSync } from 'node:child_process';
import { createRequire } from 'node:module';
import path from 'node:path';
import packageJson from './package.json';

function resolveGitSha() {
  const envKeys = [
    'VITE_UI_COMMIT',
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
    chunkSizeWarningLimit: 500,
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
          // React core — match the exact package dirs so react-router /
          // react-i18next / react-hook-form don't get swept in here.
          if (
            /\/node_modules\/(react|react-dom|scheduler|react-is)\//.test(id)
          ) {
            return 'react-vendor';
          }
          if (id.includes('react-router')) return 'router';
          if (id.includes('@tanstack')) return 'tanstack';
          if (id.includes('@fullcalendar/')) return 'fullcalendar';
          if (id.includes('@mui/icons-material')) return 'mui-icons';
          if (id.includes('@mui/x-date-pickers')) return 'mui-x';
          // MUI core + its Emotion styling engine + the small runtime helpers
          // MUI/Emotion pull in (clsx, popper, transition-group, etc.). Keep
          // them together so the `mui` chunk has no outgoing edge into the
          // catch-all `vendor` chunk — otherwise Rollup emits a circular
          // chunk (mui -> vendor -> mui) that hurts load ordering.
          if (
            id.includes('@mui/material')
            || id.includes('@mui/system')
            || id.includes('@mui/base')
            || id.includes('@mui/utils')
            || id.includes('@mui/private-theming')
            || id.includes('@mui/styled-engine')
            || id.includes('@emotion/')
            || id.includes('/node_modules/clsx/')
            || id.includes('/node_modules/@popperjs/')
            || id.includes('/node_modules/react-transition-group/')
            || id.includes('/node_modules/dom-helpers/')
            || id.includes('/node_modules/hoist-non-react-statics/')
            // Shared low-level runtime MUI/Emotion lean on. Co-locating here
            // (rather than the catch-all `vendor`) removes the only remaining
            // mui -> vendor edge, breaking the circular chunk. Other chunks
            // that also use these just gain a one-way edge into `mui`, which
            // is loaded on every route anyway.
            || id.includes('/node_modules/@babel/runtime/')
            || id.includes('/node_modules/prop-types/')
            || id.includes('/node_modules/stylis/')
          ) {
            return 'mui';
          }
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
