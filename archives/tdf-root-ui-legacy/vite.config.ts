
/// <reference types="vitest" />
import { defineConfig, type UserConfig as ViteUserConfig } from 'vite';
import react from '@vitejs/plugin-react';
import type { UserConfig as VitestUserConfig } from 'vitest/config';

const testConfig: VitestUserConfig['test'] = {
  globals: true,
  environment: 'jsdom',
  setupFiles: './src/setupTests.ts',
  css: false,
};

export default defineConfig(() => {
  const config: ViteUserConfig & { test: VitestUserConfig['test'] } = {
    plugins: [react()],
    server: { port: 5173, host: true },
    preview: { port: 4173 },
    test: testConfig,
  };

  return config;
});
