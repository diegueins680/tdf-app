/// <reference types="vitest" />
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
const testConfig = {
    globals: true,
    environment: 'jsdom',
    setupFiles: './src/setupTests.ts',
    css: false,
};
export default defineConfig(() => {
    const config = {
        plugins: [react()],
        server: { port: 5173, host: true },
        preview: { port: 4173 },
        test: testConfig,
    };
    return config;
});
