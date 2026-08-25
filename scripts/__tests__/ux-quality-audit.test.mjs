import test from 'node:test';
import assert from 'node:assert/strict';

import { auditUxSource } from '../lib/ux-quality-audit.mjs';

test('ux audit does not treat TypeScript generics as JSX nesting in non-JSX files', () => {
  const source = `
    type Listener = () => void;
    const listeners = new Set<Listener>();

    async function api<T>(path: string): Promise<T> {
      return undefined as T;
    }

    export const get = <T>(path: string) => api<T>(path);
    export const post = <T>(path: string, body: unknown) => api<T>(path);
    export const put = <T>(path: string, body: unknown) => api<T>(path);
    export const patch = <T>(path: string, body: unknown) => api<T>(path);
    export const del = <T>(path: string) => api<T>(path);
  `;

  const findings = auditUxSource(source, '/tmp/client.ts');

  assert.equal(findings.some((finding) => finding.rule === 'deep-jsx-nesting'), false);
  assert.equal(findings.some((finding) => finding.rule === 'missing-loading-state'), false);
});

test('ux audit still requires loading affordances in JSX files that fetch data', () => {
  const source = `
    export function ReportPanel() {
      fetch('/api/report');
      return <section><h1>Report</h1></section>;
    }
  `;

  const findings = auditUxSource(source, '/tmp/ReportPanel.tsx');

  assert.equal(findings.some((finding) => finding.rule === 'missing-loading-state'), true);
});
