import React, { act } from 'react';
import { createRoot } from 'react-dom/client';
import { useQueryClient } from '@tanstack/react-query';
import { jest } from '@jest/globals';

const catalogRead = jest.fn(async () => ({ catalogs: [], revision: 1 }));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: { listPublicBatch: catalogRead } }));
jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({ i18n: { language: 'es', resolvedLanguage: 'es' } }),
}));
jest.unstable_mockModule('../api/session', () => ({
  loadSessionSnapshot: () => new Promise(() => undefined),
  logoutSessionRequest: async () => undefined,
  reconcileOnboardingProgress: async () => ({ newlyCompleted: false, progress: { eligible: false } }),
}));
jest.unstable_mockModule('../analytics/posthog', () => ({
  getAnalyticsClient: () => ({ ready: false, capture() {}, identify() {}, reset() {} }),
}));
const { SessionProviders } = await import('./SessionProviders');
const { useThemeMode } = await import('../theme/AppThemeProvider');
const { useSession } = await import('./SessionContext');

function Probe() {
  const { mode } = useThemeMode();
  const { session } = useSession();
  const client = useQueryClient();
  return <span>{mode}:{session?.partyId ?? 'anonymous'}:{String(Boolean(client))}</span>;
}

test('mounts the actual production theme/query/session composition in StrictMode', async () => {
  (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  const originalMatchMedia = window.matchMedia;
  Object.defineProperty(window, 'matchMedia', { configurable: true, value: () => ({
    matches: false, media: '', onchange: null, addEventListener() {}, removeEventListener() {},
    addListener() {}, removeListener() {}, dispatchEvent() { return true; },
  }) });
  window.localStorage.clear(); window.sessionStorage.clear();
  const container = document.createElement('div'); document.body.appendChild(container);
  const root = createRoot(container);
  try {
    await act(async () => root.render(<React.StrictMode><SessionProviders><Probe /></SessionProviders></React.StrictMode>));
    await act(async () => { await new Promise(resolve => setTimeout(resolve, 30)); });
    expect(container.textContent).toBe('light:anonymous:true');
    expect(catalogRead).toHaveBeenCalled();
  } finally {
    await act(async () => root.unmount()); container.remove();
    Object.defineProperty(window, 'matchMedia', { configurable: true, value: originalMatchMedia });
  }
});
