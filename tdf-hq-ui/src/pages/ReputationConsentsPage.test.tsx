import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';

let featureFlags: string[] = [];
let resolveConsents: ((items: unknown[]) => void) | undefined;
const getMyConsents = jest.fn(() => new Promise<unknown[]>((resolve) => { resolveConsents = resolve; }));
const updateMyConsents = jest.fn();

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: { featureFlags } }),
  getStoredSessionToken: () => null,
}));
jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({
  useLocalePreferences: () => ({ locale: 'es' }),
}));
jest.unstable_mockModule('../api/reputation', () => ({
  Reputation: { getMyConsents, updateMyConsents },
}));
jest.unstable_mockModule('../api/directory', () => ({
  Directory: { setAgeAssurance: jest.fn() },
}));

const { default: ReputationConsentsPage } = await import('./ReputationConsentsPage');

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

async function renderPage() {
  const container = document.createElement('div');
  document.body.appendChild(container);
  let root: Root | null = createRoot(container);
  await act(async () => {
    root?.render(<MemoryRouter><ReputationConsentsPage /></MemoryRouter>);
  });
  return {
    container,
    async cleanup() {
      await act(async () => root?.unmount());
      root = null;
      container.remove();
    },
  };
}

describe('ReputationConsentsPage', () => {
  beforeEach(() => {
    featureFlags = [];
    getMyConsents.mockClear();
    updateMyConsents.mockReset();
    resolveConsents = undefined;
  });

  it('does not allow consent changes before the server state is loaded', async () => {
    const view = await renderPage();
    try {
      expect(view.container.querySelectorAll('input[type="checkbox"]')).toHaveLength(4);
      expect([...view.container.querySelectorAll<HTMLInputElement>('input[type="checkbox"]')].every((input) => input.disabled)).toBe(true);
    } finally {
      await view.cleanup();
    }
  });

  it('keeps new grants disabled while the pilot flag is off after loading', async () => {
    const view = await renderPage();
    try {
      await act(async () => { resolveConsents?.([]); });
      expect(view.container.textContent).toContain('todavía no está habilitado');
      expect([...view.container.querySelectorAll<HTMLInputElement>('input[type="checkbox"]')].every((input) => input.disabled)).toBe(true);
    } finally {
      await view.cleanup();
    }
  });
});
