import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { jest } from '@jest/globals';

const mockListPublicBatch = jest.fn();

jest.unstable_mockModule('../api/catalogs', () => ({
  Catalogs: {
    listPublicBatch: (...args: unknown[]) => mockListPublicBatch(...args),
  },
}));

jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({ i18n: { language: 'es', resolvedLanguage: 'es' } }),
}));

const { AppThemeProvider, useThemeMode } = await import('./AppThemeProvider');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));
const waitForCondition = async (condition: () => boolean) => {
  for (let attempt = 0; attempt < 20; attempt += 1) {
    if (condition()) return;
    await act(async () => {
      await new Promise<void>((resolve) => setTimeout(resolve, 5));
    });
  }
  throw new Error('Timed out waiting for theme catalog state');
};

function ThemeProbe() {
  const { mode, preference, preferenceId, toggleMode } = useThemeMode();
  return (
    <button type="button" data-mode={mode} data-preference={preference} data-preference-id={preferenceId} onClick={toggleMode}>
      Cambiar tema
    </button>
  );
}

describe('AppThemeProvider', () => {
  let darkModeMatches = false;
  let listeners: Set<(event: MediaQueryListEvent) => void>;
  let mountNode: HTMLDivElement;
  let root: Root;
  let queryClient: QueryClient;

  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    window.localStorage.clear();
    mockListPublicBatch.mockReset();
    mockListPublicBatch.mockResolvedValue({
      catalogs: [{
        catalog: { code: 'appearance-modes' },
        items: [
          { id: 'appearance-system', code: 'system', name: 'Sistema', active: true, workflowState: 'published' },
          { id: 'appearance-light', code: 'light', name: 'Claro', active: true, workflowState: 'published' },
          { id: 'appearance-dark', code: 'dark', name: 'Oscuro', active: true, workflowState: 'published' },
        ],
        defaults: [{
          entityId: 'appearance-system',
          scopeKind: 'appearance-mode',
          scopeId: 'global',
          version: 1,
        }],
      }],
      revision: 1,
      locale: 'es',
    });
    queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    listeners = new Set();
    Object.defineProperty(window, 'matchMedia', {
      configurable: true,
      value: (query: string) => ({
        matches: darkModeMatches,
        media: query,
        onchange: null,
        addEventListener: (_type: string, listener: (event: MediaQueryListEvent) => void) => listeners.add(listener),
        removeEventListener: (_type: string, listener: (event: MediaQueryListEvent) => void) => listeners.delete(listener),
        addListener: () => undefined,
        removeListener: () => undefined,
        dispatchEvent: () => true,
      }),
    });
    mountNode = document.createElement('div');
    document.body.appendChild(mountNode);
    root = createRoot(mountNode);
  });

  afterEach(async () => {
    await act(async () => {
      root.unmount();
      await flushPromises();
    });
    mountNode.remove();
    darkModeMatches = false;
  });

  it('follows live system changes while preserving the system preference', async () => {
    window.localStorage.setItem('tdf-hq-ui/theme-mode', 'system');
    await act(async () => {
      root.render(
        <QueryClientProvider client={queryClient}>
          <AppThemeProvider><ThemeProbe /></AppThemeProvider>
        </QueryClientProvider>,
      );
      await flushPromises();
    });

    const probe = mountNode.querySelector('button');
    await waitForCondition(() => probe?.dataset['preferenceId'] === 'appearance-system');
    expect(probe?.dataset['preference']).toBe('system');
    expect(probe?.dataset['preferenceId']).toBe('appearance-system');
    expect(probe?.dataset['mode']).toBe('light');

    darkModeMatches = true;
    await act(async () => {
      listeners.forEach((listener) => listener({ matches: true } as MediaQueryListEvent));
      await flushPromises();
    });

    expect(probe?.dataset['mode']).toBe('dark');
    expect(document.documentElement.style.colorScheme).toBe('dark');
  });

  it('turns the quick toggle into an explicit persisted choice', async () => {
    await act(async () => {
      root.render(
        <QueryClientProvider client={queryClient}>
          <AppThemeProvider><ThemeProbe /></AppThemeProvider>
        </QueryClientProvider>,
      );
      await flushPromises();
    });

    const probe = mountNode.querySelector<HTMLButtonElement>('button');
    await act(async () => {
      probe?.click();
      await flushPromises();
    });

    expect(probe?.dataset['mode']).toBe('dark');
    expect(probe?.dataset['preference']).toBe('dark');
    expect(window.localStorage.getItem('tdf-hq-ui/theme-mode')).toBe(
      JSON.stringify({ id: 'appearance-dark', code: 'dark' }),
    );
  });

  it('preserves no inactive selection and falls back to the published default', async () => {
    window.localStorage.setItem(
      'tdf-hq-ui/theme-mode',
      JSON.stringify({ id: 'appearance-dark', code: 'dark' }),
    );
    mockListPublicBatch.mockResolvedValue({
      catalogs: [{
        catalog: { code: 'appearance-modes' },
        items: [
          { id: 'appearance-system', code: 'system', name: 'Sistema', active: true, workflowState: 'published' },
          { id: 'appearance-light', code: 'light', name: 'Claro', active: true, workflowState: 'published' },
        ],
        defaults: [{
          entityId: 'appearance-system',
          scopeKind: 'appearance-mode',
          scopeId: 'global',
          version: 2,
        }],
      }],
      revision: 2,
      locale: 'es',
    });

    await act(async () => {
      root.render(
        <QueryClientProvider client={queryClient}>
          <AppThemeProvider><ThemeProbe /></AppThemeProvider>
        </QueryClientProvider>,
      );
      await flushPromises();
    });

    const probe = mountNode.querySelector('button');
    await waitForCondition(() => probe?.dataset['preferenceId'] === 'appearance-system');
    expect(probe?.dataset['preference']).toBe('system');
    expect(window.localStorage.getItem('tdf-hq-ui/theme-mode')).toBe(
      JSON.stringify({ id: 'appearance-system', code: 'system' }),
    );
  });
});
