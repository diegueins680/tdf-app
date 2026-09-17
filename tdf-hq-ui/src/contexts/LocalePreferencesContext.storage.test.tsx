import { jest } from '@jest/globals';
import { act, Component, type ReactNode } from 'react';
import { createRoot } from 'react-dom/client';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { waitFor } from '@testing-library/dom';
import i18n from '../i18n';

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: null }),
}));
jest.unstable_mockModule('../api/preferences', () => ({
  Preferences: { get: jest.fn(), update: jest.fn() },
}));
jest.unstable_mockModule('../api/catalogs', () => ({
  Catalogs: { listPublicBatch: async () => ({ catalogs: [
    { catalog: { code: 'locales' }, items: [{ id: 'synthetic-es', code: 'es' }], defaults: [] },
    { catalog: { code: 'currencies' }, items: [{ id: 'synthetic-usd', code: 'USD' }], defaults: [] },
  ] }) },
}));
const { LocalePreferencesProvider, useLocalePreferences } = await import('./LocalePreferencesContext');
class ErrorBoundary extends Component<{ children: ReactNode }, { failed: boolean }> {
  state = { failed: false };
  static getDerivedStateFromError() { return { failed: true }; }
  render() { return this.state.failed ? <p>Preference crash</p> : this.props.children; }
}
function Probe() {
  const preferences = useLocalePreferences();
  return <p>{preferences.localeId}/{preferences.currencyId}</p>;
}

beforeAll(() => {
  (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
});
afterEach(() => { jest.restoreAllMocks(); });

it.each(['operations', 'access'] as const)('applies fetched catalog preferences with storage %s denied', async (denial) => {
  await i18n.changeLanguage('es');
  const deny = () => { throw new DOMException('Synthetic storage denial', 'SecurityError'); };
  if (denial === 'access') jest.spyOn(window, 'localStorage', 'get').mockImplementation(deny);
  else jest.spyOn(Storage.prototype, 'setItem').mockImplementation(deny);
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  try {
    await act(async () => {
      root.render(<ErrorBoundary><QueryClientProvider client={client}>
        <LocalePreferencesProvider><Probe /></LocalePreferencesProvider>
      </QueryClientProvider></ErrorBoundary>);
    });
    await waitFor(() => expect(container.textContent).toBe('synthetic-es/synthetic-usd'));
    expect(i18n.language).toBe('es');
  } finally {
    await act(async () => { root.unmount(); });
    container.remove();
    client.clear();
  }
});
