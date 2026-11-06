import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, useLocation } from 'react-router-dom';
import { fireEvent, getByLabelText, getByRole, waitFor } from '@testing-library/dom';
import i18n from '../i18n';

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: null, loading: false, login: jest.fn() }),
}));
const { default: ResetPasswordPage } = await import('./ResetPasswordPage');

function LocationProbe() {
  const location = useLocation();
  return <output data-testid="recovery-location">{location.pathname}{location.search}</output>;
}

describe('expired reset recovery', () => {
  let root: Root;
  let container: HTMLDivElement;
  let client: QueryClient;
  const originalFetch = globalThis.fetch;
  const actEnvironment = globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT?: boolean };
  const originalActEnvironment = actEnvironment.IS_REACT_ACT_ENVIRONMENT;

  beforeAll(() => { actEnvironment.IS_REACT_ACT_ENVIRONMENT = true; });
  afterAll(() => { actEnvironment.IS_REACT_ACT_ENVIRONMENT = originalActEnvironment; });

  beforeEach(() => {
    container = document.createElement('div');
    document.body.appendChild(container);
    root = createRoot(container);
    client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  });

  afterEach(async () => {
    await act(async () => root.unmount());
    client.clear();
    container.remove();
    globalThis.fetch = originalFetch;
  });

  it.each(['es', 'en'])('explains the rejected token and preserves the retry destination in %s', async locale => {
    await i18n.changeLanguage(locale);
    const fetchMock = jest.fn<typeof fetch>().mockResolvedValue({
      ok: false, status: 400,
      headers: { get: () => 'text/plain' },
      text: async () => 'Invalid or expired token',
    } as unknown as Response);
    globalThis.fetch = fetchMock;
    const destination = '/mis-clasificados?crear=clasificado';
    await act(async () => {
      root.render(
        <MemoryRouter initialEntries={[`/reset?token=synthetic-reset-token&redirect=${encodeURIComponent(destination)}`]}>
          <QueryClientProvider client={client}>
            <ResetPasswordPage />
            <LocationProbe />
          </QueryClientProvider>
        </MemoryRouter>,
      );
    });
    await act(async () => {
      fireEvent.input(getByLabelText(container, i18n.t('authEntry.newPassword'), { exact: true }), { target: { value: 'Synthetic-password42!' } });
      fireEvent.input(getByLabelText(container, i18n.t('authEntry.confirmPassword'), { exact: true }), { target: { value: 'Synthetic-password42!' } });
    });
    await act(async () => {
      fireEvent.click(getByRole(container, 'button', { name: i18n.t('authEntry.savePassword'), exact: true }));
    });
    await waitFor(() => {
      expect(getByRole(container, 'alert').textContent).toBe(i18n.t('authEntry.resetTokenExpired'));
    });
    expect(getByRole(container, 'alert').textContent).not.toContain('Invalid or expired token');
    expect(fetchMock).toHaveBeenCalledTimes(1);
    const requestBody = fetchMock.mock.calls[0]?.[1]?.body;
    if (typeof requestBody !== 'string') throw new Error('Reset confirmation must send a JSON string');
    expect(JSON.parse(requestBody)).toEqual({
      token: 'synthetic-reset-token', newPassword: 'Synthetic-password42!',
    });
    const link = getByRole(container, 'link', { name: i18n.t('authEntry.requestNewLink'), exact: true });
    const url = new URL(link.getAttribute('href')!, 'http://localhost');
    expect(url.pathname).toBe('/login');
    expect(url.searchParams.get('redirect')).toBe(destination);
    expect(url.searchParams.get('recover')).toBe('1');
    expect(url.searchParams.get('lang')).toBe(locale);
    expect(url.searchParams.has('token')).toBe(false);
    await act(async () => { fireEvent.click(link); });
    expect(container.querySelector('[data-testid="recovery-location"]')?.textContent).toBe(url.pathname + url.search);
  });
});
