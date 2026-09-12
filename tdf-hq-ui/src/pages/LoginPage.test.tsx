import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import { waitFor } from '@testing-library/dom';

const GOOGLE_CREDENTIAL =
  'e30.eyJlbWFpbCI6ImFuZHJlYUBleGFtcGxlLmNvbSIsIm5hbWUiOiJBbmRyZWEifQ.signature';
const GOOGLE_CONSENT_ERROR =
  'Accept the terms and privacy policy through the signup flow before creating a Google account';

const googleLoginRequestMock = jest.fn<(payload: Record<string, unknown>) => Promise<Record<string, unknown>>>();
const loginMock = jest.fn();
const signupResetMock = jest.fn();

jest.unstable_mockModule('../api/auth', () => ({
  googleLoginRequest: (payload: Record<string, unknown>) => googleLoginRequestMock(payload),
  loginRequest: jest.fn(),
  requestPasswordReset: jest.fn(),
  signupRequest: Object.assign(jest.fn(), { reset: signupResetMock }),
}));

jest.unstable_mockModule('../api/meta', () => ({
  Meta: { health: () => new Promise(() => undefined) },
}));

jest.unstable_mockModule('../api/fans', () => ({
  Fans: { listArtists: () => Promise.resolve([]) },
}));

jest.unstable_mockModule('../api/session', () => ({
  loadSessionSnapshot: () => Promise.resolve(null),
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: null, loading: false, login: loginMock }),
}));

jest.unstable_mockModule('../theme/AppThemeProvider', () => ({
  useThemeMode: () => ({ mode: 'dark', toggleMode: jest.fn() }),
}));

jest.unstable_mockModule('../analytics/useAnalytics', () => ({
  useAnalytics: () => ({ capture: jest.fn() }),
}));

jest.unstable_mockModule('../analytics/onboardingProgress', () => ({
  markWebSignupCompleted: jest.fn(),
}));

jest.unstable_mockModule('../utils/env', () => ({
  env: { read: (key: string) => (key === 'VITE_GOOGLE_CLIENT_ID' ? 'fictional-google-client' : undefined) },
}));

jest.unstable_mockModule('../utils/logger', () => ({
  logger: { log: jest.fn(), warn: jest.fn(), error: jest.fn() },
}));

jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({
    t: (key: string) => (key === 'login.signupDialog.title' ? 'Crear cuenta' : key),
  }),
}));

const { default: LoginPage, isGoogleSignupConsentRequiredError } = await import('./LoginPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const findButton = (name: string): HTMLButtonElement | null =>
  Array.from(document.querySelectorAll<HTMLButtonElement>('button'))
    .find((button) => button.textContent?.trim() === name) ?? null;

const renderLoginPage = async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  let root: Root | null = createRoot(container);
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false }, mutations: { retry: false } },
  });
  queryClient.setQueryData(['meta', 'health', 'login'], { status: 'ok' });
  queryClient.setQueryData(['signup', 'artists'], []);

  await act(async () => {
    root?.render(
      <QueryClientProvider client={queryClient}>
        <MemoryRouter initialEntries={['/login']}>
          <LoginPage />
        </MemoryRouter>
      </QueryClientProvider>,
    );
    await flushPromises();
    await flushPromises();
  });

  return async () => {
    if (!root) return;
    await act(async () => {
      root?.unmount();
      await flushPromises();
    });
    root = null;
    queryClient.clear();
    container.remove();
  };
};

describe('LoginPage Google signup consent flow', () => {
  let googleCallback: ((response: { credential?: string }) => void) | null;

  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    googleCallback = null;
    googleLoginRequestMock.mockReset();
    loginMock.mockReset();
    document.head.querySelectorAll('script[src="https://accounts.google.com/gsi/client"]').forEach((node) => node.remove());

    const script = document.createElement('script');
    script.src = 'https://accounts.google.com/gsi/client';
    script.dataset['loaded'] = 'true';
    document.head.appendChild(script);

    window.google = {
      accounts: {
        id: {
          initialize: (options) => {
            googleCallback = options['callback'] as (response: { credential?: string }) => void;
          },
          renderButton: (element, options) => {
            const button = document.createElement('button');
            const isSignup = options['text'] === 'signup_with';
            button.type = 'button';
            button.textContent = isSignup ? 'Google signup test button' : 'Google login test button';
            button.addEventListener('click', () => googleCallback?.({ credential: GOOGLE_CREDENTIAL }));
            element.replaceChildren(button);
          },
          prompt: jest.fn(),
        },
      },
    };
  });

  afterEach(() => {
    delete window.google;
    document.head.querySelectorAll('script[src="https://accounts.google.com/gsi/client"]').forEach((node) => node.remove());
    document.body.replaceChildren();
    window.history.replaceState({}, '', '/');
  });

  it('recognizes only the server consent precondition', () => {
    expect(isGoogleSignupConsentRequiredError(new Error(` ${GOOGLE_CONSENT_ERROR} `))).toBe(true);
    expect(isGoogleSignupConsentRequiredError(new Error('Invalid Google token'))).toBe(false);
    expect(isGoogleSignupConsentRequiredError(GOOGLE_CONSENT_ERROR)).toBe(false);
  });

  it('opens the consent-first signup flow directly for new Google users', async () => {
    const cleanup = await renderLoginPage();

    try {
      await waitFor(() => {
        expect(findButton('¿Primera vez? Crear cuenta con Google')).not.toBeNull();
      });
      await act(async () => {
        findButton('¿Primera vez? Crear cuenta con Google')?.click();
        await flushPromises();
      });

      const signupDialog = document.querySelector<HTMLElement>('[role="dialog"]');
      expect(signupDialog).not.toBeNull();
      expect(signupDialog?.querySelector(
        'input[aria-label="Acepto los términos y la política de privacidad"]',
      )).not.toBeNull();
      const existingAccountButton = findButton('Ya tengo una cuenta');
      expect(existingAccountButton).not.toBeNull();
      expect(googleLoginRequestMock).not.toHaveBeenCalled();

      await act(async () => {
        existingAccountButton?.click();
        await flushPromises();
      });
      await waitFor(() => {
        expect(document.querySelector('[role="dialog"]')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  }, 15_000);

  it('hands a new Google user into signup and retries with accepted versioned terms', async () => {
    googleLoginRequestMock
      .mockRejectedValueOnce(new Error(GOOGLE_CONSENT_ERROR))
      .mockResolvedValueOnce({
        token: 'fictional-session-token',
        partyId: 404,
        roles: ['Customer'],
        modules: [],
        accountCreated: true,
      });
    const cleanup = await renderLoginPage();

    try {
      await waitFor(() => {
        expect(findButton('¿Primera vez? Crear cuenta con Google')).not.toBeNull();
      });
      const googleLoginButton = findButton('Google login test button');
      expect(googleLoginButton).not.toBeNull();

      await act(async () => {
        googleLoginButton?.click();
        await flushPromises();
        await flushPromises();
      });

      const signupDialog = document.querySelector<HTMLElement>('[role="dialog"]');
      expect(signupDialog).not.toBeNull();
      expect(signupDialog?.textContent).toContain('Esta cuenta de Google todavía no está registrada en TDF.');
      expect(document.body.textContent).not.toContain(GOOGLE_CONSENT_ERROR);
      expect(googleLoginRequestMock).toHaveBeenNthCalledWith(1, { idToken: GOOGLE_CREDENTIAL });

      const termsCheckbox = signupDialog?.querySelector<HTMLInputElement>(
        'input[aria-label="Acepto los términos y la política de privacidad"]',
      );
      expect(termsCheckbox).not.toBeNull();
      await act(async () => {
        termsCheckbox?.click();
        await flushPromises();
      });

      const googleSignupButton = findButton('Google signup test button');
      expect(googleSignupButton).not.toBeNull();
      await act(async () => {
        googleSignupButton?.click();
        await flushPromises();
        await flushPromises();
      });

      expect(googleLoginRequestMock).toHaveBeenNthCalledWith(2, {
        idToken: GOOGLE_CREDENTIAL,
        marketingOptIn: false,
        termsAccepted: true,
        termsVersion: 'tdf-account-terms-v1',
      });
      expect(loginMock).toHaveBeenCalledWith(
        expect.objectContaining({ partyId: 404, apiToken: 'fictional-session-token' }),
        { remember: true },
      );
    } finally {
      await cleanup();
    }
  }, 15_000);
});
