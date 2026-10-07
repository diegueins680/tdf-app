import i18n from '../i18n';
import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, useLocation } from 'react-router-dom';
import { fireEvent, waitFor } from '@testing-library/dom';

const GOOGLE_CREDENTIAL =
  'e30.eyJlbWFpbCI6ImFuZHJlYUBleGFtcGxlLmNvbSIsIm5hbWUiOiJBbmRyZWEifQ.signature';
const GOOGLE_CONSENT_ERROR =
  'Accept the terms and privacy policy through the signup flow before creating a Google account';

const googleLoginRequestMock = jest.fn<(payload: Record<string, unknown>) => Promise<Record<string, unknown>>>();
const loginMock = jest.fn();
const signupResetMock = jest.fn();
const signupRequestMock = jest.fn<(payload: Record<string, unknown>) => Promise<Record<string, unknown>>>();
const analyticsCaptureMock = jest.fn();

jest.unstable_mockModule('../api/auth', () => ({
  googleLoginRequest: (payload: Record<string, unknown>) => googleLoginRequestMock(payload),
  loginRequest: jest.fn(),
  requestPasswordReset: jest.fn(),
  signupRequest: Object.assign(signupRequestMock, { reset: signupResetMock }),
}));

jest.unstable_mockModule('../api/meta', () => ({
  Meta: { health: () => new Promise(() => undefined) },
}));

jest.unstable_mockModule('../api/fans', () => ({
  Fans: { listArtists: () => Promise.resolve([]) },
}));

jest.unstable_mockModule('../api/session', () => ({
  loadSessionSnapshot: () => Promise.resolve(null),
  completeOnboardingProgress: jest.fn(),
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: null, loading: false, login: loginMock }),
}));

jest.unstable_mockModule('../theme/AppThemeProvider', () => ({
  useThemeMode: () => ({ mode: 'dark', toggleMode: jest.fn() }),
}));

jest.unstable_mockModule('../analytics/useAnalytics', () => ({
  useAnalytics: () => ({ capture: analyticsCaptureMock }),
}));

jest.unstable_mockModule('../utils/env', () => ({
  env: { read: (key: string) => (key === 'VITE_GOOGLE_CLIENT_ID' ? 'fictional-google-client' : undefined) },
}));

jest.unstable_mockModule('../utils/logger', () => ({
  logger: { log: jest.fn(), warn: jest.fn(), error: jest.fn() },
}));



const { default: LoginPage, isGoogleSignupConsentRequiredError } = await import('./LoginPage');

function RouteStateProbe() { const location = useLocation(); return <output data-testid="route-state">{JSON.stringify(location.state)}</output>; }

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const findButton = (name: string): HTMLButtonElement | null =>
  Array.from(document.querySelectorAll<HTMLButtonElement>('button'))
    .find((button) => button.textContent?.trim() === name) ?? null;

const LocationProbe = () => {
  const location = useLocation();
  return <output data-testid="location">{location.pathname}{location.search}</output>;
};

const renderLoginPage = async (initialEntry = '/login') => {
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
        <MemoryRouter initialEntries={[initialEntry]}>
          <LoginPage />
          <LocationProbe />
          <RouteStateProbe />
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

  beforeEach(async () => {
    await i18n.changeLanguage('es');
    googleCallback = null;
    googleLoginRequestMock.mockReset();
    signupRequestMock.mockReset();
    analyticsCaptureMock.mockReset();
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

  it.each(['es', 'en', 'fr', 'de', 'pt'])('opens policies in the supported authentication language for %s', async (locale) => {
    await i18n.changeLanguage(locale);
    const cleanup = await renderLoginPage('/login?signup=1');
    try {
      await waitFor(() => expect(document.querySelector('[role="dialog"]')).not.toBeNull());
      const suffix = locale === 'es' ? '-es' : '';
      expect(document.querySelector(`a[href="/account/terms${suffix}.html"]`)).not.toBeNull();
      expect(document.querySelector(`a[href="/account/privacy${suffix}.html"]`)).not.toBeNull();
    } finally { await cleanup(); }
  });

  it('recognizes only the server consent precondition', () => {
    expect(isGoogleSignupConsentRequiredError(new Error(` ${GOOGLE_CONSENT_ERROR} `))).toBe(true);
    expect(isGoogleSignupConsentRequiredError(new Error('Invalid Google token'))).toBe(false);
    expect(isGoogleSignupConsentRequiredError(GOOGLE_CONSENT_ERROR)).toBe(false);
  });

  it('retains an artist claim and rejects Google callbacks that cannot carry it', async () => {
    const cleanup = await renderLoginPage('/login?signup=1&intent=artist_profile&claimArtistId=42');
    try {
      await waitFor(() => expect(document.querySelector('[role="dialog"]')).not.toBeNull());
      const dialog = document.querySelector<HTMLElement>('[role="dialog"]');
      await act(async () => {
        dialog?.querySelector<HTMLInputElement>('input[aria-label="Acepto los términos y la política de privacidad"]')?.click();
        await flushPromises();
      });
      expect(dialog?.textContent).toContain('registrarte no te concede su propiedad');
      expect(findButton('Google signup test button')).toBeNull();
      expect(googleCallback).not.toBeNull();
      await act(async () => {
        googleCallback?.({ credential: GOOGLE_CREDENTIAL });
        await flushPromises();
      });
      expect(googleLoginRequestMock).not.toHaveBeenCalled();
      expect(loginMock).not.toHaveBeenCalled();
      expect(document.querySelector('[role="dialog"]')).toBe(dialog);
      // The unavailable selection warning proves the claim ID was retained;
      // clearing it would silently turn this into a new-profile signup.
      expect(dialog?.textContent).toContain('El perfil elegido ya no está disponible para reclamar.');
    } finally {
      await cleanup();
    }
  });

  it('opens an email-and-password-only signup with clickwrap terms and an ungated Google option', async () => {
    const cleanup = await renderLoginPage();

    try {
      await waitFor(() => {
        expect(findButton('Crear cuenta general')).not.toBeNull();
      });
      // The login card no longer offers a second "first time? create with
      // Google" entry; the Google button itself handles first-time users.
      expect(findButton('¿Primera vez? Crear cuenta con Google')).toBeNull();
      await act(async () => {
        findButton('Crear cuenta general')?.click();
        await flushPromises();
      });

      const signupDialog = document.querySelector<HTMLElement>('[role="dialog"]');
      expect(signupDialog).not.toBeNull();
      // Only email and password are asked for: no name, phone or consent checkbox.
      expect(signupDialog?.querySelector('[name="givenName"]')).toBeNull();
      expect(signupDialog?.querySelector('[name="familyName"]')).toBeNull();
      expect(signupDialog?.querySelector('input[type="checkbox"]')).toBeNull();
      expect(signupDialog?.querySelector('input[name="email"]')).not.toBeNull();
      expect(signupDialog?.querySelector('input[name="newPassword"]')).not.toBeNull();
      // Terms are acknowledged next to the create button, with links.
      const createButton = findButton('Crear e ingresar');
      expect(createButton?.disabled).toBe(false);
      expect(createButton?.getAttribute('aria-describedby')).toBe('signup-consent-notice');
      const notice = document.getElementById('signup-consent-notice');
      expect(notice?.textContent).toContain('Al crear tu cuenta aceptas');
      expect(notice?.querySelectorAll('a')).toHaveLength(2);
      // Google signup is available immediately, without a consent gate.
      expect(findButton('Google signup test button')).not.toBeNull();
      expect(googleLoginRequestMock).not.toHaveBeenCalled();

      const existingAccountButton = findButton('Ya tengo una cuenta');
      expect(existingAccountButton).not.toBeNull();
      // Complete the real Dialog exit transition deterministically instead of
      // racing its timer against waitFor's wall-clock deadline on a busy host.
      jest.useFakeTimers();
      try {
        await act(async () => {
          existingAccountButton?.click();
        });
        await act(async () => {
          await jest.runOnlyPendingTimersAsync();
        });
        expect(document.querySelector('[role="dialog"]')).toBeNull();
      } finally {
        jest.useRealTimers();
      }
    } finally {
      await cleanup();
    }
  }, 15_000);

  it('offers one-tap Google account creation with the same credential when no TDF account exists', async () => {
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
        expect(findButton('Google login test button')).not.toBeNull();
      });
      expect(document.body.textContent).toContain('al continuar con Google se crea tu cuenta');

      await act(async () => {
        findButton('Google login test button')?.click();
        await flushPromises();
        await flushPromises();
      });

      const choiceDialog = document.querySelector<HTMLElement>('[role="dialog"]');
      expect(choiceDialog?.textContent).toContain('Crea tu cuenta con Google');
      // Connecting an existing password account remains available.
      expect(choiceDialog?.textContent).toContain('¿Ya tienes una cuenta TDF con contraseña?');
      expect(choiceDialog?.querySelector('input[autocomplete="current-password"]')).not.toBeNull();
      expect(document.body.textContent).not.toContain(GOOGLE_CONSENT_ERROR);
      expect(googleLoginRequestMock).toHaveBeenNthCalledWith(1, { idToken: GOOGLE_CREDENTIAL });

      await act(async () => {
        findButton('Crear mi cuenta con Google')?.click();
        await flushPromises();
        await flushPromises();
      });

      // The credential from the single Google interaction is reused: no
      // second popup and no extra form.
      expect(googleLoginRequestMock).toHaveBeenNthCalledWith(2, {
        idToken: GOOGLE_CREDENTIAL,
        marketingOptIn: false,
        termsAccepted: true,
        termsVersion: 'tdf-account-terms-v1',
        createNewAccount: true,
      });
      expect(loginMock).toHaveBeenCalledWith(
        expect.objectContaining({ partyId: 404, apiToken: 'fictional-session-token' }),
        { remember: true },
      );
      expect(analyticsCaptureMock).toHaveBeenCalledWith('signup_completed', expect.objectContaining({ method: 'google' }));
      expect(document.querySelector('[data-testid="route-state"]')?.textContent).toContain('"mobileInvitation":true');
      expect(document.querySelector('[data-testid="location"]')?.textContent).toBe('/fans');
      expect(analyticsCaptureMock).not.toHaveBeenCalledWith('onboarding_completed', expect.anything());
    } finally {
      await cleanup();
    }
  }, 15_000);

  it.each([
    ['/login', null],
    ['/login?signup=1&intent=artist_profile&claimArtistId=42', '/artista/crear?claimArtistId=42'],
  ])('signs up with only email and password from %s', async (entry, claimTarget) => {
    signupRequestMock.mockResolvedValueOnce({
      token: 'fictional-password-session', partyId: 405, roles: ['Customer'], modules: [],
    });
    const cleanup = await renderLoginPage(entry);
    try {
      await act(async () => {
        if (!claimTarget) findButton('Crear cuenta general')?.click();
        await flushPromises();
      });
      const dialog = document.querySelector<HTMLElement>('[role="dialog"]')!;
      // Invalid input is reported inline next to the field, without a request.
      await act(async () => {
        fireEvent.change(dialog.querySelector('[name="email"]')!, { target: { value: 'andrea@example' } });
        fireEvent.change(dialog.querySelector('[name="newPassword"]')!, { target: { value: 'short' } });
      });
      await act(async () => {
        findButton('Crear e ingresar')?.click();
        await flushPromises();
      });
      expect(signupRequestMock).not.toHaveBeenCalled();
      expect(dialog.textContent).toContain('Escribe un correo válido');
      expect(dialog.querySelector('[name="email"]')?.getAttribute('aria-invalid')).toBe('true');
      expect(document.activeElement).toBe(dialog.querySelector('[name="email"]'));

      await act(async () => {
        fireEvent.change(dialog.querySelector('[name="email"]')!, { target: { value: 'andrea@example.com' } });
      });
      await act(async () => {
        findButton('Crear e ingresar')?.click();
        await flushPromises();
      });
      expect(signupRequestMock).not.toHaveBeenCalled();
      expect(dialog.textContent).toContain('La contraseña necesita al menos 8 caracteres.');
      expect(dialog.querySelector('[name="newPassword"]')?.getAttribute('aria-invalid')).toBe('true');

      await act(async () => {
        fireEvent.change(dialog.querySelector('[name="newPassword"]')!, { target: { value: 'fictional-password-42' } });
      });
      await act(async () => {
        findButton('Crear e ingresar')?.click();
        await flushPromises();
      });
      // The display name starts from the email and can be changed later.
      expect(signupRequestMock).toHaveBeenCalledWith(expect.objectContaining({
        firstName: 'Andrea', lastName: '', email: 'andrea@example.com', termsAccepted: true,
        termsVersion: 'tdf-account-terms-v1', marketingOptIn: false,
      }), expect.objectContaining({ client: expect.anything() }));
      expect(signupRequestMock.mock.calls[0]?.[0]).not.toHaveProperty('phone', expect.anything());
      expect(loginMock).toHaveBeenCalledWith(
        expect.objectContaining({ partyId: 405, apiToken: 'fictional-password-session' }),
        { remember: true },
      );
      expect(signupRequestMock.mock.calls[0]?.[0]).not.toHaveProperty('claimArtistId');
      expect(document.querySelector('[data-testid="location"]')?.textContent).toBe(claimTarget ?? '/fans');
      expect(analyticsCaptureMock).toHaveBeenCalledWith('signup_completed', expect.objectContaining({ method: 'password' }));
      expect(analyticsCaptureMock).not.toHaveBeenCalledWith('first_value_completed', expect.anything());
      expect(analyticsCaptureMock).not.toHaveBeenCalledWith('onboarding_completed', expect.anything());
    } finally {
      await cleanup();
    }
  }, 15_000);
  it('focuses the login identifier when the initial page is still idle', async () => {
    const frames: FrameRequestCallback[] = [];
    const requestFrame = jest.spyOn(window, 'requestAnimationFrame').mockImplementation(callback => {
      frames.push(callback);
      return frames.length;
    });
    const cleanup = await renderLoginPage();
    try {
      expect(document.activeElement).toBe(document.body);
      expect(frames.length).toBeGreaterThan(0);
      await act(async () => { frames.splice(0).forEach(frame => frame(0)); });
      expect(document.activeElement).toBe(document.querySelector('input[autocomplete="username"]'));
    } finally {
      requestFrame.mockRestore();
      await cleanup();
    }
  });

  it('does not focus the background identifier while an opening dialog has not taken focus', async () => {
    const frames: FrameRequestCallback[] = [];
    const requestFrame = jest.spyOn(window, 'requestAnimationFrame').mockImplementation(callback => {
      frames.push(callback);
      return frames.length;
    });
    const cleanup = await renderLoginPage('/login?recover=1');
    try {
      expect(document.querySelector('[role="dialog"]')).not.toBeNull();
      const identifier = document.querySelector<HTMLInputElement>('input[autocomplete="username"]')!;
      const focusIdentifier = jest.spyOn(identifier, 'focus');
      await act(async () => {
        (document.activeElement as HTMLElement).blur();
        expect(document.activeElement).toBe(document.body);
        frames.splice(0).forEach(frame => frame(0));
        expect(focusIdentifier).not.toHaveBeenCalled();
      });
      focusIdentifier.mockRestore();
    } finally {
      requestFrame.mockRestore();
      await cleanup();
    }
  });

  it.each(['/login?recover=1', '/login?signup=1', '/login'])(
    'does not let delayed initial autofocus steal the current interaction at %s',
    async (entry) => {
      const frames: FrameRequestCallback[] = [];
      const requestFrame = jest.spyOn(window, 'requestAnimationFrame').mockImplementation(callback => {
        frames.push(callback);
        return frames.length;
      });
      const cleanup = await renderLoginPage(entry);
      try {
        const field = entry.includes('recover')
          ? document.querySelector<HTMLInputElement>('[role="dialog"] input[name="email"]')
          : entry.includes('signup')
            ? document.querySelector<HTMLInputElement>('[role="dialog"] input[name="email"]')
            : document.querySelector<HTMLInputElement>('input[autocomplete="current-password"]');
        expect(field).not.toBeNull();
        await act(async () => { field?.focus(); });
        expect(document.activeElement).toBe(field);
        expect(frames.length).toBeGreaterThan(0);
        await act(async () => { frames.splice(0).forEach(frame => frame(0)); });
        expect(document.activeElement).toBe(field);
      } finally {
        requestFrame.mockRestore();
        await cleanup();
      }
    },
  );

  it('unmounts a dismissed recovery dialog without waiting for an exit animation', async () => {
    const cleanup = await renderLoginPage('/login?recover=1&redirect=%2Ffans&lang=es');
    const dialog = () => document.querySelector('[role="dialog"][aria-labelledby="login-reset-dialog-title"]');
    try {
      await waitFor(() => expect(dialog()).not.toBeNull());
      for (let attempt = 0; attempt < 2; attempt += 1) {
        expect(findButton('Cerrar')).not.toBeNull();
        await act(async () => { findButton('Cerrar')?.click(); await flushPromises(); });
        expect(dialog()).toBeNull();
        if (attempt === 0) {
          await act(async () => { findButton('Recuperar acceso')?.click(); await flushPromises(); });
          expect(dialog()).not.toBeNull();
        }
      }
    } finally { await cleanup(); }
  });

  it('connects an existing account only after explicit credential submission', async () => {
    googleLoginRequestMock.mockRejectedValueOnce(new Error(GOOGLE_CONSENT_ERROR))
      .mockResolvedValueOnce({ token: 'fictional-session', partyId: 42, roles: [], modules: [], accountCreated: false });
    const cleanup = await renderLoginPage();
    try {
      await waitFor(() => expect(findButton('Google login test button')).not.toBeNull());
      await act(async () => { findButton('Google login test button')?.click(); await flushPromises(); });
      await waitFor(() => expect(findButton('Conectar Google')).not.toBeNull());
      const dialog = document.querySelector<HTMLElement>('[role="dialog"]');
      const setInput = (selector: string, value: string) => {
        const input = dialog?.querySelector<HTMLInputElement>(selector);
        expect(input).not.toBeNull();
        Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value')?.set?.call(input, value);
        input?.dispatchEvent(new Event('input', { bubbles: true }));
      };
      await act(async () => {
        setInput('input[autocomplete="username"]', 'existing-user');
        setInput('input[autocomplete="current-password"]', 'fictional-password');
        await flushPromises();
      });
      await act(async () => { findButton('Conectar Google')?.click(); await flushPromises(); });
      await waitFor(() => expect(googleLoginRequestMock).toHaveBeenLastCalledWith({
        idToken: GOOGLE_CREDENTIAL, linkAccount: { username: 'existing-user', password: 'fictional-password' },
      }));
      expect(loginMock).toHaveBeenCalledWith(expect.objectContaining({ partyId: 42 }), { remember: true });
    } finally { await cleanup(); }
  });

});
