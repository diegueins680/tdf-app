import { jest } from '@jest/globals';
import { StrictMode, act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

interface DriveCallbackResult {
  ok: boolean;
  message?: string;
  returnTo?: string;
}

const buildAuthUrlMock = jest.fn<(state?: string) => Promise<string>>(() => Promise.resolve(''));
const clearTokenMock = jest.fn<() => void>();
const driveConfigErrorMock = jest.fn<() => string | null>(() => null);
const consumeDriveStateMock = jest.fn<() => string | null>(() => null);
const ensureAccessTokenMock = jest.fn<() => Promise<Record<string, unknown>>>(() => Promise.resolve({}));
const exchangeCodeForTokenMock = jest.fn<(code: string) => Promise<Record<string, unknown>>>(() => Promise.resolve({}));
const getStoredTokenMock = jest.fn<() => Record<string, unknown> | null>(() => null);
const parseDriveStateMock = jest.fn<
  (state: string | null | undefined) => { nonce: string; returnTo?: string } | null
>(() => null);
const storeTokenMock = jest.fn<(token: Record<string, unknown>) => void>();

jest.unstable_mockModule('../services/googleDrive', () => ({
  buildAuthUrl: buildAuthUrlMock,
  clearToken: clearTokenMock,
  consumeDriveState: consumeDriveStateMock,
  driveConfigError: driveConfigErrorMock,
  ensureAccessToken: ensureAccessTokenMock,
  exchangeCodeForToken: exchangeCodeForTokenMock,
  getStoredToken: getStoredTokenMock,
  parseDriveState: parseDriveStateMock,
  storeToken: storeTokenMock,
}));

const { useGoogleDriveCallback } = await import('./useGoogleDriveAuth');

function CallbackProbe() {
  const result = useGoogleDriveCallback();
  return <pre data-testid="callback-result">{JSON.stringify(result)}</pre>;
}

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const renderProbe = async (strictMode = false) => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  let root: Root | null = createRoot(container);
  const element = strictMode ? (
    <StrictMode>
      <CallbackProbe />
    </StrictMode>
  ) : (
    <CallbackProbe />
  );

  await act(async () => {
    root?.render(element);
    await flushPromises();
  });

  return {
    container,
    cleanup: async () => {
      if (!root) return;
      await act(async () => {
        root?.unmount();
        await flushPromises();
      });
      root = null;
      document.body.removeChild(container);
    },
  };
};

const readCallbackResult = (container: HTMLElement): DriveCallbackResult => {
  const node = container.querySelector('[data-testid="callback-result"]');
  if (!node) throw new Error('Callback result node not found');
  return JSON.parse(node.textContent ?? '{}') as DriveCallbackResult;
};

describe('useGoogleDriveCallback', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    buildAuthUrlMock.mockReset().mockResolvedValue('https://accounts.google.com/o/oauth2/v2/auth');
    clearTokenMock.mockReset();
    driveConfigErrorMock.mockReset().mockReturnValue(null);
    consumeDriveStateMock.mockReset().mockReturnValue(null);
    ensureAccessTokenMock.mockReset().mockResolvedValue({});
    exchangeCodeForTokenMock.mockReset().mockResolvedValue({ accessToken: 'token' });
    getStoredTokenMock.mockReset().mockReturnValue(null);
    parseDriveStateMock.mockReset().mockReturnValue(null);
    storeTokenMock.mockReset();
  });

  it('rejects callbacks whose state is not present in browser storage', async () => {
    window.history.replaceState({}, '', '/oauth/google/callback?code=drive123&state=opaque-state');

    consumeDriveStateMock.mockReturnValue(null);
    parseDriveStateMock.mockReturnValue({ nonce: 'opaque-state', returnTo: '/drive' });

    const { container, cleanup } = await renderProbe();
    await act(async () => {
      await flushPromises();
    });

    expect(exchangeCodeForTokenMock).not.toHaveBeenCalled();
    expect(readCallbackResult(container)).toEqual({
      ok: false,
      message: 'Estado de OAuth inválido o expirado.',
    });

    await cleanup();
  });

  it('uses the stored state and preserves return paths containing percent characters', async () => {
    window.history.replaceState({}, '', '/oauth/google/callback?code=drive456&state=stored-state');

    consumeDriveStateMock.mockReturnValue('stored-state');
    parseDriveStateMock.mockReturnValue({ nonce: 'stored-nonce', returnTo: '/exports/100%ready' });
    exchangeCodeForTokenMock.mockResolvedValue({ accessToken: 'token' });

    const { container, cleanup } = await renderProbe();
    await act(async () => {
      await flushPromises();
    });

    expect(exchangeCodeForTokenMock).toHaveBeenCalledWith('drive456');
    expect(storeTokenMock).toHaveBeenCalledWith({ accessToken: 'token' });
    expect(readCallbackResult(container)).toEqual({
      ok: true,
      returnTo: '/exports/100%ready',
    });

    await cleanup();
  });

  it('deduplicates the callback exchange in StrictMode', async () => {
    window.history.replaceState({}, '', '/oauth/google/callback?code=strict123&state=strict-state');

    consumeDriveStateMock.mockReturnValueOnce('strict-state').mockReturnValue(null);
    parseDriveStateMock.mockReturnValue({ nonce: 'strict-nonce', returnTo: '/drive/files' });
    exchangeCodeForTokenMock.mockResolvedValue({ accessToken: 'token' });

    const { container, cleanup } = await renderProbe(true);
    await act(async () => {
      await flushPromises();
    });

    expect(exchangeCodeForTokenMock).toHaveBeenCalledTimes(1);
    expect(storeTokenMock).toHaveBeenCalledTimes(1);
    expect(readCallbackResult(container)).toEqual({
      ok: true,
      returnTo: '/drive/files',
    });

    await cleanup();
  });
});
