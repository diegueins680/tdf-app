import { jest } from '@jest/globals';
import { StrictMode } from 'react';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

interface InstagramAuthProbeState {
  status: 'idle' | 'authenticating' | 'ready' | 'error';
  error: string | null;
}

interface InstagramCallbackResult {
  ok: boolean;
  message?: string;
  returnTo?: string;
}

const buildInstagramAuthUrlMock = jest.fn<(state?: string) => string>(() => '');
const clearInstagramResultMock = jest.fn<() => void>();
const consumeInstagramStateMock = jest.fn<
  () => { state: string; returnTo?: string; issuedAt: number } | null
>(() => null);
const parseInstagramStateMock = jest.fn<(state: string | null) => Record<string, unknown> | null>(
  () => null,
);
const exchangeInstagramCodeMock = jest.fn<(code: string) => Promise<Record<string, unknown>>>(
  () => Promise.resolve({}),
);
const storeInstagramResultMock = jest.fn<(result: Record<string, unknown>) => void>();
const getStoredInstagramResultMock = jest.fn<() => Record<string, unknown> | null>(() => null);
const instagramConfigErrorMock = jest.fn<() => string | null>(() => null);

jest.unstable_mockModule('../services/instagramAuth', () => ({
  buildInstagramAuthUrl: buildInstagramAuthUrlMock,
  clearInstagramResult: clearInstagramResultMock,
  consumeInstagramState: consumeInstagramStateMock,
  exchangeInstagramCode: exchangeInstagramCodeMock,
  getStoredInstagramResult: getStoredInstagramResultMock,
  instagramConfigError: instagramConfigErrorMock,
  parseInstagramState: parseInstagramStateMock,
  storeInstagramResult: storeInstagramResultMock,
}));

const { useInstagramAuth, useInstagramCallback } = await import('./useInstagramAuth');

function AuthProbe({ returnTo }: { returnTo?: string }) {
  const { status, error, startAuth } = useInstagramAuth();
  return (
    <>
      <pre data-testid="auth-result">{JSON.stringify({ status, error })}</pre>
      <button type="button" data-testid="start-auth" onClick={() => startAuth(returnTo)}>
        Start auth
      </button>
    </>
  );
}

function CallbackProbe() {
  const result = useInstagramCallback();
  return <pre data-testid="callback-result">{JSON.stringify(result)}</pre>;
}

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const flushAll = async (cycles = 3) => {
  for (let index = 0; index < cycles; index += 1) {
    await act(async () => {
      await flushPromises();
    });
  }
};

const renderProbe = async (element: JSX.Element, strictMode = false) => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(strictMode ? <StrictMode>{element}</StrictMode> : element);
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

function readJson<T>(container: HTMLElement, selector: string): T {
  const node = container.querySelector(selector);
  if (!node) {
    throw new Error(`Node not found for selector: ${selector}`);
  }
  return JSON.parse(node.textContent ?? '{}') as T;
}

const readCallbackResult = (container: HTMLElement): InstagramCallbackResult =>
  readJson<InstagramCallbackResult>(container, '[data-testid="callback-result"]');

const readAuthResult = (container: HTMLElement): InstagramAuthProbeState =>
  readJson<InstagramAuthProbeState>(container, '[data-testid="auth-result"]');

const click = async (container: HTMLElement, selector: string) => {
  const button = container.querySelector(selector);
  if (!(button instanceof HTMLButtonElement)) {
    throw new Error(`Button not found for selector: ${selector}`);
  }
  await act(async () => {
    button.click();
    await flushPromises();
  });
};

describe('useInstagramCallback', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT =
      true;
  });

  beforeEach(() => {
    buildInstagramAuthUrlMock.mockReset().mockReturnValue('https://example.com/oauth');
    clearInstagramResultMock.mockReset();
    consumeInstagramStateMock.mockReset();
    parseInstagramStateMock.mockReset();
    exchangeInstagramCodeMock.mockReset();
    storeInstagramResultMock.mockReset();
    getStoredInstagramResultMock.mockReset().mockReturnValue(null);
    instagramConfigErrorMock.mockReset().mockReturnValue(null);
  });

  it('rejects callbacks when the browser-stored state is already missing', async () => {
    const issuedAt = Date.now();
    window.history.replaceState({}, '', '/oauth/instagram/callback?code=abc123&state=state-relay');

    consumeInstagramStateMock.mockReturnValue(null);
    parseInstagramStateMock.mockReturnValue({ returnTo: '/social/instagram', issuedAt });

    const { container, cleanup } = await renderProbe(<CallbackProbe />);
    await flushAll();

    expect(exchangeInstagramCodeMock).not.toHaveBeenCalled();
    expect(storeInstagramResultMock).not.toHaveBeenCalled();
    expect(readCallbackResult(container)).toEqual({
      ok: false,
      message: 'Estado de OAuth inválido o expirado.',
    });

    await cleanup();
  });

  it('deduplicates the callback exchange in StrictMode', async () => {
    const issuedAt = Date.now();
    window.history.replaceState({}, '', '/oauth/instagram/callback?code=strict123&state=strict-state');

    consumeInstagramStateMock
      .mockReturnValueOnce({ state: 'strict-state', returnTo: '/social/instagram', issuedAt })
      .mockReturnValue(null);
    parseInstagramStateMock.mockReturnValue({ returnTo: '/social/instagram', issuedAt });
    exchangeInstagramCodeMock.mockResolvedValue({ tokenType: 'bearer' });

    const { container, cleanup } = await renderProbe(<CallbackProbe />, true);
    await flushAll();

    expect(exchangeInstagramCodeMock).toHaveBeenCalledTimes(1);
    expect(storeInstagramResultMock).toHaveBeenCalledTimes(1);
    expect(readCallbackResult(container)).toEqual({ ok: true, returnTo: '/social/instagram' });

    await cleanup();
  });

  it('rejects reusing the same callback query after the stored state is consumed', async () => {
    const issuedAt = Date.now();
    window.history.replaceState({}, '', '/oauth/instagram/callback?code=retry123&state=retry-state');

    consumeInstagramStateMock
      .mockReturnValueOnce({ state: 'retry-state', returnTo: '/social/instagram', issuedAt })
      .mockReturnValue(null);
    exchangeInstagramCodeMock.mockResolvedValue({ tokenType: 'bearer' });

    const firstRender = await renderProbe(<CallbackProbe />);
    await flushAll();
    await firstRender.cleanup();

    const secondRender = await renderProbe(<CallbackProbe />);
    await flushAll();

    expect(exchangeInstagramCodeMock).toHaveBeenCalledTimes(1);
    expect(readCallbackResult(secondRender.container)).toEqual({
      ok: false,
      message: 'Estado de OAuth inválido o expirado.',
    });

    await secondRender.cleanup();
  });

  it('rejects parsed states that are too far in the future', async () => {
    const issuedAt = Date.now() + 2 * 60 * 1000;
    window.history.replaceState({}, '', '/oauth/instagram/callback?code=future123&state=future-state');

    consumeInstagramStateMock.mockReturnValue({ state: 'future-state', returnTo: '/social/instagram', issuedAt });

    const { container, cleanup } = await renderProbe(<CallbackProbe />);
    await flushAll();

    expect(exchangeInstagramCodeMock).not.toHaveBeenCalled();
    expect(readCallbackResult(container)).toEqual({
      ok: false,
      message: 'Estado de OAuth expirado.',
      returnTo: '/social/instagram',
    });

    await cleanup();
  });

  it('prefers detailed OAuth error messages from Meta callback params', async () => {
    const issuedAt = Date.now();
    window.history.replaceState(
      {},
      '',
      '/oauth/instagram/callback?error=access_denied&error_reason=user_denied&error_description=El%20usuario%20cancel%C3%B3&state=meta-denied',
    );

    consumeInstagramStateMock.mockReturnValue({ state: 'meta-denied', returnTo: '/social/instagram', issuedAt });

    const { container, cleanup } = await renderProbe(<CallbackProbe />);
    await flushAll();

    expect(exchangeInstagramCodeMock).not.toHaveBeenCalled();
    expect(readCallbackResult(container)).toEqual({
      ok: false,
      message: 'El usuario canceló',
      returnTo: '/social/instagram',
    });

    await cleanup();
  });
});

describe('useInstagramAuth', () => {
  beforeEach(() => {
    buildInstagramAuthUrlMock.mockReset().mockReturnValue('https://example.com/oauth');
    clearInstagramResultMock.mockReset();
    consumeInstagramStateMock.mockReset();
    parseInstagramStateMock.mockReset();
    exchangeInstagramCodeMock.mockReset();
    storeInstagramResultMock.mockReset();
    getStoredInstagramResultMock.mockReset().mockReturnValue(null);
    instagramConfigErrorMock.mockReset().mockReturnValue(null);
  });

  it('reports start-auth errors instead of keeping an invalid authenticating state', async () => {
    buildInstagramAuthUrlMock.mockImplementation(() => {
      throw new Error('OAuth init failed');
    });

    const { container, cleanup } = await renderProbe(<AuthProbe returnTo="/social/instagram" />);

    await click(container, '[data-testid="start-auth"]');

    expect(buildInstagramAuthUrlMock).toHaveBeenCalledWith('/social/instagram');
    expect(readAuthResult(container)).toEqual({
      status: 'error',
      error: 'OAuth init failed',
    });

    await cleanup();
  });
});
