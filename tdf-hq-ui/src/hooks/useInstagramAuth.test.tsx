import { jest } from '@jest/globals';
import { StrictMode } from 'react';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

interface InstagramCallbackResult {
  ok: boolean;
  message?: string;
  returnTo?: string;
}

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

jest.unstable_mockModule('../services/instagramAuth', () => ({
  buildInstagramAuthUrl: jest.fn(() => ''),
  clearInstagramResult: jest.fn(),
  consumeInstagramState: consumeInstagramStateMock,
  exchangeInstagramCode: exchangeInstagramCodeMock,
  getStoredInstagramResult: jest.fn(() => null),
  instagramConfigError: jest.fn(() => null),
  parseInstagramState: parseInstagramStateMock,
  storeInstagramResult: storeInstagramResultMock,
}));

const { useInstagramCallback } = await import('./useInstagramAuth');

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

const renderProbe = async (strictMode = false) => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(strictMode ? <StrictMode><CallbackProbe /></StrictMode> : <CallbackProbe />);
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

const readResult = (container: HTMLElement): InstagramCallbackResult => {
  const node = container.querySelector('[data-testid="callback-result"]');
  if (!node) {
    throw new Error('Callback result node not found');
  }
  return JSON.parse(node.textContent ?? '{}') as InstagramCallbackResult;
};

describe('useInstagramCallback', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT =
      true;
  });

  beforeEach(() => {
    consumeInstagramStateMock.mockReset();
    parseInstagramStateMock.mockReset();
    exchangeInstagramCodeMock.mockReset();
    storeInstagramResultMock.mockReset();
  });

  it('accepts a valid parsed state when session storage was already consumed', async () => {
    const issuedAt = Date.now();
    window.history.replaceState({}, '', '/oauth/instagram/callback?code=abc123&state=state-relay');

    consumeInstagramStateMock.mockReturnValue(null);
    parseInstagramStateMock.mockReturnValue({ returnTo: '/social/instagram', issuedAt });
    exchangeInstagramCodeMock.mockResolvedValue({ tokenType: 'bearer' });

    const { container, cleanup } = await renderProbe();
    await flushAll();

    expect(exchangeInstagramCodeMock).toHaveBeenCalledTimes(1);
    expect(exchangeInstagramCodeMock).toHaveBeenCalledWith('abc123');
    expect(storeInstagramResultMock).toHaveBeenCalledTimes(1);
    expect(readResult(container)).toEqual({ ok: true, returnTo: '/social/instagram' });

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

    const { container, cleanup } = await renderProbe(true);
    await flushAll();

    expect(exchangeInstagramCodeMock).toHaveBeenCalledTimes(1);
    expect(storeInstagramResultMock).toHaveBeenCalledTimes(1);
    expect(readResult(container)).toEqual({ ok: true, returnTo: '/social/instagram' });

    await cleanup();
  });

  it('allows retrying the same callback query after the previous run settles', async () => {
    const issuedAt = Date.now();
    window.history.replaceState({}, '', '/oauth/instagram/callback?code=retry123&state=retry-state');

    consumeInstagramStateMock.mockReturnValue(null);
    parseInstagramStateMock.mockReturnValue({ returnTo: '/social/instagram', issuedAt });
    exchangeInstagramCodeMock.mockResolvedValue({ tokenType: 'bearer' });

    const firstRender = await renderProbe();
    await flushAll();
    await firstRender.cleanup();

    const secondRender = await renderProbe();
    await flushAll();
    await secondRender.cleanup();

    expect(exchangeInstagramCodeMock).toHaveBeenCalledTimes(2);
  });
});
