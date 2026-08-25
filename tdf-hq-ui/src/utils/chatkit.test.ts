import { jest } from '@jest/globals';

jest.unstable_mockModule('../api/client', () => ({
  post: jest.fn(),
}));

const {
  CHATKIT_SCRIPT_LOAD_TIMEOUT_MS,
  CHATKIT_SCRIPT_URL,
  isChatKitScriptReady,
  loadChatKitScript,
} = await import('./chatkit');

const SHORT_CHATKIT_TEST_TIMEOUT_MS = 1_000;

const removeChatKitScripts = () => {
  document
    .querySelectorAll(`script[src="${CHATKIT_SCRIPT_URL}"]`)
    .forEach((script) => script.remove());
};

describe('loadChatKitScript', () => {
  afterEach(() => {
    removeChatKitScripts();
    jest.useRealTimers();
    jest.restoreAllMocks();
  });

  it('uses the named default script load timeout', async () => {
    jest.useFakeTimers();
    const setTimeoutSpy = jest.spyOn(window, 'setTimeout');

    const loading = loadChatKitScript();
    const rejection = expect(loading).rejects.toThrow('ChatKit tardo demasiado en cargar.');

    expect(setTimeoutSpy).toHaveBeenCalledWith(expect.any(Function), CHATKIT_SCRIPT_LOAD_TIMEOUT_MS);
    expect(document.querySelector(`script[src="${CHATKIT_SCRIPT_URL}"]`)).not.toBeNull();

    jest.runOnlyPendingTimers();

    await rejection;
    expect(document.querySelector(`script[src="${CHATKIT_SCRIPT_URL}"]`)).toBeNull();
  });

  it('rejects invalid timeout overrides before appending the script', async () => {
    await expect(loadChatKitScript({ timeoutMs: 0 })).rejects.toThrow(
      'El timeout de carga de ChatKit debe ser un numero positivo en milisegundos.',
    );

    expect(document.querySelector(`script[src="${CHATKIT_SCRIPT_URL}"]`)).toBeNull();
  });

  it('injects the ChatKit web component script asynchronously', async () => {
    expect(isChatKitScriptReady()).toBe(false);

    const loading = loadChatKitScript({ timeoutMs: SHORT_CHATKIT_TEST_TIMEOUT_MS });
    const script = document.querySelector<HTMLScriptElement>(`script[src="${CHATKIT_SCRIPT_URL}"]`);

    expect(script).not.toBeNull();
    expect(script?.src).toBe(CHATKIT_SCRIPT_URL);
    expect(script?.async).toBe(true);

    if (!window.customElements.get('openai-chatkit')) {
      window.customElements.define('openai-chatkit', class extends HTMLElement {});
    }
    script?.dispatchEvent(new Event('load'));

    await expect(loading).resolves.toBeUndefined();
    expect(isChatKitScriptReady()).toBe(true);
  });
});
