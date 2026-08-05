import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

import { AppThemeProvider, useThemeMode } from './AppThemeProvider';

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

function ThemeProbe() {
  const { mode, preference, toggleMode } = useThemeMode();
  return (
    <button type="button" data-mode={mode} data-preference={preference} onClick={toggleMode}>
      Cambiar tema
    </button>
  );
}

describe('AppThemeProvider', () => {
  let darkModeMatches = false;
  let listeners: Set<(event: MediaQueryListEvent) => void>;
  let mountNode: HTMLDivElement;
  let root: Root;

  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    window.localStorage.clear();
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
      root.render(<AppThemeProvider><ThemeProbe /></AppThemeProvider>);
      await flushPromises();
    });

    const probe = mountNode.querySelector('button');
    expect(probe?.dataset['preference']).toBe('system');
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
      root.render(<AppThemeProvider><ThemeProbe /></AppThemeProvider>);
      await flushPromises();
    });

    const probe = mountNode.querySelector<HTMLButtonElement>('button');
    await act(async () => {
      probe?.click();
      await flushPromises();
    });

    expect(probe?.dataset['mode']).toBe('dark');
    expect(probe?.dataset['preference']).toBe('dark');
    expect(window.localStorage.getItem('tdf-hq-ui/theme-mode')).toBe('dark');
  });
});
