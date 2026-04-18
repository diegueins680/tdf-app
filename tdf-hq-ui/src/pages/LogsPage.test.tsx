import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import type { LogEntry } from '../api/admin';

const getLogsMock = jest.fn<(limit?: number) => Promise<LogEntry[]>>();
const clearLogsMock = jest.fn<() => Promise<void>>();

jest.unstable_mockModule('../api/admin', () => ({
  Admin: {
    getLogs: (limit?: number) => getLogsMock(limit),
    clearLogs: () => clearLogsMock(),
  },
}));

const { default: LogsPage } = await import('./LogsPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const waitForExpectation = async (assertion: () => void, attempts = 12) => {
  let lastError: unknown;
  for (let index = 0; index < attempts; index += 1) {
    try {
      assertion();
      return;
    } catch (error) {
      lastError = error;
      await act(async () => {
        await flushPromises();
      });
    }
  }
  throw lastError;
};

const renderPage = async (container: HTMLElement) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <QueryClientProvider client={qc}>
        <LogsPage />
      </QueryClientProvider>,
    );
    await flushPromises();
    await flushPromises();
  });

  return {
    cleanup: async () => {
      if (!root) return;
      await act(async () => {
        root?.unmount();
        await flushPromises();
      });
      root = null;
      qc.clear();
      document.body.removeChild(container);
    },
  };
};

const buildLog = (overrides: Partial<LogEntry> = {}): LogEntry => ({
  logTimestamp: '2030-01-02T03:04:05.000Z',
  logLevel: 'info',
  logMessage: 'Servidor listo',
  ...overrides,
});

const getButtonByAriaLabel = (root: ParentNode, labelText: string) => {
  const button = root.querySelector(`button[aria-label="${labelText}"]`);
  if (!(button instanceof HTMLButtonElement)) throw new Error(`Button not found: ${labelText}`);
  return button;
};

const clickButton = async (button: HTMLButtonElement) => {
  await act(async () => {
    button.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
  });
};

describe('LogsPage', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
    if (!window.matchMedia) {
      Object.defineProperty(window, 'matchMedia', {
        writable: true,
        value: () => ({
          matches: false,
          media: '',
          onchange: null,
          addListener: () => undefined,
          removeListener: () => undefined,
          addEventListener: () => undefined,
          removeEventListener: () => undefined,
          dispatchEvent: () => false,
        }),
      });
    }
  });

  beforeEach(() => {
    getLogsMock.mockReset();
    clearLogsMock.mockReset();
    getLogsMock.mockResolvedValue([]);
    clearLogsMock.mockResolvedValue(undefined);
  });

  it('replaces empty log-table chrome and destructive clear action with one first-run message', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getLogsMock).toHaveBeenCalledWith(100);
        expect(container.textContent).toContain('Logs del servidor');
        expect(container.textContent).toContain(
          'Todavia no hay logs disponibles. Esta vista mostrara eventos del servidor cuando exista el primer registro.',
        );
        expect(container.querySelector('[data-testid="server-logs-empty-state"]')).not.toBeNull();
        expect(container.querySelector('table')).toBeNull();
        expect(container.querySelector('button[aria-label="Vaciar logs"]')).toBeNull();
        expect(container.querySelector('button[aria-label="Refrescar logs"]')).not.toBeNull();
        expect(container.textContent).not.toContain('No logs available');
        expect(container.textContent).not.toContain('Timestamp');
        expect(container.textContent).not.toContain('Level');
        expect(container.textContent).not.toContain('Message');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the clear action available once there are logs to remove', async () => {
    getLogsMock.mockResolvedValue([buildLog()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('table')).not.toBeNull();
        expect(container.textContent).toContain('Servidor listo');
        expect(container.querySelector('[data-testid="server-logs-empty-state"]')).toBeNull();
        expect(container.querySelector('button[aria-label="Vaciar logs"]')).not.toBeNull();
      });

      await clickButton(getButtonByAriaLabel(container, 'Vaciar logs'));

      await waitForExpectation(() => {
        expect(clearLogsMock).toHaveBeenCalledTimes(1);
      });
    } finally {
      await cleanup();
    }
  });
});
