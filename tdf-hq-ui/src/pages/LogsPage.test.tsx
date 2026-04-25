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

const hasTableHeader = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('th')).some((cell) => (cell.textContent ?? '').trim() === labelText);

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
          'Todavia no hay logs disponibles. Esta vista se actualiza automaticamente y mostrara filtros cuando exista el primer registro.',
        );
        expect(container.querySelector('[data-testid="server-logs-empty-state"]')).not.toBeNull();
        expect(container.querySelector('table')).toBeNull();
        expect(container.querySelector('button[aria-label="Vaciar logs"]')).toBeNull();
        expect(container.querySelector('button[aria-label="Refrescar logs"]')).toBeNull();
        expect(container.textContent).not.toContain('Limite');
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
        expect(container.textContent).toContain('Actualizacion automatica cada 5 segundos.');
        expect(container.querySelector('[data-testid="server-logs-empty-state"]')).toBeNull();
        expect(container.querySelector('[data-testid="server-logs-shared-level-summary"]')).toBeNull();
        expect(container.textContent).toContain('Limite');
        expect(hasTableHeader(container, 'Fecha y hora')).toBe(true);
        expect(hasTableHeader(container, 'Nivel')).toBe(true);
        expect(hasTableHeader(container, 'Mensaje')).toBe(true);
        expect(hasTableHeader(container, 'Timestamp')).toBe(false);
        expect(hasTableHeader(container, 'Level')).toBe(false);
        expect(container.querySelector('button[aria-label="Refrescar logs"]')).toBeNull();
        expect(container.querySelector('button[aria-label="Vaciar logs"]')).not.toBeNull();
        expect(container.textContent).toContain('Info');
      });

      await clickButton(getButtonByAriaLabel(container, 'Vaciar logs'));

      await waitForExpectation(() => {
        expect(clearLogsMock).toHaveBeenCalledTimes(1);
      });
    } finally {
      await cleanup();
    }
  });

  it('summarizes one shared level once and restores the level column when logs diverge again', async () => {
    getLogsMock.mockResolvedValue([
      buildLog({
        logTimestamp: '2030-01-02T03:04:05.000Z',
        logMessage: 'Servidor listo',
      }),
      buildLog({
        logTimestamp: '2030-01-02T03:05:05.000Z',
        logMessage: 'Worker conectado',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('table')).not.toBeNull();
        expect(container.querySelector('[data-testid="server-logs-shared-level-summary"]')?.textContent?.trim()).toBe(
          'Mostrando un solo nivel: Info. La columna volverá cuando esta vista mezcle niveles distintos.',
        );
        expect(hasTableHeader(container, 'Fecha y hora')).toBe(true);
        expect(hasTableHeader(container, 'Nivel')).toBe(false);
        expect(hasTableHeader(container, 'Mensaje')).toBe(true);
        expect(container.textContent).toContain('Servidor listo');
        expect(container.textContent).toContain('Worker conectado');
      });
    } finally {
      await cleanup();
    }

    getLogsMock.mockResolvedValue([
      buildLog({
        logTimestamp: '2030-01-02T03:04:05.000Z',
        logLevel: 'info',
        logMessage: 'Servidor listo',
      }),
      buildLog({
        logTimestamp: '2030-01-02T03:06:05.000Z',
        logLevel: 'error',
        logMessage: 'Disco lleno',
      }),
    ]);

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    try {
      await waitForExpectation(() => {
        expect(secondContainer.querySelector('table')).not.toBeNull();
        expect(secondContainer.querySelector('[data-testid="server-logs-shared-level-summary"]')).toBeNull();
        expect(hasTableHeader(secondContainer, 'Nivel')).toBe(true);
        expect(secondContainer.textContent).toContain('Info');
        expect(secondContainer.textContent).toContain('Error');
        expect(secondContainer.textContent).toContain('Disco lleno');
      });
    } finally {
      await secondRender.cleanup();
    }
  });

  it('keeps refresh available when the log request fails', async () => {
    getLogsMock.mockRejectedValue(new Error('logs unavailable'));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Failed to load logs: logs unavailable');
        expect(container.textContent).not.toContain('Actualizacion automatica cada 5 segundos.');
        expect(container.querySelector('[data-testid="server-logs-empty-state"]')).toBeNull();
        expect(container.textContent).toContain('Limite');
        expect(container.querySelector('button[aria-label="Refrescar logs"]')).not.toBeNull();
        expect(container.querySelector('button[aria-label="Vaciar logs"]')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });
});
