import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import type { HealthStatus } from '../api/types';

const healthMock = jest.fn<() => Promise<HealthStatus>>();

jest.unstable_mockModule('../api/meta', () => ({
  Meta: {
    health: () => healthMock(),
  },
}));

const { default: ApiStatusChip } = await import('./ApiStatusChip');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const createDeferred = <T,>() => {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>((promiseResolve) => {
    resolve = promiseResolve;
  });
  return { promise, resolve };
};

const renderChip = async (container: HTMLElement, qc: QueryClient) => {
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <QueryClientProvider client={qc}>
        <ApiStatusChip />
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

describe('ApiStatusChip', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    healthMock.mockReset();
  });

  it('shows a visible checking state while cached API status is refetching', async () => {
    const pendingHealth = createDeferred<HealthStatus>();
    healthMock.mockReturnValue(pendingHealth.promise);

    const qc = new QueryClient({
      defaultOptions: { queries: { retry: false, gcTime: 0 } },
    });
    qc.setQueryData(['meta', 'health-indicator'], { status: 'ok' } satisfies HealthStatus);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderChip(container, qc);

    try {
      expect(healthMock).toHaveBeenCalledTimes(1);
      expect(container.textContent).toContain('API: verificando...');
      expect(container.textContent).not.toContain('API: online');
      expect(container.querySelector('[role="status"]')?.getAttribute('aria-busy')).toBe('true');
      expect(container.querySelector('[role="progressbar"]')?.getAttribute('aria-label')).toBe('Verificando API');

      await act(async () => {
        pendingHealth.resolve({ status: 'ok' });
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(container.textContent).toContain('API: online');
        expect(container.querySelector('[role="status"]')?.getAttribute('aria-busy')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });
});
