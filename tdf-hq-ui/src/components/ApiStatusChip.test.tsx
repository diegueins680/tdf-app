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
  let reject!: (reason?: unknown) => void;
  const promise = new Promise<T>((promiseResolve, promiseReject) => {
    resolve = promiseResolve;
    reject = promiseReject;
  });
  return { promise, reject, resolve };
};

const renderChip = async (hostElement: HTMLElement, queryClient: QueryClient) => {
  let root: Root | null = createRoot(hostElement);

  await act(async () => {
    root?.render(
      <QueryClientProvider client={queryClient}>
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
      queryClient.clear();
      document.body.removeChild(hostElement);
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

    const checkingQueryClient = new QueryClient({
      defaultOptions: { queries: { retry: false, gcTime: 0 } },
    });
    checkingQueryClient.setQueryData(['meta', 'health-indicator'], { status: 'ok' } satisfies HealthStatus);

    const checkingContainer = document.createElement('div');
    document.body.appendChild(checkingContainer);
    const { cleanup } = await renderChip(checkingContainer, checkingQueryClient);

    try {
      expect(healthMock).toHaveBeenCalledTimes(1);
      expect(checkingContainer.textContent).toContain('API: verificando...');
      expect(checkingContainer.textContent).not.toContain('API: online');
      expect(checkingContainer.querySelector('[role="status"]')?.getAttribute('aria-busy')).toBe('true');
      expect(checkingContainer.querySelector('[role="progressbar"]')?.getAttribute('aria-label')).toBe('Verificando API');

      await act(async () => {
        pendingHealth.resolve({ status: 'ok' });
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(checkingContainer.textContent).toContain('API: online');
        expect(checkingContainer.querySelector('[role="status"]')?.getAttribute('aria-busy')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('does not keep showing stale online status after a failed health refetch', async () => {
    const failedHealth = createDeferred<HealthStatus>();
    healthMock.mockReturnValue(failedHealth.promise);

    const failedQueryClient = new QueryClient({
      defaultOptions: { queries: { retry: false, gcTime: 0 } },
    });
    failedQueryClient.setQueryData(['meta', 'health-indicator'], { status: 'ok' } satisfies HealthStatus);

    const failedContainer = document.createElement('div');
    document.body.appendChild(failedContainer);
    const { cleanup } = await renderChip(failedContainer, failedQueryClient);

    try {
      expect(healthMock).toHaveBeenCalledTimes(1);
      expect(failedContainer.textContent).toContain('API: verificando...');

      await act(async () => {
        failedHealth.reject(new Error('offline'));
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(failedContainer.textContent).toContain('API: offline');
        expect(failedContainer.textContent).not.toContain('API: online');
        expect(failedContainer.querySelector('[role="status"]')?.getAttribute('aria-busy')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });
});
