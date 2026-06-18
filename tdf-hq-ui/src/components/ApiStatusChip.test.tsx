import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { render, waitFor } from '@testing-library/react';
import type { HealthStatus } from '../api/types';

const healthMock = jest.fn<() => Promise<HealthStatus>>();

jest.unstable_mockModule('../api/meta', () => ({
  Meta: {
    health: () => healthMock(),
  },
}));

const {
  API_STATUS_CHIP_PROGRESS_SIZE_PX,
  default: ApiStatusChip,
} = await import('./ApiStatusChip');

const createDeferred = <T,>() => {
  let resolve!: (value: T) => void;
  let reject!: (reason?: unknown) => void;
  const promise = new Promise<T>((promiseResolve, promiseReject) => {
    resolve = promiseResolve;
    reject = promiseReject;
  });
  return { promise, reject, resolve };
};

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const createQueryClient = () => new QueryClient({
  defaultOptions: { queries: { retry: false, gcTime: 0 } },
});

const renderChip = (queryClient: QueryClient) =>
  render(
    <QueryClientProvider client={queryClient}>
      <ApiStatusChip />
    </QueryClientProvider>,
  );

const expectProgressbarUsesChipProgressSize = (progressbar: Element | null) => {
  expect(progressbar).not.toBeNull();
  const progressbarElement = progressbar as HTMLElement;
  expect(progressbarElement.style.width).toBe(`${API_STATUS_CHIP_PROGRESS_SIZE_PX}px`);
  expect(progressbarElement.style.height).toBe(`${API_STATUS_CHIP_PROGRESS_SIZE_PX}px`);
};

describe('ApiStatusChip', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    healthMock.mockReset();
  });

  it('shows a visible checking state on the first health lookup', async () => {
    const initialHealthLookup = createDeferred<HealthStatus>();
    healthMock.mockReturnValue(initialHealthLookup.promise);

    const firstLookupQueryClient = createQueryClient();
    const { container, unmount } = renderChip(firstLookupQueryClient);

    try {
      expect(healthMock).toHaveBeenCalledTimes(1);
      expect(container.textContent).toContain('API: verificando...');
      expect(container.textContent).not.toContain('API: online');
      expect(container.textContent).not.toContain('API: offline');
      const statusChip = container.querySelector('[role="status"]');
      expect(statusChip?.getAttribute('aria-busy')).toBe('true');
      expect(statusChip?.className).toContain('MuiChip-colorInfo');
      expect(statusChip?.className).toContain('MuiChip-outlined');
      const progressbar = container.querySelector('[role="progressbar"]');
      expect(progressbar?.getAttribute('aria-label')).toBe('Verificando API');
      expectProgressbarUsesChipProgressSize(progressbar);
    } finally {
      await act(async () => {
        initialHealthLookup.resolve({ status: 'ok' });
        await flushPromises();
      });
      unmount();
      firstLookupQueryClient.clear();
    }
  });

  it('keeps cached API status visible while refreshing it', async () => {
    const cachedStatusRefetch = createDeferred<HealthStatus>();
    healthMock.mockReturnValue(cachedStatusRefetch.promise);

    const checkingQueryClient = createQueryClient();
    checkingQueryClient.setQueryData(['meta', 'health-indicator'], { status: 'ok' } satisfies HealthStatus);

    const { container, unmount } = renderChip(checkingQueryClient);

    try {
      expect(healthMock).toHaveBeenCalledTimes(1);
      expect(container.textContent).toContain('API: online');
      expect(container.textContent).not.toContain('API: verificando...');
      expect(container.querySelector('[role="status"]')?.getAttribute('aria-busy')).toBe('true');
      const progressbar = container.querySelector('[role="progressbar"]');
      expect(progressbar?.getAttribute('aria-label')).toBe('Actualizando API');
      expectProgressbarUsesChipProgressSize(progressbar);

      await act(async () => {
        cachedStatusRefetch.resolve({ status: 'ok' });
        await flushPromises();
      });

      await waitFor(() => {
        expect(container.textContent).toContain('API: online');
        expect(container.querySelector('[role="status"]')?.getAttribute('aria-busy')).toBeNull();
        expect(container.querySelector('[role="progressbar"]')).toBeNull();
      });
    } finally {
      unmount();
      checkingQueryClient.clear();
    }
  });

  it('does not keep showing stale online status after a failed health refetch', async () => {
    const failedHealth = createDeferred<HealthStatus>();
    healthMock.mockReturnValue(failedHealth.promise);

    const failedQueryClient = createQueryClient();
    failedQueryClient.setQueryData(['meta', 'health-indicator'], { status: 'ok' } satisfies HealthStatus);

    const { container, unmount } = renderChip(failedQueryClient);

    try {
      expect(healthMock).toHaveBeenCalledTimes(1);
      expect(container.textContent).toContain('API: online');
      expect(container.querySelector('[role="status"]')?.getAttribute('aria-busy')).toBe('true');

      await act(async () => {
        failedHealth.reject(new Error('offline'));
        await flushPromises();
      });

      await waitFor(() => {
        expect(container.textContent).toContain('API: offline');
        expect(container.textContent).not.toContain('API: online');
        expect(container.querySelector('[role="status"]')?.getAttribute('aria-busy')).toBeNull();
      });
    } finally {
      unmount();
      failedQueryClient.clear();
    }
  });
});
