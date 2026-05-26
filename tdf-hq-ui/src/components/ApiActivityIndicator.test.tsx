import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

let pendingRequests = 0;
let activityListeners = new Set<() => void>();

jest.unstable_mockModule('../api/client', () => ({
  getPendingApiRequestCount: () => pendingRequests,
  subscribeToApiActivity: (listener: () => void) => {
    activityListeners.add(listener);
    return () => activityListeners.delete(listener);
  },
}));

const { default: ApiActivityIndicator } = await import('./ApiActivityIndicator');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const renderIndicator = async (container: HTMLElement) => {
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(<ApiActivityIndicator />);
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
      document.body.removeChild(container);
    },
  };
};

const emitApiActivity = async (nextPendingRequests: number) => {
  pendingRequests = nextPendingRequests;
  await act(async () => {
    activityListeners.forEach((listener) => listener());
    await flushPromises();
  });
};

describe('ApiActivityIndicator', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    pendingRequests = 0;
    activityListeners = new Set();
  });

  it('shows a visible progressbar only while shared API requests are pending', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderIndicator(container);

    try {
      expect(container.querySelector('[role="progressbar"]')).toBeNull();
      expect(container.querySelector('[role="status"]')?.getAttribute('aria-busy')).toBeNull();

      await emitApiActivity(1);

      const progressbar = container.querySelector('[role="progressbar"]');
      expect(progressbar).not.toBeNull();
      expect(progressbar?.getAttribute('aria-label')).toBe('Cargando datos');
      expect(container.querySelector('[role="status"]')?.getAttribute('aria-busy')).toBe('true');

      await emitApiActivity(0);

      expect(container.querySelector('[role="progressbar"]')).toBeNull();
      expect(container.querySelector('[role="status"]')?.getAttribute('aria-busy')).toBeNull();
    } finally {
      await cleanup();
    }
  });
});
