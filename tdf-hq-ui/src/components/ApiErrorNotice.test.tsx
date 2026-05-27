import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

jest.unstable_mockModule('../utils/logger', () => ({
  logger: {
    warn: jest.fn(),
  },
}));

jest.unstable_mockModule('../api/client', () => ({
  API_BASE_URL: '',
}));

const { default: ApiErrorNotice, ApiLoadingNotice, API_LOADING_NOTICE_CONTRACTS } = await import('./ApiErrorNotice');

const EXPECTED_API_LOADING_NOTICE_CONTRACTS = {
  loadingSpinnerSizePx: 2 * 10 - 2,
  titleFontWeight: 7 * 100,
} as const;

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const createDeferred = () => {
  let resolve!: () => void;
  const promise = new Promise<void>((resolvePromise) => {
    resolve = resolvePromise;
  });
  return { promise, resolve };
};

const renderNotice = async (container: HTMLElement) => {
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <ApiLoadingNotice
        title="Cargando contenido publicado"
        message="Consultando la versión en vivo antes de mostrar la vista previa."
        helper={<span>La vista se actualiza sola.</span>}
      />,
    );
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

const renderErrorNotice = async (container: HTMLElement, onRetry: () => void | Promise<unknown>) => {
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <ApiErrorNotice
        error={new Error('Servicio no disponible')}
        title="No pudimos cargar versiones"
        onRetry={onRetry}
      />,
    );
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

describe('ApiErrorNotice', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('renders retry as a clear native non-submit button with guarded feedback', async () => {
    const retry = createDeferred();
    const onRetry = jest.fn<() => Promise<void>>(() => retry.promise);
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderErrorNotice(container, onRetry);

    try {
      const alert = container.querySelector('[role="alert"]');
      const retryButton = container.querySelector<HTMLButtonElement>(
        'button[aria-label="Reintentar: No pudimos cargar versiones"]',
      );

      expect(retryButton).toBeInstanceOf(HTMLButtonElement);
      expect(retryButton?.textContent?.trim()).toBe('Reintentar');
      expect(retryButton?.type).toBe('button');
      expect(retryButton?.disabled).toBe(false);
      expect(retryButton?.getAttribute('aria-busy')).toBeNull();

      await act(async () => {
        retryButton?.focus();
        await flushPromises();
      });
      expect(document.activeElement).toBe(retryButton);

      await act(async () => {
        retryButton?.click();
        await flushPromises();
      });

      expect(onRetry).toHaveBeenCalledTimes(1);
      expect(retryButton?.disabled).toBe(true);
      expect(retryButton?.getAttribute('aria-busy')).toBe('true');
      expect(retryButton?.textContent).toContain('Reintentando...');
      expect(document.activeElement).toBe(alert);

      await act(async () => {
        retryButton?.click();
        await flushPromises();
      });
      expect(onRetry).toHaveBeenCalledTimes(1);

      await act(async () => {
        retry.resolve();
        await flushPromises();
      });

      expect(retryButton?.disabled).toBe(false);
      expect(retryButton?.textContent?.trim()).toBe('Reintentar');
      expect(document.activeElement).toBe(alert);
    } finally {
      await cleanup();
    }
  });

  it('activates retry from the keyboard without relying on pointer input', async () => {
    const onRetry = jest.fn<() => void>();
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderErrorNotice(container, onRetry);

    try {
      const retryButton = container.querySelector<HTMLButtonElement>(
        'button[aria-label="Reintentar: No pudimos cargar versiones"]',
      );
      expect(retryButton).toBeInstanceOf(HTMLButtonElement);

      const enterKey = new KeyboardEvent('keydown', {
        key: 'Enter',
        bubbles: true,
        cancelable: true,
      });

      await act(async () => {
        retryButton?.dispatchEvent(enterKey);
        await flushPromises();
      });

      expect(enterKey.defaultPrevented).toBe(true);
      expect(onRetry).toHaveBeenCalledTimes(1);
    } finally {
      await cleanup();
    }
  });
});

describe('ApiLoadingNotice', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('preserves the named compact loading notice contracts', () => {
    expect(API_LOADING_NOTICE_CONTRACTS).toEqual(EXPECTED_API_LOADING_NOTICE_CONTRACTS);
  });

  it('keeps loading notice dimensions and typography within valid bounds', () => {
    for (const value of Object.values(API_LOADING_NOTICE_CONTRACTS)) {
      expect(Number.isInteger(value)).toBe(true);
      expect(value).toBeGreaterThan(0);
    }

    expect(API_LOADING_NOTICE_CONTRACTS.loadingSpinnerSizePx).toBeLessThanOrEqual(2 * 10);
    expect(API_LOADING_NOTICE_CONTRACTS.titleFontWeight % 100).toBe(0);
    expect(API_LOADING_NOTICE_CONTRACTS.titleFontWeight).toBeGreaterThan(4 * 100);
    expect(API_LOADING_NOTICE_CONTRACTS.titleFontWeight).toBeLessThanOrEqual(9 * 100);
  });

  it('renders a compact polite loading state for data-fetching panels', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderNotice(container);

    try {
      const status = container.querySelector('[role="status"]');
      expect(status).not.toBeNull();
      expect(status?.getAttribute('aria-live')).toBe('polite');
      expect(status?.getAttribute('aria-busy')).toBe('true');
      expect(container.querySelector('[role="alert"]')).toBeNull();
      expect(container.querySelector('[role="progressbar"]')?.getAttribute('aria-label')).toBe(
        'Cargando contenido publicado',
      );
      expect(container.textContent).toContain('Cargando contenido publicado');
      expect(container.textContent).toContain('Consultando la versión en vivo antes de mostrar la vista previa.');
      expect(container.textContent).toContain('La vista se actualiza sola.');
    } finally {
      await cleanup();
    }
  });
});
