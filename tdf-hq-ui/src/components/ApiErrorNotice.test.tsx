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

const { ApiLoadingNotice } = await import('./ApiErrorNotice');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

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

describe('ApiLoadingNotice', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('renders a visible polite loading state for data-fetching panels', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderNotice(container);

    try {
      const status = container.querySelector('[role="status"]');
      expect(status).not.toBeNull();
      expect(status?.getAttribute('aria-live')).toBe('polite');
      expect(status?.getAttribute('aria-busy')).toBe('true');
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
