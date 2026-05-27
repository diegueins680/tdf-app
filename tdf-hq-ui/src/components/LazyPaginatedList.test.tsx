import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

const { default: LazyPaginatedList } = await import('./LazyPaginatedList');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const renderList = async (
  container: HTMLElement,
  props?: {
    loading?: boolean;
  },
) => {
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <LazyPaginatedList
        items={['Alpha', 'Beta', 'Gamma']}
        loading={props?.loading}
        pagination={{ initialRowsPerPage: 2, itemLabel: 'items', rowsPerPageOptions: [2] }}
        renderItems={(items, meta) => (
          <div data-testid="items" data-start-index={meta.startIndex} data-total-items={meta.totalItems}>
            {items.join('|')}
          </div>
        )}
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

describe('LazyPaginatedList', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('shows a compact loading affordance without hiding current rows', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderList(container, { loading: true });

    try {
      const status = container.querySelector('[role="status"]');
      const progress = container.querySelector('[role="progressbar"]');
      const items = container.querySelector('[data-testid="items"]');

      expect(status).not.toBeNull();
      expect(status?.getAttribute('aria-live')).toBe('polite');
      expect(container.firstElementChild?.getAttribute('aria-busy')).toBe('true');
      expect(progress?.getAttribute('aria-label')).toBe('Cargando resultados...');
      expect(container.textContent).toContain('Cargando resultados...');
      expect(items?.textContent).toBe('Alpha|Beta');
    } finally {
      await cleanup();
    }
  });

  it('keeps pagination controls behind one config object', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderList(container);

    try {
      const items = container.querySelector('[data-testid="items"]');

      expect(container.querySelector('[role="status"]')).toBeNull();
      expect(container.firstElementChild?.getAttribute('aria-busy')).toBeNull();
      expect(items?.textContent).toBe('Alpha|Beta');
      expect(items?.getAttribute('data-start-index')).toBe('0');
      expect(items?.getAttribute('data-total-items')).toBe('3');
      expect(container.textContent).toContain('1-2 de 3 items');
    } finally {
      await cleanup();
    }
  });
});
