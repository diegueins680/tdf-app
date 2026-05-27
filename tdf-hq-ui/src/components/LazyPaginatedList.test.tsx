import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

const { default: LazyPaginatedList } = await import('./LazyPaginatedList');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const renderList = async (
  mountNode: HTMLElement,
  props?: {
    items?: readonly string[];
    loading?: boolean;
  },
) => {
  const listItems = props?.items ?? ['Alpha', 'Beta', 'Gamma'];
  let root: Root | null = createRoot(mountNode);

  await act(async () => {
    root?.render(
      <LazyPaginatedList
        items={listItems}
        loading={props?.loading}
        pagination={{ initialRowsPerPage: 2, itemLabel: 'items', rowsPerPageOptions: [2] }}
        renderItems={(renderedItems, meta) => (
          <div data-testid="items" data-start-index={meta.startIndex} data-total-items={meta.totalItems}>
            {renderedItems.join('|')}
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
      document.body.removeChild(mountNode);
    },
  };
};

describe('LazyPaginatedList', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('shows a compact loading affordance without hiding current rows', async () => {
    const loadingHost = document.createElement('div');
    document.body.appendChild(loadingHost);
    const { cleanup } = await renderList(loadingHost, { loading: true });

    try {
      const loadingStatus = loadingHost.querySelector('[role="status"]');
      const loadingProgress = loadingHost.querySelector('[role="progressbar"]');
      const loadingItems = loadingHost.querySelector('[data-testid="items"]');

      expect(loadingStatus).not.toBeNull();
      expect(loadingStatus?.getAttribute('aria-live')).toBe('polite');
      expect(loadingHost.firstElementChild?.getAttribute('aria-busy')).toBe('true');
      expect(loadingProgress?.getAttribute('aria-label')).toBe('Cargando resultados...');
      expect(loadingHost.textContent).toContain('Cargando resultados...');
      expect(loadingItems?.textContent).toBe('Alpha|Beta');
    } finally {
      await cleanup();
    }
  });

  it('keeps pagination controls behind one config object', async () => {
    const paginationHost = document.createElement('div');
    document.body.appendChild(paginationHost);
    const { cleanup } = await renderList(paginationHost);

    try {
      const paginationItems = paginationHost.querySelector('[data-testid="items"]');

      expect(paginationHost.querySelector('[role="status"]')).toBeNull();
      expect(paginationHost.firstElementChild?.getAttribute('aria-busy')).toBeNull();
      expect(paginationItems?.textContent).toBe('Alpha|Beta');
      expect(paginationItems?.getAttribute('data-start-index')).toBe('0');
      expect(paginationItems?.getAttribute('data-total-items')).toBe('3');
      expect(paginationHost.textContent).toContain('1-2 de 3 items');
    } finally {
      await cleanup();
    }
  });

  it('keeps the rendered slice aligned with the current page metadata', async () => {
    const pagingHost = document.createElement('div');
    document.body.appendChild(pagingHost);
    const { cleanup } = await renderList(pagingHost, { items: ['Alpha', 'Beta', 'Gamma', 'Delta'] });

    try {
      const nextPageButton = Array.from(pagingHost.querySelectorAll<HTMLButtonElement>('button')).find(
        (candidate) => candidate.getAttribute('aria-label') === 'Go to next page',
      );
      expect(nextPageButton).not.toBeNull();

      await act(async () => {
        nextPageButton?.dispatchEvent(new MouseEvent('click', { bubbles: true }));
        await flushPromises();
      });

      const secondPageItems = pagingHost.querySelector('[data-testid="items"]');

      expect(secondPageItems?.textContent).toBe('Gamma|Delta');
      expect(secondPageItems?.getAttribute('data-start-index')).toBe('2');
      expect(secondPageItems?.getAttribute('data-total-items')).toBe('4');
      expect(pagingHost.textContent).toContain('3-4 de 4 items');
    } finally {
      await cleanup();
    }
  });
});
