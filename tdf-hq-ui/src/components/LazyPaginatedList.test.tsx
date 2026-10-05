import { act, Fragment, StrictMode } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import appI18n from '../i18n/index';

const { default: LazyPaginatedList } = await import('./LazyPaginatedList');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const renderList = async (
  mountNode: HTMLElement,
  props?: {
    items?: readonly string[];
    loading?: boolean;
    selectedIndex?: number;
    strictMode?: boolean;
  },
) => {
  const listItems = props?.items ?? ['Alpha', 'Beta', 'Gamma'];
  let root: Root | null = createRoot(mountNode);
  const Wrapper = props?.strictMode ? StrictMode : Fragment;

  await act(async () => {
    root?.render(
      <Wrapper>
        <LazyPaginatedList
          items={listItems}
          loading={props?.loading}
          pagination={{ initialRowsPerPage: 2, itemLabel: 'items', rowsPerPageOptions: [2], selectedIndex: props?.selectedIndex }}
          renderItems={(renderedItems, meta) => (
            <div data-testid="items" data-start-index={meta.startIndex} data-total-items={meta.totalItems}>
              {renderedItems.join('|')}
            </div>
          )}
        />
      </Wrapper>,
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
  beforeAll(async () => appI18n.changeLanguage('en'));
  afterAll(async () => appI18n.changeLanguage('es'));
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
      expect(loadingProgress?.getAttribute('aria-label')).toBe('Loading results…');
      expect(loadingHost.textContent).toContain('Loading results…');
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
      expect(paginationHost.textContent).toContain('1–2 of 3 items');
    } finally {
      await cleanup();
    }
  });

  it.each([false, true])('retains linked selection through effect replay and permits manual paging (StrictMode=%s)', async (strictMode) => {
    const host = document.createElement('div');
    document.body.appendChild(host);
    const { cleanup } = await renderList(host, {
      items: ['Alpha', 'Beta', 'Gamma', 'Delta'], selectedIndex: 2, strictMode,
    });
    try {
      expect(host.querySelector('[data-testid="items"]')?.textContent).toBe('Gamma|Delta');
      const previous = host.querySelector<HTMLButtonElement>('button[aria-label="Go to previous page"]');
      expect(previous).not.toBeNull();
      await act(async () => {
        previous?.dispatchEvent(new MouseEvent('click', { bubbles: true }));
        await flushPromises();
      });
      expect(host.querySelector('[data-testid="items"]')?.textContent).toBe('Alpha|Beta');
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
      expect(pagingHost.textContent).toContain('3–4 of 4 items');
    } finally {
      await cleanup();
    }
  });
});
