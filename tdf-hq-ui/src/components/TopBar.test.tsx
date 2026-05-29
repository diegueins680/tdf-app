import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';

const session = {
  username: 'admin',
  displayName: 'Admin',
  roles: ['admin'],
  modules: ['admin', 'crm', 'label', 'ops', 'scheduling', 'school', 'invoicing', 'internships'],
};

const noop = () => undefined;

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session }),
  getStoredSessionToken: () => null,
  getActiveSession: () => session,
  setTransientApiToken: noop,
}));

jest.unstable_mockModule('./SessionMenu', () => ({
  default: () => null,
}));

jest.unstable_mockModule('./NotificationBell', () => ({
  default: () => null,
}));

jest.unstable_mockModule('./BrandLogo', () => ({
  default: () => null,
}));

jest.unstable_mockModule('../hooks/useChatUnreadCount', () => ({
  useChatUnreadCount: () => ({ unreadCount: 0 }),
}));

jest.unstable_mockModule('../utils/logger', () => ({
  logger: { log: noop, warn: noop, error: noop },
}));

const { default: TopBar, QUICK_NAV_VISIBLE_RESULT_LIMIT, limitQuickNavItems } = await import('./TopBar');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const renderTopBar = async (container: HTMLElement) => {
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter
        initialEntries={['/inicio']}
        future={{ v7_startTransition: true, v7_relativeSplatPath: true }}
      >
        <TopBar />
      </MemoryRouter>,
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

const openPalette = async (container: HTMLElement) => {
  const trigger = container.querySelector<HTMLButtonElement>('button[aria-label="Buscar sección"]');
  await act(async () => {
    trigger?.click();
    await flushPromises();
  });
};

describe('TopBar quick-nav contracts', () => {
  it('defines a positive integer visible result limit', () => {
    expect(Number.isSafeInteger(QUICK_NAV_VISIBLE_RESULT_LIMIT)).toBe(true);
    expect(QUICK_NAV_VISIBLE_RESULT_LIMIT).toBeGreaterThan(0);
  });

  it('limits quick-nav items without reordering or mutating input', () => {
    const items = Array.from(
      { length: QUICK_NAV_VISIBLE_RESULT_LIMIT + 1 },
      (_, index) => `item-${index}`,
    );

    const visibleItems = limitQuickNavItems(items);

    expect(visibleItems).toHaveLength(QUICK_NAV_VISIBLE_RESULT_LIMIT);
    expect(visibleItems).toEqual(items.slice(0, QUICK_NAV_VISIBLE_RESULT_LIMIT));
    expect(items).toHaveLength(QUICK_NAV_VISIBLE_RESULT_LIMIT + 1);
  });
});

describe('TopBar command palette', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('shows keyboard navigation hints when the palette opens', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderTopBar(container);

    try {
      await openPalette(container);
      const dialog = document.body.querySelector('[role="dialog"]');
      expect(dialog?.textContent).toContain('para navegar');
      expect(dialog?.textContent).toContain('para abrir');
      expect(dialog?.textContent).toContain('para cerrar');
    } finally {
      await cleanup();
    }
  });

  it('caps results and tells the user how many more matches exist', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderTopBar(container);

    try {
      await openPalette(container);
      const dialog = document.body.querySelector('[role="dialog"]');
      // The full nav has more destinations than the visible contract, so the empty query is truncated.
      const renderedItems = dialog?.querySelectorAll('.MuiListItemButton-root') ?? [];
      expect(renderedItems.length).toBeLessThanOrEqual(QUICK_NAV_VISIBLE_RESULT_LIMIT);
      expect(dialog?.textContent).toContain(`Mostrando ${QUICK_NAV_VISIBLE_RESULT_LIMIT} de`);
    } finally {
      await cleanup();
    }
  });
});
