import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import { COURSE_REGISTRATIONS_NAV_LABEL } from '../utils/navigationLabels';

const session = {
  username: 'admin',
  displayName: 'Admin',
  roles: ['admin'],
  modules: ['admin', 'crm', 'label', 'ops', 'scheduling', 'school', 'invoicing', 'internships'],
};

const chatUnreadCount = { unreadCount: 0 };

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({
    session,
  }),
}));

jest.unstable_mockModule('../hooks/useChatUnreadCount', () => ({
  useChatUnreadCount: () => chatUnreadCount,
}));

const { default: SidebarNav } = await import('./SidebarNav');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const renderNav = async (container: HTMLElement, initialEntry: string) => {
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter
        initialEntries={[initialEntry]}
        future={{ v7_startTransition: true, v7_relativeSplatPath: true }}
      >
        <SidebarNav open />
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

describe('SidebarNav', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    window.localStorage.clear();
  });

  it('keeps the current admin page out of shortcuts so navigation does not repeat the active destination', async () => {
    window.localStorage.setItem(
      'tdf-quick-nav-recents',
      JSON.stringify(['/configuracion/inscripciones-curso', '/configuracion/usuarios-admin']),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderNav(container, '/configuracion/inscripciones-curso');

    try {
      const currentLinks = Array.from(
        container.querySelectorAll<HTMLAnchorElement>('a[href="/configuracion/inscripciones-curso"]'),
      );

      expect(container.textContent).toContain('ATAJOS');
      expect(currentLinks).toHaveLength(1);
      expect(currentLinks[0]?.textContent).toContain(COURSE_REGISTRATIONS_NAV_LABEL);
      expect(currentLinks[0]?.getAttribute('aria-current')).toBe('page');
    } finally {
      await cleanup();
    }
  });
});
