import { jest } from '@jest/globals';
import { act, isValidElement } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { NavGroup } from './SidebarNav';
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

const { default: SidebarNav, NAV_GROUPS, SIDEBAR_NAV_ICON_LAYOUT } = await import('./SidebarNav');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const getGroupIconFontSize = (icon: NavGroup['icon']): unknown => {
  if (!isValidElement(icon)) return undefined;
  return (icon.props as { sx?: { fontSize?: unknown } }).sx?.fontSize;
};

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

  it('keeps every top-level group icon on the shared compact-size contract', () => {
    /**
     * @precondition top-level nav group icons are React elements with MUI sx props.
     * @invariant every group header icon uses the same compact pixel dimension.
     * @postcondition any ad hoc per-group fontSize drift fails this contract.
     */
    const iconSizesByGroup = NAV_GROUPS.map((group) => [group.title, getGroupIconFontSize(group.icon)]);

    expect(iconSizesByGroup).toEqual(
      NAV_GROUPS.map((group) => [group.title, SIDEBAR_NAV_ICON_LAYOUT.groupHeaderIconSizePx]),
    );
  });

  it('hides default shortcut duplicates until there are actual recent admin destinations', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderNav(container, '/configuracion/inscripciones-curso');

    try {
      expect(container.textContent).not.toContain('ATAJOS');
      expect(container.querySelector('a[href="/inicio"]')).toBeNull();
      expect(container.querySelector('a[href="/crm/contactos"]')).toBeNull();
      expect(container.querySelector('a[href="/configuracion/inscripciones-curso"]')).not.toBeNull();
    } finally {
      await cleanup();
    }
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

  it('keeps support and tool links collapsed away from the active admin section', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderNav(container, '/configuracion/inscripciones-curso');

    try {
      expect(container.textContent).toContain('ADMIN');
      expect(container.textContent).toContain('HERRAMIENTAS');
      expect(container.textContent).toContain('AYUDA');
      expect(container.querySelector('a[href="/configuracion/inscripciones-curso"]')).not.toBeNull();
      expect(container.querySelector('a[href="/manual"]')).toBeNull();
      expect(container.querySelector('a[href="/docs"]')).toBeNull();
      expect(container.querySelector('a[href="/herramientas/chatkit"]')).toBeNull();
      expect(container.querySelector('a[href="/herramientas/token-admin"]')).toBeNull();
    } finally {
      await cleanup();
    }
  });
});
