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

const {
  default: SidebarNav,
  formatUnreadBadgeContent,
  NAV_GROUPS,
  SIDEBAR_NAV_HIGHLIGHTED_LABEL_STYLE,
  SIDEBAR_NAV_ICON_LAYOUT,
  SIDEBAR_NAV_NO_RESULTS_BOUNDARY_NAME,
  SIDEBAR_NAV_UNREAD_BADGE_DISPLAY,
  SidebarNavErrorBoundary,
} = await import('./SidebarNav');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const setInputValue = (input: HTMLInputElement, value: string) => {
  const descriptor = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value');
  if (descriptor?.set) {
    descriptor.set.call(input, value);
  } else {
    input.value = value;
  }
  input.dispatchEvent(new Event('input', { bubbles: true }));
  input.dispatchEvent(new Event('change', { bubbles: true }));
};

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

const setSearchFilter = async (container: HTMLElement, value: string) => {
  const input = container.querySelector<HTMLInputElement>('input[aria-label="Buscar sección"]');
  if (!input) throw new Error('Sidebar search input not found');

  await act(async () => {
    setInputValue(input, value);
    await flushPromises();
  });
};

describe('SidebarNav', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    window.localStorage.clear();
    chatUnreadCount.unreadCount = 0;
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

  it('caps unread badge content through the named compact-display contract', () => {
    /**
     * @precondition unread counts are positive integers by the time badge
     * content is formatted.
     * @invariant the exact-count ceiling and overflow label stay in sync.
     * @postcondition one count past the ceiling renders the overflow label.
     */
    const { maxExactCount, overflowLabel } = SIDEBAR_NAV_UNREAD_BADGE_DISPLAY;

    expect(Number.isSafeInteger(maxExactCount)).toBe(true);
    expect(maxExactCount).toBeGreaterThan(0);
    expect(overflowLabel).toBe(`${maxExactCount}+`);
    expect(formatUnreadBadgeContent(maxExactCount)).toBe(maxExactCount);
    expect(formatUnreadBadgeContent(maxExactCount + 1)).toBe(overflowLabel);
  });

  it('renders chat overflow badges with the compact-display contract', async () => {
    chatUnreadCount.unreadCount = SIDEBAR_NAV_UNREAD_BADGE_DISPLAY.maxExactCount + 1;

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderNav(container, '/chat');

    try {
      expect(container.textContent).toContain(SIDEBAR_NAV_UNREAD_BADGE_DISPLAY.overflowLabel);
    } finally {
      await cleanup();
    }
  });

  it('keeps search highlight emphasis in a named positive font-weight contract', () => {
    /**
     * @invariant highlighted label emphasis is represented by a safe integer
     * style contract rather than an inline magic number.
     */
    expect(Number.isSafeInteger(SIDEBAR_NAV_HIGHLIGHTED_LABEL_STYLE.matchFontWeight)).toBe(true);
    expect(SIDEBAR_NAV_HIGHLIGHTED_LABEL_STYLE.matchFontWeight).toBeGreaterThan(0);
  });

  it('renders a bounded fallback when the no-results subtree fails', async () => {
    const consoleError = jest.spyOn(console, 'error').mockImplementation(() => undefined);
    const container = document.createElement('div');
    document.body.appendChild(container);
    let root: Root | null = createRoot(container);

    const ThrowingChild = () => {
      throw new Error('No-results subtree failure');
    };

    try {
      await act(async () => {
        root?.render(
          <SidebarNavErrorBoundary boundaryName={SIDEBAR_NAV_NO_RESULTS_BOUNDARY_NAME}>
            <ThrowingChild />
          </SidebarNavErrorBoundary>,
        );
        await flushPromises();
      });

      expect(container.textContent).toContain('No pudimos mostrar esta sección.');
      expect(consoleError).toHaveBeenCalled();
    } finally {
      if (root) {
        await act(async () => {
          root?.unmount();
          await flushPromises();
        });
        root = null;
      }
      document.body.removeChild(container);
      consoleError.mockRestore();
    }
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

  it('trims search text before toggling shortcut and filtered-result state', async () => {
    window.localStorage.setItem(
      'tdf-quick-nav-recents',
      JSON.stringify(['/configuracion/usuarios-admin']),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderNav(container, '/configuracion/inscripciones-curso');

    try {
      expect(container.textContent).toContain('ATAJOS');
      expect(container.querySelector('a[href="/configuracion/usuarios-admin"]')).not.toBeNull();

      await setSearchFilter(container, '   ');
      expect(container.textContent).toContain('ATAJOS');
      expect(container.textContent).not.toContain('Sin coincidencias.');

      await setSearchFilter(container, '  USUARIOS  ');
      expect(container.textContent).not.toContain('ATAJOS');
      expect(container.querySelector('a[href="/configuracion/usuarios-admin"]')).not.toBeNull();
      expect(container.querySelector('a[href="/configuracion/inscripciones-curso"]')).toBeNull();
    } finally {
      await cleanup();
    }
  });
});
