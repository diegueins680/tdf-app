import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, Route, Routes } from 'react-router-dom';

const session = {
  username: 'admin',
  displayName: 'Admin',
  roles: ['admin'],
  modules: ['admin', 'crm', 'ops'],
};

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({
    session,
    loading: false,
    logout: jest.fn(),
  }),
}));

jest.unstable_mockModule('../components/SidebarNav', () => ({
  default: () => <aside data-testid="sidebar-nav" />,
}));

jest.unstable_mockModule('../components/TopBar', () => ({
  default: () => <header data-testid="top-bar" />,
}));

jest.unstable_mockModule('../components/ApiStatusChip', () => ({
  default: () => <div data-testid="api-status-chip">API helper</div>,
}));

jest.unstable_mockModule('../components/ChatKitLauncher', () => ({
  default: () => <div data-testid="chatkit-launcher">Chat helper</div>,
}));

jest.unstable_mockModule('./RouteLoadingFallback', () => ({
  default: () => <div data-testid="route-loading" />,
}));

const { Shell } = await import('./AppShell');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const renderShell = async (container: HTMLElement, initialEntry: string) => {
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter
        initialEntries={[initialEntry]}
        future={{ v7_startTransition: true, v7_relativeSplatPath: true }}
      >
        <Routes>
          <Route element={<Shell />}>
            <Route
              path="/configuracion/inscripciones-curso"
              element={<main>Course registrations admin body</main>}
            />
            <Route path="/configuracion/usuarios-admin" element={<main>Admin users body</main>} />
            <Route path="/social/inbox" element={<main>Social inbox admin body</main>} />
            <Route path="/operacion/ordenes-marketplace" element={<main>Marketplace orders admin body</main>} />
            <Route path="/configuracion/estado" element={<main>System status body</main>} />
            <Route path="/inicio" element={<main>Home body</main>} />
          </Route>
        </Routes>
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

describe('Shell', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    window.localStorage.clear();
  });

  it('hides global floating helpers on the dense course registrations admin page', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderShell(container, '/configuracion/inscripciones-curso');

    try {
      expect(container.textContent).toContain('Course registrations admin body');
      expect(container.querySelector('[data-testid="api-status-chip"]')).toBeNull();
      expect(container.querySelector('[data-testid="chatkit-launcher"]')).toBeNull();
    } finally {
      await cleanup();
    }
  });

  it('hides global floating helpers on the dense admin users page', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderShell(container, '/configuracion/usuarios-admin');

    try {
      expect(container.textContent).toContain('Admin users body');
      expect(container.querySelector('[data-testid="api-status-chip"]')).toBeNull();
      expect(container.querySelector('[data-testid="chatkit-launcher"]')).toBeNull();
    } finally {
      await cleanup();
    }
  });

  it('hides global floating helpers on the dense social inbox admin page', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderShell(container, '/social/inbox?review=1');

    try {
      expect(container.textContent).toContain('Social inbox admin body');
      expect(container.querySelector('[data-testid="api-status-chip"]')).toBeNull();
      expect(container.querySelector('[data-testid="chatkit-launcher"]')).toBeNull();
    } finally {
      await cleanup();
    }
  });

  it('hides global floating helpers on the dense marketplace orders admin page', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderShell(container, '/operacion/ordenes-marketplace');

    try {
      expect(container.textContent).toContain('Marketplace orders admin body');
      expect(container.querySelector('[data-testid="api-status-chip"]')).toBeNull();
      expect(container.querySelector('[data-testid="chatkit-launcher"]')).toBeNull();
    } finally {
      await cleanup();
    }
  });

  it('keeps global floating helpers available on lighter admin pages', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderShell(container, '/configuracion/estado');

    try {
      expect(container.textContent).toContain('System status body');
      expect(container.querySelector('[data-testid="api-status-chip"]')).not.toBeNull();
      expect(container.querySelector('[data-testid="chatkit-launcher"]')).not.toBeNull();
    } finally {
      await cleanup();
    }
  });
});
