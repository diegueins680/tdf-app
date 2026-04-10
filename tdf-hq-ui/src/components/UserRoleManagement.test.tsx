import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

type UserSummary = {
  id?: number | null;
  name?: string | null;
  email?: string | null;
  phone?: string | null;
  status?: 'Active' | 'Inactive' | null;
  roles?: string[] | null;
};

const getUsersMock = jest.fn<() => Promise<UserSummary[]>>();
const updateUserRolesMock = jest.fn<(userId: number, roles: string[]) => Promise<void>>();

jest.unstable_mockModule('../api/generated/client', () => ({
  apiClient: {
    getUsers: () => getUsersMock(),
    updateUserRoles: (userId: number, roles: string[]) => updateUserRolesMock(userId, roles),
  },
}));

const { default: UserRoleManagement } = await import('./UserRoleManagement');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const waitForExpectation = async (assertion: () => void, attempts = 12) => {
  let lastError: unknown;
  for (let index = 0; index < attempts; index += 1) {
    try {
      assertion();
      return;
    } catch (error) {
      lastError = error;
      await act(async () => {
        await flushPromises();
      });
    }
  }
  throw lastError;
};

const renderComponent = async (container: HTMLElement) => {
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(<UserRoleManagement />);
    await flushPromises();
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

const buildUser = (overrides: Partial<UserSummary> = {}): UserSummary => ({
  id: 101,
  name: 'Ada Lovelace',
  email: 'ada@example.com',
  phone: '+593999000111',
  status: 'Active',
  roles: ['Admin'],
  ...overrides,
});

const getHeaders = (container: HTMLElement) =>
  Array.from(container.querySelectorAll('thead th')).map((cell) => (cell.textContent ?? '').replace(/\s+/g, ' ').trim());

const getRowByName = (container: HTMLElement, name: string) => {
  const row = Array.from(container.querySelectorAll('tbody tr')).find((element) =>
    (element.textContent ?? '').includes(name),
  );
  if (!row) throw new Error(`Row not found for ${name}`);
  return row;
};

describe('UserRoleManagement', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
    if (!window.matchMedia) {
      Object.defineProperty(window, 'matchMedia', {
        writable: true,
        value: () => ({
          matches: false,
          media: '',
          onchange: null,
          addListener: () => undefined,
          removeListener: () => undefined,
          addEventListener: () => undefined,
          removeEventListener: () => undefined,
          dispatchEvent: () => false,
        }),
      });
    }
  });

  beforeEach(() => {
    getUsersMock.mockReset();
    updateUserRolesMock.mockReset();
    getUsersMock.mockResolvedValue([]);
    updateUserRolesMock.mockResolvedValue();
  });

  it('replaces the blank first-run table with setup guidance for admins', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        expect(getUsersMock).toHaveBeenCalledTimes(1);
        expect(container.textContent).toContain('Roles y permisos');
        expect(container.textContent).toContain(
          'Todavía no hay usuarios administrables. Cuando exista el primero, aquí aparecerán contacto, estado y roles para editar permisos desde una sola tabla.',
        );
        expect(container.querySelector('table')).toBeNull();
        expect(container.querySelectorAll('thead th')).toHaveLength(0);
        expect(container.querySelectorAll('tbody tr')).toHaveLength(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('consolidates contact details into one column so the admin table avoids repeated empty placeholders', async () => {
    getUsersMock.mockResolvedValue([
      buildUser({
        id: 101,
        name: 'Ada Lovelace',
        email: 'ada@example.com',
        phone: '   ',
      }),
      buildUser({
        id: 102,
        name: 'Grace Hopper',
        email: null,
        phone: '+593999000222',
      }),
      buildUser({
        id: 103,
        name: 'Linus QA',
        email: 'linus@example.com',
        phone: '+593999000333',
      }),
      buildUser({
        id: 104,
        name: 'Sin Contacto',
        email: '   ',
        phone: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        expect(getHeaders(container)).toEqual(['Usuario', 'Contacto', 'Roles y edición']);
        expect(container.textContent).toContain(
          'Revisa el acceso actual y ajusta roles sin salir de esta tabla. Vista actual: la columna de estado sigue oculta mientras todas las cuentas sigan activas.',
        );
        expect(container.textContent).not.toContain('Active');

        const adaRow = getRowByName(container, 'Ada Lovelace');
        expect(adaRow.textContent).toContain('ID 101');
        expect(adaRow.textContent).toContain('ada@example.com');
        expect(adaRow.textContent).toContain('Editar roles');
        expect(adaRow.textContent).not.toContain('Sin email ni teléfono');

        const graceRow = getRowByName(container, 'Grace Hopper');
        expect(graceRow.textContent).toContain('ID 102');
        expect(graceRow.textContent).toContain('+593999000222');
        expect(graceRow.textContent).not.toContain('Sin email ni teléfono');

        const linusRow = getRowByName(container, 'Linus QA');
        expect(linusRow.textContent).toContain('ID 103');
        expect(linusRow.textContent).toContain('linus@example.com');
        expect(linusRow.textContent).toContain('+593999000333');

        const missingContactRow = getRowByName(container, 'Sin Contacto');
        expect(missingContactRow.textContent).toContain('ID 104');
        expect(missingContactRow.textContent).toContain('Sin email ni teléfono');
        expect(missingContactRow.textContent).not.toContain('--');
      });
    } finally {
      await cleanup();
    }
  });

  it('hides the empty contact column until at least one admin user has contact info', async () => {
    getUsersMock.mockResolvedValue([
      buildUser({
        id: 201,
        name: 'Ada Lovelace',
        email: '   ',
        phone: null,
      }),
      buildUser({
        id: 202,
        name: 'Grace Hopper',
        email: null,
        phone: '   ',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        expect(getHeaders(container)).toEqual(['Usuario', 'Roles y edición']);
        expect(container.textContent).toContain(
          'Revisa el acceso actual y ajusta roles sin salir de esta tabla. Vista actual: la columna de contacto sigue oculta hasta que exista al menos un email o teléfono y la columna de estado sigue oculta mientras todas las cuentas sigan activas.',
        );
        expect(container.textContent).not.toContain('Sin email ni teléfono');
        expect(container.textContent).not.toContain('Active');

        const adaRow = getRowByName(container, 'Ada Lovelace');
        expect(adaRow.textContent).toContain('ID 201');

        const graceRow = getRowByName(container, 'Grace Hopper');
        expect(graceRow.textContent).toContain('ID 202');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps role editing inside the roles column so the default admin landing page drops a separate actions column', async () => {
    getUsersMock.mockResolvedValue([
      buildUser({
        id: 301,
        name: 'Grace Hopper',
        email: 'grace@example.com',
        phone: null,
        roles: ['Admin', 'Manager'],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        expect(getHeaders(container)).toEqual(['Usuario', 'Contacto', 'Roles y edición']);
        expect(container.textContent).toContain('Roles y permisos');
        expect(container.textContent).toContain(
          'Revisa el acceso actual y ajusta roles sin salir de esta tabla. Vista actual: la columna de estado sigue oculta mientras todas las cuentas sigan activas.',
        );
        expect(container.textContent).not.toContain('Acciones');
        expect(container.textContent).not.toContain('Active');

        const graceRow = getRowByName(container, 'Grace Hopper');
        expect(graceRow.textContent).toContain('Admin');
        expect(graceRow.textContent).toContain('Manager');
        expect(graceRow.textContent).toContain('Editar roles');
      });
    } finally {
      await cleanup();
    }
  });

  it('shows the status column only when at least one administrable user is inactive', async () => {
    getUsersMock.mockResolvedValue([
      buildUser({
        id: 401,
        name: 'Ada Lovelace',
        status: 'Active',
      }),
      buildUser({
        id: 402,
        name: 'Grace Hopper',
        status: 'Inactive',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        expect(getHeaders(container)).toEqual(['Usuario', 'Contacto', 'Estado', 'Roles y edición']);
        expect(container.textContent).toContain('Revisa el acceso actual y ajusta roles sin salir de esta tabla.');
        expect(container.textContent).not.toContain('Vista actual:');

        const adaRow = getRowByName(container, 'Ada Lovelace');
        expect(adaRow.textContent).toContain('Active');

        const graceRow = getRowByName(container, 'Grace Hopper');
        expect(graceRow.textContent).toContain('Inactive');
      });
    } finally {
      await cleanup();
    }
  });

  it('condenses hidden-column guidance into one summary line so the default admin view stays easier to scan', async () => {
    getUsersMock.mockResolvedValue([
      buildUser({
        id: 501,
        name: 'Ada Lovelace',
        email: '   ',
        phone: null,
      }),
      buildUser({
        id: 502,
        name: 'Grace Hopper',
        email: null,
        phone: '   ',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Revisa el acceso actual y ajusta roles sin salir de esta tabla. Vista actual: la columna de contacto sigue oculta hasta que exista al menos un email o teléfono y la columna de estado sigue oculta mientras todas las cuentas sigan activas.',
        );
        expect(container.textContent).not.toContain(
          'Todavía no hay email ni teléfono cargado. La columna de contacto aparecerá cuando exista al menos un dato para revisar.',
        );
        expect(container.textContent).not.toContain(
          'Todos los usuarios administrables están activos. La columna de estado aparecerá cuando exista al menos una cuenta inactiva.',
        );
      });
    } finally {
      await cleanup();
    }
  });
});
