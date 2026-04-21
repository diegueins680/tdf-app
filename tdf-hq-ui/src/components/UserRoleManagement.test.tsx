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

const buttonText = (element: Element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim();

const getButtonsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll<HTMLElement>('button, a')).filter(
    (element) => buttonText(element) === labelText || element.getAttribute('aria-label') === labelText,
  );

const getMenuItemByText = (labelText: string) => {
  const item = Array.from(document.body.querySelectorAll<HTMLElement>('[role="menuitem"], [role="option"]')).find(
    (element) => buttonText(element) === labelText,
  );

  if (!item) {
    throw new Error(`Menu item not found: ${labelText}`);
  }

  return item;
};

const countExactText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll<HTMLElement>('*')).filter(
    (element) => buttonText(element) === labelText,
  ).length;

const getRowByName = (container: HTMLElement, name: string) => {
  const row = Array.from(container.querySelectorAll('tbody tr')).find((element) =>
    (element.textContent ?? '').includes(name),
  );
  if (!row) throw new Error(`Row not found for ${name}`);
  return row;
};

const getContactCellText = (row: Element) => {
  const contactCell = row.querySelectorAll('td')[1];
  if (!(contactCell instanceof HTMLElement)) {
    throw new Error('Contact cell not found');
  }
  return buttonText(contactCell);
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
        expect(getHeaders(container)).toEqual(['Usuario', 'Contacto', 'Roles editables']);
        expect(container.textContent).toContain(
          'Vista actual: la columna de estado sigue oculta mientras todas las cuentas sigan activas.',
        );
        expect(container.textContent).not.toContain('Haz clic sobre los roles para editarlos sin salir de esta tabla.');
        expect(countExactText(container, 'Roles editables')).toBe(1);
        expect(container.textContent).not.toContain('Editar aquí');
        expect(container.textContent).not.toContain('Active');
        expect(container.textContent).not.toContain('Editar roles');

        const adaRow = getRowByName(container, 'Ada Lovelace');
        expect(adaRow.textContent).not.toContain('ID 101');
        expect(adaRow.textContent).toContain('ada@example.com');
        expect(getContactCellText(adaRow)).toBe('ada@example.com');
        expect(adaRow.querySelector('button[aria-label="Editar roles de Ada Lovelace"]')).not.toBeNull();
        expect(adaRow.textContent).not.toContain('Sin email ni teléfono');

        const graceRow = getRowByName(container, 'Grace Hopper');
        expect(graceRow.textContent).not.toContain('ID 102');
        expect(graceRow.textContent).toContain('+593999000222');
        expect(getContactCellText(graceRow)).toBe('+593999000222');
        expect(graceRow.textContent).not.toContain('Sin email ni teléfono');

        const linusRow = getRowByName(container, 'Linus QA');
        expect(linusRow.textContent).not.toContain('ID 103');
        expect(getContactCellText(linusRow)).toBe('linus@example.com · +593999000333');

        const missingContactRow = getRowByName(container, 'Sin Contacto');
        expect(missingContactRow.textContent).not.toContain('ID 104');
        expect(getContactCellText(missingContactRow)).toBe('Sin email ni teléfono');
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
        expect(getHeaders(container)).toEqual(['Usuario', 'Roles editables']);
        expect(container.textContent).toContain(
          'Vista actual: la columna de contacto sigue oculta hasta que exista al menos un email o teléfono y la columna de estado sigue oculta mientras todas las cuentas sigan activas.',
        );
        expect(container.textContent).not.toContain('Haz clic sobre los roles para editarlos sin salir de esta tabla.');
        expect(countExactText(container, 'Roles editables')).toBe(1);
        expect(container.textContent).not.toContain('Editar aquí');
        expect(container.textContent).not.toContain('Sin email ni teléfono');
        expect(container.textContent).not.toContain('Active');

        const adaRow = getRowByName(container, 'Ada Lovelace');
        expect(adaRow.textContent).not.toContain('ID 201');

        const graceRow = getRowByName(container, 'Grace Hopper');
        expect(graceRow.textContent).not.toContain('ID 202');
      });
    } finally {
      await cleanup();
    }
  });

  it('shows internal ids only when duplicate names need disambiguation', async () => {
    getUsersMock.mockResolvedValue([
      buildUser({
        id: 211,
        name: 'Ana Admin',
        email: 'ana.one@example.com',
      }),
      buildUser({
        id: 212,
        name: 'Ana Admin',
        email: 'ana.two@example.com',
        roles: ['Manager'],
      }),
      buildUser({
        id: 213,
        name: 'Grace Hopper',
        email: 'grace@example.com',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        const duplicateRows = Array.from(container.querySelectorAll('tbody tr')).filter((row) =>
          (row.textContent ?? '').includes('Ana Admin'),
        );

        expect(duplicateRows).toHaveLength(2);
        expect(duplicateRows[0]?.textContent).toContain('ID 211');
        expect(duplicateRows[1]?.textContent).toContain('ID 212');
        expect(getRowByName(container, 'Grace Hopper').textContent).not.toContain('ID 213');
        expect(container.querySelector('button[aria-label="Editar roles de Ana Admin (ID 211)"]')).not.toBeNull();
        expect(container.querySelector('button[aria-label="Editar roles de Ana Admin (ID 212)"]')).not.toBeNull();
        expect(container.querySelector('button[aria-label="Editar roles de Grace Hopper"]')).not.toBeNull();
        expect(container.querySelector('button[aria-label="Editar roles de Grace Hopper (ID 213)"]')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('replaces the single-user table with a compact first-user summary that keeps roles editable', async () => {
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
        expect(container.textContent).toContain('Roles y permisos');
        expect(container.textContent).toContain(
          'Primer usuario administrable. Revisa sus datos clave y edita roles aquí; cuando exista el segundo, volverá la tabla comparativa.',
        );
        expect(container.querySelector('table')).toBeNull();
        expect(container.querySelectorAll('thead th')).toHaveLength(0);
        expect(container.querySelectorAll('tbody tr')).toHaveLength(0);
        expect(container.textContent).not.toContain('Vista actual:');
        expect(countExactText(container, 'Roles editables')).toBe(1);
        expect(container.textContent).not.toContain('Editar aquí');
        expect(container.textContent).not.toContain('Active');

        expect(container.textContent).toContain('Grace Hopper');
        expect(container.textContent).not.toContain('ID 301');
        expect(container.textContent).toContain('grace@example.com');
        expect(container.textContent).toContain('Admin');
        expect(container.textContent).toContain('Manager');

        const editButton = container.querySelector('button[aria-label="Editar roles de Grace Hopper"]');
        expect(editButton).not.toBeNull();
      });

      const editButton = container.querySelector('button[aria-label="Editar roles de Grace Hopper"]');
      if (!(editButton instanceof HTMLButtonElement)) {
        throw new Error('Edit roles button not found');
      }

      await act(async () => {
        editButton.dispatchEvent(new MouseEvent('click', { bubbles: true }));
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Editar roles de Grace Hopper');
        expect(document.body.textContent).toContain('Guardar');
      });
    } finally {
      await cleanup();
    }
  });

  it('deduplicates repeated API roles so the landing summary shows one stable access snapshot per user', async () => {
    getUsersMock.mockResolvedValue([
      buildUser({
        id: 302,
        name: 'Linus QA',
        email: 'linus@example.com',
        roles: ['Manager', 'Admin', 'Manager', 'Admin'],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        const editButton = container.querySelector('button[aria-label="Editar roles de Linus QA"]');
        if (!(editButton instanceof HTMLButtonElement)) {
          throw new Error('Edit roles button not found');
        }

        const chipLabels = Array.from(editButton.querySelectorAll<HTMLElement>('.MuiChip-label')).map(buttonText);
        expect(chipLabels).toEqual(['Admin', 'Manager']);
      });
    } finally {
      await cleanup();
    }
  });

  it('merges repeated API user records before rendering duplicate role edit actions', async () => {
    getUsersMock.mockResolvedValue([
      buildUser({
        id: 303,
        name: 'Ada Lovelace',
        email: 'ada@example.com',
        phone: null,
        roles: ['Admin'],
      }),
      buildUser({
        id: 303,
        name: '   ',
        email: null,
        phone: '+593999000111',
        roles: ['Manager', 'Admin'],
      }),
      buildUser({
        id: 304,
        name: 'Grace Hopper',
        email: 'grace@example.com',
        phone: null,
        roles: ['Teacher'],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
        expect(container.querySelectorAll('button[aria-label="Editar roles de Ada Lovelace"]')).toHaveLength(1);
        expect(container.textContent?.match(/Ada Lovelace/g) ?? []).toHaveLength(1);

        const mergedRow = getRowByName(container, 'Ada Lovelace');
        expect(getContactCellText(mergedRow)).toBe('ada@example.com · +593999000111');

        const editButton = mergedRow.querySelector('button[aria-label="Editar roles de Ada Lovelace"]');
        if (!(editButton instanceof HTMLButtonElement)) {
          throw new Error('Edit roles button not found');
        }

        const chipLabels = Array.from(editButton.querySelectorAll<HTMLElement>('.MuiChip-label')).map(buttonText);
        expect(chipLabels).toEqual(['Admin', 'Manager']);
      });
    } finally {
      await cleanup();
    }
  });

  it('normalizes API role casing before rendering editable role chips', async () => {
    getUsersMock.mockResolvedValue([
      buildUser({
        id: 305,
        name: 'Linus QA',
        email: 'linus@example.com',
        roles: ['admin', 'Admin', ' manager ', 'Manager'],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        const editButton = container.querySelector('button[aria-label="Editar roles de Linus QA"]');
        if (!(editButton instanceof HTMLButtonElement)) {
          throw new Error('Edit roles button not found');
        }

        const chipLabels = Array.from(editButton.querySelectorAll<HTMLElement>('.MuiChip-label')).map(buttonText);
        expect(chipLabels).toEqual(['Admin', 'Manager']);
        expect(chipLabels).not.toContain('admin');
        expect(chipLabels).not.toContain('manager');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps empty role assignments explicit when the admin opens the edit dialog', async () => {
    getUsersMock.mockResolvedValue([
      buildUser({
        id: 306,
        name: 'Nina Sin Roles',
        email: 'nina@example.com',
        roles: [],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Nina Sin Roles');
        expect(container.textContent).toContain('Sin roles');
        expect(container.textContent).not.toContain('No roles');
      });

      const editButton = container.querySelector('button[aria-label="Editar roles de Nina Sin Roles"]');
      if (!(editButton instanceof HTMLButtonElement)) {
        throw new Error('Edit roles button not found');
      }

      await act(async () => {
        editButton.dispatchEvent(new MouseEvent('click', { bubbles: true }));
        await flushPromises();
      });

      await waitForExpectation(() => {
        const dialog = document.body.querySelector('[role="dialog"]');
        if (!(dialog instanceof HTMLElement)) {
          throw new Error('Edit roles dialog not found');
        }

        const rolesSelect = dialog.querySelector('[role="combobox"]');
        if (!(rolesSelect instanceof HTMLElement)) {
          throw new Error('Roles select not found');
        }

        expect(buttonText(rolesSelect)).toBe('Sin roles');
        expect(dialog.textContent).not.toContain('No roles');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps save disabled until the admin makes a real role change', async () => {
    getUsersMock.mockResolvedValue([
      buildUser({
        id: 304,
        name: 'Linus QA',
        email: 'linus@example.com',
        roles: ['Admin', 'Manager'],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        const editButton = container.querySelector('button[aria-label="Editar roles de Linus QA"]');
        expect(editButton).not.toBeNull();
      });

      const editButton = container.querySelector('button[aria-label="Editar roles de Linus QA"]');
      if (!(editButton instanceof HTMLButtonElement)) {
        throw new Error('Edit roles button not found');
      }

      await act(async () => {
        editButton.dispatchEvent(new MouseEvent('click', { bubbles: true }));
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain(
          'Sin cambios pendientes. Modifica la selección para habilitar Guardar cambios.',
        );
        const saveButton = getButtonsByText(document.body, 'Guardar cambios')[0];
        expect(saveButton).toBeInstanceOf(HTMLButtonElement);
        expect((saveButton as HTMLButtonElement).disabled).toBe(true);
        expect(updateUserRolesMock).not.toHaveBeenCalled();
      });
    } finally {
      await cleanup();
    }
  });

  it('summarizes the exact pending role addition before saving permissions', async () => {
    getUsersMock.mockResolvedValue([
      buildUser({
        id: 305,
        name: 'Linus QA',
        email: 'linus@example.com',
        roles: ['Admin'],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        const editButton = container.querySelector('button[aria-label="Editar roles de Linus QA"]');
        expect(editButton).not.toBeNull();
      });

      const editButton = container.querySelector('button[aria-label="Editar roles de Linus QA"]');
      if (!(editButton instanceof HTMLButtonElement)) {
        throw new Error('Edit roles button not found');
      }

      await act(async () => {
        editButton.dispatchEvent(new MouseEvent('click', { bubbles: true }));
        await flushPromises();
      });

      const rolesSelect = document.body.querySelector('[role="combobox"]');
      if (!(rolesSelect instanceof HTMLElement)) {
        throw new Error('Roles select not found');
      }

      await act(async () => {
        rolesSelect.dispatchEvent(new MouseEvent('mousedown', { bubbles: true }));
        await flushPromises();
      });

      await act(async () => {
        getMenuItemByText('Manager').dispatchEvent(new MouseEvent('click', { bubbles: true }));
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Cambio pendiente: agregar Manager.');
        expect(document.body.textContent).not.toContain('Listo para guardar esta actualización de permisos.');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps failed role saves inside the edit dialog so admins can retry without losing the list', async () => {
    updateUserRolesMock.mockRejectedValue(new Error('No se pudieron guardar permisos'));
    getUsersMock.mockResolvedValue([
      buildUser({
        id: 307,
        name: 'Linus QA',
        email: 'linus@example.com',
        roles: ['Admin'],
      }),
      buildUser({
        id: 308,
        name: 'Ada Lovelace',
        email: 'ada@example.com',
        roles: ['Teacher'],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        const editButton = container.querySelector('button[aria-label="Editar roles de Linus QA"]');
        expect(editButton).not.toBeNull();
      });

      const editButton = container.querySelector('button[aria-label="Editar roles de Linus QA"]');
      if (!(editButton instanceof HTMLButtonElement)) {
        throw new Error('Edit roles button not found');
      }

      await act(async () => {
        editButton.dispatchEvent(new MouseEvent('click', { bubbles: true }));
        await flushPromises();
      });

      const rolesSelect = document.body.querySelector('[role="combobox"]');
      if (!(rolesSelect instanceof HTMLElement)) {
        throw new Error('Roles select not found');
      }

      await act(async () => {
        rolesSelect.dispatchEvent(new MouseEvent('mousedown', { bubbles: true }));
        await flushPromises();
      });

      await act(async () => {
        getMenuItemByText('Manager').dispatchEvent(new MouseEvent('click', { bubbles: true }));
        await flushPromises();
      });

      const saveButton = getButtonsByText(document.body, 'Guardar cambios')[0];
      if (!(saveButton instanceof HTMLButtonElement)) {
        throw new Error('Save roles button not found');
      }

      await act(async () => {
        saveButton.dispatchEvent(new MouseEvent('click', { bubbles: true }));
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(updateUserRolesMock).toHaveBeenCalledWith(307, ['Admin', 'Manager']);

        const dialog = document.body.querySelector('[role="dialog"]');
        if (!(dialog instanceof HTMLElement)) {
          throw new Error('Edit roles dialog not found');
        }

        expect(dialog.textContent).toContain('No se pudieron guardar permisos');
        expect(dialog.textContent).toContain('Cambio pendiente: agregar Manager.');
        const retrySaveButton = getButtonsByText(dialog, 'Guardar cambios')[0];
        expect(retrySaveButton).toBeInstanceOf(HTMLButtonElement);
        expect((retrySaveButton as HTMLButtonElement).disabled).toBe(false);
        expect(container.textContent).toContain('Roles y permisos');
        expect(container.textContent).toContain('Linus QA');
        expect(container.textContent).toContain('Ada Lovelace');
        expect(container.querySelector('[role="alert"]')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('uses the status column as an exception marker when a mixed roster includes inactive accounts', async () => {
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
        expect(getHeaders(container)).toEqual(['Usuario', 'Contacto', 'Estado', 'Roles editables']);
        expect(countExactText(container, 'Roles editables')).toBe(1);
        expect(container.textContent).not.toContain('Haz clic sobre los roles para editarlos sin salir de esta tabla.');
        expect(container.textContent).not.toContain('Editar aquí');
        expect(container.textContent).toContain(
          'Vista actual: la columna Estado solo marca las cuentas inactivas; las activas quedan implícitas.',
        );

        const adaRow = getRowByName(container, 'Ada Lovelace');
        expect(adaRow.textContent).not.toContain('Active');
        expect(adaRow.textContent).not.toContain('Inactive');
        expect(adaRow.textContent).not.toContain('Activo');
        expect(adaRow.textContent).not.toContain('Inactivo');

        const graceRow = getRowByName(container, 'Grace Hopper');
        expect(graceRow.textContent).toContain('Inactivo');
        expect(graceRow.textContent).not.toContain('Inactive');
      });
    } finally {
      await cleanup();
    }
  });

  it('summarizes all-inactive rosters instead of repeating the same status on every row', async () => {
    getUsersMock.mockResolvedValue([
      buildUser({
        id: 411,
        name: 'Ada Lovelace',
        status: 'Inactive',
      }),
      buildUser({
        id: 412,
        name: 'Grace Hopper',
        status: 'Inactive',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderComponent(container);

    try {
      await waitForExpectation(() => {
        expect(getHeaders(container)).toEqual(['Usuario', 'Contacto', 'Roles editables']);
        expect(container.textContent).toContain(
          'Vista actual: todas las cuentas administrables están inactivas; la columna de estado volverá cuando haya cuentas activas e inactivas para comparar.',
        );
        expect(countExactText(container, 'Roles editables')).toBe(1);
        expect(countExactText(container, 'Inactivo')).toBe(0);
        expect(container.textContent).not.toContain('Inactive');
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
          'Vista actual: la columna de contacto sigue oculta hasta que exista al menos un email o teléfono y la columna de estado sigue oculta mientras todas las cuentas sigan activas.',
        );
        expect(container.textContent).not.toContain('Haz clic sobre los roles para editarlos sin salir de esta tabla.');
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
