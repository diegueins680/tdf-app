import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { AdminUser } from '../api/admin';

const listUsersMock = jest.fn<(includeInactive?: boolean) => Promise<AdminUser[]>>();

jest.unstable_mockModule('../api/admin', () => ({
  Admin: {
    listUsers: (includeInactive?: boolean) => listUsersMock(includeInactive),
  },
}));

jest.unstable_mockModule('../components/AdminUserCommunicationDialog', () => ({
  default: ({ open, user }: { open: boolean; user: AdminUser | null }) =>
    open ? <div>Dialogo abierto para {user?.username ?? 'sin usuario'}</div> : null,
}));

const { default: AdminUsersPage } = await import('./AdminUsersPage');

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

const renderPage = async (container: HTMLElement) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter
        initialEntries={['/configuracion/usuarios-admin']}
        future={{ v7_startTransition: true, v7_relativeSplatPath: true }}
      >
        <QueryClientProvider client={qc}>
          <AdminUsersPage />
        </QueryClientProvider>
      </MemoryRouter>,
    );
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
      qc.clear();
      document.body.removeChild(container);
    },
  };
};

const buttonText = (element: Element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim();

const getButtonsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll<HTMLElement>('button, a')).filter((element) => buttonText(element) === labelText);

const clickButton = async (button: HTMLElement) => {
  await act(async () => {
    button.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
  });
};

const changeInputValue = async (input: HTMLInputElement, value: string) => {
  const valueSetter = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value')?.set;
  if (!valueSetter) throw new Error('HTMLInputElement value setter not found');

  await act(async () => {
    valueSetter.call(input, value);
    input.dispatchEvent(new Event('input', { bubbles: true }));
    input.dispatchEvent(new Event('change', { bubbles: true }));
    await flushPromises();
  });
};

const getInputByLabelText = (root: ParentNode, labelText: string) => {
  const label = Array.from(root.querySelectorAll<HTMLLabelElement>('label')).find(
    (element) => buttonText(element) === labelText,
  );
  if (!label) throw new Error(`Input label not found: ${labelText}`);

  const inputId = label.htmlFor;
  if (!inputId) throw new Error(`Input label has no associated control: ${labelText}`);

  const input = label.ownerDocument.getElementById(inputId);
  if (!(input instanceof HTMLInputElement)) throw new Error(`Input not found for label: ${labelText}`);

  return input;
};

const buildUser = (overrides: Partial<AdminUser> = {}): AdminUser => ({
  userId: 101,
  partyId: 9,
  partyName: 'Ada Lovelace',
  username: 'ada',
  primaryEmail: 'ada@example.com',
  primaryPhone: '+593999000111',
  whatsapp: null,
  active: true,
  roles: ['Admin'],
  modules: ['admin'],
  ...overrides,
});

const getRowByUserId = (container: HTMLElement, userId: number) => {
  const row = container.querySelector<HTMLElement>(`[data-testid="admin-user-row-${userId}"]`);
  if (!row) throw new Error(`Row not found for user ${userId}`);
  return row;
};

describe('AdminUsersPage', () => {
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
    listUsersMock.mockReset();
    listUsersMock.mockResolvedValue([]);
  });

  it('replaces empty list chrome with a first-user empty state', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listUsersMock).toHaveBeenCalledWith(false);
        expect(container.textContent).toContain(
          'No hay usuarios todavía. Cuando exista el primero, aquí aparecerán búsqueda, filtros y señales de contacto para revisar la lista más rápido.',
        );
        expect(container.textContent).not.toContain('Buscar usuarios');
        expect(container.textContent).not.toContain('0 usuarios');
        expect(container.textContent).not.toContain('Incluir inactivos');
        expect(container.querySelector('[data-testid^="admin-user-row-"]')).toBeNull();
        expect(container.querySelector('button[aria-label="Refrescar lista de usuarios"]')).not.toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('shows only real contact channels in each row so partial contact info stays scan-friendly', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        username: 'email-only',
        primaryEmail: 'email@example.com',
        primaryPhone: null,
        whatsapp: null,
      }),
      buildUser({
        userId: 102,
        username: 'phone-only',
        primaryEmail: null,
        primaryPhone: '+593999000222',
        whatsapp: null,
      }),
      buildUser({
        userId: 103,
        username: 'with-whatsapp',
        primaryEmail: 'whatsapp@example.com',
        primaryPhone: '+593999000333',
        whatsapp: '+593999000444',
      }),
      buildUser({
        userId: 104,
        username: 'no-contact',
        primaryEmail: null,
        primaryPhone: null,
        whatsapp: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const emailOnlyRow = getRowByUserId(container, 101);
        expect(emailOnlyRow.textContent).toContain('email@example.com');
        expect(emailOnlyRow.textContent).not.toContain('Sin teléfono');
        expect(emailOnlyRow.textContent).not.toContain('Sin correo');

        const phoneOnlyRow = getRowByUserId(container, 102);
        expect(phoneOnlyRow.textContent).toContain('+593999000222');
        expect(phoneOnlyRow.textContent).not.toContain('Sin teléfono');
        expect(phoneOnlyRow.textContent).not.toContain('Sin correo');

        const whatsappRow = getRowByUserId(container, 103);
        expect(whatsappRow.textContent).toContain('+593999000444 · whatsapp@example.com');
        expect(whatsappRow.textContent).not.toContain('+593999000333');

        const noContactRow = getRowByUserId(container, 104);
        expect(noContactRow.textContent).not.toContain('Sin teléfono');
        expect(noContactRow.textContent).not.toContain('Sin correo');
        expect(noContactRow.textContent).not.toContain('Sin WhatsApp, teléfono ni correo.');
        expect(noContactRow.textContent).toContain('Falta contacto');
      });
    } finally {
      await cleanup();
    }
  });

  it('shows communication only for users with a real contact channel and explains missing-contact rows', async () => {
    listUsersMock.mockResolvedValue([
      buildUser(),
      buildUser({
        userId: 102,
        partyId: 10,
        partyName: 'Grace Hopper',
        username: 'grace-admin',
        primaryEmail: '   ',
        primaryPhone: null,
        whatsapp: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listUsersMock).toHaveBeenCalledWith(false);
        expect(getButtonsByText(container, 'Perfil')).toHaveLength(1);
        expect(getButtonsByText(container, 'Completar contacto')).toHaveLength(1);
        expect(getButtonsByText(container, 'Comunicación')).toHaveLength(1);
        expect(container.textContent).toContain(
          'Comunicación se habilita cuando el usuario ya tiene WhatsApp, teléfono o correo.',
        );
        expect(container.textContent).toContain(
          '1 usuario sigue sin canal de contacto; usa Completar contacto en esa fila.',
        );

        const missingContactRow = getRowByUserId(container, 102);
        expect(missingContactRow.textContent).toContain('Falta contacto');
        expect(missingContactRow.textContent).toContain('Completar contacto');
        expect(missingContactRow.textContent).not.toContain('Perfil');
        expect(missingContactRow.textContent).not.toContain('Sin WhatsApp, teléfono ni correo.');
      });

      await clickButton(getButtonsByText(container, 'Comunicación')[0]!);

      await waitForExpectation(() => {
        expect(container.textContent).toContain('Dialogo abierto para ada');
        expect(container.textContent).not.toContain('Dialogo abierto para grace-admin');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the first admin-user view focused on the lone row instead of showing list search chrome', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        username: 'solo-admin',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Solo hay un usuario por ahora. Cuando exista el segundo, aquí aparecerán búsqueda y resumen de resultados.',
        );
        expect(container.textContent).not.toContain('Buscar usuarios');
        expect(container.textContent).not.toContain('1 usuario');
        expect(getButtonsByText(container, 'Perfil')).toHaveLength(1);
        expect(getButtonsByText(container, 'Comunicación')).toHaveLength(1);
      });
    } finally {
      await cleanup();
    }
  });

  it('summarizes repeated roles and modules once per row so access scope is easier to scan', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 103,
        roles: [' Admin ', '', 'Teacher', 'Admin', 'Teacher'],
        modules: [' admin ', 'crm', 'crm', ''],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const row = getRowByUserId(container, 103);
        expect(row.textContent).toContain('Roles: Admin, Teacher');
        expect(row.textContent).toContain('Módulos: admin, crm');
        expect(row.textContent).not.toContain('Roles: Admin, Teacher, Admin');
        expect(row.textContent).not.toContain('Módulos: admin, crm, crm');
      });
    } finally {
      await cleanup();
    }
  });

  it('filters the page by search query and reports the visible slice for faster admin scanning', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        username: 'ada-admin',
        partyName: 'Ada Lovelace',
        primaryEmail: 'ada@example.com',
      }),
      buildUser({
        userId: 102,
        partyId: 44,
        username: 'grace-ops',
        partyName: 'Grace Hopper',
        primaryEmail: null,
        primaryPhone: '+593999000444',
        roles: ['Manager'],
        modules: ['crm'],
      }),
      buildUser({
        userId: 103,
        partyId: 55,
        username: 'linus-view',
        partyName: 'Linus QA',
        primaryEmail: 'linus@example.com',
        roles: ['ReadOnly'],
        modules: ['inventory'],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('3 usuarios');
        expect(container.textContent).toContain('Buscar usuarios');
        expect(getRowByUserId(container, 101).textContent).toContain('ada-admin');
        expect(getRowByUserId(container, 102).textContent).toContain('grace-ops');
        expect(getRowByUserId(container, 103).textContent).toContain('linus-view');
      });

      const searchInput = getInputByLabelText(container, 'Buscar usuarios');

      await changeInputValue(searchInput, 'grace');

      await waitForExpectation(() => {
        expect(container.textContent).toContain('Mostrando 1 de 3');
        expect(container.querySelector('[data-testid="admin-user-row-101"]')).toBeNull();
        expect(getRowByUserId(container, 102).textContent).toContain('grace-ops');
        expect(container.querySelector('[data-testid="admin-user-row-103"]')).toBeNull();
      });

      await changeInputValue(searchInput, 'sin coincidencias');

      await waitForExpectation(() => {
        expect(container.textContent).toContain('No hay coincidencias para "sin coincidencias".');
        expect(getButtonsByText(container, 'Limpiar búsqueda')).toHaveLength(1);
        expect(container.textContent).not.toContain('Mostrando 0 de 3');
        expect(container.querySelector('[data-testid^="admin-user-row-"]')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('offers one clear-search action when a query hides every admin user', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        username: 'ada-admin',
      }),
      buildUser({
        userId: 102,
        partyId: 44,
        username: 'grace-ops',
        partyName: 'Grace Hopper',
        primaryEmail: null,
        primaryPhone: '+593999000444',
        roles: ['Manager'],
        modules: ['crm'],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Buscar usuarios');
        expect(getRowByUserId(container, 101).textContent).toContain('ada-admin');
        expect(getRowByUserId(container, 102).textContent).toContain('grace-ops');
      });

      const searchInput = getInputByLabelText(container, 'Buscar usuarios');

      await changeInputValue(searchInput, 'sin coincidencias');

      await waitForExpectation(() => {
        expect(container.textContent).toContain('No hay coincidencias para "sin coincidencias".');
        expect(getButtonsByText(container, 'Limpiar búsqueda')).toHaveLength(1);
        expect(container.textContent).not.toContain('Mostrando 0 de 2');
        expect(container.querySelector('[data-testid^="admin-user-row-"]')).toBeNull();
      });

      await clickButton(getButtonsByText(container, 'Limpiar búsqueda')[0]!);

      await waitForExpectation(() => {
        expect(searchInput.value).toBe('');
        expect(getButtonsByText(container, 'Limpiar búsqueda')).toHaveLength(0);
        expect(getRowByUserId(container, 101).textContent).toContain('ada-admin');
        expect(getRowByUserId(container, 102).textContent).toContain('grace-ops');
      });
    } finally {
      await cleanup();
    }
  });
});
