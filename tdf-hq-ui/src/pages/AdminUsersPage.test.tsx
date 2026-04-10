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

const hasExactText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll<HTMLElement>('*')).some(
    (element) => buttonText(element) === labelText,
  );

const countExactText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll<HTMLElement>('*')).filter(
    (element) => buttonText(element) === labelText,
  ).length;

const getButtonsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll<HTMLElement>('button, a')).filter(
    (element) => buttonText(element) === labelText || element.getAttribute('aria-label') === labelText,
  );

const hasLinkWithTextAndHref = (root: ParentNode, labelText: string, href: string) =>
  Array.from(root.querySelectorAll<HTMLAnchorElement>('a')).some(
    (link) => buttonText(link) === labelText && link.getAttribute('href') === href,
  );

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
  if (!inputId) {
    const nestedInput = label.querySelector('input');
    if (!(nestedInput instanceof HTMLInputElement)) {
      throw new Error(`Input label has no associated control: ${labelText}`);
    }
    return nestedInput;
  }

  const input = label.ownerDocument.getElementById(inputId);
  if (!(input instanceof HTMLInputElement)) throw new Error(`Input not found for label: ${labelText}`);

  return input;
};

const getCheckboxByLabelText = (root: ParentNode, labelText: string) => {
  const input = getInputByLabelText(root, labelText);
  if (input.type !== 'checkbox') throw new Error(`Checkbox not found for label: ${labelText}`);
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
        expect(hasExactText(container, 'Usuarios')).toBe(true);
        expect(container.textContent).not.toContain(
          'Busca por identidad, acceso o contacto. Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
        expect(container.textContent).not.toContain('admin API');
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

  it('shows the WhatsApp CTA only for users with a phone-backed channel so email-only rows stay accurate', async () => {
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
        expect(emailOnlyRow.textContent).toContain('WhatsApp pendiente');
        expect(getButtonsByText(emailOnlyRow, 'WhatsApp')).toHaveLength(0);

        const phoneOnlyRow = getRowByUserId(container, 102);
        expect(phoneOnlyRow.textContent).toContain('+593999000222');
        expect(phoneOnlyRow.textContent).not.toContain('Sin teléfono');
        expect(phoneOnlyRow.textContent).not.toContain('Sin correo');
        expect(getButtonsByText(phoneOnlyRow, 'WhatsApp')).toHaveLength(1);

        const whatsappRow = getRowByUserId(container, 103);
        expect(whatsappRow.textContent).toContain('+593999000444 · whatsapp@example.com');
        expect(whatsappRow.textContent).not.toContain('+593999000333');
        expect(getButtonsByText(whatsappRow, 'WhatsApp')).toHaveLength(1);

        const noContactRow = getRowByUserId(container, 104);
        expect(noContactRow.textContent).not.toContain('Sin teléfono');
        expect(noContactRow.textContent).not.toContain('Sin correo');
        expect(noContactRow.textContent).not.toContain('Sin WhatsApp, teléfono ni correo.');
        expect(noContactRow.textContent).toContain('Contacto pendiente');
        expect(noContactRow.textContent).not.toContain('WhatsApp pendiente');
        expect(noContactRow.textContent).not.toContain('Falta contacto');
        expect(getButtonsByText(noContactRow, 'WhatsApp')).toHaveLength(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('shows the person name first and only keeps the system username when it adds identity context', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        partyId: 9,
        partyName: 'Ada Lovelace',
        username: 'ada-admin',
      }),
      buildUser({
        userId: 102,
        partyId: 10,
        partyName: 'grace',
        username: 'grace',
      }),
      buildUser({
        userId: 103,
        partyId: 11,
        partyName: '   ',
        username: 'linus-view',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const namedRow = getRowByUserId(container, 101);
        expect(hasExactText(namedRow, 'Ada Lovelace')).toBe(true);
        expect(hasExactText(namedRow, 'Usuario: ada-admin · ID 9')).toBe(true);

        const sameIdentityRow = getRowByUserId(container, 102);
        expect(hasExactText(sameIdentityRow, 'grace')).toBe(true);
        expect(hasExactText(sameIdentityRow, 'ID 10')).toBe(true);
        expect(sameIdentityRow.textContent).not.toContain('Usuario: grace');

        const usernameOnlyRow = getRowByUserId(container, 103);
        expect(hasExactText(usernameOnlyRow, 'linus-view')).toBe(true);
        expect(hasExactText(usernameOnlyRow, 'ID 11')).toBe(true);
        expect(usernameOnlyRow.textContent).not.toContain('Usuario: linus-view');
      });
    } finally {
      await cleanup();
    }
  });

  it('moves profile access into the linked identity so each row keeps only one explicit CTA', async () => {
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
        roles: ['Manager'],
        modules: ['crm'],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listUsersMock).toHaveBeenCalledWith(false);
        expect(getButtonsByText(container, 'Abrir perfil')).toHaveLength(0);
        expect(getButtonsByText(container, 'Completar contacto')).toHaveLength(0);
        expect(getButtonsByText(container, 'WhatsApp')).toHaveLength(1);
        expect(container.textContent).toContain(
          '2 usuarios en esta vista. 1 listo para WhatsApp y 1 pendiente de WhatsApp.',
        );
        expect(container.textContent).toContain(
          'Busca por identidad, acceso o contacto. Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
        expect(container.textContent).not.toContain('1 usuario sigue sin canal de contacto');
        expect(container.textContent).not.toContain('1 sin contacto');

        const readyContactRow = getRowByUserId(container, 101);
        expect(readyContactRow.textContent).not.toContain('Abrir perfil');
        expect(readyContactRow.textContent).not.toContain('Ver perfil');
        expect(readyContactRow.textContent).not.toContain('Perfil y contacto');
        expect(readyContactRow.querySelectorAll('button')).toHaveLength(1);
        expect(hasLinkWithTextAndHref(readyContactRow, 'Ada Lovelace', '/perfil/9')).toBe(true);

        const missingContactRow = getRowByUserId(container, 102);
        expect(missingContactRow.textContent).toContain('Contacto pendiente');
        expect(missingContactRow.textContent).not.toContain('WhatsApp pendiente');
        expect(missingContactRow.textContent).not.toContain('Abrir perfil');
        expect(missingContactRow.textContent).not.toContain('Falta contacto');
        expect(missingContactRow.textContent).not.toContain('Sin WhatsApp, teléfono ni correo.');
        expect(missingContactRow.querySelectorAll('button')).toHaveLength(0);
        expect(missingContactRow.querySelectorAll('a')).toHaveLength(1);
        expect(hasLinkWithTextAndHref(missingContactRow, 'Grace Hopper', '/perfil/10')).toBe(true);
        expect(
          Array.from(missingContactRow.querySelectorAll<HTMLAnchorElement>('a')).filter(
            (link) => link.getAttribute('href') === '/perfil/10',
          ),
        ).toHaveLength(1);
      });

      await clickButton(getButtonsByText(container, 'WhatsApp')[0]!);

      await waitForExpectation(() => {
        expect(container.textContent).toContain('Dialogo abierto para ada');
        expect(container.textContent).not.toContain('Dialogo abierto para grace-admin');
      });
    } finally {
      await cleanup();
    }
  });

  it('collapses the default multi-user guidance into one summary line instead of stacked helper copy', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        username: 'ada-admin',
      }),
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
        expect(container.textContent).toContain(
          '2 usuarios en esta vista. 1 listo para WhatsApp y 1 pendiente de WhatsApp. Vista actual: solo usuarios activos. Activa Incluir inactivos si necesitas revisar cuentas deshabilitadas.',
        );
        expect(countExactText(
          container,
          'Busca por identidad, acceso o contacto. Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        )).toBe(1);
        expect(countExactText(
          container,
          '2 usuarios en esta vista. 1 listo para WhatsApp y 1 pendiente de WhatsApp.',
        )).toBe(0);
        expect(countExactText(
          container,
          'Vista actual: solo usuarios activos. Activa Incluir inactivos si necesitas revisar cuentas deshabilitadas.',
        )).toBe(0);
        expect(countExactText(container, 'Haz clic en el nombre para abrir el perfil.')).toBe(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('summarizes the default active-only scope once and only marks inactive rows when inactive accounts are included', async () => {
    listUsersMock.mockImplementation((includeInactive = false) => Promise.resolve(
      includeInactive
        ? [
            buildUser({
              userId: 101,
              partyId: 9,
              username: 'ada-admin',
            }),
            buildUser({
              userId: 102,
              partyId: 10,
              partyName: 'Grace Hopper',
              username: 'grace-admin',
              active: false,
              primaryEmail: 'grace@example.com',
            }),
          ]
        : [
            buildUser({
              userId: 101,
              partyId: 9,
              username: 'ada-admin',
            }),
            buildUser({
              userId: 103,
              partyId: 11,
              partyName: 'Linus View',
              username: 'linus-view',
              primaryEmail: 'linus@example.com',
            }),
          ],
    ));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Vista actual: solo usuarios activos. Activa Incluir inactivos si necesitas revisar cuentas deshabilitadas.',
        );
        expect(countExactText(container, 'Activo')).toBe(0);
        expect(countExactText(container, 'Inactivo')).toBe(0);
        expect(hasExactText(getRowByUserId(container, 101), 'Ada Lovelace')).toBe(true);
        expect(hasExactText(getRowByUserId(container, 103), 'Linus View')).toBe(true);
      });

      const includeInactiveCheckbox = getCheckboxByLabelText(container, 'Incluir inactivos');

      await clickButton(includeInactiveCheckbox);

      await waitForExpectation(() => {
        expect(listUsersMock).toHaveBeenLastCalledWith(true);
        expect(container.textContent).not.toContain(
          'Vista actual: solo usuarios activos. Activa Incluir inactivos si necesitas revisar cuentas deshabilitadas.',
        );
        expect(hasExactText(getRowByUserId(container, 101), 'Activo')).toBe(false);
        expect(hasExactText(getRowByUserId(container, 102), 'Inactivo')).toBe(true);
      });
    } finally {
      await cleanup();
    }
  });

  it('hides the page-level missing-contact warning when every visible row already needs the same fix', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 201,
        partyId: 21,
        username: 'ada-no-contact',
        primaryEmail: null,
        primaryPhone: null,
        whatsapp: null,
      }),
      buildUser({
        userId: 202,
        partyId: 22,
        username: 'grace-no-contact',
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
        expect(container.textContent).not.toContain(
          'WhatsApp se habilita cuando el usuario ya tiene un número disponible.',
        );
        expect(container.textContent).not.toContain('2 sin contacto');
        expect(getButtonsByText(container, 'Completar contacto')).toHaveLength(0);
        expect(getButtonsByText(container, 'WhatsApp')).toHaveLength(0);
        expect(container.textContent).not.toContain('Falta contacto');

        const firstRow = getRowByUserId(container, 201);
        const secondRow = getRowByUserId(container, 202);
        expect(firstRow.textContent).toContain('Contacto pendiente');
        expect(secondRow.textContent).toContain('Contacto pendiente');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the first admin-user view focused on one combined first-user hint instead of stacked helper copy', async () => {
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
          'Solo hay un usuario por ahora. Revisa su perfil desde el nombre y usa WhatsApp si ya tiene un número disponible. Cuando exista el segundo, aquí aparecerán búsqueda y resumen de resultados.',
        );
        expect(
          container.textContent?.includes('Haz clic en el nombre para abrir el perfil.'),
        ).toBe(false);
        expect(container.textContent).not.toContain(
          'Busca por identidad, acceso o contacto. Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
        expect(container.textContent).not.toContain('Buscar usuarios');
        expect(container.textContent).not.toContain('1 usuario');
        expect(getButtonsByText(container, 'Abrir perfil')).toHaveLength(0);
        expect(getButtonsByText(container, 'WhatsApp')).toHaveLength(1);

        const loneRow = getRowByUserId(container, 101);
        expect(loneRow.querySelectorAll('button')).toHaveLength(1);
        expect(hasLinkWithTextAndHref(loneRow, 'Ada Lovelace', '/perfil/9')).toBe(true);
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

  it('moves shared access scope into one page summary when every visible user repeats the same roles and modules', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        username: 'ada-admin',
        roles: [' Admin ', 'Teacher', 'Admin'],
        modules: [' admin ', 'crm', 'crm'],
      }),
      buildUser({
        userId: 102,
        partyId: 44,
        username: 'grace-admin',
        partyName: 'Grace Hopper',
        primaryEmail: 'grace@example.com',
        roles: ['Teacher', 'Admin', 'Teacher'],
        modules: ['crm', 'admin', 'crm'],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Acceso compartido en esta vista: Roles: Admin, Teacher · Módulos: admin, crm.',
        );

        const firstRow = getRowByUserId(container, 101);
        const secondRow = getRowByUserId(container, 102);
        expect(firstRow.textContent).not.toContain('Roles:');
        expect(firstRow.textContent).not.toContain('Módulos:');
        expect(secondRow.textContent).not.toContain('Roles:');
        expect(secondRow.textContent).not.toContain('Módulos:');
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
        expect(container.textContent).toContain('3 usuarios en esta vista.');
        expect(container.textContent).toContain('Buscar usuarios');
        expect(getRowByUserId(container, 101).textContent).toContain('ada-admin');
        expect(getRowByUserId(container, 102).textContent).toContain('grace-ops');
        expect(getRowByUserId(container, 103).textContent).toContain('linus-view');
      });

      const searchInput = getInputByLabelText(container, 'Buscar usuarios');

      await changeInputValue(searchInput, 'grace');

      await waitForExpectation(() => {
        expect(container.textContent).toContain('Mostrando 1 de 3');
        expect(getButtonsByText(container, 'Limpiar búsqueda')).toHaveLength(1);
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

  it('hides the generic intro while a search is active so header guidance stays focused on the results', async () => {
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
        expect(container.textContent).toContain(
          'Busca por identidad, acceso o contacto. Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
      });

      const searchInput = getInputByLabelText(container, 'Buscar usuarios');

      await changeInputValue(searchInput, 'grace');

      await waitForExpectation(() => {
        expect(container.textContent).not.toContain(
          'Busca por identidad, acceso o contacto. Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
        expect(container.textContent).toContain('Mostrando 1 de 2 usuarios.');
      });

      await changeInputValue(searchInput, '');

      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Busca por identidad, acceso o contacto. Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps search guidance inside the field instead of repeating the same hint in the header summary', async () => {
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
        const searchInput = getInputByLabelText(container, 'Buscar usuarios');
        expect(searchInput.getAttribute('placeholder')).toBe('Usuario, nombre, ID, contacto o acceso');
        expect(container.textContent).toContain('2 usuarios en esta vista.');
        expect(container.textContent).not.toContain('Busca por nombre, ID, contacto o acceso.');
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
