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

const getRenderedRowUserIds = (container: HTMLElement) => (
  Array.from(container.querySelectorAll<HTMLElement>('[data-testid^="admin-user-row-"]'))
    .map((row) => Number(row.dataset['testid']?.replace('admin-user-row-', '')))
);

const getPageGuidance = (container: HTMLElement) => {
  const guidance = container.querySelector<HTMLElement>('[data-testid="admin-users-page-guidance"]');
  if (!guidance) throw new Error('Page guidance not found');
  return buttonText(guidance);
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
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
        expect(container.textContent).not.toContain('admin API');
        expect(container.textContent).toContain(
          'No hay usuarios todavía. Cuando exista el primero, aquí aparecerán búsqueda, filtros y señales de contacto para revisar la lista más rápido.',
        );
        expect(container.textContent).not.toContain('Buscar usuarios');
        expect(container.textContent).not.toContain('0 usuarios');
        expect(container.textContent).not.toContain('Incluir inactivos');
        expect(container.querySelector('[data-testid^="admin-user-row-"]')).toBeNull();
        expect(container.querySelector('button[aria-label="Refrescar lista de usuarios"]')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the initial loading state focused on first-user setup instead of a refresh action', async () => {
    listUsersMock.mockImplementation(() => new Promise(() => {}));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(hasExactText(container, 'Usuarios')).toBe(true);
        expect(container.textContent).toContain('Cargando usuarios…');
        expect(container.querySelector('button[aria-label="Refrescar lista de usuarios"]')).toBeNull();
        expect(container.textContent).not.toContain(
          'No hay usuarios todavía. Cuando exista el primero, aquí aparecerán búsqueda, filtros y señales de contacto para revisar la lista más rápido.',
        );
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps refresh visible once the page has administrable users to revisit', async () => {
    listUsersMock.mockResolvedValue([
      buildUser(),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Solo hay un usuario por ahora. Abre su perfil desde el nombre y usa WhatsApp si ya tiene un número disponible. Cuando la lista crezca, aquí aparecerán búsqueda y resumen de resultados.',
        );
        expect(container.querySelector('button[aria-label="Refrescar lista de usuarios"]')).not.toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps mixed contact-state work in the header so rows only show the available WhatsApp action', async () => {
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
        expect(emailOnlyRow.textContent).not.toContain('WhatsApp pendiente');
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
        expect(noContactRow.textContent).not.toContain('Contacto pendiente');
        expect(noContactRow.textContent).not.toContain('WhatsApp pendiente');
        expect(noContactRow.textContent).not.toContain('Falta contacto');
        expect(getButtonsByText(noContactRow, 'WhatsApp')).toHaveLength(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('distinguishes missing WhatsApp from missing contact in the page summary', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        username: 'with-phone',
        primaryEmail: 'ready@example.com',
        primaryPhone: '+593999000111',
        whatsapp: null,
      }),
      buildUser({
        userId: 102,
        username: 'email-only',
        primaryEmail: 'email@example.com',
        primaryPhone: null,
        whatsapp: null,
      }),
      buildUser({
        userId: 103,
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
        expect(getPageGuidance(container)).toBe(
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible. 3 usuarios en esta vista. 1 listo para WhatsApp, 1 pendiente de WhatsApp y 1 pendiente de contacto. Vista actual: solo usuarios activos.',
        );
      });
    } finally {
      await cleanup();
    }
  });

  it('shows the person name first and hides internal ids unless the username adds useful context', async () => {
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
        expect(hasExactText(namedRow, 'Usuario: ada-admin')).toBe(true);
        expect(namedRow.textContent).not.toContain('ID 9');

        const sameIdentityRow = getRowByUserId(container, 102);
        expect(hasExactText(sameIdentityRow, 'grace')).toBe(true);
        expect(sameIdentityRow.textContent).not.toContain('Usuario: grace');
        expect(sameIdentityRow.textContent).not.toContain('ID 10');

        const usernameOnlyRow = getRowByUserId(container, 103);
        expect(hasExactText(usernameOnlyRow, 'linus-view')).toBe(true);
        expect(usernameOnlyRow.textContent).not.toContain('Usuario: linus-view');
        expect(usernameOnlyRow.textContent).not.toContain('ID 11');

        expect(container.textContent).not.toContain('ID 9');
        expect(container.textContent).not.toContain('ID 10');
        expect(container.textContent).not.toContain('ID 11');
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
          '2 usuarios en esta vista. 1 listo para WhatsApp y 1 pendiente de contacto.',
        );
        expect(container.textContent).toContain(
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
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
        expect(missingContactRow.textContent).not.toContain('Contacto pendiente');
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

  it('keeps users without a linked profile as plain text so the row does not imply a broken action', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        partyId: null,
        username: 'solo-sin-party',
        primaryEmail: 'solo@example.com',
        primaryPhone: null,
        whatsapp: null,
      }),
      buildUser({
        userId: 102,
        partyId: 10,
        partyName: 'Grace Hopper',
        username: 'grace-admin',
        primaryEmail: 'grace@example.com',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const missingProfileRow = getRowByUserId(container, 101);
        expect(hasExactText(missingProfileRow, 'Ada Lovelace')).toBe(true);
        expect(missingProfileRow.textContent).toContain('Perfil pendiente');
        expect(hasLinkWithTextAndHref(missingProfileRow, 'Ada Lovelace', '/perfil/null')).toBe(false);
        expect(
          Array.from(missingProfileRow.querySelectorAll<HTMLAnchorElement>('a')).some(
            (link) => (link.getAttribute('href') ?? '').startsWith('/perfil/'),
          ),
        ).toBe(false);
        expect(missingProfileRow.textContent).toContain('solo@example.com');

        const linkedProfileRow = getRowByUserId(container, 102);
        expect(hasLinkWithTextAndHref(linkedProfileRow, 'Grace Hopper', '/perfil/10')).toBe(true);
        expect(linkedProfileRow.textContent).not.toContain('Perfil pendiente');
        expect(container.innerHTML).not.toContain('/perfil/null');
        expect(container.innerHTML).not.toContain('/perfil/undefined');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the multi-user intro focused on available actions when no visible user has a linked profile', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        partyId: null,
        username: 'ada-no-profile',
      }),
      buildUser({
        userId: 102,
        partyId: null,
        partyName: 'Grace Hopper',
        username: 'grace-no-profile',
        primaryEmail: 'grace@example.com',
        primaryPhone: '+593999000222',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getPageGuidance(container)).toBe(
          'Usa WhatsApp cuando haya un número disponible. El acceso al perfil aparecerá desde el nombre cuando el usuario ya tenga un perfil vinculado. 2 usuarios en esta vista. La búsqueda aparecerá desde el tercer usuario. Vista actual: solo usuarios activos.',
        );
        expect(container.textContent).not.toContain(
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
        expect(getRowByUserId(container, 101).querySelectorAll('a')).toHaveLength(0);
        expect(getRowByUserId(container, 102).querySelectorAll('a')).toHaveLength(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps two-user rosters focused by waiting to show search until the list is denser', async () => {
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
        primaryEmail: 'grace@example.com',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getPageGuidance(container)).toBe(
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible. 2 usuarios en esta vista. La búsqueda aparecerá desde el tercer usuario. Vista actual: solo usuarios activos.',
        );
        expect(container.textContent).not.toContain('Buscar usuarios');
        expect(getRowByUserId(container, 101).textContent).toContain('ada-admin');
        expect(getRowByUserId(container, 102).textContent).toContain('grace-admin');
      });
    } finally {
      await cleanup();
    }
  });

  it('deduplicates repeated user records before showing extra rows or search controls', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        username: 'ada-admin',
      }),
      buildUser({
        userId: 101,
        username: 'ada-admin',
      }),
      buildUser({
        userId: 102,
        partyId: 10,
        partyName: 'Grace Hopper',
        username: 'grace-admin',
        primaryEmail: 'grace@example.com',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getPageGuidance(container)).toBe(
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible. 2 usuarios en esta vista. La búsqueda aparecerá desde el tercer usuario. Vista actual: solo usuarios activos.',
        );
        expect(container.textContent).not.toContain('Buscar usuarios');
        expect(getRenderedRowUserIds(container)).toEqual([101, 102]);
        expect(getButtonsByText(container, 'WhatsApp')).toHaveLength(2);
      });
    } finally {
      await cleanup();
    }
  });

  it('collapses the default multi-user guidance into one summary line and skips the baseline admin access copy', async () => {
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
        expect(getPageGuidance(container)).toBe(
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible. 2 usuarios en esta vista. 1 listo para WhatsApp y 1 pendiente de contacto. La búsqueda aparecerá desde el tercer usuario. Vista actual: solo usuarios activos.',
        );
        expect(countExactText(
          container,
          '2 usuarios en esta vista. 1 listo para WhatsApp y 1 pendiente de contacto.',
        )).toBe(0);
        expect(countExactText(
          container,
          'La búsqueda aparecerá desde el tercer usuario.',
        )).toBe(0);
        expect(countExactText(
          container,
          'Vista actual: solo usuarios activos.',
        )).toBe(0);
        expect(countExactText(
          container,
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        )).toBe(0);
        expect(container.textContent).not.toContain(
          'Acceso compartido en esta vista: Roles: Admin · Módulos: admin.',
        );
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
          'Vista actual: solo usuarios activos.',
        );
        expect(container.textContent).not.toContain(
          'Activa Incluir inactivos si necesitas revisar cuentas deshabilitadas.',
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
          'Vista actual: solo usuarios activos.',
        );
        expect(hasExactText(getRowByUserId(container, 101), 'Activo')).toBe(false);
        expect(hasExactText(getRowByUserId(container, 102), 'Inactivo')).toBe(true);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the shared missing-contact fix in the header instead of repeating the same chip on every row', async () => {
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
        expect(container.textContent).toContain(
          'Abre el perfil desde el nombre para completar el contacto pendiente. WhatsApp aparecerá cuando haya un número disponible.',
        );
        expect(container.textContent).not.toContain(
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
        expect(container.textContent).not.toContain(
          'WhatsApp se habilita cuando el usuario ya tiene un número disponible.',
        );
        expect(container.textContent).not.toContain('2 sin contacto');
        expect(getButtonsByText(container, 'Completar contacto')).toHaveLength(0);
        expect(getButtonsByText(container, 'WhatsApp')).toHaveLength(0);
        expect(container.textContent).not.toContain('Falta contacto');

        const firstRow = getRowByUserId(container, 201);
        const secondRow = getRowByUserId(container, 202);
        expect(firstRow.textContent).not.toContain('Contacto pendiente');
        expect(secondRow.textContent).not.toContain('Contacto pendiente');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the shared number-setup fix in the header instead of repeating the same chip on every row', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 201,
        partyId: 21,
        username: 'ada-email-only',
        primaryEmail: 'ada@example.com',
        primaryPhone: null,
        whatsapp: null,
      }),
      buildUser({
        userId: 202,
        partyId: 22,
        username: 'grace-email-only',
        primaryEmail: 'grace@example.com',
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
          'Abre el perfil desde el nombre para agregar o corregir un número. WhatsApp aparecerá cuando haya un número disponible.',
        );
        expect(container.textContent).not.toContain(
          'Abre el perfil desde el nombre para completar el contacto pendiente. WhatsApp aparecerá cuando haya un número disponible.',
        );
        expect(container.textContent).not.toContain(
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
        expect(getRowByUserId(container, 201).textContent).not.toContain('WhatsApp pendiente');
        expect(getRowByUserId(container, 202).textContent).not.toContain('WhatsApp pendiente');
        expect(container.textContent).not.toContain('Contacto pendiente');
      });
    } finally {
      await cleanup();
    }
  });

  it('swaps the lone-user intro to number setup guidance when the first admin still lacks a WhatsApp-ready number', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        username: 'solo-no-whatsapp',
        primaryEmail: 'solo@example.com',
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
          'Solo hay un usuario por ahora. Abre su perfil desde el nombre para agregar o corregir un número. Cuando tenga un número disponible, WhatsApp aparecerá aquí. Cuando la lista crezca, aquí aparecerán búsqueda y resumen de resultados.',
        );
        expect(container.textContent).not.toContain(
          'Solo hay un usuario por ahora. Abre su perfil desde el nombre para completar el contacto pendiente. Cuando tenga un número disponible, WhatsApp aparecerá aquí. Cuando la lista crezca, aquí aparecerán búsqueda y resumen de resultados.',
        );
        expect(container.textContent).not.toContain(
          'Solo hay un usuario por ahora. Abre su perfil desde el nombre y usa WhatsApp si ya tiene un número disponible. Cuando la lista crezca, aquí aparecerán búsqueda y resumen de resultados.',
        );
        expect(getButtonsByText(container, 'WhatsApp')).toHaveLength(0);
        expect(getRowByUserId(container, 101).textContent).not.toContain('WhatsApp pendiente');
        expect(getRowByUserId(container, 101).textContent).not.toContain('Contacto pendiente');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the lone-user intro on contact setup guidance when the first admin lacks every contact channel', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        username: 'solo-no-contact',
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
        expect(container.textContent).toContain(
          'Solo hay un usuario por ahora. Abre su perfil desde el nombre para completar el contacto pendiente. Cuando tenga un número disponible, WhatsApp aparecerá aquí. Cuando la lista crezca, aquí aparecerán búsqueda y resumen de resultados.',
        );
        expect(container.textContent).not.toContain(
          'Solo hay un usuario por ahora. Abre su perfil desde el nombre para agregar o corregir un número. Cuando tenga un número disponible, WhatsApp aparecerá aquí. Cuando la lista crezca, aquí aparecerán búsqueda y resumen de resultados.',
        );
        expect(getButtonsByText(container, 'WhatsApp')).toHaveLength(0);
        expect(getRowByUserId(container, 101).textContent).not.toContain('Contacto pendiente');
        expect(getRowByUserId(container, 101).textContent).not.toContain('WhatsApp pendiente');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the lone-user intro honest when the first admin still has no linked profile', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        partyId: null,
        username: 'solo-no-profile',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getPageGuidance(container)).toBe(
          'Solo hay un usuario por ahora. Este usuario todavía no tiene un perfil vinculado, así que el nombre no abre un perfil. Usa WhatsApp si ya tiene un número disponible. Cuando la lista crezca, aquí aparecerán búsqueda y resumen de resultados.',
        );
        expect(container.textContent).not.toContain(
          'Solo hay un usuario por ahora. Abre su perfil desde el nombre y usa WhatsApp si ya tiene un número disponible. Cuando la lista crezca, aquí aparecerán búsqueda y resumen de resultados.',
        );
        expect(getButtonsByText(container, 'WhatsApp')).toHaveLength(1);
        expect(getRowByUserId(container, 101).querySelectorAll('a')).toHaveLength(0);
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
        expect(getPageGuidance(container)).toBe(
          'Solo hay un usuario por ahora. Abre su perfil desde el nombre y usa WhatsApp si ya tiene un número disponible. Cuando la lista crezca, aquí aparecerán búsqueda y resumen de resultados.',
        );
        expect(
          container.textContent?.includes('Haz clic en el nombre para abrir el perfil.'),
        ).toBe(false);
        expect(container.textContent).not.toContain(
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
        expect(container.textContent).not.toContain(
          'Vista actual: solo usuarios activos.',
        );
        expect(container.textContent).not.toContain('Buscar usuarios');
        expect(container.textContent).not.toContain('1 usuario');
        expect(getButtonsByText(container, 'Abrir perfil')).toHaveLength(0);
        expect(getButtonsByText(container, 'WhatsApp')).toHaveLength(1);

        const loneRow = getRowByUserId(container, 101);
        expect(loneRow.querySelectorAll('button')).toHaveLength(1);
        expect(hasLinkWithTextAndHref(loneRow, 'Ada Lovelace', '/perfil/9')).toBe(true);
        expect(loneRow.textContent).not.toContain('Roles:');
        expect(loneRow.textContent).not.toContain('Módulos:');
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
      buildUser({
        userId: 104,
        partyId: 44,
        username: 'grace-admin',
        partyName: 'Grace Hopper',
        primaryEmail: 'grace@example.com',
        roles: ['Manager'],
        modules: ['events'],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const row = getRowByUserId(container, 103);
        expect(hasExactText(row, 'Roles: Admin, Teacher · Módulos: admin, crm')).toBe(true);
        expect(hasExactText(row, 'Roles: Admin, Teacher')).toBe(false);
        expect(hasExactText(row, 'Módulos: admin, crm')).toBe(false);
        expect(row.textContent).not.toContain('Roles: Admin, Teacher, Admin');
        expect(row.textContent).not.toContain('Módulos: admin, crm, crm');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps active users first and alphabetizes the visible list so inactive exceptions stay grouped at the bottom', async () => {
    listUsersMock.mockImplementation((includeInactive = false) => Promise.resolve(
      includeInactive
        ? [
            buildUser({
              userId: 204,
              partyId: 24,
              partyName: 'Zed Inactive',
              username: 'zed-inactive',
              active: false,
            }),
            buildUser({
              userId: 202,
              partyId: 22,
              partyName: 'Bruno Active',
              username: 'bruno-active',
            }),
            buildUser({
              userId: 201,
              partyId: 21,
              partyName: 'Ada Active',
              username: 'ada-active',
            }),
            buildUser({
              userId: 203,
              partyId: 23,
              partyName: 'Carla Inactive',
              username: 'carla-inactive',
              active: false,
            }),
          ]
        : [
            buildUser({
              userId: 202,
              partyId: 22,
              partyName: 'Bruno Active',
              username: 'bruno-active',
            }),
            buildUser({
              userId: 201,
              partyId: 21,
              partyName: 'Ada Active',
              username: 'ada-active',
            }),
          ],
    ));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getRenderedRowUserIds(container)).toEqual([201, 202]);
      });

      const includeInactiveCheckbox = getCheckboxByLabelText(container, 'Incluir inactivos');

      await clickButton(includeInactiveCheckbox);

      await waitForExpectation(() => {
        expect(listUsersMock).toHaveBeenLastCalledWith(true);
        expect(getRenderedRowUserIds(container)).toEqual([201, 202, 203, 204]);
        expect(getRowByUserId(container, 203).textContent).toContain('Inactivo');
        expect(getRowByUserId(container, 204).textContent).toContain('Inactivo');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps shared access scope inside the main page summary when every visible user repeats the same roles and modules', async () => {
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
        expect(getPageGuidance(container)).toBe(
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible. 2 usuarios en esta vista. La búsqueda aparecerá desde el tercer usuario. Vista actual: solo usuarios activos. Acceso compartido en esta vista: Roles: Admin, Teacher · Módulos: admin, crm.',
        );
        expect(countExactText(
          container,
          'Acceso compartido en esta vista: Roles: Admin, Teacher · Módulos: admin, crm.',
        )).toBe(0);

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
        expect(container.textContent).toContain(
          'No hay coincidencias para "sin coincidencias" entre los usuarios activos. Activa Incluir inactivos si necesitas revisar cuentas deshabilitadas.',
        );
        expect(getButtonsByText(container, 'Limpiar búsqueda')).toHaveLength(1);
        expect(container.textContent).not.toContain('Mostrando 0 de 3');
        expect(container.textContent).not.toContain(
          'Vista actual: solo usuarios activos.',
        );
        expect(container.querySelector('[data-testid^="admin-user-row-"]')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps mixed filtered results focused by summarizing contact blockers once in the header', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        username: 'ada-admin',
        modules: ['admin'],
      }),
      buildUser({
        userId: 102,
        partyId: 44,
        username: 'grace-crm',
        partyName: 'Grace Hopper',
        primaryEmail: 'grace@example.com',
        primaryPhone: null,
        whatsapp: null,
        roles: ['Manager'],
        modules: ['crm'],
      }),
      buildUser({
        userId: 103,
        partyId: 55,
        username: 'bruno-crm',
        partyName: 'Bruno Ops',
        primaryEmail: null,
        primaryPhone: '+593999000555',
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
        expect(container.textContent).toContain('3 usuarios en esta vista.');
        expect(getRowByUserId(container, 102).textContent).toContain('grace-crm');
        expect(getRowByUserId(container, 103).textContent).toContain('bruno-crm');
      });

      const searchInput = getInputByLabelText(container, 'Buscar usuarios');

      await changeInputValue(searchInput, 'crm');

      await waitForExpectation(() => {
        expect(getPageGuidance(container)).toBe(
          'Mostrando 2 de 3 usuarios. 1 listo para WhatsApp y 1 pendiente de WhatsApp. Acceso compartido en esta vista: Roles: Manager · Módulos: crm.',
        );

        const emailOnlyRow = getRowByUserId(container, 102);
        const phoneReadyRow = getRowByUserId(container, 103);
        expect(emailOnlyRow.textContent).toContain('grace@example.com');
        expect(emailOnlyRow.textContent).not.toContain('WhatsApp pendiente');
        expect(emailOnlyRow.textContent).not.toContain('Contacto pendiente');
        expect(getButtonsByText(emailOnlyRow, 'WhatsApp')).toHaveLength(0);
        expect(getButtonsByText(phoneReadyRow, 'WhatsApp')).toHaveLength(1);
      });
    } finally {
      await cleanup();
    }
  });

  it('moves single-result access context into the header so the filtered row stays focused on identity and actions', async () => {
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
        expect(getRowByUserId(container, 102).textContent).toContain('grace-ops');
      });

      const searchInput = getInputByLabelText(container, 'Buscar usuarios');

      await changeInputValue(searchInput, 'grace');

      await waitForExpectation(() => {
        expect(getPageGuidance(container)).toBe(
          'Resultado único. Abre el perfil desde el nombre y usa WhatsApp si ya está disponible. Mostrando 1 de 3 usuarios. Acceso en este resultado: Roles: Manager · Módulos: crm.',
        );

        const resultRow = getRowByUserId(container, 102);
        expect(resultRow.textContent).toContain('Grace Hopper');
        expect(resultRow.textContent).toContain('+593999000444');
        expect(resultRow.textContent).not.toContain('Roles:');
        expect(resultRow.textContent).not.toContain('Módulos:');
        expect(getButtonsByText(resultRow, 'WhatsApp')).toHaveLength(1);
      });
    } finally {
      await cleanup();
    }
  });

  it('skips the baseline admin access summary when a single search result already uses the default admin scope', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        username: 'ada-admin',
        partyName: 'Ada Lovelace',
        primaryEmail: 'ada@example.com',
        roles: ['Admin'],
        modules: ['admin'],
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
        expect(getRowByUserId(container, 101).textContent).toContain('ada-admin');
      });

      const searchInput = getInputByLabelText(container, 'Buscar usuarios');

      await changeInputValue(searchInput, 'ada-admin');

      await waitForExpectation(() => {
        expect(getPageGuidance(container)).toBe(
          'Resultado único. Abre el perfil desde el nombre y usa WhatsApp si ya está disponible. Mostrando 1 de 3 usuarios.',
        );

        const resultRow = getRowByUserId(container, 101);
        expect(resultRow.textContent).toContain('Ada Lovelace');
        expect(resultRow.textContent).not.toContain('Roles:');
        expect(resultRow.textContent).not.toContain('Módulos:');
        expect(getButtonsByText(resultRow, 'WhatsApp')).toHaveLength(1);
        expect(container.textContent).not.toContain(
          'Acceso en este resultado: Roles: Admin · Módulos: admin.',
        );
      });
    } finally {
      await cleanup();
    }
  });

  it('uses number-setup guidance for an email-only single search result so the header matches the row state', async () => {
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
        username: 'grace-email',
        partyName: 'Grace Hopper',
        primaryEmail: 'grace@example.com',
        primaryPhone: null,
        whatsapp: null,
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
        expect(getRowByUserId(container, 102).textContent).toContain('grace-email');
      });

      const searchInput = getInputByLabelText(container, 'Buscar usuarios');

      await changeInputValue(searchInput, 'grace');

      await waitForExpectation(() => {
        expect(getPageGuidance(container)).toBe(
          'Resultado único. Abre el perfil desde el nombre para agregar o corregir un número. WhatsApp aparecerá cuando haya un número disponible. Mostrando 1 de 3 usuarios. Acceso en este resultado: Roles: Manager · Módulos: crm.',
        );

        const resultRow = getRowByUserId(container, 102);
        expect(resultRow.textContent).toContain('Grace Hopper');
        expect(resultRow.textContent).toContain('grace@example.com');
        expect(resultRow.textContent).not.toContain('WhatsApp pendiente');
        expect(resultRow.textContent).not.toContain('Contacto pendiente');
        expect(resultRow.textContent).not.toContain('Roles:');
        expect(resultRow.textContent).not.toContain('Módulos:');
        expect(getButtonsByText(resultRow, 'WhatsApp')).toHaveLength(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps single-result contact setup guidance in the header instead of repeating a pending chip on the row', async () => {
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
        username: 'grace-no-contact',
        partyName: 'Grace Hopper',
        primaryEmail: null,
        primaryPhone: null,
        whatsapp: null,
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
      });

      const searchInput = getInputByLabelText(container, 'Buscar usuarios');

      await changeInputValue(searchInput, 'grace');

      await waitForExpectation(() => {
        expect(getPageGuidance(container)).toBe(
          'Resultado único. Abre el perfil desde el nombre para completar el contacto pendiente. WhatsApp aparecerá cuando haya un número disponible. Mostrando 1 de 3 usuarios. Acceso en este resultado: Roles: Manager · Módulos: crm.',
        );

        const resultRow = getRowByUserId(container, 102);
        expect(resultRow.textContent).toContain('Grace Hopper');
        expect(resultRow.textContent).not.toContain('Contacto pendiente');
        expect(resultRow.textContent).not.toContain('WhatsApp pendiente');
        expect(getButtonsByText(resultRow, 'WhatsApp')).toHaveLength(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps single-result guidance honest when the matched user still has no linked profile', async () => {
    listUsersMock.mockResolvedValue([
      buildUser({
        userId: 101,
        username: 'ada-admin',
        partyName: 'Ada Lovelace',
        primaryEmail: 'ada@example.com',
      }),
      buildUser({
        userId: 102,
        partyId: null,
        username: 'grace-no-profile',
        partyName: 'Grace Hopper',
        primaryEmail: 'grace@example.com',
        primaryPhone: null,
        whatsapp: null,
      }),
      buildUser({
        userId: 103,
        partyId: 55,
        username: 'linus-view',
        partyName: 'Linus QA',
        primaryEmail: 'linus@example.com',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('3 usuarios en esta vista.');
      });

      const searchInput = getInputByLabelText(container, 'Buscar usuarios');

      await changeInputValue(searchInput, 'grace-no-profile');

      await waitForExpectation(() => {
        expect(getPageGuidance(container)).toBe(
          'Resultado único. Este usuario todavía no tiene un perfil vinculado, así que el nombre no abre un perfil. Cuando se vincule, podrás abrirlo desde el nombre para agregar o corregir un número. WhatsApp aparecerá cuando haya un número disponible. Mostrando 1 de 3 usuarios.',
        );
        expect(container.textContent).not.toContain(
          'Resultado único. Abre el perfil desde el nombre para agregar o corregir un número. WhatsApp aparecerá cuando haya un número disponible. Mostrando 1 de 3 usuarios.',
        );
        expect(getRowByUserId(container, 102).querySelectorAll('a')).toHaveLength(0);
        expect(getButtonsByText(getRowByUserId(container, 102), 'WhatsApp')).toHaveLength(0);
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
        expect(container.textContent).toContain(
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
        expect(container.textContent).toContain(
          'Vista actual: solo usuarios activos.',
        );
      });

      const searchInput = getInputByLabelText(container, 'Buscar usuarios');

      await changeInputValue(searchInput, 'grace');

      await waitForExpectation(() => {
        expect(container.textContent).not.toContain(
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
        expect(container.textContent).toContain('Mostrando 1 de 3 usuarios.');
        expect(container.textContent).not.toContain(
          'Vista actual: solo usuarios activos.',
        );
      });

      await changeInputValue(searchInput, '');

      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
        expect(container.textContent).toContain(
          'Vista actual: solo usuarios activos.',
        );
      });
    } finally {
      await cleanup();
    }
  });

  it('only mentions the active-only scope inside the empty search state while inactive accounts are still hidden', async () => {
    listUsersMock.mockImplementation((includeInactive = false) => Promise.resolve(
      includeInactive
        ? [
            buildUser({
              userId: 101,
              username: 'ada-admin',
            }),
            buildUser({
              userId: 102,
              partyId: 44,
              username: 'grace-ops',
              partyName: 'Grace Hopper',
              active: false,
              primaryEmail: 'grace@example.com',
            }),
            buildUser({
              userId: 103,
              partyId: 55,
              username: 'linus-view',
              partyName: 'Linus QA',
              primaryEmail: 'linus@example.com',
            }),
          ]
        : [
            buildUser({
              userId: 101,
              username: 'ada-admin',
            }),
            buildUser({
              userId: 103,
              partyId: 55,
              username: 'linus-view',
              partyName: 'Linus QA',
              primaryEmail: 'linus@example.com',
            }),
            buildUser({
              userId: 104,
              partyId: 66,
              username: 'bruno-admin',
              partyName: 'Bruno Ops',
              primaryEmail: 'bruno@example.com',
            }),
          ],
    ));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Buscar usuarios');
      });

      const searchInput = getInputByLabelText(container, 'Buscar usuarios');
      const includeInactiveCheckbox = getCheckboxByLabelText(container, 'Incluir inactivos');

      await changeInputValue(searchInput, 'sin coincidencias');

      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'No hay coincidencias para "sin coincidencias" entre los usuarios activos. Activa Incluir inactivos si necesitas revisar cuentas deshabilitadas.',
        );
      });

      await clickButton(includeInactiveCheckbox);

      await waitForExpectation(() => {
        expect(listUsersMock).toHaveBeenLastCalledWith(true);
        expect(container.textContent).toContain('No hay coincidencias para "sin coincidencias".');
        expect(container.textContent).not.toContain(
          'No hay coincidencias para "sin coincidencias" entre los usuarios activos. Activa Incluir inactivos si necesitas revisar cuentas deshabilitadas.',
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
        const searchInput = getInputByLabelText(container, 'Buscar usuarios');
        expect(searchInput.getAttribute('placeholder')).toBe('Usuario, nombre, ID, contacto o acceso');
        expect(container.textContent).toContain(
          'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.',
        );
        expect(container.textContent).toContain('3 usuarios en esta vista.');
        expect(container.textContent).not.toContain('La búsqueda aparecerá desde el tercer usuario.');
        expect(container.textContent).not.toContain('Busca por identidad');
        expect(container.textContent).not.toContain('Busca por nombre, ID, contacto o acceso.');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the search-owned clear action available when a query hides every admin user', async () => {
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
        expect(container.textContent).toContain('Buscar usuarios');
        expect(getRowByUserId(container, 101).textContent).toContain('ada-admin');
        expect(getRowByUserId(container, 102).textContent).toContain('grace-ops');
        expect(getRowByUserId(container, 103).textContent).toContain('linus-view');
      });

      const searchInput = getInputByLabelText(container, 'Buscar usuarios');

      await changeInputValue(searchInput, 'sin coincidencias');

      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'No hay coincidencias para "sin coincidencias" entre los usuarios activos. Activa Incluir inactivos si necesitas revisar cuentas deshabilitadas.',
        );
        expect(getButtonsByText(container, 'Limpiar búsqueda')).toHaveLength(1);
        expect(container.querySelector('[data-testid="admin-users-empty-search-reset"]')).toBeNull();
        expect(container.textContent).not.toContain('Mostrando 0 de 3');
        expect(container.querySelector('[data-testid^="admin-user-row-"]')).toBeNull();
      });

      await clickButton(getButtonsByText(container, 'Limpiar búsqueda')[0]!);

      await waitForExpectation(() => {
        expect(searchInput.value).toBe('');
        expect(getButtonsByText(container, 'Limpiar búsqueda')).toHaveLength(0);
        expect(container.querySelector('[data-testid="admin-users-empty-search-reset"]')).toBeNull();
        expect(getRowByUserId(container, 101).textContent).toContain('ada-admin');
        expect(getRowByUserId(container, 102).textContent).toContain('grace-ops');
        expect(getRowByUserId(container, 103).textContent).toContain('linus-view');
      });
    } finally {
      await cleanup();
    }
  });
});
