import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { render, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { vi } from 'vitest';
import AdminConsolePage from '../AdminConsolePage';
import type { AdminUserDTO } from '../../api/types';

const mockAuditLogs = vi.fn();
const mockConsolePreview = vi.fn();
const mockListUsers = vi.fn();
const mockSeed = vi.fn();
const mockUpdateUserRoles = vi.fn();
const mockHealthFetch = vi.fn();

vi.mock('../../api/admin', () => ({
  AdminApi: {
    auditLogs: () => mockAuditLogs(),
    consolePreview: () => mockConsolePreview(),
    listUsers: () => mockListUsers(),
    seed: () => mockSeed(),
    updateUserRoles: (userId: number, roles: string[]) => mockUpdateUserRoles(userId, roles),
  },
}));

vi.mock('../../utilities/health', () => ({
  Health: {
    fetch: () => mockHealthFetch(),
  },
}));

function createQueryClient() {
  return new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });
}

function renderPage(queryClient = createQueryClient()) {
  return {
    queryClient,
    ...render(
      <QueryClientProvider client={queryClient}>
        <AdminConsolePage />
      </QueryClientProvider>,
    ),
  };
}

function buildAdminUser(overrides: Partial<AdminUserDTO> = {}): AdminUserDTO {
  return {
    userId: 101,
    username: 'ada',
    displayName: 'Ada Lovelace',
    partyId: 9,
    roles: ['Admin'],
    status: 'ACTIVE',
    lastLoginAt: null,
    lastSeenAt: null,
    ...overrides,
  };
}

function getMenuItemByText(labelText: string) {
  const item = Array.from(document.body.querySelectorAll('[role="menuitem"], [role="option"]')).find(
    (element) => (element.textContent ?? '').trim() === labelText,
  );

  if (!(item instanceof HTMLElement)) {
    throw new Error(`Menu item not found: ${labelText}`);
  }

  return item;
}

function expectToAppearBefore(first: HTMLElement, second: HTMLElement) {
  expect(first.compareDocumentPosition(second) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
}

describe('AdminConsolePage', () => {
  beforeAll(() => {
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
    vi.clearAllMocks();
    mockHealthFetch.mockResolvedValue({ status: 'ok', db: 'ok' });
    mockAuditLogs.mockResolvedValue([]);
    mockConsolePreview.mockResolvedValue({ status: 'ok', cards: [] });
    mockListUsers.mockResolvedValue([]);
    mockSeed.mockResolvedValue(undefined);
    mockUpdateUserRoles.mockResolvedValue(undefined);
  });

  it('keeps the clean first-run state focused on onboarding instead of header refresh controls', async () => {
    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() =>
      expect(
        screen.getByText(
          /Revisa el estado del sistema, ajusta permisos y valida cambios recientes desde un solo lugar\./i,
        ),
      ).toBeInTheDocument(),
    );

    expect(
      screen.queryByRole('button', { name: /Actualizar panel/i }),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /^Actualizar$/i }),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /Refrescar/i }),
    ).not.toBeInTheDocument();
    expect(
      screen.getByText(
        /Opcional: carga datos de ejemplo para revisar usuarios, roles y auditoría sin tocar producción\./i,
      ),
    ).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /Cargar datos de ejemplo/i }),
    ).toBeInTheDocument();
    expect(
      screen.queryByText(
        /Aquí aparecerán los usuarios administrables\. Cuando exista el primero, podrás editar sus roles desde esta misma vista\./i,
      ),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        /Aquí aparecerán los cambios del sistema para confirmar quién hizo qué y cuándo\./i,
      ),
    ).not.toBeInTheDocument();
    expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
    expect(
      screen.getByText(/La auditoría aparecerá cuando se registre el primer cambio\./i),
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/Haz clic sobre un rol para editarlo desde esta misma vista\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirma quién cambió qué y cuándo antes de repetir una acción o ajustar permisos\./i),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Recorrido con demo')).not.toBeInTheDocument();
    expect(screen.queryByText('Datos de demostración')).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Seed demo data/i })).not.toBeInTheDocument();
  });

  it('keeps header admin actions hidden while first-run data is still loading', async () => {
    mockAuditLogs.mockImplementation(() => new Promise(() => undefined));
    mockConsolePreview.mockImplementation(() => new Promise(() => undefined));
    mockListUsers.mockImplementation(() => new Promise(() => undefined));

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.queryByRole('button', { name: /Actualizar panel/i })).not.toBeInTheDocument();
      expect(screen.queryByRole('button', { name: /Restablecer datos demo/i })).not.toBeInTheDocument();
      expect(screen.queryByRole('button', { name: /Cargar datos de ejemplo/i })).not.toBeInTheDocument();
    });
  });

  it('collapses the healthy service card into one friendly summary', async () => {
    renderPage();

    expect(await screen.findByText('Estado del servicio')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(/Todo listo: API y base de datos responden correctamente\./i),
      ).toBeInTheDocument();
    });

    expect(screen.queryByText(/^API:/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^Base de datos:/i)).not.toBeInTheDocument();
  });

  it('shows one explicit loading state while the service health check is still pending', async () => {
    mockHealthFetch.mockImplementation(() => new Promise(() => undefined));

    renderPage();

    expect(await screen.findByText('Estado del servicio')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText(/Comprobando API y base de datos…/i)).toBeInTheDocument();
    });

    expect(screen.queryByText(/^API:\s*—$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^Base de datos:\s*—$/i)).not.toBeInTheDocument();
  });

  it('keeps users and audit loading states compact until comparable data exists', async () => {
    mockListUsers.mockImplementation(() => new Promise(() => undefined));
    mockAuditLogs.mockImplementation(() => new Promise(() => undefined));

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Cargando usuarios…')).toBeInTheDocument();
      expect(screen.getByText('Cargando auditoría…')).toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /^Usuario$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Roles$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Fecha$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Entidad$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acción$/i })).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Todavía no hay usuarios administrables/i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Todavía no hay eventos de auditoría/i),
    ).not.toBeInTheDocument();
  });

  it('keeps detailed service checks visible when one dependency is not healthy', async () => {
    mockHealthFetch.mockResolvedValue({ status: 'ok', db: 'degraded' });

    renderPage();

    expect(await screen.findByText('Estado del servicio')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('API: ok')).toBeInTheDocument();
      expect(screen.getByText('Base de datos: degraded')).toBeInTheDocument();
    });

    expect(
      screen.queryByText(/Todo listo: API y base de datos responden correctamente\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps the header refresh action available when first-run data is empty because a query failed', async () => {
    mockHealthFetch.mockRejectedValue(new Error('Sin conexión'));

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByRole('button', { name: /Actualizar panel/i })).toBeInTheDocument();
      expect(screen.getByText('Sin conexión')).toBeInTheDocument();
    });

    expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /Cargar datos de ejemplo/i })).toBeInTheDocument();
  });

  it('keeps demo reset as one compact header action once the console already has data', async () => {
    mockListUsers.mockResolvedValue([buildAdminUser()]);

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByRole('button', { name: /Actualizar panel/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Restablecer datos demo/i })).toBeInTheDocument();
    });

    expect(screen.queryByText('Datos de demostración')).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        /Restablece los datos de demo en ambientes de prueba cuando necesites repetir el flujo sin refrescos manuales extra\./i,
      ),
    ).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Cargar datos de ejemplo/i })).not.toBeInTheDocument();
  });

  it('shows a compact first-run checklist when preview cards only duplicate built-in admin sections', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'user-management',
          title: 'Gestión de usuarios',
          body: [
            'La asignación de roles se administra desde la pantalla de Parties.',
            'Próximamente aquí se podrá crear usuarios de servicio y tokens API.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Sigue este recorrido para ubicar cada bloque sin repetir revisiones vacías\./i,
        ),
      ).toBeInTheDocument();
      expect(
        screen.getByText(
          /Opcional: carga datos de ejemplo para revisar usuarios, roles y auditoría sin tocar producción\./i,
        ),
      ).toBeInTheDocument();
      expect(
        screen.getByRole('button', { name: /Cargar datos de ejemplo/i }),
      ).toBeInTheDocument();
      expect(
        screen.getByRole('link', { name: /1\. Estado del servicio/i }),
      ).toBeInTheDocument();
      expect(
        screen.getByRole('link', { name: /2\. Usuarios y roles/i }),
      ).toBeInTheDocument();
      expect(
        screen.getByRole('link', { name: /3\. Auditoría reciente/i }),
      ).toBeInTheDocument();
      expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
      expect(
        screen.getByText(/La auditoría aparecerá cuando se registre el primer cambio\./i),
      ).toBeInTheDocument();
    });

    expect(screen.getByRole('link', { name: /1\. Estado del servicio/i })).toHaveAttribute('href', '#admin-service-health');
    expect(screen.getByRole('link', { name: /2\. Usuarios y roles/i })).toHaveAttribute('href', '#admin-users-and-roles');
    expect(screen.getByRole('link', { name: /3\. Auditoría reciente/i })).toHaveAttribute('href', '#admin-recent-audit');
    expect(screen.queryByText('Recorrido con demo')).not.toBeInTheDocument();
    expect(screen.queryByText('Datos de demostración')).not.toBeInTheDocument();
    expect(screen.queryByText('Gestión de usuarios')).not.toBeInTheDocument();
    expect(screen.queryByText(/Si es tu primera vez aquí/i)).not.toBeInTheDocument();
    expect(
      screen.queryByText(/La asignación de roles se administra desde la pantalla de Parties\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Valida el estado del servicio antes de cambiar permisos o repetir una acción\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Ajusta los accesos desde Usuarios y roles para resolver el caso actual\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirma el resultado en Auditoría reciente antes de seguir con otro cambio\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        /Todavía no hay usuarios administrables\. Cuando exista el primero, aquí verás roles, último acceso y el atajo para editar roles\./i,
      ),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        /Todavía no hay eventos de auditoría\. Cuando alguien cambie permisos o datos del sistema, aquí verás quién hizo qué y cuándo\./i,
      ),
    ).not.toBeInTheDocument();
  });

  it('keeps the first-run checklist focused when the preview renames user management to roles and permissions', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'access-control',
          title: 'Roles y permisos',
          body: [
            'Administra aquí la asignación de roles y permisos del equipo.',
            'Próximamente aquí se podrán revisar cambios de acceso recientes.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
      expect(
        screen.getByText(/La auditoría aparecerá cuando se registre el primer cambio\./i),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver módulo adicional/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Roles y permisos')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Administra aquí la asignación de roles y permisos del equipo\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps the first-run checklist focused when access control falls back to the built-in admin workflow', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'access-control',
          title: 'Control de acceso',
          body: [
            'Administra accesos y permisos del equipo desde esta vista.',
            'Próximamente aquí se podrán revisar cambios de autorización recientes.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
      expect(
        screen.getByText(/La auditoría aparecerá cuando se registre el primer cambio\./i),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver módulo adicional/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Control de acceso')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Administra accesos y permisos del equipo desde esta vista\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps the first-run checklist focused when a built-in access-control duplicate arrives under a custom preview id', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'permissions-overview',
          title: 'Control de acceso',
          body: [
            'Administra accesos y permisos del equipo desde esta vista.',
            'Próximamente aquí se podrán revisar cambios de autorización recientes.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
      expect(
        screen.getByText(/La auditoría aparecerá cuando se registre el primer cambio\./i),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver módulo adicional/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Control de acceso')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Administra accesos y permisos del equipo desde esta vista\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps the first-run checklist focused when a built-in title differs only by punctuation', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'permissions-overview-v2',
          title: 'Control de acceso:',
          body: [
            'Administra accesos y permisos del equipo desde esta vista.',
            'Próximamente aquí se podrán revisar cambios de autorización recientes.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
      expect(
        screen.getByText(/La auditoría aparecerá cuando se registre el primer cambio\./i),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver módulo adicional/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Control de acceso:')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Administra accesos y permisos del equipo desde esta vista\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps English fallback titles for built-in admin sections out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-health',
          title: 'Service health',
          body: ['Check whether the API and database are ready before changing permissions.'],
        },
        {
          cardId: 'fallback-users',
          title: 'User management',
          body: ['Adjust team access from the users and roles workflow.'],
        },
        {
          cardId: 'fallback-audit',
          title: 'Recent audit',
          body: ['Confirm who changed what before repeating an admin action.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
      expect(
        screen.getByText(/La auditoría aparecerá cuando se registre el primer cambio\./i),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Service health')).not.toBeInTheDocument();
    expect(screen.queryByText('User management')).not.toBeInTheDocument();
    expect(screen.queryByText('Recent audit')).not.toBeInTheDocument();
  });

  it('keeps terse fallback titles for built-in admin sections out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-system-status',
          title: 'Health',
          body: ['Check whether the API and database are ready before changing permissions.'],
        },
        {
          cardId: 'fallback-access-list',
          title: 'Users',
          body: ['Adjust team access from the users and roles workflow.'],
        },
        {
          cardId: 'fallback-history',
          title: 'Audit',
          body: ['Confirm who changed what before repeating an admin action.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
      expect(
        screen.getByText(/La auditoría aparecerá cuando se registre el primer cambio\./i),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Health')).not.toBeInTheDocument();
    expect(screen.queryByText('Users')).not.toBeInTheDocument();
    expect(screen.queryByText('Audit')).not.toBeInTheDocument();
  });

  it('keeps generic admin-console fallback cards out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'admin-console',
          title: 'Admin console',
          body: ['Review service health, users, roles, and audit activity from one admin landing page.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
      expect(
        screen.getByText(/La auditoría aparecerá cuando se registre el primer cambio\./i),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Admin console')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review service health, users, roles, and audit activity/i),
    ).not.toBeInTheDocument();
  });

  it('strips built-in admin copy from custom fallback cards before showing optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'admin-start-here',
          title: 'Revisión inicial',
          body: [
            'Valida el estado del servicio antes de cambiar permisos o repetir una acción.',
            'Ajusta los accesos desde Usuarios y roles para resolver el caso actual.',
            'Confirma el resultado en Auditoría reciente antes de seguir con otro cambio.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
      expect(
        screen.getByText(/La auditoría aparecerá cuando se registre el primer cambio\./i),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('button', { name: /Opcional: ver Revisión inicial/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Revisión inicial')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Valida el estado del servicio antes de cambiar permisos o repetir una acción\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Ajusta los accesos desde Usuarios y roles para resolver el caso actual\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirma el resultado en Auditoría reciente antes de seguir con otro cambio\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps unique preview cards collapsed during first-run until the admin asks to see them inline', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens',
          title: 'Tokens de servicio',
          body: [
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();
    await screen.findByText('Primeros pasos');
    const getFirstRunAlert = () => {
      const firstRunAlert = screen.getByText('Primeros pasos').closest('[role="alert"]');
      if (!(firstRunAlert instanceof HTMLElement)) {
        throw new Error('Expected first-run alert container to render');
      }

      return firstRunAlert;
    };

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        within(getFirstRunAlert()).getByRole(
          'button',
          { name: /^Opcional: ver Tokens de servicio$/i },
        ),
      ).toBeInTheDocument();
      expect(
        within(getFirstRunAlert()).queryByRole(
          'button',
          { name: /^Opcional: ver 1 módulo adicional$/i },
        ),
      ).not.toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Cargar datos de ejemplo/i })).toBeInTheDocument();
    });

    expect(
      screen.queryByText(
        /Hay 1 módulo adicional fuera del recorrido inicial\. Revísalo solo si ya necesitas ese flujo\./i,
      ),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        /Empiezan ocultos para que el recorrido inicial siga centrado en salud, usuarios y auditoría\./i,
      ),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).not.toBeInTheDocument();
    expect(within(getFirstRunAlert()).queryByRole('button', { name: /Mostrar Tokens de servicio/i })).not.toBeInTheDocument();

    expect(
      within(getFirstRunAlert()).queryByRole('button', { name: /4\. Ver Tokens de servicio/i }),
    ).not.toBeInTheDocument();
    expect(
      within(getFirstRunAlert()).queryByText(/^Tokens de servicio$/i),
    ).not.toBeInTheDocument();

    await user.click(
      within(getFirstRunAlert()).getByRole(
        'button',
        { name: /^Opcional: ver Tokens de servicio$/i },
      ),
    );

    expect(await within(getFirstRunAlert()).findByText('Módulos opcionales')).toBeInTheDocument();
    expect(
      within(getFirstRunAlert()).getByText(
        /Revísalos aquí solo si ya necesitas ese flujo extra, sin salir del recorrido inicial\./i,
      ),
    ).toBeInTheDocument();
    expect(await within(getFirstRunAlert()).findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      within(getFirstRunAlert()).getByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(within(getFirstRunAlert()).getByRole('button', { name: /Ocultar módulos adicionales/i })).toBeInTheDocument();
    expect(screen.getAllByRole('button', { name: /Ocultar módulos adicionales/i })).toHaveLength(1);

    expect(screen.queryByText('Datos de demostración')).not.toBeInTheDocument();
  });

  it('deduplicates repeated preview cards so the console only shows each extra workflow once', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens-a',
          title: 'Tokens de servicio',
          body: [
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
          ],
        },
        {
          cardId: 'service-tokens-b',
          title: 'Tokens de servicio',
          body: [
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getByRole(
          'button',
          { name: /^Opcional: ver Tokens de servicio$/i },
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver Tokens de servicio$/i },
      ),
    );

    expect(await screen.findByText('Módulos opcionales')).toBeInTheDocument();
    expect(await screen.findAllByText('Tokens de servicio')).toHaveLength(1);
    expect(
      screen.getAllByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).toHaveLength(1);

    expect(screen.queryByText('Datos de demostración')).not.toBeInTheDocument();
  });

  it('deduplicates fallback cards with different titles when the body copy is identical', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens',
          title: 'Tokens de servicio',
          body: [
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
          ],
        },
        {
          cardId: 'shared-credentials',
          title: 'Credenciales compartidas',
          body: [
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole(
          'button',
          { name: /^Opcional: ver Tokens de servicio$/i },
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('button', { name: /^Opcional: ver 2 módulos adicionales$/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Credenciales compartidas')).not.toBeInTheDocument();

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver Tokens de servicio$/i },
      ),
    );

    expect(await screen.findByText('Módulos opcionales')).toBeInTheDocument();
    expect(screen.getAllByText('Tokens de servicio')).toHaveLength(1);
    expect(screen.queryByText('Credenciales compartidas')).not.toBeInTheDocument();
    expect(
      screen.getAllByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\.?/i,
      ),
    ).toHaveLength(1);
  });

  it('merges repeated module titles into one additional card so admins only review that workflow once', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens-overview',
          title: 'Tokens de servicio',
          body: [
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
          ],
        },
        {
          cardId: 'service-tokens-rotation',
          title: '  Tokens de servicio  ',
          body: [
            'Programa una rotación semanal sin salir de esta consola.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole(
          'button',
          { name: /^Opcional: ver Tokens de servicio$/i },
        ),
      ).toBeInTheDocument();
    });

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver Tokens de servicio$/i },
      ),
    );

    expect(await screen.findByText('Módulos opcionales')).toBeInTheDocument();
    expect(screen.getAllByText('Tokens de servicio')).toHaveLength(1);
    expect(
      screen.getByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).toBeInTheDocument();
    expect(
      screen.getByText(/Programa una rotación semanal sin salir de esta consola\./i),
    ).toBeInTheDocument();
  });

  it('merges repeated module titles when fallback discovery only changes punctuation', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens-overview',
          title: 'Tokens de servicio',
          body: [
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
          ],
        },
        {
          cardId: 'service-tokens-rotation',
          title: 'Tokens de servicio:',
          body: [
            'Programa una rotación semanal sin salir de esta consola.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole(
          'button',
          { name: /^Opcional: ver Tokens de servicio$/i },
        ),
      ).toBeInTheDocument();
    });

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver Tokens de servicio$/i },
      ),
    );

    expect(await screen.findByText('Módulos opcionales')).toBeInTheDocument();
    expect(screen.getAllByText('Tokens de servicio')).toHaveLength(1);
    expect(screen.queryByText('Tokens de servicio:')).not.toBeInTheDocument();
    expect(
      screen.getByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).toBeInTheDocument();
    expect(
      screen.getByText(/Programa una rotación semanal sin salir de esta consola\./i),
    ).toBeInTheDocument();
  });

  it('deduplicates repeated module copy when fallback discovery only changes punctuation', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens-overview',
          title: 'Tokens de servicio',
          body: [
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios',
          ],
        },
        {
          cardId: 'service-tokens-rotation',
          title: 'Tokens de servicio',
          body: [
            'Programa una rotación semanal sin salir de esta consola.',
            'Programa una rotación semanal sin salir de esta consola',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole(
          'button',
          { name: /^Opcional: ver Tokens de servicio$/i },
        ),
      ).toBeInTheDocument();
    });

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver Tokens de servicio$/i },
      ),
    );

    expect(await screen.findByText('Módulos opcionales')).toBeInTheDocument();
    expect(
      screen.getAllByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\.?/i,
      ),
    ).toHaveLength(1);
    expect(
      screen.getAllByText(/Programa una rotación semanal sin salir de esta consola\.?/i),
    ).toHaveLength(1);
  });

  it('keeps the first-run CTA focused on the hidden module count before expanding extras', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens',
          title: 'Tokens de servicio',
          body: [
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
          ],
        },
        {
          cardId: 'integrations',
          title: 'Integraciones',
          body: [
            'Revisa conectores pendientes sin salir de la consola.',
          ],
        },
        {
          cardId: 'api-access',
          title: 'Acceso API',
          body: [
            'Consulta credenciales técnicas y accesos de servicio.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole(
          'button',
          { name: /^Opcional: ver 3 módulos adicionales$/i },
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Acceso API')).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /Tokens de servicio|Integraciones|Acceso API/i }),
    ).not.toBeInTheDocument();
  });

  it('keeps the standalone CTA focused on the hidden module count before expanding extras', async () => {
    mockListUsers.mockResolvedValue([buildAdminUser()]);
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens',
          title: 'Tokens de servicio',
          body: [
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
          ],
        },
        {
          cardId: 'integrations',
          title: 'Integraciones',
          body: [
            'Revisa conectores pendientes sin salir de la consola.',
          ],
        },
        {
          cardId: 'api-access',
          title: 'Acceso API',
          body: [
            'Consulta credenciales técnicas y accesos de servicio.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole(
          'button',
          { name: /^Ver 3 módulos adicionales$/i },
        ),
      ).toBeInTheDocument();
      expect(screen.getByText('Módulos adicionales')).toBeInTheDocument();
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
    });

    expect(screen.queryByText('Acceso API')).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /Tokens de servicio|Integraciones|Acceso API/i }),
    ).not.toBeInTheDocument();
  });

  it('sorts standalone additional modules by title so fallback discovery stays easy to scan', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([buildAdminUser()]);
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens',
          title: 'Tokens de servicio',
          body: [
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
          ],
        },
        {
          cardId: 'api-access',
          title: 'Acceso API',
          body: [
            'Consulta credenciales técnicas y accesos de servicio.',
          ],
        },
        {
          cardId: 'integrations',
          title: 'Integraciones',
          body: [
            'Revisa conectores pendientes sin salir de la consola.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole(
          'button',
          { name: /^Ver 3 módulos adicionales$/i },
        ),
      ).toBeInTheDocument();
    });

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Ver 3 módulos adicionales$/i },
      ),
    );

    expect(await screen.findByText('Acceso API')).toBeInTheDocument();
    expect(screen.getByText('Integraciones')).toBeInTheDocument();
    expect(screen.getByText('Tokens de servicio')).toBeInTheDocument();

    expectToAppearBefore(screen.getByText('Acceso API'), screen.getByText('Integraciones'));
    expectToAppearBefore(screen.getByText('Integraciones'), screen.getByText('Tokens de servicio'));
  });

  it('ignores empty preview cards so placeholder admin modules do not hide the first-run checklist', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens-placeholder',
          title: '   Tokens de servicio   ',
          body: ['   ', '\n\n'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Sigue este recorrido para ubicar cada bloque sin repetir revisiones vacías\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
  });

  it('ignores placeholder-only preview cards so first-run stays focused on real admin workflows', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens-placeholder',
          title: 'Tokens de servicio',
          body: [
            'Estamos trabajando en esta vista. Próximamente encontrarás la funcionalidad completa aquí.',
            'Si necesitas priorizar esta sección, comparte los requisitos con el equipo de producto.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Sigue este recorrido para ubicar cada bloque sin repetir revisiones vacías\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('button', { name: /4\. Ver Tokens de servicio/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        /Estamos trabajando en esta vista\. Próximamente encontrarás la funcionalidad completa aquí\./i,
      ),
    ).not.toBeInTheDocument();
  });

  it('ignores English placeholder-only preview cards so fallback discovery does not add dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens-placeholder',
          title: 'Service tokens',
          body: [
            'We are working on this view. Full functionality will be available here soon.',
            'If you need to prioritize this section, share the requirements with the product team.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Sigue este recorrido para ubicar cada bloque sin repetir revisiones vacías\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('button', { name: /Service tokens/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Service tokens')).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        /We are working on this view\. Full functionality will be available here soon\./i,
      ),
    ).not.toBeInTheDocument();
  });

  it('strips placeholder filler from mixed preview cards so optional modules only show actionable copy', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens',
          title: 'Tokens de servicio',
          body: [
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
            'Estamos trabajando en esta vista. Próximamente encontrarás la funcionalidad completa aquí.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole(
          'button',
          { name: /^Opcional: ver Tokens de servicio$/i },
        ),
      ).toBeInTheDocument();
    });

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver Tokens de servicio$/i },
      ),
    );

    expect(await screen.findByText('Módulos opcionales')).toBeInTheDocument();
    expect(
      screen.getByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).toBeInTheDocument();
    expect(
      screen.queryByText(
        /Estamos trabajando en esta vista\. Próximamente encontrarás la funcionalidad completa aquí\./i,
      ),
    ).not.toBeInTheDocument();
  });

  it('shows the first-run checklist only when the console is actually empty', async () => {
    mockListUsers.mockResolvedValue([buildAdminUser()]);
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'audit-1',
        actorId: 101,
        entity: 'user',
        entityId: '101',
        action: 'roles.updated',
        diff: 'Admin -> Admin, Manager',
        createdAt: '2026-04-09T15:30:00.000Z',
      },
    ]);

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
      expect(screen.getByText('roles.updated')).toBeInTheDocument();
    });

    expect(screen.queryByText('Primeros pasos')).not.toBeInTheDocument();
  });

  it('keeps standalone additional modules collapsed until the admin explicitly expands them', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([buildAdminUser()]);
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens',
          title: 'Tokens de servicio',
          body: [
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Módulos adicionales')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Ver Tokens de servicio/i })).toBeInTheDocument();
      expect(screen.queryByRole('button', { name: /Ver 1 módulo adicional/i })).not.toBeInTheDocument();
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
    });

    expect(
      screen.getByText(
        /Tarjetas auxiliares del panel\. Ábrelas solo cuando ya confirmaste salud, usuarios y auditoría\./i,
      ),
    ).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /Ver Tokens de servicio/i }),
    ).toHaveAttribute('aria-expanded', 'false');
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /Ver Tokens de servicio/i }));

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /Ocultar módulos adicionales/i }),
    ).toHaveAttribute('aria-expanded', 'true');
  });

  it('refreshes every admin dataset from the single panel action', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([buildAdminUser()]);
    const { queryClient } = renderPage();
    const invalidateQueriesSpy = vi.spyOn(queryClient, 'invalidateQueries');

    await waitFor(() =>
      expect(
        screen.getByRole('button', { name: /Actualizar panel/i }),
      ).toBeEnabled(),
    );

    await user.click(screen.getByRole('button', { name: /Actualizar panel/i }));

    await waitFor(() => {
      expect(invalidateQueriesSpy).toHaveBeenCalledWith({ queryKey: ['admin', 'health'] });
      expect(invalidateQueriesSpy).toHaveBeenCalledWith({ queryKey: ['admin', 'console'] });
      expect(invalidateQueriesSpy).toHaveBeenCalledWith({ queryKey: ['admin', 'users'] });
      expect(invalidateQueriesSpy).toHaveBeenCalledWith({ queryKey: ['admin', 'audit'] });
    });
  });

  it('refreshes the full admin panel after seeding demo data', async () => {
    const user = userEvent.setup();
    const { queryClient } = renderPage();
    const invalidateQueriesSpy = vi.spyOn(queryClient, 'invalidateQueries');

    await waitFor(() =>
      expect(
        screen.getByRole('button', { name: /Cargar datos de ejemplo/i }),
      ).toBeEnabled(),
    );

    await user.click(screen.getByRole('button', { name: /Cargar datos de ejemplo/i }));

    await waitFor(() => {
      expect(mockSeed).toHaveBeenCalledTimes(1);
      expect(invalidateQueriesSpy).toHaveBeenCalledWith({ queryKey: ['admin', 'health'] });
      expect(invalidateQueriesSpy).toHaveBeenCalledWith({ queryKey: ['admin', 'console'] });
      expect(invalidateQueriesSpy).toHaveBeenCalledWith({ queryKey: ['admin', 'users'] });
      expect(invalidateQueriesSpy).toHaveBeenCalledWith({ queryKey: ['admin', 'audit'] });
    });
  });

  it('keeps a single detailed users empty state when the page is not in first-run checklist mode', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens',
          title: 'Tokens de servicio',
          body: ['Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.'],
        },
      ],
    });
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'audit-1',
        actorId: 101,
        entity: 'user',
        entityId: '101',
        action: 'roles.updated',
        diff: 'Admin -> Admin, Manager',
        createdAt: '2026-04-09T15:30:00.000Z',
      },
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Todavía no hay usuarios administrables\. Cuando exista el primero, aquí verás roles, último acceso y el atajo para editar roles\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText('roles.updated')).toBeInTheDocument();
    });

    expect(screen.queryByText('Aún no hay usuarios administrables.')).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        /Aquí aparecerán los usuarios administrables\. Cuando exista el primero, podrás editar sus roles desde esta misma vista\./i,
      ),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Haz clic sobre un rol para editarlo desde esta misma vista\./i),
    ).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Usuario$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Roles$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /Último acceso/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Permisos$/i })).not.toBeInTheDocument();
  });

  it('replaces the single-user table with a compact first-user summary', async () => {
    mockListUsers.mockResolvedValue([buildAdminUser()]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Primer usuario administrable\. Usa el botón del rol para ajustar accesos; cuando exista una segunda cuenta, volverá la tabla comparativa\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
      expect(screen.getByText('Usuario: ada')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveTextContent('Admin');
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveAttribute('title', 'Editar roles de Ada Lovelace');
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).not.toHaveAttribute('aria-describedby');
      expect(screen.queryByText('Party #9')).not.toBeInTheDocument();
      expect(
        within(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).getByTestId('EditOutlinedIcon'),
      ).toBeInTheDocument();
      expect(screen.queryByText('Editar roles: Admin')).not.toBeInTheDocument();
      expect(screen.queryByText(/Roles · Clic para editar/i)).not.toBeInTheDocument();
      expect(screen.queryByText(/Haz clic en el rol para editarlo aquí/i)).not.toBeInTheDocument();
      expect(screen.queryByText(/Edita sus roles aquí/i)).not.toBeInTheDocument();
      expect(screen.queryByText(/^Último acceso:/i)).not.toBeInTheDocument();
      expect(screen.queryByText('Estado: Activo')).not.toBeInTheDocument();
      expect(screen.queryByText(/Revisa esta cuenta aquí/i)).not.toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /^Usuario$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Roles$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /Último acceso/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
  });

  it('hides internal party ids unless two admin rows would otherwise look identical', async () => {
    mockListUsers.mockResolvedValue([
      buildAdminUser(),
      buildAdminUser({
        userId: 102,
        username: 'grace',
        displayName: 'Grace Hopper',
        partyId: 10,
        roles: ['Manager'],
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
      expect(screen.getByText('Grace Hopper')).toBeInTheDocument();
    });

    expect(screen.queryByText('Party #9')).not.toBeInTheDocument();
    expect(screen.queryByText('Party #10')).not.toBeInTheDocument();
  });

  it('shows party ids again when duplicate admin identities need a disambiguator', async () => {
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        username: 'ana-admin',
        displayName: 'Ana Admin',
        partyId: 9,
      }),
      buildAdminUser({
        userId: 102,
        username: 'ana-admin',
        displayName: 'Ana Admin',
        partyId: 10,
        roles: ['Manager'],
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getAllByText('Ana Admin')).toHaveLength(2);
      expect(screen.getAllByText('Usuario: ana-admin')).toHaveLength(2);
    });

    expect(screen.getByText('Party #9')).toBeInTheDocument();
    expect(screen.getByText('Party #10')).toBeInTheDocument();
  });

  it('deduplicates repeated users before deciding whether the page needs the full comparison table', async () => {
    mockListUsers.mockResolvedValue([buildAdminUser(), buildAdminUser()]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Primer usuario administrable\. Usa el botón del rol para ajustar accesos; cuando exista una segunda cuenta, volverá la tabla comparativa\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getAllByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveLength(1);
    });

    expect(screen.queryByRole('columnheader', { name: /^Usuario$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Roles$/i })).not.toBeInTheDocument();
  });

  it('shows the single-user status only when that account needs attention', async () => {
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        status: 'INVITED',
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Primer usuario administrable\. Usa el botón del rol para ajustar accesos; cuando exista una segunda cuenta, volverá la tabla comparativa\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText('Estado: Invitado')).toBeInTheDocument();
    });
  });

  it('shows the single-user last access only when that timestamp exists', async () => {
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        lastSeenAt: '2026-04-10T12:00:00.000Z',
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Primer usuario administrable\. Usa el botón del rol para ajustar accesos; cuando exista una segunda cuenta, volverá la tabla comparativa\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText(/^Último acceso:/i)).toBeInTheDocument();
    });
  });

  it('moves the inline edit affordance onto each role button and keeps the helper line for hidden columns only', async () => {
    mockListUsers.mockResolvedValue([
      buildAdminUser(),
      buildAdminUser({
        userId: 102,
        username: 'grace',
        displayName: 'Grace Hopper',
        partyId: 10,
        roles: ['Manager'],
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Vista actual: la columna de último acceso reaparecerá cuando exista al menos un ingreso registrado y la columna de estado reaparecerá cuando exista una cuenta invitada o suspendida\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.queryByText(/^Haz clic sobre un rol para editarlo desde esta misma vista\.$/i)).not.toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Roles$/i })).toBeInTheDocument();
      expect(screen.queryByText(/Clic para editar/i)).not.toBeInTheDocument();
      expect(screen.queryByText('Editar aquí')).not.toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Roles y edición$/i })).not.toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Permisos$/i })).not.toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /Último acceso/i })).not.toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveTextContent('Admin');
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveAttribute('title', 'Editar roles de Ada Lovelace');
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).not.toHaveAttribute('aria-describedby');
      expect(within(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).getByTestId('EditOutlinedIcon')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Editar roles de Grace Hopper' })).toHaveTextContent('Manager');
      expect(screen.getByRole('button', { name: 'Editar roles de Grace Hopper' })).toHaveAttribute('title', 'Editar roles de Grace Hopper');
      expect(screen.getByRole('button', { name: 'Editar roles de Grace Hopper' })).not.toHaveAttribute('aria-describedby');
      expect(within(screen.getByRole('button', { name: 'Editar roles de Grace Hopper' })).getByTestId('EditOutlinedIcon')).toBeInTheDocument();
      expect(screen.queryByText(/^Editar$/i)).not.toBeInTheDocument();
      expect(screen.queryByText(/^Editar roles$/i)).not.toBeInTheDocument();
      expect(screen.queryByText(/^Activo$/i)).not.toBeInTheDocument();
    });
  });

  it('hides unknown user statuses until one account needs status context', async () => {
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        status: null,
      }),
      buildAdminUser({
        userId: 102,
        username: 'grace',
        displayName: 'Grace Hopper',
        partyId: 10,
        roles: ['Manager'],
        status: null,
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
      expect(screen.getByText('Grace Hopper')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Vista actual: la columna de último acceso reaparecerá cuando exista al menos un ingreso registrado y la columna de estado reaparecerá cuando exista una cuenta invitada o suspendida\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
    expect(screen.queryAllByText('—')).toHaveLength(0);
  });

  it('shows the status column again as soon as one admin account needs that extra context', async () => {
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        lastSeenAt: '2026-04-10T12:00:00.000Z',
      }),
      buildAdminUser({
        userId: 102,
        username: 'grace',
        displayName: 'Grace Hopper',
        partyId: 10,
        roles: ['Manager'],
        status: 'INVITED',
        lastSeenAt: '2026-04-10T13:00:00.000Z',
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.queryByText(
          /Vista actual:/i,
        ),
      ).not.toBeInTheDocument();
      expect(screen.queryByText(/Clic para editar/i)).not.toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /Último acceso/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Estado$/i })).toBeInTheDocument();
      expect(screen.getByText('Invitado')).toBeInTheDocument();
      expect(within(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).getByTestId('EditOutlinedIcon')).toBeInTheDocument();
      expect(within(screen.getByRole('button', { name: 'Editar roles de Grace Hopper' })).getByTestId('EditOutlinedIcon')).toBeInTheDocument();
    });

    const activeRow = screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' }).closest('tr');
    const invitedRow = screen.getByRole('button', { name: 'Editar roles de Grace Hopper' }).closest('tr');

    if (!(activeRow instanceof HTMLTableRowElement) || !(invitedRow instanceof HTMLTableRowElement)) {
      throw new Error('Expected admin user rows to render inside the comparison table');
    }

    expect(within(activeRow).queryByText(/^Activo$/i)).not.toBeInTheDocument();
    expect(within(activeRow).getByText('—')).toBeInTheDocument();
    expect(within(invitedRow).getByText('Invitado')).toBeInTheDocument();
  });

  it('summarizes the exact pending role change before enabling save', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([buildAdminUser()]);

    renderPage();

    const editButton = await screen.findByRole('button', { name: 'Editar roles de Ada Lovelace' });
    await user.click(editButton);

    const saveButton = await screen.findByRole('button', { name: /Guardar cambios/i });
    expect(
      screen.getByText(/Sin cambios pendientes\. Modifica la selección para habilitar Guardar cambios\./i),
    ).toBeInTheDocument();
    expect(saveButton).toBeDisabled();

    const rolesSelect = document.body.querySelector('[role="combobox"]');
    if (!(rolesSelect instanceof HTMLElement)) {
      throw new Error('Roles select not found');
    }

    await user.click(rolesSelect);
    await user.click(getMenuItemByText('Manager'));
    await user.keyboard('{Escape}');

    await waitFor(() => {
      expect(saveButton).toBeEnabled();
      expect(screen.getByText(/Cambio pendiente: agregar Manager\./i)).toBeInTheDocument();
    });
  });

  it('normalizes repeated role labels so the admin page shows one stable access summary', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        roles: ['Manager', 'Admin', 'Manager'],
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveTextContent('Admin, Manager');
      expect(screen.queryByText('Editar roles: Admin, Manager')).not.toBeInTheDocument();
      expect(screen.queryByText('Editar roles: Manager, Admin, Manager')).not.toBeInTheDocument();
    });

    await user.click(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' }));

    expect(
      await screen.findByText(
        /Roles actuales: Admin, Manager\. Ajusta la selección para abrir o retirar módulos en esta cuenta\./i,
      ),
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/Roles actuales: Manager, Admin, Manager\./i),
    ).not.toBeInTheDocument();
  });

  it('labels empty role assignments explicitly instead of showing a dash in the edit flow', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        roles: [],
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveTextContent('Sin roles');
      expect(screen.queryByText(/^—$/i)).not.toBeInTheDocument();
    });

    await user.click(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' }));

    expect(
      await screen.findByText(
        /Roles actuales: Sin roles\. Ajusta la selección para abrir o retirar módulos en esta cuenta\./i,
      ),
    ).toBeInTheDocument();
    expect(screen.queryByText(/Roles actuales: —\./i)).not.toBeInTheDocument();
  });

  it('flags role selections that collapse to the same app navigation', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([buildAdminUser()]);

    renderPage();

    const editButton = await screen.findByRole('button', { name: 'Editar roles de Ada Lovelace' });
    await user.click(editButton);

    expect(
      screen.queryByText(/Estos roles muestran la misma navegación principal en esta app:/i),
    ).not.toBeInTheDocument();

    const rolesSelect = document.body.querySelector('[role="combobox"]');
    if (!(rolesSelect instanceof HTMLElement)) {
      throw new Error('Roles select not found');
    }

    await user.click(rolesSelect);
    await user.click(getMenuItemByText('Manager'));
    await user.keyboard('{Escape}');

    expect(
      await screen.findByText(
        /Estos roles muestran la misma navegación principal en esta app: Admin y Manager\. Revisa si necesitas ambos antes de guardar\./i,
      ),
    ).toBeInTheDocument();
  });

  it('keeps the role dialog free of equivalent-navigation warnings until the admin makes a change', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([buildAdminUser({ roles: ['Admin', 'Manager'] })]);

    renderPage();

    const editButton = await screen.findByRole('button', { name: 'Editar roles de Ada Lovelace' });
    await user.click(editButton);

    expect(
      await screen.findByText(
        /Roles actuales: Admin, Manager\. Ajusta la selección para abrir o retirar módulos en esta cuenta\./i,
      ),
    ).toBeInTheDocument();
    expect(
      screen.getByText(/Sin cambios pendientes\. Modifica la selección para habilitar Guardar cambios\./i),
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/Estos roles muestran la misma navegación principal en esta app:/i),
    ).not.toBeInTheDocument();
  });

  it('replaces the single-audit table with a compact first-event summary', async () => {
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'audit-1',
        actorId: 101,
        entity: 'user',
        entityId: '101',
        action: 'roles.updated',
        diff: 'Admin -> Admin, Manager',
        createdAt: '2026-04-09T15:30:00.000Z',
      },
    ]);

    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Primer evento de auditoría\. Revísalo aquí; cuando exista el segundo, volverá la tabla cronológica\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText(/Acción:\s*roles\.updated/i)).toBeInTheDocument();
      expect(screen.getByText(/Entidad:\s*user · 101/i)).toBeInTheDocument();
      expect(screen.getByText(/Actor:\s*101/i)).toBeInTheDocument();
      expect(screen.getByText(/Detalle:\s*Admin -> Admin, Manager/i)).toBeInTheDocument();
    });

    expect(
      screen.queryByText(/Confirma quién cambió qué y cuándo antes de repetir una acción o ajustar permisos\./i),
    ).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Fecha$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Entidad$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acción$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Actor$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Detalle$/i })).not.toBeInTheDocument();
  });

  it('shows the known admin identity in audit attribution before falling back to raw ids', async () => {
    mockListUsers.mockResolvedValue([buildAdminUser()]);
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'audit-1',
        actorId: 101,
        entity: 'user',
        entityId: '101',
        action: 'roles.updated',
        diff: 'Admin -> Admin, Manager',
        createdAt: '2026-04-09T15:30:00.000Z',
      },
    ]);

    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText(/Actor:\s*Ada Lovelace \(ada\)/i)).toBeInTheDocument();
    });

    expect(screen.queryByText(/Actor:\s*101/i)).not.toBeInTheDocument();
  });

  it('hides empty actor and detail rows in the single-audit summary until that context exists', async () => {
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'audit-1',
        actorId: null,
        entity: 'package',
        entityId: 'PKG-1',
        action: 'package.created',
        diff: '   ',
        createdAt: '2026-04-09T15:30:00.000Z',
      },
    ]);

    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Primer evento de auditoría\. Revísalo aquí; cuando exista el segundo, volverá la tabla cronológica\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText(/Acción:\s*package\.created/i)).toBeInTheDocument();
      expect(screen.getByText(/Entidad:\s*package · PKG-1/i)).toBeInTheDocument();
    });

    expect(screen.queryByText(/Actor:\s*Sistema/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Detalle:/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^—$/i)).not.toBeInTheDocument();
  });

  it('deduplicates repeated audit entries so one repeated event keeps the compact first-event summary', async () => {
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'audit-1',
        actorId: 101,
        entity: 'user',
        entityId: '101',
        action: 'roles.updated',
        diff: 'Admin -> Admin, Manager',
        createdAt: '2026-04-09T15:30:00.000Z',
      },
      {
        auditId: 'audit-2',
        actorId: 101,
        entity: 'user',
        entityId: '101',
        action: 'roles.updated',
        diff: 'Admin -> Admin, Manager',
        createdAt: '2026-04-09T15:30:00.000Z',
      },
    ]);

    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Primer evento de auditoría\. Revísalo aquí; cuando exista el segundo, volverá la tabla cronológica\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText(/Acción:\s*roles\.updated/i)).toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /^Fecha$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Entidad$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acción$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Actor$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Detalle$/i })).not.toBeInTheDocument();
    expect(screen.getAllByText(/roles\.updated/i)).toHaveLength(1);
  });

  it('hides empty audit columns until one event adds actor or detail context', async () => {
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'audit-1',
        actorId: null,
        entity: 'package',
        entityId: 'PKG-1',
        action: 'package.created',
        diff: null,
        createdAt: '2026-04-09T15:30:00.000Z',
      },
      {
        auditId: 'audit-2',
        actorId: null,
        entity: 'package',
        entityId: 'PKG-2',
        action: 'package.synced',
        diff: '   ',
        createdAt: '2026-04-09T16:00:00.000Z',
      },
    ]);

    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Vista actual: la columna de actor reaparecerá cuando un cambio quede asociado a una cuenta específica y la columna de detalle reaparecerá cuando exista información extra para revisar\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Fecha$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Entidad$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Acción$/i })).toBeInTheDocument();
      expect(screen.getByText('package.created')).toBeInTheDocument();
      expect(screen.getByText('package.synced')).toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /^Actor$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Detalle$/i })).not.toBeInTheDocument();
    expect(screen.queryByText(/^Sistema$/i)).not.toBeInTheDocument();
  });

  it('shows audit actor and detail columns again as soon as one event needs them', async () => {
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'audit-1',
        actorId: null,
        entity: 'package',
        entityId: 'PKG-1',
        action: 'package.created',
        diff: null,
        createdAt: '2026-04-09T15:30:00.000Z',
      },
      {
        auditId: 'audit-2',
        actorId: 777,
        entity: 'user',
        entityId: 'USR-1',
        action: 'roles.updated',
        diff: 'Admin -> Admin, Manager',
        createdAt: '2026-04-09T16:00:00.000Z',
      },
    ]);

    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.queryByText(/Vista actual:/i),
      ).not.toBeInTheDocument();
      expect(
        screen.queryByText(
          /Confirma quién cambió qué y cuándo antes de repetir una acción o ajustar permisos\./i,
        ),
      ).not.toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Actor$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Detalle$/i })).toBeInTheDocument();
      expect(screen.getByText('777')).toBeInTheDocument();
      expect(screen.getByText('Admin -> Admin, Manager')).toBeInTheDocument();
    });
  });

  it('keeps a single detailed audit empty state when the page is not in first-run checklist mode', async () => {
    mockListUsers.mockResolvedValue([buildAdminUser()]);

    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Todavía no hay eventos de auditoría\. Cuando alguien cambie permisos o datos del sistema, aquí verás quién hizo qué y cuándo\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByText(
        /Aquí aparecerán los cambios del sistema para confirmar quién hizo qué y cuándo\./i,
      ),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirma quién cambió qué y cuándo antes de repetir una acción o ajustar permisos\./i),
    ).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Fecha$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Entidad$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acción$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Actor$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Detalle$/i })).not.toBeInTheDocument();
  });

  it('shows the system username only when it adds new identity context in the admin table', async () => {
    mockListUsers.mockResolvedValue([
      buildAdminUser(),
      buildAdminUser({
        userId: 102,
        username: 'grace',
        displayName: 'grace',
        partyId: 10,
        roles: ['Manager'],
      }),
      buildAdminUser({
        userId: 103,
        username: 'linus',
        displayName: '   ',
        partyId: 11,
        roles: ['ReadOnly'],
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.queryByRole('columnheader', { name: /^Permisos$/i })).not.toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Acciones$/i })).not.toBeInTheDocument();
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
      expect(screen.getByText('Usuario: ada')).toBeInTheDocument();
      expect(screen.queryByText('Usuario: grace')).not.toBeInTheDocument();
      expect(screen.queryByText('Usuario: linus')).not.toBeInTheDocument();
      expect(screen.getAllByText('grace')).toHaveLength(1);
      expect(screen.getAllByText('linus')).toHaveLength(1);
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Editar roles de grace' })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Editar roles de linus' })).toBeInTheDocument();
    });
  });
});
