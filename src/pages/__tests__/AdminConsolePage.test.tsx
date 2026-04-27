import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act, render, screen, waitFor, within } from '@testing-library/react';
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
      screen.getByRole('button', { name: /Cargar datos de ejemplo/i }),
    ).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /Cargar datos de ejemplo \(opcional\)/i }),
    ).toBeInTheDocument();
    expect(
      screen.queryByText(
        /Carga datos de ejemplo para revisar usuarios, roles y auditoría sin tocar producción\./i,
      ),
    ).not.toBeInTheDocument();
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
    expect(screen.getByTestId('admin-first-run-users-status')).toHaveTextContent('Aún no hay usuarios administrables.');
    expect(
      screen.getByTestId('admin-first-run-audit-status'),
    ).toHaveTextContent('La auditoría aparecerá cuando se registre el primer cambio.');
    expect(
      screen.queryByText(/Haz clic en un rol para editarlo desde esta misma vista\./i),
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

  it('compresses degraded service checks into one warning line plus status chips', async () => {
    mockHealthFetch.mockResolvedValue({ status: 'ok', db: 'degraded' });

    renderPage();

    expect(await screen.findByText('Estado del servicio')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Atención: base de datos requiere revisión antes de cambiar permisos o seguir con otras acciones administrativas\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText('API: ok')).toBeInTheDocument();
      expect(screen.getByText('Base de datos: degraded')).toBeInTheDocument();
      expect(screen.getAllByTestId('admin-service-health-chip')).toHaveLength(2);
    });

    expect(
      screen.queryByText(/Todo listo: API y base de datos responden correctamente\./i),
    ).not.toBeInTheDocument();
    expect(screen.getByRole('button', { name: /Revisar estado del servicio/i })).toBeInTheDocument();
    expect(
      screen.getByText(
        /Primero resuelve el estado del servicio; luego se habilitarán usuarios, auditoría y datos de ejemplo\./i,
      ),
    ).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Cargar datos de ejemplo/i })).not.toBeInTheDocument();
  });

  it('keeps first-run onboarding focused on service health until that gate is ready', async () => {
    mockHealthFetch.mockResolvedValue({ status: 'ok', db: 'degraded' });
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
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Primero resuelve el estado del servicio; luego se habilitarán usuarios, auditoría y datos de ejemplo\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText('Base de datos: degraded')).toBeInTheDocument();
    });

    expect(screen.queryByRole('link', { name: /1\. Estado del servicio/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('link', { name: /2\. Usuarios y roles/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('link', { name: /3\. Auditoría reciente/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Usuarios y roles')).not.toBeInTheDocument();
    expect(screen.queryByText('Auditoría reciente')).not.toBeInTheDocument();
    expect(screen.queryByText('Aún no hay usuarios administrables.')).not.toBeInTheDocument();
    expect(screen.queryByText(/La auditoría aparecerá cuando se registre el primer cambio\./i)).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /Opcional: ver Tokens de servicio/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos opcionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
  });

  it('keeps the first-run refresh action inside onboarding guidance when the health check fails', async () => {
    mockHealthFetch.mockRejectedValue(new Error('Sin conexión'));

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Sin conexión')).toBeInTheDocument();
      expect(screen.getAllByRole('button', { name: /Revisar estado del servicio/i })).toHaveLength(1);
    });

    expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
    const gettingStartedAlert = screen.getByText('Primeros pasos').closest('[role="alert"]');
    expect(gettingStartedAlert).not.toBeNull();
    expect(
      within(gettingStartedAlert as HTMLElement).getByRole('button', { name: /Revisar estado del servicio/i }),
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        /Primero resuelve el estado del servicio; luego se habilitarán usuarios, auditoría y datos de ejemplo\./i,
      ),
    ).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Cargar datos de ejemplo/i })).not.toBeInTheDocument();
  });

  it('keeps demo seeding hidden until first-run users and audit data are confirmed', async () => {
    mockListUsers.mockRejectedValue(new Error('Usuarios no disponibles'));

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Usuarios no disponibles')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Reintentar carga inicial/i })).toBeInTheDocument();
      expect(
        screen.getByText(
          /Actualiza el panel para confirmar usuarios y auditoría antes de cargar datos de ejemplo\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Cargar datos de ejemplo/i })).not.toBeInTheDocument();
  });

  it('keeps first-run guidance focused when only optional admin preview modules fail', async () => {
    mockConsolePreview.mockRejectedValue(new Error('Vista dinámica no disponible'));

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getByText(/No se pudo cargar el panel dinámico\. Mostrando la consola base\./i),
      ).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Cargar datos de ejemplo/i })).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Revisar estado del servicio/i }),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /Reintentar carga inicial/i }),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /Actualizar panel/i }),
    ).not.toBeInTheDocument();
  });

  it('keeps the header focused on refresh once the console already has admin data', async () => {
    mockListUsers.mockResolvedValue([buildAdminUser()]);

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByRole('button', { name: /Actualizar panel/i })).toBeInTheDocument();
    });

    expect(screen.queryByRole('button', { name: /Restablecer datos demo/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Datos de demostración')).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        /Restablece los datos de demo en ambientes de prueba cuando necesites repetir el flujo sin refrescos manuales extra\./i,
      ),
    ).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Cargar datos de ejemplo/i })).not.toBeInTheDocument();
  });

  it('separates the single-user role summary from the edit action so first-time admins see both clearly', async () => {
    mockListUsers.mockResolvedValue([buildAdminUser()]);

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
      expect(screen.getByText('Roles: Admin')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Editar roles de Ada Lovelace/i })).toHaveTextContent('Editar roles');
    });

    expect(screen.queryByRole('button', { name: /^Admin$/i })).not.toBeInTheDocument();
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
        screen.getByRole('button', { name: /Cargar datos de ejemplo/i }),
      ).toBeInTheDocument();
      expect(
        screen.getByRole('button', { name: /Cargar datos de ejemplo \(opcional\)/i }),
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
      expect(screen.getByTestId('admin-first-run-users-status')).toHaveTextContent('Aún no hay usuarios administrables.');
      expect(
        screen.getByTestId('admin-first-run-audit-status'),
      ).toHaveTextContent('La auditoría aparecerá cuando se registre el primer cambio.');
    });

    expect(
      screen.queryByText(
        /Carga datos de ejemplo para revisar usuarios, roles y auditoría sin tocar producción\./i,
      ),
    ).not.toBeInTheDocument();

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

  it('keeps fallback cards that only repeat the admin console intro out of first-run extras', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'workspace-ops',
          title: 'Operaciones del espacio',
          body: [
            'Revisa el estado del sistema, ajusta permisos y valida cambios recientes desde un solo lugar.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getAllByText(
          /Revisa el estado del sistema, ajusta permisos y valida cambios recientes desde un solo lugar\./i,
        ),
      ).toHaveLength(1);
    });

    expect(screen.queryByText('Operaciones del espacio')).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Opcional: ver Operaciones del espacio/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
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

  it('keeps access-management fallback cards out of first-run optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-access-management',
          title: 'Access management',
          body: [
            'Review team access before changing administrative permissions.',
          ],
        },
        {
          cardId: 'fallback-gestion-accesos',
          title: 'Gestión de accesos',
          body: [
            'Revisa accesos del equipo antes de cambiar permisos administrativos.',
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
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Access management')).not.toBeInTheDocument();
    expect(screen.queryByText('Gestión de accesos')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review team access before changing administrative permissions\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa accesos del equipo antes de cambiar permisos administrativos\./i),
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

  it('keeps system-status fallback cards out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-system-status',
          title: 'System status',
          body: ['Monitor API, database, and uptime before changing access.'],
        },
        {
          cardId: 'estado-sistema',
          title: 'Estado del sistema',
          body: ['Consulta disponibilidad, base de datos y latencia antes de ajustar accesos.'],
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
    expect(screen.queryByText('System status')).not.toBeInTheDocument();
    expect(screen.queryByText('Estado del sistema')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Monitor API, database, and uptime before changing access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Consulta disponibilidad, base de datos y latencia antes de ajustar accesos\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps connector-formatted built-in access titles out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-users-roles',
          title: 'Users & roles',
          body: ['Review which teammates can access each module.'],
        },
        {
          cardId: 'fallback-role-permissions',
          title: 'Roles & permissions',
          body: ['Map roles to the workspace permissions before changing access.'],
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
    expect(screen.queryByText('Users & roles')).not.toBeInTheDocument();
    expect(screen.queryByText('Roles & permissions')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review which teammates can access each module\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Map roles to the workspace permissions before changing access\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps wrapper-labeled built-in admin titles out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-service-health-section',
          title: 'Service health section',
          body: ['Verify API readiness and latency before opening admin access changes.'],
        },
        {
          cardId: 'fallback-users-roles-module',
          title: 'Users & roles module',
          body: ['Check which teammates can still open each admin workflow.'],
        },
        {
          cardId: 'fallback-auditoria-reciente-pantalla',
          title: 'Auditoría reciente pantalla',
          body: ['Sigue los cambios críticos del workspace desde un solo historial.'],
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
    expect(screen.queryByText('Service health section')).not.toBeInTheDocument();
    expect(screen.queryByText('Users & roles module')).not.toBeInTheDocument();
    expect(screen.queryByText('Auditoría reciente pantalla')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Verify API readiness and latency before opening admin access changes\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Check which teammates can still open each admin workflow\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Sigue los cambios críticos del workspace desde un solo historial\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps user-permission fallback titles out of first-run optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-user-permissions',
          title: 'Usuarios y permisos',
          body: ['Revisa que cada usuario tenga los permisos correctos antes de cambiar accesos.'],
        },
        {
          cardId: 'fallback-user-permissions-en',
          title: 'User permissions',
          body: ['Review which users can access admin workflows before making changes.'],
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
    expect(screen.queryByText('Usuarios y permisos')).not.toBeInTheDocument();
    expect(screen.queryByText('User permissions')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa que cada usuario tenga los permisos correctos/i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review which users can access admin workflows/i),
    ).not.toBeInTheDocument();
  });

  it('keeps user-administration fallback titles from duplicating the built-in users workflow', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-user-administration',
          title: 'User administration',
          body: ['Review admin users before changing access.'],
        },
        {
          cardId: 'fallback-administracion-usuarios',
          title: 'Administración de usuarios',
          body: ['Revisa usuarios administrables antes de cambiar accesos.'],
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
    expect(screen.queryByText('User administration')).not.toBeInTheDocument();
    expect(screen.queryByText('Administración de usuarios')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review admin users before changing access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa usuarios administrables antes de cambiar accesos\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps role-access fallback titles out of first-run optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-roles-accesos',
          title: 'Roles y accesos',
          body: ['Revisa qué accesos puede abrir cada rol antes de cambiar permisos.'],
        },
        {
          cardId: 'fallback-accesos-roles',
          title: 'Accesos y roles',
          body: ['Confirma los roles asignados antes de repetir un cambio administrativo.'],
        },
        {
          cardId: 'fallback-roles-access',
          title: 'Roles and access',
          body: ['Review which workflows each role can open before changing permissions.'],
        },
        {
          cardId: 'fallback-access-roles',
          title: 'Access and roles',
          body: ['Confirm role coverage before repeating an admin access change.'],
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
    expect(screen.queryByText('Roles y accesos')).not.toBeInTheDocument();
    expect(screen.queryByText('Accesos y roles')).not.toBeInTheDocument();
    expect(screen.queryByText('Roles and access')).not.toBeInTheDocument();
    expect(screen.queryByText('Access and roles')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa qué accesos puede abrir cada rol antes de cambiar permisos\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirm role coverage before repeating an admin access change\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps implementation-prefixed built-in fallback titles out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'preview-user-management',
          title: 'Fallback: User management',
          body: ['Review admin users before changing access.'],
        },
        {
          cardId: 'planned-service-health',
          title: 'Preview - Service health',
          body: ['Monitor API and database readiness before changing permissions.'],
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
    expect(screen.queryByText('Fallback: User management')).not.toBeInTheDocument();
    expect(screen.queryByText('Preview - Service health')).not.toBeInTheDocument();
    expect(screen.queryByText(/Review admin users before changing access\./i)).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Monitor API and database readiness before changing permissions\./i),
    ).not.toBeInTheDocument();
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

  it('keeps admin-overview fallback cards out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-admin-overview',
          title: 'Admin overview',
          body: ['Review service checks, access changes, and audit follow-up before changing settings.'],
        },
        {
          cardId: 'fallback-resumen-administrativo',
          title: 'Resumen administrativo',
          body: ['Consulta salud, accesos y auditoría antes de repetir revisiones.'],
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
    expect(screen.queryByText('Admin overview')).not.toBeInTheDocument();
    expect(screen.queryByText('Resumen administrativo')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review service checks, access changes, and audit follow-up/i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Consulta salud, accesos y auditoría antes de repetir revisiones\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps getting-started fallback cards from duplicating the first-run checklist', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-getting-started',
          title: 'Getting started',
          body: ['Review service health, users, roles, and audit activity before changing settings.'],
        },
        {
          cardId: 'fallback-inicio-rapido',
          title: 'Inicio rápido',
          body: ['Revisa salud, usuarios y auditoría antes de cambiar permisos.'],
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

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Getting started')).not.toBeInTheDocument();
    expect(screen.queryByText('Inicio rápido')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review service health, users, roles, and audit activity/i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa salud, usuarios y auditoría antes de cambiar permisos\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps generic settings fallback cards from duplicating the configuration shell', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-settings',
          title: 'Settings',
          body: ['Review workspace settings before changing permissions.'],
        },
        {
          cardId: 'fallback-preferences',
          title: 'Preferencias',
          body: ['Ajusta preferencias del sistema desde Configuración.'],
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
    expect(screen.queryByText('Settings')).not.toBeInTheDocument();
    expect(screen.queryByText('Preferencias')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review workspace settings before changing permissions\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Ajusta preferencias del sistema desde Configuración\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps system and workspace configuration fallback cards from duplicating the configuration shell', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-system-configuration',
          title: 'System configuration',
          body: ['Review workspace toggles before changing permissions.'],
        },
        {
          cardId: 'fallback-workspace-configuration',
          title: 'Configuración del espacio de trabajo',
          body: ['Valida ajustes globales antes de cambiar accesos.'],
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
    expect(screen.queryByText('System configuration')).not.toBeInTheDocument();
    expect(screen.queryByText('Configuración del espacio de trabajo')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review workspace toggles before changing permissions\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Valida ajustes globales antes de cambiar accesos\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps demo-data fallback cards from duplicating the first-run seed action', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'demo-data',
          title: 'Demo workspace',
          body: ['Generate sample users, roles, and audit events for review.'],
        },
        {
          cardId: 'fallback-demo-fixtures',
          title: 'Datos de ejemplo',
          body: ['Prepara usuarios y auditoría de demostración para validar el panel.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getByRole('button', { name: /Cargar datos de ejemplo/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Demo workspace')).not.toBeInTheDocument();
    expect(screen.queryByText(/^Datos de ejemplo$/i)).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Generate sample users, roles, and audit events for review\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Prepara usuarios y auditoría de demostración para validar el panel\./i),
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
          { name: /^Opcional: ver 1 módulo adicional$/i },
        ),
      ).toBeInTheDocument();
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
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(
      await within(getFirstRunAlert()).findByRole('button', { name: /Ocultar módulo opcional/i }),
    ).toBeInTheDocument();
    expect(
      within(getFirstRunAlert()).getByText(
        /Revísalo aquí solo si ya necesitas este flujo extra, sin salir del recorrido inicial\./i,
      ),
    ).toBeInTheDocument();
    expect(await within(getFirstRunAlert()).findAllByText('Tokens de servicio')).toHaveLength(1);
    expect(
      within(getFirstRunAlert()).getByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(within(getFirstRunAlert()).getByRole('button', { name: /Ocultar módulo opcional/i })).toBeInTheDocument();
    expect(screen.getAllByRole('button', { name: /Ocultar módulo opcional/i })).toHaveLength(1);

    expect(screen.queryByText('Datos de demostración')).not.toBeInTheDocument();
  });

  it('keeps a single long first-run module title compact until the admin expands it', async () => {
    const user = userEvent.setup();
    const longModuleTitle = 'Configuración operativa para credenciales externas compartidas';
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'shared-credentials',
          title: longModuleTitle,
          body: [
            'Revisa credenciales técnicas y responsables sin salir del recorrido inicial.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();
    await screen.findByText('Primeros pasos');

    const firstRunAlert = screen.getByText('Primeros pasos').closest('[role="alert"]');
    if (!(firstRunAlert instanceof HTMLElement)) {
      throw new Error('Expected first-run alert container to render');
    }

    await waitFor(() => {
      expect(
        within(firstRunAlert).getByRole('button', { name: 'Opcional: ver 1 módulo adicional' }),
      ).toBeInTheDocument();
    });

    const actionButton = within(firstRunAlert).getByRole('button', { name: 'Opcional: ver 1 módulo adicional' });
    expect(actionButton).not.toHaveAttribute('title');
    expect(actionButton).toHaveTextContent('Opcional: ver 1 módulo adicional');
    expect(within(firstRunAlert).queryByText(longModuleTitle)).not.toBeInTheDocument();

    await user.click(actionButton);

    expect(await within(firstRunAlert).findByRole('button', { name: /Ocultar módulo opcional/i })).toBeInTheDocument();
    expect(await within(firstRunAlert).findAllByText(longModuleTitle)).toHaveLength(1);
    expect(
      within(firstRunAlert).getByText(/Revisa credenciales técnicas y responsables sin salir del recorrido inicial\./i),
    ).toBeInTheDocument();
  });

  it('removes the first-run optional modules area when no actionable fallback cards remain', async () => {
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

    const { queryClient } = renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await user.click(
      await screen.findByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await screen.findByRole('button', { name: /Ocultar módulo opcional/i })).toBeInTheDocument();
    expect(screen.getAllByText('Tokens de servicio')).toHaveLength(1);

    act(() => {
      queryClient.setQueryData(['admin', 'console'], { status: 'preview', cards: [] });
    });

    await waitFor(() => {
      expect(screen.queryByRole('button', { name: /Ocultar módulo opcional/i })).not.toBeInTheDocument();
      expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
    });

    expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
  });

  it('collapses expanded first-run modules when fallback discovery changes the module set', async () => {
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

    const { queryClient } = renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await user.click(
      await screen.findByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await screen.findByRole('button', { name: /Ocultar módulo opcional/i })).toBeInTheDocument();
    expect(screen.getByText('Tokens de servicio')).toBeInTheDocument();

    act(() => {
      queryClient.setQueryData(['admin', 'console'], {
        status: 'preview',
        cards: [
          {
            cardId: 'integrations',
            title: 'Integraciones',
            body: [
              'Revisa conectores pendientes sin salir de la consola.',
            ],
          },
        ],
      });
    });

    await waitFor(() => {
      expect(screen.queryByText('Módulos opcionales')).not.toBeInTheDocument();
      expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
      expect(screen.getByRole('button', { name: /^Opcional: ver Integraciones$/i })).toBeInTheDocument();
    });

    expect(
      screen.queryByText(/Revisa conectores pendientes sin salir de la consola\./i),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
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
          { name: /^Opcional: ver 1 módulo adicional$/i },
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await screen.findByRole('button', { name: /Ocultar módulo opcional/i })).toBeInTheDocument();
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
          { name: /^Opcional: ver 1 módulo adicional$/i },
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('button', { name: /^Opcional: ver 2 módulos adicionales$/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Credenciales compartidas')).not.toBeInTheDocument();

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await screen.findByRole('button', { name: /Ocultar módulo opcional/i })).toBeInTheDocument();
    expect(screen.getAllByText('Tokens de servicio')).toHaveLength(1);
    expect(screen.queryByText('Credenciales compartidas')).not.toBeInTheDocument();
    expect(
      screen.getAllByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\.?/i,
      ),
    ).toHaveLength(1);
  });

  it('deduplicates fallback cards when the same body paragraphs arrive in a different order', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens',
          title: 'Tokens de servicio',
          body: [
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
            'Programa una rotación semanal sin salir de esta consola.',
          ],
        },
        {
          cardId: 'shared-credentials',
          title: 'Credenciales compartidas',
          body: [
            'Programa una rotación semanal sin salir de esta consola.',
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
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
          { name: /^Opcional: ver 1 módulo adicional$/i },
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('button', { name: /^Opcional: ver 2 módulos adicionales$/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Credenciales compartidas')).not.toBeInTheDocument();

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await screen.findByRole('button', { name: /Ocultar módulo opcional/i })).toBeInTheDocument();
    expect(screen.getAllByText('Tokens de servicio')).toHaveLength(1);
    expect(screen.queryByText('Credenciales compartidas')).not.toBeInTheDocument();
    expect(
      screen.getAllByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).toHaveLength(1);
    expect(
      screen.getAllByText(/Programa una rotación semanal sin salir de esta consola\./i),
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
          { name: /^Opcional: ver 1 módulo adicional$/i },
        ),
      ).toBeInTheDocument();
    });

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await screen.findByRole('button', { name: /Ocultar módulo opcional/i })).toBeInTheDocument();
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

  it('merges renamed fallback cards that reuse the same module id', async () => {
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
          cardId: 'service-tokens',
          title: 'Credenciales compartidas',
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
          { name: /^Opcional: ver 1 módulo adicional$/i },
        ),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 2 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Credenciales compartidas')).not.toBeInTheDocument();

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await screen.findByRole('button', { name: /Ocultar módulo opcional/i })).toBeInTheDocument();
    expect(screen.getAllByText('Tokens de servicio')).toHaveLength(1);
    expect(screen.queryByText('Credenciales compartidas')).not.toBeInTheDocument();
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
          { name: /^Opcional: ver 1 módulo adicional$/i },
        ),
      ).toBeInTheDocument();
    });

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await screen.findByRole('button', { name: /Ocultar módulo opcional/i })).toBeInTheDocument();
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
          { name: /^Opcional: ver 1 módulo adicional$/i },
        ),
      ).toBeInTheDocument();
    });

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await screen.findByRole('button', { name: /Ocultar módulo opcional/i })).toBeInTheDocument();
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

  it('shows one shared refresh notice while standalone additional modules refetch in place', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([buildAdminUser()]);
    mockConsolePreview
      .mockResolvedValueOnce({
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
        ],
      })
      .mockImplementationOnce(() => new Promise(() => undefined));

    const { queryClient } = renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole(
          'button',
          { name: /^Ver 2 módulos adicionales$/i },
        ),
      ).toBeInTheDocument();
    });

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Ver 2 módulos adicionales$/i },
      ),
    );

    await waitFor(() => {
      expect(screen.getByText('Tokens de servicio')).toBeInTheDocument();
      expect(screen.getByText('Integraciones')).toBeInTheDocument();
    });

    expect(screen.queryByText(/Actualizando módulos…/i)).not.toBeInTheDocument();

    act(() => {
      void queryClient.invalidateQueries({ queryKey: ['admin', 'console'] });
    });

    await waitFor(() => {
      expect(screen.getByText(/Actualizando módulos…/i)).toBeInTheDocument();
    });

    expect(screen.getAllByText(/Actualizando módulos…/i)).toHaveLength(1);
    expect(screen.queryByText(/^Actualizando…$/i)).not.toBeInTheDocument();
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

  it('ignores terse Spanish coming-soon preview cards so fallback discovery does not add dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens-placeholder',
          title: 'Tokens de servicio',
          body: ['Próximamente.'],
        },
        {
          cardId: 'api-access-placeholder',
          title: 'Acceso API',
          body: ['Próximamente podrás revisar credenciales técnicas aquí.'],
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

    expect(screen.queryByRole('button', { name: /Tokens de servicio|Acceso API/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
    expect(screen.queryByText('Acceso API')).not.toBeInTheDocument();
    expect(screen.queryByText(/Próximamente/i)).not.toBeInTheDocument();
  });

  it('ignores terse implementation-placeholder cards so first-run keeps only actionable modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens-placeholder',
          title: 'Service tokens',
          body: ['Not implemented yet.', 'TBD'],
        },
        {
          cardId: 'api-access-placeholder',
          title: 'Acceso API',
          body: ['Pendiente de implementación.', 'En construcción.'],
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

    expect(screen.queryByRole('button', { name: /Service tokens|Acceso API/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Service tokens')).not.toBeInTheDocument();
    expect(screen.queryByText('Acceso API')).not.toBeInTheDocument();
    expect(screen.queryByText(/Not implemented yet/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Pendiente de implementación/i)).not.toBeInTheDocument();
  });

  it('ignores in-development preview cards so fallback discovery does not add dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'integrations-placeholder',
          title: 'Integraciones',
          body: ['En desarrollo.'],
        },
        {
          cardId: 'service-tokens-placeholder',
          title: 'Service tokens',
          body: ['Work in progress.', 'To be determined.'],
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

    expect(screen.queryByRole('button', { name: /Integraciones|Service tokens/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Service tokens')).not.toBeInTheDocument();
    expect(screen.queryByText(/En desarrollo/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Work in progress/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/To be determined/i)).not.toBeInTheDocument();
  });

  it('ignores no-data fallback cards so first-run users do not open dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'integrations-empty',
          title: 'Integraciones',
          body: ['No data available yet.'],
        },
        {
          cardId: 'service-tokens-empty',
          title: 'Tokens de servicio',
          body: ['Sin datos disponibles.'],
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

    expect(screen.queryByRole('button', { name: /Integraciones|Tokens de servicio/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
    expect(screen.queryByText(/No data available/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Sin datos disponibles/i)).not.toBeInTheDocument();
  });

  it('ignores no-record fallback cards so first-run users do not open dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'integrations-records-empty',
          title: 'Integraciones',
          body: ['No hay registros todavía.'],
        },
        {
          cardId: 'service-tokens-items-empty',
          title: 'Service tokens',
          body: ['No items found.'],
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

    expect(screen.queryByRole('button', { name: /Integraciones|Service tokens/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Service tokens')).not.toBeInTheDocument();
    expect(screen.queryByText(/No hay registros/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No items found/i)).not.toBeInTheDocument();
  });

  it('ignores no-result fallback cards so first-run users do not open dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'integrations-results-empty',
          title: 'Integraciones',
          body: ['No results found.'],
        },
        {
          cardId: 'service-tokens-results-empty',
          title: 'Tokens de servicio',
          body: ['Sin resultados.'],
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

    expect(screen.queryByRole('button', { name: /Integraciones|Tokens de servicio/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
    expect(screen.queryByText(/No results found/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Sin resultados/i)).not.toBeInTheDocument();
  });

  it('ignores no-information fallback cards so first-run users do not open dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'integrations-information-empty',
          title: 'Integraciones',
          body: ['Sin información disponible.'],
        },
        {
          cardId: 'service-tokens-elements-empty',
          title: 'Service tokens',
          body: ['No elements available yet.'],
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

    expect(screen.queryByRole('button', { name: /Integraciones|Service tokens/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Service tokens')).not.toBeInTheDocument();
    expect(screen.queryByText(/Sin información disponible/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No elements available/i)).not.toBeInTheDocument();
  });

  it('ignores permission and unavailable fallback cards so first-run users do not open dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens-denied',
          title: 'Tokens de servicio',
          body: ['No tienes permisos suficientes para abrir esta sección.'],
        },
        {
          cardId: 'integrations-unavailable',
          title: 'Integraciones',
          body: ['Temporarily unavailable. Permission required.'],
        },
        {
          cardId: 'admin-role-required',
          title: 'Credenciales admin',
          body: ['Requires admin role before this module can be opened.'],
        },
        {
          cardId: 'restricted-admin-access',
          title: 'Acceso restringido',
          body: ['Requiere permisos de administrador para abrir esta sección.'],
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

    expect(
      screen.queryByRole('button', {
        name: /Tokens de servicio|Integraciones|Credenciales admin|Acceso restringido/i,
      }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Credenciales admin')).not.toBeInTheDocument();
    expect(screen.queryByText('Acceso restringido')).not.toBeInTheDocument();
    expect(screen.queryByText(/No tienes permisos suficientes/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Temporarily unavailable/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Requires admin role/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Requiere permisos de administrador/i)).not.toBeInTheDocument();
  });

  it('ignores dedicated-flow token fallback cards so first-run users do not open dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'api-tokens',
          title: 'Tokens API',
          body: [
            'Los tokens de servicio deben administrarse desde un flujo dedicado.',
            'El acceso quedará separado de usuarios humanos para integraciones internas.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Cargar datos de ejemplo/i })).toBeInTheDocument();
    });

    expect(screen.queryByRole('button', { name: /Tokens API/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens API')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Los tokens de servicio deben administrarse desde un flujo dedicado\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/El acceso quedará separado de usuarios humanos para integraciones internas\./i),
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
          { name: /^Opcional: ver 1 módulo adicional$/i },
        ),
      ).toBeInTheDocument();
    });

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await screen.findByRole('button', { name: /Ocultar módulo opcional/i })).toBeInTheDocument();
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

  it('strips repeated card titles from fallback bodies so optional modules do not echo themselves', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens',
          title: 'Tokens de servicio',
          body: [
            'Tokens de servicio',
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
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
          { name: /^Opcional: ver 1 módulo adicional$/i },
        ),
      ).toBeInTheDocument();
    });

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await screen.findByRole('button', { name: /Ocultar módulo opcional/i })).toBeInTheDocument();
    expect(screen.getAllByText(/^Tokens de servicio$/i)).toHaveLength(1);
    expect(
      screen.getByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).toBeInTheDocument();
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
      expect(screen.getByText(/Acción:\s*Roles actualizados/i)).toBeInTheDocument();
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
      expect(screen.getByText('Tokens de servicio')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Ver detalles de Tokens de servicio/i })).toBeInTheDocument();
      expect(screen.queryByRole('button', { name: /Ver 1 módulo adicional/i })).not.toBeInTheDocument();
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
    });

    expect(
      screen.getByText('Opcional'),
    ).toBeInTheDocument();
    expect(
      screen.queryByText(
        /Tarjeta auxiliar del panel\. Ábrela solo cuando ya confirmaste salud, usuarios y auditoría\./i,
      ),
    ).not.toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /Ver detalles de Tokens de servicio/i }),
    ).toHaveAttribute('aria-expanded', 'false');
    expect(
      screen.getByRole('button', { name: /Ver detalles de Tokens de servicio/i }),
    ).toHaveTextContent('Ver detalles');
    expect(
      screen.queryByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /Ver detalles de Tokens de servicio/i }));

    expect(await screen.findAllByText('Tokens de servicio')).toHaveLength(1);
    expect(
      screen.getByText('Opcional'),
    ).toBeInTheDocument();
    expect(
      screen.queryByText(
        /Tarjeta auxiliar del panel\. Ábrela solo cuando ya confirmaste salud, usuarios y auditoría\./i,
      ),
    ).not.toBeInTheDocument();
    expect(
      screen.getByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /Ocultar módulo adicional/i }),
    ).toHaveAttribute('aria-expanded', 'true');
  });

  it('keeps a single long standalone module title compact until the admin expands it', async () => {
    const user = userEvent.setup();
    const longModuleTitle = 'Configuración operativa para credenciales externas compartidas';
    mockListUsers.mockResolvedValue([buildAdminUser()]);
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'shared-credentials',
          title: longModuleTitle,
          body: [
            'Revisa credenciales técnicas y responsables sin salir de esta consola.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText(longModuleTitle)).toBeInTheDocument();
      expect(screen.getByRole('button', { name: `Ver detalles de ${longModuleTitle}` })).toBeInTheDocument();
    });

    const actionButton = screen.getByRole('button', { name: `Ver detalles de ${longModuleTitle}` });
    expect(actionButton).toHaveAttribute('title', `Ver detalles de ${longModuleTitle}`);
    expect(actionButton).toHaveTextContent('Ver detalles');
    expect(screen.getByText('Opcional')).toBeInTheDocument();
    expect(
      screen.queryByText(
        /Tarjeta auxiliar del panel\. Ábrela solo cuando ya confirmaste salud, usuarios y auditoría\./i,
      ),
    ).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /^Ver 1 módulo adicional$/i })).not.toBeInTheDocument();
    await user.click(actionButton);

    expect(await screen.findAllByText(longModuleTitle)).toHaveLength(1);
    expect(
      screen.getByText(/Revisa credenciales técnicas y responsables sin salir de esta consola\./i),
    ).toBeInTheDocument();
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

  it('keeps demo seeding as a one-shot first-run action with one post-seed success cue', async () => {
    const user = userEvent.setup();

    renderPage();

    await user.click(
      await screen.findByRole('button', { name: /Cargar datos de ejemplo/i }),
    );

    await waitFor(() => {
      expect(mockSeed).toHaveBeenCalledTimes(1);
      expect(
        screen.getByText(/Datos de demostración preparados correctamente\./i),
      ).toBeInTheDocument();
      expect(
        screen.queryByText(
          /Datos de ejemplo cargados\. Espera el refresco automático de usuarios y auditoría antes de repetir cualquier revisión\./i,
        ),
      ).not.toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Cargar datos de ejemplo/i }),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /Cargando ejemplo/i }),
    ).not.toBeInTheDocument();
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
      expect(screen.getByText(/Acción:\s*Roles actualizados/i)).toBeInTheDocument();
    });

    expect(screen.queryByText('Aún no hay usuarios administrables.')).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        /Aquí aparecerán los usuarios administrables\. Cuando exista el primero, podrás editar sus roles desde esta misma vista\./i,
      ),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Haz clic en un rol para editarlo desde esta misma vista\./i),
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
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
      expect(screen.getByText('Usuario: ada')).toBeInTheDocument();
      expect(
        screen.getByText(/Revisa los roles actuales y usa Editar roles para ajustar permisos desde esta misma vista\./i),
      ).toBeInTheDocument();
      expect(
        screen.getByText(/Vista compacta: último acceso y estado aparecerán cuando aporten contexto\./i),
      ).toBeInTheDocument();
      expect(screen.getByText('Admin')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveTextContent('Editar');
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveAttribute('title', 'Editar roles de Ada Lovelace. Roles actuales: Admin');
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).not.toHaveAttribute('aria-describedby');
      expect(screen.queryByText('Party #9')).not.toBeInTheDocument();
      expect(screen.queryByTestId('EditOutlinedIcon')).not.toBeInTheDocument();
      expect(screen.queryByText('Editar roles: Admin')).not.toBeInTheDocument();
      expect(screen.queryByText(/Roles · Clic para editar/i)).not.toBeInTheDocument();
      expect(screen.queryByText(/Haz clic en el rol para editarlo aquí/i)).not.toBeInTheDocument();
      expect(screen.queryByText(/Edita sus roles aquí/i)).not.toBeInTheDocument();
      expect(screen.queryByText(/^Roles$/i)).not.toBeInTheDocument();
      expect(screen.queryByText(/^Último acceso:/i)).not.toBeInTheDocument();
      expect(screen.queryByText('Estado: Activo')).not.toBeInTheDocument();
      expect(screen.queryByText(/Revisa esta cuenta aquí/i)).not.toBeInTheDocument();
      expect(
        screen.queryByText(
          /Primer usuario administrable\. Usa el botón del rol para ajustar accesos; cuando exista una segunda cuenta, volverá la tabla comparativa\./i,
        ),
      ).not.toBeInTheDocument();
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
        screen.getByText(/Revisa los roles actuales y usa Editar roles para ajustar permisos desde esta misma vista\./i),
      ).toBeInTheDocument();
      expect(
        screen.getByText(/Vista compacta: último acceso y estado aparecerán cuando aporten contexto\./i),
      ).toBeInTheDocument();
      expect(screen.getAllByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveLength(1);
    });

    expect(
      screen.queryByText(
        /Primer usuario administrable\. Usa el botón del rol para ajustar accesos; cuando exista una segunda cuenta, volverá la tabla comparativa\./i,
      ),
    ).not.toBeInTheDocument();

    expect(screen.queryByRole('columnheader', { name: /^Usuario$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Roles$/i })).not.toBeInTheDocument();
  });

  it('merges duplicate admin user records so one row keeps the complete role summary', async () => {
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        roles: ['Admin'],
      }),
      buildAdminUser({
        displayName: '   ',
        roles: ['Manager'],
        lastSeenAt: '2026-04-10T12:00:00.000Z',
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getAllByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveLength(1);
      expect(screen.getByText('Admin, Manager')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveTextContent('Editar roles');
      expect(screen.getByText(/^Último acceso:/i)).toBeInTheDocument();
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
        screen.getByText(/Revisa los roles actuales y usa Editar roles para ajustar permisos desde esta misma vista\./i),
      ).toBeInTheDocument();
      expect(screen.getByText('Estado: Invitado')).toBeInTheDocument();
    });

    expect(
      screen.queryByText(
        /Primer usuario administrable\. Usa el botón del rol para ajustar accesos; cuando exista una segunda cuenta, volverá la tabla comparativa\./i,
      ),
    ).not.toBeInTheDocument();
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
        screen.getByText(/Revisa los roles actuales y usa Editar roles para ajustar permisos desde esta misma vista\./i),
      ).toBeInTheDocument();
      expect(screen.getByText(/^Último acceso:/i)).toBeInTheDocument();
    });

    expect(
      screen.queryByText(
        /Primer usuario administrable\. Usa el botón del rol para ajustar accesos; cuando exista una segunda cuenta, volverá la tabla comparativa\./i,
      ),
    ).not.toBeInTheDocument();
  });

  it('keeps one shared role-edit hint above the table instead of repeating it in the column header', async () => {
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
          /Revisa los roles actuales y usa Editar roles para ajustar permisos desde esta misma vista\. Vista compacta: último acceso y estado aparecerán cuando aporten contexto\./i,
        ),
      ).toBeInTheDocument();
      expect(
        screen.queryByText(/^Vista compacta: último acceso y estado aparecerán cuando aporten contexto\.$/i),
      ).not.toBeInTheDocument();
      expect(screen.queryByText(/Haz clic en un rol para editarlo desde esta misma vista\./i)).not.toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Roles$/i })).toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Roles editables$/i })).not.toBeInTheDocument();
      expect(screen.queryByText(/Clic para editar/i)).not.toBeInTheDocument();
      expect(screen.queryByText('Editar aquí')).not.toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Roles y edición$/i })).not.toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Permisos$/i })).not.toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /Último acceso/i })).not.toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
      expect(screen.getByText('Admin')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveTextContent('Editar roles');
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveAttribute('title', 'Editar roles de Ada Lovelace. Roles actuales: Admin');
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).not.toHaveAttribute('aria-describedby');
      expect(screen.getByText('Manager')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Editar roles de Grace Hopper' })).toHaveTextContent('Editar');
      expect(screen.getByRole('button', { name: 'Editar roles de Grace Hopper' })).toHaveAttribute('title', 'Editar roles de Grace Hopper. Roles actuales: Manager');
      expect(screen.getByRole('button', { name: 'Editar roles de Grace Hopper' })).not.toHaveAttribute('aria-describedby');
      expect(screen.queryByTestId('EditOutlinedIcon')).not.toBeInTheDocument();
      expect(screen.queryByText(/^Editar$/i)).not.toBeInTheDocument();
      expect(screen.getAllByRole('button', { name: /Editar roles de /i })).toHaveLength(2);
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
          /Revisa los roles actuales y usa Editar roles para ajustar permisos desde esta misma vista\. Vista compacta: último acceso y estado aparecerán cuando aporten contexto\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.queryByText(/Haz clic en un rol para editarlo desde esta misma vista\./i)).not.toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Roles$/i })).toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Roles editables$/i })).not.toBeInTheDocument();
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
        screen.getByText(/Revisa los roles actuales y usa Editar roles para ajustar permisos desde esta misma vista\./i),
      ).toBeInTheDocument();
      expect(screen.queryByText(/Haz clic en un rol para editarlo desde esta misma vista\./i)).not.toBeInTheDocument();
      expect(screen.queryByText(/Vista compacta:/i)).not.toBeInTheDocument();
      expect(screen.queryByText(/Clic para editar/i)).not.toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Roles$/i })).toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Roles editables$/i })).not.toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /Último acceso/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Estado$/i })).toBeInTheDocument();
      expect(screen.getByText('Invitado')).toBeInTheDocument();
      expect(screen.queryByTestId('EditOutlinedIcon')).not.toBeInTheDocument();
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

  it('sorts admin users by visible identity so the access table stays easy to scan', async () => {
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        userId: 103,
        username: 'zoe',
        displayName: 'Zoe Washburne',
        partyId: 11,
        roles: ['ReadOnly'],
      }),
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
      expect(screen.getByText('Zoe Washburne')).toBeInTheDocument();
    });

    expectToAppearBefore(screen.getByText('Ada Lovelace'), screen.getByText('Grace Hopper'));
    expectToAppearBefore(screen.getByText('Grace Hopper'), screen.getByText('Zoe Washburne'));
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
      expect(screen.getByText('Admin, Manager')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveTextContent('Editar');
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

  it('keeps long role lists compact until the admin opens the role editor', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        roles: ['Admin', 'Manager', 'Engineer', 'Teacher', 'Reception'],
      }),
      buildAdminUser({
        userId: 102,
        username: 'grace',
        displayName: 'Grace Hopper',
        partyId: 10,
        roles: ['ReadOnly'],
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      const roleButton = screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' });

      expect(roleButton).toHaveTextContent('Editar');
      expect(roleButton).toHaveAttribute('title', 'Editar roles de Ada Lovelace. Roles actuales: Admin, Manager, Engineer, Teacher, Reception');
      expect(screen.getByText('Admin, Manager +3 roles')).toBeInTheDocument();
      expect(screen.queryByText('Engineer, Teacher, Reception')).not.toBeInTheDocument();
    });

    expect(
      screen.queryByText('Admin, Manager, Engineer, Teacher, Reception'),
    ).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' }));

    expect(
      await screen.findByText(
        /Roles actuales: Admin, Manager, Engineer, Teacher, Reception\. Ajusta la selección para abrir o retirar módulos en esta cuenta\./i,
      ),
    ).toBeInTheDocument();

    const rolesSelect = document.body.querySelector('[role="combobox"]');
    if (!(rolesSelect instanceof HTMLElement)) {
      throw new Error('Roles select not found');
    }

    expect(rolesSelect).toHaveTextContent('Admin, Manager +3 roles');
    expect(rolesSelect).toHaveAttribute('title', 'Admin, Manager, Engineer, Teacher, Reception');
    expect(rolesSelect).not.toHaveTextContent('Engineer, Teacher, Reception');
  });

  it('turns empty role assignments into an explicit action while keeping the edit flow accurate', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        roles: [],
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Sin roles')).toBeInTheDocument();
      expect(
        screen.getByText(/Revisa los roles actuales y usa Asignar roles para ajustar permisos desde esta misma vista\./i),
      ).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveTextContent('Asignar roles');
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveAttribute('title', 'Editar roles de Ada Lovelace. Roles actuales: Sin roles');
      expect(screen.queryByText(/^—$/i)).not.toBeInTheDocument();
    });

    await user.click(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' }));

    expect(
      await screen.findByText(
        /Roles actuales: Sin roles\. Ajusta la selección para abrir o retirar módulos en esta cuenta\./i,
      ),
    ).toBeInTheDocument();
    const rolesSelect = document.body.querySelector('[role="combobox"]');
    if (!(rolesSelect instanceof HTMLElement)) {
      throw new Error('Roles select not found');
    }

    expect(rolesSelect).toHaveTextContent('Asignar roles');
    expect(rolesSelect).toHaveAttribute('title', 'Sin roles');
    expect(rolesSelect).not.toHaveTextContent('—');
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

  it('pins currently assigned roles to the top of the role editor so admins can verify the current state first', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([buildAdminUser({ roles: ['Teacher', 'Reception'] })]);

    renderPage();

    const editButton = await screen.findByRole('button', { name: 'Editar roles de Ada Lovelace' });
    await user.click(editButton);

    const rolesSelect = document.body.querySelector('[role="combobox"]');
    if (!(rolesSelect instanceof HTMLElement)) {
      throw new Error('Roles select not found');
    }

    await user.click(rolesSelect);

    const teacherOption = getMenuItemByText('Teacher');
    const receptionOption = getMenuItemByText('Reception');
    const adminOption = getMenuItemByText('Admin');

    expectToAppearBefore(teacherOption, adminOption);
    expectToAppearBefore(receptionOption, adminOption);
  });

  it('keeps newly selected roles below the original role set while the editor stays open', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([buildAdminUser({ roles: ['Teacher', 'Reception'] })]);

    renderPage();

    const editButton = await screen.findByRole('button', { name: 'Editar roles de Ada Lovelace' });
    await user.click(editButton);

    const rolesSelect = document.body.querySelector('[role="combobox"]');
    if (!(rolesSelect instanceof HTMLElement)) {
      throw new Error('Roles select not found');
    }

    await user.click(rolesSelect);
    await user.click(getMenuItemByText('Admin'));

    const teacherOption = getMenuItemByText('Teacher');
    const receptionOption = getMenuItemByText('Reception');
    const adminOption = getMenuItemByText('Admin');

    expectToAppearBefore(teacherOption, adminOption);
    expectToAppearBefore(receptionOption, adminOption);
    expect(
      await screen.findByText(/Cambio pendiente: agregar Admin\./i),
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
      expect(screen.getByText(/Acción:\s*Roles actualizados/i)).toBeInTheDocument();
      expect(screen.getByText(/Entidad:\s*Usuario · 101/i)).toBeInTheDocument();
      expect(screen.getByText(/Actor:\s*Usuario #101/i)).toBeInTheDocument();
      expect(screen.getByText(/Detalle:\s*Admin -> Admin, Manager/i)).toBeInTheDocument();
    });
    expect(screen.getByText(/Acción:\s*Roles actualizados/i)).toHaveAttribute('title', 'roles.updated');
    expect(screen.getByText(/Entidad:\s*Usuario · 101/i)).toHaveAttribute('title', 'user · 101');
    expect(screen.queryByText(/Acción:\s*roles\.updated/i)).not.toBeInTheDocument();

    expect(
      screen.queryByText(/Confirma quién cambió qué y cuándo antes de repetir una acción o ajustar permisos\./i),
    ).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Fecha$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Entidad$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acción$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Actor$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Detalle$/i })).not.toBeInTheDocument();
  });

  it('shows the known admin identity in audit attribution before using a labeled id fallback', async () => {
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
      expect(screen.getByText(/Acción:\s*Package created/i)).toBeInTheDocument();
      expect(screen.getByText(/Entidad:\s*Paquete · PKG-1/i)).toBeInTheDocument();
    });

    expect(screen.getByText(/Acción:\s*Package created/i)).toHaveAttribute('title', 'package.created');
    expect(screen.getByText(/Entidad:\s*Paquete · PKG-1/i)).toHaveAttribute('title', 'package · PKG-1');
    expect(screen.queryByText(/Acción:\s*package\.created/i)).not.toBeInTheDocument();
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
      expect(screen.getByText(/Acción:\s*Roles actualizados/i)).toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /^Fecha$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Entidad$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acción$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Actor$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Detalle$/i })).not.toBeInTheDocument();
    expect(screen.queryByText(/roles\.updated/i)).not.toBeInTheDocument();
    expect(screen.getByText(/Acción:\s*Roles actualizados/i)).toHaveAttribute('title', 'roles.updated');
  });

  it('merges repeated audit ids so one richer event keeps the compact first-event summary', async () => {
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'audit-1',
        actorId: null,
        entity: 'user',
        entityId: '101',
        action: 'roles.updated',
        diff: '   ',
        createdAt: '2026-04-09T15:30:00.000Z',
      },
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
      expect(screen.getByText(/Acción:\s*Roles actualizados/i)).toBeInTheDocument();
      expect(screen.getByText(/Actor:\s*Usuario #101/i)).toBeInTheDocument();
      expect(screen.getByText(/Detalle:\s*Admin -> Admin, Manager/i)).toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /^Fecha$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Entidad$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acción$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Actor$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Detalle$/i })).not.toBeInTheDocument();
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
          /Vista compacta: actor y detalle aparecerán cuando aporten contexto\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Fecha$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Entidad$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Acción$/i })).toBeInTheDocument();
      expect(screen.getByText('Package created')).toBeInTheDocument();
      expect(screen.getByText('Package synced')).toBeInTheDocument();
    });

    expect(screen.getByText('Package created')).toHaveAttribute('title', 'package.created');
    expect(screen.getByText('Package synced')).toHaveAttribute('title', 'package.synced');
    expect(screen.queryByText('package.created')).not.toBeInTheDocument();
    expect(screen.queryByText('package.synced')).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Actor$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Detalle$/i })).not.toBeInTheDocument();
    expect(screen.queryByText(/^Sistema$/i)).not.toBeInTheDocument();
  });

  it('shows user-facing audit entity labels while preserving raw entity details', async () => {
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
        diff: 'Admin -> Manager',
        createdAt: '2026-04-09T16:00:00.000Z',
      },
    ]);

    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Usuario · USR-1')).toHaveAttribute('title', 'user · USR-1');
      expect(screen.getByText('Paquete · PKG-1')).toHaveAttribute('title', 'package · PKG-1');
    });

    expect(screen.queryByText('user · USR-1')).not.toBeInTheDocument();
    expect(screen.queryByText('package · PKG-1')).not.toBeInTheDocument();
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
        screen.queryByText(/Vista compacta:/i),
      ).not.toBeInTheDocument();
      expect(
        screen.queryByText(
          /Confirma quién cambió qué y cuándo antes de repetir una acción o ajustar permisos\./i,
        ),
      ).not.toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Actor$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Detalle$/i })).toBeInTheDocument();
      expect(screen.getByText('Usuario #777')).toBeInTheDocument();
      expect(screen.getByText('Admin -> Admin, Manager')).toBeInTheDocument();
    });

    expect(screen.queryByText(/^777$/i)).not.toBeInTheDocument();
  });

  it('sorts the recent audit table from newest to oldest regardless of API order', async () => {
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'audit-older',
        actorId: null,
        entity: 'package',
        entityId: 'PKG-1',
        action: 'package.created',
        diff: null,
        createdAt: '2026-04-09T15:30:00.000Z',
      },
      {
        auditId: 'audit-newer',
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
      expect(screen.getByText('Package created')).toBeInTheDocument();
      expect(screen.getByText('Roles actualizados')).toBeInTheDocument();
    });

    expectToAppearBefore(screen.getByText('Roles actualizados'), screen.getByText('Package created'));
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
