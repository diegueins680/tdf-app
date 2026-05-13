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
    const firstRunSeedButton = screen.getByRole('button', { name: /^Cargar datos de ejemplo$/i });
    expect(firstRunSeedButton).toBeInTheDocument();
    expect(screen.getAllByRole('button', { name: /Cargar datos de ejemplo/i })).toHaveLength(1);
    expect(firstRunSeedButton).toHaveAttribute(
      'title',
      'Carga datos de ejemplo para revisar usuarios, roles y auditoría sin tocar producción.',
    );
    expect(
      screen.queryByRole('button', { name: /Cargar datos de ejemplo \(opcional\)/i }),
    ).not.toBeInTheDocument();
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

  it('shows only service dependencies that need attention as status chips', async () => {
    mockHealthFetch.mockResolvedValue({ status: 'ok', db: 'degraded' });

    renderPage();

    expect(await screen.findByText('Estado del servicio')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Atención: base de datos requiere revisión antes de cambiar permisos o seguir con otras acciones administrativas\./i,
        ),
      ).toBeInTheDocument();
      const statusChips = screen.getAllByTestId('admin-service-health-chip');
      expect(statusChips).toHaveLength(1);
      expect(statusChips[0]).toHaveTextContent('Base de datos: requiere revisión');
      expect(statusChips[0]).toHaveAttribute('title', 'Base de datos: degraded');
    });

    expect(screen.queryByText('Base de datos: degraded')).not.toBeInTheDocument();
    expect(screen.queryByText('API: ok')).not.toBeInTheDocument();
    expect(screen.queryByText('API: listo')).not.toBeInTheDocument();
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
      expect(screen.getByText('Base de datos: requiere revisión')).toBeInTheDocument();
    });

    expect(
      screen.getAllByText(
        /Primero resuelve el estado del servicio; luego se habilitarán usuarios, auditoría y datos de ejemplo\./i,
      ),
    ).toHaveLength(1);
    expect(
      screen.queryByText(/Sigue este recorrido para ubicar cada bloque sin repetir revisiones vacías\./i),
    ).not.toBeInTheDocument();
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

  it('keeps service health as the first-run blocker when users also fail to load', async () => {
    mockHealthFetch.mockResolvedValue({ status: 'ok', db: 'degraded' });
    mockListUsers.mockRejectedValue(new Error('Usuarios no disponibles'));

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Primero resuelve el estado del servicio; luego se habilitarán usuarios, auditoría y datos de ejemplo\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText('Base de datos: requiere revisión')).toBeInTheDocument();
      expect(screen.getByText('Revisar estado del servicio')).toBeInTheDocument();
    });

    expect(
      screen.queryByText(
        /Actualiza el panel para confirmar usuarios y auditoría antes de cargar datos de ejemplo\./i,
      ),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Usuarios no disponibles')).not.toBeInTheDocument();
    expect(screen.queryByText('Usuarios y roles')).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Reintentar carga inicial/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Cargar datos de ejemplo/i })).not.toBeInTheDocument();
  });

  it('keeps role editing read-only until service health is ready', async () => {
    mockHealthFetch.mockResolvedValue({ status: 'ok', db: 'degraded' });
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
          /Atención: base de datos requiere revisión antes de cambiar permisos o seguir con otras acciones administrativas\./i,
        ),
      ).toBeInTheDocument();
      expect(
        screen.getByText(
          /Resuelve el estado del servicio para habilitar la edición de roles desde esta misma vista\. Último acceso y estado aparecerán cuando aporten contexto\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.queryByText(/Vista compacta:/i)).not.toBeInTheDocument();
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
      expect(screen.getByText('Grace Hopper')).toBeInTheDocument();
      expect(screen.getByText('Admin')).toBeInTheDocument();
      expect(screen.getByText('Manager')).toBeInTheDocument();
    });

    expect(screen.queryByRole('button', { name: /Editar roles de Ada Lovelace/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Editar roles de Grace Hopper/i })).not.toBeInTheDocument();
    expect(screen.queryByText(/usa el botón de cada fila/i)).not.toBeInTheDocument();
  });

  it('keeps optional preview errors hidden while first-run service health is blocked', async () => {
    mockHealthFetch.mockResolvedValue({ status: 'ok', db: 'degraded' });
    mockConsolePreview.mockRejectedValue(new Error('Vista dinámica no disponible'));

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Primero resuelve el estado del servicio; luego se habilitarán usuarios, auditoría y datos de ejemplo\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText('Base de datos: requiere revisión')).toBeInTheDocument();
    });

    expect(
      screen.queryByText(/No se pudo cargar el panel dinámico/i),
    ).not.toBeInTheDocument();
    expect(screen.queryByText(/Vista dinámica no disponible/i)).not.toBeInTheDocument();
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

  it('keeps optional first-run modules hidden until required admin data loads cleanly', async () => {
    mockListUsers.mockRejectedValue(new Error('Usuarios no disponibles'));
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
      expect(screen.getByText('Usuarios no disponibles')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Reintentar carga inicial/i })).toBeInTheDocument();
      expect(
        screen.getByText(
          /Actualiza el panel para confirmar usuarios y auditoría antes de cargar datos de ejemplo\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver 1 módulo adicional/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos opcionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Usa este espacio para rotar credenciales compartidas/i),
    ).not.toBeInTheDocument();
  });

  it('keeps first-run guidance focused when only optional admin preview modules fail', async () => {
    mockConsolePreview.mockRejectedValue(new Error('Vista dinámica no disponible'));

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Cargar datos de ejemplo/i })).toBeInTheDocument();
    });

    expect(
      screen.queryByText(/No se pudo cargar el panel dinámico/i),
    ).not.toBeInTheDocument();
    expect(screen.queryByText(/Vista dinámica no disponible/i)).not.toBeInTheDocument();
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

  it('does not make first-run onboarding wait for optional preview modules', async () => {
    mockConsolePreview.mockImplementation(() => new Promise(() => undefined));

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /^Cargar datos de ejemplo$/i })).toBeInTheDocument();
      expect(screen.getByTestId('admin-first-run-users-status')).toHaveTextContent('Aún no hay usuarios administrables.');
      expect(
        screen.getByTestId('admin-first-run-audit-status'),
      ).toHaveTextContent('La auditoría aparecerá cuando se registre el primer cambio.');
    });

    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
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

  it('ignores blank audit fallback rows so first-run onboarding stays focused', async () => {
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'stub-audit',
        actorId: null,
        entity: '   ',
        entityId: '   ',
        action: '   ',
        diff: '   ',
        createdAt: '   ',
      },
    ]);

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByTestId('admin-first-run-users-status')).toHaveTextContent('Aún no hay usuarios administrables.');
      expect(
        screen.getByTestId('admin-first-run-audit-status'),
      ).toHaveTextContent('La auditoría aparecerá cuando se registre el primer cambio.');
      expect(screen.getByRole('button', { name: /^Cargar datos de ejemplo$/i })).toBeInTheDocument();
    });

    expect(screen.queryByText(/Primer evento de auditoría/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Acción:/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Entidad:/i)).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Entidad$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acción$/i })).not.toBeInTheDocument();
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
        screen.getByRole('button', { name: /^Cargar datos de ejemplo$/i }),
      ).toBeInTheDocument();
      expect(screen.getAllByRole('button', { name: /Cargar datos de ejemplo/i })).toHaveLength(1);
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

  it('keeps generated admin summary cards from duplicating the first-run walkthrough', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'workspace-overview',
          title: 'Workspace overview',
          body: [
            'Review system status, users, roles, and audit activity from one admin landing page.',
          ],
        },
        {
          cardId: 'fallback-admin-summary',
          title: 'Admin snapshot',
          body: [
            'Review system health, users, roles, and audit activity from one admin landing page.',
          ],
        },
        {
          cardId: 'resumen-workspace',
          title: 'Resumen del espacio de trabajo',
          body: [
            'Revisa estado del sistema, usuarios, roles y auditoría desde una sola consola administrativa.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByTestId('admin-first-run-users-status')).toHaveTextContent('Aún no hay usuarios administrables.');
      expect(
        screen.getByTestId('admin-first-run-audit-status'),
      ).toHaveTextContent('La auditoría aparecerá cuando se registre el primer cambio.');
    });

    expect(screen.queryByRole('button', { name: /Opcional: ver .*módulo/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Workspace overview')).not.toBeInTheDocument();
    expect(screen.queryByText('Admin snapshot')).not.toBeInTheDocument();
    expect(screen.queryByText('Resumen del espacio de trabajo')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review system status, users, roles, and audit activity/i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa estado del sistema, usuarios, roles y auditoría/i),
    ).not.toBeInTheDocument();
  });

  it('keeps generated admin-checklist copy from duplicating the first-run walkthrough', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-operational-checklist',
          title: 'Operational checklist',
          body: [
            'Confirm service health, users, roles, and audit before changing access.',
          ],
        },
        {
          cardId: 'fallback-revision-segura',
          title: 'Revisión segura',
          body: [
            'Confirma salud, usuarios, roles y auditoría antes de cambiar accesos.',
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
      expect(screen.getByTestId('admin-first-run-users-status')).toHaveTextContent('Aún no hay usuarios administrables.');
      expect(
        screen.getByTestId('admin-first-run-audit-status'),
      ).toHaveTextContent('La auditoría aparecerá cuando se registre el primer cambio.');
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Operational checklist')).not.toBeInTheDocument();
    expect(screen.queryByText('Revisión segura')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirm service health, users, roles, and audit before changing access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirma salud, usuarios, roles y auditoría antes de cambiar accesos\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps admin-center fallback cards from duplicating the console intro', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-admin-center',
          title: 'Admin center',
          body: ['Review admin settings, service status, users, roles, and audit from one hub.'],
        },
        {
          cardId: 'administration-hub',
          title: 'Administration hub',
          body: ['Review admin users and audit history from this overview.'],
        },
        {
          cardId: 'centro-admin',
          title: 'Centro de administración',
          body: ['Revisa estado, usuarios y auditoría desde este centro.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByTestId('admin-first-run-users-status')).toHaveTextContent('Aún no hay usuarios administrables.');
      expect(
        screen.getByTestId('admin-first-run-audit-status'),
      ).toHaveTextContent('La auditoría aparecerá cuando se registre el primer cambio.');
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Admin center')).not.toBeInTheDocument();
    expect(screen.queryByText('Administration hub')).not.toBeInTheDocument();
    expect(screen.queryByText('Centro de administración')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review admin settings, service status, users, roles, and audit/i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa estado, usuarios y auditoría desde este centro\./i),
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

  it('filters duplicate admin guidance by body copy before showing optional first-run modules', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-workspace-checklist',
          title: 'Workspace checklist',
          body: [
            'Review team access before changing administrative permissions.',
          ],
        },
        {
          cardId: 'fallback-activity-review',
          title: 'Actividad del panel',
          body: [
            'Revisa cambios administrativos recientes antes de repetir acciones.',
          ],
        },
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
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 3 módulos adicionales$/i }),
    ).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(/Usa este espacio para rotar credenciales compartidas/i),
    ).toBeInTheDocument();
    expect(screen.queryByText('Workspace checklist')).not.toBeInTheDocument();
    expect(screen.queryByText('Actividad del panel')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review team access before changing administrative permissions\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa cambios administrativos recientes antes de repetir acciones\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps access-overview fallback cards from duplicating the users workflow', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-access-overview',
          title: 'Access overview',
          body: [
            'Review team permissions and module access before editing admin roles.',
          ],
        },
        {
          cardId: 'fallback-resumen-accesos',
          title: 'Resumen de accesos',
          body: [
            'Revisa permisos del equipo y módulos disponibles antes de editar roles.',
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
    expect(screen.queryByText('Access overview')).not.toBeInTheDocument();
    expect(screen.queryByText('Resumen de accesos')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review team permissions and module access before editing admin roles\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa permisos del equipo y módulos disponibles antes de editar roles\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps user-access overview fallback cards from duplicating the users workflow', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-user-access-overview',
          title: 'User access overview',
          body: [
            'Review which users can open protected modules before editing roles.',
          ],
        },
        {
          cardId: 'fallback-resumen-acceso-usuarios',
          title: 'Resumen de acceso de usuarios',
          body: [
            'Revisa qué usuarios pueden abrir módulos protegidos antes de editar roles.',
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
    expect(screen.queryByText('User access overview')).not.toBeInTheDocument();
    expect(screen.queryByText('Resumen de acceso de usuarios')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review which users can open protected modules before editing roles\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa qué usuarios pueden abrir módulos protegidos antes de editar roles\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps team-access fallback cards from duplicating the users workflow', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-team-access',
          title: 'Team access',
          body: [
            'Review which teammates can open each protected workflow before changing roles.',
          ],
        },
        {
          cardId: 'fallback-accesos-equipo',
          title: 'Accesos del equipo',
          body: [
            'Revisa permisos del equipo antes de cambiar roles administrativos.',
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
    expect(screen.queryByText('Team access')).not.toBeInTheDocument();
    expect(screen.queryByText('Accesos del equipo')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review which teammates can open each protected workflow before changing roles\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa permisos del equipo antes de cambiar roles administrativos\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps authorization fallback cards out of first-run optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-authorization',
          title: 'Authorization',
          body: [
            'Review authorization rules before editing admin access.',
          ],
        },
        {
          cardId: 'fallback-autorizaciones',
          title: 'Autorizaciones',
          body: [
            'Revisa autorizaciones del equipo antes de editar accesos administrativos.',
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
    expect(screen.queryByText('Authorization')).not.toBeInTheDocument();
    expect(screen.queryByText('Autorizaciones')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review authorization rules before editing admin access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa autorizaciones del equipo antes de editar accesos administrativos\./i),
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

  it('keeps activity-log fallback titles from duplicating recent audit', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-activity-log',
          title: 'Activity log',
          body: ['Review administrative activity before repeating access changes.'],
        },
        {
          cardId: 'fallback-registro-actividad',
          title: 'Registro de actividad',
          body: ['Revisa cambios administrativos recientes antes de repetir acciones.'],
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
    expect(screen.queryByText('Activity log')).not.toBeInTheDocument();
    expect(screen.queryByText('Registro de actividad')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review administrative activity before repeating access changes\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa cambios administrativos recientes antes de repetir acciones\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps recent-activity fallback titles from duplicating recent audit', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-recent-activity',
          title: 'Recent activity',
          body: ['Review recent admin changes before repeating actions.'],
        },
        {
          cardId: 'fallback-recent-changes',
          title: 'Recent changes',
          body: ['Review recent workspace changes before repeating admin actions.'],
        },
        {
          cardId: 'fallback-actividad-reciente',
          title: 'Actividad reciente',
          body: ['Revisa actividad reciente del sistema antes de repetir acciones.'],
        },
        {
          cardId: 'fallback-cambios-recientes',
          title: 'Cambios recientes',
          body: ['Revisa cambios recientes del sistema antes de repetir acciones.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getByText(/La auditoría aparecerá cuando se registre el primer cambio\./i),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Recent activity')).not.toBeInTheDocument();
    expect(screen.queryByText('Recent changes')).not.toBeInTheDocument();
    expect(screen.queryByText('Actividad reciente')).not.toBeInTheDocument();
    expect(screen.queryByText('Cambios recientes')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review recent admin changes before repeating actions\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review recent workspace changes before repeating admin actions\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa actividad reciente del sistema antes de repetir acciones\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa cambios recientes del sistema antes de repetir acciones\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps event-log fallback titles from duplicating recent audit', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-activity-feed',
          title: 'Activity feed',
          body: ['Review system events before repeating admin changes.'],
        },
        {
          cardId: 'fallback-event-log',
          title: 'Event log',
          body: ['Review event history before repeating admin changes.'],
        },
        {
          cardId: 'fallback-system-log',
          title: 'System log',
          body: ['Review workspace logs before changing access.'],
        },
        {
          cardId: 'fallback-bitacora-sistema',
          title: 'Bitacora del sistema',
          body: ['Revisa eventos del sistema antes de repetir acciones administrativas.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getByText(/La auditoría aparecerá cuando se registre el primer cambio\./i),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Activity feed')).not.toBeInTheDocument();
    expect(screen.queryByText('Event log')).not.toBeInTheDocument();
    expect(screen.queryByText('System log')).not.toBeInTheDocument();
    expect(screen.queryByText('Bitacora del sistema')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review system events before repeating admin changes\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa eventos del sistema antes de repetir acciones administrativas\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps audit-trail fallback titles from duplicating recent audit', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-audit-trail',
          title: 'Audit trail',
          body: ['Review the audit trail before repeating administrative changes.'],
        },
        {
          cardId: 'fallback-historial-cambios',
          title: 'Historial de cambios',
          body: ['Revisa los cambios administrativos antes de repetir una acción.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getByText(/La auditoría aparecerá cuando se registre el primer cambio\./i),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Audit trail')).not.toBeInTheDocument();
    expect(screen.queryByText('Historial de cambios')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review the audit trail before repeating administrative changes\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa los cambios administrativos antes de repetir una acción\./i),
    ).not.toBeInTheDocument();
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

  it('keeps API and database health fallback cards from duplicating service health', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-api-health',
          title: 'API health',
          body: ['Review API latency and uptime before changing admin permissions.'],
        },
        {
          cardId: 'fallback-database-status',
          title: 'Database status',
          body: ['Confirm database readiness before loading admin demo data.'],
        },
        {
          cardId: 'fallback-estado-api',
          title: 'Estado de API',
          body: ['Revisa disponibilidad y latencia antes de ajustar accesos.'],
        },
        {
          cardId: 'fallback-api-connectivity',
          title: 'API connectivity',
          body: ['Confirm API connectivity before updating admin roles.'],
        },
        {
          cardId: 'fallback-db-connection',
          title: 'Conexión de base de datos',
          body: ['Valida la conexión de base de datos antes del recorrido inicial.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByText('Estado del servicio')).toBeInTheDocument();
      expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('API health')).not.toBeInTheDocument();
    expect(screen.queryByText('Database status')).not.toBeInTheDocument();
    expect(screen.queryByText('Estado de API')).not.toBeInTheDocument();
    expect(screen.queryByText('API connectivity')).not.toBeInTheDocument();
    expect(screen.queryByText('Conexión de base de datos')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review API latency and uptime before changing admin permissions\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirm database readiness before loading admin demo data\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa disponibilidad y latencia antes de ajustar accesos\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirm API connectivity before updating admin roles\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Valida la conexión de base de datos antes del recorrido inicial\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps bare API and database fallback cards from duplicating service health', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'api',
          title: 'API',
          body: ['Monitoriza disponibilidad antes de cambiar accesos administrativos.'],
        },
        {
          cardId: 'database',
          title: 'Database',
          body: ['Confirm storage readiness before opening admin access changes.'],
        },
        {
          cardId: 'base-de-datos',
          title: 'Base de datos',
          body: ['Confirma conexión antes de ajustar permisos administrativos.'],
        },
        {
          cardId: 'db',
          title: 'DB',
          body: ['Review database uptime before editing roles.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByText('Estado del servicio')).toBeInTheDocument();
      expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText(/^API$/i)).not.toBeInTheDocument();
    expect(screen.queryByText('Database')).not.toBeInTheDocument();
    expect(screen.queryByText('Base de datos')).not.toBeInTheDocument();
    expect(screen.queryByText(/^DB$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Monitoriza disponibilidad/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Confirm storage readiness/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Confirma conexión/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Review database uptime/i)).not.toBeInTheDocument();
  });

  it('keeps readiness fallback cards from duplicating service health', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-service-check',
          title: 'Service readiness',
          body: ['Confirm API and database readiness before updating admin roles.'],
        },
        {
          cardId: 'database-readiness',
          title: 'Database readiness',
          body: ['Check database availability before loading sample admin data.'],
        },
        {
          cardId: 'fallback-disponibilidad',
          title: 'Disponibilidad del sistema',
          body: ['Valida disponibilidad de API y base de datos antes del recorrido inicial.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByText('Estado del servicio')).toBeInTheDocument();
      expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Service readiness')).not.toBeInTheDocument();
    expect(screen.queryByText('Database readiness')).not.toBeInTheDocument();
    expect(screen.queryByText('Disponibilidad del sistema')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirm API and database readiness before updating admin roles\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Check database availability before loading sample admin data\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Valida disponibilidad de API y base de datos antes del recorrido inicial\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps backend-readiness fallback cards from duplicating service health', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-backend-readiness',
          title: 'Backend readiness',
          body: ['Confirm backend readiness before changing admin access.'],
        },
        {
          cardId: 'fallback-server-status',
          title: 'Server status',
          body: ['Review server availability before editing roles.'],
        },
        {
          cardId: 'fallback-estado-backend',
          title: 'Estado del backend',
          body: ['Valida API y base de datos antes del recorrido inicial.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByText('Estado del servicio')).toBeInTheDocument();
      expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Backend readiness')).not.toBeInTheDocument();
    expect(screen.queryByText('Server status')).not.toBeInTheDocument();
    expect(screen.queryByText('Estado del backend')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirm backend readiness before changing admin access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review server availability before editing roles\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Valida API y base de datos antes del recorrido inicial\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps health-check fallback cards from duplicating service health', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-health-checks',
          title: 'Health checks',
          body: ['Review API and database checks before changing admin access.'],
        },
        {
          cardId: 'fallback-dependency-checks',
          title: 'Dependency checks',
          body: ['Confirm service dependencies are ready before editing roles.'],
        },
        {
          cardId: 'fallback-api-checks',
          title: 'API checks',
          body: ['Review API checks before loading sample admin data.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(screen.getByText('Estado del servicio')).toBeInTheDocument();
      expect(screen.getByText('Aún no hay usuarios administrables.')).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /Opcional: ver .*módulo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Health checks')).not.toBeInTheDocument();
    expect(screen.queryByText('Dependency checks')).not.toBeInTheDocument();
    expect(screen.queryByText('API checks')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review API and database checks before changing admin access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirm service dependencies are ready before editing roles\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review API checks before loading sample admin data\./i),
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

  it('keeps area-labeled built-in admin titles out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-service-health-area',
          title: 'Area of service health',
          body: ['Confirm API and database readiness before changing admin access.'],
        },
        {
          cardId: 'fallback-users-roles-area',
          title: 'Área de usuarios y roles',
          body: ['Review assigned teammates before changing workspace access.'],
        },
        {
          cardId: 'fallback-audit-zone',
          title: 'Zona de auditoría reciente',
          body: ['Review who changed what before repeating an admin action.'],
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
    expect(screen.queryByText('Area of service health')).not.toBeInTheDocument();
    expect(screen.queryByText('Área de usuarios y roles')).not.toBeInTheDocument();
    expect(screen.queryByText('Zona de auditoría reciente')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirm API and database readiness before changing admin access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review assigned teammates before changing workspace access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review who changed what before repeating an admin action\./i),
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

  it('keeps console-labeled built-in admin titles out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-console-service-health',
          title: 'Console: Service health',
          body: ['Review uptime before changing access.'],
        },
        {
          cardId: 'fallback-consola-usuarios',
          title: 'Consola de usuarios y roles',
          body: ['Confirma usuarios administrables antes de ajustar roles.'],
        },
        {
          cardId: 'fallback-console-audit',
          title: 'Admin console audit trail',
          body: ['Review the latest admin changes before repeating an action.'],
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
    expect(screen.queryByText('Console: Service health')).not.toBeInTheDocument();
    expect(screen.queryByText('Consola de usuarios y roles')).not.toBeInTheDocument();
    expect(screen.queryByText('Admin console audit trail')).not.toBeInTheDocument();
    expect(screen.queryByText(/Review uptime before changing access\./i)).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirma usuarios administrables antes de ajustar roles\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review the latest admin changes before repeating an action\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps tab-labeled built-in admin titles out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-service-health-tab',
          title: 'Service health tab',
          body: ['Review current API readiness before editing admin access.'],
        },
        {
          cardId: 'fallback-users-roles-tab',
          title: 'Users & roles tab',
          body: ['Review assigned roles and protected workflows before changing team access.'],
        },
        {
          cardId: 'fallback-auditoria-reciente-pestana',
          title: 'Auditoría reciente pestaña',
          body: ['Review the latest administrative changes before repeating an action.'],
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
    expect(screen.queryByText('Service health tab')).not.toBeInTheDocument();
    expect(screen.queryByText('Users & roles tab')).not.toBeInTheDocument();
    expect(screen.queryByText('Auditoría reciente pestaña')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review current API readiness before editing admin access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review assigned roles and protected workflows before changing team access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review the latest administrative changes before repeating an action\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps panel-style built-in admin titles out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-service-health-panel',
          title: 'Service health panel',
          body: ['Verify API readiness before changing admin access.'],
        },
        {
          cardId: 'fallback-users-roles-tile',
          title: 'Users & roles tile',
          body: ['Review which teammates can open each protected workflow.'],
        },
        {
          cardId: 'fallback-recent-audit-dashboard',
          title: 'Recent audit dashboard',
          body: ['Check recent access changes before repeating an action.'],
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
    expect(screen.queryByText('Service health panel')).not.toBeInTheDocument();
    expect(screen.queryByText('Users & roles tile')).not.toBeInTheDocument();
    expect(screen.queryByText('Recent audit dashboard')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Verify API readiness before changing admin access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review which teammates can open each protected workflow\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Check recent access changes before repeating an action\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps table and list labeled built-in admin titles out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-users-list',
          title: 'Users list',
          body: ['Review assigned teammates before changing workspace permissions.'],
        },
        {
          cardId: 'fallback-lista-usuarios',
          title: 'Lista de usuarios',
          body: ['Confirma usuarios administrables antes de ajustar roles.'],
        },
        {
          cardId: 'fallback-service-health-grid',
          title: 'Service health grid',
          body: ['Review uptime and dependency checks before changing access.'],
        },
        {
          cardId: 'fallback-audit-table',
          title: 'Recent audit table',
          body: ['Check latest admin history before repeating access changes.'],
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
    expect(screen.queryByText('Users list')).not.toBeInTheDocument();
    expect(screen.queryByText('Lista de usuarios')).not.toBeInTheDocument();
    expect(screen.queryByText('Service health grid')).not.toBeInTheDocument();
    expect(screen.queryByText('Recent audit table')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review assigned teammates before changing workspace permissions\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Check latest admin history before repeating access changes\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps workflow-labeled built-in admin titles out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-service-health-workflow',
          title: 'Service health workflow',
          body: ['Verify API readiness before changing admin access.'],
        },
        {
          cardId: 'fallback-users-roles-flow',
          title: 'Users & roles flow',
          body: ['Review which teammates can open each protected workflow.'],
        },
        {
          cardId: 'fallback-auditoria-reciente-flujo',
          title: 'Flujo: Auditoría reciente',
          body: ['Check recent access changes before repeating an action.'],
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
    expect(screen.queryByText('Service health workflow')).not.toBeInTheDocument();
    expect(screen.queryByText('Users & roles flow')).not.toBeInTheDocument();
    expect(screen.queryByText('Flujo: Auditoría reciente')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Verify API readiness before changing admin access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review which teammates can open each protected workflow\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Check recent access changes before repeating an action\./i),
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

  it('keeps generic permission fallback titles from duplicating the built-in users workflow', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-permissions',
          title: 'Permissions',
          body: ['Review access rules before assigning admin roles.'],
        },
        {
          cardId: 'fallback-permisos',
          title: 'Permisos',
          body: ['Revisa reglas de acceso antes de asignar roles administrativos.'],
        },
        {
          cardId: 'fallback-access-permissions',
          title: 'Access permissions',
          body: ['Confirm who can open protected workflows before changing access.'],
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
    expect(screen.queryByText('Permissions')).not.toBeInTheDocument();
    expect(screen.queryByText('Permisos')).not.toBeInTheDocument();
    expect(screen.queryByText('Access permissions')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review access rules before assigning admin roles\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa reglas de acceso antes de asignar roles administrativos\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps permission-matrix fallback titles from duplicating the built-in roles workflow', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-permission-matrix',
          title: 'Permission matrix',
          body: ['Review role coverage across modules before editing admin access.'],
        },
        {
          cardId: 'fallback-matriz-permisos',
          title: 'Matriz de permisos',
          body: ['Revisa cobertura de roles por módulo antes de cambiar accesos administrativos.'],
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
    expect(screen.queryByText('Permission matrix')).not.toBeInTheDocument();
    expect(screen.queryByText('Matriz de permisos')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review role coverage across modules before editing admin access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa cobertura de roles por módulo antes de cambiar accesos administrativos\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps access-profile fallback titles from duplicating the built-in users workflow', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-perfiles-acceso',
          title: 'Perfiles de acceso',
          body: ['Revisa perfiles de acceso antes de cambiar permisos administrativos.'],
        },
        {
          cardId: 'fallback-access-profiles',
          title: 'Access profiles',
          body: ['Review access profiles before changing admin permissions.'],
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
    expect(screen.queryByText('Perfiles de acceso')).not.toBeInTheDocument();
    expect(screen.queryByText('Access profiles')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa perfiles de acceso antes de cambiar permisos administrativos\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review access profiles before changing admin permissions\./i),
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

  it('keeps account-management fallback titles from duplicating the built-in users workflow', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'account-management',
          title: 'Workspace accounts',
          body: ['Review admin account access before changing roles.'],
        },
        {
          cardId: 'fallback-cuentas-admin',
          title: 'Cuentas administrativas',
          body: ['Revisa cuentas admin antes de cambiar accesos.'],
        },
        {
          cardId: 'fallback-team-members',
          title: 'Team members',
          body: ['Review team members before assigning roles.'],
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
    expect(screen.queryByText('Workspace accounts')).not.toBeInTheDocument();
    expect(screen.queryByText('Cuentas administrativas')).not.toBeInTheDocument();
    expect(screen.queryByText('Team members')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review admin account access before changing roles\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa cuentas admin antes de cambiar accesos\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review team members before assigning roles\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps user-access fallback titles from duplicating the built-in access workflow', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-user-access',
          title: 'User access',
          body: ['Review who can open protected admin workflows before changing roles.'],
        },
        {
          cardId: 'fallback-acceso-administrativo',
          title: 'Acceso administrativo',
          body: ['Confirma permisos administrativos antes de cambiar roles.'],
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
    expect(screen.queryByText('User access')).not.toBeInTheDocument();
    expect(screen.queryByText('Acceso administrativo')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review who can open protected admin workflows before changing roles\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirma permisos administrativos antes de cambiar roles\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps access-security fallback titles from duplicating the built-in users workflow', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-access-security',
          title: 'Access security',
          body: ['Review security and team permissions before editing admin roles.'],
        },
        {
          cardId: 'fallback-seguridad-permisos',
          title: 'Seguridad y permisos',
          body: ['Revisa seguridad y permisos del equipo antes de cambiar roles administrativos.'],
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
    expect(screen.queryByText('Access security')).not.toBeInTheDocument();
    expect(screen.queryByText('Seguridad y permisos')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review security and team permissions before editing admin roles\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa seguridad y permisos del equipo antes de cambiar roles administrativos\./i),
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

  it('keeps bare role-assignment fallback titles from duplicating the users workflow', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-roles',
          title: 'Roles',
          body: ['Review assigned roles before editing team access.'],
        },
        {
          cardId: 'fallback-asignacion-roles',
          title: 'Asignación de roles',
          body: ['Confirma roles asignados antes de cambiar permisos administrativos.'],
        },
        {
          cardId: 'fallback-role-assignments',
          title: 'Role assignments',
          body: ['Review role assignments before changing admin permissions.'],
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
    expect(screen.queryByText('Roles')).not.toBeInTheDocument();
    expect(screen.queryByText('Asignación de roles')).not.toBeInTheDocument();
    expect(screen.queryByText('Role assignments')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review assigned roles before editing team access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirma roles asignados antes de cambiar permisos administrativos\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review role assignments before changing admin permissions\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps access-review fallback titles from duplicating the users workflow', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-access-review',
          title: 'Access review',
          body: ['Review team roles before changing permissions.'],
        },
        {
          cardId: 'fallback-permissions-review',
          title: 'Permissions review',
          body: ['Review current role coverage before editing team access.'],
        },
        {
          cardId: 'fallback-revision-accesos',
          title: 'Revisión de accesos',
          body: ['Revisa roles del equipo antes de cambiar permisos administrativos.'],
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
    expect(screen.queryByText('Access review')).not.toBeInTheDocument();
    expect(screen.queryByText('Permissions review')).not.toBeInTheDocument();
    expect(screen.queryByText('Revisión de accesos')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review team roles before changing permissions\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review current role coverage before editing team access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa roles del equipo antes de cambiar permisos administrativos\./i),
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

  it('keeps admin-labeled built-in fallback titles out of optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-admin-service-health',
          title: 'Admin service health',
          body: ['Review uptime before changing permissions.'],
        },
        {
          cardId: 'fallback-administracion-usuarios-roles',
          title: 'Administración: usuarios y roles',
          body: ['Revisa accesos del equipo antes de ajustar permisos.'],
        },
        {
          cardId: 'fallback-admin-recent-audit',
          title: 'Admin recent audit',
          body: ['Review admin events before repeating a change.'],
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
    expect(screen.queryByText('Admin service health')).not.toBeInTheDocument();
    expect(screen.queryByText('Administración: usuarios y roles')).not.toBeInTheDocument();
    expect(screen.queryByText('Admin recent audit')).not.toBeInTheDocument();
    expect(screen.queryByText(/Review uptime before changing permissions\./i)).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa accesos del equipo antes de ajustar permisos\./i),
    ).not.toBeInTheDocument();
    expect(screen.queryByText(/Review admin events before repeating a change\./i)).not.toBeInTheDocument();
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

  it('keeps admin-home fallback cards from duplicating the first-run landing page', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-admin-home',
          title: 'Admin home',
          body: ['Review health, access, and audit follow-up before changing settings.'],
        },
        {
          cardId: 'fallback-inicio-administracion',
          title: 'Inicio de administración',
          body: ['Consulta salud, accesos y auditoría antes de continuar.'],
        },
        {
          cardId: 'admin-landing',
          title: 'Home',
          body: ['Review the admin landing page before changing access.'],
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
    expect(screen.queryByText('Admin home')).not.toBeInTheDocument();
    expect(screen.queryByText('Inicio de administración')).not.toBeInTheDocument();
    expect(screen.queryByText('Home')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review health, access, and audit follow-up/i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Consulta salud, accesos y auditoría antes de continuar\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps dashboard-labeled admin landing fallbacks out of first-run optional modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-panel-administrativo',
          title: 'Panel administrativo',
          body: ['Vista principal para monitorear configuración, acceso y auditoría.'],
        },
        {
          cardId: 'fallback-administrative-dashboard',
          title: 'Administrative dashboard',
          body: ['Review the administrative workspace before changing access.'],
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
    expect(screen.queryByText('Panel administrativo')).not.toBeInTheDocument();
    expect(screen.queryByText('Administrative dashboard')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Vista principal para monitorear configuración, acceso y auditoría\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review the administrative workspace before changing access\./i),
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
        {
          cardId: 'fallback-admin-onboarding',
          title: 'Admin onboarding',
          body: ['Confirm the admin onboarding checklist before changing access.'],
        },
        {
          cardId: 'fallback-bienvenida-admin',
          title: 'Bienvenida administrativa',
          body: ['Revisa la bienvenida administrativa antes de cargar datos de ejemplo.'],
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
    expect(screen.queryByText('Admin onboarding')).not.toBeInTheDocument();
    expect(screen.queryByText('Bienvenida administrativa')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review service health, users, roles, and audit activity/i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa salud, usuarios y auditoría antes de cambiar permisos\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Confirm the admin onboarding checklist before changing access\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa la bienvenida administrativa antes de cargar datos de ejemplo\./i),
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
        {
          cardId: 'fallback-sample-fixtures',
          title: 'Demo fixtures',
          body: ['Generate sample users, roles, and audit events for review.'],
        },
        {
          cardId: 'fallback-datos-prueba',
          title: 'Datos de prueba',
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
    expect(screen.queryByText('Demo fixtures')).not.toBeInTheDocument();
    expect(screen.queryByText('Datos de prueba')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Generate sample users, roles, and audit events for review\./i),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Prepara usuarios y auditoría de demostración para validar el panel\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps test-fixture fallback cards from duplicating the first-run seed action', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'test-data',
          title: 'Sandbox records',
          body: ['Load temporary admin records for sandbox review.'],
        },
        {
          cardId: 'demo-fixtures',
          title: 'Demo fixtures',
          body: ['Prepare a disposable workspace for onboarding checks.'],
        },
        {
          cardId: 'datos-prueba',
          title: 'Datos de prueba',
          body: ['Prepara registros temporales para revisar la consola.'],
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
    expect(screen.queryByText('Sandbox records')).not.toBeInTheDocument();
    expect(screen.queryByText('Demo fixtures')).not.toBeInTheDocument();
    expect(screen.queryByText('Datos de prueba')).not.toBeInTheDocument();
    expect(screen.queryByText(/Load temporary admin records/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Prepare a disposable workspace/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Prepara registros temporales/i)).not.toBeInTheDocument();
  });

  it('keeps sandbox-record fallback copy hidden even when generated with custom ids', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-sandbox-records',
          title: 'Sandbox records',
          body: ['Load temporary admin records for sandbox review.'],
        },
        {
          cardId: 'fallback-disposable-workspace',
          title: 'Disposable workspace',
          body: ['Prepare a disposable workspace for onboarding checks.'],
        },
        {
          cardId: 'fallback-registros-temporales',
          title: 'Registros temporales',
          body: ['Prepara registros temporales para revisar la consola.'],
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
    expect(screen.queryByText('Sandbox records')).not.toBeInTheDocument();
    expect(screen.queryByText('Disposable workspace')).not.toBeInTheDocument();
    expect(screen.queryByText('Registros temporales')).not.toBeInTheDocument();
    expect(screen.queryByText(/Load temporary admin records/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Prepare a disposable workspace/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Prepara registros temporales/i)).not.toBeInTheDocument();
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

  it('keeps the first-run action ahead of optional fallback modules', async () => {
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

    const seedButton = await screen.findByRole('button', { name: /^Cargar datos de ejemplo$/i });
    const optionalModulesButton = await screen.findByRole(
      'button',
      { name: /^Opcional: ver 1 módulo adicional$/i },
    );

    expectToAppearBefore(seedButton, optionalModulesButton);
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
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

  it('removes repeated first-run module titles from fallback card body copy', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens',
          title: 'Tokens de servicio',
          body: [
            'Tokens de servicio: Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
            'Tokens de servicio',
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

    await user.click(
      await within(firstRunAlert).findByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await within(firstRunAlert).findAllByText('Tokens de servicio')).toHaveLength(1);
    expect(
      within(firstRunAlert).getByText(
        /^Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\.$/i,
      ),
    ).toBeInTheDocument();
    expect(
      within(firstRunAlert).queryByText(/Tokens de servicio: Usa este espacio/i),
    ).not.toBeInTheDocument();
  });

  it('strips list markers from fallback module copy before showing optional details', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens',
          title: 'Tokens de servicio',
          body: [
            '- Tokens de servicio: Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
            '2. Programa una rotación semanal sin salir de esta consola.',
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

    await user.click(
      await within(firstRunAlert).findByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await within(firstRunAlert).findAllByText('Tokens de servicio')).toHaveLength(1);
    expect(
      within(firstRunAlert).getByText(
        /^Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\.$/i,
      ),
    ).toBeInTheDocument();
    expect(
      within(firstRunAlert).getByText(/^Programa una rotación semanal sin salir de esta consola\.$/i),
    ).toBeInTheDocument();
    expect(within(firstRunAlert).queryByText(/^- Tokens de servicio:/i)).not.toBeInTheDocument();
    expect(within(firstRunAlert).queryByText(/^2\. Programa/i)).not.toBeInTheDocument();
  });

  it('strips markdown formatting and still filters built-in fallback sections', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-markdown-health',
          title: '**Service health**',
          body: [
            'Review API readiness before changing admin permissions.',
          ],
        },
        {
          cardId: 'service-tokens',
          title: '**Tokens de servicio**',
          body: [
            '- **Tokens de servicio:** Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
            '2. `Programa una rotación semanal sin salir de esta consola.`',
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
        within(firstRunAlert).getByRole(
          'button',
          { name: /^Opcional: ver 1 módulo adicional$/i },
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('button', { name: /^Opcional: ver 2 módulos adicionales$/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Service health')).not.toBeInTheDocument();
    expect(screen.queryByText(/\*\*Service health\*\*/i)).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Review API readiness before changing admin permissions\./i),
    ).not.toBeInTheDocument();

    await user.click(
      within(firstRunAlert).getByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await within(firstRunAlert).findAllByText('Tokens de servicio')).toHaveLength(1);
    expect(
      within(firstRunAlert).getByText(
        /^Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\.$/i,
      ),
    ).toBeInTheDocument();
    expect(
      within(firstRunAlert).getByText(/^Programa una rotación semanal sin salir de esta consola\.$/i),
    ).toBeInTheDocument();
    expect(within(firstRunAlert).queryByText(/\*\*Tokens de servicio/i)).not.toBeInTheDocument();
    expect(within(firstRunAlert).queryByText(/`Programa una rotación/i)).not.toBeInTheDocument();
    expect(within(firstRunAlert).queryByText(/^2\. Programa/i)).not.toBeInTheDocument();
  });

  it('strips italic markdown from fallback modules before showing optional details', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens',
          title: '*Tokens de servicio*',
          body: [
            '- *Tokens de servicio:* Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
            '2. _Programa una rotación semanal sin salir de esta consola._',
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

    await user.click(
      await within(firstRunAlert).findByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await within(firstRunAlert).findAllByText('Tokens de servicio')).toHaveLength(1);
    expect(
      within(firstRunAlert).getByText(
        /^Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\.$/i,
      ),
    ).toBeInTheDocument();
    expect(
      within(firstRunAlert).getByText(/^Programa una rotación semanal sin salir de esta consola\.$/i),
    ).toBeInTheDocument();
    expect(within(firstRunAlert).queryByText(/\*Tokens de servicio/i)).not.toBeInTheDocument();
    expect(within(firstRunAlert).queryByText(/_Programa una rotación/i)).not.toBeInTheDocument();
  });

  it('strips markdown links from fallback modules before showing first-run details', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens',
          title: '[Tokens de servicio](/configuracion/tokens)',
          body: [
            '[Tokens de servicio](/configuracion/tokens): Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
            'Abre [rotación semanal](/configuracion/tokens/rotacion) desde esta consola.',
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

    await user.click(
      await within(firstRunAlert).findByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await within(firstRunAlert).findAllByText('Tokens de servicio')).toHaveLength(1);
    expect(
      within(firstRunAlert).getByText(
        /^Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\.$/i,
      ),
    ).toBeInTheDocument();
    expect(
      within(firstRunAlert).getByText(/^Abre rotación semanal desde esta consola\.$/i),
    ).toBeInTheDocument();
    expect(within(firstRunAlert).queryByText(/\[Tokens de servicio\]\(/i)).not.toBeInTheDocument();
    expect(within(firstRunAlert).queryByText(/\/configuracion\/tokens/i)).not.toBeInTheDocument();
  });

  it('strips generated fallback prefixes from unique optional module titles', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'fallback-service-tokens',
          title: 'Fallback: Tokens de servicio',
          body: [
            'Fallback: Tokens de servicio: Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
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

    await user.click(
      await within(firstRunAlert).findByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await within(firstRunAlert).findAllByText('Tokens de servicio')).toHaveLength(1);
    expect(
      within(firstRunAlert).getByText(
        /^Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\.$/i,
      ),
    ).toBeInTheDocument();
    expect(within(firstRunAlert).queryByText(/Fallback:/i)).not.toBeInTheDocument();
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

  it('merges fallback cards when one optional module only extends another', async () => {
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
            'Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.',
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
    expect(screen.getByText(/Programa una rotación semanal sin salir de esta consola\./i)).toBeInTheDocument();
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

  it('merges wrapper-labeled custom fallback modules into one optional workflow', async () => {
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
          cardId: 'service-tokens-panel',
          title: 'Tokens de servicio panel',
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
    expect(screen.queryByText('Tokens de servicio panel')).not.toBeInTheDocument();

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await screen.findByRole('button', { name: /Ocultar módulo opcional/i })).toBeInTheDocument();
    expect(screen.getAllByText('Tokens de servicio')).toHaveLength(1);
    expect(screen.queryByText('Tokens de servicio panel')).not.toBeInTheDocument();
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

  it('ignores preview cards with placeholder titles before first-run admins open optional modules', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens-placeholder-title',
          title: 'Placeholder',
          body: [
            'Revisa llaves internas antes de rotar credenciales compartidas.',
          ],
        },
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
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 2 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Placeholder')).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(/Usa este espacio para rotar credenciales compartidas/i),
    ).toBeInTheDocument();
    expect(screen.queryByText('Placeholder')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa llaves internas antes de rotar credenciales compartidas/i),
    ).not.toBeInTheDocument();
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

  it('ignores not-ready preview cards so first-run only shows actionable admin modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'api-tokens-not-ready',
          title: 'Tokens API',
          body: ['Esta vista aún no está lista todavía.'],
        },
        {
          cardId: 'service-keys-not-ready',
          title: 'Service keys',
          body: ['This workflow is not ready yet.'],
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

    expect(screen.queryByRole('button', { name: /Tokens API|Service keys/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens API')).not.toBeInTheDocument();
    expect(screen.queryByText('Service keys')).not.toBeInTheDocument();
    expect(screen.queryByText(/Esta vista aún no está lista todavía\./i)).not.toBeInTheDocument();
    expect(screen.queryByText(/This workflow is not ready yet\./i)).not.toBeInTheDocument();
  });

  it('ignores section-not-ready fallback cards while keeping real first-run modules', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'external-credentials-section-not-ready',
          title: 'Credenciales externas',
          body: ['This section is not ready yet.'],
        },
        {
          cardId: 'admin-automations-area-not-ready',
          title: 'Automatizaciones admin',
          body: ['Esta área no está lista para este workspace.'],
        },
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
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 3 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Credenciales externas')).not.toBeInTheDocument();
    expect(screen.queryByText('Automatizaciones admin')).not.toBeInTheDocument();
    expect(screen.queryByText(/This section is not ready/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Esta área no está lista/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).toBeInTheDocument();
    expect(screen.queryByText('Credenciales externas')).not.toBeInTheDocument();
    expect(screen.queryByText('Automatizaciones admin')).not.toBeInTheDocument();
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
        {
          cardId: 'empty-report',
          title: 'Reporte operativo',
          body: ['No data to display.'],
        },
        {
          cardId: 'empty-dashboard',
          title: 'Tablero operativo',
          body: ['No hay datos para mostrar.'],
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
        name: /Integraciones|Tokens de servicio|Reporte operativo|Tablero operativo/i,
      }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
    expect(screen.queryByText('Reporte operativo')).not.toBeInTheDocument();
    expect(screen.queryByText('Tablero operativo')).not.toBeInTheDocument();
    expect(screen.queryByText(/No data available/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Sin datos disponibles/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No data to display/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No hay datos para mostrar/i)).not.toBeInTheDocument();
  });

  it('ignores item-empty fallback cards while keeping real first-run modules', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'integrations-items-empty',
          title: 'Integraciones',
          body: ['No items to display.'],
        },
        {
          cardId: 'service-keys-empty',
          title: 'Service keys',
          body: ['No items to show yet.'],
        },
        {
          cardId: 'credenciales-sin-elementos',
          title: 'Credenciales externas',
          body: ['No hay elementos para mostrar.'],
        },
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
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 4 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Service keys')).not.toBeInTheDocument();
    expect(screen.queryByText('Credenciales externas')).not.toBeInTheDocument();
    expect(screen.queryByText(/No items to display/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No items to show/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No hay elementos para mostrar/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(/Usa este espacio para rotar credenciales compartidas/i),
    ).toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Service keys')).not.toBeInTheDocument();
    expect(screen.queryByText('Credenciales externas')).not.toBeInTheDocument();
  });

  it('ignores terse no-data fallback cards without hiding actionable setup copy', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'integrations-empty',
          title: 'Integraciones',
          body: ['Sin datos.'],
        },
        {
          cardId: 'ops-empty',
          title: 'Reporte operativo',
          body: ['No data:'],
        },
        {
          cardId: 'team-contact-setup',
          title: 'Datos de contacto del equipo',
          body: [
            'Sin datos de contacto cargados. Invita al equipo para completar el perfil administrativo.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Reporte operativo')).not.toBeInTheDocument();
    expect(screen.queryByText(/^Sin datos\.$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^No data:$/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Datos de contacto del equipo')).toBeInTheDocument();
    expect(
      screen.getByText(
        /Sin datos de contacto cargados\. Invita al equipo para completar el perfil administrativo\./i,
      ),
    ).toBeInTheDocument();
  });

  it('ignores terse user and audit empty-state cards without hiding actionable onboarding copy', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'admin-team-empty',
          title: 'Equipo administrativo',
          body: ['No users yet.'],
        },
        {
          cardId: 'audit-history-empty',
          title: 'Historial operativo',
          body: ['No audit events found.'],
        },
        {
          cardId: 'usuarios-empty',
          title: 'Invitados administrativos',
          body: ['Sin usuarios.'],
        },
        {
          cardId: 'admin-users-empty',
          title: 'Operadores administrativos',
          body: ['No admin users found.'],
        },
        {
          cardId: 'usuarios-administrables-empty',
          title: 'Usuarios administrables',
          body: ['Aún no hay usuarios administrables.'],
        },
        {
          cardId: 'auditoria-empty',
          title: 'Bitácora operativa',
          body: ['Aún no hay eventos de auditoría.'],
        },
        {
          cardId: 'admin-audit-empty',
          title: 'Historial administrativo',
          body: ['No administrative audit events yet.'],
        },
        {
          cardId: 'secure-invites',
          title: 'Invitación segura',
          body: [
            'Sin usuarios asignados a este flujo. Invita al responsable antes de activar accesos compartidos.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
      expect(screen.getByTestId('admin-first-run-users-status')).toHaveTextContent('Aún no hay usuarios administrables.');
      expect(
        screen.getByTestId('admin-first-run-audit-status'),
      ).toHaveTextContent('La auditoría aparecerá cuando se registre el primer cambio.');
    });

    expect(screen.queryByText('Equipo administrativo')).not.toBeInTheDocument();
    expect(screen.queryByText('Historial operativo')).not.toBeInTheDocument();
    expect(screen.queryByText('Invitados administrativos')).not.toBeInTheDocument();
    expect(screen.queryByText('Operadores administrativos')).not.toBeInTheDocument();
    expect(screen.queryByText('Usuarios administrables')).not.toBeInTheDocument();
    expect(screen.queryByText('Bitácora operativa')).not.toBeInTheDocument();
    expect(screen.queryByText('Historial administrativo')).not.toBeInTheDocument();
    expect(screen.queryByText(/^No users yet\.$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^No audit events found\.$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^Sin usuarios\.$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^No admin users found\.$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^Aún no hay usuarios administrables\.$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^Aún no hay eventos de auditoría\.$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^No administrative audit events yet\.$/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Invitación segura')).toBeInTheDocument();
    expect(
      screen.getByText(
        /Sin usuarios asignados a este flujo\. Invita al responsable antes de activar accesos compartidos\./i,
      ),
    ).toBeInTheDocument();
  });

  it('ignores registered-user and registered-audit empty cards without hiding invite setup copy', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'registered-users-empty',
          title: 'Usuarios registrados',
          body: ['No registered users yet.'],
        },
        {
          cardId: 'usuarios-registrados-empty',
          title: 'Registro de usuarios',
          body: ['No hay usuarios registrados.'],
        },
        {
          cardId: 'audit-registered-empty',
          title: 'Eventos registrados',
          body: ['No registered audit events yet.'],
        },
        {
          cardId: 'eventos-registrados-empty',
          title: 'Bitácora registrada',
          body: ['Aún no hay eventos registrados.'],
        },
        {
          cardId: 'invite-setup',
          title: 'Invitación segura',
          body: [
            'No hay usuarios registrados en este flujo. Invita al responsable antes de activar accesos compartidos.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
      expect(screen.getByTestId('admin-first-run-users-status')).toHaveTextContent('Aún no hay usuarios administrables.');
      expect(
        screen.getByTestId('admin-first-run-audit-status'),
      ).toHaveTextContent('La auditoría aparecerá cuando se registre el primer cambio.');
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 5 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Usuarios registrados')).not.toBeInTheDocument();
    expect(screen.queryByText('Registro de usuarios')).not.toBeInTheDocument();
    expect(screen.queryByText('Eventos registrados')).not.toBeInTheDocument();
    expect(screen.queryByText('Bitácora registrada')).not.toBeInTheDocument();
    expect(screen.queryByText(/^No registered users yet\.$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^No hay usuarios registrados\.$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^No registered audit events yet\.$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^Aún no hay eventos registrados\.$/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Invitación segura')).toBeInTheDocument();
    expect(
      screen.getByText(
        /No hay usuarios registrados en este flujo\. Invita al responsable antes de activar accesos compartidos\./i,
      ),
    ).toBeInTheDocument();
    expect(screen.queryByText('Usuarios registrados')).not.toBeInTheDocument();
    expect(screen.queryByText('Eventos registrados')).not.toBeInTheDocument();
  });

  it('ignores terse activity-empty fallback cards without hiding actionable setup copy', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'audit-activity-empty',
          title: 'Actividad operacional',
          body: ['No activity yet.'],
        },
        {
          cardId: 'recent-activity-empty',
          title: 'Bitácora técnica',
          body: ['Sin actividad reciente.'],
        },
        {
          cardId: 'recent-changes-empty',
          title: 'Revisión operativa',
          body: ['No recent changes yet.'],
        },
        {
          cardId: 'cambios-recientes-empty',
          title: 'Historial técnico',
          body: ['Sin cambios recientes.'],
        },
        {
          cardId: 'activity-setup',
          title: 'Seguimiento de actividad',
          body: [
            'Sin actividad reciente registrada. Programa una revisión semanal para confirmar accesos críticos.',
          ],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(screen.queryByText('Actividad operacional')).not.toBeInTheDocument();
    expect(screen.queryByText('Bitácora técnica')).not.toBeInTheDocument();
    expect(screen.queryByText('Revisión operativa')).not.toBeInTheDocument();
    expect(screen.queryByText('Historial técnico')).not.toBeInTheDocument();
    expect(screen.queryByText(/^No activity yet\.$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^Sin actividad reciente\.$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^No recent changes yet\.$/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^Sin cambios recientes\.$/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Seguimiento de actividad')).toBeInTheDocument();
    expect(
      screen.getByText(
        /Sin actividad reciente registrada\. Programa una revisión semanal para confirmar accesos críticos\./i,
      ),
    ).toBeInTheDocument();
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
        {
          cardId: 'ops-records-empty',
          title: 'Reporte operativo',
          body: ['No records to show.'],
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
      screen.queryByRole('button', { name: /Integraciones|Service tokens|Reporte operativo/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Service tokens')).not.toBeInTheDocument();
    expect(screen.queryByText('Reporte operativo')).not.toBeInTheDocument();
    expect(screen.queryByText(/No hay registros/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No items found/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No records to show/i)).not.toBeInTheDocument();
  });

  it('ignores grid row and entry fallback cards so first-run users do not open empty table modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'admin-row-empty',
          title: 'Reporte operativo',
          body: ['No rows to display.'],
        },
        {
          cardId: 'admin-entry-empty',
          title: 'Bitácora técnica',
          body: ['Sin entradas todavía.'],
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
      screen.queryByRole('button', { name: /Reporte operativo|Bitácora técnica/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Reporte operativo')).not.toBeInTheDocument();
    expect(screen.queryByText('Bitácora técnica')).not.toBeInTheDocument();
    expect(screen.queryByText(/No rows to display/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Sin entradas/i)).not.toBeInTheDocument();
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
        {
          cardId: 'admin-information-empty',
          title: 'Reporte operativo',
          body: ['No information to display.'],
        },
        {
          cardId: 'admin-informacion-empty',
          title: 'Bitácora técnica',
          body: ['No hay información para mostrar.'],
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
        name: /Integraciones|Service tokens|Reporte operativo|Bitácora técnica/i,
      }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Service tokens')).not.toBeInTheDocument();
    expect(screen.queryByText('Reporte operativo')).not.toBeInTheDocument();
    expect(screen.queryByText('Bitácora técnica')).not.toBeInTheDocument();
    expect(screen.queryByText(/Sin información disponible/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No information to display/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No hay información para mostrar/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No elements available/i)).not.toBeInTheDocument();
  });

  it('ignores no-content fallback cards so first-run users do not open dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'integrations-content-empty',
          title: 'Integraciones',
          body: ['Sin contenido por ahora.'],
        },
        {
          cardId: 'service-tokens-content-empty',
          title: 'Service tokens',
          body: ['No content available yet.'],
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
    expect(screen.queryByText(/Sin contenido/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No content available/i)).not.toBeInTheDocument();
  });

  it('ignores select-to-view-detail fallback cards while keeping actionable setup copy', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'integrations-select-placeholder',
          title: 'Integraciones',
          body: ['Select a record to view details.'],
        },
        {
          cardId: 'audit-row-select-placeholder',
          title: 'Detalle operativo',
          body: ['Selecciona una fila para ver detalles.'],
        },
        {
          cardId: 'credential-rotation',
          title: 'Rotación de credenciales',
          body: ['Programa una revisión semanal de credenciales compartidas.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 3 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Detalle operativo')).not.toBeInTheDocument();
    expect(screen.queryByText(/Select a record to view details/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Selecciona una fila para ver detalles/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Rotación de credenciales')).toBeInTheDocument();
    expect(
      screen.getByText(/Programa una revisión semanal de credenciales compartidas\./i),
    ).toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Detalle operativo')).not.toBeInTheDocument();
  });

  it('ignores nothing-here fallback cards so first-run users do not open dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'integrations-nothing-here',
          title: 'Integraciones',
          body: ['Nothing here yet.'],
        },
        {
          cardId: 'service-tokens-nada',
          title: 'Tokens de servicio',
          body: ['No hay nada que mostrar todavía.'],
        },
        {
          cardId: 'api-access-empty',
          title: 'Acceso API',
          body: ['Nada que mostrar por ahora.'],
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

    expect(screen.queryByRole('button', { name: /Integraciones|Tokens de servicio|Acceso API/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
    expect(screen.queryByText('Acceso API')).not.toBeInTheDocument();
    expect(screen.queryByText(/Nothing here/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No hay nada que mostrar/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Nada que mostrar/i)).not.toBeInTheDocument();
  });

  it('ignores not-applicable fallback cards so first-run users do not open dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'integrations-not-applicable',
          title: 'Integraciones',
          body: ['N/A'],
        },
        {
          cardId: 'service-tokens-not-applicable',
          title: 'Service tokens',
          body: ['Not applicable for this workspace.'],
        },
        {
          cardId: 'api-access-no-aplica',
          title: 'Acceso API',
          body: ['No aplica por ahora.'],
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

    expect(screen.queryByRole('button', { name: /Integraciones|Service tokens|Acceso API/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Service tokens')).not.toBeInTheDocument();
    expect(screen.queryByText('Acceso API')).not.toBeInTheDocument();
    expect(screen.queryByText(/N\/A/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Not applicable/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No aplica/i)).not.toBeInTheDocument();
  });

  it('ignores terse workflow-empty fallback cards while keeping real optional modules', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'automations-workflows-empty',
          title: 'Automatizaciones',
          body: ['No workflows configured yet.'],
        },
        {
          cardId: 'flujos-internos-empty',
          title: 'Flujos internos',
          body: ['No hay flujos disponibles todavía.'],
        },
        {
          cardId: 'service-tokens',
          title: 'Tokens de servicio',
          body: ['Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 3 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Automatizaciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Flujos internos')).not.toBeInTheDocument();
    expect(screen.queryByText(/No workflows configured/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No hay flujos disponibles/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(/Usa este espacio para rotar credenciales compartidas/i),
    ).toBeInTheDocument();
    expect(screen.queryByText('Automatizaciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Flujos internos')).not.toBeInTheDocument();
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

  it('ignores generic unavailable fallback cards so first-run users do not open dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'external-credentials-unavailable',
          title: 'Credenciales externas',
          body: ['Unavailable in this workspace.'],
        },
        {
          cardId: 'service-integrations-unavailable',
          title: 'Integraciones técnicas',
          body: ['Esta sección no está disponible en este entorno.'],
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
    expect(screen.queryByText('Credenciales externas')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones técnicas')).not.toBeInTheDocument();
    expect(screen.queryByText(/Unavailable in this workspace/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/no está disponible/i)).not.toBeInTheDocument();
  });

  it('ignores not-yet-available fallback cards while keeping real first-run modules', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'service-tokens-not-yet',
          title: 'Service tokens',
          body: ['Not yet available in this workspace.'],
        },
        {
          cardId: 'technical-integrations-not-ready',
          title: 'Integraciones técnicas',
          body: ['Not ready for this workspace.'],
        },
        {
          cardId: 'not-yet-available-title',
          title: 'Not yet available',
          body: ['Review future connector access here.'],
        },
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
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 4 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Service tokens')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones técnicas')).not.toBeInTheDocument();
    expect(screen.queryByText('Not yet available')).not.toBeInTheDocument();
    expect(screen.queryByText(/Not yet available in this workspace/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Not ready for this workspace/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(/Usa este espacio para rotar credenciales compartidas/i),
    ).toBeInTheDocument();
    expect(screen.queryByText('Service tokens')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones técnicas')).not.toBeInTheDocument();
    expect(screen.queryByText('Not yet available')).not.toBeInTheDocument();
  });

  it('ignores support-hand-off fallback cards so first-run users do not open dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'external-credentials-contact-support',
          title: 'Credenciales externas',
          body: ['Contact support to enable this module.'],
        },
        {
          cardId: 'technical-integrations-admin-contact',
          title: 'Integraciones técnicas',
          body: ['Ask your administrator to enable this workflow before using it.'],
        },
        {
          cardId: 'service-keys-contacto-admin',
          title: 'Llaves de servicio',
          body: ['Contacta al administrador para habilitar este módulo.'],
        },
        {
          cardId: 'api-access-pide-admin',
          title: 'Acceso API',
          body: ['Pide al administrador que habilite este flujo.'],
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
        name: /Credenciales externas|Integraciones técnicas|Llaves de servicio|Acceso API/i,
      }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Credenciales externas')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones técnicas')).not.toBeInTheDocument();
    expect(screen.queryByText('Llaves de servicio')).not.toBeInTheDocument();
    expect(screen.queryByText('Acceso API')).not.toBeInTheDocument();
    expect(screen.queryByText(/Contact support to enable/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Ask your administrator to enable/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Contacta al administrador/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Pide al administrador/i)).not.toBeInTheDocument();
  });

  it('ignores setup-required fallback cards so first-run users do not open dead-end modules', async () => {
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'external-credentials-setup',
          title: 'Credenciales externas',
          body: ['Setup required before this module can be opened.'],
        },
        {
          cardId: 'technical-integrations-unconfigured',
          title: 'Integraciones técnicas',
          body: ['Aún no está configurado en este entorno.'],
        },
        {
          cardId: 'service-tokens-unconfigured',
          title: 'Service tokens',
          body: ['Not configured yet.'],
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
      screen.queryByRole('button', { name: /Credenciales externas|Integraciones técnicas|Service tokens/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Módulos adicionales')).not.toBeInTheDocument();
    expect(screen.queryByText('Credenciales externas')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones técnicas')).not.toBeInTheDocument();
    expect(screen.queryByText('Service tokens')).not.toBeInTheDocument();
    expect(screen.queryByText(/Setup required/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/no está configurado/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Not configured yet/i)).not.toBeInTheDocument();
  });

  it('ignores configuration-required fallback cards so first-run users only open actionable modules', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'integrations-configuration-required',
          title: 'Integrations setup',
          body: ['Configuration required before this module can be opened.'],
        },
        {
          cardId: 'tokens-configuracion-requerida',
          title: 'Configuración requerida',
          body: ['Requiere configuración antes de mostrar credenciales.'],
        },
        {
          cardId: 'service-tokens',
          title: 'Tokens de servicio',
          body: ['Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios.'],
        },
      ],
    });

    renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primeros pasos')).toBeInTheDocument();
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 3 módulos adicionales$/i }),
    ).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(/Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i),
    ).toBeInTheDocument();
    expect(screen.queryByText('Integrations setup')).not.toBeInTheDocument();
    expect(screen.queryByText('Configuración requerida')).not.toBeInTheDocument();
    expect(screen.queryByText(/Configuration required before this module can be opened\./i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Requiere configuración antes de mostrar credenciales\./i)).not.toBeInTheDocument();
  });

  it('ignores disabled and not-enabled fallback cards so first-run users only open actionable modules', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'technical-integrations-disabled',
          title: 'Integraciones técnicas',
          body: ['This feature is disabled for this workspace.'],
        },
        {
          cardId: 'service-keys-not-enabled',
          title: 'Llaves de servicio',
          body: ['Esta funcionalidad no está habilitada en este entorno.'],
        },
        {
          cardId: 'feature-flag-disabled',
          title: 'Feature disabled',
          body: ['Contact support to enable this feature flag.'],
        },
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
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 4 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones técnicas')).not.toBeInTheDocument();
    expect(screen.queryByText('Llaves de servicio')).not.toBeInTheDocument();
    expect(screen.queryByText('Feature disabled')).not.toBeInTheDocument();
    expect(screen.queryByText(/This feature is disabled/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/no está habilitada/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Contact support to enable/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(/Usa este espacio para rotar credenciales compartidas/i),
    ).toBeInTheDocument();
    expect(screen.queryByText('Integraciones técnicas')).not.toBeInTheDocument();
    expect(screen.queryByText('Llaves de servicio')).not.toBeInTheDocument();
    expect(screen.queryByText('Feature disabled')).not.toBeInTheDocument();
  });

  it('ignores read-only and no-action fallback cards while keeping real first-run modules', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'ops-preview-only',
          title: 'Vista operativa',
          body: ['Read-only preview; no actions available yet.'],
        },
        {
          cardId: 'integraciones-solo-lectura',
          title: 'Integraciones',
          body: ['Vista de solo lectura; sin acciones disponibles.'],
        },
        {
          cardId: 'service-tokens-reference',
          title: 'Referencia de tokens',
          body: ['Reference only.'],
        },
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
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 4 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Vista operativa')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Referencia de tokens')).not.toBeInTheDocument();
    expect(screen.queryByText(/Read-only preview/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/sin acciones disponibles/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Reference only/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(/Usa este espacio para rotar credenciales compartidas/i),
    ).toBeInTheDocument();
    expect(screen.queryByText('Vista operativa')).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Referencia de tokens')).not.toBeInTheDocument();
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

  it('ignores navigation-only fallback cards while keeping real first-run modules', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'admin-navigation-shortcuts',
          title: 'Atajos de navegación',
          body: ['Usa el menú lateral para abrir Estado del servicio, Usuarios y Auditoría.'],
        },
        {
          cardId: 'admin-sidebar-navigation',
          title: 'Navigation shortcuts',
          body: ['Use the side navigation to open service health, users, and audit.'],
        },
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
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 3 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Atajos de navegación')).not.toBeInTheDocument();
    expect(screen.queryByText('Navigation shortcuts')).not.toBeInTheDocument();
    expect(screen.queryByText(/Usa el menú lateral/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Use the side navigation/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(/Usa este espacio para rotar credenciales compartidas/i),
    ).toBeInTheDocument();
    expect(screen.queryByText('Atajos de navegación')).not.toBeInTheDocument();
    expect(screen.queryByText('Navigation shortcuts')).not.toBeInTheDocument();
  });

  it('ignores quick-link fallback cards that duplicate first-run section anchors', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'quick-links',
          title: 'Quick links',
          body: ['Jump to service health, users, roles, and audit from this admin panel.'],
        },
        {
          cardId: 'accesos-rapidos',
          title: 'Accesos rápidos',
          body: ['Salta a estado del servicio, usuarios y auditoría desde este panel.'],
        },
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
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 3 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Quick links')).not.toBeInTheDocument();
    expect(screen.queryByText('Accesos rápidos')).not.toBeInTheDocument();
    expect(screen.queryByText(/Jump to service health/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Salta a estado del servicio/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(/Usa este espacio para rotar credenciales compartidas/i),
    ).toBeInTheDocument();
    expect(screen.queryByText('Quick links')).not.toBeInTheDocument();
    expect(screen.queryByText('Accesos rápidos')).not.toBeInTheDocument();
  });

  it('ignores quick-action fallback cards that duplicate built-in admin actions', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'admin-quick-actions',
          title: 'Quick actions',
          body: ['Refresh the panel, load demo data, and edit roles from one action area.'],
        },
        {
          cardId: 'acciones-rapidas',
          title: 'Acciones rápidas',
          body: ['Usa acciones rápidas para revisar estado, usuarios y auditoría desde este panel.'],
        },
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
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 3 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Quick actions')).not.toBeInTheDocument();
    expect(screen.queryByText('Acciones rápidas')).not.toBeInTheDocument();
    expect(screen.queryByText(/Refresh the panel/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Usa acciones rápidas/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(/Usa este espacio para rotar credenciales compartidas/i),
    ).toBeInTheDocument();
    expect(screen.queryByText('Quick actions')).not.toBeInTheDocument();
    expect(screen.queryByText('Acciones rápidas')).not.toBeInTheDocument();
  });

  it('ignores no-action-required fallback cards so first-run users only open actionable modules', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'integrations-no-action',
          title: 'Integraciones',
          body: ['No action required.'],
        },
        {
          cardId: 'credenciales-sin-accion',
          title: 'Credenciales externas',
          body: ['No se requiere acción administrativa.'],
        },
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
      expect(
        screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 3 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Credenciales externas')).not.toBeInTheDocument();
    expect(screen.queryByText(/No action required/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/No se requiere acción administrativa/i)).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /^Opcional: ver 1 módulo adicional$/i }));

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(/Usa este espacio para rotar credenciales compartidas/i),
    ).toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Credenciales externas')).not.toBeInTheDocument();
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

  it('ignores generated dummy-copy fallback cards while keeping real first-run modules', async () => {
    const user = userEvent.setup();
    mockConsolePreview.mockResolvedValue({
      status: 'preview',
      cards: [
        {
          cardId: 'integrations-dummy-copy',
          title: 'Integraciones',
          body: ['Lorem ipsum dolor sit amet.'],
        },
        {
          cardId: 'ops-report-sample-copy',
          title: 'Reporte operativo',
          body: ['Sample content goes here.'],
        },
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
      expect(
        screen.getByRole(
          'button',
          { name: /^Opcional: ver 1 módulo adicional$/i },
        ),
      ).toBeInTheDocument();
    });

    expect(
      screen.queryByRole('button', { name: /^Opcional: ver 3 módulos adicionales$/i }),
    ).not.toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Reporte operativo')).not.toBeInTheDocument();
    expect(screen.queryByText(/Lorem ipsum/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Sample content goes here/i)).not.toBeInTheDocument();

    await user.click(
      screen.getByRole(
        'button',
        { name: /^Opcional: ver 1 módulo adicional$/i },
      ),
    );

    expect(await screen.findByText('Tokens de servicio')).toBeInTheDocument();
    expect(
      screen.getByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).toBeInTheDocument();
    expect(screen.queryByText('Integraciones')).not.toBeInTheDocument();
    expect(screen.queryByText('Reporte operativo')).not.toBeInTheDocument();
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

  it('collapses expanded standalone modules when fallback discovery changes the module set', async () => {
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

    const { queryClient } = renderPage();

    expect(await screen.findByText('Consola de administración')).toBeInTheDocument();

    await user.click(
      await screen.findByRole('button', { name: /Ver detalles de Tokens de servicio/i }),
    );

    expect(
      await screen.findByText(
        /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
      ),
    ).toBeInTheDocument();

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
      expect(screen.queryByText('Tokens de servicio')).not.toBeInTheDocument();
      expect(screen.getByText('Integraciones')).toBeInTheDocument();
      expect(
        screen.getByRole('button', { name: /Ver detalles de Integraciones/i }),
      ).toHaveAttribute('aria-expanded', 'false');
    });

    expect(screen.queryByRole('button', { name: /Ocultar módulo adicional/i })).not.toBeInTheDocument();
    expect(
      screen.queryByText(/Revisa conectores pendientes sin salir de la consola\./i),
    ).not.toBeInTheDocument();
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
        screen.getByText(/Último acceso y estado aparecerán cuando aporten contexto\./i),
      ).toBeInTheDocument();
      expect(screen.queryByText(/Vista compacta:/i)).not.toBeInTheDocument();
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
        screen.getByText(/Último acceso y estado aparecerán cuando aporten contexto\./i),
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
          /Revisa los roles actuales y usa el botón de cada fila para ajustar permisos desde esta misma vista\. Último acceso y estado aparecerán cuando aporten contexto\./i,
        ),
      ).toBeInTheDocument();
      expect(
        screen.queryByText(/Revisa los roles actuales y usa Editar roles para ajustar permisos/i),
      ).not.toBeInTheDocument();
      expect(
        screen.queryByText(/^Último acceso y estado aparecerán cuando aporten contexto\.$/i),
      ).not.toBeInTheDocument();
      expect(screen.queryByText(/Vista compacta:/i)).not.toBeInTheDocument();
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
      const roleEditButtons = screen.getAllByRole('button', { name: /Editar roles de /i });
      expect(roleEditButtons).toHaveLength(2);
      expect(roleEditButtons[0]).toHaveTextContent(/^Editar roles$/);
      expect(roleEditButtons[0]).toHaveAttribute('title', 'Editar roles de Ada Lovelace. Roles actuales: Admin');
      expect(roleEditButtons[0]).not.toHaveAttribute('aria-describedby');
      expect(screen.getByText('Manager')).toBeInTheDocument();
      expect(roleEditButtons[1]).toHaveTextContent(/^Editar$/);
      expect(roleEditButtons[1]).toHaveAttribute('title', 'Editar roles de Grace Hopper. Roles actuales: Manager');
      expect(roleEditButtons[1]).not.toHaveAttribute('aria-describedby');
      expect(screen.queryByTestId('EditOutlinedIcon')).not.toBeInTheDocument();
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
          /Revisa los roles actuales y usa el botón de cada fila para ajustar permisos desde esta misma vista\. Último acceso y estado aparecerán cuando aporten contexto\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.queryByText(/Haz clic en un rol para editarlo desde esta misma vista\./i)).not.toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Roles$/i })).toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Roles editables$/i })).not.toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
    expect(screen.queryAllByText('—')).toHaveLength(0);
  });

  it('hides invalid last-access timestamps from the users table until a real date adds context', async () => {
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        lastSeenAt: 'not-a-date',
      }),
      buildAdminUser({
        userId: 102,
        username: 'grace',
        displayName: 'Grace Hopper',
        partyId: 10,
        roles: ['Manager'],
        lastLoginAt: '   ',
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
      expect(screen.getByText('Grace Hopper')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Último acceso y estado aparecerán cuando aporten contexto\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.queryByText(/Vista compacta:/i)).not.toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /Último acceso/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Fecha no disponible')).not.toBeInTheDocument();
    expect(screen.queryByText('not-a-date')).not.toBeInTheDocument();
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
        screen.getByText(/Revisa los roles actuales y usa el botón de cada fila para ajustar permisos desde esta misma vista\./i),
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
    expect(within(activeRow).queryByText('—')).not.toBeInTheDocument();
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

  it('keeps large admin user lists compact until the admin asks for the rest', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([
      buildAdminUser(),
      buildAdminUser({
        userId: 102,
        username: 'grace',
        displayName: 'Grace Hopper',
        partyId: 10,
        roles: ['Manager'],
      }),
      buildAdminUser({
        userId: 103,
        username: 'linus',
        displayName: 'Linus Torvalds',
        partyId: 11,
        roles: ['ReadOnly'],
      }),
      buildAdminUser({
        userId: 104,
        username: 'marie',
        displayName: 'Marie Curie',
        partyId: 12,
        roles: ['Accounting'],
      }),
      buildAdminUser({
        userId: 105,
        username: 'nikola',
        displayName: 'Nikola Tesla',
        partyId: 13,
        roles: ['Engineer'],
      }),
      buildAdminUser({
        userId: 106,
        username: 'rosalind',
        displayName: 'Rosalind Franklin',
        partyId: 14,
        roles: ['Teacher'],
      }),
      buildAdminUser({
        userId: 107,
        username: 'zoe',
        displayName: 'Zoe Washburne',
        partyId: 15,
        roles: ['Reception'],
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
      expect(screen.getByText('Nikola Tesla')).toBeInTheDocument();
      expect(screen.queryByText('Rosalind Franklin')).not.toBeInTheDocument();
      expect(screen.queryByText('Zoe Washburne')).not.toBeInTheDocument();
      expect(screen.getAllByRole('button', { name: /Editar roles de /i })).toHaveLength(5);
      expect(screen.getByRole('button', { name: /Ver 2 usuarios más/i })).toHaveAttribute('aria-expanded', 'false');
    });

    await user.click(screen.getByRole('button', { name: /Ver 2 usuarios más/i }));

    await waitFor(() => {
      expect(screen.getByText('Rosalind Franklin')).toBeInTheDocument();
      expect(screen.getByText('Zoe Washburne')).toBeInTheDocument();
      expect(screen.getAllByRole('button', { name: /Editar roles de /i })).toHaveLength(7);
      expect(screen.getByRole('button', { name: /Mostrar solo 5 usuarios/i })).toHaveAttribute('aria-expanded', 'true');
    });

    await user.click(screen.getByRole('button', { name: /Mostrar solo 5 usuarios/i }));

    await waitFor(() => {
      expect(screen.queryByText('Rosalind Franklin')).not.toBeInTheDocument();
      expect(screen.queryByText('Zoe Washburne')).not.toBeInTheDocument();
      expect(screen.getAllByRole('button', { name: /Editar roles de /i })).toHaveLength(5);
    });
  });

  it('summarizes hidden admin statuses in the overflow action without adding a status column early', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([
      buildAdminUser(),
      buildAdminUser({
        userId: 102,
        username: 'grace',
        displayName: 'Grace Hopper',
        partyId: 10,
        roles: ['Manager'],
      }),
      buildAdminUser({
        userId: 103,
        username: 'linus',
        displayName: 'Linus Torvalds',
        partyId: 11,
        roles: ['ReadOnly'],
      }),
      buildAdminUser({
        userId: 104,
        username: 'marie',
        displayName: 'Marie Curie',
        partyId: 12,
        roles: ['Accounting'],
      }),
      buildAdminUser({
        userId: 105,
        username: 'nikola',
        displayName: 'Nikola Tesla',
        partyId: 13,
        roles: ['Engineer'],
      }),
      buildAdminUser({
        userId: 106,
        username: 'zoe',
        displayName: 'Zoe Washburne',
        partyId: 14,
        roles: ['Teacher'],
        status: 'INVITED',
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
      expect(screen.getByText('Nikola Tesla')).toBeInTheDocument();
      expect(screen.queryByText('Zoe Washburne')).not.toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Ver 1 usuario más \(1 requiere atención\)/i })).toHaveAttribute('aria-expanded', 'false');
      expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
      expect(screen.queryByText('Invitado')).not.toBeInTheDocument();
    });

    await user.click(screen.getByRole('button', { name: /Ver 1 usuario más \(1 requiere atención\)/i }));

    await waitFor(() => {
      expect(screen.getByText('Zoe Washburne')).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Estado$/i })).toBeInTheDocument();
      expect(screen.getByText('Invitado')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Mostrar solo 5 usuarios/i })).toHaveAttribute('aria-expanded', 'true');
    });
  });

  it('summarizes hidden roleless admins in the overflow action without expanding the table', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([
      buildAdminUser(),
      buildAdminUser({
        userId: 102,
        username: 'grace',
        displayName: 'Grace Hopper',
        partyId: 10,
        roles: ['Manager'],
      }),
      buildAdminUser({
        userId: 103,
        username: 'linus',
        displayName: 'Linus Torvalds',
        partyId: 11,
        roles: ['ReadOnly'],
      }),
      buildAdminUser({
        userId: 104,
        username: 'marie',
        displayName: 'Marie Curie',
        partyId: 12,
        roles: ['Accounting'],
      }),
      buildAdminUser({
        userId: 105,
        username: 'nikola',
        displayName: 'Nikola Tesla',
        partyId: 13,
        roles: ['Engineer'],
      }),
      buildAdminUser({
        userId: 106,
        username: 'zoe',
        displayName: 'Zoe Washburne',
        partyId: 14,
        roles: [],
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
      expect(screen.getByText('Nikola Tesla')).toBeInTheDocument();
      expect(screen.queryByText('Zoe Washburne')).not.toBeInTheDocument();
      expect(
        screen.getByRole('button', { name: /Ver 1 usuario más \(1 requiere atención\)/i }),
      ).toHaveAttribute('aria-expanded', 'false');
      expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
      expect(screen.queryByText('Sin roles')).not.toBeInTheDocument();
    });

    await user.click(screen.getByRole('button', { name: /Ver 1 usuario más \(1 requiere atención\)/i }));

    await waitFor(() => {
      expect(screen.getByText('Zoe Washburne')).toBeInTheDocument();
      expect(screen.getByText('Sin roles')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Asignar roles de Zoe Washburne' })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Mostrar solo 5 usuarios/i })).toHaveAttribute('aria-expanded', 'true');
    });

    expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Activo')).not.toBeInTheDocument();
  });

  it('keeps role review uncluttered until a pending role change needs save and discard actions', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([buildAdminUser()]);

    renderPage();

    const editButton = await screen.findByRole('button', { name: 'Editar roles de Ada Lovelace' });
    await user.click(editButton);

    expect(
      screen.getByText(/Sin cambios pendientes\. Modifica la selección para mostrar Guardar cambios\./i),
    ).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /^Cerrar$/i })).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Guardar cambios/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Descartar cambios/i })).not.toBeInTheDocument();

    const rolesSelect = document.body.querySelector('[role="combobox"]');
    if (!(rolesSelect instanceof HTMLElement)) {
      throw new Error('Roles select not found');
    }

    await user.click(rolesSelect);
    await user.click(getMenuItemByText('Manager'));
    await user.keyboard('{Escape}');

    await waitFor(() => {
      const saveButton = screen.getByRole('button', { name: /Guardar cambios/i });
      expect(saveButton).toBeEnabled();
      expect(screen.getByRole('button', { name: /Descartar cambios/i })).toBeEnabled();
      expect(screen.queryByRole('button', { name: /^Cerrar$/i })).not.toBeInTheDocument();
      expect(screen.getByText(/Cambio pendiente: agregar Manager\./i)).toBeInTheDocument();
    });
  });

  it('refreshes users and audit after saving roles so admins can verify the change inline', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([buildAdminUser()]);

    const { queryClient } = renderPage();
    const invalidateQueriesSpy = vi.spyOn(queryClient, 'invalidateQueries');

    await user.click(await screen.findByRole('button', { name: 'Editar roles de Ada Lovelace' }));

    const rolesSelect = document.body.querySelector('[role="combobox"]');
    if (!(rolesSelect instanceof HTMLElement)) {
      throw new Error('Roles select not found');
    }

    await user.click(rolesSelect);
    await user.click(getMenuItemByText('Teacher'));
    await user.keyboard('{Escape}');

    expect(
      await screen.findByText(
        /Cambio pendiente: agregar Teacher\. Al guardar, usuarios y auditoría se actualizarán automáticamente\./i,
      ),
    ).toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /Guardar cambios/i }));

    await waitFor(() => {
      expect(mockUpdateUserRoles).toHaveBeenCalledWith(101, ['Admin', 'Teacher']);
      expect(invalidateQueriesSpy).toHaveBeenCalledWith({ queryKey: ['admin', 'users'] });
      expect(invalidateQueriesSpy).toHaveBeenCalledWith({ queryKey: ['admin', 'audit'] });
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

  it('keeps pending long role selections compact while the helper explains the change', async () => {
    const user = userEvent.setup();
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        roles: ['Admin', 'Manager', 'Engineer', 'Teacher', 'Reception'],
      }),
    ]);

    renderPage();

    await user.click(await screen.findByRole('button', { name: 'Editar roles de Ada Lovelace' }));

    const rolesSelect = document.body.querySelector('[role="combobox"]');
    if (!(rolesSelect instanceof HTMLElement)) {
      throw new Error('Roles select not found');
    }

    await user.click(rolesSelect);
    await user.click(getMenuItemByText('ReadOnly'));
    await user.keyboard('{Escape}');

    await waitFor(() => {
      expect(rolesSelect).toHaveTextContent('Admin, Manager +4 roles');
      expect(rolesSelect).toHaveAttribute('title', 'Admin, Manager, Engineer, Teacher, Reception, ReadOnly');
      expect(
        screen.getByText(
          /Cambio pendiente: agregar ReadOnly\. Al guardar, usuarios y auditoría se actualizarán automáticamente\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(rolesSelect).not.toHaveTextContent('Engineer, Teacher, Reception, ReadOnly');
  });

  it('keeps long single-user role summaries compact while preserving the full role detail', async () => {
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        roles: ['Admin', 'Manager', 'Engineer', 'Teacher', 'Reception'],
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      const roleSummary = screen.getByText('Roles: Admin, Manager +3 roles');

      expect(roleSummary).toHaveAttribute('title', 'Admin, Manager, Engineer, Teacher, Reception');
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toHaveTextContent('Editar roles');
    });

    expect(
      screen.queryByText('Roles: Admin, Manager, Engineer, Teacher, Reception'),
    ).not.toBeInTheDocument();
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
      expect(screen.getByRole('button', { name: 'Asignar roles de Ada Lovelace' })).toHaveTextContent('Asignar roles');
      expect(screen.getByRole('button', { name: 'Asignar roles de Ada Lovelace' })).toHaveAttribute('title', 'Asignar roles de Ada Lovelace. Roles actuales: Sin roles');
      expect(screen.queryByRole('button', { name: 'Editar roles de Ada Lovelace' })).not.toBeInTheDocument();
      expect(screen.queryByText(/^—$/i)).not.toBeInTheDocument();
    });

    await user.click(screen.getByRole('button', { name: 'Asignar roles de Ada Lovelace' }));

    expect(await screen.findByRole('heading', { name: 'Asignar roles · Ada Lovelace' })).toBeInTheDocument();
    expect(screen.queryByRole('heading', { name: 'Editar roles · Ada Lovelace' })).not.toBeInTheDocument();
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

    expect(screen.queryByText(/Nota: .*muestran la misma navegación principal/i)).not.toBeInTheDocument();

    const rolesSelect = document.body.querySelector('[role="combobox"]');
    if (!(rolesSelect instanceof HTMLElement)) {
      throw new Error('Roles select not found');
    }

    await user.click(rolesSelect);
    await user.click(getMenuItemByText('Manager'));
    await user.keyboard('{Escape}');

    expect(
      await screen.findByText(
        /Cambio pendiente: agregar Manager\. Nota: Admin y Manager muestran la misma navegación principal; revisa si necesitas todos antes de guardar\./i,
      ),
    ).toBeInTheDocument();
    expect(screen.queryByRole('alert')).not.toBeInTheDocument();
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
      screen.getByText(/Sin cambios pendientes\. Modifica la selección para mostrar Guardar cambios\./i),
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/Nota: .*muestran la misma navegación principal/i),
    ).not.toBeInTheDocument();
    expect(screen.queryByRole('alert')).not.toBeInTheDocument();
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

  it('keeps long single-audit details compact while preserving the full diff', async () => {
    const longDiff = 'Admin -> Admin, Manager; Teacher -> Teacher, Accounting; Reception -> Reception, ReadOnly; Artist -> Artist, Vendor';
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'audit-1',
        actorId: 101,
        entity: 'user',
        entityId: '101',
        action: 'roles.updated',
        diff: longDiff,
        createdAt: '2026-04-09T15:30:00.000Z',
      },
    ]);

    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText(/Acción:\s*Roles actualizados/i)).toBeInTheDocument();
    });

    const compactDetail = screen.getByText((content) => (
      content.startsWith('Detalle: Admin -> Admin, Manager; Teacher -> Teacher')
      && content.endsWith('…')
    ));
    expect(compactDetail).toHaveAttribute('title', longDiff);
    expect(compactDetail).not.toHaveTextContent(longDiff);
    expect(screen.queryByText(longDiff)).not.toBeInTheDocument();
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

  it('hides invalid admin timestamps from compact summaries until a real date adds context', async () => {
    mockListUsers.mockResolvedValue([
      buildAdminUser({
        lastSeenAt: 'not-a-date',
      }),
    ]);
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'audit-1',
        actorId: 101,
        entity: 'user',
        entityId: '101',
        action: 'roles.updated',
        diff: 'Admin -> Manager',
        createdAt: 'not-a-date',
      },
    ]);

    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText(/Acción:\s*Roles actualizados/i)).toBeInTheDocument();
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
    });

    expect(screen.queryByText(/^Último acceso:/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^Fecha:/i)).not.toBeInTheDocument();
    expect(screen.queryByText('Fecha no disponible')).not.toBeInTheDocument();
    expect(screen.queryByText(/Invalid Date/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/not-a-date/i)).not.toBeInTheDocument();
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
      expect(screen.getByText(/Acción:\s*Paquete creado/i)).toBeInTheDocument();
      expect(screen.getByText(/Entidad:\s*Paquete · PKG-1/i)).toBeInTheDocument();
    });

    expect(screen.getByText(/Acción:\s*Paquete creado/i)).toHaveAttribute('title', 'package.created');
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

  it('deduplicates repeated audit entries when fallback data only changes whitespace', async () => {
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
        entity: ' package ',
        entityId: ' PKG-1 ',
        action: ' package.created ',
        diff: '   ',
        createdAt: ' 2026-04-09T15:30:00.000Z ',
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
      expect(screen.getByText(/Acción:\s*Paquete creado/i)).toBeInTheDocument();
      expect(screen.getByText(/Entidad:\s*Paquete · PKG-1/i)).toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /^Fecha$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Entidad$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acción$/i })).not.toBeInTheDocument();
    expect(screen.queryByText(/^—$/i)).not.toBeInTheDocument();
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
          /Actor y detalle aparecerán cuando aporten contexto\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.queryByText(/Vista compacta:/i)).not.toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Fecha$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Entidad$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Acción$/i })).toBeInTheDocument();
      expect(screen.getByText('Paquete creado')).toBeInTheDocument();
      expect(screen.getByText('Paquete sincronizado')).toBeInTheDocument();
    });

    expect(screen.getByText('Paquete creado')).toHaveAttribute('title', 'package.created');
    expect(screen.getByText('Paquete sincronizado')).toHaveAttribute('title', 'package.synced');
    expect(screen.queryByText('package.created')).not.toBeInTheDocument();
    expect(screen.queryByText('package.synced')).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Actor$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Detalle$/i })).not.toBeInTheDocument();
    expect(screen.queryByText(/^Sistema$/i)).not.toBeInTheDocument();
  });

  it('hides invalid audit timestamps from the table until a real date adds context', async () => {
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'audit-1',
        actorId: 101,
        entity: 'user',
        entityId: '101',
        action: 'roles.updated',
        diff: 'Admin -> Manager',
        createdAt: 'not-a-date',
      },
      {
        auditId: 'audit-2',
        actorId: 102,
        entity: 'package',
        entityId: 'PKG-2',
        action: 'package.synced',
        diff: 'Paquete sincronizado con facturación',
        createdAt: '   ',
      },
    ]);

    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(/Fecha aparecerá cuando aporte contexto\./i),
      ).toBeInTheDocument();
      expect(screen.queryByText(/Vista compacta:/i)).not.toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Entidad$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Acción$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Actor$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Detalle$/i })).toBeInTheDocument();
      expect(screen.getByText('Roles actualizados')).toBeInTheDocument();
      expect(screen.getByText('Paquete sincronizado')).toBeInTheDocument();
      expect(screen.getByText('Usuario #101')).toBeInTheDocument();
      expect(screen.getByText('Usuario #102')).toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /^Fecha$/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Fecha no disponible')).not.toBeInTheDocument();
    expect(screen.queryByText('not-a-date')).not.toBeInTheDocument();
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

  it('keeps long audit table details compact while preserving the full diff', async () => {
    const longDiff = 'Admin -> Admin, Manager; Teacher -> Teacher, Accounting; Reception -> Reception, ReadOnly; Artist -> Artist, Vendor';
    mockAuditLogs.mockResolvedValue([
      {
        auditId: 'audit-1',
        actorId: null,
        entity: 'package',
        entityId: 'PKG-1',
        action: 'package.synced',
        diff: '   ',
        createdAt: '2026-04-09T15:30:00.000Z',
      },
      {
        auditId: 'audit-2',
        actorId: 777,
        entity: 'user',
        entityId: 'USR-1',
        action: 'roles.updated',
        diff: longDiff,
        createdAt: '2026-04-09T16:00:00.000Z',
      },
    ]);

    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Roles actualizados')).toBeInTheDocument();
      expect(screen.getByText('Paquete sincronizado')).toBeInTheDocument();
    });

    const compactDetail = screen.getByText((content) => (
      content.startsWith('Admin -> Admin, Manager; Teacher -> Teacher')
      && content.endsWith('…')
    ));
    expect(compactDetail).toHaveAttribute('title', longDiff);
    expect(compactDetail).not.toHaveTextContent(longDiff);
    expect(screen.queryByText(longDiff)).not.toBeInTheDocument();

    const emptyDetailRow = screen.getByText('Paquete sincronizado').closest('tr');
    if (!(emptyDetailRow instanceof HTMLElement)) {
      throw new Error('Expected audit row to render inside the table');
    }

    expect(within(emptyDetailRow).getByText('—')).toBeInTheDocument();
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
      expect(screen.getByText('Paquete creado')).toBeInTheDocument();
      expect(screen.getByText('Roles actualizados')).toBeInTheDocument();
    });

    expectToAppearBefore(screen.getByText('Roles actualizados'), screen.getByText('Paquete creado'));
  });

  it('keeps the audit table focused on the five newest events until expanded', async () => {
    const user = userEvent.setup();
    mockAuditLogs.mockResolvedValue(
      Array.from({ length: 7 }, (_, index) => {
        const auditNumber = index + 1;

        return {
          auditId: `audit-${auditNumber}`,
          actorId: auditNumber <= 2 ? 777 : null,
          entity: 'package',
          entityId: `PKG-${auditNumber}`,
          action: `package.synced.${auditNumber}`,
          diff: auditNumber <= 2 ? `Older detail ${auditNumber}` : null,
          createdAt: `2026-04-09T1${auditNumber}:00:00.000Z`,
        };
      }),
    );

    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Paquete · PKG-7')).toBeInTheDocument();
      expect(screen.getByText('Paquete · PKG-3')).toBeInTheDocument();
      expect(screen.queryByText('Paquete · PKG-2')).not.toBeInTheDocument();
      expect(screen.queryByText('Paquete · PKG-1')).not.toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Actor$/i })).not.toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Detalle$/i })).not.toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Ver 2 cambios anteriores/i })).toBeInTheDocument();
    });

    await user.click(screen.getByRole('button', { name: /Ver 2 cambios anteriores/i }));

    await waitFor(() => {
      expect(screen.getByText('Paquete · PKG-2')).toBeInTheDocument();
      expect(screen.getByText('Paquete · PKG-1')).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Actor$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Detalle$/i })).toBeInTheDocument();
      expect(screen.getByText('Older detail 2')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Mostrar solo cambios recientes/i })).toBeInTheDocument();
    });

    await user.click(screen.getByRole('button', { name: /Mostrar solo cambios recientes/i }));

    await waitFor(() => {
      expect(screen.queryByText('Paquete · PKG-2')).not.toBeInTheDocument();
      expect(screen.queryByText('Paquete · PKG-1')).not.toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Ver 2 cambios anteriores/i })).toBeInTheDocument();
    });
  });

  it('summarizes hidden audit actor/detail context in the overflow action without adding columns early', async () => {
    const user = userEvent.setup();
    mockAuditLogs.mockResolvedValue(
      Array.from({ length: 6 }, (_, index) => {
        const auditNumber = index + 1;
        const isHiddenOlderEntry = auditNumber === 1;

        return {
          auditId: `audit-${auditNumber}`,
          actorId: isHiddenOlderEntry ? 777 : null,
          entity: 'package',
          entityId: `PKG-${auditNumber}`,
          action: `package.synced.${auditNumber}`,
          diff: isHiddenOlderEntry ? 'Revisó permisos después de importar usuarios.' : null,
          createdAt: `2026-04-09T1${auditNumber}:00:00.000Z`,
        };
      }),
    );

    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Paquete · PKG-6')).toBeInTheDocument();
      expect(screen.getByText('Paquete · PKG-2')).toBeInTheDocument();
      expect(screen.queryByText('Paquete · PKG-1')).not.toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Actor$/i })).not.toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Detalle$/i })).not.toBeInTheDocument();
      expect(
        screen.getByRole('button', { name: /Ver 1 cambio anterior \(1 con actor o detalle\)/i }),
      ).toHaveAttribute('aria-expanded', 'false');
    });

    await user.click(screen.getByRole('button', { name: /Ver 1 cambio anterior \(1 con actor o detalle\)/i }));

    await waitFor(() => {
      expect(screen.getByText('Paquete · PKG-1')).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Actor$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Detalle$/i })).toBeInTheDocument();
      expect(screen.getByText('Usuario #777')).toBeInTheDocument();
      expect(screen.getByText('Revisó permisos después de importar usuarios.')).toBeInTheDocument();
    });
  });

  it('collapses expanded audit history when a refreshed audit feed arrives', async () => {
    const user = userEvent.setup();
    mockAuditLogs.mockResolvedValue(
      Array.from({ length: 7 }, (_, index) => {
        const auditNumber = index + 1;

        return {
          auditId: `audit-${auditNumber}`,
          actorId: null,
          entity: 'package',
          entityId: `PKG-${auditNumber}`,
          action: `package.synced.${auditNumber}`,
          diff: null,
          createdAt: `2026-04-09T1${auditNumber}:00:00.000Z`,
        };
      }),
    );

    const { queryClient } = renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await user.click(await screen.findByRole('button', { name: /Ver 2 cambios anteriores/i }));

    expect(await screen.findByText('Paquete · PKG-1')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /Mostrar solo cambios recientes/i })).toBeInTheDocument();

    act(() => {
      queryClient.setQueryData(
        ['admin', 'audit'],
        Array.from({ length: 7 }, (_, index) => {
          const auditNumber = index + 1;

          return {
            auditId: `audit-next-${auditNumber}`,
            actorId: null,
            entity: 'package',
            entityId: `PKG-NEXT-${auditNumber}`,
            action: `package.synced.next.${auditNumber}`,
            diff: null,
            createdAt: `2026-04-10T1${auditNumber}:00:00.000Z`,
          };
        }),
      );
    });

    await waitFor(() => {
      expect(screen.getByText('Paquete · PKG-NEXT-7')).toBeInTheDocument();
      expect(screen.getByText('Paquete · PKG-NEXT-3')).toBeInTheDocument();
      expect(screen.queryByText('Paquete · PKG-NEXT-2')).not.toBeInTheDocument();
      expect(screen.queryByText('Paquete · PKG-NEXT-1')).not.toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Ver 2 cambios anteriores/i })).toBeInTheDocument();
    });

    expect(screen.queryByRole('button', { name: /Mostrar solo cambios recientes/i })).not.toBeInTheDocument();
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
