import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { render, screen, waitFor } from '@testing-library/react';
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

  it('uses one page-level refresh action and a clear header description', async () => {
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
      screen.getByRole('button', { name: /Actualizar panel/i }),
    ).toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /^Actualizar$/i }),
    ).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /Refrescar/i }),
    ).not.toBeInTheDocument();
    expect(
      screen.getByText(/Al terminar, el panel se actualiza automáticamente para evitar un refresco manual extra\./i),
    ).toBeInTheDocument();
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
          /Valida el estado del servicio antes de cambiar permisos o repetir una acción\./i,
        ),
      ).toBeInTheDocument();
      expect(
        screen.getByText(
          /Ajusta los accesos desde Usuarios y roles para resolver el caso actual\./i,
        ),
      ).toBeInTheDocument();
      expect(
        screen.getByText(
          /Confirma el resultado en Auditoría reciente antes de seguir con otro cambio\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByText('Gestión de usuarios')).not.toBeInTheDocument();
    expect(screen.queryByText(/Si es tu primera vez aquí/i)).not.toBeInTheDocument();
    expect(
      screen.queryByText(/La asignación de roles se administra desde la pantalla de Parties\./i),
    ).not.toBeInTheDocument();
  });

  it('keeps unique preview cards that add context beyond the built-in admin sections', async () => {
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
      expect(
        screen.getByText(
          /Usa este espacio para rotar credenciales compartidas sin tocar los permisos de usuarios\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByText('Primeros pasos')).not.toBeInTheDocument();
  });

  it('refreshes every admin dataset from the single panel action', async () => {
    const user = userEvent.setup();
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
        screen.getByRole('button', { name: /Seed demo data/i }),
      ).toBeEnabled(),
    );

    await user.click(screen.getByRole('button', { name: /Seed demo data/i }));

    await waitFor(() => {
      expect(mockSeed).toHaveBeenCalledTimes(1);
      expect(invalidateQueriesSpy).toHaveBeenCalledWith({ queryKey: ['admin', 'health'] });
      expect(invalidateQueriesSpy).toHaveBeenCalledWith({ queryKey: ['admin', 'console'] });
      expect(invalidateQueriesSpy).toHaveBeenCalledWith({ queryKey: ['admin', 'users'] });
      expect(invalidateQueriesSpy).toHaveBeenCalledWith({ queryKey: ['admin', 'audit'] });
    });
  });

  it('replaces the empty users grid with first-run guidance instead of showing blank table chrome', async () => {
    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Todavía no hay usuarios administrables\. Cuando exista el primero, aquí verás roles, último acceso y el atajo para editar roles\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /^Usuario$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Roles$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /Último acceso/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Permisos$/i })).not.toBeInTheDocument();
  });

  it('keeps the role editing action inside the roles column instead of adding a duplicate permissions column', async () => {
    mockListUsers.mockResolvedValue([buildAdminUser()]);

    renderPage();

    expect(await screen.findByText('Usuarios y roles')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByRole('columnheader', { name: /^Roles$/i })).toBeInTheDocument();
      expect(screen.queryByRole('columnheader', { name: /^Permisos$/i })).not.toBeInTheDocument();
      expect(screen.getByText('Admin')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Editar roles de Ada Lovelace' })).toBeInTheDocument();
      expect(screen.getByText('Editar roles')).toBeInTheDocument();
    });
  });

  it('replaces the empty audit table with first-run guidance instead of blank table chrome', async () => {
    renderPage();

    expect(await screen.findByText('Auditoría reciente')).toBeInTheDocument();

    await waitFor(() => {
      expect(
        screen.getByText(
          /Todavía no hay eventos de auditoría\. Cuando alguien cambie permisos o datos del sistema, aquí verás quién hizo qué y cuándo\./i,
        ),
      ).toBeInTheDocument();
    });

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
