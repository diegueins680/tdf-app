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
          /Todavía no hay usuarios administrables\. Cuando exista el primero, aquí verás roles, último acceso y el atajo para editar permisos\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /^Usuario$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Roles$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /Último acceso/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acciones$/i })).not.toBeInTheDocument();
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
      expect(screen.getByText('Ada Lovelace')).toBeInTheDocument();
      expect(screen.getByText('Usuario: ada')).toBeInTheDocument();
      expect(screen.queryByText('Usuario: grace')).not.toBeInTheDocument();
      expect(screen.queryByText('Usuario: linus')).not.toBeInTheDocument();
      expect(screen.getAllByText('grace')).toHaveLength(1);
      expect(screen.getAllByText('linus')).toHaveLength(1);
    });
  });
});
