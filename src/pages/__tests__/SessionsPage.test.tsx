import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { render, screen, waitFor } from '@testing-library/react';
import { vi } from 'vitest';
import SessionsPage from '../SessionsPage';

const mockSessionsList = vi.fn();
const mockSessionsOptions = vi.fn();
const mockSessionsCreate = vi.fn();
const mockSessionsDetail = vi.fn();
const mockSessionsUpdate = vi.fn();
const mockRoomsList = vi.fn();
const mockInventoryList = vi.fn();

vi.mock('../../api/sessions', () => ({
  Sessions: {
    list: (params?: { page?: number; pageSize?: number }) => mockSessionsList(params),
    options: () => mockSessionsOptions(),
    create: (body: unknown) => mockSessionsCreate(body),
    detail: (id: string) => mockSessionsDetail(id),
    update: (id: string, body: unknown) => mockSessionsUpdate(id, body),
  },
}));

vi.mock('../../api/rooms', () => ({
  Rooms: {
    list: () => mockRoomsList(),
  },
}));

vi.mock('../../api/inventory', () => ({
  Inventory: {
    list: (params?: { page?: number; pageSize?: number }) => mockInventoryList(params),
  },
}));

function createQueryClient() {
  return new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });
}

function renderPage() {
  const queryClient = createQueryClient();

  return render(
    <QueryClientProvider client={queryClient}>
      <SessionsPage />
    </QueryClientProvider>,
  );
}

describe('SessionsPage', () => {
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
    mockSessionsList.mockResolvedValue({
      items: [],
      page: 1,
      pageSize: 10,
      total: 0,
    });
    mockSessionsOptions.mockResolvedValue({ bands: [] });
    mockRoomsList.mockResolvedValue([]);
    mockInventoryList.mockResolvedValue({
      items: [],
      page: 1,
      pageSize: 500,
      total: 0,
    });
    mockSessionsCreate.mockResolvedValue(undefined);
    mockSessionsDetail.mockResolvedValue(undefined);
    mockSessionsUpdate.mockResolvedValue(undefined);
  });

  it('replaces the empty sessions table with a focused first-run state until the first session exists', async () => {
    renderPage();

    expect(await screen.findByText('Sesiones')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByRole('button', { name: /Nueva sesión/i })).toBeInTheDocument();
      expect(screen.getByText('Todavía no hay sesiones registradas.')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Empieza con Nueva sesión\. La tabla, edición rápida y paginación aparecerán cuando exista la primera sesión\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('table')).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Horario$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Servicio$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Booking$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Ingeniero$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Salas$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acciones$/i })).not.toBeInTheDocument();
    expect(screen.queryByText(/No hay sesiones registradas todavía\./i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Rows per page/i)).not.toBeInTheDocument();
  });
});
