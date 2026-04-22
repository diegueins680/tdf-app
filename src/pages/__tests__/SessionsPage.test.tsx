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

  it('replaces the one-row schedule table with a compact first-session summary and one edit action', async () => {
    mockSessionsList.mockResolvedValue({
      items: [
        {
          sessionId: 'session-1',
          sBookingRef: 'BK-001',
          sBandId: null,
          sClientPartyRef: null,
          sService: 'Grabación vocal',
          sStartAt: '2026-04-15T15:00:00.000Z',
          sEndAt: '2026-04-15T17:00:00.000Z',
          sEngineerRef: 'ada@tdf',
          sAssistantRef: null,
          sRoomIds: ['room-a'],
          sSampleRate: null,
          sBitDepth: null,
          sDaw: null,
          sSessionFolderDriveId: null,
          sNotes: null,
          sInputListRows: [],
          sStatus: 'InPrep',
        },
      ],
      page: 1,
      pageSize: 10,
      total: 1,
    });
    mockRoomsList.mockResolvedValue([
      { roomId: 'room-a', rName: 'Sala A' },
    ]);

    renderPage();

    expect(await screen.findByText('Sesiones')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primera sesión registrada.')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Revísala aquí; cuando exista la segunda, volverán la tabla, la edición rápida y la paginación para coordinar varias sesiones\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText(/Servicio:\s*Grabación vocal/i)).toBeInTheDocument();
      expect(screen.getByText(/Booking:\s*BK-001/i)).toBeInTheDocument();
      expect(screen.getByText(/Ingeniero:\s*ada@tdf/i)).toBeInTheDocument();
      expect(screen.getByText(/Salas:\s*Sala A/i)).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Editar sesión/i })).toBeInTheDocument();
      expect(screen.getByText('En preparación')).toBeInTheDocument();
    });

    expect(screen.queryByRole('table')).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Horario$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Servicio$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Booking$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Ingeniero$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Salas$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acciones$/i })).not.toBeInTheDocument();
    expect(screen.queryByText(/Rows per page/i)).not.toBeInTheDocument();
  });

  it('hides empty optional metadata rows in the single-session summary so first-run stays focused on real context', async () => {
    mockSessionsList.mockResolvedValue({
      items: [
        {
          sessionId: 'session-1',
          sBookingRef: null,
          sBandId: null,
          sClientPartyRef: null,
          sService: 'Producción',
          sStartAt: '2026-04-15T15:00:00.000Z',
          sEndAt: '2026-04-15T17:00:00.000Z',
          sEngineerRef: '   ',
          sAssistantRef: null,
          sRoomIds: [],
          sSampleRate: null,
          sBitDepth: null,
          sDaw: null,
          sSessionFolderDriveId: null,
          sNotes: null,
          sInputListRows: [],
          sStatus: 'InPrep',
        },
      ],
      page: 1,
      pageSize: 10,
      total: 1,
    });

    renderPage();

    expect(await screen.findByText('Sesiones')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primera sesión registrada.')).toBeInTheDocument();
      expect(screen.getByText(/Servicio:\s*Producción/i)).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Editar sesión/i })).toBeInTheDocument();
    });

    expect(screen.queryByText(/Booking:/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Ingeniero:/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Salas:/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^—$/i)).not.toBeInTheDocument();
  });

  it('hides empty optional schedule columns once multiple sessions exist', async () => {
    mockSessionsList.mockResolvedValue({
      items: [
        {
          sessionId: 'session-1',
          sBookingRef: null,
          sBandId: null,
          sClientPartyRef: null,
          sService: 'Producción',
          sStartAt: '2026-04-15T15:00:00.000Z',
          sEndAt: '2026-04-15T17:00:00.000Z',
          sEngineerRef: '   ',
          sAssistantRef: null,
          sRoomIds: [],
          sSampleRate: null,
          sBitDepth: null,
          sDaw: null,
          sSessionFolderDriveId: null,
          sNotes: null,
          sInputListRows: [],
          sStatus: 'InPrep',
        },
        {
          sessionId: 'session-2',
          sBookingRef: '',
          sBandId: null,
          sClientPartyRef: null,
          sService: 'Ensayo',
          sStartAt: '2026-04-16T15:00:00.000Z',
          sEndAt: '2026-04-16T17:00:00.000Z',
          sEngineerRef: null,
          sAssistantRef: null,
          sRoomIds: [],
          sSampleRate: null,
          sBitDepth: null,
          sDaw: null,
          sSessionFolderDriveId: null,
          sNotes: null,
          sInputListRows: [],
          sStatus: 'InSession',
        },
      ],
      page: 1,
      pageSize: 10,
      total: 2,
    });

    renderPage();

    expect(await screen.findByText('Sesiones')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByRole('table')).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Horario$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Servicio$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Estado$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Acciones$/i })).toBeInTheDocument();
      expect(screen.getByText('Producción')).toBeInTheDocument();
      expect(screen.getByText('Ensayo')).toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /^Booking$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Ingeniero$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Salas$/i })).not.toBeInTheDocument();
    expect(screen.queryByText(/^—$/i)).not.toBeInTheDocument();
  });
});
