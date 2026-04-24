import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { render, screen, waitFor } from '@testing-library/react';
import { vi } from 'vitest';
import BookingsPage from '../BookingsPage';

const mockBookingsList = vi.fn();
const mockBookingsCreate = vi.fn();
const mockPartiesListByRole = vi.fn();
const calendarConstructorMock = vi.fn();
const calendarRenderMock = vi.fn();
const calendarDestroyMock = vi.fn();

vi.mock('../../api/bookings', () => ({
  Bookings: {
    list: () => mockBookingsList(),
    create: (body: unknown) => mockBookingsCreate(body),
  },
}));

vi.mock('../../api/parties', () => ({
  Parties: {
    listByRole: (role: string) => mockPartiesListByRole(role),
  },
}));

vi.mock('@fullcalendar/core', () => ({
  Calendar: class MockCalendar {
    constructor(...args: unknown[]) {
      calendarConstructorMock(...args);
    }

    render() {
      calendarRenderMock();
    }

    destroy() {
      calendarDestroyMock();
    }
  },
}));

vi.mock('@fullcalendar/daygrid', () => ({ default: {} }));
vi.mock('@fullcalendar/timegrid', () => ({ default: {} }));
vi.mock('@fullcalendar/interaction', () => ({ default: {} }));

function createQueryClient() {
  return new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });
}

function renderPage() {
  const queryClient = createQueryClient();

  return render(
    <QueryClientProvider client={queryClient}>
      <BookingsPage />
    </QueryClientProvider>,
  );
}

describe('BookingsPage', () => {
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
    mockBookingsList.mockResolvedValue([]);
    mockBookingsCreate.mockResolvedValue(undefined);
    mockPartiesListByRole.mockResolvedValue([]);
  });

  it('shows one explicit loading state while reservations are still loading', async () => {
    mockBookingsList.mockImplementation(() => new Promise(() => undefined));

    renderPage();

    expect(await screen.findByText('Agenda')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText(/Cargando agenda y reservas…/i)).toBeInTheDocument();
      expect(screen.getByText('Cargando agenda…')).toBeInTheDocument();
    });

    expect(calendarConstructorMock).not.toHaveBeenCalled();
    expect(calendarRenderMock).not.toHaveBeenCalled();
  });

  it('replaces the empty calendar with one first-step action until the first booking exists', async () => {
    renderPage();

    expect(await screen.findByText('Agenda')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByRole('button', { name: /Nueva reserva/i })).toBeInTheDocument();
      expect(screen.getByText('Todavía no hay reservas registradas.')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Empieza con Nueva reserva\. El calendario aparecerá cuando exista la primera reserva\./i,
        ),
      ).toBeInTheDocument();
      expect(
        screen.getByText(
          /Usa Nueva reserva para cargar la primera\. Cuando exista al menos una, aquí podrás revisar la semana completa y seleccionar nuevos horarios desde el calendario\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(calendarConstructorMock).not.toHaveBeenCalled();
    expect(calendarRenderMock).not.toHaveBeenCalled();
  });

  it('keeps the calendar available once at least one booking exists', async () => {
    mockBookingsList.mockResolvedValue([
      {
        bookingId: 1,
        title: 'Grabación vocal',
        startsAt: '2026-04-24T15:00:00.000Z',
        endsAt: '2026-04-24T16:00:00.000Z',
        status: 'Confirmed',
        notes: null,
        partyId: null,
        serviceOrderId: null,
        serviceType: null,
        resources: [],
      },
    ]);

    renderPage();

    expect(await screen.findByText('Agenda')).toBeInTheDocument();

    await waitFor(() => {
      expect(calendarConstructorMock).toHaveBeenCalledTimes(1);
      expect(calendarRenderMock).toHaveBeenCalledTimes(1);
      expect(
        screen.getByText(
          /Selecciona un bloque del calendario o usa Nueva reserva para registrar un horario manualmente\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByText('Todavía no hay reservas registradas.')).not.toBeInTheDocument();
  });
});
