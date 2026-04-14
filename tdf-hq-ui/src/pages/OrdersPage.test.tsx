import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { BookingDTO, PartyDTO } from '../api/types';

const listBookingsMock = jest.fn<() => Promise<BookingDTO[]>>();
const listPartiesMock = jest.fn<() => Promise<PartyDTO[]>>();
const updateBookingMock = jest.fn<(id: number, payload: unknown) => Promise<BookingDTO>>();

jest.unstable_mockModule('../api/bookings', () => ({
  Bookings: {
    list: () => listBookingsMock(),
    update: (id: number, payload: unknown) => updateBookingMock(id, payload),
  },
}));

jest.unstable_mockModule('../api/parties', () => ({
  Parties: {
    list: () => listPartiesMock(),
  },
}));

const { default: OrdersPage } = await import('./OrdersPage');

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

const buttonText = (element: Element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim();

const clickButton = async (button: HTMLElement) => {
  await act(async () => {
    button.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
    await flushPromises();
  });
};

const renderPage = async (container: HTMLElement) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter future={{ v7_startTransition: true, v7_relativeSplatPath: true }}>
        <QueryClientProvider client={qc}>
          <OrdersPage />
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

const hasTableHeader = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('th')).some((cell) => (cell.textContent ?? '').trim() === labelText);

const getRowByBookingId = (root: ParentNode, bookingId: number) => {
  const row = root.querySelector<HTMLElement>(`[data-testid="orders-row-${bookingId}"]`);
  if (!(row instanceof HTMLElement)) {
    throw new Error(`Row not found: ${bookingId}`);
  }
  return row;
};

describe('OrdersPage', () => {
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
    listBookingsMock.mockReset();
    listPartiesMock.mockReset();
    updateBookingMock.mockReset();
    listPartiesMock.mockResolvedValue([]);
    updateBookingMock.mockResolvedValue({} as BookingDTO);
  });

  it('replaces the empty sessions table with first-run guidance instead of table chrome', async () => {
    listBookingsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Primeras sesiones');
        expect(container.textContent).toContain(
          'Todavía no hay sesiones registradas. Usa Nueva sesión para cargar la primera y volver a esta vista cuando necesites revisar horario, servicio, booking, recursos y estado en una sola tabla.',
        );
        expect(container.textContent).toContain(
          'La tabla y la paginación aparecerán cuando exista al menos una sesión para comparar.',
        );
        expect(container.querySelector('table')).toBeNull();
        expect(hasTableHeader(container, 'Horario')).toBe(false);
        expect(hasTableHeader(container, 'Acciones')).toBe(false);
        expect(container.querySelector('button[aria-label="Actualizar lista de sesiones"]')).toBeNull();
        expect(container.textContent).not.toContain('No hay sesiones registradas todavía.');
        expect(container.textContent).not.toContain('Rows per page');
        expect(
          Array.from(container.querySelectorAll('button')).filter(
            (button) => buttonText(button) === 'Nueva sesión',
          ),
        ).toHaveLength(1);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the first loading pass focused on setup instead of table chrome and refresh controls', async () => {
    listBookingsMock.mockImplementation(() => new Promise(() => {}));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Cargando sesiones…');
        expect(container.textContent).toContain(
          'La tabla aparecerá cuando termine esta primera carga para que puedas comparar horario, servicio, booking, recursos y estado desde una sola vista.',
        );
        expect(container.querySelector('table')).toBeNull();
        expect(hasTableHeader(container, 'Horario')).toBe(false);
        expect(hasTableHeader(container, 'Acciones')).toBe(false);
        expect(container.querySelector('button[aria-label="Actualizar lista de sesiones"]')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('uses row click for editing and only shows the Live Sessions shortcut on recording rows', async () => {
    listBookingsMock.mockResolvedValue([
      {
        bookingId: 101,
        title: 'Tracking principal',
        startsAt: '2026-04-13T10:00:00-05:00',
        endsAt: '2026-04-13T12:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Grabación de banda',
        serviceOrderId: 88,
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-1', brRoomName: 'Vale', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
      {
        bookingId: 102,
        title: 'Mezcla EP',
        startsAt: '2026-04-14T14:00:00-05:00',
        endsAt: '2026-04-14T16:00:00-05:00',
        status: 'Tentative',
        serviceType: 'Mixing',
        resources: [
          { brRoomId: 'studio-b', brRoomName: 'Studio B', brRole: 'room' },
        ],
      } satisfies BookingDTO,
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Haz clic en una fila para editar la sesión. Live Sessions aparece solo en sesiones de grabación.',
        );
        expect(container.querySelector('button[aria-label="Actualizar lista de sesiones"]')).not.toBeNull();
        expect(container.textContent).not.toContain('Filas por página');
        expect(container.textContent).not.toContain('Rows per page');
        expect(hasTableHeader(container, 'Acciones')).toBe(false);
        expect(hasTableHeader(container, 'Live Sessions')).toBe(true);
        expect(container.querySelectorAll('button[aria-label^="Abrir Live Sessions para sesión "]')).toHaveLength(1);
        expect(Array.from(container.querySelectorAll('button')).map(buttonText)).not.toContain('Editar');
        expect(Array.from(container.querySelectorAll('button')).map(buttonText)).not.toContain('Crear input list');
        expect(container.querySelector('button[aria-label="Abrir Live Sessions para sesión 102"]')).toBeNull();
      });

      await clickButton(getRowByBookingId(container, 102));

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Editar sesión #102');
        expect(document.body.textContent).not.toContain('Editar sesión #101');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps pagination hidden until the sessions list actually needs a second page and localizes it when it does', async () => {
    listBookingsMock.mockResolvedValue(
      Array.from({ length: 11 }, (_, index) => ({
        bookingId: 301 + index,
        title: `Sesion ${index + 1}`,
        startsAt: `2026-04-${String((index % 9) + 10).padStart(2, '0')}T10:00:00-05:00`,
        endsAt: `2026-04-${String((index % 9) + 10).padStart(2, '0')}T11:00:00-05:00`,
        status: index % 2 === 0 ? 'Confirmed' : 'Tentative',
        serviceType: 'Mixing',
        resources: [
          { brRoomId: `studio-${index + 1}`, brRoomName: `Studio ${index + 1}`, brRole: 'room' },
        ],
      } satisfies BookingDTO)),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Filas por página');
        expect(container.textContent).toContain('1-10 de 11');
        expect(container.textContent).not.toContain('Rows per page');
      });
    } finally {
      await cleanup();
    }
  });

  it('hides the extra Live Sessions column when no visible row can use it', async () => {
    listBookingsMock.mockResolvedValue([
      {
        bookingId: 201,
        title: 'Mezcla EP',
        startsAt: '2026-04-14T14:00:00-05:00',
        endsAt: '2026-04-14T16:00:00-05:00',
        status: 'Tentative',
        serviceType: 'Mixing',
        resources: [
          { brRoomId: 'studio-b', brRoomName: 'Studio B', brRole: 'room' },
        ],
      } satisfies BookingDTO,
      {
        bookingId: 202,
        title: 'Master final',
        startsAt: '2026-04-15T11:00:00-05:00',
        endsAt: '2026-04-15T13:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Mastering',
        resources: [
          { brRoomId: 'studio-c', brRoomName: 'Studio C', brRole: 'room' },
        ],
      } satisfies BookingDTO,
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Haz clic en una fila para editar la sesión y revisar horario, servicio, recursos y estado.',
        );
        expect(hasTableHeader(container, 'Acciones')).toBe(false);
        expect(hasTableHeader(container, 'Live Sessions')).toBe(false);
        expect(container.querySelectorAll('button[aria-label^="Abrir Live Sessions para sesión "]')).toHaveLength(0);
      });
    } finally {
      await cleanup();
    }
  });
});
