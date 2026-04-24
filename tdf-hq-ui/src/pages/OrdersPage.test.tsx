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

const countOccurrencesIgnoringCase = (value: string, fragment: string) =>
  value.toLocaleLowerCase().split(fragment.toLocaleLowerCase()).length - 1;

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
        expect(container.textContent).toContain(
          'Revisa horario, servicio, booking, recursos y estado desde una sola tabla.',
        );
        expect(container.textContent).toContain('Primeras sesiones');
        expect(container.textContent).toContain(
          'Todavía no hay sesiones registradas. Usa Nueva sesión para cargar la primera y volver a esta vista cuando necesites revisar horario, servicio, booking, recursos y estado en una sola tabla.',
        );
        expect(container.textContent).toContain(
          'La tabla y la paginación aparecerán cuando exista al menos una sesión para comparar.',
        );
        expect(countOccurrencesIgnoringCase(container.textContent ?? '', 'Usa Nueva sesión')).toBe(1);
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

  it('replaces the one-row sessions table with a first-session summary and one edit action', async () => {
    listBookingsMock.mockResolvedValue([
      {
        bookingId: 101,
        title: 'Tracking principal',
        startsAt: '2026-04-13T10:00:00-05:00',
        endsAt: '2026-04-13T12:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Mixing',
        serviceOrderId: 88,
        customerName: 'Ada Sessions',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-1', brRoomName: 'Vale', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Revisa la primera sesión desde un resumen simple. La tabla volverá cuando exista una segunda.',
        );
        expect(container.textContent).toContain('Primera sesión registrada');
        expect(container.textContent).toContain(
          'Revísala aquí sin tabla ni paginación. Cuando exista la segunda, volverá la vista comparativa para revisar horario, servicio, booking, recursos y estado lado a lado.',
        );
        expect(container.textContent).toContain('Horario:');
        expect(container.textContent).toContain('Servicio: Mixing');
        expect(container.textContent).toContain('Booking: Ada Sessions');
        expect(container.textContent).toContain('Detalle: SO #88');
        expect(container.textContent).toContain('Ingeniero: Vale');
        expect(container.textContent).toContain('Salas: Studio A');
        expect(container.textContent).toContain('Confirmada');
        expect(container.querySelector('table')).toBeNull();
        expect(hasTableHeader(container, 'Horario')).toBe(false);
        expect(hasTableHeader(container, 'Live Sessions')).toBe(false);
        expect(container.textContent).not.toContain('Filas por página');
        expect(container.textContent).not.toContain('Rows per page');
        expect(
          Array.from(container.querySelectorAll('button')).filter(
            (button) => buttonText(button) === 'Editar sesión',
          ),
        ).toHaveLength(1);
      });
    } finally {
      await cleanup();
    }
  });

  it('hides empty engineer and room placeholders in the first-session summary so setup stays focused on real context', async () => {
    listBookingsMock.mockResolvedValue([
      {
        bookingId: 111,
        title: 'Sesión sin recursos',
        startsAt: '2026-04-13T10:00:00-05:00',
        endsAt: '2026-04-13T12:00:00-05:00',
        status: 'Tentative',
        serviceType: 'Mixing',
        customerName: 'Ada Sessions',
        resources: [],
      } satisfies BookingDTO,
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Primera sesión registrada');
        expect(container.textContent).toContain('Servicio: Mixing');
        expect(container.textContent).toContain('Booking: Ada Sessions');
        expect(container.textContent).not.toContain('Ingeniero:');
        expect(container.textContent).not.toContain('Salas:');
        expect(container.textContent).not.toContain('Ingeniero: —');
        expect(container.textContent).not.toContain('Salas: —');
      });
    } finally {
      await cleanup();
    }
  });

  it('uses the session title as booking context in the first-session summary before falling back to a generic id', async () => {
    listBookingsMock.mockResolvedValue([
      {
        bookingId: 112,
        title: 'Tracking principal',
        startsAt: '2026-04-13T10:00:00-05:00',
        endsAt: '2026-04-13T12:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Mixing',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
        ],
      } satisfies BookingDTO,
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const summaryText = container.textContent ?? '';

        expect(summaryText).toContain('Primera sesión registrada');
        expect(summaryText).toContain('Servicio: Mixing');
        expect(summaryText).toContain('Booking: Tracking principal');
        expect(summaryText).not.toContain('Booking #112');
        expect(summaryText).not.toContain('Sin booking asignado');
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

  it('summarizes one shared room once and restores the room column when sessions diverge again', async () => {
    listBookingsMock.mockResolvedValue([
      {
        bookingId: 151,
        title: 'Tracking principal',
        startsAt: '2026-04-13T10:00:00-05:00',
        endsAt: '2026-04-13T12:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Grabación',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-1', brRoomName: 'Vale', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
      {
        bookingId: 152,
        title: 'Edición vocal',
        startsAt: '2026-04-14T14:00:00-05:00',
        endsAt: '2026-04-14T16:00:00-05:00',
        status: 'Tentative',
        serviceType: 'Edición',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-2', brRoomName: 'Nina', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const text = container.textContent ?? '';
        expect(text).toContain(
          'Mostrando una sola sala: Studio A. La columna volverá cuando esta vista mezcle salas distintas.',
        );
        expect(countOccurrencesIgnoringCase(text, 'Mostrando una sola sala: Studio A.')).toBe(1);
        expect(countOccurrencesIgnoringCase(text, 'Studio A')).toBe(1);
        expect(hasTableHeader(container, 'Salas')).toBe(false);
        expect(text).toContain('Tracking principal');
        expect(text).toContain('Edición vocal');
        expect(text).not.toContain('Booking #151');
        expect(text).not.toContain('Booking #152');
      });
    } finally {
      await cleanup();
    }

    listBookingsMock.mockResolvedValue([
      {
        bookingId: 151,
        title: 'Tracking principal',
        startsAt: '2026-04-13T10:00:00-05:00',
        endsAt: '2026-04-13T12:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Grabación',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-1', brRoomName: 'Vale', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
      {
        bookingId: 152,
        title: 'Edición vocal',
        startsAt: '2026-04-14T14:00:00-05:00',
        endsAt: '2026-04-14T16:00:00-05:00',
        status: 'Tentative',
        serviceType: 'Edición',
        resources: [
          { brRoomId: 'studio-b', brRoomName: 'Studio B', brRole: 'room' },
          { brRoomId: 'eng-2', brRoomName: 'Nina', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
    ]);

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    try {
      await waitForExpectation(() => {
        const text = secondContainer.textContent ?? '';
        expect(text).not.toContain('Mostrando una sola sala:');
        expect(hasTableHeader(secondContainer, 'Salas')).toBe(true);
        expect(text).toContain('Studio A');
        expect(text).toContain('Studio B');
      });
    } finally {
      await secondRender.cleanup();
    }
  });

  it('summarizes one shared engineer once and restores the engineer column when sessions diverge again', async () => {
    listBookingsMock.mockResolvedValue([
      {
        bookingId: 156,
        title: 'Tracking principal',
        startsAt: '2026-04-13T10:00:00-05:00',
        endsAt: '2026-04-13T12:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Grabación',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-1', brRoomName: 'Vale', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
      {
        bookingId: 157,
        title: 'Edición vocal',
        startsAt: '2026-04-14T14:00:00-05:00',
        endsAt: '2026-04-14T16:00:00-05:00',
        status: 'Tentative',
        serviceType: 'Edición',
        resources: [
          { brRoomId: 'studio-b', brRoomName: 'Studio B', brRole: 'room' },
          { brRoomId: 'eng-1', brRoomName: 'Vale', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const text = container.textContent ?? '';
        expect(text).toContain(
          'Mostrando un solo ingeniero: Vale. La columna volverá cuando esta vista mezcle ingenieros distintos.',
        );
        expect(countOccurrencesIgnoringCase(text, 'Mostrando un solo ingeniero: Vale.')).toBe(1);
        expect(countOccurrencesIgnoringCase(text, 'Vale')).toBe(1);
        expect(hasTableHeader(container, 'Ingeniero')).toBe(false);
        expect(text).toContain('Tracking principal');
        expect(text).toContain('Edición vocal');
        expect(text).not.toContain('Booking #156');
        expect(text).not.toContain('Booking #157');
      });
    } finally {
      await cleanup();
    }

    listBookingsMock.mockResolvedValue([
      {
        bookingId: 156,
        title: 'Tracking principal',
        startsAt: '2026-04-13T10:00:00-05:00',
        endsAt: '2026-04-13T12:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Grabación',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-1', brRoomName: 'Vale', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
      {
        bookingId: 157,
        title: 'Edición vocal',
        startsAt: '2026-04-14T14:00:00-05:00',
        endsAt: '2026-04-14T16:00:00-05:00',
        status: 'Tentative',
        serviceType: 'Edición',
        resources: [
          { brRoomId: 'studio-b', brRoomName: 'Studio B', brRole: 'room' },
          { brRoomId: 'eng-2', brRoomName: 'Nina', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
    ]);

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    try {
      await waitForExpectation(() => {
        const text = secondContainer.textContent ?? '';
        expect(text).not.toContain('Mostrando un solo ingeniero:');
        expect(hasTableHeader(secondContainer, 'Ingeniero')).toBe(true);
        expect(text).toContain('Vale');
        expect(text).toContain('Nina');
      });
    } finally {
      await secondRender.cleanup();
    }
  });

  it('summarizes one shared service once and restores the service column when sessions diverge again', async () => {
    listBookingsMock.mockResolvedValue([
      {
        bookingId: 161,
        title: 'Voz principal',
        startsAt: '2026-04-13T10:00:00-05:00',
        endsAt: '2026-04-13T12:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Mixing',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-1', brRoomName: 'Vale', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
      {
        bookingId: 162,
        title: 'Coros finales',
        startsAt: '2026-04-14T14:00:00-05:00',
        endsAt: '2026-04-14T16:00:00-05:00',
        status: 'Tentative',
        serviceType: 'Mixing',
        resources: [
          { brRoomId: 'studio-b', brRoomName: 'Studio B', brRole: 'room' },
          { brRoomId: 'eng-2', brRoomName: 'Nina', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const text = container.textContent ?? '';
        expect(text).toContain(
          'Mostrando un solo servicio: Mixing. La columna volverá cuando esta vista mezcle servicios distintos.',
        );
        expect(countOccurrencesIgnoringCase(text, 'Mostrando un solo servicio: Mixing.')).toBe(1);
        expect(countOccurrencesIgnoringCase(text, 'Mixing')).toBe(1);
        expect(hasTableHeader(container, 'Servicio')).toBe(false);
        expect(text).toContain('Voz principal');
        expect(text).toContain('Coros finales');
        expect(text).not.toContain('Booking #161');
        expect(text).not.toContain('Booking #162');
      });
    } finally {
      await cleanup();
    }

    listBookingsMock.mockResolvedValue([
      {
        bookingId: 161,
        title: 'Voz principal',
        startsAt: '2026-04-13T10:00:00-05:00',
        endsAt: '2026-04-13T12:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Mixing',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-1', brRoomName: 'Vale', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
      {
        bookingId: 162,
        title: 'Coros finales',
        startsAt: '2026-04-14T14:00:00-05:00',
        endsAt: '2026-04-14T16:00:00-05:00',
        status: 'Tentative',
        serviceType: 'Mastering',
        resources: [
          { brRoomId: 'studio-b', brRoomName: 'Studio B', brRole: 'room' },
          { brRoomId: 'eng-2', brRoomName: 'Nina', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
    ]);

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    try {
      await waitForExpectation(() => {
        const text = secondContainer.textContent ?? '';
        expect(text).not.toContain('Mostrando un solo servicio:');
        expect(hasTableHeader(secondContainer, 'Servicio')).toBe(true);
        expect(text).toContain('Mixing');
        expect(text).toContain('Mastering');
      });
    } finally {
      await secondRender.cleanup();
    }
  });

  it('summarizes one shared status once and restores the status column when sessions diverge again', async () => {
    listBookingsMock.mockResolvedValue([
      {
        bookingId: 166,
        title: 'Tracking principal',
        startsAt: '2026-04-13T10:00:00-05:00',
        endsAt: '2026-04-13T12:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Grabación',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-1', brRoomName: 'Vale', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
      {
        bookingId: 167,
        title: 'Edición vocal',
        startsAt: '2026-04-14T14:00:00-05:00',
        endsAt: '2026-04-14T16:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Edición',
        resources: [
          { brRoomId: 'studio-b', brRoomName: 'Studio B', brRole: 'room' },
          { brRoomId: 'eng-2', brRoomName: 'Nina', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const text = container.textContent ?? '';
        expect(text).toContain(
          'Mostrando un solo estado: Confirmada. La columna volverá cuando esta vista mezcle estados distintos.',
        );
        expect(countOccurrencesIgnoringCase(text, 'Mostrando un solo estado: Confirmada.')).toBe(1);
        expect(countOccurrencesIgnoringCase(text, 'Confirmada')).toBe(1);
        expect(hasTableHeader(container, 'Estado')).toBe(false);
        expect(text).toContain('Tracking principal');
        expect(text).toContain('Edición vocal');
      });
    } finally {
      await cleanup();
    }

    listBookingsMock.mockResolvedValue([
      {
        bookingId: 166,
        title: 'Tracking principal',
        startsAt: '2026-04-13T10:00:00-05:00',
        endsAt: '2026-04-13T12:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Grabación',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-1', brRoomName: 'Vale', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
      {
        bookingId: 167,
        title: 'Edición vocal',
        startsAt: '2026-04-14T14:00:00-05:00',
        endsAt: '2026-04-14T16:00:00-05:00',
        status: 'Tentative',
        serviceType: 'Edición',
        resources: [
          { brRoomId: 'studio-b', brRoomName: 'Studio B', brRole: 'room' },
          { brRoomId: 'eng-2', brRoomName: 'Nina', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
    ]);

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    try {
      await waitForExpectation(() => {
        const text = secondContainer.textContent ?? '';
        expect(text).not.toContain('Mostrando un solo estado:');
        expect(hasTableHeader(secondContainer, 'Estado')).toBe(true);
        expect(text).toContain('Confirmada');
        expect(text).toContain('Tentativa');
      });
    } finally {
      await secondRender.cleanup();
    }
  });

  it('combines shared service and room context into one helper line when both columns are unnecessary', async () => {
    listBookingsMock.mockResolvedValue([
      {
        bookingId: 171,
        title: 'Voz principal',
        startsAt: '2026-04-13T10:00:00-05:00',
        endsAt: '2026-04-13T12:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Mixing',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-1', brRoomName: 'Vale', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
      {
        bookingId: 172,
        title: 'Coros finales',
        startsAt: '2026-04-14T14:00:00-05:00',
        endsAt: '2026-04-14T16:00:00-05:00',
        status: 'Tentative',
        serviceType: 'Mixing',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-2', brRoomName: 'Nina', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const text = container.textContent ?? '';
        expect(text).toContain(
          'Mostrando un solo servicio: Mixing y una sola sala: Studio A. Las columnas volverán cuando ya no coincidan servicios o salas.',
        );
        expect(text).not.toContain('Mostrando un solo servicio: Mixing. La columna volverá cuando esta vista mezcle servicios distintos.');
        expect(text).not.toContain('Mostrando una sola sala: Studio A. La columna volverá cuando esta vista mezcle salas distintas.');
        expect(
          countOccurrencesIgnoringCase(
            text,
            'Mostrando un solo servicio: Mixing y una sola sala: Studio A. Las columnas volverán cuando ya no coincidan servicios o salas.',
          ),
        ).toBe(1);
        expect(hasTableHeader(container, 'Servicio')).toBe(false);
        expect(hasTableHeader(container, 'Salas')).toBe(false);
        expect(text).toContain('Voz principal');
        expect(text).toContain('Coros finales');
        expect(text).not.toContain('Booking #171');
        expect(text).not.toContain('Booking #172');
      });
    } finally {
      await cleanup();
    }
  });

  it('combines every shared column summary into one helper line so dense views do not stack repeated guidance', async () => {
    listBookingsMock.mockResolvedValue([
      {
        bookingId: 181,
        title: 'Tracking principal',
        startsAt: '2026-04-13T10:00:00-05:00',
        endsAt: '2026-04-13T12:00:00-05:00',
        status: 'Confirmed',
        serviceType: 'Mixing',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-1', brRoomName: 'Vale', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
      {
        bookingId: 182,
        title: 'Coros finales',
        startsAt: '2026-04-14T14:00:00-05:00',
        endsAt: '2026-04-14T16:00:00-05:00',
        status: 'Tentative',
        serviceType: 'Mixing',
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
          { brRoomId: 'eng-1', brRoomName: 'Vale', brRole: 'engineer' },
        ],
      } satisfies BookingDTO,
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const text = container.textContent ?? '';
        expect(text).toContain(
          'Mostrando un solo servicio: Mixing, un solo ingeniero: Vale y una sola sala: Studio A. Las columnas volverán cuando ya no coincidan servicios, ingenieros o salas.',
        );
        expect(text).not.toContain('Mostrando un solo servicio: Mixing. La columna volverá cuando esta vista mezcle servicios distintos.');
        expect(text).not.toContain('Mostrando un solo ingeniero: Vale. La columna volverá cuando esta vista mezcle ingenieros distintos.');
        expect(text).not.toContain('Mostrando una sola sala: Studio A. La columna volverá cuando esta vista mezcle salas distintas.');
        expect(
          countOccurrencesIgnoringCase(
            text,
            'Mostrando un solo servicio: Mixing, un solo ingeniero: Vale y una sola sala: Studio A. Las columnas volverán cuando ya no coincidan servicios, ingenieros o salas.',
          ),
        ).toBe(1);
        expect(hasTableHeader(container, 'Servicio')).toBe(false);
        expect(hasTableHeader(container, 'Ingeniero')).toBe(false);
        expect(hasTableHeader(container, 'Salas')).toBe(false);
        expect(text).toContain('Tracking principal');
        expect(text).toContain('Coros finales');
        expect(text).not.toContain('Booking #181');
        expect(text).not.toContain('Booking #182');
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

  it('removes repeated booking identity from the first-session summary so the booking context only appears once', async () => {
    listBookingsMock.mockResolvedValue([
      {
        bookingId: 401,
        title: 'Sesion de voz',
        startsAt: '2026-04-16T09:00:00-05:00',
        endsAt: '2026-04-16T10:00:00-05:00',
        status: 'Confirmed',
        customerName: 'Ada Client',
        partyDisplayName: 'ada client',
        serviceOrderId: 88,
        resources: [
          { brRoomId: 'studio-a', brRoomName: 'Studio A', brRole: 'room' },
        ],
      } satisfies BookingDTO,
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const summaryText = container.textContent ?? '';

        expect(summaryText).toContain('Primera sesión registrada');
        expect(summaryText).toContain('Booking: Ada Client');
        expect(summaryText).toContain('Detalle: SO #88');
        expect(countOccurrencesIgnoringCase(summaryText, 'Ada Client')).toBe(1);
      });
    } finally {
      await cleanup();
    }
  });
});
