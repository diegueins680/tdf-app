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

const getButtonByAriaLabel = (root: ParentNode, labelText: string) => {
  const button = root.querySelector(`button[aria-label="${labelText}"]`);
  if (!(button instanceof HTMLButtonElement)) {
    throw new Error(`Button not found: ${labelText}`);
  }
  return button;
};

const getMenuItemByText = (root: ParentNode, labelText: string) => {
  const item = Array.from(root.querySelectorAll('[role="menuitem"]')).find(
    (element) => buttonText(element) === labelText,
  );
  if (!(item instanceof HTMLElement)) {
    throw new Error(`Menu item not found: ${labelText}`);
  }
  return item;
};

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

  it('keeps one actions trigger per row and moves recording-only actions into the overflow menu', async () => {
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
          'Visualiza órdenes de estudio, su horario, recursos asignados y estado operacional. Usa Acciones para editar la sesión o abrir flujos específicos como Live Sessions.',
        );
        expect(container.querySelectorAll('button[aria-label^="Abrir acciones para sesión "]')).toHaveLength(2);
        expect(Array.from(container.querySelectorAll('button')).map(buttonText)).not.toContain('Editar');
        expect(Array.from(container.querySelectorAll('button')).map(buttonText)).not.toContain('Crear input list');
      });

      await clickButton(getButtonByAriaLabel(container, 'Abrir acciones para sesión 101'));

      await waitForExpectation(() => {
        expect(getMenuItemByText(document.body, 'Editar sesión')).toBeTruthy();
        expect(getMenuItemByText(document.body, 'Abrir Live Sessions')).toBeTruthy();
      });

      await clickButton(getButtonByAriaLabel(container, 'Abrir acciones para sesión 102'));

      await waitForExpectation(() => {
        expect(getMenuItemByText(document.body, 'Editar sesión')).toBeTruthy();
        expect(
          Array.from(document.body.querySelectorAll('[role="menuitem"]')).some(
            (element) => buttonText(element) === 'Abrir Live Sessions',
          ),
        ).toBe(false);
      });
    } finally {
      await cleanup();
    }
  });
});
