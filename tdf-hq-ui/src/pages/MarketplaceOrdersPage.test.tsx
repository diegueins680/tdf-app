import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { MarketplaceOrderDTO, MarketplaceOrderUpdatePayload } from '../api/types';

const listOrdersMock = jest.fn<(params?: { status?: string; limit?: number; offset?: number }) => Promise<MarketplaceOrderDTO[]>>();
const updateOrderMock = jest.fn<
  (orderId: string, payload: MarketplaceOrderUpdatePayload) => Promise<MarketplaceOrderDTO>
>();

jest.unstable_mockModule('../api/marketplace', () => ({
  Marketplace: {
    listOrders: (params?: { status?: string; limit?: number; offset?: number }) => listOrdersMock(params),
    updateOrder: (orderId: string, payload: MarketplaceOrderUpdatePayload) => updateOrderMock(orderId, payload),
  },
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({
    session: {
      username: 'admin',
      displayName: 'Admin',
      roles: ['Admin'],
      modules: ['admin'],
    },
  }),
}));

const { default: MarketplaceOrdersPage } = await import('./MarketplaceOrdersPage');

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

const buildOrder = (overrides: Partial<MarketplaceOrderDTO> = {}): MarketplaceOrderDTO => ({
  moOrderId: 'order-1',
  moCartId: 'cart-1',
  moCurrency: 'usd',
  moTotalUsdCents: 10000,
  moTotalDisplay: 'USD $100.00',
  moStatus: 'pending',
  moStatusHistory: [],
  moBuyerName: 'Ada Lovelace',
  moBuyerEmail: 'ada@example.com',
  moBuyerPhone: '+593999000111',
  moPaymentProvider: 'paypal',
  moPaypalOrderId: null,
  moPaypalPayerEmail: null,
  moPaidAt: null,
  moCreatedAt: '2030-01-01T12:00:00.000Z',
  moUpdatedAt: '2030-01-01T12:00:00.000Z',
  moItems: [
    {
      moiListingId: 'listing-1',
      moiTitle: 'Vintage Mic',
      moiQuantity: 1,
      moiUnitPriceUsdCents: 10000,
      moiSubtotalCents: 10000,
      moiUnitPriceDisplay: 'USD $100.00',
      moiSubtotalDisplay: 'USD $100.00',
    },
  ],
  ...overrides,
});

const renderPage = async (container: HTMLElement) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter
        initialEntries={['/marketplace/ordenes']}
        future={{ v7_startTransition: true, v7_relativeSplatPath: true }}
      >
        <QueryClientProvider client={qc}>
          <MarketplaceOrdersPage />
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

const countLabelsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('label')).filter((element) => {
    const text = (element.textContent ?? '').replace('*', '').trim();
    return text === labelText;
  }).length;

const queryActionByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll<HTMLElement>('button, a')).find(
    (element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim() === labelText,
  ) ?? null;

const getInputByLabel = (root: ParentNode, labelText: string) => {
  const label = Array.from(root.querySelectorAll<HTMLLabelElement>('label')).find(
    (element) => (element.textContent ?? '').replace('*', '').trim() === labelText,
  );
  if (!label) throw new Error(`Label not found: ${labelText}`);

  const inputId = label.htmlFor;
  if (!inputId) throw new Error(`Label has no associated control: ${labelText}`);

  const input = label.ownerDocument.getElementById(inputId);
  if (!(input instanceof HTMLInputElement)) throw new Error(`Input not found for label: ${labelText}`);

  return input;
};

const setInputValue = async (input: HTMLInputElement, value: string) => {
  const valueSetter = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value')?.set;
  if (!valueSetter) throw new Error('HTMLInputElement value setter not found');

  await act(async () => {
    valueSetter.call(input, value);
    input.dispatchEvent(new Event('input', { bubbles: true }));
    input.dispatchEvent(new Event('change', { bubbles: true }));
    await flushPromises();
  });
};

const clickFirstOrderRow = async (root: ParentNode) => {
  const row = root.querySelector('tbody tr');
  if (!(row instanceof HTMLElement)) {
    throw new Error('Order row not found');
  }

  await act(async () => {
    row.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
  });
};

describe('MarketplaceOrdersPage', () => {
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
    listOrdersMock.mockReset();
    updateOrderMock.mockReset();
    listOrdersMock.mockResolvedValue([buildOrder()]);
    updateOrderMock.mockResolvedValue(buildOrder({ moStatus: 'paid', moPaidAt: '2030-01-01T13:00:00.000Z' }));
  });

  it('replaces the first-order empty view with one guided state instead of list-only filter and export chrome', async () => {
    listOrdersMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listOrdersMock).toHaveBeenCalledWith({ status: undefined, limit: 200 });
        expect(countLabelsByText(container, 'Buscar por comprador, email o ID')).toBe(0);
        expect(countLabelsByText(container, 'Estado del listado')).toBe(0);
        expect(countLabelsByText(container, 'Método de pago')).toBe(0);
        expect(countLabelsByText(container, 'Desde')).toBe(0);
        expect(countLabelsByText(container, 'Hasta')).toBe(0);
        expect(container.textContent).toContain(
          'Todavía no hay órdenes. Cuando llegue la primera, aquí aparecerán búsqueda, filtros y exportación para revisar la bandeja.',
        );
        expect(container.textContent).not.toContain('Atajos rápidos');
        expect(container.textContent).not.toContain('0 pagados');
        expect(container.textContent).not.toContain('0 pendientes');
        expect(queryActionByText(container, 'Exportar CSV')).toBeNull();
        expect(queryActionByText(container, 'Limpiar filtros')).toBeNull();
        expect(queryActionByText(container, 'Ir al marketplace')).not.toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the list filter label distinct from the order editor status field', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listOrdersMock).toHaveBeenCalledWith({ status: undefined, limit: 200 });
        expect(countLabelsByText(container, 'Estado del listado')).toBe(1);
        expect(countLabelsByText(container, 'Estado')).toBe(0);
        expect(container.querySelector('tbody tr')).not.toBeNull();
      });

      await clickFirstOrderRow(container);

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Detalle de la orden');
        expect(countLabelsByText(document.body, 'Estado del listado')).toBe(1);
        expect(countLabelsByText(document.body, 'Nuevo estado')).toBe(1);
        expect(countLabelsByText(document.body, 'Estado')).toBe(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('uses the existing reset-filters control when a search hides the current order list', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listOrdersMock).toHaveBeenCalledWith({ status: undefined, limit: 200 });
        expect(container.querySelector('tbody tr')).not.toBeNull();
        expect(queryActionByText(container, 'Limpiar filtros')).not.toBeNull();
      });

      const searchInput = getInputByLabel(container, 'Buscar por comprador, email o ID');
      await setInputValue(searchInput, 'sin coincidencias');

      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'No hay órdenes en la vista actual. Usa Limpiar filtros para volver a la bandeja completa.',
        );
        expect(queryActionByText(container, 'Limpiar filtros')).not.toBeNull();
        expect(queryActionByText(container, 'Ir al marketplace')).toBeNull();
        expect(container.querySelector('tbody tr')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });
});
