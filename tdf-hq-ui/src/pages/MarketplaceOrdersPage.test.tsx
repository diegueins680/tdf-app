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
const normalizeText = (value: string | null | undefined) => (value ?? '').replace(/\s+/g, ' ').trim();

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

const buildOrderItem = (
  overrides: Partial<MarketplaceOrderDTO['moItems'][number]> = {},
): MarketplaceOrderDTO['moItems'][number] => ({
  moiListingId: 'listing-1',
  moiTitle: 'Vintage Mic',
  moiQuantity: 1,
  moiUnitPriceUsdCents: 10000,
  moiSubtotalCents: 10000,
  moiUnitPriceDisplay: 'USD $100.00',
  moiSubtotalDisplay: 'USD $100.00',
  ...overrides,
});

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
  moItems: [buildOrderItem()],
  ...overrides,
});

const orderSearchLabel = 'Buscar por comprador, contacto o ID';

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
    const text = normalizeText((element.textContent ?? '').replace('*', ''));
    return text === labelText;
  }).length;

const getTableHeaders = (root: ParentNode) =>
  Array.from(root.querySelectorAll('th')).map((element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim());

const getColumnTextsByHeader = (root: ParentNode, headerText: string) => {
  const headers = getTableHeaders(root);
  const columnIndex = headers.indexOf(headerText);
  if (columnIndex < 0) return [];

  return Array.from(root.querySelectorAll('tbody tr')).map((row) => (
    row.querySelectorAll('td')[columnIndex]?.textContent ?? ''
  ).replace(/\s+/g, ' ').trim());
};

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

const clickActionByText = async (root: ParentNode, labelText: string) => {
  const action = queryActionByText(root, labelText);
  if (!(action instanceof HTMLElement)) {
    throw new Error(`Action not found: ${labelText}`);
  }

  await act(async () => {
    action.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
  });
};

const clickButtonByAriaLabel = async (root: ParentNode, ariaLabel: string) => {
  const button = root.querySelector<HTMLElement>(`button[aria-label="${ariaLabel}"]`);
  if (!(button instanceof HTMLElement)) {
    throw new Error(`Action not found for aria-label: ${ariaLabel}`);
  }

  await act(async () => {
    button.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
  });
};

const clickCheckboxByLabel = async (root: ParentNode, labelText: string) => {
  const label = Array.from(root.querySelectorAll<HTMLLabelElement>('label')).find(
    (element) => normalizeText(element.textContent) === labelText,
  );
  if (!(label instanceof HTMLLabelElement)) {
    throw new Error(`Checkbox label not found: ${labelText}`);
  }

  const input = label.querySelector<HTMLInputElement>('input[type="checkbox"]');
  if (!(input instanceof HTMLInputElement)) {
    throw new Error(`Checkbox input not found for label: ${labelText}`);
  }

  await act(async () => {
    input.click();
    await flushPromises();
    await flushPromises();
  });
};

const selectOptionByLabel = async (root: ParentNode, labelText: string, optionText: string) => {
  const label = Array.from(root.querySelectorAll<HTMLLabelElement>('label')).find(
    (element) => normalizeText((element.textContent ?? '').replace('*', '')) === labelText,
  );
  if (!(label instanceof HTMLLabelElement) || !label.id) {
    throw new Error(`Select label not found: ${labelText}`);
  }

  const trigger = document.body.querySelector<HTMLElement>(`[role="combobox"][aria-labelledby*="${label.id}"]`);
  if (!(trigger instanceof HTMLElement)) {
    throw new Error(`Select trigger not found for label: ${labelText}`);
  }

  await act(async () => {
    trigger.dispatchEvent(new MouseEvent('mousedown', { bubbles: true }));
    await flushPromises();
    await flushPromises();
  });

  const option = Array.from(document.body.querySelectorAll<HTMLElement>('[role="option"], [role="menuitem"]')).find(
    (element) => normalizeText(element.textContent) === optionText,
  );
  if (!(option instanceof HTMLElement)) {
    throw new Error(`Select option not found: ${optionText}`);
  }

  await act(async () => {
    option.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
    await flushPromises();
  });
};

const clickFirstOrderRow = async (root: ParentNode) => {
  const explicitOpenAction = queryActionByText(root, 'Abrir orden');
  if (explicitOpenAction instanceof HTMLElement) {
    await act(async () => {
      explicitOpenAction.dispatchEvent(new MouseEvent('click', { bubbles: true }));
      await flushPromises();
    });
    return;
  }

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
        expect(countLabelsByText(container, orderSearchLabel)).toBe(0);
        expect(countLabelsByText(container, 'Estado del listado')).toBe(0);
        expect(countLabelsByText(container, 'Método de pago')).toBe(0);
        expect(countLabelsByText(container, 'Desde')).toBe(0);
        expect(countLabelsByText(container, 'Hasta')).toBe(0);
        expect(container.textContent).not.toContain(
          'Órdenes del marketplace. Solo Admin/Operación pueden editar estados y pagos.',
        );
        expect(container.textContent).toContain(
          'Todavía no hay órdenes. Cuando llegue la primera, aquí aparecerán búsqueda, filtros y exportación para revisar la bandeja.',
        );
        expect(container.textContent).not.toContain('Atajos rápidos');
        expect(container.textContent).not.toContain('0 pagados');
        expect(container.textContent).not.toContain('0 pendientes');
        expect(queryActionByText(container, 'Exportar CSV')).toBeNull();
        expect(queryActionByText(container, 'Limpiar filtros')).toBeNull();
        expect(queryActionByText(container, 'Ir al marketplace')).not.toBeNull();
        expect(container.querySelector('button[aria-label="Recargar órdenes"]')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the first marketplace load focused on setup instead of a refresh action', async () => {
    listOrdersMock.mockImplementation(() => new Promise(() => {}));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Cargando órdenes...');
        expect(container.textContent).not.toContain(
          'Órdenes del marketplace. Solo Admin/Operación pueden editar estados y pagos.',
        );
        expect(container.querySelector('button[aria-label="Recargar órdenes"]')).toBeNull();
        expect(queryActionByText(container, 'Ir al marketplace')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('replaces the first marketplace order row with a focused summary and one explicit open action', async () => {
    listOrdersMock.mockResolvedValue([buildOrder()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listOrdersMock).toHaveBeenCalledWith({ status: undefined, limit: 200 });
        expect(countLabelsByText(container, orderSearchLabel)).toBe(0);
        expect(countLabelsByText(container, 'Estado del listado')).toBe(0);
        expect(countLabelsByText(container, 'Método de pago')).toBe(0);
        expect(countLabelsByText(container, 'Desde')).toBe(0);
        expect(countLabelsByText(container, 'Hasta')).toBe(0);
        expect(container.textContent).not.toContain(
          'Órdenes del marketplace. Solo Admin/Operación pueden editar estados y pagos.',
        );
        expect(container.textContent).toContain(
          'Solo hay una orden por ahora. Revisa estado, pago y datos del comprador desde este resumen. Cuando llegue la segunda, aquí aparecerán filtros y exportación.',
        );
        expect(container.textContent).toContain('Pedido: order-1');
        expect(container.textContent).toContain('Comprador: Ada Lovelace');
        expect(container.textContent).toContain('Email: ada@example.com');
        expect(container.textContent).toContain('Teléfono: +593999000111');
        expect(container.textContent).toContain('Estado: Pendiente');
        expect(container.textContent).toContain('Pago: PayPal');
        expect(container.textContent).toContain('Total: USD $100.00');
        expect(container.textContent).toContain('Items: 1 × Vintage Mic');
        expect(container.textContent).not.toContain('Atajos rápidos');
        expect(queryActionByText(container, 'Exportar CSV')).toBeNull();
        expect(queryActionByText(container, 'Limpiar filtros')).toBeNull();
        expect(queryActionByText(container, 'Abrir orden')).not.toBeNull();
        expect(container.querySelector('button[aria-label="Recargar órdenes"]')).toBeNull();
        expect(container.querySelector('table')).toBeNull();
        expect(container.querySelector('[data-testid="marketplace-single-order-summary"]')).not.toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('restores the refresh action once the page has a real order list to revisit', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Órdenes del marketplace. Solo Admin/Operación pueden editar estados y pagos.',
        );
        expect(container.querySelector('button[aria-label="Recargar órdenes"]')).not.toBeNull();
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
      });
    } finally {
      await cleanup();
    }
  });

  it('collapses preset shortcuts into one quick-view control instead of four duplicate filter actions', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moStatus: 'pending',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moPaymentProvider: 'datafast',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(countLabelsByText(container, 'Vista rápida')).toBe(1);
        expect(container.textContent).toContain(
          'Aplica una vista base y reemplaza los filtros actuales antes de revisar resultados.',
        );
        expect(container.textContent).toContain(
          'Los filtros activos aparecerán aquí cuando acotes la bandeja. Limpiar filtros aparecerá en ese momento.',
        );
        expect(container.textContent).not.toContain('Atajos rápidos');
        expect(queryActionByText(container, 'Últimos 7 días')).toBeNull();
        expect(queryActionByText(container, 'Tarjeta pendiente')).toBeNull();
        expect(queryActionByText(container, 'Limpiar filtros')).toBeNull();
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
      });
    } finally {
      await cleanup();
    }
  });

  it('hides date and payment filters behind one explicit toggle until someone asks for them', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moStatus: 'pending',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moPaymentProvider: 'datafast',
        moStatus: 'paid',
        moPaidAt: '2030-01-02T12:30:00.000Z',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(queryActionByText(container, 'Mostrar fechas y pago')).not.toBeNull();
        expect(countLabelsByText(container, 'Desde')).toBe(0);
        expect(countLabelsByText(container, 'Hasta')).toBe(0);
        expect(container.textContent).not.toContain('Solo con pago registrado');
      });

      await clickActionByText(container, 'Mostrar fechas y pago');

      await waitForExpectation(() => {
        expect(queryActionByText(container, 'Ocultar fechas y pago')).not.toBeNull();
        expect(countLabelsByText(container, 'Desde')).toBe(1);
        expect(countLabelsByText(container, 'Hasta')).toBe(1);
        expect(container.textContent).toContain('Solo con pago registrado');
      });
    } finally {
      await cleanup();
    }
  });

  it('drops the duplicate paid-only control once the list is already filtered to paid orders', async () => {
    const pendingOrder = buildOrder({
      moOrderId: 'order-1',
      moStatus: 'pending',
      moPaidAt: null,
    });
    const paidOrder = buildOrder({
      moOrderId: 'order-2',
      moCartId: 'cart-2',
      moBuyerName: 'Grace Hopper',
      moBuyerEmail: 'grace@example.com',
      moStatus: 'paid',
      moPaidAt: '2030-01-02T12:30:00.000Z',
      moCreatedAt: '2030-01-02T12:00:00.000Z',
      moUpdatedAt: '2030-01-02T12:00:00.000Z',
    });
    const approvedOrder = buildOrder({
      moOrderId: 'order-3',
      moCartId: 'cart-3',
      moBuyerName: 'Katherine Johnson',
      moBuyerEmail: 'katherine@example.com',
      moStatus: 'approved',
      moPaymentProvider: 'datafast',
      moPaidAt: '2030-01-03T12:30:00.000Z',
      moCreatedAt: '2030-01-03T12:00:00.000Z',
      moUpdatedAt: '2030-01-03T12:00:00.000Z',
    });

    listOrdersMock.mockImplementation((params) =>
      Promise.resolve(
        params?.status === 'paid'
          ? [paidOrder]
          : [pendingOrder, paidOrder, approvedOrder],
      ));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(queryActionByText(container, 'Mostrar fechas y pago')).not.toBeNull();
        expect(countLabelsByText(container, 'Estado del listado')).toBe(1);
      });

      await clickActionByText(container, 'Mostrar fechas y pago');

      await waitForExpectation(() => {
        expect(queryActionByText(container, 'Ocultar fechas y pago')).not.toBeNull();
        expect(container.textContent).toContain('Solo con pago registrado');
      });

      await clickCheckboxByLabel(container, 'Solo con pago registrado');

      await waitForExpectation(() => {
        expect(container.textContent).toContain('Con pago');
        expect(countLabelsByText(container, 'Estado del listado')).toBe(1);
      });

      await selectOptionByLabel(container, 'Estado del listado', 'Pagado');

      await waitForExpectation(() => {
        expect(listOrdersMock).toHaveBeenLastCalledWith({ status: 'paid', limit: 200 });
        expect(queryActionByText(container, 'Ocultar fechas')).not.toBeNull();
        expect(queryActionByText(container, 'Ocultar fechas y pago')).toBeNull();
        expect(container.textContent).not.toContain('Solo con pago registrado');
        expect(container.textContent).not.toContain('Con pago');
        expect(container.textContent).toContain('Estado: Pagado');
      });
    } finally {
      await cleanup();
    }
  });

  it('replaces a single real status filter with context copy when the current list already shares one status', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerName: 'Ada Lovelace',
        moPaymentProvider: 'paypal',
        moStatus: 'pending',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moPaymentProvider: 'datafast',
        moStatus: 'pending',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listOrdersMock).toHaveBeenCalledWith({ status: undefined, limit: 200 });
        expect(countLabelsByText(container, 'Estado del listado')).toBe(0);
        expect(countLabelsByText(container, 'Método de pago')).toBe(1);
        expect(container.textContent).toContain(
          'Todos los pedidos visibles comparten el estado Pendiente. El filtro de estado aparecerá cuando esta vista mezcle más de un estado.',
        );
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the list filter label distinct from the order editor status field', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moStatus: 'pending',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moStatus: 'paid',
        moPaidAt: '2030-01-02T12:30:00.000Z',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

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

  it('replaces a single real payment-method filter with context copy when the current list already uses one provider', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerName: 'Ada Lovelace',
        moPaymentProvider: 'paypal',
        moStatus: 'pending',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moPaymentProvider: 'paypal',
        moStatus: 'paid',
        moPaidAt: '2030-01-02T12:30:00.000Z',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listOrdersMock).toHaveBeenCalledWith({ status: undefined, limit: 200 });
        expect(countLabelsByText(container, 'Estado del listado')).toBe(1);
        expect(countLabelsByText(container, 'Método de pago')).toBe(0);
        expect(getTableHeaders(container)).not.toContain('Pago');
        expect(getColumnTextsByHeader(container, 'Pago')).toEqual([]);
        expect(container.textContent).toContain(
          'Todos los pedidos visibles usan PayPal. El filtro de método aparecerá cuando esta vista mezcle más de un canal de pago.',
        );
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
      });
    } finally {
      await cleanup();
    }
  });

  it('hides PayPal payer emails already shown as buyer emails and keeps distinct payer accounts', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerEmail: 'ada@example.com',
        moPaymentProvider: 'paypal',
        moPaypalPayerEmail: 'ada@example.com',
        moStatus: 'pending',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moPaymentProvider: 'paypal',
        moPaypalPayerEmail: '  GRACE@example.com  ',
        moStatus: 'pending',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const firstRender = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getTableHeaders(container)).not.toContain('Pago');
        expect(container.textContent).toContain(
          'Todos los pedidos visibles comparten el estado Pendiente y usan PayPal.',
        );
        expect((container.textContent?.match(/ada@example\.com/g) ?? [])).toHaveLength(1);
        expect((container.textContent?.match(/grace@example\.com/g) ?? [])).toHaveLength(1);
        expect(container.textContent).not.toContain('GRACE@example.com');
      });
    } finally {
      await firstRender.cleanup();
    }

    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerEmail: 'ada@example.com',
        moPaymentProvider: 'paypal',
        moPaypalPayerEmail: 'payer@example.com',
        moStatus: 'pending',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moPaymentProvider: 'paypal',
        moPaypalPayerEmail: null,
        moStatus: 'pending',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const nextContainer = document.createElement('div');
    document.body.appendChild(nextContainer);
    const secondRender = await renderPage(nextContainer);

    try {
      await waitForExpectation(() => {
        expect(getTableHeaders(nextContainer)).toContain('Pago');
        expect(nextContainer.textContent).toContain('payer@example.com');
        expect((nextContainer.textContent?.match(/ada@example\.com/g) ?? [])).toHaveLength(1);
      });
    } finally {
      await secondRender.cleanup();
    }
  });

  it('keeps the payment column when visible rows need per-order payment context', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerName: 'Ada Lovelace',
        moPaymentProvider: 'paypal',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moPaymentProvider: 'datafast',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getTableHeaders(container)).toContain('Pago');
        expect(getColumnTextsByHeader(container, 'Pago')).toEqual(['Tarjeta (Datafast)', 'PayPal']);
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
      });
    } finally {
      await cleanup();
    }
  });

  it('combines shared status and payment context into one helper block when neither filter needs to render', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerName: 'Ada Lovelace',
        moPaymentProvider: 'paypal',
        moStatus: 'pending',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moPaymentProvider: 'paypal',
        moStatus: 'pending',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listOrdersMock).toHaveBeenCalledWith({ status: undefined, limit: 200 });
        expect(countLabelsByText(container, 'Estado del listado')).toBe(0);
        expect(countLabelsByText(container, 'Método de pago')).toBe(0);
        expect(container.textContent).toContain(
          'Todos los pedidos visibles comparten el estado Pendiente y usan PayPal. Los filtros de estado y método aparecerán cuando esta vista mezcle más de un estado o canal de pago.',
        );
        expect(container.textContent).not.toContain(
          'Todos los pedidos visibles comparten el estado Pendiente. El filtro de estado aparecerá cuando esta vista mezcle más de un estado.',
        );
        expect(container.textContent).not.toContain(
          'Todos los pedidos visibles usan PayPal. El filtro de método aparecerá cuando esta vista mezcle más de un canal de pago.',
        );
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
      });
    } finally {
      await cleanup();
    }
  });

  it('shows the buyer phone column only when the current visible order list includes phone data', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerPhone: '   ',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moBuyerPhone: null,
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const firstRender = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
        expect(getTableHeaders(container)).not.toContain('Contacto');
        expect(container.textContent).not.toContain('+593999000111');
      });
    } finally {
      await firstRender.cleanup();
    }

    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerPhone: '+593999000111',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moBuyerPhone: null,
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const nextContainer = document.createElement('div');
    document.body.appendChild(nextContainer);
    const secondRender = await renderPage(nextContainer);

    try {
      await waitForExpectation(() => {
        expect(nextContainer.querySelectorAll('tbody tr')).toHaveLength(2);
        expect(getTableHeaders(nextContainer)).toContain('Contacto');
        expect(nextContainer.textContent).toContain('+593999000111');
      });
    } finally {
      await secondRender.cleanup();
    }
  });

  it('shows the paid-at column only when the visible order list includes a payment timestamp', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moPaidAt: null,
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moPaidAt: null,
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const firstRender = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
        expect(getTableHeaders(container)).not.toContain('Pagado');
      });
    } finally {
      await firstRender.cleanup();
    }

    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moPaidAt: null,
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moStatus: 'paid',
        moPaidAt: '2030-01-02T12:30:00.000Z',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const nextContainer = document.createElement('div');
    document.body.appendChild(nextContainer);
    const secondRender = await renderPage(nextContainer);

    try {
      await waitForExpectation(() => {
        expect(nextContainer.querySelectorAll('tbody tr')).toHaveLength(2);
        expect(getTableHeaders(nextContainer)).toContain('Pagado');
      });
    } finally {
      await secondRender.cleanup();
    }
  });

  it('keeps item summaries readable by omitting duplicate single-item counts', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerName: 'Ada Lovelace',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
        moItems: [buildOrderItem({ moiListingId: 'listing-2', moiTitle: 'Patch Cable' })],
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const firstRender = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getColumnTextsByHeader(container, 'Items')).toEqual([
          '1 × Patch Cable',
          '1 × Vintage Mic',
        ]);
        expect(getColumnTextsByHeader(container, 'Items').join(' ')).not.toContain('1 1 ×');
      });
    } finally {
      await firstRender.cleanup();
    }

    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerName: 'Ada Lovelace',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
        moItems: [
          buildOrderItem({ moiListingId: 'listing-2', moiTitle: 'Patch Cable' }),
          buildOrderItem({ moiListingId: 'listing-3', moiTitle: 'Mic Stand' }),
        ],
      }),
    ]);

    const nextContainer = document.createElement('div');
    document.body.appendChild(nextContainer);
    const secondRender = await renderPage(nextContainer);

    try {
      await waitForExpectation(() => {
        const itemTexts = getColumnTextsByHeader(nextContainer, 'Items');
        expect(itemTexts[0]).toContain('2 items');
        expect(itemTexts[0]).toContain('1 × Patch Cable · 1 × Mic Stand');
        expect(itemTexts[1]).toBe('1 × Vintage Mic');
      });
    } finally {
      await secondRender.cleanup();
    }
  });

  it('keeps the header breakdown tied to the visible order list and hides it once search leaves one bucket', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerName: 'Ada Lovelace',
        moPaymentProvider: 'paypal',
        moStatus: 'pending',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moPaymentProvider: 'paypal',
        moStatus: 'paid',
        moPaidAt: '2030-01-02T12:30:00.000Z',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('1 pagados');
        expect(container.textContent).toContain('1 pendientes');
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
        expect(queryActionByText(container, 'Exportar CSV')).not.toBeNull();
      });

      const searchInput = getInputByLabel(container, orderSearchLabel);
      await setInputValue(searchInput, 'grace');

      await waitForExpectation(() => {
        expect(container.querySelector('tbody tr')).toBeNull();
        expect(container.querySelector('[data-testid="marketplace-single-order-summary"]')).not.toBeNull();
        expect(container.textContent).not.toContain('1 pagados');
        expect(container.textContent).not.toContain('1 pendientes');
        expect(container.textContent).not.toContain('0 pendientes');
        expect(queryActionByText(container, 'Exportar CSV')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps search-only filtering owned by the field instead of duplicating tray actions', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerName: 'Ada Lovelace',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(countLabelsByText(container, 'Vista rápida')).toBe(1);
      });

      const searchInput = getInputByLabel(container, orderSearchLabel);
      await setInputValue(searchInput, 'grace');

      await waitForExpectation(() => {
        expect(container.querySelector('tbody tr')).toBeNull();
        expect(container.querySelector('[data-testid="marketplace-single-order-summary"]')).not.toBeNull();
        expect(searchInput.value).toBe('grace');
        expect(countLabelsByText(container, 'Vista rápida')).toBe(0);
        expect(container.textContent).not.toContain('Busca: grace');
        expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
        expect(queryActionByText(container, 'Copiar enlace de filtros')).toBeNull();
        expect(queryActionByText(container, 'Limpiar filtros')).toBeNull();
        expect(container.textContent).toContain(
          'La búsqueda activa se maneja desde el campo superior. Usa Limpiar ahí para volver a la bandeja completa. Los demás filtros aparecerán aquí cuando combines más criterios.',
        );
      });

      await clickButtonByAriaLabel(container, 'Limpiar búsqueda');

      await waitForExpectation(() => {
        expect(searchInput.value).toBe('');
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
        expect(countLabelsByText(container, 'Vista rápida')).toBe(1);
        expect(container.textContent).not.toContain('Busca: grace');
        expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).toBeNull();
        expect(queryActionByText(container, 'Copiar enlace de filtros')).toBeNull();
        expect(queryActionByText(container, 'Limpiar filtros')).toBeNull();
        expect(container.textContent).toContain(
          'Los filtros activos aparecerán aquí cuando acotes la bandeja. Limpiar filtros aparecerá en ese momento.',
        );
      });
    } finally {
      await cleanup();
    }
  });

  it('hides the generic refresh action while search is active so the field owns the recovery path', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerName: 'Ada Lovelace',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('button[aria-label="Recargar órdenes"]')).not.toBeNull();
      });

      const searchInput = getInputByLabel(container, orderSearchLabel);
      await setInputValue(searchInput, 'grace');

      await waitForExpectation(() => {
        expect(searchInput.value).toBe('grace');
        expect(container.querySelector('tbody tr')).toBeNull();
        expect(container.querySelector('[data-testid="marketplace-single-order-summary"]')).not.toBeNull();
        expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
        expect(container.querySelector('button[aria-label="Recargar órdenes"]')).toBeNull();
      });

      await clickButtonByAriaLabel(container, 'Limpiar búsqueda');

      await waitForExpectation(() => {
        expect(searchInput.value).toBe('');
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
        expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).toBeNull();
        expect(container.querySelector('button[aria-label="Recargar órdenes"]')).not.toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps search reset in the field while the tray clears only extra filters', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerName: 'Ada Lovelace',
        moStatus: 'pending',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moStatus: 'paid',
        moPaidAt: '2030-01-02T12:30:00.000Z',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
      });

      const searchInput = getInputByLabel(container, orderSearchLabel);
      await setInputValue(searchInput, 'ada');

      await waitForExpectation(() => {
        expect(searchInput.value).toBe('ada');
        expect(container.querySelector('tbody tr')).toBeNull();
        expect(container.querySelector('[data-testid="marketplace-single-order-summary"]')).not.toBeNull();
        expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
      });

      await clickActionByText(container, 'Mostrar fechas y pago');
      const fromDateInput = getInputByLabel(container, 'Desde');
      await setInputValue(fromDateInput, '2030-01-02');

      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'No hay órdenes en la vista actual. Usa Limpiar otros filtros para conservar la búsqueda o Limpiar dentro del campo para volver a la bandeja completa.',
        );
        expect(queryActionByText(container, 'Limpiar otros filtros')).not.toBeNull();
        expect(queryActionByText(container, 'Limpiar filtros')).toBeNull();
        expect(queryActionByText(container, 'Copiar enlace de filtros')).not.toBeNull();
      });

      await clickActionByText(container, 'Limpiar otros filtros');

      await waitForExpectation(() => {
        expect(searchInput.value).toBe('ada');
        expect(container.querySelector('tbody tr')).toBeNull();
        expect(container.querySelector('[data-testid="marketplace-single-order-summary"]')).not.toBeNull();
        expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
        expect(queryActionByText(container, 'Limpiar otros filtros')).toBeNull();
        expect(queryActionByText(container, 'Copiar enlace de filtros')).toBeNull();
        expect(container.textContent).toContain(
          'La búsqueda activa se maneja desde el campo superior. Usa Limpiar ahí para volver a la bandeja completa. Los demás filtros aparecerán aquí cuando combines más criterios.',
        );
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps phone lookup inside the existing contact search instead of adding phone filter chrome', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerName: 'Ada Lovelace',
        moBuyerPhone: '+593 999 000 111',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moBuyerPhone: '+593 988 777 666',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(countLabelsByText(container, orderSearchLabel)).toBe(1);
        expect(countLabelsByText(container, 'Buscar por comprador, email o ID')).toBe(0);
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
        expect(container.textContent).not.toContain('Filtrar por teléfono');
      });

      const searchInput = getInputByLabel(container, orderSearchLabel);
      await setInputValue(searchInput, '999000111');

      await waitForExpectation(() => {
        expect(container.querySelector('tbody tr')).toBeNull();
        expect(container.querySelector('[data-testid="marketplace-single-order-summary"]')).not.toBeNull();
        expect(container.textContent).toContain('Ada Lovelace');
        expect(container.textContent).not.toContain('Grace Hopper');
        expect(container.textContent).not.toContain('No hay órdenes para la búsqueda actual.');
        expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
        expect(queryActionByText(container, 'Limpiar filtros')).toBeNull();
        expect(listOrdersMock).toHaveBeenCalledTimes(1);
      });
    } finally {
      await cleanup();
    }
  });

  it('treats whitespace-only search as empty instead of showing search recovery chrome', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moBuyerName: 'Ada Lovelace',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(countLabelsByText(container, 'Vista rápida')).toBe(1);
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
      });

      const searchInput = getInputByLabel(container, orderSearchLabel);
      await setInputValue(searchInput, '   ');

      await waitForExpectation(() => {
        expect(searchInput.value).toBe('');
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
        expect(countLabelsByText(container, 'Vista rápida')).toBe(1);
        expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).toBeNull();
        expect(queryActionByText(container, 'Limpiar filtros')).toBeNull();
        expect(container.textContent).not.toContain('La búsqueda activa se maneja desde el campo superior.');
        expect(container.textContent).toContain(
          'Los filtros activos aparecerán aquí cuando acotes la bandeja. Limpiar filtros aparecerá en ese momento.',
        );
        expect(listOrdersMock).toHaveBeenCalledTimes(1);
      });
    } finally {
      await cleanup();
    }
  });

  it('uses row click as the primary table action and keeps copy helpers collapsed behind one labeled menu', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moStatus: 'pending',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moStatus: 'paid',
        moPaidAt: '2030-01-02T12:30:00.000Z',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Haz clic en una fila para revisar estado, pago y datos del comprador.',
        );
        expect(container.textContent).not.toContain('Revisa el estado, pagos y detalles de cada pedido.');
        expect(container.textContent).not.toContain('Acciones');
        expect(container.querySelectorAll('button[aria-label^="Copiar fila del pedido "]')).toHaveLength(0);
        expect(container.querySelectorAll('tbody button[aria-label^="Copiar ID del pedido "]')).toHaveLength(0);
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
      });

      await clickFirstOrderRow(container);

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Detalle de la orden');
        expect(document.body.querySelectorAll('button[aria-label^="Copiar ID del pedido "]')).toHaveLength(0);
        expect(queryActionByText(document.body, 'Copiar')).not.toBeNull();
        expect(document.body.textContent).not.toContain('Copiar ID');
        expect(document.body.textContent).not.toContain('Copiar resumen');
      });

      await clickActionByText(document.body, 'Copiar');

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Copiar ID');
        expect(document.body.textContent).toContain('Copiar resumen');
      });
    } finally {
      await cleanup();
    }
  });

  it('humanizes payment provider labels in the order detail and copied summary', async () => {
    const writeTextMock = jest.fn<(text: string) => Promise<void>>().mockResolvedValue(undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText: writeTextMock },
    });

    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moPaymentProvider: 'datafast',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moPaymentProvider: 'paypal',
        moCreatedAt: '2030-01-01T12:00:00.000Z',
        moUpdatedAt: '2030-01-01T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
      });

      await clickFirstOrderRow(container);

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Detalle de la orden');
        expect(document.body.textContent).toContain('Pago: Tarjeta (Datafast)');
        expect(document.body.textContent).not.toContain('Pago: datafast');
      });

      await clickActionByText(document.body, 'Copiar');

      await waitForExpectation(() => {
        expect(
          Array.from(document.body.querySelectorAll<HTMLElement>('[role="menuitem"]')).some(
            (item) => (item.textContent ?? '').trim() === 'Copiar resumen',
          ),
        ).toBe(true);
      });

      await act(async () => {
        const copySummaryMenuItem = Array.from(document.body.querySelectorAll<HTMLElement>('[role="menuitem"]')).find(
          (item) => (item.textContent ?? '').trim() === 'Copiar resumen',
        );
        copySummaryMenuItem?.dispatchEvent(new MouseEvent('click', { bubbles: true }));
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(writeTextMock).toHaveBeenCalledWith(expect.stringContaining('Pago: Tarjeta (Datafast)'));
        expect(writeTextMock).not.toHaveBeenCalledWith(expect.stringContaining('Pago: datafast'));
      });
    } finally {
      await cleanup();
    }
  });

  it('omits per-row currency captions when the total already carries the currency', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'usd-0001',
        moCurrency: 'usd',
        moTotalDisplay: 'USD $100.00',
      }),
      buildOrder({
        moOrderId: 'eur-0002',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moBuyerPhone: null,
        moCurrency: 'eur',
        moTotalDisplay: '€80.00',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const orderCells = Array.from(container.querySelectorAll('tbody tr td:first-child'));
        const usdOrderCell = orderCells.find((cell) => cell.textContent?.includes('usd-0001'));
        const eurOrderCell = orderCells.find((cell) => cell.textContent?.includes('eur-0002'));

        expect(usdOrderCell?.textContent).not.toContain('USD');
        expect(eurOrderCell?.textContent).toContain('EUR');
        expect(container.textContent).toContain('USD $100.00');
        expect(container.textContent).toContain('€80.00');
      });
    } finally {
      await cleanup();
    }
  });

  it('summarizes a shared missing currency once instead of repeating row captions', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'usd-0001',
        moCurrency: 'usd',
        moTotalDisplay: '$100.00',
      }),
      buildOrder({
        moOrderId: 'usd-0002',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moBuyerPhone: null,
        moCurrency: 'USD',
        moTotalDisplay: '$80.00',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const orderCells = Array.from(container.querySelectorAll('tbody tr td:first-child'));
        const sharedCurrency = container.querySelector('[data-testid="marketplace-orders-shared-currency"]');

        expect(sharedCurrency?.textContent?.trim()).toBe('Moneda visible: USD.');
        expect((container.textContent?.match(/Moneda visible: USD\./g) ?? [])).toHaveLength(1);
        expect(orderCells.map((cell) => cell.textContent ?? '').join(' ')).not.toContain('USD');
        expect(container.textContent).toContain('$100.00');
        expect(container.textContent).toContain('$80.00');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the payment shortcut available for non-paid orders that still need that action', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moStatus: 'pending',
        moPaidAt: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(queryActionByText(container, 'Abrir orden')).not.toBeNull();
        expect(container.querySelector('[data-testid="marketplace-single-order-summary"]')).not.toBeNull();
      });

      await clickFirstOrderRow(container);

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Detalle de la orden');
        expect(queryActionByText(document.body, 'Marcar pagado ahora')).not.toBeNull();
        expect(countLabelsByText(document.body, 'Fecha de pago')).toBe(0);
      });

      await clickActionByText(document.body, 'Marcar pagado ahora');

      await waitForExpectation(() => {
        expect(countLabelsByText(document.body, 'Fecha de pago')).toBe(1);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps missing-provider guidance hidden until a paid state needs it', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moStatus: 'pending',
        moPaymentProvider: null,
        moPaidAt: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(queryActionByText(container, 'Abrir orden')).not.toBeNull();
        expect(container.querySelector('[data-testid="marketplace-single-order-summary"]')).not.toBeNull();
      });

      await clickFirstOrderRow(container);

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Detalle de la orden');
        expect(document.body.textContent).not.toContain('No hay método de pago registrado.');
      });

      await clickActionByText(document.body, 'Marcar pagado ahora');

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('No hay método de pago registrado.');
        const saveButton = queryActionByText(document.body, 'Guardar cambios');
        expect(saveButton).toBeInstanceOf(HTMLButtonElement);
        expect((saveButton as HTMLButtonElement).disabled).toBe(true);
      });

      const providerInput = getInputByLabel(document.body, 'Proveedor de pago');
      await setInputValue(providerInput, 'manual');

      await waitForExpectation(() => {
        expect(document.body.textContent).not.toContain('No hay método de pago registrado.');
        const saveButton = queryActionByText(document.body, 'Guardar cambios');
        expect(saveButton).toBeInstanceOf(HTMLButtonElement);
        expect((saveButton as HTMLButtonElement).disabled).toBe(false);
      });
    } finally {
      await cleanup();
    }
  });

  it('collapses paid-state setup blockers into one warning when method and date are both still missing', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moStatus: 'pending',
        moPaymentProvider: null,
        moPaidAt: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(queryActionByText(container, 'Abrir orden')).not.toBeNull();
        expect(container.querySelector('[data-testid="marketplace-single-order-summary"]')).not.toBeNull();
      });

      await clickFirstOrderRow(container);

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Detalle de la orden');
        expect(document.body.textContent).not.toContain(
          'Completa el método de pago y la fecha del cobro para dejar la orden como pagada.',
        );
      });

      await selectOptionByLabel(document.body, 'Nuevo estado', 'Pagado');

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain(
          'Completa el método de pago y la fecha del cobro para dejar la orden como pagada.',
        );
        expect(document.body.textContent).not.toContain(
          'No hay método de pago registrado. Ingresa paypal, datafast o manual para dejar trazabilidad.',
        );
        expect(document.body.textContent).not.toContain(
          'Agrega la fecha y hora del cobro si marcas la orden como pagada.',
        );
        const saveButton = queryActionByText(document.body, 'Guardar cambios');
        expect(saveButton).toBeInstanceOf(HTMLButtonElement);
        expect((saveButton as HTMLButtonElement).disabled).toBe(true);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps save disabled until the order editor has a real change', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moStatus: 'pending',
        moPaymentProvider: 'paypal',
        moPaidAt: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(queryActionByText(container, 'Abrir orden')).not.toBeNull();
        expect(container.querySelector('[data-testid="marketplace-single-order-summary"]')).not.toBeNull();
      });

      await clickFirstOrderRow(container);

      await waitForExpectation(() => {
        const saveButton = queryActionByText(document.body, 'Guardar cambios');
        expect(saveButton).toBeInstanceOf(HTMLButtonElement);
        expect((saveButton as HTMLButtonElement).disabled).toBe(true);
      });

      const providerInput = getInputByLabel(document.body, 'Proveedor de pago');
      await setInputValue(providerInput, 'datafast');

      await waitForExpectation(() => {
        const saveButton = queryActionByText(document.body, 'Guardar cambios');
        expect(saveButton).toBeInstanceOf(HTMLButtonElement);
        expect((saveButton as HTMLButtonElement).disabled).toBe(false);
      });
    } finally {
      await cleanup();
    }
  });

  it('hides the payment shortcut once a paid order already has payment data recorded', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
        moStatus: 'paid',
        moPaidAt: '2030-01-03T12:30:00.000Z',
        moCreatedAt: '2030-01-03T12:00:00.000Z',
        moUpdatedAt: '2030-01-03T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(queryActionByText(container, 'Abrir orden')).not.toBeNull();
        expect(container.querySelector('[data-testid="marketplace-single-order-summary"]')).not.toBeNull();
      });

      await clickFirstOrderRow(container);

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Detalle de la orden');
        expect(queryActionByText(document.body, 'Marcar pagado ahora')).toBeNull();
        expect(queryActionByText(document.body, 'Registrar fecha de pago ahora')).toBeNull();
        expect(queryActionByText(document.body, 'Guardar cambios')).not.toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('replaces a one-row marketplace search result with the order summary instead of keeping table export chrome', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
        expect(queryActionByText(container, 'Exportar CSV')).not.toBeNull();
        expect(container.querySelector('[data-testid="marketplace-single-order-summary"]')).toBeNull();
      });

      const searchInput = getInputByLabel(container, orderSearchLabel);
      await setInputValue(searchInput, 'grace@example.com');

      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'La búsqueda dejó una sola orden visible. Revísala aquí y usa Limpiar dentro del campo para volver a comparar pedidos.',
        );
        expect(container.querySelector('[data-testid="marketplace-single-order-summary"]')).not.toBeNull();
        expect(container.textContent).toContain('Pedido: order-2');
        expect(container.textContent).toContain('Comprador: Grace Hopper');
        expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
        expect(queryActionByText(container, 'Exportar CSV')).toBeNull();
        expect(container.querySelector('tbody tr')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps search-empty states tied to the field instead of showing a duplicate tray reset', async () => {
    listOrdersMock.mockResolvedValue([
      buildOrder({
        moOrderId: 'order-1',
      }),
      buildOrder({
        moOrderId: 'order-2',
        moCartId: 'cart-2',
        moBuyerName: 'Grace Hopper',
        moBuyerEmail: 'grace@example.com',
        moCreatedAt: '2030-01-02T12:00:00.000Z',
        moUpdatedAt: '2030-01-02T12:00:00.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listOrdersMock).toHaveBeenCalledWith({ status: undefined, limit: 200 });
        expect(container.querySelector('tbody tr')).not.toBeNull();
        expect(queryActionByText(container, 'Limpiar filtros')).toBeNull();
        expect(container.textContent).toContain(
          'Los filtros activos aparecerán aquí cuando acotes la bandeja. Limpiar filtros aparecerá en ese momento.',
        );
      });

      const searchInput = getInputByLabel(container, orderSearchLabel);
      await setInputValue(searchInput, 'sin coincidencias');

      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'No hay órdenes para la búsqueda actual. Usa Limpiar dentro del campo de búsqueda para volver a la bandeja completa.',
        );
        expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
        expect(queryActionByText(container, 'Copiar enlace de filtros')).toBeNull();
        expect(queryActionByText(container, 'Limpiar filtros')).toBeNull();
        expect(queryActionByText(container, 'Exportar CSV')).toBeNull();
        expect(container.textContent).not.toContain(
          'Los filtros activos aparecerán aquí cuando acotes la bandeja. Limpiar filtros aparecerá en ese momento.',
        );
        expect(queryActionByText(container, 'Ir al marketplace')).toBeNull();
        expect(container.querySelector('tbody tr')).toBeNull();
      });

      await clickButtonByAriaLabel(container, 'Limpiar búsqueda');

      await waitForExpectation(() => {
        expect(container.querySelector('tbody tr')).not.toBeNull();
        expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).toBeNull();
        expect(queryActionByText(container, 'Copiar enlace de filtros')).toBeNull();
        expect(queryActionByText(container, 'Limpiar filtros')).toBeNull();
        expect(container.textContent).toContain(
          'Los filtros activos aparecerán aquí cuando acotes la bandeja. Limpiar filtros aparecerá en ese momento.',
        );
      });
    } finally {
      await cleanup();
    }
  });
});
