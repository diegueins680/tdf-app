import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { MarketplaceCartDTO, MarketplaceItemDTO, MarketplaceOrderDTO } from '../api/types';

const listMock = jest.fn<() => Promise<MarketplaceItemDTO[]>>();
const getCartMock = jest.fn<(cartId: string) => Promise<MarketplaceCartDTO>>();
const createCartMock = jest.fn<() => Promise<MarketplaceCartDTO>>();
const upsertItemMock = jest.fn<(cartId: string) => Promise<MarketplaceCartDTO>>();
const checkoutMock = jest.fn<(cartId: string, payload: unknown) => Promise<MarketplaceOrderDTO>>();
const datafastCheckoutMock = jest.fn();
const createPaypalOrderMock = jest.fn();
const capturePaypalOrderMock = jest.fn();
const inventoryUpdateMock = jest.fn();

jest.unstable_mockModule('../api/marketplace', () => ({
  Marketplace: {
    list: () => listMock(),
    getCart: (cartId: string) => getCartMock(cartId),
    createCart: () => createCartMock(),
    upsertItem: (cartId: string) => upsertItemMock(cartId),
    checkout: (cartId: string, payload: unknown) => checkoutMock(cartId, payload as never),
    datafastCheckout: (...args: unknown[]) => datafastCheckoutMock(...args),
    createPaypalOrder: (...args: unknown[]) => createPaypalOrderMock(...args),
    capturePaypalOrder: (...args: unknown[]) => capturePaypalOrderMock(...args),
  },
}));

jest.unstable_mockModule('../api/inventory', () => ({
  Inventory: {
    update: (...args: unknown[]) => inventoryUpdateMock(...args),
  },
}));

jest.unstable_mockModule('../components/GoogleDriveUploadWidget', () => ({
  default: () => null,
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: null }),
  getStoredSessionToken: () => null,
}));

const { default: MarketplacePage } = await import('../pages/MarketplacePage');

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

const buildListing = (overrides: Partial<MarketplaceItemDTO> = {}): MarketplaceItemDTO =>
  ({
    miListingId: 'listing-1',
    miAssetId: 'asset-1',
    miTitle: 'Vintage Mic',
    miBrand: 'Shure',
    miModel: 'SM58',
    miCategory: 'Mics',
    miCondition: 'used',
    miPurpose: 'sale',
    miStatus: 'En stock',
    miPriceUsdCents: 10000,
    miPriceDisplay: 'USD $100.00',
    miPhotoUrl: null,
    ...overrides,
  }) as MarketplaceItemDTO;

const buildCart = (overrides: Partial<MarketplaceCartDTO> = {}): MarketplaceCartDTO =>
  ({
    mcCartId: 'cart-1',
    mcSubtotalDisplay: 'USD $100.00',
    mcItems: [
      {
        mciListingId: 'listing-1',
        mciTitle: 'Vintage Mic',
        mciQuantity: 1,
        mciSubtotalDisplay: 'USD $100.00',
        mciUnitPriceDisplay: 'USD $100.00',
        mciCategory: 'Mics',
      },
    ],
    ...overrides,
  }) as MarketplaceCartDTO;

const buildOrder = (overrides: Partial<MarketplaceOrderDTO> = {}): MarketplaceOrderDTO =>
  ({
    moOrderId: 'order-1',
    moStatus: 'pending_contact',
    moTotalDisplay: 'USD $100.00',
    moCurrency: 'usd',
    moBuyerName: 'Saved Buyer',
    moBuyerEmail: 'saved@example.com',
    moBuyerPhone: null,
    moCreatedAt: '2030-01-01T12:00:00.000Z',
    moPaidAt: null,
    moPaymentProvider: null,
    moCartId: 'cart-1',
    moItems: [
      {
        moiListingId: 'listing-1',
        moiTitle: 'Vintage Mic',
        moiQuantity: 1,
        moiSubtotalDisplay: 'USD $100.00',
      },
    ],
    moStatusHistory: [],
    ...overrides,
  }) as MarketplaceOrderDTO;

const renderPage = async (container: HTMLElement) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);
  await act(async () => {
    root?.render(
      <MemoryRouter future={{ v7_startTransition: true, v7_relativeSplatPath: true }}>
        <QueryClientProvider client={qc}>
          <MarketplacePage />
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
    },
  };
};

const getInputByLabel = (container: HTMLElement, labelText: string) => {
  const labels = Array.from(container.querySelectorAll('label'));
  const label = labels.find((el) => {
    const text = (el.textContent ?? '').replace('*', '').trim();
    return text === labelText;
  });
  if (!label) throw new Error(`Label not found: ${labelText}`);
  const forId = label.getAttribute('for');
  if (forId) {
    const input = document.getElementById(forId);
    if (input instanceof HTMLInputElement) return input;
  }
  const fallback = label.parentElement?.querySelector<HTMLInputElement>('input,textarea');
  if (!fallback) throw new Error(`Input not found for label: ${labelText}`);
  return fallback;
};

const setInputValue = (input: HTMLInputElement, value: string) => {
  const descriptor = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value');
  if (descriptor?.set) {
    descriptor.set.call(input, value);
  } else {
    input.value = value;
  }
  input.dispatchEvent(new Event('input', { bubbles: true }));
  input.dispatchEvent(new Event('change', { bubbles: true }));
};

const clickButtonByText = (label: string, pick: 'first' | 'last' = 'first') => {
  const buttons = Array.from(document.querySelectorAll('button')).filter(
    (candidate) => candidate.textContent?.trim() === label,
  );
  const button = pick === 'last' ? buttons.at(-1) : buttons[0];
  if (!button) throw new Error(`Button not found: ${label}`);
  button.click();
};

describe('MarketplacePage', () => {
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
    listMock.mockReset();
    getCartMock.mockReset();
    createCartMock.mockReset();
    upsertItemMock.mockReset();
    checkoutMock.mockReset();
    datafastCheckoutMock.mockReset();
    createPaypalOrderMock.mockReset();
    capturePaypalOrderMock.mockReset();
    inventoryUpdateMock.mockReset();
    listMock.mockResolvedValue([buildListing()]);
    getCartMock.mockResolvedValue(buildCart());
    createCartMock.mockResolvedValue(buildCart({ mcCartId: 'new-cart', mcItems: [] }));
    upsertItemMock.mockResolvedValue(buildCart());
    checkoutMock.mockResolvedValue(buildOrder());
    window.localStorage.clear();
    window.history.pushState({}, '', '/marketplace');
  });

  it('prefers URL filters over saved filters on first render', async () => {
    listMock.mockResolvedValue([
      buildListing({ miCategory: 'Mics', miCondition: 'used', miPurpose: 'sale' }),
      buildListing({
        miListingId: 'listing-2',
        miAssetId: 'asset-2',
        miTitle: 'Stage Piano',
        miCategory: 'Keys',
        miCondition: 'demo',
        miPurpose: 'rent',
      }),
    ]);
    window.localStorage.setItem(
      'tdf-marketplace-filters',
      JSON.stringify({ search: 'saved filter', category: 'Keys', sort: 'title-asc', purpose: 'rent', condition: 'demo' }),
    );
    window.history.pushState({}, '', '/marketplace?q=url+term&cat=Mics&sort=price-desc&purpose=sale&cond=used');

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getInputByLabel(container, 'Buscar equipo').value).toBe('url term');
      expect(container.textContent).toContain('Categoría: Mics');
      expect(container.textContent).toContain('Modalidad: Venta');
      expect(container.textContent).toContain('Condición: used');
      expect(window.location.search).toBe('?q=url+term&cat=Mics&sort=price-desc&purpose=sale&cond=used');
    });

    await cleanup();
    document.body.removeChild(container);
  });

  it('drops stale saved category and condition filters once listings load', async () => {
    listMock.mockResolvedValue([
      buildListing({ miCategory: 'Mics', miCondition: 'used', miPurpose: 'sale' }),
      buildListing({
        miListingId: 'listing-2',
        miAssetId: 'asset-2',
        miTitle: 'Stage Piano',
        miCategory: 'Keys',
        miCondition: 'demo',
        miPurpose: 'rent',
      }),
    ]);
    window.localStorage.setItem(
      'tdf-marketplace-filters',
      JSON.stringify({ category: 'Missing', condition: 'broken', sort: 'relevance', purpose: 'all', search: '' }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Vintage Mic');
      expect(container.textContent).toContain('Stage Piano');
      expect(window.location.search).toBe('');
      const savedFilters = JSON.parse(window.localStorage.getItem('tdf-marketplace-filters') ?? '{}') as {
        category?: string;
        condition?: string;
      };
      expect(savedFilters.category).toBe('all');
      expect(savedFilters.condition).toBe('all');
      expect(container.textContent).not.toContain('Categoría: Missing');
      expect(container.textContent).not.toContain('Condición: broken');
    });

    await cleanup();
    document.body.removeChild(container);
  });

  it('opens the requested listing from the listing query parameter', async () => {
    listMock.mockResolvedValue([
      buildListing(),
      buildListing({
        miListingId: 'listing-2',
        miAssetId: 'asset-2',
        miTitle: 'Rare Synth',
        miBrand: 'Roland',
        miModel: 'Juno',
        miCategory: 'Keys',
      }),
    ]);
    window.history.pushState({}, '', '/marketplace?listing=listing-2');

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Detalle del equipo');
      expect(document.body.textContent).toContain('Rare Synth');
      expect(document.body.textContent).toContain('Roland Juno');
    });

    await cleanup();
    document.body.removeChild(container);
  });

  it('restores the original saved buyer snapshot and clears buyer storage cleanly', async () => {
    window.localStorage.setItem('tdf-marketplace-cart-id', 'cart-1');
    window.localStorage.setItem(
      'tdf-marketplace-buyer',
      JSON.stringify({
        name: 'Saved Buyer',
        email: 'saved@example.com',
        phone: '0999999999',
        pref: 'phone',
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Usar datos guardados');
      expect(container.textContent).toContain('Limpiar datos');
    });

    const nameInput = getInputByLabel(container, 'Nombre completo');
    const emailInput = getInputByLabel(container, 'Email');

    await act(async () => {
      setInputValue(nameInput, 'Edited Buyer');
      setInputValue(emailInput, 'edited@example.com');
      await flushPromises();
    });

    await act(async () => {
      clickButtonByText('Usar datos guardados');
      await flushPromises();
    });

    expect(getInputByLabel(container, 'Nombre completo').value).toBe('Saved Buyer');
    expect(getInputByLabel(container, 'Email').value).toBe('saved@example.com');

    await act(async () => {
      clickButtonByText('Limpiar datos');
      await flushPromises();
    });

    expect(getInputByLabel(container, 'Nombre completo').value).toBe('');
    expect(getInputByLabel(container, 'Email').value).toBe('');
    expect(window.localStorage.getItem('tdf-marketplace-buyer')).toBeNull();

    await cleanup();
    document.body.removeChild(container);
  });

  it('clears persisted cart state after a successful contact checkout', async () => {
    window.localStorage.setItem('tdf-marketplace-cart-id', 'cart-1');
    window.localStorage.setItem(
      'tdf-marketplace-cart-meta',
      JSON.stringify({ cartId: 'cart-1', count: 1, updatedAt: Date.now() }),
    );
    window.localStorage.setItem(
      'tdf-marketplace-buyer',
      JSON.stringify({ name: 'Saved Buyer', email: 'saved@example.com', pref: 'email' }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Confirmar pedido');
    });

    await act(async () => {
      clickButtonByText('Confirmar pedido');
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Revisa tu pedido');
    });

    await act(async () => {
      clickButtonByText('Confirmar pedido', 'last');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(checkoutMock).toHaveBeenCalled();
      expect(window.localStorage.getItem('tdf-marketplace-cart-id')).toBeNull();
      expect(window.localStorage.getItem('tdf-marketplace-cart-meta')).toBeNull();
      expect(document.body.textContent).toContain('Pedido enviado');
      expect(document.body.textContent).not.toContain('Tienes un carrito guardado');
    });

    await cleanup();
    document.body.removeChild(container);
  });
});
