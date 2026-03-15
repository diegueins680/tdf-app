import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { MarketplaceOrderDTO } from '../api/types';

const confirmDatafastPaymentMock = jest.fn<(orderId: string, resourcePath: string) => Promise<MarketplaceOrderDTO>>();

jest.unstable_mockModule('../api/marketplace', () => ({
  Marketplace: {
    confirmDatafastPayment: (orderId: string, resourcePath: string) =>
      confirmDatafastPaymentMock(orderId, resourcePath),
  },
}));

const { default: DatafastReturnPage } = await import('../pages/DatafastReturnPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const renderPage = async (container: HTMLElement) => {
  let root: Root | null = createRoot(container);
  await act(async () => {
    root?.render(
      <MemoryRouter future={{ v7_startTransition: true, v7_relativeSplatPath: true }}>
        <DatafastReturnPage />
      </MemoryRouter>,
    );
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
    },
  };
};

const buildOrder = (overrides: Partial<MarketplaceOrderDTO> = {}): MarketplaceOrderDTO =>
  ({
    moOrderId: 'order-1',
    moStatus: 'datafast_pending',
    moTotalDisplay: '$10.00',
    moCurrency: 'usd',
    moBuyerName: 'Test User',
    moBuyerEmail: 'test@example.com',
    moBuyerPhone: null,
    moCreatedAt: '2030-01-01T12:00:00.000Z',
    moPaidAt: null,
    moPaymentProvider: 'datafast',
    moCartId: 'cart-1',
    moItems: [],
    moStatusHistory: [],
    ...overrides,
  }) as MarketplaceOrderDTO;

describe('DatafastReturnPage', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    confirmDatafastPaymentMock.mockReset();
    window.localStorage.clear();
    window.history.pushState({}, '', '/marketplace/pago-datafast?orderId=order-1&resourcePath=resource-1');
  });

  it('keeps the cart and shows a review state for non-paid Datafast statuses', async () => {
    confirmDatafastPaymentMock.mockResolvedValueOnce(buildOrder());
    window.localStorage.setItem('tdf-marketplace-cart-id', 'cart-1');
    window.localStorage.setItem('tdf-marketplace-cart-meta', '{"items":1}');
    window.localStorage.setItem('tdf-marketplace-buyer', '{"name":"Test User"}');

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    expect(confirmDatafastPaymentMock).toHaveBeenCalledWith('order-1', 'resource-1');
    expect(container.textContent).toContain('Pago con tarjeta en revisión');
    expect(container.textContent).not.toContain('Pago confirmado. ¡Gracias por tu compra!');
    expect(window.localStorage.getItem('tdf-marketplace-cart-id')).toBe('cart-1');
    expect(window.localStorage.getItem('tdf-marketplace-cart-meta')).toBe('{"items":1}');
    expect(window.localStorage.getItem('tdf-marketplace-buyer')).toBe('{"name":"Test User"}');

    await cleanup();
    document.body.removeChild(container);
  });

  it('clears the cart only after a paid Datafast status is confirmed', async () => {
    confirmDatafastPaymentMock.mockResolvedValueOnce(buildOrder({ moStatus: 'datafast_paid', moPaidAt: '2030-01-01T12:05:00.000Z' }));
    window.localStorage.setItem('tdf-marketplace-cart-id', 'cart-1');
    window.localStorage.setItem('tdf-marketplace-cart-meta', '{"items":1}');
    window.localStorage.setItem('tdf-marketplace-buyer', '{"name":"Test User"}');

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    expect(container.textContent).toContain('Pago confirmado. ¡Gracias por tu compra!');
    expect(window.localStorage.getItem('tdf-marketplace-cart-id')).toBeNull();
    expect(window.localStorage.getItem('tdf-marketplace-cart-meta')).toBeNull();
    expect(window.localStorage.getItem('tdf-marketplace-buyer')).toBeNull();

    await cleanup();
    document.body.removeChild(container);
  });
});
