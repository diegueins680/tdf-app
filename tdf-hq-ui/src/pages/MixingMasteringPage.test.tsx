import { jest } from '@jest/globals';
import { fireEvent, render, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type {
  ServiceStorefrontOrderCreate,
  ServiceStorefrontOrderDTO,
  ServiceStorefrontPackageDTO,
} from '../api/serviceStorefront';

const packageDto: ServiceStorefrontPackageDTO = {
  sspId: 'package-1',
  sspServiceKind: 'Mastering',
  sspTier: 'Pro',
  sspName: 'Mastering Profesional',
  sspDescription: 'Paquete autorizado',
  sspPriceUsdCents: 7000,
  sspCurrency: 'USD',
  sspMinSongCount: 1,
  sspMaxSongCount: 3,
  sspTurnaroundDays: 5,
  sspRevisionCount: 2,
  sspDeliverables: [],
  sspFeatures: [],
  sspActive: true,
  sspSortOrder: 1,
};

const createdOrder: ServiceStorefrontOrderDTO = {
  ssoId: 'order-id',
  ssoOrderNumber: 'TDF-REAL1234',
  ssoBuyerName: 'Test Buyer',
  ssoBuyerEmail: 'buyer@example.com',
  ssoPackageId: packageDto.sspId,
  ssoServiceKind: packageDto.sspServiceKind,
  ssoTier: packageDto.sspTier,
  ssoPriceUsdCents: packageDto.sspPriceUsdCents,
  ssoCurrency: 'USD',
  ssoStatus: 'awaiting_payment',
  ssoLookupToken: 'private-lookup-token',
  ssoSongCount: 1,
  ssoCreatedAt: '2030-01-01T00:00:00Z',
  ssoUpdatedAt: '2030-01-01T00:00:00Z',
};

const listPackagesMock = jest.fn<() => Promise<ServiceStorefrontPackageDTO[]>>();
const createOrderMock = jest.fn<(idempotencyKey: string, payload: ServiceStorefrontOrderCreate) => Promise<ServiceStorefrontOrderDTO>>();
const selectManualPaymentMock = jest.fn<() => Promise<ServiceStorefrontOrderDTO>>();
const createDatafastCheckoutMock = jest.fn<() => Promise<never>>();

const EmptyIcon = () => null;
jest.unstable_mockModule('@mui/icons-material', () => ({
  MusicNote: EmptyIcon,
  CheckCircle: EmptyIcon,
  ExpandMore: EmptyIcon,
  ArrowBack: EmptyIcon,
  ArrowForward: EmptyIcon,
  Headphones: EmptyIcon,
  Mic: EmptyIcon,
  Album: EmptyIcon,
  Timer: EmptyIcon,
  Refresh: EmptyIcon,
  Security: EmptyIcon,
  Speed: EmptyIcon,
}));

jest.unstable_mockModule('../api/serviceStorefront', () => ({
  ServiceStorefront: {
    listPackages: listPackagesMock,
    createOrder: createOrderMock,
    selectManualPayment: selectManualPaymentMock,
    createDatafastCheckout: createDatafastCheckoutMock,
    createPaypalOrder: jest.fn(),
    capturePaypalOrder: jest.fn(),
  },
}));

jest.unstable_mockModule('../components/reviews/ExperienceReviews', () => ({
  default: () => null,
}));

const { default: MixingMasteringPage } = await import('./MixingMasteringPage');

const renderPage = () => {
  const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  return render(
    <MemoryRouter>
      <QueryClientProvider client={queryClient}>
        <MixingMasteringPage />
      </QueryClientProvider>
    </MemoryRouter>,
  );
};

const reachPaymentStep = async () => {
  renderPage();
  fireEvent.click(await screen.findByRole('button', { name: 'Seleccionar' }));
  fireEvent.change(screen.getByRole('textbox', { name: /Tu nombre/ }), { target: { value: 'Test Buyer' } });
  fireEvent.change(screen.getByRole('textbox', { name: /Email/ }), { target: { value: 'buyer@example.com' } });
  fireEvent.click(screen.getByRole('button', { name: 'Continuar al pago' }));
};

describe('MixingMasteringPage commercial truthfulness', () => {
  beforeEach(() => {
    listPackagesMock.mockReset().mockResolvedValue([packageDto]);
    createOrderMock.mockReset();
    selectManualPaymentMock.mockReset();
    createDatafastCheckoutMock.mockReset();
    window.sessionStorage.clear();
  });

  it('does not fabricate an order or show success after the API fails', async () => {
    createOrderMock.mockRejectedValueOnce(new Error('API unavailable; no order was created'));
    await reachPaymentStep();
    fireEvent.click(screen.getByRole('button', { name: /Datafast/ }));

    expect(await screen.findByText('API unavailable; no order was created')).toBeTruthy();
    expect(screen.queryByText('Pedido creado')).toBeNull();
    expect(document.body.textContent).not.toContain('(Demo)');
    expect(document.body.textContent).not.toMatch(/TDF-[A-Z0-9]{6,}/);
  });

  it('keeps bank transfer pending and links to the service tracker', async () => {
    createOrderMock.mockResolvedValueOnce(createdOrder);
    selectManualPaymentMock.mockResolvedValueOnce({
      ...createdOrder,
      ssoStatus: 'awaiting_manual_confirmation',
      ssoLookupToken: null,
      ssoPaymentProvider: 'bank_transfer',
    });
    await reachPaymentStep();
    fireEvent.click(screen.getByRole('button', { name: /Transferencia bancaria/ }));

    expect(await screen.findByText('Pedido creado')).toBeTruthy();
    expect(screen.getByText(/todavía no es un pago/)).toBeTruthy();
    const trackingLink = screen.getByRole('link', { name: 'Ver estado del pedido' });
    expect(trackingLink.getAttribute('href')).toContain('/mezcla-mastering/pedido/TDF-REAL1234#access=private-lookup-token');
    expect(trackingLink.getAttribute('href')).not.toContain('/marketplace/orden/');
  });
});
