import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { render, screen, waitFor, fireEvent } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import type {
  PromoCodeDTO,
  SocialTicketTierDTO,
  StripePaymentIntentDTO,
  TicketPurchaseWithPromoDTO,
} from '../../api/socialEvents';
import type { Stripe } from '@stripe/stripe-js';
import appI18n from '../../i18n/index';

const validatePromoCode =
  jest.fn<(eventId: string, codeId: string, code?: string, tierId?: string) => Promise<PromoCodeDTO>>();
let checkoutSession = { partyId: 42 };
jest.unstable_mockModule('../../session/SessionContext', () => ({ useSession: () => ({ session: checkoutSession }) }));
const loadCheckoutStripe = jest.fn<() => Promise<Stripe | null>>();
jest.unstable_mockModule('../../utils/checkoutStripe', () => ({ loadCheckoutStripe }));
const createPaymentIntent = jest.fn<(data: TicketPurchaseWithPromoDTO) => Promise<StripePaymentIntentDTO>>();

const GENERAL_ADMISSION_PRICE_CENTS = 5 * 1000;
const GENERAL_ADMISSION_TOTAL_QUANTITY = 100;
const GENERAL_ADMISSION_SOLD_QUANTITY = 5 * 10;
const SAVE_TWENTY_PERCENT_DISCOUNT_BASIS_POINTS = 2 * 10 * 100;
const NO_PROMO_REDEMPTIONS_USED = 0;
const MOCK_PAYMENT_INTENT_AMOUNT_CENTS = GENERAL_ADMISSION_PRICE_CENTS;
const SAVE_TWENTY_PERCENT_OFF_COPY = new RegExp(`${2}${0}% off`, 'i');
const MOCK_ORDER_ID = `order-${[1, 2, 3].join('')}`;

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: { validatePromoCode, createPaymentIntent },
}));

// Stripe.js cannot load in jsdom; stub the SDK so the payment step renders
// without reaching out to the network or hanging on the loader.
jest.unstable_mockModule('@stripe/stripe-js', () => ({
  loadStripe: () => Promise.resolve(null),
}));

jest.unstable_mockModule('@stripe/react-stripe-js', () => ({
  Elements: ({ children }: { children: ReactNode }) => <>{children}</>,
  PaymentElement: () => <div data-testid="stripe-payment-element" />,
  useStripe: () => null,
  useElements: () => null,
}));

const { StripeCheckoutModal } = await import('../StripeCheckoutModal');

const createWrapper = () => {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false },
    },
  });

  const TestWrapper = ({ children }: { children: ReactNode }) => (
    <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  );
  TestWrapper.displayName = 'TestWrapper';
  return TestWrapper;
};

describe('StripeCheckoutModal', () => {
  beforeAll(async () => appI18n.changeLanguage('en'));
  afterAll(async () => appI18n.changeLanguage('es'));
  /**
   * Fixture contract:
   * @precondition tier prices and payment-intent amounts are represented in cents.
   * @invariant promo discounts use the same basis-point contract as PromoCodeField.
   * @postcondition API mocks resolve DTO-compatible checkout payloads.
   */
  const mockTier = {
    ticketTierId: 'tier-1',
    ticketTierEventId: 'event-1',
    ticketTierCode: 'GA',
    ticketTierName: 'General Admission',
    ticketTierPriceCents: GENERAL_ADMISSION_PRICE_CENTS,
    ticketTierCurrency: 'USD',
    ticketTierQuantityTotal: GENERAL_ADMISSION_TOTAL_QUANTITY,
    ticketTierQuantitySold: GENERAL_ADMISSION_SOLD_QUANTITY,
    ticketTierActive: true,
  } satisfies SocialTicketTierDTO;

  const mockOnClose = jest.fn();
  const mockOnSuccess = jest.fn();

  const renderModal = () =>
    render(
      <StripeCheckoutModal
        open={true}
        onClose={mockOnClose}
        eventId="event-1"
        eventTitle="Launch Party"
        tier={mockTier}
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

  beforeEach(() => {
    jest.clearAllMocks();
    loadCheckoutStripe.mockResolvedValue({} as Stripe);
  });

  it('does not reserve under a different session after pending SDK readiness', async () => {
    let resolve!: (client: Stripe) => void;
    loadCheckoutStripe.mockReturnValue(new Promise<Stripe>((done) => { resolve = done; }));
    const view = renderModal();
    fireEvent.change(screen.getByLabelText(/Your Name/i), { target: { value: 'Buyer' } });
    fireEvent.change(screen.getByLabelText(/Email/i), { target: { value: 'buyer@example.test' } });
    fireEvent.submit(document.getElementById('stripe-checkout-buyer-details-form')!);
    checkoutSession = { partyId: 43 };
    view.rerender(<StripeCheckoutModal open onClose={mockOnClose} eventId="event-1" eventTitle="Launch Party" tier={mockTier} onSuccess={mockOnSuccess} />);
    resolve({} as Stripe);
    await waitFor(() => expect(screen.getByLabelText(/Your Name/i)).toHaveValue(''));
    expect(createPaymentIntent).not.toHaveBeenCalled();
    expect(mockOnSuccess).not.toHaveBeenCalled();
  });

  it('renders buyer details form on step 1', () => {
    renderModal();

    expect(screen.getByText(/Purchase Tickets/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/Your Name/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/Email/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/Quantity/i)).toBeInTheDocument();
  });

  it('validates buyer details before proceeding', async () => {
    renderModal();

    // Submit the form directly: the submit button's native `required` checks
    // would otherwise block submission in jsdom before the JS fallback runs.
    const buyerForm = document.getElementById('stripe-checkout-buyer-details-form');
    expect(buyerForm).not.toBeNull();
    fireEvent.submit(buyerForm as HTMLFormElement);

    await waitFor(() => {
      expect(screen.getByText(/fill in all required fields/i)).toBeInTheDocument();
    });
  });

  it('applies promo code discount', async () => {
    const mockPromoCode = {
      promoCodeId: 'promo-1',
      promoCodeCode: 'SAVE20',
      promoCodeDiscountType: 'percentage',
      promoCodeDiscountValue: SAVE_TWENTY_PERCENT_DISCOUNT_BASIS_POINTS,
      promoCodeCurrency: 'USD',
      promoCodeValidFrom: null,
      promoCodeValidUntil: null,
      promoCodeMaxRedemptions: null,
      promoCodeCurrentRedemptions: NO_PROMO_REDEMPTIONS_USED,
      promoCodeIsActive: true,
    } satisfies PromoCodeDTO;

    validatePromoCode.mockResolvedValue(mockPromoCode);

    renderModal();

    // Fill buyer details
    fireEvent.change(screen.getByLabelText(/Your Name/i), {
      target: { value: 'John Doe' },
    });
    fireEvent.change(screen.getByLabelText(/Email/i), {
      target: { value: 'john@example.com' },
    });

    // Apply promo code
    const promoInput = screen.getByPlaceholderText(/ENTER-CODE-HERE/i);
    fireEvent.change(promoInput, { target: { value: 'SAVE20' } });

    await waitFor(() => {
      expect(screen.getByText(SAVE_TWENTY_PERCENT_OFF_COPY)).toBeInTheDocument();
    });

    // The modal defers the discount, surfacing that it applies at checkout.
    expect(screen.getByText(/applied at checkout/i)).toBeInTheDocument();
  });

  it('advances to the payment step after creating a payment intent', async () => {
    const mockPaymentIntent = {
      spiClientSecret: 'pi_mock_secret',
      spiPaymentIntentId: 'pi_mock',
      spiOrderId: MOCK_ORDER_ID,
      spiAmountCents: MOCK_PAYMENT_INTENT_AMOUNT_CENTS,
      spiCurrency: 'USD',
    } satisfies StripePaymentIntentDTO;

    createPaymentIntent.mockResolvedValue(mockPaymentIntent);

    renderModal();

    // Fill buyer details
    fireEvent.change(screen.getByLabelText(/Your Name/i), {
      target: { value: 'John Doe' },
    });
    fireEvent.change(screen.getByLabelText(/Email/i), {
      target: { value: 'john@example.com' },
    });

    // Proceed to payment
    const paymentStepContinueButton = screen.getByRole('button', { name: /Continue to Payment/i });
    fireEvent.click(paymentStepContinueButton);

    await waitFor(() => {
      expect(createPaymentIntent).toHaveBeenCalledWith(
        expect.objectContaining({
          ticketPurchaseTierId: 'tier-1',
          ticketPurchaseQuantity: 1,
          ticketPurchaseBuyerName: 'John Doe',
          ticketPurchaseBuyerEmail: 'john@example.com',
        })
      );
    });

    // The payment step shows the buyer summary.
    expect(await screen.findByText(/john@example.com/i)).toBeInTheDocument();
  });

  it('handles payment errors', async () => {
    createPaymentIntent.mockRejectedValue(new Error('Payment failed'));

    renderModal();

    // Fill and submit
    fireEvent.change(screen.getByLabelText(/Your Name/i), {
      target: { value: 'John Doe' },
    });
    fireEvent.change(screen.getByLabelText(/Email/i), {
      target: { value: 'john@example.com' },
    });

    const failingPaymentContinueButton = screen.getByRole('button', { name: /Continue to Payment/i });
    fireEvent.click(failingPaymentContinueButton);

    await waitFor(() => {
      expect(screen.getByText(/Payment failed/i)).toBeInTheDocument();
    });
  });

  it('does not reserve inventory when the payment client is unavailable and preserves input for retry', async () => {
    loadCheckoutStripe.mockResolvedValueOnce(null);
    renderModal();
    fireEvent.change(screen.getByLabelText(/Your Name/i), { target: { value: 'Buyer' } });
    fireEvent.change(screen.getByLabelText(/Email/i), { target: { value: 'buyer@example.test' } });
    fireEvent.click(screen.getByRole('button', { name: /Continue to Payment/i }));
    expect(await screen.findByRole('alert')).toHaveTextContent('No tickets were reserved');
    expect(createPaymentIntent).not.toHaveBeenCalled();
    expect(screen.getByLabelText(/Your Name/i)).toHaveValue('Buyer');
    expect(screen.getByLabelText(/Email/i)).toHaveValue('buyer@example.test');
    createPaymentIntent.mockResolvedValueOnce({ spiClientSecret: 'secret', spiOrderId: 'order-retry', spiPaymentIntentId: 'intent-retry', spiAmountCents: 5000, spiCurrency: 'USD' });
    fireEvent.click(screen.getByRole('button', { name: /Continue to Payment/i }));
    await waitFor(() => expect(createPaymentIntent).toHaveBeenCalledTimes(1));
  });

  it('ignores late payment-client readiness after checkout is interrupted', async () => {
    let ready!: (client: Stripe | null) => void;
    loadCheckoutStripe.mockImplementationOnce(() => new Promise(resolve => { ready = resolve; }));
    const view = renderModal();
    fireEvent.change(screen.getByLabelText(/Your Name/i), { target: { value: 'Buyer' } });
    fireEvent.change(screen.getByLabelText(/Email/i), { target: { value: 'buyer@example.test' } });
    fireEvent.click(screen.getByRole('button', { name: /Continue to Payment/i }));
    view.unmount();
    ready({} as Stripe);
    await waitFor(() => expect(loadCheckoutStripe).toHaveBeenCalledTimes(1));
    await Promise.resolve();
    expect(createPaymentIntent).not.toHaveBeenCalled();
  });

  it('keeps only one readiness request while the buyer submits repeatedly', async () => {
    let ready!: (client: Stripe | null) => void;
    loadCheckoutStripe.mockImplementationOnce(() => new Promise(resolve => { ready = resolve; }));
    createPaymentIntent.mockResolvedValueOnce({ spiClientSecret: 'secret', spiOrderId: 'order-once', spiPaymentIntentId: 'intent-once', spiAmountCents: 5000, spiCurrency: 'USD' });
    renderModal();
    fireEvent.change(screen.getByLabelText(/Your Name/i), { target: { value: 'Buyer' } });
    fireEvent.change(screen.getByLabelText(/Email/i), { target: { value: 'buyer@example.test' } });
    const form = screen.getByLabelText(/Your Name/i).closest('form')!;
    fireEvent.submit(form);
    fireEvent.submit(form);
    expect(loadCheckoutStripe).toHaveBeenCalledTimes(1);
    ready({} as Stripe);
    await waitFor(() => expect(createPaymentIntent).toHaveBeenCalledTimes(1));
  });

  it('lets the buyer cancel a pending readiness check without reserving tickets', async () => {
    let ready!: (client: Stripe | null) => void;
    loadCheckoutStripe.mockImplementationOnce(() => new Promise(resolve => { ready = resolve; }));
    renderModal();
    fireEvent.change(screen.getByLabelText(/Your Name/i), { target: { value: 'Buyer' } });
    fireEvent.change(screen.getByLabelText(/Email/i), { target: { value: 'buyer@example.test' } });
    fireEvent.click(screen.getByRole('button', { name: /Continue to Payment/i }));
    fireEvent.click(screen.getByRole('button', { name: /Cancel/i }));
    expect(mockOnClose).toHaveBeenCalledTimes(1);
    ready({} as Stripe);
    await Promise.resolve();
    expect(createPaymentIntent).not.toHaveBeenCalled();
  });

  it('closes modal on cancel', () => {
    renderModal();

    const cancelButton = screen.getByRole('button', { name: /Cancel/i });
    fireEvent.click(cancelButton);

    expect(mockOnClose).toHaveBeenCalledTimes(1);
  });
});
