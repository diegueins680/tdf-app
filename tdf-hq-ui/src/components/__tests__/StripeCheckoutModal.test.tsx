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

const validatePromoCode =
  jest.fn<(eventId: string, codeId: string, code?: string, tierId?: string) => Promise<PromoCodeDTO>>();
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
          tpwpTierId: 'tier-1',
          tpwpQuantity: 1,
          tpwpBuyerName: 'John Doe',
          tpwpBuyerEmail: 'john@example.com',
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

  it('closes modal on cancel', () => {
    renderModal();

    const cancelButton = screen.getByRole('button', { name: /Cancel/i });
    fireEvent.click(cancelButton);

    expect(mockOnClose).toHaveBeenCalledTimes(1);
  });
});
