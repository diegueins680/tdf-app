import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { render, screen, waitFor, fireEvent } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { ReactNode } from 'react';

const validatePromoCode = jest.fn<() => Promise<unknown>>();
const createPaymentIntent = jest.fn<() => Promise<unknown>>();

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

  return ({ children }: { children: ReactNode }) => (
    <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  );
};

describe('StripeCheckoutModal', () => {
  const mockTier = {
    ticketTierId: 'tier-1',
    ticketTierEventId: 'event-1',
    ticketTierCode: 'GA',
    ticketTierName: 'General Admission',
    ticketTierPriceCents: 5000,
    ticketTierCurrency: 'USD',
    ticketTierQuantityTotal: 100,
    ticketTierQuantitySold: 50,
    ticketTierActive: true,
  };

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

    const continueButton = screen.getByRole('button', { name: /Continue to Payment/i });
    fireEvent.click(continueButton);

    await waitFor(() => {
      expect(screen.getByText(/fill in all required fields/i)).toBeInTheDocument();
    });
  });

  it('applies promo code discount', async () => {
    const mockPromoCode = {
      promoCodeId: 'promo-1',
      promoCodeCode: 'SAVE20',
      promoCodeDiscountType: 'percentage',
      promoCodeDiscountValue: 2000,
      promoCodeCurrency: 'USD',
      promoCodeValidFrom: null,
      promoCodeValidUntil: null,
      promoCodeMaxRedemptions: null,
      promoCodeCurrentRedemptions: 0,
      promoCodeIsActive: true,
    };

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
      expect(screen.getByText(/20% off/i)).toBeInTheDocument();
    });

    // The modal defers the discount, surfacing that it applies at checkout.
    expect(screen.getByText(/applied at checkout/i)).toBeInTheDocument();
  });

  it('advances to the payment step after creating a payment intent', async () => {
    const mockPaymentIntent = {
      spiClientSecret: 'pi_mock_secret',
      spiOrderId: 'order-123',
      spiAmount: 5000,
    };

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
    const continueButton = screen.getByRole('button', { name: /Continue to Payment/i });
    fireEvent.click(continueButton);

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

    const continueButton = screen.getByRole('button', { name: /Continue to Payment/i });
    fireEvent.click(continueButton);

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
