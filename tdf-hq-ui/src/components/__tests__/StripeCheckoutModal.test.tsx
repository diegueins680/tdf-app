import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { render, screen, waitFor, fireEvent } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { Elements } from '@stripe/react-stripe-js';
import { loadStripe } from '@stripe/stripe-js';
import type { ReactNode } from 'react';

const validatePromoCode = jest.fn<() => Promise<unknown>>();
const createPaymentIntent = jest.fn<() => Promise<unknown>>();

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: { validatePromoCode, createPaymentIntent },
}));

const { StripeCheckoutModal } = await import('../StripeCheckoutModal');

const stripePromise = loadStripe('pk_test_mock');

const createWrapper = () => {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false },
    },
  });

  return ({ children }: { children: ReactNode }) => (
    <QueryClientProvider client={queryClient}>
      <Elements stripe={stripePromise}>{children}</Elements>
    </QueryClientProvider>
  );
};

describe('StripeCheckoutModal', () => {
  const mockTier = {
    ettId: 'tier-1',
    ettEventId: 'event-1',
    ettName: 'General Admission',
    ettPrice: 5000,
    ettCurrency: 'USD',
    ettQuantityTotal: 100,
    ettQuantitySold: 50,
    ettSalesStartDate: '2026-01-01T00:00:00Z',
    ettSalesEndDate: '2026-12-31T23:59:59Z',
  };

  const mockOnClose = jest.fn();
  const mockOnSuccess = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('renders buyer details form on step 1', () => {
    render(
      <StripeCheckoutModal
        open={true}
        onClose={mockOnClose}
        tier={mockTier}
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

    expect(screen.getByText('Complete Your Purchase')).toBeInTheDocument();
    expect(screen.getByLabelText(/Your Name/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/Email Address/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/Quantity/i)).toBeInTheDocument();
  });

  it('validates buyer details before proceeding', async () => {
    render(
      <StripeCheckoutModal
        open={true}
        onClose={mockOnClose}
        tier={mockTier}
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

    const nextButton = screen.getByRole('button', { name: /Next/i });
    fireEvent.click(nextButton);

    await waitFor(() => {
      expect(screen.getByText(/Name is required/i)).toBeInTheDocument();
    });
  });

  it('applies promo code discount', async () => {
    const mockPromoCode = {
      pcId: 'promo-1',
      pcCode: 'SAVE20',
      pcDiscountType: 'percentage',
      pcDiscountValue: 20,
      pcValidFrom: null,
      pcValidUntil: null,
      pcMaxRedemptions: null,
      pcCurrentRedemptions: 0,
      pcIsActive: true,
    };

    validatePromoCode.mockResolvedValue(
      mockPromoCode
    );

    render(
      <StripeCheckoutModal
        open={true}
        onClose={mockOnClose}
        tier={mockTier}
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

    // Fill buyer details
    fireEvent.change(screen.getByLabelText(/Your Name/i), {
      target: { value: 'John Doe' },
    });
    fireEvent.change(screen.getByLabelText(/Email Address/i), {
      target: { value: 'john@example.com' },
    });

    // Apply promo code
    const promoInput = screen.getByPlaceholderText(/Enter promo code/i);
    fireEvent.change(promoInput, { target: { value: 'SAVE20' } });

    await waitFor(() => {
      expect(screen.getByText(/20% off/i)).toBeInTheDocument();
    });

    // Check discounted price
    expect(screen.getByText(/\$40.00/i)).toBeInTheDocument(); // $50 - 20%
  });

  it('creates payment intent and shows confirmation', async () => {
    const mockPaymentIntent = {
      spiClientSecret: 'pi_mock_secret',
      spiOrderId: 'order-123',
      spiAmount: 5000,
    };

    createPaymentIntent.mockResolvedValue(
      mockPaymentIntent
    );

    render(
      <StripeCheckoutModal
        open={true}
        onClose={mockOnClose}
        tier={mockTier}
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

    // Fill buyer details
    fireEvent.change(screen.getByLabelText(/Your Name/i), {
      target: { value: 'John Doe' },
    });
    fireEvent.change(screen.getByLabelText(/Email Address/i), {
      target: { value: 'john@example.com' },
    });

    // Proceed to payment
    const nextButton = screen.getByRole('button', { name: /Next/i });
    fireEvent.click(nextButton);

    await waitFor(() => {
      expect(screen.getByText(/Payment Details/i)).toBeInTheDocument();
    });
  });

  it('handles payment errors', async () => {
    createPaymentIntent.mockRejectedValue(
      new Error('Payment failed')
    );

    render(
      <StripeCheckoutModal
        open={true}
        onClose={mockOnClose}
        tier={mockTier}
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

    // Fill and submit
    fireEvent.change(screen.getByLabelText(/Your Name/i), {
      target: { value: 'John Doe' },
    });
    fireEvent.change(screen.getByLabelText(/Email Address/i), {
      target: { value: 'john@example.com' },
    });

    const nextButton = screen.getByRole('button', { name: /Next/i });
    fireEvent.click(nextButton);

    await waitFor(() => {
      expect(screen.getByText(/Payment failed/i)).toBeInTheDocument();
    });
  });

  it('closes modal on cancel', () => {
    render(
      <StripeCheckoutModal
        open={true}
        onClose={mockOnClose}
        tier={mockTier}
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

    const cancelButton = screen.getByRole('button', { name: /Cancel/i });
    fireEvent.click(cancelButton);

    expect(mockOnClose).toHaveBeenCalledTimes(1);
  });
});
