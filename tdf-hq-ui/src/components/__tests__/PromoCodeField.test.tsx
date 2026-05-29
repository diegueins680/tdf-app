import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { render, screen, waitFor, fireEvent } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { ReactNode } from 'react';

const validatePromoCode = jest.fn<() => Promise<unknown>>();

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: { validatePromoCode },
}));

const { PromoCodeField } = await import('../PromoCodeField');

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

describe('PromoCodeField', () => {
  const mockOnValidCode = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('renders promo code input field', () => {
    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onValidCode={mockOnValidCode}
      />,
      { wrapper: createWrapper() }
    );

    expect(screen.getByPlaceholderText(/Enter promo code/i)).toBeInTheDocument();
  });

  it('validates promo code with debounce', async () => {
    const mockPromoCode = {
      pcId: 'promo-1',
      pcCode: 'SAVE20',
      pcDiscountType: 'percentage',
      pcDiscountValue: 20,
      pcValidFrom: null,
      pcValidUntil: null,
      pcMaxRedemptions: 100,
      pcCurrentRedemptions: 50,
      pcIsActive: true,
    };

    validatePromoCode.mockResolvedValue(
      mockPromoCode
    );

    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onValidCode={mockOnValidCode}
      />,
      { wrapper: createWrapper() }
    );

    const input = screen.getByPlaceholderText(/Enter promo code/i);
    fireEvent.change(input, { target: { value: 'SAVE20' } });

    // Wait for debounce and validation
    await waitFor(
      () => {
        expect(validatePromoCode).toHaveBeenCalledWith(
          'event-1',
          'SAVE20',
          'tier-1'
        );
      },
      { timeout: 1000 }
    );

    await waitFor(() => {
      expect(screen.getByText(/20% off/i)).toBeInTheDocument();
    });

    expect(mockOnValidCode).toHaveBeenCalledWith(mockPromoCode);
  });

  it('shows error for invalid promo code', async () => {
    validatePromoCode.mockRejectedValue(
      new Error('Invalid promo code')
    );

    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onValidCode={mockOnValidCode}
      />,
      { wrapper: createWrapper() }
    );

    const input = screen.getByPlaceholderText(/Enter promo code/i);
    fireEvent.change(input, { target: { value: 'INVALID' } });

    await waitFor(
      () => {
        expect(screen.getByText(/Invalid promo code/i)).toBeInTheDocument();
      },
      { timeout: 1000 }
    );

    expect(mockOnValidCode).not.toHaveBeenCalled();
  });

  it('shows expiry date for time-limited promo codes', async () => {
    const mockPromoCode = {
      pcId: 'promo-1',
      pcCode: 'EARLYBIRD',
      pcDiscountType: 'fixed_amount',
      pcDiscountValue: 1000,
      pcValidFrom: '2026-01-01T00:00:00Z',
      pcValidUntil: '2026-12-31T23:59:59Z',
      pcMaxRedemptions: null,
      pcCurrentRedemptions: 0,
      pcIsActive: true,
    };

    validatePromoCode.mockResolvedValue(
      mockPromoCode
    );

    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onValidCode={mockOnValidCode}
      />,
      { wrapper: createWrapper() }
    );

    const input = screen.getByPlaceholderText(/Enter promo code/i);
    fireEvent.change(input, { target: { value: 'EARLYBIRD' } });

    await waitFor(
      () => {
        expect(screen.getByText(/Valid until/i)).toBeInTheDocument();
      },
      { timeout: 1000 }
    );
  });

  it('shows usage limit information', async () => {
    const mockPromoCode = {
      pcId: 'promo-1',
      pcCode: 'LIMITED',
      pcDiscountType: 'percentage',
      pcDiscountValue: 15,
      pcValidFrom: null,
      pcValidUntil: null,
      pcMaxRedemptions: 100,
      pcCurrentRedemptions: 95,
      pcIsActive: true,
    };

    validatePromoCode.mockResolvedValue(
      mockPromoCode
    );

    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onValidCode={mockOnValidCode}
      />,
      { wrapper: createWrapper() }
    );

    const input = screen.getByPlaceholderText(/Enter promo code/i);
    fireEvent.change(input, { target: { value: 'LIMITED' } });

    await waitFor(
      () => {
        expect(screen.getByText(/5 uses remaining/i)).toBeInTheDocument();
      },
      { timeout: 1000 }
    );
  });

  it('clears validation when input is emptied', async () => {
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
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onValidCode={mockOnValidCode}
      />,
      { wrapper: createWrapper() }
    );

    const input = screen.getByPlaceholderText(/Enter promo code/i);

    // Enter code
    fireEvent.change(input, { target: { value: 'SAVE20' } });
    await waitFor(() => {
      expect(screen.getByText(/20% off/i)).toBeInTheDocument();
    }, { timeout: 1000 });

    // Clear code
    fireEvent.change(input, { target: { value: '' } });

    await waitFor(() => {
      expect(screen.queryByText(/20% off/i)).not.toBeInTheDocument();
    });
  });
});
