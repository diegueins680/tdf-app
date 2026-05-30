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
  const mockOnPromoApplied = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('renders promo code input field', () => {
    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onPromoApplied={mockOnPromoApplied}
      />,
      { wrapper: createWrapper() }
    );

    expect(screen.getByPlaceholderText(/ENTER-CODE-HERE/i)).toBeInTheDocument();
  });

  it('validates promo code with debounce', async () => {
    const mockPromoCode = {
      promoCodeId: 'promo-1',
      promoCodeCode: 'SAVE20',
      promoCodeDiscountType: 'percentage',
      promoCodeDiscountValue: 2000,
      promoCodeCurrency: 'USD',
      promoCodeValidFrom: null,
      promoCodeValidUntil: null,
      promoCodeMaxRedemptions: 100,
      promoCodeCurrentRedemptions: 50,
      promoCodeIsActive: true,
    };

    validatePromoCode.mockResolvedValue(mockPromoCode);

    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onPromoApplied={mockOnPromoApplied}
      />,
      { wrapper: createWrapper() }
    );

    const input = screen.getByPlaceholderText(/ENTER-CODE-HERE/i);
    fireEvent.change(input, { target: { value: 'SAVE20' } });

    // Wait for debounce and validation
    await waitFor(
      () => {
        expect(validatePromoCode).toHaveBeenCalledWith(
          'event-1',
          'SAVE20',
          'SAVE20',
          'tier-1'
        );
      },
      { timeout: 1000 }
    );

    await waitFor(() => {
      expect(screen.getByText(/20% off/i)).toBeInTheDocument();
    });

    expect(mockOnPromoApplied).toHaveBeenLastCalledWith('SAVE20');
  });

  it('shows error for invalid promo code', async () => {
    validatePromoCode.mockRejectedValue(new Error('Invalid promo code'));

    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onPromoApplied={mockOnPromoApplied}
      />,
      { wrapper: createWrapper() }
    );

    const input = screen.getByPlaceholderText(/ENTER-CODE-HERE/i);
    fireEvent.change(input, { target: { value: 'INVALID' } });

    await waitFor(
      () => {
        expect(screen.getByText(/Invalid promo code/i)).toBeInTheDocument();
      },
      { timeout: 1000 }
    );

    // Invalid codes report null to the parent.
    expect(mockOnPromoApplied).toHaveBeenLastCalledWith(null);
  });

  it('shows expiry date for time-limited promo codes', async () => {
    const mockPromoCode = {
      promoCodeId: 'promo-1',
      promoCodeCode: 'EARLYBIRD',
      promoCodeDiscountType: 'fixed',
      promoCodeDiscountValue: 1000,
      promoCodeCurrency: 'USD',
      promoCodeValidFrom: '2026-01-01T00:00:00Z',
      promoCodeValidUntil: '2026-12-31T23:59:59Z',
      promoCodeMaxRedemptions: null,
      promoCodeCurrentRedemptions: 0,
      promoCodeIsActive: true,
    };

    validatePromoCode.mockResolvedValue(mockPromoCode);

    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onPromoApplied={mockOnPromoApplied}
      />,
      { wrapper: createWrapper() }
    );

    const input = screen.getByPlaceholderText(/ENTER-CODE-HERE/i);
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
      promoCodeId: 'promo-1',
      promoCodeCode: 'LIMITED',
      promoCodeDiscountType: 'percentage',
      promoCodeDiscountValue: 1500,
      promoCodeCurrency: 'USD',
      promoCodeValidFrom: null,
      promoCodeValidUntil: null,
      promoCodeMaxRedemptions: 100,
      promoCodeCurrentRedemptions: 95,
      promoCodeIsActive: true,
    };

    validatePromoCode.mockResolvedValue(mockPromoCode);

    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onPromoApplied={mockOnPromoApplied}
      />,
      { wrapper: createWrapper() }
    );

    const input = screen.getByPlaceholderText(/ENTER-CODE-HERE/i);
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

    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onPromoApplied={mockOnPromoApplied}
      />,
      { wrapper: createWrapper() }
    );

    const input = screen.getByPlaceholderText(/ENTER-CODE-HERE/i);

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
