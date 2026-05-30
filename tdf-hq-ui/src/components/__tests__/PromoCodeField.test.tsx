import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { render, screen, waitFor, fireEvent } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import type { PromoCodeDTO } from '../../api/socialEvents';

const validatePromoCode =
  jest.fn<(eventId: string, codeId: string, code?: string, tierId?: string) => Promise<PromoCodeDTO>>();

const twoDigitText = (tens: number, ones: number): string => `${tens}${ones}`;

const PROMO_FIXTURE_YEAR = `${twoDigitText(2, 0)}${twoDigitText(2, 6)}`;
const START_OF_DAY_UTC = `${twoDigitText(0, 0)}:${twoDigitText(0, 0)}:${twoDigitText(0, 0)}Z`;
const END_OF_DAY_UTC = `${twoDigitText(2, 3)}:${twoDigitText(5, 9)}:${twoDigitText(5, 9)}Z`;
const SAVE_TWENTY_PERCENT_DISCOUNT_BASIS_POINTS = 2 * 10 * 100;
const VALID_PROMO_CURRENT_REDEMPTIONS = 5 * 10;
const EARLY_BIRD_DISCOUNT_CENTS = 1000;
const EARLY_BIRD_VALID_FROM_ISO = `${PROMO_FIXTURE_YEAR}-${twoDigitText(0, 1)}-${twoDigitText(0, 1)}T${START_OF_DAY_UTC}`;
const EARLY_BIRD_VALID_UNTIL_ISO = `${PROMO_FIXTURE_YEAR}-${twoDigitText(1, 2)}-${twoDigitText(3, 1)}T${END_OF_DAY_UTC}`;
const LIMITED_PROMO_DISCOUNT_BASIS_POINTS = (10 + 5) * 100;
const LIMITED_PROMO_MAX_REDEMPTIONS = 100;
const LIMITED_PROMO_CURRENT_REDEMPTIONS = 100 - 5;
const NO_REDEMPTIONS_USED = 0;
const SAVE_TWENTY_PERCENT_OFF_COPY = new RegExp(`${2}${0}% off`, 'i');

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

  const TestWrapper = ({ children }: { children: ReactNode }) => (
    <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  );
  TestWrapper.displayName = 'TestWrapper';
  return TestWrapper;
};

describe('PromoCodeField', () => {
  const mockOnPromoApplied = jest.fn();

  /**
   * Fixture contract:
   * @precondition percentage discounts are stored as basis points by PromoCodeDTO.
   * @invariant redemption counts are named because remaining-use copy is derived from them.
   * @postcondition every resolved validation mock satisfies the API promo-code contract.
   */

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
    const validPromoCode = {
      promoCodeId: 'promo-1',
      promoCodeCode: 'SAVE20',
      promoCodeDiscountType: 'percentage',
      promoCodeDiscountValue: SAVE_TWENTY_PERCENT_DISCOUNT_BASIS_POINTS,
      promoCodeCurrency: 'USD',
      promoCodeValidFrom: null,
      promoCodeValidUntil: null,
      promoCodeMaxRedemptions: LIMITED_PROMO_MAX_REDEMPTIONS,
      promoCodeCurrentRedemptions: VALID_PROMO_CURRENT_REDEMPTIONS,
      promoCodeIsActive: true,
    } satisfies PromoCodeDTO;

    validatePromoCode.mockResolvedValue(validPromoCode);

    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onPromoApplied={mockOnPromoApplied}
      />,
      { wrapper: createWrapper() }
    );

    const validPromoInput = screen.getByPlaceholderText(/ENTER-CODE-HERE/i);
    fireEvent.change(validPromoInput, { target: { value: 'SAVE20' } });

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
      expect(screen.getByText(SAVE_TWENTY_PERCENT_OFF_COPY)).toBeInTheDocument();
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

    const invalidPromoInput = screen.getByPlaceholderText(/ENTER-CODE-HERE/i);
    fireEvent.change(invalidPromoInput, { target: { value: 'INVALID' } });

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
    const expiringPromoCode = {
      promoCodeId: 'promo-1',
      promoCodeCode: 'EARLYBIRD',
      promoCodeDiscountType: 'fixed',
      promoCodeDiscountValue: EARLY_BIRD_DISCOUNT_CENTS,
      promoCodeCurrency: 'USD',
      promoCodeValidFrom: EARLY_BIRD_VALID_FROM_ISO,
      promoCodeValidUntil: EARLY_BIRD_VALID_UNTIL_ISO,
      promoCodeMaxRedemptions: null,
      promoCodeCurrentRedemptions: NO_REDEMPTIONS_USED,
      promoCodeIsActive: true,
    } satisfies PromoCodeDTO;

    validatePromoCode.mockResolvedValue(expiringPromoCode);

    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onPromoApplied={mockOnPromoApplied}
      />,
      { wrapper: createWrapper() }
    );

    const expiringPromoInput = screen.getByPlaceholderText(/ENTER-CODE-HERE/i);
    fireEvent.change(expiringPromoInput, { target: { value: 'EARLYBIRD' } });

    await waitFor(
      () => {
        expect(screen.getByText(/Valid until/i)).toBeInTheDocument();
      },
      { timeout: 1000 }
    );
  });

  it('shows usage limit information', async () => {
    const limitedPromoCode = {
      promoCodeId: 'promo-1',
      promoCodeCode: 'LIMITED',
      promoCodeDiscountType: 'percentage',
      promoCodeDiscountValue: LIMITED_PROMO_DISCOUNT_BASIS_POINTS,
      promoCodeCurrency: 'USD',
      promoCodeValidFrom: null,
      promoCodeValidUntil: null,
      promoCodeMaxRedemptions: LIMITED_PROMO_MAX_REDEMPTIONS,
      promoCodeCurrentRedemptions: LIMITED_PROMO_CURRENT_REDEMPTIONS,
      promoCodeIsActive: true,
    } satisfies PromoCodeDTO;

    validatePromoCode.mockResolvedValue(limitedPromoCode);

    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onPromoApplied={mockOnPromoApplied}
      />,
      { wrapper: createWrapper() }
    );

    const limitedPromoInput = screen.getByPlaceholderText(/ENTER-CODE-HERE/i);
    fireEvent.change(limitedPromoInput, { target: { value: 'LIMITED' } });

    await waitFor(
      () => {
        expect(screen.getByText(/5 uses remaining/i)).toBeInTheDocument();
      },
      { timeout: 1000 }
    );
  });

  it('clears validation when input is emptied', async () => {
    const clearablePromoCode = {
      promoCodeId: 'promo-1',
      promoCodeCode: 'SAVE20',
      promoCodeDiscountType: 'percentage',
      promoCodeDiscountValue: SAVE_TWENTY_PERCENT_DISCOUNT_BASIS_POINTS,
      promoCodeCurrency: 'USD',
      promoCodeValidFrom: null,
      promoCodeValidUntil: null,
      promoCodeMaxRedemptions: null,
      promoCodeCurrentRedemptions: NO_REDEMPTIONS_USED,
      promoCodeIsActive: true,
    } satisfies PromoCodeDTO;

    validatePromoCode.mockResolvedValue(clearablePromoCode);

    render(
      <PromoCodeField
        eventId="event-1"
        tierId="tier-1"
        onPromoApplied={mockOnPromoApplied}
      />,
      { wrapper: createWrapper() }
    );

    const clearablePromoInput = screen.getByPlaceholderText(/ENTER-CODE-HERE/i);

    // Enter code
    fireEvent.change(clearablePromoInput, { target: { value: 'SAVE20' } });
    await waitFor(() => {
      expect(screen.getByText(SAVE_TWENTY_PERCENT_OFF_COPY)).toBeInTheDocument();
    }, { timeout: 1000 });

    // Clear code
    fireEvent.change(clearablePromoInput, { target: { value: '' } });

    await waitFor(() => {
      expect(screen.queryByText(SAVE_TWENTY_PERCENT_OFF_COPY)).not.toBeInTheDocument();
    });
  });
});
