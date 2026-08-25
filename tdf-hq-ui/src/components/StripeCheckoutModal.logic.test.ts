import type { SocialTicketTierDTO } from '../api/socialEvents';
import {
  CHECKOUT_MAX_QUANTITY,
  CHECKOUT_MIN_QUANTITY,
  CHECKOUT_STEP_CONFIRMATION,
  CHECKOUT_STEP_PAYMENT,
  CURRENCY_MINOR_UNITS_PER_MAJOR,
  checkoutFormReducer,
  checkoutModalReducer,
  formatTicketTierPrice,
  initialCheckoutFormState,
  initialCheckoutModalState,
  normalizeCheckoutQuantity,
} from './StripeCheckoutModal.logic';

const SAMPLE_TICKET_TIER_PRICE_CENTS = 5 * 5 * CURRENCY_MINOR_UNITS_PER_MAJOR;
const SAMPLE_TICKET_TIER_TOTAL_QUANTITY = CURRENCY_MINOR_UNITS_PER_MAJOR;
const SAMPLE_TICKET_TIER_SOLD_QUANTITY = 0;
const SAMPLE_TICKET_PURCHASE_QUANTITY = 3;
const SAMPLE_NORMALIZED_QUANTITY = 4;
const CHECKOUT_QUANTITY_ABOVE_MAX = CHECKOUT_MAX_QUANTITY + 1;
const CHECKOUT_QUANTITY_ABOVE_MAX_TEXT = String(CHECKOUT_QUANTITY_ABOVE_MAX);

const sampleTicketTier: SocialTicketTierDTO = {
  ticketTierCode: 'general',
  ticketTierName: 'General',
  ticketTierPriceCents: SAMPLE_TICKET_TIER_PRICE_CENTS,
  ticketTierCurrency: 'usd',
  ticketTierQuantityTotal: SAMPLE_TICKET_TIER_TOTAL_QUANTITY,
  ticketTierQuantitySold: SAMPLE_TICKET_TIER_SOLD_QUANTITY,
  ticketTierActive: true,
};

describe('checkout form logic', () => {
  it('formats ticket totals from tier price and quantity', () => {
    expect(formatTicketTierPrice(sampleTicketTier, SAMPLE_TICKET_PURCHASE_QUANTITY)).toBe('USD 75.00');
  });

  it('rejects invalid price formatting inputs at the contract boundary', () => {
    expect(() => formatTicketTierPrice(sampleTicketTier, 0)).toThrow(RangeError);
  });

  it('normalizes buyer quantity input into the allowed checkout range', () => {
    expect(normalizeCheckoutQuantity('')).toBe(CHECKOUT_MIN_QUANTITY);
    expect(normalizeCheckoutQuantity('0')).toBe(CHECKOUT_MIN_QUANTITY);
    expect(normalizeCheckoutQuantity(String(SAMPLE_NORMALIZED_QUANTITY))).toBe(SAMPLE_NORMALIZED_QUANTITY);
    expect(normalizeCheckoutQuantity(CHECKOUT_QUANTITY_ABOVE_MAX_TEXT)).toBe(CHECKOUT_MAX_QUANTITY);
  });

  it('tracks payment submission state explicitly', () => {
    const startedCheckoutFormState = checkoutFormReducer(initialCheckoutFormState, { type: 'submitStarted' });
    expect(startedCheckoutFormState).toEqual({
      processing: true,
      error: null,
    });

    const failedCheckoutFormState = checkoutFormReducer(startedCheckoutFormState, {
      type: 'submitFailed',
      error: 'Payment failed',
    });
    expect(failedCheckoutFormState).toEqual({
      processing: false,
      error: 'Payment failed',
    });
  });
});

describe('checkout modal logic', () => {
  it('updates buyer details without carrying stale errors', () => {
    const namedCheckoutModalState = checkoutModalReducer(
      { ...initialCheckoutModalState, error: 'Required' },
      { type: 'buyerFieldChanged', field: 'name', value: 'Diego' },
    );
    expect(namedCheckoutModalState.buyerDetails.name).toBe('Diego');
    expect(namedCheckoutModalState.error).toBeNull();
  });

  it('moves through buyer submit, payment, success, and reset states', () => {
    const loadingCheckoutModalState = checkoutModalReducer(initialCheckoutModalState, { type: 'buyerSubmitStarted' });
    expect(loadingCheckoutModalState).toMatchObject({
      loading: true,
      error: null,
    });

    const readyCheckoutModalState = checkoutModalReducer(loadingCheckoutModalState, {
      type: 'paymentIntentReady',
      clientSecret: 'pi_secret',
      orderId: 'order-123',
    });
    expect(readyCheckoutModalState).toMatchObject({
      activeStep: CHECKOUT_STEP_PAYMENT,
      loading: false,
      clientSecret: 'pi_secret',
      orderId: 'order-123',
      error: null,
    });

    const succeededCheckoutModalState = checkoutModalReducer(readyCheckoutModalState, { type: 'paymentSucceeded' });
    expect(succeededCheckoutModalState).toMatchObject({
      activeStep: CHECKOUT_STEP_CONFIRMATION,
      loading: false,
      error: null,
    });

    const resetCheckoutModalState = checkoutModalReducer(succeededCheckoutModalState, { type: 'reset' });
    expect(resetCheckoutModalState).toEqual(initialCheckoutModalState);
  });
});
