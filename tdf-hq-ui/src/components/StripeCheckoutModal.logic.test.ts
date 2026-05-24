import type { SocialTicketTierDTO } from '../api/socialEvents';
import {
  checkoutFormReducer,
  checkoutModalReducer,
  formatTicketTierPrice,
  initialCheckoutFormState,
  initialCheckoutModalState,
  normalizeCheckoutQuantity,
} from './StripeCheckoutModal.logic';

const sampleTicketTier: SocialTicketTierDTO = {
  ticketTierCode: 'general',
  ticketTierName: 'General',
  ticketTierPriceCents: 2500,
  ticketTierCurrency: 'usd',
  ticketTierQuantityTotal: 100,
  ticketTierQuantitySold: 0,
  ticketTierActive: true,
};

describe('checkout form logic', () => {
  it('formats ticket totals from tier price and quantity', () => {
    expect(formatTicketTierPrice(sampleTicketTier, 3)).toBe('USD 75.00');
  });

  it('normalizes buyer quantity input into the allowed checkout range', () => {
    expect(normalizeCheckoutQuantity('')).toBe(1);
    expect(normalizeCheckoutQuantity('0')).toBe(1);
    expect(normalizeCheckoutQuantity('4')).toBe(4);
    expect(normalizeCheckoutQuantity('11')).toBe(10);
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
    });
    expect(readyCheckoutModalState).toMatchObject({
      activeStep: 1,
      loading: false,
      clientSecret: 'pi_secret',
      error: null,
    });

    const succeededCheckoutModalState = checkoutModalReducer(readyCheckoutModalState, { type: 'paymentSucceeded' });
    expect(succeededCheckoutModalState).toMatchObject({
      activeStep: 2,
      loading: false,
      error: null,
    });

    const resetCheckoutModalState = checkoutModalReducer(succeededCheckoutModalState, { type: 'reset' });
    expect(resetCheckoutModalState).toEqual(initialCheckoutModalState);
  });
});
