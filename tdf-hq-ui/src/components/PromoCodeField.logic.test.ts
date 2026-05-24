import type { PromoCodeDTO } from '../api/socialEvents';
import { initialPromoCodeState, promoCodeReducer } from './PromoCodeField.logic';

const activePromoCode: PromoCodeDTO = {
  promoCodeCode: 'SAVE10',
  promoCodeDiscountType: 'percentage',
  promoCodeDiscountValue: 1000,
  promoCodeCurrency: 'USD',
  promoCodeCurrentRedemptions: 0,
  promoCodeIsActive: true,
};

describe('promoCodeReducer', () => {
  it('tracks code entry and successful validation explicitly', () => {
    const codeChangedPromoState = promoCodeReducer(initialPromoCodeState, {
      type: 'codeChanged',
      code: 'SAVE10',
    });
    expect(codeChangedPromoState).toMatchObject({
      code: 'SAVE10',
      validating: false,
      validPromo: null,
      error: null,
    });

    const startedPromoValidationState = promoCodeReducer(codeChangedPromoState, { type: 'validationStarted' });
    expect(startedPromoValidationState).toMatchObject({
      validating: true,
      error: null,
    });

    const succeededPromoValidationState = promoCodeReducer(startedPromoValidationState, {
      type: 'validationSucceeded',
      promo: activePromoCode,
    });
    expect(succeededPromoValidationState).toMatchObject({
      validating: false,
      validPromo: activePromoCode,
      error: null,
    });
  });

  it('clears stale promo data on validation failure, reset, and manual clear', () => {
    const failedPromoValidationState = promoCodeReducer(
      { ...initialPromoCodeState, validating: true, validPromo: activePromoCode },
      { type: 'validationFailed', error: 'Invalid promo code' },
    );
    expect(failedPromoValidationState).toMatchObject({
      validating: false,
      validPromo: null,
      error: 'Invalid promo code',
    });

    const resetPromoValidationState = promoCodeReducer(failedPromoValidationState, { type: 'validationReset' });
    expect(resetPromoValidationState).toMatchObject({
      validating: false,
      validPromo: null,
      error: null,
    });

    const clearedPromoValidationState = promoCodeReducer(
      { ...resetPromoValidationState, code: 'SAVE10', debouncedCode: 'SAVE10' },
      { type: 'cleared' },
    );
    expect(clearedPromoValidationState).toEqual(initialPromoCodeState);
  });
});
