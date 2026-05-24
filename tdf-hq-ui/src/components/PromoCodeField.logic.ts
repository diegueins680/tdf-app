import type { PromoCodeDTO } from '../api/socialEvents';

export interface PromoCodeState {
  code: string;
  debouncedCode: string;
  validating: boolean;
  validPromo: PromoCodeDTO | null;
  error: string | null;
}

export type PromoCodeAction =
  | { type: 'codeChanged'; code: string }
  | { type: 'debouncedCodeChanged'; code: string }
  | { type: 'validationStarted' }
  | { type: 'validationSucceeded'; promo: PromoCodeDTO }
  | { type: 'validationFailed'; error: string }
  | { type: 'validationReset' }
  | { type: 'cleared' };

export const initialPromoCodeState: PromoCodeState = {
  code: '',
  debouncedCode: '',
  validating: false,
  validPromo: null,
  error: null,
};

export function promoCodeReducer(state: PromoCodeState, action: PromoCodeAction): PromoCodeState {
  let nextPromoCodeState = state;

  switch (action.type) {
    case 'codeChanged':
      nextPromoCodeState = { ...state, code: action.code };
      break;
    case 'debouncedCodeChanged':
      nextPromoCodeState = { ...state, debouncedCode: action.code };
      break;
    case 'validationStarted':
      nextPromoCodeState = { ...state, validating: true, error: null };
      break;
    case 'validationSucceeded':
      nextPromoCodeState = { ...state, validating: false, validPromo: action.promo, error: null };
      break;
    case 'validationFailed':
      nextPromoCodeState = { ...state, validating: false, validPromo: null, error: action.error };
      break;
    case 'validationReset':
      nextPromoCodeState = { ...state, validating: false, validPromo: null, error: null };
      break;
    case 'cleared':
      nextPromoCodeState = initialPromoCodeState;
      break;
    default:
      break;
  }

  return nextPromoCodeState;
}
