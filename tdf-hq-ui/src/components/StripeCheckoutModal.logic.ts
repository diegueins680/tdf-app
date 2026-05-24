import type { SocialTicketTierDTO } from '../api/socialEvents';

export interface BuyerDetailsState {
  name: string;
  email: string;
  quantity: number;
}

export interface CheckoutFormState {
  processing: boolean;
  error: string | null;
}

export interface CheckoutModalState {
  activeStep: number;
  buyerDetails: BuyerDetailsState;
  promoCode: string | null;
  clientSecret: string | null;
  loading: boolean;
  error: string | null;
}

export type BuyerField = 'name' | 'email' | 'quantity';

export type CheckoutFormAction =
  | { type: 'submitStarted' }
  | { type: 'submitFailed'; error: string };

export type CheckoutModalAction =
  | { type: 'buyerFieldChanged'; field: BuyerField; value: string | number }
  | { type: 'promoChanged'; promoCode: string | null }
  | { type: 'buyerSubmitStarted' }
  | { type: 'buyerSubmitFailed'; error: string }
  | { type: 'paymentIntentReady'; clientSecret: string }
  | { type: 'backToBuyerDetails' }
  | { type: 'paymentSucceeded' }
  | { type: 'reset' };

export const initialBuyerDetails: BuyerDetailsState = {
  name: '',
  email: '',
  quantity: 1,
};

export const initialCheckoutFormState: CheckoutFormState = {
  processing: false,
  error: null,
};

export const initialCheckoutModalState: CheckoutModalState = {
  activeStep: 0,
  buyerDetails: initialBuyerDetails,
  promoCode: null,
  clientSecret: null,
  loading: false,
  error: null,
};

export function checkoutFormReducer(state: CheckoutFormState, action: CheckoutFormAction): CheckoutFormState {
  let nextCheckoutFormState = state;

  switch (action.type) {
    case 'submitStarted':
      nextCheckoutFormState = { processing: true, error: null };
      break;
    case 'submitFailed':
      nextCheckoutFormState = { processing: false, error: action.error };
      break;
    default:
      break;
  }

  return nextCheckoutFormState;
}

export function checkoutModalReducer(state: CheckoutModalState, action: CheckoutModalAction): CheckoutModalState {
  let nextCheckoutModalState = state;

  switch (action.type) {
    case 'buyerFieldChanged':
      nextCheckoutModalState = {
        ...state,
        buyerDetails: {
          ...state.buyerDetails,
          [action.field]: action.value,
        },
        error: null,
      };
      break;
    case 'promoChanged':
      nextCheckoutModalState = { ...state, promoCode: action.promoCode };
      break;
    case 'buyerSubmitStarted':
      nextCheckoutModalState = { ...state, loading: true, error: null };
      break;
    case 'buyerSubmitFailed':
      nextCheckoutModalState = { ...state, loading: false, error: action.error };
      break;
    case 'paymentIntentReady':
      nextCheckoutModalState = { ...state, activeStep: 1, loading: false, clientSecret: action.clientSecret, error: null };
      break;
    case 'backToBuyerDetails':
      nextCheckoutModalState = { ...state, activeStep: 0, error: null };
      break;
    case 'paymentSucceeded':
      nextCheckoutModalState = { ...state, activeStep: 2, loading: false, error: null };
      break;
    case 'reset':
      nextCheckoutModalState = initialCheckoutModalState;
      break;
    default:
      break;
  }

  return nextCheckoutModalState;
}

export function formatTicketTierPrice(tier: SocialTicketTierDTO, quantity: number): string {
  const totalCents = tier.ticketTierPriceCents * quantity;
  const currencyCode = (tier.ticketTierCurrency ?? 'USD').toUpperCase();
  return `${currencyCode} ${(totalCents / 100).toFixed(2)}`;
}

export function normalizeCheckoutQuantity(rawValue: string): number {
  const parsedQuantity = Number.parseInt(rawValue, 10);
  if (!Number.isFinite(parsedQuantity)) {
    return 1;
  }

  return Math.min(10, Math.max(1, parsedQuantity));
}
