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

export const CHECKOUT_MIN_QUANTITY = 1;
export const CHECKOUT_MAX_QUANTITY = 10;
export const CHECKOUT_STEP_BUYER_DETAILS = 0;
export const CHECKOUT_STEP_PAYMENT = 1;
export const CHECKOUT_STEP_CONFIRMATION = 2;
export const CURRENCY_MINOR_UNITS_PER_MAJOR = 100;

export const initialBuyerDetails: BuyerDetailsState = {
  name: '',
  email: '',
  quantity: CHECKOUT_MIN_QUANTITY,
};

export const initialCheckoutFormState: CheckoutFormState = {
  processing: false,
  error: null,
};

export const initialCheckoutModalState: CheckoutModalState = {
  activeStep: CHECKOUT_STEP_BUYER_DETAILS,
  buyerDetails: initialBuyerDetails,
  promoCode: null,
  clientSecret: null,
  loading: false,
  error: null,
};

export function checkoutFormReducer(state: CheckoutFormState, action: CheckoutFormAction): CheckoutFormState {
  /*
   * Contract:
   * precondition: state is a CheckoutFormState and action is a CheckoutFormAction variant.
   * invariant: processing is true only while submission is in flight.
   * postcondition: submit failure records an error and leaves processing false.
   */
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
  /*
   * Contract:
   * precondition: state is a CheckoutModalState and action is a CheckoutModalAction variant.
   * invariant: activeStep remains within the buyer, payment, and confirmation steps.
   * postcondition: reset returns the initial modal state.
   */
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
      nextCheckoutModalState = {
        ...state,
        activeStep: CHECKOUT_STEP_PAYMENT,
        loading: false,
        clientSecret: action.clientSecret,
        error: null,
      };
      break;
    case 'backToBuyerDetails':
      nextCheckoutModalState = { ...state, activeStep: CHECKOUT_STEP_BUYER_DETAILS, error: null };
      break;
    case 'paymentSucceeded':
      nextCheckoutModalState = {
        ...state,
        activeStep: CHECKOUT_STEP_CONFIRMATION,
        loading: false,
        error: null,
      };
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
  /*
   * precondition: tier price is finite minor-unit currency and quantity has already been normalized.
   * postcondition: output uses major units.
   */
  if (!Number.isFinite(tier.ticketTierPriceCents) || tier.ticketTierPriceCents < 0) {
    throw new RangeError('Ticket tier price must be a non-negative finite number of currency minor units.');
  }
  if (!Number.isInteger(quantity) || quantity < CHECKOUT_MIN_QUANTITY) {
    throw new RangeError('Checkout quantity must be a positive integer.');
  }

  const totalCents = tier.ticketTierPriceCents * quantity;
  const currencyCode = (tier.ticketTierCurrency ?? 'USD').toUpperCase();
  return `${currencyCode} ${(totalCents / CURRENCY_MINOR_UNITS_PER_MAJOR).toFixed(2)}`;
}

export function normalizeCheckoutQuantity(rawValue: string): number {
  /*
   * precondition: rawValue is the raw quantity input text.
   * invariant: decimal parsing is explicit.
   * postcondition: result stays in range.
   */
  const parsedQuantity = Number.parseInt(rawValue, 10);
  if (!Number.isFinite(parsedQuantity)) {
    return CHECKOUT_MIN_QUANTITY;
  }

  return Math.min(CHECKOUT_MAX_QUANTITY, Math.max(CHECKOUT_MIN_QUANTITY, parsedQuantity));
}
