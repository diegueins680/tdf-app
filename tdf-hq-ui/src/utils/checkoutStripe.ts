import { loadStripe, type Stripe } from '@stripe/stripe-js';

/** Resolve a usable payment client before requesting any inventory reservation. */
export function loadCheckoutStripe(): Promise<Stripe | null> {
  const key = import.meta.env?.VITE_STRIPE_PUBLISHABLE_KEY?.trim();
  return key ? loadStripe(key).catch(() => null) : Promise.resolve(null);
}
