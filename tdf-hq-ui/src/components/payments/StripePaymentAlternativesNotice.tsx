import { useEffect, useState } from 'react';
import { useStripe } from '@stripe/react-stripe-js';
import { Alert } from '@mui/material';
import PaymentAlternativesNotice from './PaymentAlternativesNotice';

const STRIPE_LABELS: Record<string, [string, string]> = {
  card: ['Tarjeta', 'Card'], link: ['Link', 'Link'], paypal: ['PayPal', 'PayPal'],
  us_bank_account: ['Cuenta bancaria de EE. UU.', 'US bank account'],
  sepa_debit: ['Débito SEPA', 'SEPA debit'], customer_balance: ['Transferencia bancaria', 'Bank transfer'],
  cashapp: ['Cash App Pay', 'Cash App Pay'], amazon_pay: ['Amazon Pay', 'Amazon Pay'],
  klarna: ['Klarna', 'Klarna'], affirm: ['Affirm', 'Affirm'],
  afterpay_clearpay: ['Afterpay / Clearpay', 'Afterpay / Clearpay'],
};

/** Stripe may enable several methods on one PaymentIntent. Reuse that intent. */
export default function StripePaymentAlternativesNotice({ clientSecret, failedMethod, english = false }: {
  clientSecret: string;
  failedMethod: string | null;
  english?: boolean;
}) {
  const stripe = useStripe();
  const [result, setResult] = useState<{ secret: string; methods: string[]; status: string } | null>(null);
  useEffect(() => {
    let active = true;
    if (typeof stripe?.retrievePaymentIntent !== 'function') return undefined;
    void stripe.retrievePaymentIntent(clientSecret).then(({ paymentIntent }) => {
      if (!active || paymentIntent?.client_secret !== clientSecret) return;
      const canChoose = paymentIntent.status === 'requires_payment_method';
      setResult({ secret: clientSecret, methods: canChoose ? paymentIntent.payment_method_types : [], status: paymentIntent.status });
    }).catch(() => { /* An unknown result must never become a confirmed decline. */ });
    return () => { active = false; };
  }, [clientSecret, stripe]);
  const current = result?.secret === clientSecret ? result : null;
  const methods = current?.methods.map((method) => `stripe_${method}`) ?? [];
  const labels = Object.fromEntries((current?.methods ?? []).map((method) => [
    `stripe_${method}`,
    `${STRIPE_LABELS[method]?.[english ? 1 : 0] ?? method.replace(/_/g, ' ')} · Stripe`,
  ]));
  if (current && current.status !== 'requires_payment_method') {
    return <Alert severity="info">{english
      ? 'Verify the original payment in your order before paying again. Stripe has not reported a retryable decline.'
      : 'Verifica el pago original en tu orden antes de volver a pagar. Stripe no ha informado un rechazo que permita reintentar.'}</Alert>;
  }
  return <PaymentAlternativesNotice methods={methods} methodLabels={labels}
    failedMethod={failedMethod ? `stripe_${failedMethod}` : null}
    blocked={!current} availabilityUnknown={!current} english={english} />;
}
