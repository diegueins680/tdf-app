import { useEffect, useReducer, useRef } from 'react';
import {
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Button,
  TextField,
  Typography,
  Box,
  CircularProgress,
  Alert,
  Stepper,
  Step,
  StepLabel,
  Divider,
} from '@mui/material';
import { Elements, PaymentElement, useStripe, useElements } from '@stripe/react-stripe-js';
import { loadStripe, type StripeElementsOptions } from '@stripe/stripe-js';
import { SocialEventsAPI, type TicketPurchaseWithPromoDTO, type SocialTicketTierDTO } from '../api/socialEvents';
import { PromoCodeField } from './PromoCodeField';

const stripePromise = loadStripe(import.meta.env['VITE_STRIPE_PUBLISHABLE_KEY'] ?? '');

interface StripeCheckoutModalProps {
  open: boolean;
  onClose: () => void;
  eventId: string;
  eventTitle: string;
  tier: SocialTicketTierDTO;
  onSuccess: (orderId: string) => void;
}

interface CheckoutFormProps {
  tier: SocialTicketTierDTO;
  buyerDetails: BuyerDetailsState;
  promoCode: string | null;
  onSuccess: (orderId: string) => void;
  onBack: () => void;
}

interface BuyerDetailsState {
  name: string;
  email: string;
  quantity: number;
}

interface CheckoutFormState {
  processing: boolean;
  error: string | null;
}

interface CheckoutModalState {
  activeStep: number;
  buyerDetails: BuyerDetailsState;
  promoCode: string | null;
  clientSecret: string | null;
  loading: boolean;
  error: string | null;
}

type BuyerField = 'name' | 'email' | 'quantity';

interface DivRef {
  current: HTMLDivElement | null;
}

interface InputRef {
  current: HTMLInputElement | null;
}

interface HTMLElementRef {
  current: HTMLElement | null;
}

interface TimerRef {
  current: number | null;
}

interface StringRef {
  current: string | null;
}

type CheckoutFormAction =
  | { type: 'submitStarted' }
  | { type: 'submitFailed'; error: string };

type CheckoutModalAction =
  | { type: 'buyerFieldChanged'; field: BuyerField; value: string | number }
  | { type: 'promoChanged'; promoCode: string | null }
  | { type: 'buyerSubmitStarted' }
  | { type: 'buyerSubmitFailed'; error: string }
  | { type: 'paymentIntentReady'; clientSecret: string }
  | { type: 'backToBuyerDetails' }
  | { type: 'paymentSucceeded' }
  | { type: 'reset' };

const BUYER_FORM_ID = 'stripe-checkout-buyer-details-form';

const CHECKOUT_COPY = {
  title: 'Purchase Tickets',
  steps: {
    buyer: 'Buyer Details',
    payment: 'Payment',
    confirmation: 'Confirmation',
  },
  labels: {
    name: 'Your Name',
    email: 'Email',
    quantity: 'Quantity',
  },
  helpers: {
    email: 'Tickets will be sent to this email',
    promoPending: 'Promo code will be applied at checkout',
  },
  actions: {
    back: 'Back',
    cancel: 'Cancel',
    continue: 'Continue to Payment',
    close: 'Close',
  },
  errors: {
    requiredFields: 'Please fill in all required fields',
    stripeMissing: 'Stripe not initialized',
    paymentSubmit: 'Payment submission failed',
    paymentFailed: 'Payment failed',
    paymentUnclear: 'Payment status unclear. Please contact support.',
    paymentIntent: 'Failed to create payment intent',
    unexpected: 'An unexpected error occurred',
  },
  status: {
    buyer: 'Buyer',
    quantity: 'Quantity',
    promoApplied: 'Promo code applied',
    total: 'Total',
    success: 'Payment Successful!',
    sent: 'Your tickets have been sent to',
  },
};

const initialBuyerDetails: BuyerDetailsState = {
  name: '',
  email: '',
  quantity: 1,
};

const initialCheckoutFormState: CheckoutFormState = {
  processing: false,
  error: null,
};

const initialCheckoutModalState: CheckoutModalState = {
  activeStep: 0,
  buyerDetails: initialBuyerDetails,
  promoCode: null,
  clientSecret: null,
  loading: false,
  error: null,
};

function checkoutFormReducer(state: CheckoutFormState, action: CheckoutFormAction): CheckoutFormState {
  switch (action.type) {
    case 'submitStarted':
      return { processing: true, error: null };
    case 'submitFailed':
      return { processing: false, error: action.error };
    default:
      return state;
  }
}

function checkoutModalReducer(state: CheckoutModalState, action: CheckoutModalAction): CheckoutModalState {
  switch (action.type) {
    case 'buyerFieldChanged':
      return {
        ...state,
        buyerDetails: {
          ...state.buyerDetails,
          [action.field]: action.value,
        },
        error: null,
      };
    case 'promoChanged':
      return { ...state, promoCode: action.promoCode };
    case 'buyerSubmitStarted':
      return { ...state, loading: true, error: null };
    case 'buyerSubmitFailed':
      return { ...state, loading: false, error: action.error };
    case 'paymentIntentReady':
      return { ...state, activeStep: 1, loading: false, clientSecret: action.clientSecret, error: null };
    case 'backToBuyerDetails':
      return { ...state, activeStep: 0, error: null };
    case 'paymentSucceeded':
      return { ...state, activeStep: 2, loading: false, error: null };
    case 'reset':
      return initialCheckoutModalState;
    default:
      return state;
  }
}

function CheckoutForm({ tier, buyerDetails, promoCode, onSuccess, onBack }: CheckoutFormProps) {
  const stripe = useStripe();
  const elements = useElements();
  const [state, dispatch] = useReducer(checkoutFormReducer, initialCheckoutFormState);
  const paymentSummaryRef = useRef(null) as DivRef;
  const paymentErrorRef = useRef(null) as DivRef;

  useEffect(() => {
    paymentSummaryRef.current?.focus();
  }, []);

  useEffect(() => {
    if (state.error) {
      paymentErrorRef.current?.focus();
    }
  }, [state.error]);

  const submitPayment = async () => {
    if (!stripe || !elements) {
      dispatch({ type: 'submitFailed', error: CHECKOUT_COPY.errors.stripeMissing });
      window.requestAnimationFrame(() => paymentErrorRef.current?.focus());
      return;
    }

    dispatch({ type: 'submitStarted' });

    try {
      const { error: submitError } = await elements.submit();
      if (submitError) {
        dispatch({ type: 'submitFailed', error: submitError.message ?? CHECKOUT_COPY.errors.paymentSubmit });
        return;
      }

      const { error: confirmError, paymentIntent } = await stripe.confirmPayment({
        elements,
        redirect: 'if_required',
      });

      if (confirmError) {
        dispatch({ type: 'submitFailed', error: confirmError.message ?? CHECKOUT_COPY.errors.paymentFailed });
        return;
      }

      if (paymentIntent?.status === 'succeeded') {
        const orderId = String(paymentIntent.metadata?.orderId ?? paymentIntent.id);
        onSuccess(orderId);
        return;
      }

      dispatch({ type: 'submitFailed', error: CHECKOUT_COPY.errors.paymentUnclear });
    } catch (err) {
      dispatch({ type: 'submitFailed', error: err instanceof Error ? err.message : CHECKOUT_COPY.errors.unexpected });
    }
  };

  const handlePaymentFormSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    void submitPayment();
  };

  return (
    <form onSubmit={handlePaymentFormSubmit} data-focus-management="payment-errors">
      <Box ref={paymentSummaryRef} tabIndex={-1} sx={{ mb: 3, outline: 'none' }}>
        <Typography variant="body2" color="text.secondary" gutterBottom>
          {CHECKOUT_COPY.status.buyer}: {buyerDetails.name} ({buyerDetails.email})
        </Typography>
        <Typography variant="body2" color="text.secondary" gutterBottom>
          {CHECKOUT_COPY.status.quantity}: {buyerDetails.quantity} x {tier.ticketTierName}
        </Typography>
        {promoCode && (
          <Typography variant="body2" color="success.main" gutterBottom>
            {CHECKOUT_COPY.status.promoApplied}: {promoCode}
          </Typography>
        )}
      </Box>

      <PaymentElement />

      {state.error && (
        <Alert ref={paymentErrorRef} tabIndex={-1} severity="error" sx={{ mt: 2 }}>
          {state.error}
        </Alert>
      )}

      <Box sx={{ display: 'flex', gap: 2, mt: 3 }}>
        <Button
          disabled={state.processing}
          onClick={onBack}
          onKeyDown={(event) => {
            if (event.key === 'ArrowLeft') {
              event.preventDefault();
              onBack();
            }
          }}
          data-focus-management="buyer-details"
          fullWidth
        >
          {CHECKOUT_COPY.actions.back}
        </Button>
        <Button
          type="submit"
          variant="contained"
          disabled={!stripe || state.processing}
          fullWidth
        >
          {state.processing ? <CircularProgress size={24} /> : `Pay ${formatPrice(tier, buyerDetails.quantity)}`}
        </Button>
      </Box>
    </form>
  );
}

function formatPrice(tier: SocialTicketTierDTO, quantity: number): string {
  const total = tier.ticketTierPriceCents * quantity;
  const currency = (tier.ticketTierCurrency ?? 'USD').toUpperCase();
  return `${currency} ${(total / 100).toFixed(2)}`;
}

function normalizeQuantity(rawValue: string): number {
  const parsed = Number.parseInt(rawValue, 10);
  if (!Number.isFinite(parsed)) return 1;
  return Math.min(10, Math.max(1, parsed));
}

export function StripeCheckoutModal({ open, onClose, eventId, eventTitle, tier, onSuccess }: StripeCheckoutModalProps) {
  const [state, dispatch] = useReducer(checkoutModalReducer, initialCheckoutModalState);
  const returnFocusRef = useRef(null) as HTMLElementRef;
  const nameInputRef = useRef(null) as InputRef;
  const emailInputRef = useRef(null) as InputRef;
  const quantityInputRef = useRef(null) as InputRef;
  const buyerErrorRef = useRef(null) as DivRef;
  const successSummaryRef = useRef(null) as DivRef;
  const successTimerRef = useRef(null) as TimerRef;
  const pendingSuccessOrderIdRef = useRef(null) as StringRef;

  useEffect(() => {
    if (open) {
      returnFocusRef.current = document.activeElement instanceof HTMLElement ? document.activeElement : null;
    }
  }, [open]);

  useEffect(() => {
    if (state.error) {
      buyerErrorRef.current?.focus();
    }
  }, [state.error]);

  useEffect(() => {
    if (state.activeStep === 2) {
      successSummaryRef.current?.focus();
    }
  }, [state.activeStep]);

  useEffect(() => {
    return () => {
      if (successTimerRef.current !== null) {
        window.clearTimeout(successTimerRef.current);
      }
    };
  }, []);

  const submitBuyerDetails = async () => {
    const name = state.buyerDetails.name.trim();
    const email = state.buyerDetails.email.trim();
    const quantity = state.buyerDetails.quantity;
    const invalidField = !name
      ? nameInputRef
      : !email
        ? emailInputRef
        : quantity < 1
          ? quantityInputRef
          : null;

    if (invalidField) {
      dispatch({ type: 'buyerSubmitFailed', error: CHECKOUT_COPY.errors.requiredFields });
      window.requestAnimationFrame(() => invalidField.current?.focus());
      return;
    }

    dispatch({ type: 'buyerSubmitStarted' });

    try {
      const payload: TicketPurchaseWithPromoDTO = {
        tpwpTierId: tier.ticketTierId ?? '',
        tpwpQuantity: quantity,
        tpwpBuyerName: name,
        tpwpBuyerEmail: email,
        tpwpPromoCode: state.promoCode ?? undefined,
      };

      const response = await SocialEventsAPI.createPaymentIntent(payload);
      dispatch({ type: 'paymentIntentReady', clientSecret: response.spiClientSecret });
    } catch (err) {
      dispatch({
        type: 'buyerSubmitFailed',
        error: err instanceof Error ? err.message : CHECKOUT_COPY.errors.paymentIntent,
      });
    }
  };

  const handleBuyerDetailsSubmit = (e?: React.FormEvent) => {
    e?.preventDefault();
    void submitBuyerDetails();
  };

  const restoreFocusAfterClose = () => {
    window.requestAnimationFrame(() => returnFocusRef.current?.focus());
  };

  const completePendingSuccess = () => {
    const orderId = pendingSuccessOrderIdRef.current;
    pendingSuccessOrderIdRef.current = null;
    if (orderId) {
      onSuccess(orderId);
    }
  };

  const handleClose = () => {
    if (successTimerRef.current !== null) {
      window.clearTimeout(successTimerRef.current);
      successTimerRef.current = null;
    }
    completePendingSuccess();
    dispatch({ type: 'reset' });
    onClose();
    restoreFocusAfterClose();
  };

  const handlePaymentSuccess = (orderId: string) => {
    pendingSuccessOrderIdRef.current = orderId;
    dispatch({ type: 'paymentSucceeded' });
    successTimerRef.current = window.setTimeout(() => {
      successTimerRef.current = null;
      handleClose();
    }, 2000);
  };

  const handleBackToBuyerDetails = () => {
    dispatch({ type: 'backToBuyerDetails' });
    window.requestAnimationFrame(() => nameInputRef.current?.focus());
  };

  const stripeOptions: StripeElementsOptions = state.clientSecret
    ? {
        clientSecret: state.clientSecret,
        appearance: {
          theme: 'stripe',
        },
      }
    : {};

  return (
    <Dialog open={open} onClose={handleClose} maxWidth="sm" fullWidth aria-labelledby="stripe-checkout-title">
      <DialogTitle id="stripe-checkout-title">
        {CHECKOUT_COPY.title} - {eventTitle}
        <Typography variant="body2" color="text.secondary">
          {tier.ticketTierName}
        </Typography>
      </DialogTitle>

      <DialogContent>
        <Stepper activeStep={state.activeStep} sx={{ mb: 3 }}>
          <Step>
            <StepLabel>{CHECKOUT_COPY.steps.buyer}</StepLabel>
          </Step>
          <Step>
            <StepLabel>{CHECKOUT_COPY.steps.payment}</StepLabel>
          </Step>
          <Step>
            <StepLabel>{CHECKOUT_COPY.steps.confirmation}</StepLabel>
          </Step>
        </Stepper>

        {state.activeStep === 0 && (
          <Box
            id={BUYER_FORM_ID}
            component="form"
            onSubmit={handleBuyerDetailsSubmit}
            data-focus-management="buyer-details-errors"
          >
            <TextField
              label={CHECKOUT_COPY.labels.name}
              fullWidth
              required
              inputRef={nameInputRef}
              value={state.buyerDetails.name}
              onChange={(e) => dispatch({ type: 'buyerFieldChanged', field: 'name', value: e.target.value })}
              margin="normal"
            />
            <TextField
              label={CHECKOUT_COPY.labels.email}
              type="email"
              fullWidth
              required
              inputRef={emailInputRef}
              value={state.buyerDetails.email}
              onChange={(e) => dispatch({ type: 'buyerFieldChanged', field: 'email', value: e.target.value })}
              margin="normal"
              helperText={CHECKOUT_COPY.helpers.email}
            />
            <TextField
              label={CHECKOUT_COPY.labels.quantity}
              type="number"
              fullWidth
              required
              inputRef={quantityInputRef}
              value={state.buyerDetails.quantity}
              onChange={(e) =>
                dispatch({ type: 'buyerFieldChanged', field: 'quantity', value: normalizeQuantity(e.target.value) })
              }
              margin="normal"
              InputProps={{ inputProps: { min: 1, max: 10 } }}
            />

            <Divider sx={{ my: 2 }} />

            <PromoCodeField
              eventId={eventId}
              tierId={tier.ticketTierId ?? ''}
              onPromoApplied={(promoCode) => dispatch({ type: 'promoChanged', promoCode })}
            />

            <Box sx={{ mt: 3 }}>
              <Typography variant="h6">
                {CHECKOUT_COPY.status.total}: {formatPrice(tier, state.buyerDetails.quantity)}
              </Typography>
              {state.promoCode && (
                <Typography variant="body2" color="success.main">
                  {CHECKOUT_COPY.helpers.promoPending}
                </Typography>
              )}
            </Box>

            {state.error && (
              <Alert ref={buyerErrorRef} tabIndex={-1} severity="error" sx={{ mt: 2 }}>
                {state.error}
              </Alert>
            )}
          </Box>
        )}

        {state.activeStep === 1 && state.clientSecret && (
          <Elements stripe={stripePromise} options={stripeOptions}>
            <CheckoutForm
              tier={tier}
              buyerDetails={state.buyerDetails}
              promoCode={state.promoCode}
              onSuccess={handlePaymentSuccess}
              onBack={handleBackToBuyerDetails}
            />
          </Elements>
        )}

        {state.activeStep === 2 && (
          <Box ref={successSummaryRef} tabIndex={-1} sx={{ textAlign: 'center', py: 4, outline: 'none' }}>
            <Typography variant="h5" gutterBottom color="success.main">
              {CHECKOUT_COPY.status.success}
            </Typography>
            <Typography variant="body1" color="text.secondary">
              {CHECKOUT_COPY.status.sent} {state.buyerDetails.email}
            </Typography>
            <CircularProgress sx={{ mt: 2 }} />
          </Box>
        )}
      </DialogContent>

      <DialogActions>
        {state.activeStep === 0 && (
          <>
            <Button
              disabled={state.loading}
              onClick={handleClose}
              onKeyDown={(event) => {
                if (event.key === 'Escape') {
                  event.preventDefault();
                  event.stopPropagation();
                  handleClose();
                }
              }}
              data-focus-management="dialog-restore"
            >
              {CHECKOUT_COPY.actions.cancel}
            </Button>
            <Button
              type="submit"
              form={BUYER_FORM_ID}
              variant="contained"
              disabled={state.loading}
            >
              {state.loading ? <CircularProgress size={24} /> : CHECKOUT_COPY.actions.continue}
            </Button>
          </>
        )}
        {state.activeStep === 2 && (
          <Button
            disabled={state.loading}
            onClick={handleClose}
            onKeyDown={(event) => {
              if (event.key === 'Escape') {
                event.preventDefault();
                event.stopPropagation();
                handleClose();
              }
            }}
            data-focus-management="dialog-restore"
            variant="contained"
          >
            {CHECKOUT_COPY.actions.close}
          </Button>
        )}
      </DialogActions>
    </Dialog>
  );
}
