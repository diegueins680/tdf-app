import { Alert, AlertTitle, Box, Typography } from '@mui/material';

const LABELS: Record<string, [string, string]> = {
  stripe: ['Tarjeta · Stripe', 'Card · Stripe'],
  datafast: ['Tarjeta · Datafast', 'Card · Datafast'],
  paypal: ['PayPal', 'PayPal'],
  bank_transfer: ['Transferencia bancaria', 'Bank transfer'],
  placetopay_card: ['Tarjeta · PlaceToPay', 'Card · PlaceToPay'],
  placetopay_bank_redirect: ['Pago bancario en línea · PlaceToPay', 'Online bank payment · PlaceToPay'],
  placetopay_deuna_qr: ['QR DeUna! · PlaceToPay', 'DeUna! QR · PlaceToPay'],
  payphone_card: ['Tarjeta · PayPhone', 'Card · PayPhone'],
  payphone_wallet: ['App PayPhone', 'PayPhone app'],
};

interface PaymentAlternativesNoticeProps {
  /** Only methods the current purchase actually supports, including browser requirements. */
  methods: readonly string[];
  /** Offered by the order, but browser availability has not been checked here. */
  methodsToCheck?: readonly string[];
  failedMethod?: string | null;
  /** A failed status read or timeout is not proof that no charge occurred. */
  blocked?: boolean;
  english?: boolean;
  methodLabels?: Readonly<Record<string, string>>;
  availabilityUnknown?: boolean;
}

export default function PaymentAlternativesNotice({
  methods,
  methodsToCheck = [],
  failedMethod,
  blocked = false,
  english = false,
  methodLabels = {},
  availabilityUnknown = false,
}: PaymentAlternativesNoticeProps) {
  const alternatives = [...new Set(availabilityUnknown ? [] : methods)].filter((method) =>
    method !== failedMethod && (Object.prototype.hasOwnProperty.call(LABELS, method)
      || Object.prototype.hasOwnProperty.call(methodLabels, method)));
  const unchecked = [...new Set(availabilityUnknown ? [] : methodsToCheck)].filter((method) =>
    method !== failedMethod && !alternatives.includes(method)
    && (Object.prototype.hasOwnProperty.call(LABELS, method)
      || Object.prototype.hasOwnProperty.call(methodLabels, method)));
  const uncheckedTitle = english
    ? 'Options to check when returning to your order'
    : 'Opciones por comprobar al volver a tu orden';

  return (
    <Alert severity="info" role="status">
      <AlertTitle>{english ? 'Other payment methods for this purchase' : 'Otros medios de pago para esta compra'}</AlertTitle>
      {alternatives.length > 0 ? (
        <>
          <Box component="ul" sx={{ my: 1, pl: 2.5 }}>
            {alternatives.map((method) => (
              <li key={method}>{methodLabels[method] ?? LABELS[method]![english ? 1 : 0]}</li>
            ))}
          </Box>
          <Typography variant="body2">
            {blocked
              ? (english
                ? 'Before choosing another option, verify the original payment status. Do not pay again while it is pending or unconfirmed.'
                : 'Antes de elegir otra opción, verifica el estado del pago original. No vuelvas a pagar mientras esté pendiente o sin confirmar.')
              : (english
                ? 'Choose another option in the payment controls for this purchase.'
                : 'Elige otra opción en los controles de pago de esta compra.')}
          </Typography>
        </>
      ) : unchecked.length === 0 ? (
        <Typography variant="body2">
          {availabilityUnknown ? (english
            ? 'Other payment methods could not be checked. Review the options in the secure payment form or contact TDF with your order reference.'
            : 'No pudimos consultar otros medios de pago. Revisa las opciones del formulario seguro o contacta a TDF con la referencia de tu orden.') : english
            ? 'No other payment method is enabled for this purchase. Contact TDF with your order reference for help.'
            : 'No hay otro medio habilitado para esta compra. Contacta a TDF con la referencia de tu orden para recibir ayuda.'}
          {blocked && (english
            ? ' Verify the original payment before paying again.'
            : ' Verifica el pago original antes de volver a pagar.')}
        </Typography>
      ) : blocked && <Typography variant="body2">{english
        ? 'Verify the original payment before paying again.'
        : 'Verifica el pago original antes de volver a pagar.'}</Typography>}
      {unchecked.length > 0 && (
        <Box component="section" aria-label={uncheckedTitle} sx={{ mt: alternatives.length ? 1.5 : 0 }}>
          <Typography variant="body2" fontWeight={700}>{uncheckedTitle}</Typography>
          <Box component="ul" sx={{ my: 1, pl: 2.5 }}>
            {unchecked.map((method) => <li key={method}>{methodLabels[method] ?? LABELS[method]![english ? 1 : 0]}</li>)}
          </Box>
          <Typography variant="body2">{english
            ? 'Their availability in this browser will be checked when you open the payment form.'
            : 'Su disponibilidad en este navegador se comprobará al abrir el formulario de pago.'}</Typography>
        </Box>
      )}
    </Alert>
  );
}
