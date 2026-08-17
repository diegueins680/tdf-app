import { useCallback, useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Stack,
  Typography,
} from '@mui/material';
import { Link as RouterLink, useLocation, useParams } from 'react-router-dom';
import { DateTime } from 'luxon';
import {
  Bookings,
  loadPublicBookingLookupToken,
  type PublicBookingCheckoutDTO,
} from '../api/bookings';
import { useMetaTags } from '../hooks/useMetaTags';

const paidStatuses = new Set(['paid', 'partially_refunded', 'refunded']);

const statusCopy = (status: string): { title: string; detail: string; severity: 'success' | 'info' | 'warning' } => {
  if (status === 'paid') {
    return {
      title: 'Depósito verificado',
      detail: 'El servidor verificó el pago. La reserva está confirmada; el saldo y la prestación siguen su propio flujo.',
      severity: 'success',
    };
  }
  if (status === 'partially_refunded') {
    return {
      title: 'Depósito parcialmente reembolsado',
      detail: 'El pago fue verificado y existe un reembolso parcial registrado. La prestación se muestra por separado.',
      severity: 'info',
    };
  }
  if (status === 'refunded') {
    return {
      title: 'Depósito reembolsado',
      detail: 'El pago fue verificado y después reembolsado. Esto no implica por sí solo que la reserva esté cancelada.',
      severity: 'info',
    };
  }
  if (status === 'disputed' || status === 'chargeback') {
    return {
      title: 'Pago en disputa',
      detail: 'El depósito tiene una disputa o contracargo registrado. Contacta a soporte y consulta por separado el estado de la reserva.',
      severity: 'warning',
    };
  }
  if (status === 'processing') {
    return {
      title: 'Pago en verificación',
      detail: 'El proveedor todavía no confirmó el resultado. Esta pantalla no representa un cobro exitoso.',
      severity: 'info',
    };
  }
  if (status === 'failed') {
    return {
      title: 'Intento no confirmado',
      detail: 'No existe evidencia verificable de pago. Puedes reintentar mientras la retención siga vigente.',
      severity: 'warning',
    };
  }
  if (status === 'expired' || status === 'cancelled') {
    return {
      title: 'Retención finalizada',
      detail: 'Este horario ya no está retenido. Si el proveedor muestra un movimiento, contacta a soporte para conciliación.',
      severity: 'warning',
    };
  }
  return {
    title: 'Depósito pendiente',
    detail: 'La orden existe, pero aún no hay un pago confirmado por el servidor.',
    severity: 'info',
  };
};

export default function PublicBookingOrderTrackingPage() {
  useMetaTags({
    title: 'Estado de reserva',
    description: 'Consulta segura del pago y la prestación de una reserva TDF.',
  });
  const { bookingId: rawBookingId } = useParams<{ bookingId: string }>();
  const location = useLocation();
  const bookingId = Number(rawBookingId);
  const validBookingId = Number.isSafeInteger(bookingId) && bookingId > 0;
  const lookupToken = validBookingId ? loadPublicBookingLookupToken(bookingId) : null;
  const resourcePath = useMemo(
    () => new URLSearchParams(location.search).get('resourcePath'),
    [location.search],
  );
  const [order, setOrder] = useState<PublicBookingCheckoutDTO | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const refresh = useCallback(async () => {
    if (!validBookingId || !lookupToken) {
      setError('No encontramos un acceso seguro para esta orden en este navegador.');
      setLoading(false);
      return;
    }
    setLoading(true);
    setError(null);
    try {
      const next = resourcePath
        ? await Bookings.confirmPublicDatafastStatus(bookingId, resourcePath, lookupToken)
        : await Bookings.getPublicCheckout(bookingId, lookupToken);
      setOrder(next);
    } catch {
      setError('No pudimos verificar esta orden. Revisa el enlace o vuelve a intentar; no se confirmó ningún pago.');
    } finally {
      setLoading(false);
    }
  }, [bookingId, lookupToken, resourcePath, validBookingId]);

  useEffect(() => {
    void refresh();
  }, [refresh]);

  const paymentCopy = order ? statusCopy(order.paymentStatus) : null;
  const holdLabel = order
    ? DateTime.fromISO(order.holdExpiresAt).toLocaleString(DateTime.DATETIME_MED)
    : null;

  return (
    <Box sx={{ minHeight: '70vh', display: 'flex', alignItems: 'center', justifyContent: 'center', py: 5 }}>
      <Card sx={{ width: '100%', maxWidth: 720, borderRadius: 3 }}>
        <CardContent sx={{ p: { xs: 3, md: 5 } }}>
          <Stack spacing={2.5}>
            <Stack spacing={0.5}>
              <Typography variant="overline" color="text.secondary">Reserva TDF</Typography>
              <Typography variant="h4" fontWeight={800}>Estado de tu orden</Typography>
              <Typography color="text.secondary">
                Pago y prestación se muestran por separado. Un retorno del navegador nunca confirma el depósito.
              </Typography>
            </Stack>

            {loading && (
              <Stack direction="row" spacing={1.5} alignItems="center" role="status">
                <CircularProgress size={22} />
                <Typography>Consultando evidencia del servidor…</Typography>
              </Stack>
            )}

            {error && <Alert severity="warning">{error}</Alert>}

            {order && paymentCopy && (
              <>
                <Alert severity={paymentCopy.severity}>
                  <Typography fontWeight={800}>{paymentCopy.title}</Typography>
                  <Typography variant="body2">{paymentCopy.detail}</Typography>
                </Alert>
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                  <Chip label={`Orden ${order.booking.bookingId}`} />
                  <Chip label={`Pago: ${order.paymentStatus}`} color={paidStatuses.has(order.paymentStatus) ? 'success' : 'default'} />
                  <Chip label={`Servicio: ${order.fulfillmentStatus}`} />
                  <Chip label={`Retención: ${holdLabel}`} />
                </Stack>
                <Typography>
                  Depósito: <strong>{order.quote.currency} {(order.quote.depositMinor / 100).toFixed(2)}</strong>
                  {' '}· Saldo posterior: <strong>{order.quote.currency} {(order.quote.balanceMinor / 100).toFixed(2)}</strong>
                </Typography>
                {order.manualPayment?.status === 'awaiting_evidence' && (
                  <Alert severity="info" variant="outlined">
                    Elegiste transferencia bancaria, pero todavía no enviaste una referencia para revisión. No existe un pago confirmado.
                  </Alert>
                )}
                {order.manualPayment?.status === 'submitted' && (
                  <Alert severity="info" variant="outlined">
                    La referencia bancaria fue recibida y espera revisión independiente. Esto todavía no confirma el depósito.
                  </Alert>
                )}
                {order.manualPayment?.status === 'under_review' && (
                  <Alert severity="info" variant="outlined">
                    La referencia bancaria está bajo revisión financiera. La reserva permanece pendiente hasta una aprobación verificada.
                  </Alert>
                )}
                {order.manualPayment?.status === 'rejected' && (
                  <Alert severity="warning" variant="outlined">
                    La evidencia bancaria fue rechazada. Regresa al flujo de reserva o contacta a TDF para corregirla; no se confirmó ningún pago.
                  </Alert>
                )}
              </>
            )}

            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
              <Button variant="contained" onClick={() => void refresh()} disabled={loading || !lookupToken}>
                Verificar de nuevo
              </Button>
              <Button variant="outlined" component={RouterLink} to="/reservar">
                Nueva reserva
              </Button>
            </Stack>
          </Stack>
        </CardContent>
      </Card>
    </Box>
  );
}
