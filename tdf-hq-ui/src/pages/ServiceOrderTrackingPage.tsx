import { useMemo } from 'react';
import { Alert, Box, Button, Card, CardContent, Chip, CircularProgress, Divider, Stack, Typography } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { Link as RouterLink, useParams } from 'react-router-dom';
import { ServiceStorefront } from '../api/serviceStorefront';

const readFragmentToken = (): string => {
  if (typeof window === 'undefined') return '';
  const params = new URLSearchParams(window.location.hash.replace(/^#/, ''));
  return params.get('access')?.trim() ?? '';
};

const statusMeta = (status: string) => {
  switch (status) {
    case 'paid':
      return { label: 'Pago confirmado', severity: 'success' as const, description: 'El proveedor confirmó el pago ante el servidor. El cumplimiento del servicio se gestiona por separado.' };
    case 'awaiting_manual_confirmation':
      return { label: 'Verificación manual pendiente', severity: 'info' as const, description: 'TDF todavía no ha verificado la transferencia, efectivo o POS. Esta selección no equivale a pago.' };
    case 'datafast_pending':
    case 'paypal_pending':
      return { label: 'Procesando pago', severity: 'warning' as const, description: 'El proveedor aún no ha entregado una confirmación verificable. No vuelvas a pagar sin revisar este estado.' };
    case 'payment_failed':
      return { label: 'Pago no confirmado', severity: 'error' as const, description: 'El intento no pudo verificarse como pagado. El pedido sigue existiendo para soporte o reintento.' };
    case 'awaiting_payment':
    case 'pending_payment':
    default:
      return { label: 'Esperando pago', severity: 'info' as const, description: 'El pedido fue creado, pero no existe un pago confirmado.' };
  }
};

export default function ServiceOrderTrackingPage() {
  const { orderNumber = '' } = useParams<{ orderNumber: string }>();
  const lookupToken = useMemo(() => {
    const fragmentToken = readFragmentToken();
    if (fragmentToken) return fragmentToken;
    return typeof window === 'undefined' ? '' : sessionStorage.getItem(`tdf-service-order:${orderNumber}`)?.trim() ?? '';
  }, [orderNumber]);

  const orderQuery = useQuery({
    queryKey: ['service-storefront-order', orderNumber, lookupToken],
    queryFn: () => ServiceStorefront.getOrder(orderNumber, lookupToken),
    enabled: Boolean(orderNumber && lookupToken),
    retry: false,
  });

  if (!lookupToken) {
    return (
      <Box maxWidth={560} mx="auto" py={8} px={2}>
        <Alert severity="warning">
          Este enlace no incluye la clave privada de seguimiento. Usa el enlace original o solicita ayuda a TDF.
        </Alert>
      </Box>
    );
  }

  if (orderQuery.isLoading) {
    return <Stack alignItems="center" py={10}><CircularProgress aria-label="Cargando pedido" /></Stack>;
  }

  if (orderQuery.isError || !orderQuery.data) {
    return (
      <Box maxWidth={560} mx="auto" py={8} px={2}>
        <Alert severity="error">
          No pudimos abrir el pedido. La referencia o la clave privada no son válidas.
        </Alert>
      </Box>
    );
  }

  const order = orderQuery.data;
  const meta = statusMeta(order.ssoStatus);
  return (
    <Box maxWidth={680} mx="auto" py={6} px={2}>
      <Stack spacing={3}>
        <Typography variant="h4" component="h1" fontWeight={800}>Seguimiento de mezcla y mastering</Typography>
        <Card variant="outlined">
          <CardContent>
            <Stack spacing={2}>
              <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" gap={1}>
                <Typography variant="h6">Pedido {order.ssoOrderNumber}</Typography>
                <Chip label={meta.label} color={meta.severity === 'error' ? 'error' : meta.severity} />
              </Stack>
              <Alert severity={meta.severity}>{meta.description}</Alert>
              <Divider />
              <Typography>{order.ssoServiceKind} · {order.ssoTier}</Typography>
              <Typography>{order.ssoSongCount} canción(es)</Typography>
              <Typography fontWeight={700}>
                Total inmutable: {(order.ssoPriceUsdCents / 100).toLocaleString('en-US', { style: 'currency', currency: order.ssoCurrency })}
              </Typography>
              {order.ssoPaymentProvider && <Typography color="text.secondary">Método: {order.ssoPaymentProvider}</Typography>}
              <Typography variant="caption" color="text.secondary">
                Pago y entrega del servicio son estados independientes. Un pago confirmado no significa que los archivos hayan sido recibidos o entregados.
              </Typography>
            </Stack>
          </CardContent>
        </Card>
        <Button component={RouterLink} to="/mezcla-mastering" variant="outlined">Volver a servicios</Button>
      </Stack>
    </Box>
  );
}
