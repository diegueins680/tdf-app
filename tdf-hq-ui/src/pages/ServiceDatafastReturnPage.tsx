import { useEffect, useMemo, useState } from 'react';
import { Alert, Box, Button, Card, CardContent, CircularProgress, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { ServiceStorefront, type ServiceStorefrontOrderDTO } from '../api/serviceStorefront';

const queryParam = (name: string): string => {
  if (typeof window === 'undefined') return '';
  return new URLSearchParams(window.location.search).get(name)?.trim() ?? '';
};

export default function ServiceDatafastReturnPage() {
  const orderNumber = useMemo(() => queryParam('orderId'), []);
  const resourcePath = useMemo(() => queryParam('resourcePath') || queryParam('id'), []);
  const lookupToken = useMemo(
    () => (typeof window === 'undefined' ? '' : sessionStorage.getItem(`tdf-service-order:${orderNumber}`)?.trim() ?? ''),
    [orderNumber],
  );
  const [order, setOrder] = useState<ServiceStorefrontOrderDTO | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (!orderNumber || !resourcePath || !lookupToken) {
      setError('Faltan los datos privados necesarios para verificar esta operación.');
      return;
    }
    ServiceStorefront.confirmDatafastPayment(orderNumber, lookupToken, resourcePath)
      .then(setOrder)
      .catch(() => setError('No pudimos verificar el pago con Datafast. No se marcará como pagado.'));
  }, [lookupToken, orderNumber, resourcePath]);

  const trackingUrl = lookupToken
    ? `/mezcla-mastering/pedido/${encodeURIComponent(orderNumber)}#access=${encodeURIComponent(lookupToken)}`
    : '/mezcla-mastering';

  return (
    <Box maxWidth={560} mx="auto" py={8} px={2}>
      <Stack spacing={3}>
        <Typography variant="h4" component="h1" fontWeight={800}>Verificación Datafast</Typography>
        {!order && !error && <Stack alignItems="center"><CircularProgress aria-label="Verificando pago" /></Stack>}
        {error && <Alert severity="error">{error}</Alert>}
        {order && (
          <Card variant="outlined">
            <CardContent>
              <Stack spacing={2}>
                <Alert severity={order.ssoStatus === 'paid' ? 'success' : 'warning'}>
                  {order.ssoStatus === 'paid'
                    ? 'Datafast confirmó importe, moneda y referencia ante el servidor. El pago está confirmado.'
                    : 'Datafast no confirmó todavía un pago válido. El pedido permanece pendiente o en revisión.'}
                </Alert>
                <Typography>Pedido {order.ssoOrderNumber}</Typography>
                <Typography>Estado técnico: {order.ssoStatus}</Typography>
              </Stack>
            </CardContent>
          </Card>
        )}
        <Button component={RouterLink} to={trackingUrl} variant="contained">Ver seguimiento privado</Button>
      </Stack>
    </Box>
  );
}
