import { useEffect, useState } from 'react';
import { Alert, Box, Button, Card, CardContent, Chip, CircularProgress, Divider, Stack, Typography } from '@mui/material';
import { useLocation, useNavigate } from 'react-router-dom';
import { Marketplace } from '../api/marketplace';
import type { MarketplaceOrderDTO } from '../api/types';

function getQueryParam(name: string): string | null {
  const params = new URLSearchParams(typeof window !== 'undefined' ? window.location.search : '');
  const value = params.get(name);
  return value && value.trim() !== '' ? value : null;
}

export default function DatafastReturnPage() {
  const orderId = getQueryParam('orderId');
  const resourcePath = getQueryParam('resourcePath') ?? getQueryParam('id');
  const navigate = useNavigate();
  const location = useLocation();
  const [status, setStatus] = useState<'loading' | 'success' | 'error'>('loading');
  const [order, setOrder] = useState<MarketplaceOrderDTO | null>(null);
  const [message, setMessage] = useState<string>('Confirmando tu pago...');

  const statusColor = (value?: string | null): 'default' | 'success' | 'warning' | 'info' => {
    if (!value) return 'default';
    const lower = value.toLowerCase();
    if (lower.includes('paid') || lower.includes('pag')) return 'success';
    if (lower.includes('pending')) return 'warning';
    if (lower.includes('contact')) return 'info';
    return 'default';
  };

  useEffect(() => {
    const run = async () => {
      if (!orderId || !resourcePath) {
        setStatus('error');
        setMessage('Faltan datos de la transacción.');
        return;
      }
      try {
        const dto = await Marketplace.confirmDatafastPayment(orderId, resourcePath);
        setOrder(dto);
        setStatus('success');
        // Limpia el carrito local para evitar dobles compras tras un pago exitoso.
        localStorage.removeItem('tdf-marketplace-cart-id');
        localStorage.removeItem('tdf-marketplace-cart-meta');
        localStorage.removeItem('tdf-marketplace-buyer');
        setMessage('Pago confirmado. ¡Gracias por tu compra!');
      } catch (err) {
        console.error(err);
        setStatus('error');
        setMessage('No pudimos confirmar tu pago. Si ya pagaste, contáctanos.');
      }
    };
    void run();
  }, [orderId, resourcePath, location.key]);

  return (
    <Box sx={{ minHeight: '100vh', bgcolor: 'background.default', display: 'flex', alignItems: 'center', justifyContent: 'center', p: 2 }}>
      <Stack spacing={2} maxWidth={480} width="100%">
        <Typography variant="h5" fontWeight={800}>
          Pago con tarjeta
        </Typography>
        {status === 'loading' && (
          <Stack spacing={1} alignItems="center">
            <CircularProgress size={24} />
            <Typography variant="body2" color="text.secondary">
              {message}
            </Typography>
          </Stack>
        )}
        {status === 'success' && order && (
          <Stack spacing={2}>
            <Alert severity="success">{message}</Alert>
            <Card variant="outlined">
              <CardContent>
                <Stack spacing={1}>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <Typography variant="subtitle1" fontWeight={700}>
                      Seguimiento de tu orden
                    </Typography>
                    <Chip label={order.moStatus} color={statusColor(order.moStatus)} size="small" />
                  </Stack>
                  <Typography variant="body2" color="text.secondary">
                    Te avisaremos cuando cambie el estado. Guarda este ID: {order.moOrderId}.
                  </Typography>
                  {order.moItems?.length > 0 && (
                    <Stack spacing={0.5}>
                      <Typography variant="caption" color="text.secondary">
                        Ítems
                      </Typography>
                      {order.moItems.map((it) => (
                        <Typography key={it.moiListingId} variant="body2">
                          {it.moiQuantity} × {it.moiTitle || 'Ítem'} — {it.moiSubtotalDisplay}
                        </Typography>
                      ))}
                    </Stack>
                  )}
                  {order.moStatusHistory?.length > 0 && (
                    <Stack spacing={0.5}>
                      <Typography variant="caption" color="text.secondary">
                        Historial
                      </Typography>
                      {order.moStatusHistory.map(([st, ts]) => (
                        <Typography key={`${st}-${ts}`} variant="body2" color="text.secondary">
                          {new Date(ts).toLocaleString()} — {st}
                        </Typography>
                      ))}
                    </Stack>
                  )}
                  <Divider />
                  <Typography variant="body2" color="text.secondary">
                    Si necesitas ayuda con tu pago o entrega, contáctanos y ten a mano tu ID de pedido.
                  </Typography>
                </Stack>
              </CardContent>
            </Card>
          </Stack>
        )}
        {status === 'error' && (
          <Alert severity="error">
            {message} Revisa tu conexión y vuelve a intentar desde el marketplace.
          </Alert>
        )}
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
          <Button variant="contained" onClick={() => navigate('/marketplace')} fullWidth>
            Volver al marketplace
          </Button>
          <Button
            variant="outlined"
            color="info"
            href="mailto:hola@tdf.lat?subject=Ayuda%20con%20mi%20pago"
            fullWidth
          >
            Necesito ayuda
          </Button>
        </Stack>
      </Stack>
    </Box>
  );
}
