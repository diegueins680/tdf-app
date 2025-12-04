import { useEffect, useState } from 'react';
import { Alert, Box, Button, CircularProgress, Stack, Typography } from '@mui/material';
import { useLocation, useNavigate } from 'react-router-dom';
import { Marketplace } from '../api/marketplace';
import type { MarketplaceOrderDTO } from '../api/types';

function useQueryParam(name: string): string | null {
  const params = new URLSearchParams(typeof window !== 'undefined' ? window.location.search : '');
  const value = params.get(name);
  return value && value.trim() !== '' ? value : null;
}

export default function DatafastReturnPage() {
  const orderId = useQueryParam('orderId');
  const navigate = useNavigate();
  const location = useLocation();
  const [status, setStatus] = useState<'loading' | 'success' | 'error'>('loading');
  const [order, setOrder] = useState<MarketplaceOrderDTO | null>(null);

  useEffect(() => {
    const run = async () => {
      if (!orderId) {
        setStatus('error');
        return;
      }
      try {
        const dto = await Marketplace.datafastStatus({ orderId });
        setOrder(dto);
        setStatus('success');
      } catch {
        setStatus('error');
      }
    };
    void run();
  }, [orderId, location.key]);

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
              Confirmando tu pago...
            </Typography>
          </Stack>
        )}
        {status === 'success' && order && (
          <Alert severity="success">
            Pago recibido. ID de pedido: <strong>{order.moOrderId}</strong>. Estado: {order.moStatus}.
          </Alert>
        )}
        {status === 'error' && <Alert severity="error">No pudimos confirmar tu pago. Si ya pagaste, contáctanos.</Alert>}
        <Stack direction="row" spacing={1}>
          <Button variant="contained" onClick={() => navigate('/marketplace')}>
            Volver al marketplace
          </Button>
          {orderId && (
            <Button variant="outlined" onClick={() => navigate(`/operacion/ordenes-marketplace?orderId=${orderId}`)}>
              Ver pedido
            </Button>
          )}
        </Stack>
      </Stack>
    </Box>
  );
}
