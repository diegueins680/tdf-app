import { useEffect, useMemo, useState } from 'react';
import { useParams, Link as RouterLink } from 'react-router-dom';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Divider,
  Link,
  Stack,
  Typography,
} from '@mui/material';
import ArrowBackIcon from '@mui/icons-material/ArrowBack';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import type { MarketplaceOrderDTO } from '../api/types';
import { Marketplace } from '../api/marketplace';

const statusColor = (value?: string | null): 'default' | 'success' | 'warning' | 'info' => {
  if (!value) return 'default';
  const lower = value.toLowerCase();
  if (lower.includes('paid') || lower.includes('pag')) return 'success';
  if (lower.includes('pending')) return 'warning';
  if (lower.includes('contact')) return 'info';
  return 'default';
};

export default function MarketplaceOrderTrackingPage() {
  const { orderId } = useParams<{ orderId: string }>();
  const [order, setOrder] = useState<MarketplaceOrderDTO | null>(null);
  const [status, setStatus] = useState<'loading' | 'error' | 'success'>('loading');

  useEffect(() => {
    const run = async () => {
      if (!orderId) {
        setStatus('error');
        return;
      }
      try {
        const dto = await Marketplace.getOrder(orderId);
        setOrder(dto);
        setStatus('success');
      } catch {
        setStatus('error');
      }
    };
    void run();
  }, [orderId]);

  const timeline = useMemo(() => order?.moStatusHistory ?? [], [order]);

  const copyLink = () => {
    if (typeof window === 'undefined') return;
    const url = window.location.href;
    if (navigator?.clipboard?.writeText) {
      navigator.clipboard.writeText(url).catch((err) => console.warn('No se pudo copiar el enlace de seguimiento', err));
    }
  };

  return (
    <Box sx={{ minHeight: '100vh', bgcolor: 'background.default', display: 'flex', alignItems: 'center', justifyContent: 'center', p: 2 }}>
      <Stack spacing={2} maxWidth={720} width="100%">
        <Stack direction="row" spacing={1} alignItems="center">
          <Button component={RouterLink} to="/marketplace" startIcon={<ArrowBackIcon />} size="small" variant="text">
            Volver al marketplace
          </Button>
          <Box flex={1} />
          <Button
            size="small"
            startIcon={<ContentCopyIcon />}
            variant="outlined"
            onClick={copyLink}
          >
            Copiar enlace
          </Button>
        </Stack>

        <Typography variant="h4" fontWeight={800}>
          Seguimiento de pedido
        </Typography>
        {status === 'loading' && (
          <Stack spacing={1} alignItems="center">
            <CircularProgress size={24} />
            <Typography variant="body2" color="text.secondary">
              Cargando pedido...
            </Typography>
          </Stack>
        )}
        {status === 'error' && (
          <Alert severity="error">No pudimos encontrar este pedido. Verifica el enlace o contacta soporte.</Alert>
        )}
        {status === 'success' && order && (
          <Stack spacing={2}>
            <Card variant="outlined">
              <CardContent>
                <Stack direction="row" justifyContent="space-between" alignItems="center" spacing={1}>
                  <Typography variant="h6" fontWeight={700}>
                    Pedido {order.moOrderId}
                  </Typography>
                  <Chip label={order.moStatus} color={statusColor(order.moStatus)} />
                </Stack>
                <Typography variant="body2" color="text.secondary">
                  Total: {order.moTotalDisplay} · Creado: {new Date(order.moCreatedAt).toLocaleString()}
                </Typography>
                {order.moPaidAt && (
                  <Typography variant="body2" color="text.secondary">
                    Pagado el {new Date(order.moPaidAt).toLocaleString()} via {order.moPaymentProvider?.toUpperCase() ?? '—'}
                  </Typography>
                )}
                <Divider sx={{ my: 2 }} />
                <Stack spacing={0.75}>
                  <Typography variant="subtitle2">Ítems</Typography>
                  {order.moItems.map((it) => (
                    <Stack key={it.moiListingId} direction="row" justifyContent="space-between" alignItems="center">
                      <Typography variant="body2">
                        {it.moiQuantity} × {it.moiTitle || 'Ítem'}
                      </Typography>
                      <Typography variant="body2" fontWeight={700}>
                        {it.moiSubtotalDisplay}
                      </Typography>
                    </Stack>
                  ))}
                  <Typography variant="body2" fontWeight={800}>
                    Total: {order.moTotalDisplay}
                  </Typography>
                </Stack>
              </CardContent>
            </Card>

            <Card variant="outlined">
              <CardContent>
                <Stack spacing={1}>
                  <Typography variant="subtitle1" fontWeight={700}>
                    Historial de estado
                  </Typography>
                  {timeline.length === 0 && (
                    <Typography variant="body2" color="text.secondary">
                      Aún no hay cambios registrados.
                    </Typography>
                  )}
                  {timeline.map(([st, ts]) => (
                    <Stack key={`${st}-${ts}`} direction="row" spacing={1} alignItems="center">
                      <Chip size="small" label={st} color={statusColor(st)} />
                      <Typography variant="body2" color="text.secondary">
                        {new Date(ts).toLocaleString()}
                      </Typography>
                    </Stack>
                  ))}
                </Stack>
              </CardContent>
            </Card>

            <Typography variant="caption" color="text.secondary">
              ¿Dudas? Contáctanos en <Link href="mailto:hola@tdf.lat">hola@tdf.lat</Link> con tu ID de pedido.
            </Typography>
          </Stack>
        )}
      </Stack>
    </Box>
  );
}
