import { useMemo } from 'react';
import { useParams } from 'react-router-dom';
import {
  Alert,
  Box,
  Button,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Typography,
} from '@mui/material';
import { useReceiptQuery } from '../../api/hq';
import type { components } from '../../api/generated/lessons-and-receipts';

function formatAmount(cents: number, currency: string) {
  const amount = cents / 100;
  try {
    return new Intl.NumberFormat('es-EC', { style: 'currency', currency }).format(amount);
  } catch {
    return `${amount.toFixed(2)} ${currency}`;
  }
}

export default function ReceiptView() {
  const { receiptId } = useParams<{ receiptId: string }>();

  const query = useReceiptQuery(receiptId ?? null);

  type Receipt = components['schemas']['Receipt'];
  type ReceiptLine = NonNullable<Receipt['line_items']>[number];

  const { receipt, lines } = useMemo(() => {
    const data = query.data as Receipt | undefined;
    if (!data) {
      return { receipt: undefined, lines: [] as Array<ReceiptLine & { lineSubtotal: number; lineTotal: number }> };
    }
    const enriched = (data.line_items ?? []).map((item: ReceiptLine) => {
      const lineSubtotal = (item.qty ?? 0) * (item.unit_price_cents ?? 0);
      const lineTotal = item.total_cents ?? lineSubtotal;
      return { ...item, lineSubtotal, lineTotal };
    });
    return { receipt: data, lines: enriched };
  }, [query.data]);

  const currency = receipt?.currency ?? 'USD';

  return (
    <Box p={2}>
      <Stack direction="row" justifyContent="space-between" alignItems="center" mb={2}>
        <div>
          <Typography variant="h5">
            {receipt ? `Recibo ${receipt.receipt_number}` : `Recibo #${receiptId ?? ''}`}
          </Typography>
          <Typography variant="subtitle1" color="text.secondary">
            {receipt?.issued_at ? new Date(receipt.issued_at).toLocaleString() : '—'}
          </Typography>
        </div>
        <Button variant="outlined" disabled>
          Descargar PDF (próximamente)
        </Button>
      </Stack>

      <Paper sx={{ p: 3 }}>
        {query.isLoading && <Typography>Cargando…</Typography>}
        {query.isError && (
          <Alert severity="error">{(query.error as Error).message}</Alert>
        )}
        {receipt && (
          <Stack spacing={3}>
            <Stack spacing={0.5}>
              <Typography variant="h6">{receipt.buyer_name ?? 'Cliente'}</Typography>
              {receipt.buyer_email && (
                <Typography color="text.secondary">{receipt.buyer_email}</Typography>
              )}
              {receipt.payment_id && (
                <Typography color="text.secondary">
                  Pago relacionado: {receipt.payment_id}
                </Typography>
              )}
            </Stack>

            <TableContainer>
              <Table size="small">
                <TableHead>
                  <TableRow>
                    <TableCell>Descripción</TableCell>
                    <TableCell>Cantidad</TableCell>
                    <TableCell align="right">Unitario</TableCell>
                    <TableCell align="right">IVA</TableCell>
                    <TableCell align="right">Total</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {lines.map((line) => (
                    <TableRow key={`${line.description}-${line.qty}-${line.unit_price_cents}`}>
                      <TableCell>{line.description}</TableCell>
                      <TableCell>{line.qty}</TableCell>
                      <TableCell align="right">
                        {formatAmount(line.unit_price_cents ?? 0, currency)}
                      </TableCell>
                      <TableCell align="right">
                        {line.lineTotal - line.lineSubtotal > 0
                          ? formatAmount(line.lineTotal - line.lineSubtotal, currency)
                          : '—'}
                      </TableCell>
                      <TableCell align="right">
                        {formatAmount(line.lineTotal, currency)}
                      </TableCell>
                    </TableRow>
                  ))}
                  {lines.length === 0 && (
                    <TableRow>
                      <TableCell colSpan={5}>
                        <Typography variant="body2" color="text.secondary" align="center">
                          No se registraron conceptos en este recibo.
                        </Typography>
                      </TableCell>
                    </TableRow>
                  )}
                </TableBody>
              </Table>
            </TableContainer>

            <Stack spacing={0.5} sx={{ ml: 'auto', minWidth: 240 }}>
              <Stack direction="row" justifyContent="space-between">
                <Typography color="text.secondary">Subtotal</Typography>
                <Typography>{formatAmount(receipt.subtotal_cents ?? 0, currency)}</Typography>
              </Stack>
              <Stack direction="row" justifyContent="space-between">
                <Typography color="text.secondary">IVA</Typography>
                <Typography>{formatAmount(receipt.tax_cents ?? 0, currency)}</Typography>
              </Stack>
              <Stack direction="row" justifyContent="space-between">
                <Typography fontWeight={600}>Total</Typography>
                <Typography fontWeight={600}>
                  {formatAmount(receipt.total_cents ?? 0, currency)}
                </Typography>
              </Stack>
            </Stack>
          </Stack>
        )}
      </Paper>
    </Box>
  );
}
