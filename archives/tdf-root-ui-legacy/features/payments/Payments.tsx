import { cents } from '../../api/client';
import { usePaymentsQuery } from '../../api/hq';
import {
  Box,
  Paper,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Typography,
} from '@mui/material';

export default function Payments() {
  const { data, isLoading, isError, error } = usePaymentsQuery();
  const payments = data ?? [];

  return (
    <Box p={2}>
      <Typography variant="h5" mb={2}>Pagos registrados</Typography>
      <TableContainer component={Paper}>
        <Table size="small">
          <TableHead>
            <TableRow>
              <TableCell>ID</TableCell>
              <TableCell>Enrollment</TableCell>
              <TableCell>Método</TableCell>
              <TableCell>Referencia</TableCell>
              <TableCell>Estado</TableCell>
              <TableCell>Pagado</TableCell>
              <TableCell align="right">Monto</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {isLoading && (
              <TableRow><TableCell colSpan={7}>Cargando…</TableCell></TableRow>
            )}
            {isError && (
              <TableRow>
                <TableCell colSpan={7} sx={{ color: 'error.main' }}>
                  {(error as Error).message}
                </TableCell>
              </TableRow>
            )}
            {payments.map(payment => (
              <TableRow key={payment.id}>
                <TableCell>{payment.id}</TableCell>
                <TableCell>{payment.enrollment_id ?? '—'}</TableCell>
                <TableCell>{payment.method ?? '—'}</TableCell>
                <TableCell>{payment.reference ?? '—'}</TableCell>
                <TableCell sx={{ textTransform: 'capitalize' }}>{payment.status ?? 'pending'}</TableCell>
                <TableCell>
                  {payment.paid_at ? new Date(payment.paid_at).toLocaleString() : '—'}
                </TableCell>
                <TableCell align="right">
                  {cents(payment.amount_cents ?? 0, payment.currency ?? 'USD')}
                </TableCell>
              </TableRow>
            ))}
            {!isLoading && !isError && payments.length === 0 && (
              <TableRow>
                <TableCell colSpan={7} sx={{ color: 'text.secondary' }}>
                  No se encontraron pagos.
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </TableContainer>
    </Box>
  );
}
