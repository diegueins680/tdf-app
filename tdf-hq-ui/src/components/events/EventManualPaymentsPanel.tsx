import { Alert, Button, Chip, Stack, TextField, Typography } from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState } from 'react';

import { SocialEventsAPI, type TicketManualPaymentDTO } from '../../api/socialEvents';

const STATUS_LABEL: Record<TicketManualPaymentDTO['tmpEvidenceStatus'], string> = {
  awaiting_evidence: 'Esperando comprobante',
  submitted: 'Por verificar',
  under_review: 'En revisión',
  approved: 'Aprobada',
  rejected: 'Rechazada',
};

const reviewable = (payment: TicketManualPaymentDTO) =>
  payment.tmpEvidenceStatus === 'submitted' || payment.tmpEvidenceStatus === 'under_review';

interface EventManualPaymentsPanelProps {
  eventId: string;
  formatMoney: (minor: number, currency: string) => string;
  formatDate: (iso: string) => string;
}

// Approving confirms money actually arrived in the bank account; the server
// then records the payment and issues tickets. Rejection notes are shown to the buyer.
export default function EventManualPaymentsPanel({ eventId, formatMoney, formatDate }: EventManualPaymentsPanelProps) {
  const queryClient = useQueryClient();
  const [notes, setNotes] = useState<Record<string, string>>({});
  const [error, setError] = useState<string | null>(null);
  const queryKey = ['event-manual-payments', eventId];
  const payments = useQuery({
    queryKey,
    queryFn: () => SocialEventsAPI.listManualPayments(eventId),
  });
  const review = useMutation({
    mutationFn: ({ orderId, action }: { orderId: string; action: 'approve' | 'reject' }) =>
      SocialEventsAPI.reviewManualPayment(eventId, orderId, {
        tmprAction: action,
        tmprNotes: (notes[orderId] ?? '').trim(),
      }),
    onSuccess: () => {
      setError(null);
      void queryClient.invalidateQueries({ queryKey });
    },
    onError: (failure) => setError(failure instanceof Error ? failure.message : 'No se pudo registrar la revisión.'),
  });

  if (payments.isLoading) return <Typography variant="body2" color="text.secondary">Cargando transferencias...</Typography>;
  if (payments.error) return <Alert severity="warning">No se pudieron cargar las transferencias bancarias.</Alert>;
  const rows = payments.data ?? [];
  if (rows.length === 0) return null;

  return (
    <Stack spacing={1}>
      <Typography variant="subtitle2" fontWeight={700}>Transferencias bancarias</Typography>
      <Alert severity="info">
        Aprueba solo después de ver el depósito exacto en la cuenta bancaria. La nota de un rechazo se muestra al comprador.
      </Alert>
      {error && <Alert severity="error">{error}</Alert>}
      {rows.map((payment) => {
        const note = notes[payment.tmpOrderId] ?? '';
        return (
          <Stack key={payment.tmpOrderId} spacing={1} sx={{ border: 1, borderColor: 'divider', borderRadius: 1, p: 1.5 }}>
            <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap alignItems="center">
              <Typography variant="body2" fontWeight={700}>{payment.tmpPaymentReference}</Typography>
              <Chip size="small" label={STATUS_LABEL[payment.tmpEvidenceStatus]} color={reviewable(payment) ? 'warning' : 'default'} />
              <Typography variant="body2">
                {formatMoney(payment.tmpAmountMinor, payment.tmpCurrency)} · {payment.tmpQuantity} entrada(s)
              </Typography>
            </Stack>
            <Typography variant="body2" color="text.secondary">
              {payment.tmpBuyerName ?? 'Sin nombre'} · {payment.tmpBuyerEmail ?? 'sin email'}
              {payment.tmpCustomerReference ? ` · Comprobante: ${payment.tmpCustomerReference}` : ''}
              {payment.tmpSubmittedAt ? ` · Reportado: ${formatDate(payment.tmpSubmittedAt)}` : ''}
            </Typography>
            {reviewable(payment) && (
              <>
                <Typography variant="caption" color="text.secondary">
                  Cupo reservado hasta {formatDate(payment.tmpHoldExpiresAt)}
                </Typography>
                <TextField
                  size="small"
                  label="Nota de revisión (obligatoria)"
                  value={note}
                  onChange={(event) => setNotes((prev) => ({ ...prev, [payment.tmpOrderId]: event.target.value }))}
                  inputProps={{ maxLength: 2000 }}
                />
                <Stack direction="row" spacing={1}>
                  <Button
                    size="small"
                    variant="contained"
                    color="success"
                    disabled={review.isPending || note.trim().length < 3}
                    onClick={() => review.mutate({ orderId: payment.tmpOrderId, action: 'approve' })}
                  >
                    Aprobar y emitir
                  </Button>
                  <Button
                    size="small"
                    variant="outlined"
                    color="error"
                    disabled={review.isPending || note.trim().length < 3}
                    onClick={() => review.mutate({ orderId: payment.tmpOrderId, action: 'reject' })}
                  >
                    Rechazar
                  </Button>
                </Stack>
              </>
            )}
            {payment.tmpReviewNotes && !reviewable(payment) && (
              <Typography variant="caption" color="text.secondary">Nota: {payment.tmpReviewNotes}</Typography>
            )}
          </Stack>
        );
      })}
    </Stack>
  );
}
