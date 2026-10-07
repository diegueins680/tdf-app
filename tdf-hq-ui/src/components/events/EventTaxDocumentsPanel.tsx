import { Alert, Button, Chip, Stack, Typography } from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState } from 'react';

import { SocialEventsAPI, type TicketTaxDocumentDTO } from '../../api/socialEvents';

const STATUS_LABEL: Record<TicketTaxDocumentDTO['ttdStatus'], string> = {
  pending: 'Pendiente',
  submitted: 'Enviada al SRI',
  authorized: 'Autorizada',
  rejected: 'No autorizada',
  uncertain: 'Por conciliar',
  failed: 'Rechazada por el proveedor',
};

const STATUS_COLOR: Record<TicketTaxDocumentDTO['ttdStatus'], 'default' | 'success' | 'warning' | 'error'> = {
  pending: 'default',
  submitted: 'default',
  authorized: 'success',
  rejected: 'error',
  uncertain: 'warning',
  failed: 'error',
};

interface EventTaxDocumentsPanelProps {
  eventId: string;
  formatMoney: (minor: number, currency: string) => string;
  formatDate: (iso: string) => string;
}

// Only invoices the provider refused before creating them can be resent;
// "Por conciliar" must be checked in the Dátil dashboard to avoid duplicates.
export default function EventTaxDocumentsPanel({ eventId, formatMoney, formatDate }: EventTaxDocumentsPanelProps) {
  const queryClient = useQueryClient();
  const [error, setError] = useState<string | null>(null);
  const queryKey = ['event-tax-documents', eventId];
  const documents = useQuery({ queryKey, queryFn: () => SocialEventsAPI.listTaxDocuments(eventId) });
  const retry = useMutation({
    mutationFn: (documentId: string) => SocialEventsAPI.retryTaxDocument(eventId, documentId),
    onSuccess: (rows) => {
      setError(null);
      queryClient.setQueryData(queryKey, rows);
    },
    onError: (failure) => setError(failure instanceof Error ? failure.message : 'No se pudo reenviar la factura.'),
  });

  if (documents.isLoading || documents.error) return null;
  const rows = documents.data ?? [];
  if (rows.length === 0) return null;

  return (
    <Stack spacing={1}>
      <Typography variant="subtitle2" fontWeight={700}>Facturas y notas de crédito electrónicas</Typography>
      {error && <Alert severity="error">{error}</Alert>}
      {rows.map((document) => (
        <Stack key={document.ttdId} spacing={0.5} sx={{ border: 1, borderColor: 'divider', borderRadius: 1, p: 1.5 }}>
          <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap alignItems="center">
            <Typography variant="body2" fontWeight={700}>
              {document.ttdKind === 'credit_note' ? 'Nota de crédito' : 'Factura'} {document.ttdNumber}
            </Typography>
            <Chip size="small" label={STATUS_LABEL[document.ttdStatus]} color={STATUS_COLOR[document.ttdStatus]} />
            <Typography variant="body2">
              {formatMoney(document.ttdAmountMinor, 'USD')} · Orden TDF-{document.ttdOrderId}
              {document.ttdEnvironment === 'sandbox' ? ' · Pruebas' : ''}
            </Typography>
          </Stack>
          {document.ttdAuthorizationNumber && (
            <Typography variant="caption" color="text.secondary" sx={{ wordBreak: 'break-all' }}>
              Autorización {document.ttdAuthorizationNumber}
              {document.ttdAuthorizedAt ? ` · ${formatDate(document.ttdAuthorizedAt)}` : ''}
            </Typography>
          )}
          {document.ttdLastError && (
            <Typography variant="caption" color="error" sx={{ wordBreak: 'break-word' }}>{document.ttdLastError}</Typography>
          )}
          {document.ttdStatus === 'failed' && (
            <Button size="small" variant="outlined" sx={{ alignSelf: 'flex-start' }} disabled={retry.isPending}
              onClick={() => retry.mutate(document.ttdId)}>
              Reenviar factura
            </Button>
          )}
        </Stack>
      ))}
    </Stack>
  );
}
