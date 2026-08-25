import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControl,
  InputLabel,
  MenuItem,
  Select,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import RefreshOutlinedIcon from '@mui/icons-material/RefreshOutlined';
import ReplayOutlinedIcon from '@mui/icons-material/ReplayOutlined';

import PageShell, { EmptyState } from '../components/PageShell';
import {
  CommerceOperations,
  type CommerceProviderEvent,
  type CommerceProviderEventStatus,
} from '../api/commerceOperations';

const FILTER_STATUSES: CommerceProviderEventStatus[] = [
  'dead_letter', 'retry', 'processing', 'pending', 'processed', 'ignored',
];

const COPY = {
  es: {
    title: 'Eventos de pago',
    subtitle: 'Inbox verificado, reintentos acotados y revisión de errores permanentes.',
    warning: 'Esta vista nunca expone el payload cifrado. Reintentar no marca una orden como pagada: el worker vuelve a validar los enlaces inmutables antes de cualquier transición.',
    filter: 'Estado', all: 'Todos', refresh: 'Actualizar', empty: 'No hay eventos para este filtro.',
    attempts: 'Intentos', received: 'Recibido', next: 'Próximo intento', checkout: 'Checkout',
    replay: 'Reintentar evento', dialogTitle: 'Reintentar evento en dead letter',
    reason: 'Motivo y corrección aplicada', cancel: 'Cancelar', confirm: 'Registrar y reintentar',
    reasonHelp: 'Describe la causa reparada en una sola línea (8–500 caracteres). La evidencia queda inmutable.',
    loadError: 'No se pudo cargar el inbox de eventos de pago.',
    replayError: 'No se pudo registrar el reintento.',
  },
  en: {
    title: 'Payment events',
    subtitle: 'Verified inbox, bounded retries, and permanent-failure review.',
    warning: 'This view never exposes the encrypted payload. Replaying does not mark an order paid: the worker revalidates immutable bindings before any transition.',
    filter: 'Status', all: 'All', refresh: 'Refresh', empty: 'No events match this filter.',
    attempts: 'Attempts', received: 'Received', next: 'Next attempt', checkout: 'Checkout',
    replay: 'Replay event', dialogTitle: 'Replay dead-letter event',
    reason: 'Reason and remediation applied', cancel: 'Cancel', confirm: 'Record and replay',
    reasonHelp: 'Describe the repaired cause on one line (8–500 characters). Evidence is immutable.',
    loadError: 'The payment-event inbox could not be loaded.',
    replayError: 'The replay could not be recorded.',
  },
} as const;

const statusColor = (status: CommerceProviderEventStatus) => {
  if (status === 'dead_letter') return 'error' as const;
  if (status === 'retry' || status === 'processing') return 'warning' as const;
  if (status === 'processed') return 'success' as const;
  return 'default' as const;
};

const formatTimestamp = (value: string | null | undefined, locale: string) => {
  if (!value) return '—';
  const parsed = new Date(value);
  return Number.isNaN(parsed.getTime()) ? value : parsed.toLocaleString(locale);
};

const containsControlCharacter = (value: string) => [...value].some((character) => {
  const codePoint = character.codePointAt(0) ?? 0;
  return codePoint <= 31 || (codePoint >= 127 && codePoint <= 159);
});

export default function CommerceProviderEventsPage() {
  const { i18n } = useTranslation();
  const language = i18n.resolvedLanguage?.startsWith('en') ? 'en' : 'es';
  const copy = COPY[language];
  const locale = language === 'en' ? 'en-US' : 'es-EC';
  const queryClient = useQueryClient();
  const [status, setStatus] = useState<CommerceProviderEventStatus | ''>('dead_letter');
  const [selected, setSelected] = useState<CommerceProviderEvent | null>(null);
  const [reason, setReason] = useState('');

  const eventsQuery = useQuery({
    queryKey: ['commerce-provider-events', status],
    queryFn: () => CommerceOperations.listProviderEvents({
      status: status || undefined,
      limit: 100,
    }),
    retry: false,
  });

  const replayMutation = useMutation({
    mutationFn: ({ eventId, replayReason }: { eventId: string; replayReason: string }) =>
      CommerceOperations.replayProviderEvent(eventId, replayReason),
    onSuccess: async () => {
      setSelected(null);
      setReason('');
      await queryClient.invalidateQueries({ queryKey: ['commerce-provider-events'] });
    },
  });

  const sortedEvents = useMemo(
    () => [...(eventsQuery.data ?? [])].sort(
      (left, right) => Date.parse(right.cpeReceivedAt) - Date.parse(left.cpeReceivedAt),
    ),
    [eventsQuery.data],
  );
  const normalizedReason = reason.trim();
  const reasonValid = normalizedReason.length >= 8
    && normalizedReason.length <= 500
    && !containsControlCharacter(normalizedReason);

  return (
    <PageShell
      title={copy.title}
      subtitle={copy.subtitle}
      actions={(
        <Button
          variant="outlined"
          startIcon={<RefreshOutlinedIcon />}
          onClick={() => void eventsQuery.refetch()}
          disabled={eventsQuery.isFetching}
        >
          {copy.refresh}
        </Button>
      )}
    >
      <Stack spacing={2}>
        <Alert severity="info">{copy.warning}</Alert>
        <FormControl size="small" sx={{ width: { xs: '100%', sm: 240 } }}>
          <InputLabel id="provider-event-status-label">{copy.filter}</InputLabel>
          <Select
            labelId="provider-event-status-label"
            label={copy.filter}
            value={status}
            onChange={(event) => setStatus(event.target.value as CommerceProviderEventStatus | '')}
          >
            <MenuItem value="">{copy.all}</MenuItem>
            {FILTER_STATUSES.map((candidate) => (
              <MenuItem key={candidate} value={candidate}>{candidate}</MenuItem>
            ))}
          </Select>
        </FormControl>

        {eventsQuery.isError && <Alert severity="error">{copy.loadError}</Alert>}
        {!eventsQuery.isLoading && !eventsQuery.isError && sortedEvents.length === 0 && (
          <EmptyState title={copy.empty} />
        )}
        <Box
          sx={{
            display: 'grid',
            gap: 2,
            gridTemplateColumns: { xs: '1fr', xl: 'repeat(2, minmax(0, 1fr))' },
          }}
        >
          {sortedEvents.map((event) => (
            <Card key={event.cpeId} variant="outlined" data-testid="commerce-provider-event-card">
              <CardContent>
                <Stack spacing={1.5}>
                  <Stack direction="row" gap={1} flexWrap="wrap" alignItems="center">
                    <Chip size="small" label={event.cpeStatus} color={statusColor(event.cpeStatus)} />
                    <Chip size="small" variant="outlined" label={event.cpeProvider} />
                    <Chip size="small" variant="outlined" label={event.cpeEnvironment} />
                  </Stack>
                  <Typography variant="subtitle1" fontWeight={700} sx={{ overflowWrap: 'anywhere' }}>
                    {event.cpeEventType}
                  </Typography>
                  <Typography variant="body2" color="text.secondary" sx={{ overflowWrap: 'anywhere' }}>
                    {event.cpeProviderEventId}
                  </Typography>
                  {event.cpeErrorSummary && (
                    <Alert severity="warning">{event.cpeErrorSummary}</Alert>
                  )}
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={{ xs: 0.5, sm: 2 }}>
                    <Typography variant="body2">{copy.attempts}: {event.cpeAttemptCount}</Typography>
                    <Typography variant="body2">{copy.received}: {formatTimestamp(event.cpeReceivedAt, locale)}</Typography>
                  </Stack>
                  {event.cpeNextAttemptAt && (
                    <Typography variant="body2">{copy.next}: {formatTimestamp(event.cpeNextAttemptAt, locale)}</Typography>
                  )}
                  {event.cpeCheckoutId && (
                    <Typography variant="body2" sx={{ overflowWrap: 'anywhere' }}>
                      {copy.checkout}: {event.cpeCheckoutId}
                    </Typography>
                  )}
                  {event.cpeStatus === 'dead_letter' && (
                    <Button
                      variant="contained"
                      color="warning"
                      startIcon={<ReplayOutlinedIcon />}
                      onClick={() => {
                        setSelected(event);
                        setReason('');
                        replayMutation.reset();
                      }}
                    >
                      {copy.replay}
                    </Button>
                  )}
                </Stack>
              </CardContent>
            </Card>
          ))}
        </Box>
      </Stack>

      <Dialog
        open={selected !== null}
        onClose={() => !replayMutation.isPending && setSelected(null)}
        fullWidth
        maxWidth="sm"
      >
        <DialogTitle>{copy.dialogTitle}</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ pt: 1 }}>
            <Alert severity="warning">{copy.reasonHelp}</Alert>
            <TextField
              label={copy.reason}
              value={reason}
              onChange={(event) => setReason(event.target.value)}
              inputProps={{ maxLength: 500 }}
              required
            />
            {replayMutation.isError && <Alert severity="error">{copy.replayError}</Alert>}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setSelected(null)} disabled={replayMutation.isPending}>
            {copy.cancel}
          </Button>
          <Button
            variant="contained"
            disabled={!selected || !reasonValid || replayMutation.isPending}
            onClick={() => selected && replayMutation.mutate({
              eventId: selected.cpeId,
              replayReason: normalizedReason,
            })}
          >
            {copy.confirm}
          </Button>
        </DialogActions>
      </Dialog>
    </PageShell>
  );
}
