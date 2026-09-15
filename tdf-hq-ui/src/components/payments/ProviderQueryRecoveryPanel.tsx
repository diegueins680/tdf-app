import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import { Alert, Box, Button, Card, CardContent, Chip, FormControl, InputLabel, NativeSelect, Stack, Typography } from '@mui/material';
import { CommerceOperations, type CommerceProviderQueries, type CommerceProviderQuery } from '../../api/commerceOperations';

const COPY = {
  es: {
    title: 'Recuperación de consultas de pago',
    notice: 'Solo lectura. Una consulta finalizada no significa un pago exitoso. Esta vista no reintenta cobros ni muestra operaciones sin un recurso vinculado. Las páginas pueden cambiar mientras trabaja el procesador.',
    environment: 'Entorno de consultas', status: 'Estado de consulta', all: 'Todos',
    sandbox: 'Pruebas', production: 'Producción', refresh: 'Actualizar informe de consultas',
    pending: 'Pendiente', processing: 'En curso', retry: 'Consulta por reintentar',
    completed: 'Consulta finalizada', deadLetter: 'Requiere revisión',
    loading: 'Cargando informe de consultas…', error: 'Informe de consultas no disponible. No se muestran datos anteriores; vuelve a actualizar.',
    missing: 'El esquema de recuperación no está instalado. Esto no confirma que la cola esté vacía.',
    flagOn: 'La activación en base de datos está encendida. No confirma un procesador activo ni credenciales o contratos válidos.',
    flagOff: 'La recuperación programada está desactivada en base de datos. No cambies esta opción sin la verificación requerida.',
    empty: 'No hay consultas registradas para este entorno y filtro.', observed: 'Informe solicitado',
    attempts: 'Consultas intentadas', operation: 'Operación original', certainty: 'Certeza del resultado',
    checkout: 'Checkout', attempt: 'Intento de pago', next: 'No antes de', lease: 'Vencimiento de asignación',
    last: 'Última consulta', outcome: 'Último resultado técnico', finished: 'Finalizada',
    budget: 'Próximo cupo compartido de consulta (no garantiza ejecución)', previous: 'Página anterior de consultas',
    more: 'Página siguiente de consultas', page: 'Desplazamiento', limit: 'Límite de navegación alcanzado. Reduce el filtro o solicita una revisión operativa.',
  },
  en: {
    title: 'Payment query recovery',
    notice: 'Read only. A completed query is not a successful payment. This view does not retry charges or show operations without a bound resource. Pages can change while workers run.',
    environment: 'Query environment', status: 'Query status', all: 'All',
    sandbox: 'Sandbox', production: 'Production', refresh: 'Refresh query report',
    pending: 'Pending', processing: 'In progress', retry: 'Query retry pending',
    completed: 'Query completed', deadLetter: 'Review required',
    loading: 'Loading query report…', error: 'Query report unavailable. Previous data is hidden; refresh to try again.',
    missing: 'Recovery schema is not installed. This does not establish that the queue is empty.',
    flagOn: 'The database switch is enabled. This does not establish worker liveness or valid credentials and contracts.',
    flagOff: 'Scheduled recovery is disabled in the database. Do not change it without the required verification.',
    empty: 'No queries are recorded for this environment and filter.', observed: 'Report requested',
    attempts: 'Queries attempted', operation: 'Original operation', certainty: 'Outcome certainty',
    checkout: 'Checkout', attempt: 'Payment attempt', next: 'Not before', lease: 'Lease expires',
    last: 'Last query', outcome: 'Last technical outcome', finished: 'Completed at',
    budget: 'Next shared query slot (execution is not guaranteed)', previous: 'Previous query page',
    more: 'Next query page', page: 'Offset', limit: 'Navigation limit reached. Narrow the filter or request operational review.',
  },
} as const;

export default function ProviderQueryRecoveryPanel() {
  const { i18n } = useTranslation();
  const english = i18n.resolvedLanguage?.startsWith('en') ?? false;
  const copy = COPY[english ? 'en' : 'es'];
  const [environment, setEnvironment] = useState<CommerceProviderQueries['cpqsEnvironment']>('sandbox');
  const [status, setStatus] = useState<CommerceProviderQuery['cpqStatus'] | ''>('dead_letter');
  const [offset, setOffset] = useState(0);
  const query = useQuery({
    queryKey: ['commerce-provider-queries', environment, status, offset],
    queryFn: () => CommerceOperations.listProviderQueries({ environment, status: status || undefined, offset, limit: 25 }),
    retry: false,
    staleTime: 0,
    gcTime: 0,
    refetchOnMount: 'always',
  });
  // Do not paint cached operational evidence during authorization refresh or
  // after errors, and never present a response under another environment label.
  const mismatchedEnvironment = query.data && query.data.cpqsEnvironment !== environment;
  const report = !query.isFetching && !query.isError && !mismatchedEnvironment ? query.data : undefined;
  const date = (value: string | null) => {
    if (!value) return '—';
    const parsed = new Date(value);
    return Number.isNaN(parsed.getTime()) ? '—' : parsed.toLocaleString(english ? 'en-US' : 'es-EC');
  };
  const statusLabel = (value: CommerceProviderQuery['cpqStatus']) => {
    if (value === 'pending') return copy.pending;
    if (value === 'processing') return copy.processing;
    if (value === 'retry') return copy.retry;
    if (value === 'completed') return copy.completed;
    return copy.deadLetter;
  };

  return (
    <Stack component="section" aria-labelledby="provider-query-title" spacing={2}>
      <Typography id="provider-query-title" variant="h6">{copy.title}</Typography>
      <Alert severity="info">{copy.notice}</Alert>
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ sm: 'center' }}>
        <FormControl>
          <InputLabel htmlFor="provider-query-environment">{copy.environment}</InputLabel>
          <NativeSelect inputProps={{ id: 'provider-query-environment' }} value={environment} onChange={(event) => {
            setEnvironment(event.target.value === 'production' ? 'production' : 'sandbox');
            setOffset(0);
          }}>
            <option value="sandbox">{copy.sandbox}</option>
            <option value="production">{copy.production}</option>
          </NativeSelect>
        </FormControl>
        <FormControl>
          <InputLabel htmlFor="provider-query-status">{copy.status}</InputLabel>
          <NativeSelect inputProps={{ id: 'provider-query-status' }} value={status} onChange={(event) => {
            setStatus(event.target.value as CommerceProviderQuery['cpqStatus'] | '');
            setOffset(0);
          }}>
            <option value="">{copy.all}</option>
            <option value="pending">{copy.pending}</option>
            <option value="processing">{copy.processing}</option>
            <option value="retry">{copy.retry}</option>
            <option value="completed">{copy.completed}</option>
            <option value="dead_letter">{copy.deadLetter}</option>
          </NativeSelect>
        </FormControl>
        <Button variant="outlined" disabled={query.isFetching} onClick={() => void query.refetch()}>{copy.refresh}</Button>
      </Stack>
      {query.isFetching && <Typography role="status">{copy.loading}</Typography>}
      {(query.isError || mismatchedEnvironment) && <Alert severity="error">{copy.error}</Alert>}
      {report && !report.cpqsSchemaReady && <Alert severity="warning">{copy.missing}</Alert>}
      {report?.cpqsSchemaReady && <>
        <Typography variant="caption">{copy.observed}: {date(report.cpqsGeneratedAt)} · {environment}</Typography>
        <Alert severity="warning">{report.cpqsRecoveryFlagEnabled ? copy.flagOn : copy.flagOff}</Alert>
        {report.cpqsBudgets.map((budget) => <Typography key={budget.cpqbProvider} variant="body2">
          {budget.cpqbProvider} · {copy.budget}: {date(budget.cpqbNextQueryAt)}
        </Typography>)}
        {report.cpqsJobs.length === 0 && <Typography>{copy.empty}</Typography>}
        <Box sx={{ display: 'grid', gap: 1.5, gridTemplateColumns: { xs: '1fr', lg: 'repeat(2, minmax(0, 1fr))' } }}>
          {report.cpqsJobs.map((job) => <Card component="article" key={job.cpqOperationId} variant="outlined">
            <CardContent sx={{ overflowWrap: 'anywhere' }}>
              <Stack direction="row" gap={1} flexWrap="wrap" alignItems="center">
                <Typography fontWeight={700}>{job.cpqProvider}</Typography>
                <Chip size="small" label={statusLabel(job.cpqStatus)} color={job.cpqStatus === 'dead_letter' ? 'warning' : 'default'} />
              </Stack>
              <Typography variant="body2">{copy.operation}: {job.cpqOperationId} · {job.cpqOperationStatus}</Typography>
              <Typography variant="body2">{copy.certainty}: {job.cpqOutcomeCertainty}</Typography>
              <Typography variant="body2">{copy.checkout}: {job.cpqCheckoutId}</Typography>
              <Typography variant="body2">{copy.attempt}: {job.cpqPaymentAttemptId}</Typography>
              <Typography variant="body2">{copy.attempts}: {job.cpqAttemptCount}</Typography>
              <Typography variant="body2">{copy.last}: {date(job.cpqLastAttemptAt)}</Typography>
              {(job.cpqStatus === 'pending' || job.cpqStatus === 'retry') && <Typography variant="body2">{copy.next}: {date(job.cpqNextAttemptAt)}</Typography>}
              {job.cpqLeaseExpiresAt && <Typography variant="body2">{copy.lease}: {date(job.cpqLeaseExpiresAt)}</Typography>}
              {job.cpqCompletedAt && <Typography variant="body2">{copy.finished}: {date(job.cpqCompletedAt)}</Typography>}
              {job.cpqLastOutcome && <Typography variant="body2">{copy.outcome}: {job.cpqLastOutcome}</Typography>}
            </CardContent>
          </Card>)}
        </Box>
        <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
          <Button disabled={offset === 0} onClick={() => setOffset(Math.max(0, offset - report.cpqsLimit))}>{copy.previous}</Button>
          <Typography variant="caption">{copy.page}: {report.cpqsOffset}</Typography>
          <Button disabled={!report.cpqsHasMore || offset + report.cpqsLimit > 10000} onClick={() => setOffset(offset + report.cpqsLimit)}>{copy.more}</Button>
        </Stack>
        {report.cpqsHasMore && offset + report.cpqsLimit > 10000 && <Alert severity="warning">{copy.limit}</Alert>}
      </>}
    </Stack>
  );
}
