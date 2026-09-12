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
  type CommercePaymentOverview,
  type CommerceProviderEvent,
  type CommerceProviderEventStatus,
} from '../api/commerceOperations';

const FILTER_STATUSES: CommerceProviderEventStatus[] = [
  'dead_letter', 'retry', 'processing', 'pending', 'processed', 'ignored',
];

const COPY = {
  es: {
    title: 'Operaciones de pago',
    subtitle: 'Preparación de proveedores, estados financieros e inbox verificado.',
    warning: 'Esta vista nunca expone el payload cifrado. Reintentar no marca una orden como pagada: el worker vuelve a validar los enlaces inmutables antes de cualquier transición.',
    filter: 'Estado', all: 'Todos', refresh: 'Actualizar', empty: 'No hay eventos para este filtro.',
    attempts: 'Intentos', received: 'Recibido', next: 'Próximo intento', checkout: 'Checkout',
    replay: 'Reintentar evento', dialogTitle: 'Reintentar evento en dead letter',
    reason: 'Motivo y corrección aplicada', cancel: 'Cancelar', confirm: 'Registrar y reintentar',
    reasonHelp: 'Describe la causa reparada en una sola línea (8–500 caracteres). La evidencia queda inmutable.',
    loadError: 'No se pudo cargar el inbox de eventos de pago.',
    replayError: 'No se pudo registrar el reintento.',
    readiness: 'Preparación de proveedores', ready: 'Listo', blocked: 'Bloqueado',
    contract: 'Contrato', credentials: 'Credenciales', feature: 'Activación',
    verifiedCapabilities: 'Capacidades verificadas', noCapabilities: 'Sin capacidades verificadas',
    overviewError: 'No se pudo cargar el resumen financiero de pagos.',
    paymentStates: 'Estados canónicos', captured: 'Capturado', refunded: 'Reembolsado',
    reconciliation: 'Excepciones de conciliación', settlements: 'Liquidaciones',
    sellerBalances: 'Saldos de vendedores', payouts: 'Pagos a vendedores',
    refunds: 'Reembolsos', disputes: 'Disputas', financialBreakdown: 'Desglose financiero',
    commissions: 'Comisiones', providerFees: 'Tarifas del proveedor', taxes: 'Impuestos',
    sellerNet: 'Neto del vendedor', gross: 'Bruto', net: 'Neto', withholding: 'Retenciones',
    chargebacks: 'Contracargos', amount: 'Monto', source: 'Fuente', expected: 'Esperado', actual: 'Real',
    records: 'registros', noRecords: 'Sin registros', providerEvents: 'Eventos de proveedor',
  },
  en: {
    title: 'Payment operations',
    subtitle: 'Provider readiness, financial states, and a verified event inbox.',
    warning: 'This view never exposes the encrypted payload. Replaying does not mark an order paid: the worker revalidates immutable bindings before any transition.',
    filter: 'Status', all: 'All', refresh: 'Refresh', empty: 'No events match this filter.',
    attempts: 'Attempts', received: 'Received', next: 'Next attempt', checkout: 'Checkout',
    replay: 'Replay event', dialogTitle: 'Replay dead-letter event',
    reason: 'Reason and remediation applied', cancel: 'Cancel', confirm: 'Record and replay',
    reasonHelp: 'Describe the repaired cause on one line (8–500 characters). Evidence is immutable.',
    loadError: 'The payment-event inbox could not be loaded.',
    replayError: 'The replay could not be recorded.',
    readiness: 'Provider readiness', ready: 'Ready', blocked: 'Blocked',
    contract: 'Contract', credentials: 'Credentials', feature: 'Activation',
    verifiedCapabilities: 'Verified capabilities', noCapabilities: 'No verified capabilities',
    overviewError: 'The payment financial overview could not be loaded.',
    paymentStates: 'Canonical states', captured: 'Captured', refunded: 'Refunded',
    reconciliation: 'Reconciliation exceptions', settlements: 'Settlements',
    sellerBalances: 'Seller balances', payouts: 'Seller payouts',
    refunds: 'Refunds', disputes: 'Disputes', financialBreakdown: 'Financial breakdown',
    commissions: 'Commissions', providerFees: 'Provider fees', taxes: 'Taxes',
    sellerNet: 'Seller net', gross: 'Gross', net: 'Net', withholding: 'Withholding',
    chargebacks: 'Chargebacks', amount: 'Amount', source: 'Source', expected: 'Expected', actual: 'Actual',
    records: 'records', noRecords: 'No records', providerEvents: 'Provider events',
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

const formatMinor = (minor: number, currency: string, locale: string) => {
  try {
    return new Intl.NumberFormat(locale, { style: 'currency', currency }).format(minor / 100);
  } catch {
    return `${currency} ${(minor / 100).toFixed(2)}`;
  }
};

const overviewRecordCount = (overview: CommercePaymentOverview | undefined) => ({
  reconciliation: overview?.cpoReconciliationExceptions.reduce((sum, item) => sum + item.crsCount, 0) ?? 0,
  settlements: overview?.cpoSettlements.reduce((sum, item) => sum + item.cssCount, 0) ?? 0,
  sellerBalances: overview?.cpoSellerBalances.reduce((sum, item) => sum + item.csbEntryCount, 0) ?? 0,
  payouts: overview?.cpoPayouts.reduce((sum, item) => sum + item.cpsCount, 0) ?? 0,
  refunds: overview?.cpoRefunds.reduce((sum, item) => sum + item.crfCount, 0) ?? 0,
  disputes: overview?.cpoDisputes.reduce((sum, item) => sum + item.cdsCount, 0) ?? 0,
});

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

  const overviewQuery = useQuery({
    queryKey: ['commerce-payment-overview'],
    queryFn: CommerceOperations.getPaymentOverview,
    retry: false,
  });

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
  const overviewCounts = overviewRecordCount(overviewQuery.data);

  return (
    <PageShell
      title={copy.title}
      subtitle={copy.subtitle}
      actions={(
        <Button
          variant="outlined"
          startIcon={<RefreshOutlinedIcon />}
          onClick={() => void Promise.all([eventsQuery.refetch(), overviewQuery.refetch()])}
          disabled={eventsQuery.isFetching || overviewQuery.isFetching}
        >
          {copy.refresh}
        </Button>
      )}
    >
      <Stack spacing={2}>
        <Alert severity="info">{copy.warning}</Alert>
        {overviewQuery.isError && <Alert severity="error">{copy.overviewError}</Alert>}
        {overviewQuery.data && (
          <Stack spacing={2}>
            <Typography variant="h6">{copy.readiness}</Typography>
            <Box sx={{ display: 'grid', gap: 2, gridTemplateColumns: { xs: '1fr', lg: 'repeat(2, minmax(0, 1fr))' } }}>
              {overviewQuery.data.cpoProviderAccounts.map((account) => {
                const ready = account.cpaEnabled
                  && account.cpaFeatureEnabled
                  && account.cpaStatus === 'ready'
                  && account.cpaContractStatus === 'approved'
                  && account.cpaCredentialStatus === 'validated';
                const capabilities = account.cpaCapabilities
                  .filter((capability) => capability.cpcVerificationStatus.endsWith('_verified'));
                return (
                  <Card key={`${account.cpaEnvironment}:${account.cpaProvider}`} variant="outlined" data-testid="commerce-provider-readiness-card">
                    <CardContent>
                      <Stack spacing={1}>
                        <Stack direction="row" gap={1} flexWrap="wrap" alignItems="center">
                          <Typography variant="subtitle1" fontWeight={700}>{account.cpaProvider}</Typography>
                          <Chip size="small" variant="outlined" label={account.cpaEnvironment} />
                          <Chip size="small" color={ready ? 'success' : 'warning'} label={ready ? copy.ready : copy.blocked} />
                        </Stack>
                        <Typography variant="body2">{copy.contract}: {account.cpaContractStatus}</Typography>
                        <Typography variant="body2">{copy.credentials}: {account.cpaCredentialStatus}</Typography>
                        <Typography variant="body2">{copy.feature}: {account.cpaFeatureEnabled ? copy.ready : copy.blocked}</Typography>
                        <Typography variant="caption" color="text.secondary">
                          {copy.verifiedCapabilities}: {capabilities.length > 0
                            ? capabilities.map((capability) => `${capability.cpcPaymentMethod}/${capability.cpcCapability}`).join(', ')
                            : copy.noCapabilities}
                        </Typography>
                        {account.cpaDisabledReason && <Alert severity="warning">{account.cpaDisabledReason}</Alert>}
                      </Stack>
                    </CardContent>
                  </Card>
                );
              })}
            </Box>

            <Typography variant="h6">{copy.paymentStates}</Typography>
            {overviewQuery.data.cpoPaymentIntents.length === 0
              ? <Typography color="text.secondary">{copy.noRecords}</Typography>
              : (
                <Box sx={{ display: 'grid', gap: 1.5, gridTemplateColumns: { xs: '1fr', md: 'repeat(3, minmax(0, 1fr))' } }}>
                  {overviewQuery.data.cpoPaymentIntents.map((summary) => (
                    <Card key={`${summary.cpiCurrency}:${summary.cpiStatus}`} variant="outlined">
                      <CardContent>
                        <Typography variant="subtitle2">{summary.cpiStatus} · {summary.cpiCount} {copy.records}</Typography>
                        <Typography variant="body2">{copy.captured}: {formatMinor(summary.cpiCapturedMinor, summary.cpiCurrency, locale)}</Typography>
                        <Typography variant="body2">{copy.refunded}: {formatMinor(summary.cpiRefundedMinor, summary.cpiCurrency, locale)}</Typography>
                      </CardContent>
                    </Card>
                  ))}
                </Box>
              )}

            <Box sx={{ display: 'grid', gap: 1.5, gridTemplateColumns: { xs: '1fr', sm: 'repeat(2, minmax(0, 1fr))', xl: 'repeat(3, minmax(0, 1fr))' } }}>
              {([
                [copy.reconciliation, overviewCounts.reconciliation],
                [copy.settlements, overviewCounts.settlements],
                [copy.sellerBalances, overviewCounts.sellerBalances],
                [copy.payouts, overviewCounts.payouts],
                [copy.refunds, overviewCounts.refunds],
                [copy.disputes, overviewCounts.disputes],
              ] as const).map(([label, count]) => (
                <Card key={label} variant="outlined">
                  <CardContent>
                    <Typography variant="subtitle2">{label}</Typography>
                    <Typography variant="h5">{count}</Typography>
                    <Typography variant="caption" color="text.secondary">{copy.records}</Typography>
                  </CardContent>
                </Card>
              ))}
            </Box>

            <Typography variant="h6">{copy.financialBreakdown}</Typography>
            <Box sx={{ display: 'grid', gap: 1.5, gridTemplateColumns: { xs: '1fr', md: 'repeat(2, minmax(0, 1fr))' } }}>
              {overviewQuery.data.cpoAmountComponents.map((item) => (
                <Card key={`${item.cacCurrency}:${item.cacComponentType}:${item.cacSource}`} variant="outlined">
                  <CardContent>
                    <Typography variant="subtitle2">{item.cacComponentType} · {item.cacCount} {copy.records}</Typography>
                    <Typography variant="body2">{copy.amount}: {formatMinor(item.cacAmountMinor, item.cacCurrency, locale)}</Typography>
                    <Typography variant="caption" color="text.secondary">{copy.source}: {item.cacSource}</Typography>
                  </CardContent>
                </Card>
              ))}
              {overviewQuery.data.cpoCommissions.map((item) => (
                <Card key={`${item.ccmEnvironment}:${item.ccmProvider}:${item.ccmCurrency}`} variant="outlined">
                  <CardContent>
                    <Typography variant="subtitle2">{copy.commissions} · {item.ccmProvider} · {item.ccmEnvironment}</Typography>
                    <Typography variant="body2">{copy.gross}: {formatMinor(item.ccmBasisAmountMinor, item.ccmCurrency, locale)}</Typography>
                    <Typography variant="body2">{copy.commissions}: {formatMinor(item.ccmCommissionMinor, item.ccmCurrency, locale)}</Typography>
                    <Typography variant="body2">{copy.providerFees}: {formatMinor(item.ccmProviderFeeMinor, item.ccmCurrency, locale)}</Typography>
                    <Typography variant="body2">{copy.taxes}: {formatMinor(item.ccmTaxMinor, item.ccmCurrency, locale)}</Typography>
                    <Typography variant="body2">{copy.sellerNet}: {formatMinor(item.ccmSellerNetMinor, item.ccmCurrency, locale)}</Typography>
                  </CardContent>
                </Card>
              ))}
              {overviewQuery.data.cpoSettlements.map((item) => (
                <Card key={`${item.cssEnvironment}:${item.cssProvider}:${item.cssCurrency}:${item.cssStatus}`} variant="outlined">
                  <CardContent>
                    <Typography variant="subtitle2">{copy.settlements} · {item.cssProvider} · {item.cssStatus}</Typography>
                    <Typography variant="body2">{copy.gross}: {formatMinor(item.cssGrossMinor, item.cssCurrency, locale)}</Typography>
                    <Typography variant="body2">{copy.providerFees}: {formatMinor(item.cssFeeMinor, item.cssCurrency, locale)}</Typography>
                    <Typography variant="body2">{copy.withholding}: {formatMinor(item.cssWithholdingMinor, item.cssCurrency, locale)}</Typography>
                    <Typography variant="body2">{copy.refunds}: {formatMinor(item.cssRefundMinor, item.cssCurrency, locale)}</Typography>
                    <Typography variant="body2">{copy.chargebacks}: {formatMinor(item.cssChargebackMinor, item.cssCurrency, locale)}</Typography>
                    <Typography variant="body2">{copy.net}: {formatMinor(item.cssNetMinor, item.cssCurrency, locale)}</Typography>
                  </CardContent>
                </Card>
              ))}
              {overviewQuery.data.cpoRefunds.map((item) => (
                <Card key={`${item.crfEnvironment}:${item.crfProvider}:${item.crfCurrency}:${item.crfStatus}`} variant="outlined">
                  <CardContent>
                    <Typography variant="subtitle2">{copy.refunds} · {item.crfProvider} · {item.crfStatus}</Typography>
                    <Typography variant="body2">{formatMinor(item.crfAmountMinor, item.crfCurrency, locale)} · {item.crfCount} {copy.records}</Typography>
                  </CardContent>
                </Card>
              ))}
              {overviewQuery.data.cpoDisputes.map((item) => (
                <Card key={`${item.cdsEnvironment}:${item.cdsProvider}:${item.cdsCurrency}:${item.cdsKind}:${item.cdsStatus}`} variant="outlined">
                  <CardContent>
                    <Typography variant="subtitle2">{copy.disputes} · {item.cdsProvider} · {item.cdsKind}</Typography>
                    <Typography variant="body2">{item.cdsStatus} · {formatMinor(item.cdsAmountMinor, item.cdsCurrency, locale)} · {item.cdsCount} {copy.records}</Typography>
                  </CardContent>
                </Card>
              ))}
              {overviewQuery.data.cpoSellerBalances.map((item) => (
                <Card key={`${item.csbEnvironment}:${item.csbProvider}:${item.csbCurrency}:${item.csbAvailability}`} variant="outlined">
                  <CardContent>
                    <Typography variant="subtitle2">{copy.sellerBalances} · {item.csbProvider} · {item.csbAvailability}</Typography>
                    <Typography variant="body2">{formatMinor(item.csbNetAmountMinor, item.csbCurrency, locale)} · {item.csbEntryCount} {copy.records}</Typography>
                  </CardContent>
                </Card>
              ))}
              {overviewQuery.data.cpoPayouts.map((item) => (
                <Card key={`${item.cpsEnvironment}:${item.cpsProvider}:${item.cpsCurrency}:${item.cpsStatus}`} variant="outlined">
                  <CardContent>
                    <Typography variant="subtitle2">{copy.payouts} · {item.cpsProvider} · {item.cpsStatus}</Typography>
                    <Typography variant="body2">{formatMinor(item.cpsAmountMinor, item.cpsCurrency, locale)} · {item.cpsCount} {copy.records}</Typography>
                  </CardContent>
                </Card>
              ))}
              {overviewQuery.data.cpoReconciliationExceptions.map((item) => (
                <Card key={`${item.crsEnvironment}:${item.crsProvider}:${item.crsCurrency ?? 'none'}:${item.crsStatus}`} variant="outlined">
                  <CardContent>
                    <Typography variant="subtitle2">{copy.reconciliation} · {item.crsProvider} · {item.crsStatus}</Typography>
                    {item.crsCurrency && <Typography variant="body2">{copy.expected}: {formatMinor(item.crsExpectedMinor, item.crsCurrency, locale)} · {copy.actual}: {formatMinor(item.crsActualMinor, item.crsCurrency, locale)}</Typography>}
                    <Typography variant="caption" color="text.secondary">{item.crsCount} {copy.records}</Typography>
                  </CardContent>
                </Card>
              ))}
            </Box>
            {overviewQuery.data.cpoAmountComponents.length === 0
              && overviewQuery.data.cpoCommissions.length === 0
              && overviewQuery.data.cpoSettlements.length === 0
              && overviewQuery.data.cpoRefunds.length === 0
              && overviewQuery.data.cpoDisputes.length === 0
              && overviewQuery.data.cpoSellerBalances.length === 0
              && overviewQuery.data.cpoPayouts.length === 0
              && overviewQuery.data.cpoReconciliationExceptions.length === 0
              && <Typography color="text.secondary">{copy.noRecords}</Typography>}
          </Stack>
        )}

        <Typography variant="h6">{copy.providerEvents}</Typography>
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
