import { useEffect, useMemo, useRef, useState } from 'react';
import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Card,
  CardContent,
  Divider,
  Grid,
  MenuItem,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  TextField,
  Typography,
  Snackbar,
} from '@mui/material';
import AddCircleIcon from '@mui/icons-material/AddCircle';
import RefreshIcon from '@mui/icons-material/Refresh';
import DescriptionIcon from '@mui/icons-material/Description';
import ReceiptIcon from '@mui/icons-material/Receipt';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import type { PaymentCreate, PaymentDTO } from '../api/types';
import { Payments } from '../api/payments';
import GoogleDriveUploadWidget from '../components/GoogleDriveUploadWidget';
import { useOptionalSession } from '../session/SessionContext';
import { useOwnedTabDraft } from '../session/useOwnedTabDraft';
import { parseFormMetadata } from '../session/recoverableDraft';
import { useTranslation } from 'react-i18next';
import type { DriveFileInfo } from '../services/googleDrive';
import { toLocalDateInputValue } from '../utils/dateOnly';
import SessionInvoiceGeneratorCard from '../components/SessionInvoiceGeneratorCard';
import LazyPaginatedList from '../components/LazyPaginatedList';
import { useCurrency } from '../contexts/CurrencyContext';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { useDocumentTitle } from '../hooks/useDocumentTitle';
import { EmptyState } from '../components/PageShell';
import { PartySelector } from '../components/party-selector/PartySelector';
import type { PartySelectorOption } from '../api/partySelector';

const PAYMENT_METHODS = ['Produbanco', 'Bank', 'Cash', 'Card', 'Crypto', 'Other'] as const;
const CONCEPT_PRESETS = ['Honorarios', 'Adelanto', 'Licencia', 'Reembolso', 'Otros'];
const MONTH_CODES = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'] as const;

const toPeriod = (isoDate: string) => {
  const raw = isoDate.trim();
  const match = /^(\d{4})-(\d{2})-(\d{2})(?:$|T)/.exec(raw);
  if (!match) return '';
  const [, yearRaw, monthRaw, dayRaw] = match;
  if (!yearRaw || !monthRaw || !dayRaw) return '';
  const year = Number.parseInt(yearRaw, 10);
  const month = Number.parseInt(monthRaw, 10);
  const day = Number.parseInt(dayRaw, 10);
  if (!Number.isSafeInteger(year) || month < 1 || month > 12 || day < 1 || day > 31) return '';
  const utcDate = new Date(Date.UTC(year, month - 1, day));
  if (
    utcDate.getUTCFullYear() !== year
    || utcDate.getUTCMonth() !== month - 1
    || utcDate.getUTCDate() !== day
  ) {
    return '';
  }
  return `${MONTH_CODES[month - 1]}-${year}`;
};

const parseOptionalPositiveInt = (value: string): number | null | 'invalid' => {
  const raw = value.trim();
  if (!raw) return null;
  if (!/^\d+$/.test(raw)) return 'invalid';
  const parsed = Number.parseInt(raw, 10);
  return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : 'invalid';
};

export function PaymentForm({
  onCreated,
  defaultParty,
  payments,
}: {
  onCreated: () => void;
  defaultParty?: PartySelectorOption | null;
  payments: PaymentDTO[];
}) {
  const { currency: preferredCurrency, supportedCurrencies } = useLocalePreferences();
  const { formatMoney } = useCurrency();
  const [toast, setToast] = useState<string | null>(null);
  const qc = useQueryClient();
  const recoverySession = useOptionalSession();
  const ownerId = recoverySession?.loading ? null : recoverySession?.session?.partyId ?? null;
  const { i18n: recoveryLocale } = useTranslation();
  const initialDraft = {
    selectedParty: null as PartySelectorOption | null, paidAt: toLocalDateInputValue(), amount: '', currency: preferredCurrency,
    method: 'Produbanco', reference: 'N/A', concept: 'Honorarios', period: toPeriod(toLocalDateInputValue()),
    attachmentUrl: '', attachmentName: '', invoiceId: '', orderId: '',
  };
  const recovery = useOwnedTabDraft(initialDraft, {
    ownerId, scope: `payment-composer:${defaultParty?.partyId ?? 'general'}`,
    parse: (value) => {
      if (!value || typeof value !== 'object') return undefined;
      const data = value as Record<string, unknown>;
      const partyValue = data['selectedParty'];
      let party: PartySelectorOption | null = null;
      if (partyValue !== null) {
        if (!partyValue || typeof partyValue !== 'object') return undefined;
        const p = partyValue as Record<string, unknown>;
        const id = p['partyId'], kind = p['partyType'], name = p['displayName'], status = p['accountStatus'];
        if (typeof id !== 'number' || !Number.isSafeInteger(id) || id <= 0 || typeof name !== 'string'
          || (kind !== 'person' && kind !== 'organization') || (status !== 'active' && status !== 'inactive' && status !== 'no-account')) return undefined;
        const username = p['username'], avatar = p['avatarUrl'], secondary = p['secondaryLabel'];
        if ((username !== null && typeof username !== 'string') || (avatar !== null && typeof avatar !== 'string')
          || (secondary !== null && typeof secondary !== 'string')) return undefined;
        party = { partyId: id, partyType: kind, displayName: name, accountStatus: status, username, avatarUrl: avatar, secondaryLabel: secondary };
      }
      const fields = parseFormMetadata({ ...data, selectedParty: null }, initialDraft);
      return fields ? { ...fields, selectedParty: party } : undefined;
    },
  });
  const currentRecovery = useRef(recovery);
  currentRecovery.current = recovery;
  const [selectedParty, setSelectedParty] = recovery.field('selectedParty');
  const [paidAt, setPaidAt] = recovery.field('paidAt');
  const [amount, setAmount] = recovery.field('amount');
  const [currency, setCurrency] = recovery.field('currency');
  const [method, setMethod] = recovery.field('method');
  const [reference, setReference] = recovery.field('reference');
  const [concept, setConcept] = recovery.field('concept');
  const [period, setPeriod] = recovery.field('period');
  const [attachmentUrl, setAttachmentUrl] = recovery.field('attachmentUrl');
  const [attachmentName, setAttachmentName] = recovery.field('attachmentName');
  const [invoiceId, setInvoiceId] = recovery.field('invoiceId');
  const [orderId, setOrderId] = recovery.field('orderId');
  const [error, setError] = useState<string | null>(null);
  const [fieldHints, setFieldHints] = useState<{ amount?: string; party?: string; date?: string }>({});
  const invoiceOptions = useMemo(
    () => Array.from(new Set(payments.map((p) => p.payInvoiceId).filter(Boolean))).map((v) => String(v)),
    [payments],
  );
  const orderOptions = useMemo(
    () => Array.from(new Set(payments.map((p) => p.payOrderId).filter(Boolean))).map((v) => String(v)),
    [payments],
  );
  const lastPaymentForParty = useMemo(() => {
    if (!selectedParty) return null;
    return (
      payments
        .filter((p) => p.payPartyId === selectedParty.partyId)
        .sort((a, b) => (a.payId < b.payId ? 1 : -1))[0] ?? null
    );
  }, [payments, selectedParty]);

  const priorDefault = useRef({ ownerId, value: defaultParty });
  useEffect(() => {
    const previous = priorDefault.current;
    priorDefault.current = { ownerId, value: defaultParty };
    if (previous.ownerId !== ownerId && previous.value === defaultParty) return;
    if (!currentRecovery.current.restored) setSelectedParty(defaultParty ?? null);
  }, [defaultParty, ownerId, setSelectedParty]);

  useEffect(() => {
    if (!currentRecovery.current.restored) setPeriod(toPeriod(paidAt));
  }, [paidAt, setPeriod]);

  const lastPrefilledParty = useRef<number | null>(null);
  useEffect(() => {
    if (!selectedParty) { lastPrefilledParty.current = null; return; }
    if (lastPrefilledParty.current === selectedParty.partyId) return;
    lastPrefilledParty.current = selectedParty.partyId;
    // Background queries and reload recovery must not replace edited values.
    // The explicit copy-last-payment action remains available below.
    if (!currentRecovery.current.restored && lastPaymentForParty) {
      setConcept((prev) => lastPaymentForParty.payConcept ?? prev);
      setMethod(lastPaymentForParty.payMethod);
      setCurrency(lastPaymentForParty.payCurrency);
      setReference(lastPaymentForParty.payReference ?? 'N/A');
      setPeriod((prev) => lastPaymentForParty.payPeriod ?? prev);
    }
  }, [selectedParty, lastPaymentForParty, setConcept, setMethod, setCurrency, setReference, setPeriod]);

  const mutation = useMutation<PaymentDTO, Error, { body: PaymentCreate; submittedDraft: typeof recovery.value; isCurrentOwner: () => boolean }>({
    mutationFn: (request) => {
      if (!request.isCurrentOwner()) throw new Error('La sesión del formulario cambió.');
      return Payments.create(request.body);
    },
    onSuccess: (_, request) => {
      if (!request.isCurrentOwner()) return;
      void qc.invalidateQueries({ queryKey: ['payments'] });
      if (JSON.stringify(currentRecovery.current.value) !== JSON.stringify(request.submittedDraft)) {
        setToast('Pago registrado. Conservamos tus cambios posteriores sin guardar.');
        return;
      }
      recovery.discard();
      setAmount('');
      setReference('N/A');
      setAttachmentUrl('');
      setAttachmentName('');
      onCreated();
      setToast('Pago registrado');
    },
    onError: (err, request) => { if (request.isCurrentOwner()) setError(err.message); },
  });

  const resetPaymentMutation = mutation.reset;
  useEffect(() => { resetPaymentMutation(); setToast(null); }, [ownerId, defaultParty?.partyId, resetPaymentMutation]);

  const handleSubmit = () => {
    const parsedPartyId = selectedParty?.partyId;
    if (!parsedPartyId) {
      setError('Elige un contacto de la lista.');
      setFieldHints((prev) => ({ ...prev, party: 'Selecciona un contacto de la lista desplegable.' }));
      return;
    }
    const normalizedAmount = Number.parseFloat(amount.replace(',', '.'));
    if (Number.isNaN(normalizedAmount) || normalizedAmount <= 0) {
      setError('Ingresa un monto valido');
      setFieldHints((prev) => ({ ...prev, amount: 'Usa solo números. Ej: 120.50' }));
      return;
    }
    if (!paidAt) {
      setError('Selecciona una fecha de pago');
      setFieldHints((prev) => ({ ...prev, date: 'Selecciona la fecha del pago.' }));
      return;
    }
    const parsedOrderId = parseOptionalPositiveInt(orderId);
    if (parsedOrderId === 'invalid') {
      setError('Orden inválida: usa un ID numérico positivo o déjalo vacío.');
      return;
    }
    const parsedInvoiceId = parseOptionalPositiveInt(invoiceId);
    if (parsedInvoiceId === 'invalid') {
      setError('Factura inválida: usa un ID numérico positivo o déjalo vacío.');
      return;
    }
    const conceptValue = concept.trim() || 'Honorarios';
    const periodValue = period.trim() || null;
    setError(null);
    setFieldHints({});
    const payload: PaymentCreate = {
      pcPartyId: parsedPartyId,
      pcOrderId: parsedOrderId,
      pcInvoiceId: parsedInvoiceId,
      pcAmountCents: Math.round(normalizedAmount * 100),
      pcCurrency: currency.trim() || preferredCurrency,
      pcMethod: method,
      pcReference: reference.trim() || null,
      pcPaidAt: paidAt,
      pcConcept: conceptValue,
      pcPeriod: periodValue,
      pcAttachmentUrl: attachmentUrl.trim() || null,
    };
    mutation.mutate({ body: payload, submittedDraft: recovery.value, isCurrentOwner: recovery.isCurrentOwner });
  };

  return (
    <Card variant="outlined">
      <CardContent>
        <Stack direction="row" alignItems="center" justifyContent="space-between" sx={{ mb: 2 }}>
          <Typography variant="h6">Nuevo pago manual</Typography>
          <Button
            startIcon={<AddCircleIcon />}
            variant="contained"
            onClick={handleSubmit}
            disabled={mutation.isPending}
          >
            Registrar pago
          </Button>
        </Stack>
          <Grid container spacing={2}>
            <Grid item xs={12} md={4}>
              <PartySelector value={selectedParty} onChange={setSelectedParty} field={{ label: 'Contacto', required: true, helperText: fieldHints.party ?? 'Busca por nombre o @username.' }} search={{ context: 'billing_contact', kind: 'any', accountOnly: false }} />
          </Grid>
          <Grid item xs={12} md={4}>
            <TextField
              label="Fecha de pago"
              type="date"
              fullWidth
              value={paidAt}
              onChange={(e) => setPaidAt(e.target.value)}
              InputLabelProps={{ shrink: true }}
              required
              helperText={fieldHints.date}
              error={Boolean(fieldHints.date)}
            />
          </Grid>
            <Grid item xs={12} md={4}>
              <TextField
                label="Periodo"
                fullWidth
                value={period}
                onChange={(e) => setPeriod(e.target.value.toUpperCase())}
              placeholder="EJ. DEC-2025"
            />
          </Grid>
          <Grid item xs={12} md={4}>
            <TextField
              label={`Monto (${currency})`}
              fullWidth
              value={amount}
              onChange={(e) => setAmount(e.target.value)}
              placeholder="Ej. 399.00"
              required
              InputProps={{ startAdornment: <Box sx={{ mr: 1, fontWeight: 700 }}>{currency}</Box> }}
              helperText={fieldHints.amount}
              error={Boolean(fieldHints.amount)}
            />
          </Grid>
          <Grid item xs={12} md={4}>
            <TextField
              label="Moneda"
              select
              fullWidth
              value={currency}
              onChange={(e) => setCurrency(e.target.value)}
            >
              {supportedCurrencies.map((code) => (
                <MenuItem key={code} value={code}>
                  {code}
                </MenuItem>
              ))}
            </TextField>
          </Grid>
          <Grid item xs={12} md={4}>
            <TextField
              label="Metodo"
              select
              fullWidth
              value={method}
              onChange={(e) => setMethod(e.target.value)}
            >
              {PAYMENT_METHODS.map((m) => (
                <MenuItem key={m} value={m}>
                  {m}
                </MenuItem>
              ))}
            </TextField>
          </Grid>
          <Grid item xs={12} md={6}>
            <Autocomplete
              freeSolo
              options={CONCEPT_PRESETS}
              value={concept}
              onChange={(_, value) => setConcept(value ?? '')}
              inputValue={concept}
              onInputChange={(_, value) => setConcept(value)}
              renderInput={(params) => (
                <TextField
                  {...params}
                  label="Concepto"
                  helperText="Elige o escribe el concepto del pago."
                />
              )}
            />
          </Grid>
          <Grid item xs={12} md={6}>
            <TextField
              label="Referencia"
              fullWidth
              value={reference}
              onChange={(e) => setReference(e.target.value)}
              placeholder="Transferencia, recibo, etc."
            />
          </Grid>
          <Grid item xs={12} md={6}>
            <Autocomplete
              freeSolo
              options={invoiceOptions}
              value={invoiceId}
              onChange={(_, value) => setInvoiceId(value ?? '')}
              inputValue={invoiceId}
              onInputChange={(_, value) => setInvoiceId(value)}
              renderInput={(params) => (
                <TextField
                  {...params}
                  label="Factura (ID opcional)"
                  placeholder="Vincula con factura si aplica"
                />
              )}
            />
          </Grid>
          <Grid item xs={12} md={6}>
            <Autocomplete
              freeSolo
              options={orderOptions}
              value={orderId}
              onChange={(_, value) => setOrderId(value ?? '')}
              inputValue={orderId}
              onInputChange={(_, value) => setOrderId(value)}
              renderInput={(params) => (
                <TextField
                  {...params}
                  label="Orden (ID opcional)"
                  placeholder="Vincula con orden si aplica"
                />
              )}
            />
          </Grid>
          <Grid item xs={12}>
            <Stack spacing={1.5}>
              {recovery.restored && <Alert severity="info">{recoveryLocale.language.startsWith('en') ? 'We restored your unsaved payment in this tab.' : 'Recuperamos el pago sin guardar de esta pestaña.'}</Alert>}
              <GoogleDriveUploadWidget
                recoveryScope={`payment:${selectedParty?.partyId ?? 'new'}`}
                formRecoveryAvailable={recovery.persistenceAvailable}
                label={attachmentName ? `Adjunto: ${attachmentName}` : 'Subir comprobante (PDF/imagen)'}
                helperText="Elige dónde guardar el comprobante. Los archivos de TDF conservan acceso privado."
                accept="application/pdf,image/*"
                multiple={false}
                onComplete={(files: DriveFileInfo[]) => {
                  const file = files[0];
                  if (!file) return;
                  setAttachmentUrl(file.publicUrl ?? file.webContentLink ?? file.webViewLink ?? '');
                  setAttachmentName(file.name);
                  setError(null);
                }}
                dense
                />
              <TextField
                label="URL de respaldo (opcional)"
                fullWidth
                value={attachmentUrl}
                onChange={(e) => setAttachmentUrl(e.target.value)}
                placeholder="Link a comprobante o carpeta"
                helperText="Puedes pegar un enlace existente si ya tienes el archivo."
              />
            </Stack>
          </Grid>
        </Grid>
        {error && (
          <Alert severity="error" sx={{ mt: 2 }}>
            {error}
          </Alert>
        )}
        {mutation.isSuccess && !error && (
          <Alert severity="success" sx={{ mt: 2 }}>
            Pago registrado correctamente.
          </Alert>
        )}
        {lastPaymentForParty && (
          <Alert severity="info" sx={{ mt: 2 }}>
            Último pago de este contacto: {formatMoney(lastPaymentForParty.payAmountCents / 100, lastPaymentForParty.payCurrency)} · {lastPaymentForParty.payMethod}.{' '}
            <Button
              size="small"
              onClick={() => {
                setAmount(String(lastPaymentForParty.payAmountCents / 100));
                setCurrency(lastPaymentForParty.payCurrency);
                setMethod(lastPaymentForParty.payMethod);
                setConcept(lastPaymentForParty.payConcept ?? concept);
                setReference(lastPaymentForParty.payReference ?? 'N/A');
                setPeriod(lastPaymentForParty.payPeriod ?? period);
              }}
            >
              Copiar datos
            </Button>
          </Alert>
        )}
        <Snackbar
          open={Boolean(toast)}
          autoHideDuration={2200}
          onClose={() => setToast(null)}
          anchorOrigin={{ vertical: 'bottom', horizontal: 'center' }}
          message={toast ?? ''}
        />
      </CardContent>
    </Card>
  );
}

export default function PaymentsPage() {
  useDocumentTitle('Finanzas / Pagos');
  const { formatMoney } = useCurrency();
  const [partyFilter, setPartyFilter] = useState<PartySelectorOption | null>(null);
  const [fromFilter, setFromFilter] = useState<string>('');
  const [toFilter, setToFilter] = useState<string>('');
  const [methodFilter, setMethodFilter] = useState<string>('all');

  const paymentsQuery = useQuery<PaymentDTO[]>({
    queryKey: ['payments', partyFilter?.partyId ?? 'all'],
    queryFn: () => Payments.list(partyFilter?.partyId),
  });

  const payments = useMemo(() => paymentsQuery.data ?? [], [paymentsQuery.data]);
  const filteredPayments = useMemo(
    () =>
      payments.filter((pay) => {
        if (fromFilter && pay.payPaidAt < fromFilter) return false;
        if (toFilter && pay.payPaidAt > toFilter) return false;
        if (methodFilter !== 'all' && pay.payMethod !== methodFilter) return false;
        return true;
      }),
    [fromFilter, methodFilter, payments, toFilter],
  );
  const paymentsWithAttachments = useMemo(
    () => filteredPayments.filter((payment) => payment.payAttachment),
    [filteredPayments],
  );
  const paymentPaginationResetKey = [partyFilter?.partyId ?? 'all', fromFilter, toFilter, methodFilter].join('|');

  return (
    <Box>
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" gap={2} sx={{ mb: 3 }}>
        <Box>
          <Typography variant="h4" gutterBottom>
            Pagos manuales
          </Typography>
          <Typography color="text.secondary">
            Registra pagos manuales y genera facturas por sesión desde el mismo módulo financiero.
          </Typography>
        </Box>
        <Stack direction="row" gap={1} flexWrap="wrap" alignItems="center">
          <Box sx={{ minWidth: 280 }}><PartySelector value={partyFilter} onChange={setPartyFilter} field={{ label: 'Filtrar por contacto' }} search={{ context: 'billing_contact', kind: 'any', accountOnly: false }} /></Box>
          <TextField
            label="Desde"
            type="date"
            size="small"
            value={fromFilter}
            onChange={(e) => setFromFilter(e.target.value)}
            InputLabelProps={{ shrink: true }}
          />
          <TextField
            label="Hasta"
            type="date"
            size="small"
            value={toFilter}
            onChange={(e) => setToFilter(e.target.value)}
            InputLabelProps={{ shrink: true }}
          />
          <TextField
            label="Metodo"
            size="small"
            select
            value={methodFilter}
            onChange={(e) => setMethodFilter(e.target.value)}
          >
            <MenuItem value="all">(Todos)</MenuItem>
            {PAYMENT_METHODS.map((m) => (
              <MenuItem key={m} value={m}>
                {m}
              </MenuItem>
            ))}
          </TextField>
          <Button
            variant="text"
            onClick={() => {
              setPartyFilter(null);
              setFromFilter('');
              setToFilter('');
              setMethodFilter('all');
            }}
          >
            Quitar filtro
          </Button>
          <Button
            startIcon={<RefreshIcon />}
            onClick={() => {
              void paymentsQuery.refetch();
            }}
            disabled={paymentsQuery.isFetching}
            variant="outlined"
          >
            Refrescar
          </Button>
        </Stack>
      </Stack>

      <Grid container spacing={3}>
        <Grid item xs={12} lg={5}>
          <Stack spacing={3}>
            <PaymentForm
              onCreated={() => {
                void paymentsQuery.refetch();
              }}
              defaultParty={partyFilter}
              payments={payments}
            />
            <SessionInvoiceGeneratorCard />
          </Stack>
        </Grid>
        <Grid item xs={12} lg={7}>
          <Paper variant="outlined" sx={{ p: 2 }}>
            <Stack direction="row" justifyContent="space-between" alignItems="center" sx={{ mb: 1 }}>
              <Box>
                <Typography variant="h6">Pagos registrados</Typography>
                <Typography variant="body2" color="text.secondary">
                  {partyFilter
                    ? `Filtrando por ${partyFilter.displayName} · ID ${partyFilter.partyId}`
                    : 'Ultimos 200 pagos'}
                </Typography>
              </Box>
              {paymentsQuery.isFetching && <Typography variant="body2">Cargando...</Typography>}
            </Stack>
            {filteredPayments.length === 0 ? (
              <EmptyState
                icon={<ReceiptIcon />}
                title="Sin pagos"
                description="No hay pagos registrados con este filtro."
              />
            ) : (
              <LazyPaginatedList
                items={filteredPayments}
                pagination={{ itemLabel: 'pagos', initialRowsPerPage: 25, resetKey: paymentPaginationResetKey }}
                renderItems={(visiblePayments) => (
                  <Table size="small">
                    <TableHead>
                      <TableRow>
                        <TableCell>ID</TableCell>
                        <TableCell>Contacto</TableCell>
                        <TableCell>Fecha</TableCell>
                        <TableCell>Periodo</TableCell>
                        <TableCell>Monto</TableCell>
                        <TableCell>Metodo</TableCell>
                        <TableCell>Referencia</TableCell>
                        <TableCell>Comprobante</TableCell>
                      </TableRow>
                    </TableHead>
                    <TableBody>
                      {visiblePayments.map((pay) => (
                          <TableRow key={pay.payId} hover>
                            <TableCell>{pay.payId}</TableCell>
                            <TableCell>
                              <Typography variant="body2" fontWeight={600}>
                                {pay.payPartyDisplayName}
                              </Typography>
                            </TableCell>
                            <TableCell>{pay.payPaidAt.split(' ')[0]}</TableCell>
                            <TableCell>{pay.payPeriod ?? '-'}</TableCell>
                            <TableCell>{formatMoney(pay.payAmountCents / 100, pay.payCurrency)}</TableCell>
                            <TableCell>{pay.payMethod}</TableCell>
                            <TableCell>{pay.payReference ?? '-'}</TableCell>
                            <TableCell>
                              {pay.payAttachment ? (
                                <Button
                                  size="small"
                                  startIcon={<DescriptionIcon fontSize="small" />}
                                  component="a"
                                  href={pay.payAttachment}
                                  target="_blank"
                                  rel="noreferrer"
                                >
                                  Ver
                                </Button>
                              ) : (
                                <Typography variant="body2" color="text.secondary">
                                  —
                                </Typography>
                              )}
                            </TableCell>
                          </TableRow>
                      ))}
                    </TableBody>
                  </Table>
                )}
              />
            )}
            {paymentsWithAttachments.length > 0 && (
              <>
                <Divider sx={{ my: 2 }} />
                <Stack gap={1}>
                  <Typography variant="subtitle1">Adjuntos</Typography>
                  <LazyPaginatedList
                    items={paymentsWithAttachments}
                    pagination={{ itemLabel: 'adjuntos', initialRowsPerPage: 10, resetKey: paymentPaginationResetKey }}
                    renderItems={(visiblePaymentsWithAttachments) => (
                      <Stack gap={1}>
                        {visiblePaymentsWithAttachments.map((p) => (
                          <Box key={p.payId}>
                            <Typography variant="body2">
                              #{p.payId} - {p.payPeriod ?? p.payPaidAt} -{' '}
                              <a href={p.payAttachment ?? '#'} target="_blank" rel="noreferrer">
                                {p.payAttachment}
                              </a>
                            </Typography>
                          </Box>
                        ))}
                      </Stack>
                    )}
                  />
                </Stack>
              </>
            )}
          </Paper>
        </Grid>
      </Grid>
    </Box>
  );
}
