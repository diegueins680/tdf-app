import { useEffect, useMemo, useState } from 'react';
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
} from '@mui/material';
import AddCircleIcon from '@mui/icons-material/AddCircle';
import RefreshIcon from '@mui/icons-material/Refresh';
import DescriptionIcon from '@mui/icons-material/Description';
import { createFilterOptions } from '@mui/material/Autocomplete';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import type { PaymentCreate, PaymentDTO, PartyDTO } from '../api/types';
import { Payments } from '../api/payments';
import { Parties } from '../api/parties';
import GoogleDriveUploadWidget from '../components/GoogleDriveUploadWidget';
import type { DriveFileInfo } from '../services/googleDrive';
import { useSnackbar } from 'notistack';

const PAYMENT_METHODS = ['Produbanco', 'Bank', 'Cash', 'Card', 'Crypto', 'Other'] as const;
const CURRENCY_OPTIONS = ['USD', 'EUR', 'COP'];
const CONCEPT_PRESETS = ['Honorarios', 'Adelanto', 'Licencia', 'Reembolso', 'Otros'];

const partyFilterOptions = createFilterOptions<PartyDTO>({
  stringify: (option) =>
    [
      option.displayName,
      option.primaryEmail,
      option.primaryPhone,
      option.instagram,
      option.partyId,
    ]
      .filter(Boolean)
      .join(' ')
      .toLowerCase(),
});

const toPeriod = (isoDate: string) => {
  const date = new Date(isoDate);
  if (Number.isNaN(date.getTime())) return '';
  const month = date.toLocaleString('en-US', { month: 'short' }).toUpperCase();
  return `${month}-${date.getFullYear()}`;
};

const formatAmount = (cents: number, currency: string) =>
  new Intl.NumberFormat('en-US', { style: 'currency', currency }).format(cents / 100);

function PaymentForm({
  onCreated,
  parties,
  defaultParty,
  payments,
}: {
  onCreated: () => void;
  parties: PartyDTO[];
  defaultParty?: PartyDTO | null;
  payments: PaymentDTO[];
}) {
  const { enqueueSnackbar } = useSnackbar();
  const qc = useQueryClient();
  const [selectedParty, setSelectedParty] = useState<PartyDTO | null>(defaultParty ?? null);
  const [partyInput, setPartyInput] = useState<string>('');
  const [paidAt, setPaidAt] = useState<string>(new Date().toISOString().slice(0, 10));
  const [amount, setAmount] = useState<string>('');
  const [currency, setCurrency] = useState<string>('USD');
  const [method, setMethod] = useState<string>('Produbanco');
  const [reference, setReference] = useState<string>('N/A');
  const [concept, setConcept] = useState<string>('Honorarios');
  const [period, setPeriod] = useState<string>(toPeriod(new Date().toISOString()));
  const [attachmentUrl, setAttachmentUrl] = useState<string>('');
  const [attachmentName, setAttachmentName] = useState<string>('');
  const [invoiceId, setInvoiceId] = useState<string>('');
  const [orderId, setOrderId] = useState<string>('');
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

  useEffect(() => {
    setSelectedParty(defaultParty ?? null);
  }, [defaultParty]);

  useEffect(() => {
    setPeriod(toPeriod(paidAt));
  }, [paidAt]);

  useEffect(() => {
    if (selectedParty && lastPaymentForParty) {
      setConcept(lastPaymentForParty.payConcept ?? concept);
      setMethod(lastPaymentForParty.payMethod);
      setCurrency(lastPaymentForParty.payCurrency);
      setReference(lastPaymentForParty.payReference ?? 'N/A');
      setPeriod(lastPaymentForParty.payPeriod ?? period);
    }
  }, [selectedParty, lastPaymentForParty]);

  const mutation = useMutation<PaymentDTO, Error, PaymentCreate>({
    mutationFn: (body) => Payments.create(body),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['payments'] });
      setAmount('');
      setReference('N/A');
      setAttachmentUrl('');
      setAttachmentName('');
      onCreated();
      enqueueSnackbar('Pago registrado', { variant: 'success' });
    },
    onError: (err) => setError(err.message),
  });

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
    setError(null);
    setFieldHints({});
    const payload: PaymentCreate = {
      pcPartyId: parsedPartyId,
      pcOrderId: orderId.trim() ? Number(orderId) : null,
      pcInvoiceId: invoiceId.trim() ? Number(invoiceId) : null,
      pcAmountCents: Math.round(normalizedAmount * 100),
      pcCurrency: currency.trim() || 'USD',
      pcMethod: method,
      pcReference: reference.trim() || null,
      pcPaidAt: paidAt,
      pcConcept: concept.trim() || 'Honorarios',
      pcPeriod: period.trim() || null,
      pcAttachmentUrl: attachmentUrl.trim() || null,
    };
    mutation.mutate(payload);
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
              <Autocomplete
                options={parties}
                value={selectedParty}
                onChange={(_, value) => {
                  setSelectedParty(value);
                }}
                inputValue={partyInput}
                onInputChange={(_, value) => setPartyInput(value)}
                filterOptions={partyFilterOptions}
                getOptionLabel={(option) => `${option.displayName} · ID ${option.partyId}${option.primaryEmail ? ` · ${option.primaryEmail}` : ''}`}
                isOptionEqualToValue={(option, value) => option.partyId === value.partyId}
              noOptionsText="Sin coincidencias. Revisa Contactos."
              renderInput={(params) => (
                <TextField
                  {...params}
                  label="Contacto"
                  required
                  helperText={fieldHints.party ?? 'Busca por nombre, email o ID.'}
                  error={Boolean(fieldHints.party)}
                />
              )}
            />
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
              {CURRENCY_OPTIONS.map((code) => (
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
              <GoogleDriveUploadWidget
                label={attachmentName ? `Adjunto: ${attachmentName}` : 'Subir comprobante (PDF/imagen) a Drive'}
                helperText="Se almacenará en Drive y guardaremos el enlace público."
                accept="application/pdf,image/*"
                multiple={false}
                dense
                onComplete={(files: DriveFileInfo[]) => {
                  const file = files[0];
                  if (!file) return;
                  setAttachmentUrl(file.webViewLink || file.webContentLink || '');
                  setAttachmentName(file.name);
                  setError(null);
                }}
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
            Último pago de este contacto: {formatAmount(lastPaymentForParty.payAmountCents, lastPaymentForParty.payCurrency)} · {lastPaymentForParty.payMethod}.{' '}
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
      </CardContent>
    </Card>
  );
}

export default function PaymentsPage() {
  const [partyFilter, setPartyFilter] = useState<PartyDTO | null>(null);
  const [partyFilterInput, setPartyFilterInput] = useState<string>('');
  const [fromFilter, setFromFilter] = useState<string>('');
  const [toFilter, setToFilter] = useState<string>('');
  const [methodFilter, setMethodFilter] = useState<string>('all');

  const paymentsQuery = useQuery<PaymentDTO[]>({
    queryKey: ['payments', partyFilter?.partyId ?? 'all'],
    queryFn: () => Payments.list(partyFilter?.partyId),
  });

  const partiesQuery = useQuery<PartyDTO[]>({
    queryKey: ['parties', 'all'],
    queryFn: () => Parties.list(),
  });

  const parties = useMemo<PartyDTO[]>(() => partiesQuery.data ?? [], [partiesQuery.data]);

  const partyLookup = useMemo(() => {
    const map = new Map<number, PartyDTO>();
    parties.forEach((party) => map.set(party.partyId, party));
    return map;
  }, [parties]);

  const payments = paymentsQuery.data ?? [];

  return (
    <Box>
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" gap={2} sx={{ mb: 3 }}>
        <Box>
          <Typography variant="h4" gutterBottom>
            Pagos manuales
          </Typography>
          <Typography color="text.secondary">
            Carga pagos de honorarios directamente al CRM. Filtra por contacto para ver su historial.
          </Typography>
        </Box>
        <Stack direction="row" gap={1} flexWrap="wrap" alignItems="center">
          <Autocomplete
            sx={{ minWidth: 280 }}
            size="small"
            options={parties}
            loading={partiesQuery.isFetching}
            value={partyFilter}
            onChange={(_, value) => setPartyFilter(value)}
            inputValue={partyFilterInput}
            onInputChange={(_, value) => setPartyFilterInput(value)}
            filterOptions={partyFilterOptions}
            getOptionLabel={(option) => `${option.displayName} · ID ${option.partyId}${option.primaryEmail ? ` · ${option.primaryEmail}` : ''}`}
            isOptionEqualToValue={(option, value) => option.partyId === value.partyId}
            noOptionsText="Sin coincidencias"
            renderInput={(params) => (
              <TextField
                {...params}
                label="Filtrar por contacto"
                placeholder="Nombre, correo o ID"
              />
            )}
          />
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
              setPartyFilterInput('');
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
          <PaymentForm
            onCreated={() => {
              void paymentsQuery.refetch();
            }}
            parties={parties}
            defaultParty={partyFilter}
            payments={payments}
          />
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
            {payments.length === 0 ? (
              <Alert severity="info">No hay pagos registrados con este filtro.</Alert>
            ) : (
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
                  {payments
                    .filter((pay) => {
                      if (fromFilter && pay.payPaidAt < fromFilter) return false;
                      if (toFilter && pay.payPaidAt > toFilter) return false;
                      if (methodFilter !== 'all' && pay.payMethod !== methodFilter) return false;
                      return true;
                    })
                    .map((pay) => {
                    const contact = partyLookup.get(pay.payPartyId);
                    return (
                      <TableRow key={pay.payId} hover>
                        <TableCell>{pay.payId}</TableCell>
                        <TableCell>
                          <Typography variant="body2" fontWeight={600}>
                            {contact?.displayName ?? 'Contacto desconocido'}
                          </Typography>
                          <Typography variant="body2" color="text.secondary">
                            ID {pay.payPartyId}
                            {contact?.primaryEmail ? ` · ${contact.primaryEmail}` : ''}
                          </Typography>
                        </TableCell>
                        <TableCell>{pay.payPaidAt.split(' ')[0]}</TableCell>
                        <TableCell>{pay.payPeriod ?? '-'}</TableCell>
                        <TableCell>{formatAmount(pay.payAmountCents, pay.payCurrency)}</TableCell>
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
                    );
                  })}
                </TableBody>
              </Table>
            )}
            {payments.some((p) => p.payAttachment) && (
              <>
                <Divider sx={{ my: 2 }} />
                <Stack gap={1}>
                  <Typography variant="subtitle1">Adjuntos</Typography>
                  {payments
                    .filter((p) => p.payAttachment)
                    .map((p) => (
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
              </>
            )}
          </Paper>
        </Grid>
      </Grid>
    </Box>
  );
}
