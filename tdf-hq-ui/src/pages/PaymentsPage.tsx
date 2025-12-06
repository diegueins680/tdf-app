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
import { createFilterOptions } from '@mui/material/Autocomplete';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import type { PaymentCreate, PaymentDTO, PartyDTO } from '../api/types';
import { Payments } from '../api/payments';
import { Parties } from '../api/parties';

const PAYMENT_METHODS = ['Produbanco', 'Bank', 'Cash', 'Crypto'] as const;

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
}: {
  onCreated: () => void;
  parties: PartyDTO[];
  defaultParty?: PartyDTO | null;
}) {
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
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    setSelectedParty(defaultParty ?? null);
  }, [defaultParty]);

  useEffect(() => {
    setPeriod(toPeriod(paidAt));
  }, [paidAt]);

  const mutation = useMutation<PaymentDTO, Error, PaymentCreate>({
    mutationFn: (body) => Payments.create(body),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['payments'] });
      setAmount('');
      setReference('N/A');
      setAttachmentUrl('');
      onCreated();
    },
    onError: (err) => setError(err.message),
  });

  const handleSubmit = () => {
    const parsedPartyId = selectedParty?.partyId;
    if (!parsedPartyId) {
      setError('Elige un contacto de la lista.');
      return;
    }
    const normalizedAmount = Number.parseFloat(amount.replace(',', '.'));
    if (Number.isNaN(normalizedAmount) || normalizedAmount <= 0) {
      setError('Ingresa un monto valido');
      return;
    }
    if (!paidAt) {
      setError('Selecciona una fecha de pago');
      return;
    }
    setError(null);
    const payload: PaymentCreate = {
      pcPartyId: parsedPartyId,
      pcOrderId: null,
      pcInvoiceId: null,
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
              onChange={(_, value) => setSelectedParty(value)}
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
                  helperText="Busca por nombre, email o ID."
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
              label="Monto (USD)"
              fullWidth
              value={amount}
              onChange={(e) => setAmount(e.target.value)}
              placeholder="Ej. 399.00"
              required
              InputProps={{ startAdornment: <Box sx={{ mr: 1 }}>$</Box> }}
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
              <MenuItem value="USD">USD</MenuItem>
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
            <TextField
              label="Concepto"
              fullWidth
              value={concept}
              onChange={(e) => setConcept(e.target.value)}
              placeholder="Honorarios, adelanto, etc."
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
          <Grid item xs={12}>
            <TextField
              label="URL de respaldo (opcional)"
              fullWidth
              value={attachmentUrl}
              onChange={(e) => setAttachmentUrl(e.target.value)}
              placeholder="Link a comprobante o carpeta"
            />
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
      </CardContent>
    </Card>
  );
}

export default function PaymentsPage() {
  const [partyFilter, setPartyFilter] = useState<PartyDTO | null>(null);
  const [partyFilterInput, setPartyFilterInput] = useState<string>('');

  const paymentsQuery = useQuery<PaymentDTO[]>({
    queryKey: ['payments', partyFilter?.partyId ?? 'all'],
    queryFn: () => Payments.list(partyFilter?.partyId),
  });

  const partiesQuery = useQuery<PartyDTO[]>({
    queryKey: ['parties', 'all'],
    queryFn: () => Parties.list(),
  });

  const parties = partiesQuery.data ?? [];

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
          <Button
            variant="text"
            onClick={() => {
              setPartyFilter(null);
              setPartyFilterInput('');
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
                  </TableRow>
                </TableHead>
                <TableBody>
                  {payments.map((pay) => {
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
