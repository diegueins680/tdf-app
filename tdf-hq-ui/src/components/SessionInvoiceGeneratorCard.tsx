import { useEffect, useMemo, useState } from 'react';
import AddCircleIcon from '@mui/icons-material/AddCircle';
import DescriptionIcon from '@mui/icons-material/Description';
import RefreshIcon from '@mui/icons-material/Refresh';
import RemoveCircleOutlineIcon from '@mui/icons-material/RemoveCircleOutline';
import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Card,
  CardContent,
  Checkbox,
  Chip,
  Divider,
  Grid,
  MenuItem,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  TextField,
  Typography,
} from '@mui/material';
import { createFilterOptions } from '@mui/material/Autocomplete';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Invoices, type GenerateSessionInvoiceInput, type GenerateSessionInvoiceResponse, type InvoiceDTO } from '../api/invoices';
import { Sessions, type SessionDTO } from '../api/sessions';
import type { PartyDTO } from '../api/types';

interface SessionInvoiceGeneratorCardProps {
  parties: PartyDTO[];
}

interface InvoiceLineDraft {
  id: string;
  description: string;
  quantity: string;
  unitAmount: string;
  taxBps: string;
  sriCode: string;
  sriAuxiliaryCode: string;
  sriAdditionalInfo: string;
  sriIvaCode: string;
}

const TAX_RATE_OPTIONS = [
  { value: '0', label: '0% (IVA 0)' },
  { value: '500', label: '5% (IVA 5)' },
  { value: '1500', label: '15% (IVA 15)' },
] as const;

const sessionFilterOptions = createFilterOptions<SessionDTO>({
  stringify: (option) =>
    [
      option.sessionId,
      option.sService,
      option.sStatus,
      option.sBookingRef,
      option.sClientPartyRef,
      option.sEngineerRef,
    ]
      .filter(Boolean)
      .join(' ')
      .toLowerCase(),
});

const partyFilterOptions = createFilterOptions<PartyDTO>({
  stringify: (option) =>
    [
      option.displayName,
      option.primaryEmail,
      option.primaryPhone,
      option.instagram,
      option.partyId,
      option.taxId,
    ]
      .filter(Boolean)
      .join(' ')
      .toLowerCase(),
});

const makeLineDraft = (description = ''): InvoiceLineDraft => ({
  id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
  description,
  quantity: '1',
  unitAmount: '',
  taxBps: '0',
  sriCode: '',
  sriAuxiliaryCode: '',
  sriAdditionalInfo: '',
  sriIvaCode: '',
});

const parsePositiveInteger = (raw: string | null | undefined): number | null => {
  const trimmed = raw?.trim() ?? '';
  if (!/^\d+$/.test(trimmed)) return null;
  const parsed = Number.parseInt(trimmed, 10);
  return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : null;
};

const formatAmount = (cents: number, currency: string) =>
  new Intl.NumberFormat('en-US', { style: 'currency', currency }).format(cents / 100);

const formatSessionDate = (iso: string) => {
  const parsed = new Date(iso);
  if (Number.isNaN(parsed.getTime())) return iso;
  return new Intl.DateTimeFormat('es-EC', {
    dateStyle: 'medium',
    timeStyle: 'short',
  }).format(parsed);
};

const sessionLabel = (session: SessionDTO, partyMap: Map<number, PartyDTO>) => {
  const partyId = parsePositiveInteger(session.sClientPartyRef);
  const partyName = partyId != null ? partyMap.get(partyId)?.displayName : null;
  const customer = partyName ?? session.sClientPartyRef ?? 'Sin cliente';
  return `${formatSessionDate(session.sStartAt)} · ${session.sService} · ${customer}`;
};

const toOptionalText = (value: string): string | undefined => {
  const trimmed = value.trim();
  return trimmed === '' ? undefined : trimmed;
};

const normalizeLineDraft = (line: InvoiceLineDraft): GenerateSessionInvoiceInput['lineItems'][number] => {
  const description = line.description.trim();
  if (!description) {
    throw new Error('Cada línea necesita una descripción.');
  }
  const quantity = Number.parseInt(line.quantity.trim(), 10);
  if (!Number.isSafeInteger(quantity) || quantity <= 0) {
    throw new Error(`Cantidad inválida para "${description}".`);
  }
  const unitAmount = Number.parseFloat(line.unitAmount.trim().replace(',', '.'));
  if (Number.isNaN(unitAmount) || unitAmount < 0) {
    throw new Error(`Monto inválido para "${description}".`);
  }
  const taxBps = Number.parseInt(line.taxBps.trim(), 10);
  if (!Number.isSafeInteger(taxBps) || taxBps < 0) {
    throw new Error(`IVA inválido para "${description}".`);
  }
  return {
    description,
    quantity,
    unitCents: Math.round(unitAmount * 100),
    taxBps,
    sriCode: toOptionalText(line.sriCode),
    sriAuxiliaryCode: toOptionalText(line.sriAuxiliaryCode),
    sriAdditionalInfo: toOptionalText(line.sriAdditionalInfo),
    sriIvaCode: toOptionalText(line.sriIvaCode),
  };
};

const isSriError = (value: GenerateSessionInvoiceResponse['sri']): value is { ok: false; error: string } =>
  typeof value === 'object' && value != null && 'error' in value;

export default function SessionInvoiceGeneratorCard({ parties }: SessionInvoiceGeneratorCardProps) {
  const qc = useQueryClient();
  const [sessionInput, setSessionInput] = useState('');
  const [selectedSession, setSelectedSession] = useState<SessionDTO | null>(null);
  const [customerInput, setCustomerInput] = useState('');
  const [selectedCustomer, setSelectedCustomer] = useState<PartyDTO | null>(null);
  const [currency, setCurrency] = useState('USD');
  const [invoiceNumber, setInvoiceNumber] = useState('');
  const [notes, setNotes] = useState('');
  const [certificatePassword, setCertificatePassword] = useState('');
  const [generateReceipt, setGenerateReceipt] = useState(false);
  const [issueSri, setIssueSri] = useState(true);
  const [formError, setFormError] = useState<string | null>(null);
  const [result, setResult] = useState<GenerateSessionInvoiceResponse | null>(null);
  const [lines, setLines] = useState<InvoiceLineDraft[]>([makeLineDraft()]);

  const sessionsQuery = useQuery({
    queryKey: ['sessions', 'invoice-generator'],
    queryFn: () => Sessions.list({ page: 1, pageSize: 100 }),
    staleTime: 5 * 60 * 1000,
  });

  const sessionInvoicesQuery = useQuery<InvoiceDTO[]>({
    queryKey: ['session-invoices', selectedSession?.sessionId ?? 'none'],
    queryFn: () => Invoices.listBySession(selectedSession?.sessionId ?? ''),
    enabled: Boolean(selectedSession?.sessionId),
  });

  const partyMap = useMemo(() => {
    const next = new Map<number, PartyDTO>();
    parties.forEach((party) => next.set(party.partyId, party));
    return next;
  }, [parties]);

  const resolvedSessionCustomer = useMemo(() => {
    const partyId = parsePositiveInteger(selectedSession?.sClientPartyRef);
    return partyId != null ? partyMap.get(partyId) ?? null : null;
  }, [partyMap, selectedSession?.sClientPartyRef]);

  useEffect(() => {
    if (!selectedSession) return;
    setLines((current) => {
      if (current.length !== 1) return current;
      const first = current[0];
      if (!first) return current;
      if (first.description.trim() || first.unitAmount.trim()) return current;
      return [
        {
          ...first,
          description: selectedSession.sService,
          sriAdditionalInfo: selectedSession.sNotes ?? selectedSession.sService,
        },
      ];
    });
    setResult(null);
    setFormError(null);
  }, [selectedSession]);

  const generateMutation = useMutation<GenerateSessionInvoiceResponse, Error, GenerateSessionInvoiceInput>({
    mutationFn: async (payload) => {
      if (!selectedSession?.sessionId) {
        throw new Error('Selecciona una sesión.');
      }
      return Invoices.generateForSession(selectedSession.sessionId, payload);
    },
    onSuccess: async (response) => {
      setResult(response);
      setFormError(null);
      await qc.invalidateQueries({ queryKey: ['session-invoices', selectedSession?.sessionId ?? 'none'] });
    },
    onError: (error) => {
      setFormError(error.message);
    },
  });

  const handleGenerate = () => {
    if (!selectedSession) {
      setFormError('Selecciona una sesión antes de generar la factura.');
      return;
    }
    if (lines.length === 0) {
      setFormError('Agrega al menos una línea a la factura.');
      return;
    }
    const sessionCustomerId = parsePositiveInteger(selectedSession.sClientPartyRef);
    if (!selectedCustomer && sessionCustomerId == null) {
      setFormError('La sesión no tiene un cliente numérico. Selecciona el contacto facturable.');
      return;
    }

    try {
      const payload: GenerateSessionInvoiceInput = {
        customerId: selectedCustomer?.partyId ?? undefined,
        currency: toOptionalText(currency) ?? 'USD',
        number: toOptionalText(invoiceNumber),
        notes: toOptionalText(notes),
        lineItems: lines.map(normalizeLineDraft),
        generateReceipt,
        issueSri,
        certificatePassword: toOptionalText(certificatePassword),
      };
      setFormError(null);
      setResult(null);
      generateMutation.mutate(payload);
    } catch (error) {
      setFormError(error instanceof Error ? error.message : String(error));
    }
  };

  const totalPreview = useMemo(
    () =>
      lines.reduce((sum, line) => {
        const quantity = Number.parseInt(line.quantity.trim(), 10);
        const unitAmount = Number.parseFloat(line.unitAmount.trim().replace(',', '.'));
        if (!Number.isFinite(quantity) || quantity <= 0 || Number.isNaN(unitAmount) || unitAmount < 0) return sum;
        return sum + quantity * unitAmount;
      }, 0),
    [lines],
  );

  return (
    <Card variant="outlined">
      <CardContent>
        <Stack direction="row" alignItems="center" justifyContent="space-between" sx={{ mb: 2 }}>
          <Box>
            <Typography variant="h6">Facturar sesión</Typography>
            <Typography variant="body2" color="text.secondary">
              Genera la factura interna y, si corresponde, la emite en SRI usando OpenClaw.
            </Typography>
          </Box>
          <Button
            startIcon={<RefreshIcon />}
            variant="outlined"
            onClick={() => {
              void sessionsQuery.refetch();
              if (selectedSession?.sessionId) {
                void sessionInvoicesQuery.refetch();
              }
            }}
            disabled={sessionsQuery.isFetching || sessionInvoicesQuery.isFetching}
          >
            Refrescar
          </Button>
        </Stack>

        <Grid container spacing={2}>
          <Grid item xs={12}>
            <Autocomplete
              options={sessionsQuery.data?.items ?? []}
              loading={sessionsQuery.isFetching}
              value={selectedSession}
              onChange={(_, value) => setSelectedSession(value)}
              inputValue={sessionInput}
              onInputChange={(_, value) => setSessionInput(value)}
              filterOptions={sessionFilterOptions}
              getOptionLabel={(option) => sessionLabel(option, partyMap)}
              isOptionEqualToValue={(option, value) => option.sessionId === value.sessionId}
              renderInput={(params) => (
                <TextField
                  {...params}
                  label="Sesión"
                  placeholder="Busca por servicio, cliente, booking o UUID"
                  helperText="Se cargan las 100 sesiones más recientes."
                />
              )}
            />
          </Grid>

          {selectedSession && (
            <Grid item xs={12}>
              <Stack direction={{ xs: 'column', md: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                <Chip label={`UUID ${selectedSession.sessionId.slice(0, 8)}…`} />
                <Chip label={selectedSession.sStatus} color="info" variant="outlined" />
                <Chip label={selectedSession.sService} variant="outlined" />
                <Chip label={`Inicio ${formatSessionDate(selectedSession.sStartAt)}`} variant="outlined" />
                {resolvedSessionCustomer ? (
                  <Chip
                    label={`Cliente sesión: ${resolvedSessionCustomer.displayName} · ID ${resolvedSessionCustomer.partyId}`}
                    color="success"
                    variant="outlined"
                  />
                ) : selectedSession.sClientPartyRef ? (
                  <Chip label={`clientPartyRef: ${selectedSession.sClientPartyRef}`} variant="outlined" />
                ) : (
                  <Chip label="Sin cliente en sesión" color="warning" variant="outlined" />
                )}
              </Stack>
            </Grid>
          )}

          <Grid item xs={12} md={6}>
            <Autocomplete
              options={parties}
              value={selectedCustomer}
              onChange={(_, value) => setSelectedCustomer(value)}
              inputValue={customerInput}
              onInputChange={(_, value) => setCustomerInput(value)}
              filterOptions={partyFilterOptions}
              getOptionLabel={(option) =>
                `${option.displayName} · ID ${option.partyId}${option.taxId ? ` · ${option.taxId}` : ''}`
              }
              isOptionEqualToValue={(option, value) => option.partyId === value.partyId}
              renderInput={(params) => (
                <TextField
                  {...params}
                  label="Cliente facturable (opcional)"
                  helperText="Si lo dejas vacío, se usará el clientPartyRef de la sesión."
                />
              )}
            />
          </Grid>
          <Grid item xs={12} md={3}>
            <TextField
              label="Moneda"
              fullWidth
              value={currency}
              onChange={(event) => setCurrency(event.target.value.toUpperCase())}
            />
          </Grid>
          <Grid item xs={12} md={3}>
            <TextField
              label="Número interno (opcional)"
              fullWidth
              value={invoiceNumber}
              onChange={(event) => setInvoiceNumber(event.target.value)}
            />
          </Grid>
          <Grid item xs={12}>
            <TextField
              label="Notas internas"
              fullWidth
              multiline
              minRows={2}
              value={notes}
              onChange={(event) => setNotes(event.target.value)}
            />
          </Grid>
        </Grid>

        <Divider sx={{ my: 2 }} />

        <Stack spacing={2}>
          <Stack direction="row" alignItems="center" justifyContent="space-between">
            <Box>
              <Typography variant="subtitle1">Líneas de factura</Typography>
              <Typography variant="body2" color="text.secondary">
                Cada línea se transforma en producto si no existe en el SRI.
              </Typography>
            </Box>
            <Button
              startIcon={<AddCircleIcon />}
              onClick={() => setLines((current) => [...current, makeLineDraft(selectedSession?.sService ?? '')])}
            >
              Agregar línea
            </Button>
          </Stack>

          {lines.map((line, index) => (
            <Box
              key={line.id}
              sx={{ border: (theme) => `1px solid ${theme.palette.divider}`, borderRadius: 2, p: 2 }}
            >
              <Stack direction="row" alignItems="center" justifyContent="space-between" sx={{ mb: 1.5 }}>
                <Typography variant="subtitle2">Línea #{index + 1}</Typography>
                <Button
                  color="inherit"
                  startIcon={<RemoveCircleOutlineIcon />}
                  onClick={() => setLines((current) => current.filter((item) => item.id !== line.id))}
                  disabled={lines.length === 1}
                >
                  Quitar
                </Button>
              </Stack>
              <Grid container spacing={2}>
                <Grid item xs={12} md={6}>
                  <TextField
                    label="Descripción"
                    fullWidth
                    value={line.description}
                    onChange={(event) =>
                      setLines((current) =>
                        current.map((item) =>
                          item.id === line.id ? { ...item, description: event.target.value } : item,
                        ),
                      )
                    }
                  />
                </Grid>
                <Grid item xs={12} md={2}>
                  <TextField
                    label="Cantidad"
                    fullWidth
                    value={line.quantity}
                    onChange={(event) =>
                      setLines((current) =>
                        current.map((item) =>
                          item.id === line.id ? { ...item, quantity: event.target.value } : item,
                        ),
                      )
                    }
                  />
                </Grid>
                <Grid item xs={12} md={2}>
                  <TextField
                    label="Unitario USD"
                    fullWidth
                    value={line.unitAmount}
                    onChange={(event) =>
                      setLines((current) =>
                        current.map((item) =>
                          item.id === line.id ? { ...item, unitAmount: event.target.value } : item,
                        ),
                      )
                    }
                    placeholder="75.00"
                  />
                </Grid>
                <Grid item xs={12} md={2}>
                  <TextField
                    label="IVA"
                    select
                    fullWidth
                    value={line.taxBps}
                    onChange={(event) =>
                      setLines((current) =>
                        current.map((item) =>
                          item.id === line.id ? { ...item, taxBps: event.target.value } : item,
                        ),
                      )
                    }
                  >
                    {TAX_RATE_OPTIONS.map((option) => (
                      <MenuItem key={option.value} value={option.value}>
                        {option.label}
                      </MenuItem>
                    ))}
                  </TextField>
                </Grid>
                <Grid item xs={12} md={3}>
                  <TextField
                    label="Código SRI"
                    fullWidth
                    value={line.sriCode}
                    onChange={(event) =>
                      setLines((current) =>
                        current.map((item) =>
                          item.id === line.id ? { ...item, sriCode: event.target.value } : item,
                        ),
                      )
                    }
                  />
                </Grid>
                <Grid item xs={12} md={3}>
                  <TextField
                    label="Código auxiliar"
                    fullWidth
                    value={line.sriAuxiliaryCode}
                    onChange={(event) =>
                      setLines((current) =>
                        current.map((item) =>
                          item.id === line.id ? { ...item, sriAuxiliaryCode: event.target.value } : item,
                        ),
                      )
                    }
                  />
                </Grid>
                <Grid item xs={12} md={3}>
                  <TextField
                    label="Info adicional SRI"
                    fullWidth
                    value={line.sriAdditionalInfo}
                    onChange={(event) =>
                      setLines((current) =>
                        current.map((item) =>
                          item.id === line.id ? { ...item, sriAdditionalInfo: event.target.value } : item,
                        ),
                      )
                    }
                  />
                </Grid>
                <Grid item xs={12} md={3}>
                  <TextField
                    label="Código IVA SRI (opcional)"
                    fullWidth
                    value={line.sriIvaCode}
                    onChange={(event) =>
                      setLines((current) =>
                        current.map((item) =>
                          item.id === line.id ? { ...item, sriIvaCode: event.target.value } : item,
                        ),
                      )
                    }
                    placeholder="0, 5, 4"
                  />
                </Grid>
              </Grid>
            </Box>
          ))}
        </Stack>

        <Divider sx={{ my: 2 }} />

        <Grid container spacing={2} sx={{ mb: 2 }}>
          <Grid item xs={12} md={4}>
            <TextField
              label="Clave certificado (opcional)"
              type="password"
              fullWidth
              value={certificatePassword}
              onChange={(event) => setCertificatePassword(event.target.value)}
              helperText="Si la dejas vacía y issueSri está activo, el backend se quedará en espera de firma."
            />
          </Grid>
          <Grid item xs={12} md={4}>
            <Stack sx={{ height: '100%' }} justifyContent="center">
              <Stack direction="row" alignItems="center" spacing={1}>
                <Checkbox
                  checked={generateReceipt}
                  onChange={(event) => setGenerateReceipt(event.target.checked)}
                />
                <Typography>Generar receipt interno</Typography>
              </Stack>
            </Stack>
          </Grid>
          <Grid item xs={12} md={4}>
            <Stack sx={{ height: '100%' }} justifyContent="center">
              <Stack direction="row" alignItems="center" spacing={1}>
                <Checkbox
                  checked={issueSri}
                  onChange={(event) => setIssueSri(event.target.checked)}
                />
                <Typography>Emitir en SRI</Typography>
              </Stack>
            </Stack>
          </Grid>
        </Grid>

        <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" alignItems={{ xs: 'flex-start', md: 'center' }} gap={2}>
          <Typography variant="subtitle1">
            Total estimado: {formatAmount(Math.round(totalPreview * 100), currency || 'USD')}
          </Typography>
          <Button
            variant="contained"
            startIcon={<DescriptionIcon />}
            onClick={handleGenerate}
            disabled={generateMutation.isPending}
          >
            Generar factura
          </Button>
        </Stack>

        {formError && (
          <Alert severity="error" sx={{ mt: 2 }}>
            {formError}
          </Alert>
        )}

        {result && (
          <Alert severity={isSriError(result.sri) ? 'warning' : 'success'} sx={{ mt: 2 }}>
            <Stack spacing={0.5}>
              <Typography variant="body2" fontWeight={700}>
                Factura #{result.invoice.invId}
                {result.invoice.number ? ` · ${result.invoice.number}` : ''}
              </Typography>
              <Typography variant="body2">
                Estado interno: {result.invoice.statusI} · Total: {formatAmount(result.invoice.totalC, result.invoice.currency)}
              </Typography>
              {isSriError(result.sri) ? (
                <Typography variant="body2">SRI: {result.sri.error}</Typography>
              ) : result.sri ? (
                <Typography variant="body2">
                  SRI: {result.sri.status}
                  {result.sri.invoiceNumber ? ` · Comprobante ${result.sri.invoiceNumber}` : ''}
                  {result.sri.authorizationNumber ? ` · Autorización ${result.sri.authorizationNumber}` : ''}
                </Typography>
              ) : (
                <Typography variant="body2">SRI no ejecutado.</Typography>
              )}
            </Stack>
          </Alert>
        )}

        <Divider sx={{ my: 2 }} />

        <Stack spacing={1}>
          <Typography variant="subtitle1">Facturas existentes de la sesión</Typography>
          {selectedSession == null ? (
            <Alert severity="info">Selecciona una sesión para ver sus facturas vinculadas.</Alert>
          ) : sessionInvoicesQuery.isLoading ? (
            <Typography variant="body2" color="text.secondary">
              Cargando facturas de la sesión…
            </Typography>
          ) : sessionInvoicesQuery.isError ? (
            <Alert severity="error">{sessionInvoicesQuery.error.message}</Alert>
          ) : (sessionInvoicesQuery.data ?? []).length === 0 ? (
            <Alert severity="info">Esta sesión todavía no tiene facturas vinculadas.</Alert>
          ) : (
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>ID</TableCell>
                  <TableCell>Número</TableCell>
                  <TableCell>Estado</TableCell>
                  <TableCell>Total</TableCell>
                  <TableCell>SRI</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {(sessionInvoicesQuery.data ?? []).map((invoice) => (
                  <TableRow key={invoice.invId} hover>
                    <TableCell>{invoice.invId}</TableCell>
                    <TableCell>{invoice.number ?? '—'}</TableCell>
                    <TableCell>{invoice.statusI}</TableCell>
                    <TableCell>{formatAmount(invoice.totalC, invoice.currency)}</TableCell>
                    <TableCell>{invoice.sriDocumentId ?? '—'}</TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          )}
        </Stack>
      </CardContent>
    </Card>
  );
}
