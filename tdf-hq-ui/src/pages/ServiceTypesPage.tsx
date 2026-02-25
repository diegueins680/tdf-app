import { useEffect, useMemo, useRef, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Grid,
  IconButton,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  TextField,
  Typography,
  MenuItem,
  FormControlLabel,
  Checkbox,
  Chip,
  CircularProgress,
  Autocomplete,
} from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import AddIcon from '@mui/icons-material/Add';
import EditIcon from '@mui/icons-material/Edit';
import DeleteIcon from '@mui/icons-material/Delete';
import { Services } from '../api/services';
import type {
  PricingModel,
  ServiceCatalogCreate,
  ServiceCatalogDTO,
  ServiceCatalogUpdate,
  ServiceKind,
} from '../api/types';
import { mergeServiceTypes, type ServiceType } from '../utils/serviceTypesStore';

interface FormState {
  id?: string;
  name: string;
  price: string;
  currency: string;
  billingUnit: string;
  kind: ServiceKind;
  pricingModel: PricingModel;
  taxBps: string;
  active: boolean;
}

const SERVICE_KIND_OPTIONS: readonly ServiceKind[] = ['Recording', 'Mixing', 'Mastering', 'Rehearsal', 'Classes', 'EventProduction'];
const PRICING_MODEL_OPTIONS: readonly PricingModel[] = ['Hourly', 'PerSong', 'Package', 'Quote', 'Retainer'];
const SERVICE_QUERY_KEY = ['service-catalog', 'admin'];

const isServiceKind = (value: string): value is ServiceKind =>
  SERVICE_KIND_OPTIONS.some((kind) => kind === value);

const isPricingModel = (value: string): value is PricingModel =>
  PRICING_MODEL_OPTIONS.some((model) => model === value);

const normalizeServiceKind = (value: string | null | undefined): ServiceKind => {
  const trimmed = value?.trim() ?? '';
  return isServiceKind(trimmed) ? trimmed : 'Recording';
};

const normalizePricingModel = (value: string | null | undefined): PricingModel => {
  const trimmed = value?.trim() ?? '';
  return isPricingModel(trimmed) ? trimmed : 'Hourly';
};

const formatPrice = (svc: ServiceType) => {
  if (svc.priceCents == null) return '—';
  const amount = svc.priceCents / 100;
  return `${svc.currency} ${amount.toLocaleString(undefined, { minimumFractionDigits: 0, maximumFractionDigits: 2 })}`;
};

export default function ServiceTypesPage() {
  const qc = useQueryClient();
  const servicesQuery = useQuery<ServiceCatalogDTO[]>({
    queryKey: SERVICE_QUERY_KEY,
    queryFn: () => Services.list(true),
    staleTime: 5 * 60 * 1000,
  });
  const items = useMemo<ServiceType[]>(
    () => mergeServiceTypes(servicesQuery.data, { includeInactive: true, sort: false }),
    [servicesQuery.data],
  );
  const [dialogOpen, setDialogOpen] = useState(false);
  const [form, setForm] = useState<FormState>({
    name: '',
    price: '',
    currency: 'USD',
    billingUnit: '',
    kind: 'Recording',
    pricingModel: 'Hourly',
    taxBps: '',
    active: true,
  });
  const [error, setError] = useState<string | null>(null);
  const [fieldErrors, setFieldErrors] = useState<{ price?: string; tax?: string }>({});
  const nameInputRef = useRef<HTMLInputElement | null>(null);

  useEffect(() => {
    if (dialogOpen) {
      setTimeout(() => nameInputRef.current?.focus(), 50);
    }
  }, [dialogOpen]);

  const createMutation = useMutation({
    mutationFn: (payload: ServiceCatalogCreate) => Services.create(payload),
    onSuccess: () => qc.invalidateQueries({ queryKey: SERVICE_QUERY_KEY }),
  });
  const updateMutation = useMutation({
    mutationFn: ({ id, payload }: { id: string; payload: ServiceCatalogUpdate }) => Services.update(id, payload),
    onSuccess: () => qc.invalidateQueries({ queryKey: SERVICE_QUERY_KEY }),
  });
  const deleteMutation = useMutation({
    mutationFn: (id: string) => Services.remove(id),
    onSuccess: () => qc.invalidateQueries({ queryKey: SERVICE_QUERY_KEY }),
  });

  const sortedItems = useMemo(
    () => [...items].sort((a, b) => a.name.localeCompare(b.name)),
    [items],
  );
  const currencyOptions = useMemo(
    () => Array.from(new Set(items.map((item) => item.currency).filter(Boolean))),
    [items],
  );
  const unitOptions = useMemo(
      () =>
        Array.from(
          new Set(
            items
              .map((item) => item.billingUnit)
              .filter((unit): unit is string => Boolean(unit?.trim())),
        ),
      ),
    [items],
  );

  const handleOpenNew = () => {
    setForm({
      name: '',
      price: '',
      currency: 'USD',
      billingUnit: '',
      kind: 'Recording',
      pricingModel: 'Hourly',
      taxBps: '',
      active: true,
    });
    setError(null);
    setDialogOpen(true);
  };

  const handleEdit = (item: ServiceType) => {
    setForm({
      id: item.id,
      name: item.name,
      price: item.priceCents != null ? String(item.priceCents / 100) : '',
      currency: item.currency,
      billingUnit: item.billingUnit ?? '',
      kind: normalizeServiceKind(item.kind),
      pricingModel: normalizePricingModel(item.pricingModel),
      taxBps: item.taxBps != null ? String(item.taxBps) : '',
      active: item.active,
    });
    setError(null);
    setDialogOpen(true);
  };

  const handleDelete = (id: string) => {
    if (!window.confirm('¿Desactivar este servicio del catálogo?')) return;
    deleteMutation.mutate(id, {
      onError: (err) =>
        setError(err instanceof Error ? err.message : 'No se pudo desactivar el servicio.'),
    });
  };

  const handleSubmit = async (evt: React.FormEvent) => {
    evt.preventDefault();
    setFieldErrors({});
    setError(null);
    const cleanName = form.name.trim();
    if (!cleanName) {
      setError('Agrega un nombre.');
      return;
    }
    const priceRaw = form.price.trim();
    const priceNumber = priceRaw === '' ? null : Number(priceRaw);
    const priceInvalid = priceNumber !== null && (Number.isNaN(priceNumber) || priceNumber < 0);
    if (priceInvalid) {
      setError('Corrige los campos resaltados.');
      setFieldErrors((prev) => ({ ...prev, price: 'Usa un valor numérico mayor o igual a 0 o deja vacío.' }));
      return;
    }
    const rateCents = priceNumber === null ? null : Math.round(priceNumber * 100);
    const taxRaw = form.taxBps.trim();
    const taxNumber = taxRaw === '' ? null : Number(taxRaw);
    const taxInvalid = taxNumber !== null && (Number.isNaN(taxNumber) || taxNumber < 0);
    if (taxInvalid) {
      setError('Corrige los campos resaltados.');
      setFieldErrors((prev) => ({ ...prev, tax: 'Ingresa puntos base numéricos o deja vacío si no aplica.' }));
      return;
    }
    const currency = form.currency.trim() || 'USD';
    const billingUnit = form.billingUnit.trim() || null;
    setError(null);
    try {
      if (form.id) {
        await updateMutation.mutateAsync({
          id: form.id,
          payload: {
            scuName: cleanName,
            scuKind: form.kind,
            scuPricingModel: form.pricingModel,
            scuRateCents: rateCents,
            scuCurrency: currency,
            scuBillingUnit: billingUnit,
            scuTaxBps: taxNumber,
            scuActive: form.active,
          },
        });
      } else {
        await createMutation.mutateAsync({
          sccName: cleanName,
          sccKind: form.kind,
          sccPricingModel: form.pricingModel,
          sccRateCents: rateCents,
          sccCurrency: currency,
          sccBillingUnit: billingUnit,
          sccTaxBps: taxNumber,
          sccActive: form.active,
        });
      }
      setDialogOpen(false);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'No se pudo guardar el servicio.');
    }
  };

  const isLoading = servicesQuery.isLoading;

  return (
    <Box sx={{ color: '#e2e8f0' }}>
      <Stack direction="row" justifyContent="space-between" alignItems="center" mb={2}>
        <Box>
          <Typography variant="h5" fontWeight={800}>
            Tipos de servicio
          </Typography>
          <Typography variant="body2" color="rgba(226,232,240,0.75)">
            Mantén el catálogo de servicios del estudio con precios. Se usa al crear sesiones en el calendario.
          </Typography>
        </Box>
        <Button variant="contained" startIcon={<AddIcon />} onClick={handleOpenNew} sx={{ textTransform: 'none' }}>
          Nuevo servicio
        </Button>
      </Stack>

      <Card sx={{ bgcolor: 'rgba(255,255,255,0.02)', border: '1px solid rgba(255,255,255,0.08)' }}>
        <CardContent>
          {servicesQuery.isError && (
            <Alert severity="error" sx={{ mb: 2 }}>
              No se pudo cargar el catálogo de servicios. Usando valores locales por ahora.
            </Alert>
          )}
          {sortedItems.length === 0 ? (
            <Alert severity="info">Aún no tienes servicios. Crea el primero.</Alert>
          ) : (
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Servicio</TableCell>
                  <TableCell>Precio</TableCell>
                  <TableCell>Unidad</TableCell>
                  <TableCell>Tipo</TableCell>
                  <TableCell>Modelo</TableCell>
                  <TableCell>Estado</TableCell>
                  <TableCell align="right">Acciones</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {sortedItems.map((item) => (
                  <TableRow key={item.id} hover>
                    <TableCell>{item.name}</TableCell>
                    <TableCell>{formatPrice(item)}</TableCell>
                    <TableCell>{item.billingUnit ?? '—'}</TableCell>
                    <TableCell>{item.kind ?? '—'}</TableCell>
                    <TableCell>{item.pricingModel ?? '—'}</TableCell>
                    <TableCell>
                      <Chip
                        size="small"
                        label={item.active ? 'Activo' : 'Inactivo'}
                        color={item.active ? 'success' : 'default'}
                        variant={item.active ? 'filled' : 'outlined'}
                      />
                    </TableCell>
                    <TableCell align="right">
                      <IconButton size="small" onClick={() => handleEdit(item)}>
                        <EditIcon fontSize="small" />
                      </IconButton>
                      <IconButton size="small" onClick={() => handleDelete(item.id)}>
                        <DeleteIcon fontSize="small" />
                      </IconButton>
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          )}
          {isLoading && (
            <Stack direction="row" spacing={1} alignItems="center" sx={{ mt: 2 }}>
              <CircularProgress size={18} />
              <Typography variant="body2" color="text.secondary">
                Cargando servicios...
              </Typography>
            </Stack>
          )}
        </CardContent>
      </Card>

      <Dialog open={dialogOpen} onClose={() => setDialogOpen(false)} maxWidth="sm" fullWidth>
        <form
          onSubmit={(evt) => {
            void handleSubmit(evt);
          }}
        >
          <DialogTitle>{form.id ? 'Editar servicio' : 'Nuevo servicio'}</DialogTitle>
          <DialogContent dividers>
            <Grid container spacing={2}>
              <Grid item xs={12}>
                <TextField
                  label="Nombre"
                  value={form.name}
                  onChange={(e) => setForm((prev) => ({ ...prev, name: e.target.value }))}
                  fullWidth
                  required
                  inputRef={nameInputRef}
                />
              </Grid>
              <Grid item xs={6}>
                <TextField
                  label="Precio"
                  type="number"
                  value={form.price}
                  inputProps={{ inputMode: 'decimal', min: 0 }}
                  onChange={(e) => {
                    const next = e.target.value;
                    setFieldErrors((prev) => ({ ...prev, price: undefined }));
                    setForm((prev) => ({ ...prev, price: next }));
                  }}
                  fullWidth
                  error={Boolean(fieldErrors.price)}
                  helperText={fieldErrors.price ?? 'Monto base. Deja vacío si es a cotizar.'}
                />
              </Grid>
              <Grid item xs={6}>
                <Autocomplete
                  freeSolo
                  options={currencyOptions}
                  value={form.currency}
                  onChange={(_evt, value) => setForm((prev) => ({ ...prev, currency: value ?? prev.currency }))}
                  onInputChange={(_evt, value) => setForm((prev) => ({ ...prev, currency: value }))}
                  renderInput={(params) => (
                    <TextField
                      {...params}
                      label="Moneda"
                      required
                      fullWidth
                      helperText="Código ISO ej. USD, EUR, COP"
                      inputProps={{ ...params.inputProps, inputMode: 'text' }}
                    />
                  )}
                />
              </Grid>
              <Grid item xs={12}>
                <Autocomplete
                  freeSolo
                  options={unitOptions}
                  value={form.billingUnit}
                  onChange={(_evt, value) => setForm((prev) => ({ ...prev, billingUnit: value ?? '' }))}
                  onInputChange={(_evt, value) => setForm((prev) => ({ ...prev, billingUnit: value }))}
                  renderInput={(params) => (
                    <TextField {...params} label="Unidad (hora, canción, episodio...)" fullWidth />
                  )}
                />
              </Grid>
              <Grid item xs={6}>
                <TextField
                  select
                  label="Tipo"
                  value={form.kind}
                  onChange={(e) => setForm((prev) => ({ ...prev, kind: normalizeServiceKind(e.target.value) }))}
                  fullWidth
                >
                  {SERVICE_KIND_OPTIONS.map((kind) => (
                    <MenuItem key={kind} value={kind}>
                      {kind}
                    </MenuItem>
                  ))}
                </TextField>
              </Grid>
              <Grid item xs={6}>
                <TextField
                  select
                  label="Modelo de cobro"
                  value={form.pricingModel}
                  onChange={(e) =>
                    setForm((prev) => ({ ...prev, pricingModel: normalizePricingModel(e.target.value) }))
                  }
                  fullWidth
                >
                  {PRICING_MODEL_OPTIONS.map((model) => (
                    <MenuItem key={model} value={model}>
                      {model}
                    </MenuItem>
                  ))}
                </TextField>
              </Grid>
              <Grid item xs={6}>
                <TextField
                  label="Impuesto (bps)"
                  type="number"
                  value={form.taxBps}
                  inputProps={{ inputMode: 'decimal', min: 0 }}
                  onChange={(e) => {
                    const next = e.target.value;
                    setFieldErrors((prev) => ({ ...prev, tax: undefined }));
                    setForm((prev) => ({ ...prev, taxBps: next }));
                  }}
                  fullWidth
                  error={Boolean(fieldErrors.tax)}
                  helperText={fieldErrors.tax ?? 'Base points ej. 1200 = 12% (deja vacío si no aplica)'}
                />
              </Grid>
              <Grid item xs={6} sx={{ display: 'flex', alignItems: 'center' }}>
                <FormControlLabel
                  control={
                    <Checkbox
                      checked={form.active}
                      onChange={(e) => setForm((prev) => ({ ...prev, active: e.target.checked }))}
                    />
                  }
                  label="Activo en cat&aacute;logo"
                />
              </Grid>
              {error && (
                <Grid item xs={12}>
                  <Alert severity="error">{error}</Alert>
                </Grid>
              )}
            </Grid>
          </DialogContent>
          <DialogActions>
            <Button onClick={() => setDialogOpen(false)}>Cancelar</Button>
            <Button type="submit" variant="contained" disabled={createMutation.isPending || updateMutation.isPending}>
              {createMutation.isPending || updateMutation.isPending ? 'Guardando...' : 'Guardar'}
            </Button>
          </DialogActions>
        </form>
      </Dialog>
    </Box>
  );
}
