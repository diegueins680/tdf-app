import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Checkbox,
  Chip,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControl,
  FormControlLabel,
  InputLabel,
  MenuItem,
  Select,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  TextField,
  Typography,
} from '@mui/material';
import { Catalogs, type CatalogDraft, type CatalogItem, type CatalogRevision } from '../api/catalogs';
import { Rooms } from '../api/rooms';
import { Services } from '../api/services';
import type { RoomDTO, ServiceCatalogDTO } from '../api/types';
import { mergeServiceTypes, type ServiceType } from '../utils/serviceTypesStore';
import LazyPaginatedList from '../components/LazyPaginatedList';

const SERVICE_QUERY_KEY = ['service-catalog', 'admin'] as const;
const SERVICE_ITEMS_QUERY_KEY = ['catalog', 'services', 'admin-items'] as const;
const SERVICE_REVISIONS_QUERY_KEY = ['catalog', 'services', 'revisions'] as const;

type DefaultResourceDraft = NonNullable<CatalogDraft['serviceOffering']>['defaultResources'][number];

export interface OfferingForm {
  entityId?: string;
  baseVersion?: number;
  code: string;
  nameEs: string;
  nameEn: string;
  descriptionEs: string;
  descriptionEn: string;
  sortOrder: number;
  categoryId: string;
  pricingModelId: string;
  rateCents: string;
  currencyId: string;
  billingUnitEs: string;
  billingUnitEn: string;
  taxRateId: string;
  defaultDurationMinutes: string;
  requiresEngineer: boolean;
  resources: DefaultResourceDraft[];
  reason: string;
}

const emptyForm = (): OfferingForm => ({
  code: '',
  nameEs: '',
  nameEn: '',
  descriptionEs: '',
  descriptionEn: '',
  sortOrder: 0,
  categoryId: '',
  pricingModelId: '',
  rateCents: '',
  currencyId: '',
  billingUnitEs: '',
  billingUnitEn: '',
  taxRateId: '',
  defaultDurationMinutes: '',
  requiresEngineer: false,
  resources: [],
  reason: '',
});

const optionalText = (value: string): string | undefined => value.trim() || undefined;
const optionalInteger = (value: string): number | undefined => {
  if (value.trim() === '') return undefined;
  const parsed = Number(value);
  return Number.isSafeInteger(parsed) ? parsed : undefined;
};

const correlationId = (): string => {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return `service-offering:${crypto.randomUUID()}`;
  }
  return `service-offering:${Date.now()}`;
};

const formatPrice = (service: ServiceType): string => {
  if (service.priceCents == null) return 'A cotizar';
  const amount = service.priceCents / 100;
  const unit = service.billingUnit ? ` / ${service.billingUnit}` : '';
  return `${service.currency} ${amount.toLocaleString(undefined, {
    maximumFractionDigits: 2,
    minimumFractionDigits: 0,
  })}${unit}`;
};

export const buildServiceOfferingDraft = (form: OfferingForm): CatalogDraft => ({
  entityId: form.entityId,
  baseVersion: form.baseVersion,
  code: form.code.trim(),
  nameEs: form.nameEs.trim(),
  nameEn: form.nameEn.trim(),
  descriptionEs: optionalText(form.descriptionEs),
  descriptionEn: optionalText(form.descriptionEn),
  searchAliasesEs: [],
  searchAliasesEn: [],
  sortOrder: form.sortOrder,
  serviceOffering: {
    categoryId: form.categoryId,
    pricingModelId: form.pricingModelId,
    rateCents: optionalInteger(form.rateCents),
    currencyId: form.currencyId,
    billingUnitEs: optionalText(form.billingUnitEs),
    billingUnitEn: optionalText(form.billingUnitEn),
    taxRateId: optionalText(form.taxRateId),
    defaultDurationMinutes: optionalInteger(form.defaultDurationMinutes),
    requiresEngineer: form.requiresEngineer,
    defaultResources: form.resources,
  },
  reason: form.reason.trim(),
  sourcePlatform: 'web-admin',
  correlationId: correlationId(),
});

const revisionStateColor = (state: string): 'default' | 'info' | 'success' | 'error' | 'warning' => {
  switch (state) {
    case 'review': return 'info';
    case 'published': return 'success';
    case 'rejected': return 'error';
    case 'approved': return 'success';
    default: return 'warning';
  }
};

export default function ServiceTypesPage() {
  const queryClient = useQueryClient();
  const [dialogOpen, setDialogOpen] = useState(false);
  const [form, setForm] = useState<OfferingForm>(emptyForm);
  const [reviewNotes, setReviewNotes] = useState<Record<string, string>>({});

  const servicesQuery = useQuery<ServiceCatalogDTO[]>({
    queryKey: SERVICE_QUERY_KEY,
    queryFn: () => Services.list(true),
    staleTime: 5 * 60 * 1000,
  });
  const serviceItemsQuery = useQuery({
    queryKey: SERVICE_ITEMS_QUERY_KEY,
    queryFn: () => Catalogs.listItems('services', { includeInactive: true, pageSize: 200 }),
  });
  const categoriesQuery = useQuery({
    queryKey: ['catalog', 'service-categories', 'options'],
    queryFn: () => Catalogs.listItems('service-categories', { pageSize: 200 }),
  });
  const pricingModelsQuery = useQuery({
    queryKey: ['catalog', 'service-pricing-models', 'options'],
    queryFn: () => Catalogs.listItems('service-pricing-models', { pageSize: 200 }),
  });
  const resourceSelectionModesQuery = useQuery({
    queryKey: ['catalog', 'service-resource-selection-modes', 'options'],
    queryFn: () => Catalogs.listItems('service-resource-selection-modes', { pageSize: 50 }),
  });
  const currenciesQuery = useQuery({
    queryKey: ['catalog', 'currencies', 'options'],
    queryFn: () => Catalogs.listItems('currencies', { pageSize: 200 }),
  });
  const taxRatesQuery = useQuery({
    queryKey: ['catalog', 'tax-rates', 'options'],
    queryFn: () => Catalogs.listItems('tax-rates', { pageSize: 200 }),
  });
  const roomsQuery = useQuery<RoomDTO[]>({ queryKey: ['rooms', 'service-defaults'], queryFn: Rooms.list });
  const revisionsQuery = useQuery<CatalogRevision[]>({
    queryKey: SERVICE_REVISIONS_QUERY_KEY,
    queryFn: () => Catalogs.listRevisions('services', 1, 100),
  });

  const services = useMemo(
    () => mergeServiceTypes(servicesQuery.data, { includeInactive: true, sort: false }),
    [servicesQuery.data],
  );
  const itemById = useMemo(
    () => new Map((serviceItemsQuery.data?.items ?? []).map((item) => [item.id, item])),
    [serviceItemsQuery.data?.items],
  );
  const optionsUnavailable = categoriesQuery.isError
    || pricingModelsQuery.isError
    || resourceSelectionModesQuery.isError
    || currenciesQuery.isError
    || roomsQuery.isError;
  const defaultResourceSelectionModeId = resourceSelectionModesQuery.data?.items.find(
    (item) => item.code === 'all',
  )?.id ?? resourceSelectionModesQuery.data?.items[0]?.id ?? '';

  const refresh = async () => {
    await Promise.all([
      queryClient.invalidateQueries({ queryKey: SERVICE_QUERY_KEY }),
      queryClient.invalidateQueries({ queryKey: SERVICE_ITEMS_QUERY_KEY }),
      queryClient.invalidateQueries({ queryKey: SERVICE_REVISIONS_QUERY_KEY }),
    ]);
  };
  const createRevision = useMutation({
    mutationFn: (draft: CatalogDraft) => Catalogs.createRevision('services', draft),
    onSuccess: async () => {
      setDialogOpen(false);
      setForm(emptyForm());
      await refresh();
    },
  });
  const submitRevision = useMutation({
    mutationFn: Catalogs.submitRevision,
    onSuccess: refresh,
  });
  const approveRevision = useMutation({
    mutationFn: ({ id, notes }: { id: string; notes: string }) => Catalogs.approveRevision(id, {
      notes,
      emergencyOverride: false,
    }),
    onSuccess: refresh,
  });
  const rejectRevision = useMutation({
    mutationFn: ({ id, notes }: { id: string; notes: string }) => Catalogs.rejectRevision(id, {
      notes,
      emergencyOverride: false,
    }),
    onSuccess: refresh,
  });

  const openCreate = () => {
    const next = emptyForm();
    next.sortOrder = services.length * 10;
    next.categoryId = categoriesQuery.data?.items[0]?.id ?? '';
    next.pricingModelId = pricingModelsQuery.data?.items[0]?.id ?? '';
    next.currencyId = currenciesQuery.data?.items.find((item) => item.code === 'USD')?.id
      ?? currenciesQuery.data?.items[0]?.id
      ?? '';
    setForm(next);
    setDialogOpen(true);
  };

  const openEdit = (service: ServiceType) => {
    const item = itemById.get(service.id);
    setForm({
      entityId: service.id,
      baseVersion: item?.version,
      code: service.code,
      nameEs: servicesQuery.data?.find((entry) => entry.scId === service.id)?.scNameEs ?? service.name,
      nameEn: servicesQuery.data?.find((entry) => entry.scId === service.id)?.scNameEn ?? service.name,
      descriptionEs: item?.descriptionEs ?? '',
      descriptionEn: item?.descriptionEn ?? '',
      sortOrder: item?.sortOrder ?? 0,
      categoryId: service.categoryId,
      pricingModelId: service.pricingModelId,
      rateCents: service.priceCents == null ? '' : String(service.priceCents),
      currencyId: service.currencyId,
      billingUnitEs: service.billingUnit ?? '',
      billingUnitEn: service.billingUnit ?? '',
      taxRateId: service.taxRateId ?? '',
      defaultDurationMinutes: service.defaultDurationMinutes == null ? '' : String(service.defaultDurationMinutes),
      requiresEngineer: service.requiresEngineer,
      resources: service.defaultResources.map((resource) => ({
        resourceId: resource.sdrResourceId,
        selectionModeId: resource.sdrSelectionModeId,
        sortOrder: resource.sdrSortOrder,
      })),
      reason: '',
    });
    setDialogOpen(true);
  };

  const setResourceSelected = (roomId: string, selected: boolean) => {
    setForm((current) => ({
      ...current,
      resources: selected
        ? [...current.resources, {
            resourceId: roomId,
            selectionModeId: defaultResourceSelectionModeId,
            sortOrder: current.resources.length * 10,
          }]
        : current.resources.filter((resource) => resource.resourceId !== roomId),
    }));
  };
  const setResourceMode = (roomId: string, selectionModeId: DefaultResourceDraft['selectionModeId']) => {
    setForm((current) => ({
      ...current,
      resources: current.resources.map((resource) => (
        resource.resourceId === roomId ? { ...resource, selectionModeId } : resource
      )),
    }));
  };

  const submitForm = () => {
    createRevision.mutate(buildServiceOfferingDraft(form));
  };
  const formValid = Boolean(
    form.code.trim()
      && form.nameEs.trim()
      && form.nameEn.trim()
      && form.categoryId
      && form.pricingModelId
      && form.currencyId
      && form.resources.every((resource) => resource.selectionModeId)
      && form.reason.trim(),
  );

  return (
    <Box sx={{ color: '#e2e8f0' }}>
      <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" spacing={1.5} mb={2}>
        <Stack spacing={0.75}>
          <Typography variant="h5" fontWeight={800}>Catálogo de servicios</Typography>
          <Typography variant="body2" color="rgba(226,232,240,0.75)">
            Ofertas canónicas y borradores versionados. Publicar exige revisión; la agenda sólo escribe IDs aprobados.
          </Typography>
        </Stack>
        <Button variant="contained" onClick={openCreate} disabled={optionsUnavailable}>Crear borrador</Button>
      </Stack>

      {(servicesQuery.isError || optionsUnavailable) && (
        <Alert severity="error" sx={{ mb: 2 }}>
          No se pudo cargar toda la configuración canónica. No se habilitarán valores locales ni escritura parcial.
        </Alert>
      )}

      <Card sx={{ bgcolor: 'rgba(255,255,255,0.02)', border: '1px solid rgba(255,255,255,0.08)', mb: 2 }}>
        <CardContent>
          {servicesQuery.isLoading ? (
            <Stack direction="row" spacing={1} alignItems="center"><CircularProgress size={18} /><Typography variant="body2">Cargando servicios…</Typography></Stack>
          ) : services.length === 0 ? (
            <Alert severity="info">No hay ofertas de servicio publicadas.</Alert>
          ) : (
            <LazyPaginatedList
              items={services}
              pagination={{ itemLabel: 'servicios', initialRowsPerPage: 25 }}
              renderItems={(visibleServices) => (
                <Table size="small" aria-label="Servicios canónicos">
                  <TableHead><TableRow>
                    <TableCell>Servicio</TableCell><TableCell>Categoría</TableCell><TableCell>Precio</TableCell>
                    <TableCell>Duración</TableCell><TableCell>Ingeniería</TableCell><TableCell>Recursos</TableCell>
                    <TableCell>Estado</TableCell><TableCell align="right">Acciones</TableCell>
                  </TableRow></TableHead>
                  <TableBody>{visibleServices.map((service) => (
                    <TableRow key={service.id} hover>
                      <TableCell><Stack spacing={0.25}><Typography variant="body2">{service.name}</Typography><Typography variant="caption" color="text.secondary">{service.code} · {service.id}</Typography></Stack></TableCell>
                      <TableCell>{service.kind ?? '—'}</TableCell>
                      <TableCell>{formatPrice(service)}</TableCell>
                      <TableCell>{service.defaultDurationMinutes ? `${service.defaultDurationMinutes} min` : '—'}</TableCell>
                      <TableCell>{service.requiresEngineer ? 'Requerida' : 'Opcional'}</TableCell>
                      <TableCell>{service.defaultResources.map((resource) => resource.sdrResourceName).join(' · ') || '—'}</TableCell>
                      <TableCell><Chip size="small" label={service.active ? 'Activo' : 'Inactivo'} color={service.active ? 'success' : 'default'} /></TableCell>
                      <TableCell align="right"><Button size="small" onClick={() => openEdit(service)}>Crear revisión</Button></TableCell>
                    </TableRow>
                  ))}</TableBody>
                </Table>
              )}
            />
          )}
        </CardContent>
      </Card>

      <Card sx={{ bgcolor: 'rgba(255,255,255,0.02)', border: '1px solid rgba(255,255,255,0.08)' }}>
        <CardContent>
          <Typography variant="h6" mb={1}>Revisiones recientes</Typography>
          {revisionsQuery.isLoading ? <CircularProgress size={18} /> : (revisionsQuery.data?.length ?? 0) === 0 ? (
            <Typography variant="body2" color="text.secondary">No hay borradores ni revisiones recientes.</Typography>
          ) : (
            <Table size="small" aria-label="Revisiones de servicios">
              <TableHead><TableRow><TableCell>Oferta</TableCell><TableCell>Estado</TableCell><TableCell>Notas de revisión</TableCell><TableCell align="right">Acciones</TableCell></TableRow></TableHead>
              <TableBody>{revisionsQuery.data?.map((revision) => {
                const notes = reviewNotes[revision.id] ?? '';
                return (
                  <TableRow key={revision.id}>
                    <TableCell><Typography variant="body2">{revision.draft.nameEs}</Typography><Typography variant="caption" color="text.secondary">{revision.id}</Typography></TableCell>
                    <TableCell><Chip size="small" label={revision.workflowState} color={revisionStateColor(revision.workflowState)} /></TableCell>
                    <TableCell><TextField size="small" value={notes} onChange={(event) => setReviewNotes((current) => ({ ...current, [revision.id]: event.target.value }))} label="Nota o motivo" inputProps={{ 'aria-label': `Nota para ${revision.draft.nameEs}` }} /></TableCell>
                    <TableCell align="right"><Stack direction="row" spacing={0.5} justifyContent="flex-end">
                      {(revision.workflowState === 'draft' || revision.workflowState === 'rejected') && <Button size="small" onClick={() => submitRevision.mutate(revision.id)}>Enviar</Button>}
                      {revision.workflowState === 'review' && <Button size="small" color="success" disabled={!notes.trim()} onClick={() => approveRevision.mutate({ id: revision.id, notes })}>Aprobar</Button>}
                      {revision.workflowState === 'review' && <Button size="small" color="error" disabled={!notes.trim()} onClick={() => rejectRevision.mutate({ id: revision.id, notes })}>Rechazar</Button>}
                    </Stack></TableCell>
                  </TableRow>
                );
              })}</TableBody>
            </Table>
          )}
        </CardContent>
      </Card>

      <Dialog open={dialogOpen} onClose={() => setDialogOpen(false)} fullWidth maxWidth="md">
        <DialogTitle>{form.entityId ? 'Crear revisión de servicio' : 'Crear oferta en borrador'}</DialogTitle>
        <DialogContent>
          <Stack spacing={2} mt={1}>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
              <TextField fullWidth required label="Código interno" value={form.code} onChange={(event) => setForm({ ...form, code: event.target.value })} />
              <TextField fullWidth label="Precio en centavos" type="number" value={form.rateCents} onChange={(event) => setForm({ ...form, rateCents: event.target.value })} inputProps={{ min: 0 }} />
              <TextField fullWidth label="Orden manual" type="number" value={form.sortOrder} onChange={(event) => setForm({ ...form, sortOrder: Number(event.target.value) })} inputProps={{ min: 0 }} />
            </Stack>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
              <TextField fullWidth required label="Nombre en español" value={form.nameEs} onChange={(event) => setForm({ ...form, nameEs: event.target.value })} />
              <TextField fullWidth required label="Nombre en inglés" value={form.nameEn} onChange={(event) => setForm({ ...form, nameEn: event.target.value })} />
            </Stack>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
              <TextField fullWidth multiline minRows={2} label="Descripción en español" value={form.descriptionEs} onChange={(event) => setForm({ ...form, descriptionEs: event.target.value })} />
              <TextField fullWidth multiline minRows={2} label="Descripción en inglés" value={form.descriptionEn} onChange={(event) => setForm({ ...form, descriptionEn: event.target.value })} />
            </Stack>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={1.5}>
              <TextField select fullWidth required label="Categoría" value={form.categoryId} onChange={(event) => setForm({ ...form, categoryId: event.target.value })}>{categoriesQuery.data?.items.map((item: CatalogItem) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}</TextField>
              <TextField select fullWidth required label="Modelo de precio" value={form.pricingModelId} onChange={(event) => setForm({ ...form, pricingModelId: event.target.value })}>{pricingModelsQuery.data?.items.map((item: CatalogItem) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}</TextField>
              <TextField select fullWidth required label="Moneda" value={form.currencyId} onChange={(event) => setForm({ ...form, currencyId: event.target.value })}>{currenciesQuery.data?.items.map((item: CatalogItem) => <MenuItem key={item.id} value={item.id}>{item.code} · {item.name}</MenuItem>)}</TextField>
            </Stack>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={1.5}>
              <TextField fullWidth label="Unidad en español" value={form.billingUnitEs} onChange={(event) => setForm({ ...form, billingUnitEs: event.target.value })} inputProps={{ maxLength: 80 }} />
              <TextField fullWidth label="Unidad en inglés" value={form.billingUnitEn} onChange={(event) => setForm({ ...form, billingUnitEn: event.target.value })} inputProps={{ maxLength: 80 }} />
              <TextField fullWidth label="Duración predeterminada (min)" type="number" value={form.defaultDurationMinutes} onChange={(event) => setForm({ ...form, defaultDurationMinutes: event.target.value })} inputProps={{ min: 15, max: 840 }} />
            </Stack>
            <TextField select fullWidth label="Tasa tributaria gobernada (opcional)" value={form.taxRateId} onChange={(event) => setForm({ ...form, taxRateId: event.target.value })}><MenuItem value="">Sin tasa</MenuItem>{taxRatesQuery.data?.items.map((item: CatalogItem) => <MenuItem key={item.id} value={item.id}>{item.code} · {item.name}</MenuItem>)}</TextField>
            <FormControlLabel control={<Checkbox checked={form.requiresEngineer} onChange={(event) => setForm({ ...form, requiresEngineer: event.target.checked })} />} label="Requiere ingeniería" />
            <FormControl fullWidth>
              <InputLabel id="service-default-resources-label">Recursos predeterminados</InputLabel>
              <Select labelId="service-default-resources-label" multiple value={form.resources.map((resource) => resource.resourceId)} label="Recursos predeterminados" renderValue={(selected) => selected.map((id) => roomsQuery.data?.find((room) => room.roomId === id)?.rName ?? id).join(', ')} onChange={(event) => {
                const selected = event.target.value as string[];
                const selectedSet = new Set(selected);
                setForm((current) => ({ ...current, resources: selected.map((roomId, index) => current.resources.find((resource) => resource.resourceId === roomId) ?? { resourceId: roomId, selectionModeId: defaultResourceSelectionModeId, sortOrder: index * 10 }).filter((resource) => selectedSet.has(resource.resourceId)) }));
              }}>{roomsQuery.data?.map((room) => <MenuItem key={room.roomId} value={room.roomId}><Checkbox checked={form.resources.some((resource) => resource.resourceId === room.roomId)} />{room.rName}</MenuItem>)}</Select>
            </FormControl>
            {form.resources.map((resource) => {
              const room = roomsQuery.data?.find((candidate) => candidate.roomId === resource.resourceId);
              return <Stack key={resource.resourceId} direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}><FormControlLabel control={<Checkbox checked onChange={(event) => setResourceSelected(resource.resourceId, event.target.checked)} />} label={room?.rName ?? resource.resourceId} /><TextField select size="small" required label="Política" value={resource.selectionModeId} onChange={(event) => setResourceMode(resource.resourceId, event.target.value)}>{resourceSelectionModesQuery.data?.items.map((mode) => <MenuItem key={mode.id} value={mode.id}>{mode.name}</MenuItem>)}</TextField></Stack>;
            })}
            <TextField required multiline minRows={2} label="Motivo del cambio" value={form.reason} onChange={(event) => setForm({ ...form, reason: event.target.value })} />
            {createRevision.isError && <Alert severity="error">No se pudo crear el borrador. Verifica permisos, relaciones y versiones.</Alert>}
          </Stack>
        </DialogContent>
        <DialogActions><Button onClick={() => setDialogOpen(false)}>Cancelar</Button><Button variant="contained" disabled={!formValid || createRevision.isPending} onClick={submitForm}>Guardar borrador</Button></DialogActions>
      </Dialog>
    </Box>
  );
}
