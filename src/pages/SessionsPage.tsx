import { useEffect, useMemo, useState } from 'react';
import {
  Box,
  Button,
  Checkbox,
  Chip,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Divider,
  FormControl,
  FormControlLabel,
  IconButton,
  InputLabel,
  MenuItem,
  Paper,
  Select,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TablePagination,
  TableRow,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import { AddCircleOutline, DeleteOutline } from '@mui/icons-material';
import { Controller, useFieldArray, useForm, useWatch, type Control, type FieldErrors } from 'react-hook-form';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Sessions } from '../api/sessions';
import { Rooms } from '../api/rooms';
import { Inventory } from '../api/inventory';
import type { AssetDTO, BandChoiceDTO, Page, RoomDTO, SessionCreate, SessionDTO, SessionInputRowPayload, SessionUpdate } from '../api/types';

const STATUS_OPTIONS: Array<{ value: string; label: string; color: 'default' | 'success' | 'warning' | 'info' | 'error' }> = [
  { value: 'InPrep', label: 'En preparación', color: 'info' },
  { value: 'InSession', label: 'En sesión', color: 'success' },
  { value: 'Break', label: 'Break', color: 'warning' },
  { value: 'Editing', label: 'Edición', color: 'default' },
  { value: 'Approved', label: 'Aprobada', color: 'success' },
  { value: 'Delivered', label: 'Entregada', color: 'success' },
  { value: 'Closed', label: 'Cerrada', color: 'default' },
];

const isMicCategory = (category: string) => {
  const normalized = category.toLowerCase();
  return normalized.includes('mic');
};

const isPreampCategory = (category: string) => {
  const normalized = category.toLowerCase();
  return normalized.includes('preamp') || normalized.includes('pre amp') || normalized.includes('preampl');
};

const normalizeAssetId = (value?: string) => value?.trim() ?? '';

const ensureOptionForCurrentValue = (options: AssetDTO[], currentId: string): AssetDTO[] => {
  if (!currentId) return options;
  if (options.some(option => option.assetId === currentId)) return options;
  return [
    ...options,
    {
      assetId: currentId,
      name: currentId,
      category: 'Otro',
      status: 'Desconocido',
    },
  ];
};

const optionalNumberString = z
  .string()
  .optional()
  .refine(
    (value) => value === undefined || value.trim() === '' || !Number.isNaN(Number(value)),
    { message: 'Debe ser un número válido' },
  );

const inputRowSchema = z.object({
  channelNumber: z
    .string()
    .min(1, 'Requerido')
    .refine((value) => !Number.isNaN(Number(value)), { message: 'Debe ser un número' }),
  trackName: z.string().optional(),
  instrument: z.string().optional(),
  micId: z.string().optional(),
  standId: z.string().optional(),
  cableId: z.string().optional(),
  preampId: z.string().optional(),
  insertOutboardId: z.string().optional(),
  converterChannel: z.string().optional(),
  phantom: z.boolean().optional(),
  polarity: z.boolean().optional(),
  hpf: z.boolean().optional(),
  pad: z.boolean().optional(),
  notes: z.string().optional(),
});

const sessionSchema = z.object({
  bookingRef: z.string().optional(),
  bandId: z.string().optional(),
  clientPartyRef: z.string().optional(),
  service: z.string().min(1, 'Requerido'),
  startAt: z.string().min(1, 'Requerido'),
  endAt: z.string().min(1, 'Requerido'),
  engineerRef: z.string().min(1, 'Ingresa un ingeniero'),
  assistantRef: z.string().optional(),
  roomIds: z.array(z.string()).min(1, 'Selecciona al menos una sala'),
  sampleRate: optionalNumberString,
  bitDepth: optionalNumberString,
  daw: z.string().optional(),
  sessionFolderDriveId: z.string().optional(),
  notes: z.string().optional(),
  inputListRows: z.array(inputRowSchema).default([]),
  status: z.string().optional(),
});

type SessionFormValues = z.infer<typeof sessionSchema>;
type InputRowFormValues = z.infer<typeof inputRowSchema>;

function formatDateRange(start: string, end: string) {
  const startDate = new Date(start);
  const endDate = new Date(end);
  return `${startDate.toLocaleString()} → ${endDate.toLocaleTimeString()}`;
}

const toInputValue = (date: Date) => {
  const offset = date.getTimezoneOffset();
  const adjusted = new Date(date.getTime() - offset * 60 * 1000);
  return adjusted.toISOString().slice(0, 16);
};

const isoToLocalInput = (iso: string) => {
  const parsed = new Date(iso);
  if (Number.isNaN(parsed.getTime())) return iso;
  return toInputValue(parsed);
};

const buildDefaultSessionFormValues = (): SessionFormValues => {
  const now = new Date();
  const twoHoursLater = new Date(now.getTime() + 2 * 60 * 60 * 1000);
  return {
    bookingRef: '',
    bandId: '',
    clientPartyRef: '',
    service: '',
    startAt: toInputValue(now),
    endAt: toInputValue(twoHoursLater),
    engineerRef: '',
    assistantRef: '',
    roomIds: [],
    sampleRate: '',
    bitDepth: '',
    daw: '',
    sessionFolderDriveId: '',
    notes: '',
    inputListRows: [],
    status: 'InPrep',
  };
};

const cleanString = (value?: string) => {
  if (value === undefined) return undefined;
  const trimmed = value.trim();
  return trimmed.length ? trimmed : undefined;
};

const stringOrNull = (value?: string) => {
  if (value === undefined) return undefined;
  const trimmed = value.trim();
  if (!trimmed.length) return null;
  return trimmed;
};

const numberFromString = (value?: string): number | null => {
  if (value === undefined) return null;
  const trimmed = value.trim();
  if (!trimmed.length) return null;
  const parsed = Number(trimmed);
  return Number.isNaN(parsed) ? null : parsed;
};

const numberOrNull = (value?: string) => {
  if (value === undefined) return undefined;
  const trimmed = value.trim();
  if (!trimmed.length) return null;
  const parsed = Number(trimmed);
  return Number.isNaN(parsed) ? undefined : parsed;
};

const mapInputRows = (rows: InputRowFormValues[]): SessionInputRowPayload[] => {
  const payload: SessionInputRowPayload[] = [];
  rows.forEach((row) => {
    const channelNumber = numberFromString(row.channelNumber);
    if (channelNumber === null) {
      return;
    }

    const entry: SessionInputRowPayload = { channelNumber };

    const trackName = stringOrNull(row.trackName);
    if (trackName !== undefined) entry.trackName = trackName;

    const instrument = stringOrNull(row.instrument);
    if (instrument !== undefined) entry.instrument = instrument;

    const micId = stringOrNull(row.micId);
    if (micId !== undefined) entry.micId = micId;

    const standId = stringOrNull(row.standId);
    if (standId !== undefined) entry.standId = standId;

    const cableId = stringOrNull(row.cableId);
    if (cableId !== undefined) entry.cableId = cableId;

    const preampId = stringOrNull(row.preampId);
    if (preampId !== undefined) entry.preampId = preampId;

    const insertOutboardId = stringOrNull(row.insertOutboardId);
    if (insertOutboardId !== undefined) entry.insertOutboardId = insertOutboardId;

    const converterChannel = stringOrNull(row.converterChannel);
    if (converterChannel !== undefined) entry.converterChannel = converterChannel;

    if (row.phantom !== undefined) entry.phantom = row.phantom;
    if (row.polarity !== undefined) entry.polarity = row.polarity;
    if (row.hpf !== undefined) entry.hpf = row.hpf;
    if (row.pad !== undefined) entry.pad = row.pad;

    const notes = stringOrNull(row.notes);
    if (notes !== undefined) entry.notes = notes;

    payload.push(entry);
  });
  return payload;
};

const sessionDtoToFormValues = (session: SessionDTO): SessionFormValues => ({
  bookingRef: session.sBookingRef ?? '',
  bandId: session.sBandId ?? '',
  clientPartyRef: session.sClientPartyRef ?? '',
  service: session.sService ?? '',
  startAt: isoToLocalInput(session.sStartAt),
  endAt: isoToLocalInput(session.sEndAt),
  engineerRef: session.sEngineerRef ?? '',
  assistantRef: session.sAssistantRef ?? '',
  roomIds: session.sRoomIds ?? [],
  sampleRate: session.sSampleRate != null ? String(session.sSampleRate) : '',
  bitDepth: session.sBitDepth != null ? String(session.sBitDepth) : '',
  daw: session.sDaw ?? '',
  sessionFolderDriveId: session.sSessionFolderDriveId ?? '',
  notes: session.sNotes ?? '',
  inputListRows: (session.sInputListRows ?? []).map((row, index) => ({
    channelNumber: row?.channelNumber != null ? String(row.channelNumber) : String(index + 1),
    trackName: row?.trackName ?? '',
    instrument: row?.instrument ?? '',
    micId: row?.micId ?? '',
    standId: row?.standId ?? '',
    cableId: row?.cableId ?? '',
    preampId: row?.preampId ?? '',
    insertOutboardId: row?.insertOutboardId ?? '',
    converterChannel: row?.converterChannel ?? '',
    phantom: row?.phantom ?? false,
    polarity: row?.polarity ?? false,
    hpf: row?.hpf ?? false,
    pad: row?.pad ?? false,
    notes: row?.notes ?? '',
  })),
  status: session.sStatus ?? 'InPrep',
});

const createEmptyRow = (index: number): InputRowFormValues => ({
  channelNumber: String(index + 1),
  trackName: '',
  instrument: '',
  micId: '',
  standId: '',
  cableId: '',
  preampId: '',
  insertOutboardId: '',
  converterChannel: '',
  phantom: false,
  polarity: false,
  hpf: false,
  pad: false,
  notes: '',
});

const toSessionCreatePayload = (values: SessionFormValues): SessionCreate => ({
  scBookingRef: stringOrNull(values.bookingRef),
  scBandId: stringOrNull(values.bandId),
  scClientPartyRef: stringOrNull(values.clientPartyRef),
  scService: values.service.trim(),
  scStartAt: new Date(values.startAt).toISOString(),
  scEndAt: new Date(values.endAt).toISOString(),
  scEngineerRef: values.engineerRef.trim(),
  scAssistantRef: stringOrNull(values.assistantRef),
  scRoomIds: values.roomIds,
  scSampleRate: numberOrNull(values.sampleRate),
  scBitDepth: numberOrNull(values.bitDepth),
  scDaw: stringOrNull(values.daw),
  scSessionFolderDriveId: stringOrNull(values.sessionFolderDriveId),
  scNotes: stringOrNull(values.notes),
  scInputListRows: mapInputRows(values.inputListRows),
  scStatus: cleanString(values.status),
});

const toSessionUpdatePayload = (values: SessionFormValues): SessionUpdate => ({
  suBookingRef: stringOrNull(values.bookingRef),
  suBandId: stringOrNull(values.bandId),
  suClientPartyRef: stringOrNull(values.clientPartyRef),
  suService: values.service.trim(),
  suStartAt: new Date(values.startAt).toISOString(),
  suEndAt: new Date(values.endAt).toISOString(),
  suEngineerRef: values.engineerRef.trim(),
  suAssistantRef: stringOrNull(values.assistantRef),
  suRoomIds: values.roomIds,
  suSampleRate: numberOrNull(values.sampleRate),
  suBitDepth: numberOrNull(values.bitDepth),
  suDaw: stringOrNull(values.daw),
  suSessionFolderDriveId: stringOrNull(values.sessionFolderDriveId),
  suNotes: stringOrNull(values.notes),
  suInputListRows: mapInputRows(values.inputListRows),
  suStatus: cleanString(values.status),
});

type SessionFormFieldsProps = {
  control: Control<SessionFormValues>;
  errors: FieldErrors<SessionFormValues>;
  rooms: RoomDTO[];
  bandChoices: BandChoiceDTO[];
  bandChoicesLoading?: boolean;
  showStatus?: boolean;
};

function SessionFormFields({ control, errors, rooms, bandChoices, bandChoicesLoading = false, showStatus = false }: SessionFormFieldsProps) {
  const { fields, append, remove } = useFieldArray({ control, name: 'inputListRows' });
  const watchedInputRows = useWatch({ control, name: 'inputListRows' }) ?? [];

  const inventoryQuery = useQuery<Page<AssetDTO>>({
    queryKey: ['inventory', 'session-form'],
    queryFn: () => Inventory.list({ pageSize: 500 }),
  });

  const inventoryItems = inventoryQuery.data?.items ?? [];

  const micAssets = useMemo(
    () => inventoryItems.filter(asset => asset.category && isMicCategory(asset.category)),
    [inventoryItems],
  );

  const preampAssets = useMemo(
    () => inventoryItems.filter(asset => asset.category && isPreampCategory(asset.category)),
    [inventoryItems],
  );

  const selectedMicIds = useMemo(() => {
    const ids = new Set<string>();
    watchedInputRows.forEach(row => {
      const id = normalizeAssetId(row?.micId);
      if (id) ids.add(id);
    });
    return ids;
  }, [watchedInputRows]);

  const selectedPreampIds = useMemo(() => {
    const ids = new Set<string>();
    watchedInputRows.forEach(row => {
      const id = normalizeAssetId(row?.preampId);
      if (id) ids.add(id);
    });
    return ids;
  }, [watchedInputRows]);

  return (
    <Stack spacing={3} sx={{ mt: 1 }}>
      <Stack spacing={2}>
        <Controller
          name="service"
          control={control}
          render={({ field }) => (
            <TextField
              {...field}
              label="Servicio"
              placeholder="Grabación, mezcla, ensayo…"
              error={!!errors.service}
              helperText={errors.service?.message}
              fullWidth
            />
          )}
        />
        <Controller
          name="bookingRef"
          control={control}
          render={({ field }) => (
            <TextField
              {...field}
              label="Referencia de booking"
              placeholder="BK-2025-001"
              fullWidth
            />
          )}
        />
        <Controller
          name="clientPartyRef"
          control={control}
          render={({ field }) => (
            <TextField
              {...field}
              label="Cliente (party ref)"
              placeholder="party-uuid"
              fullWidth
            />
          )}
        />
        <Controller
          name="bandId"
          control={control}
          render={({ field }) => (
            <TextField
              {...field}
              value={field.value ?? ''}
              select
              label="Banda"
              placeholder="Selecciona una banda"
              SelectProps={{ displayEmpty: true }}
              error={!!errors.bandId}
              helperText={
                errors.bandId?.message
                  ?? (bandChoicesLoading
                    ? 'Cargando bandas…'
                    : bandChoices.length === 0
                      ? 'No hay bandas registradas'
                      : undefined)
              }
              disabled={bandChoicesLoading && bandChoices.length === 0}
              fullWidth
            >
              <MenuItem value="">
                <em>Sin banda</em>
              </MenuItem>
              {bandChoices.map((band) => (
                <MenuItem key={band.bandId} value={band.bandId}>
                  {band.name}
                </MenuItem>
              ))}
            </TextField>
          )}
        />
        <Controller
          name="startAt"
          control={control}
          render={({ field }) => (
            <TextField
              {...field}
              label="Inicio"
              type="datetime-local"
              InputLabelProps={{ shrink: true }}
              error={!!errors.startAt}
              helperText={errors.startAt?.message}
              fullWidth
            />
          )}
        />
        <Controller
          name="endAt"
          control={control}
          render={({ field }) => (
            <TextField
              {...field}
              label="Fin"
              type="datetime-local"
              InputLabelProps={{ shrink: true }}
              error={!!errors.endAt}
              helperText={errors.endAt?.message}
              fullWidth
            />
          )}
        />
        <Controller
          name="engineerRef"
          control={control}
          render={({ field }) => (
            <TextField
              {...field}
              label="Ingeniero principal"
              placeholder="usuario@tdf"
              error={!!errors.engineerRef}
              helperText={errors.engineerRef?.message}
              fullWidth
            />
          )}
        />
        <Controller
          name="assistantRef"
          control={control}
          render={({ field }) => (
            <TextField
              {...field}
              label="Asistente"
              placeholder="asistente@tdf"
              fullWidth
            />
          )}
        />
        <Controller
          name="roomIds"
          control={control}
          render={({ field }) => (
            <FormControl fullWidth error={!!errors.roomIds}>
              <InputLabel id="rooms-label">Salas asignadas</InputLabel>
              <Select
                {...field}
                labelId="rooms-label"
                label="Salas asignadas"
                multiple
                value={field.value ?? []}
                onChange={(event) => field.onChange(event.target.value as string[])}
                renderValue={(vals) => (vals as string[]).map(id => rooms.find(r => r.roomId === id)?.rName ?? id).join(', ')}
              >
                {rooms.map(room => (
                  <MenuItem key={room.roomId} value={room.roomId}>
                    {room.rName}
                  </MenuItem>
                ))}
              </Select>
              {errors.roomIds && (
                <Typography color="error" variant="caption">
                  {errors.roomIds.message as string}
                </Typography>
              )}
            </FormControl>
          )}
        />
        <Controller
          name="sampleRate"
          control={control}
          render={({ field }) => (
            <TextField
              {...field}
              label="Sample rate (Hz)"
              type="number"
              error={!!errors.sampleRate}
              helperText={errors.sampleRate?.message}
              fullWidth
            />
          )}
        />
        <Controller
          name="bitDepth"
          control={control}
          render={({ field }) => (
            <TextField
              {...field}
              label="Bit depth"
              type="number"
              error={!!errors.bitDepth}
              helperText={errors.bitDepth?.message}
              fullWidth
            />
          )}
        />
        <Controller
          name="daw"
          control={control}
          render={({ field }) => (
            <TextField
              {...field}
              label="DAW"
              placeholder="Pro Tools, Logic, Ableton…"
              fullWidth
            />
          )}
        />
        <Controller
          name="sessionFolderDriveId"
          control={control}
          render={({ field }) => (
            <TextField
              {...field}
              label="Carpeta en Drive"
              placeholder="drive-folder-id"
              fullWidth
            />
          )}
        />
        <Controller
          name="notes"
          control={control}
          render={({ field }) => (
            <TextField
              {...field}
              label="Notas"
              placeholder="Backline, requerimientos, contactos…"
              multiline
              minRows={3}
              fullWidth
            />
          )}
        />
        {showStatus && (
          <Controller
            name="status"
            control={control}
            render={({ field }) => (
              <FormControl fullWidth>
                <InputLabel id="status-label">Estado</InputLabel>
                <Select
                  {...field}
                  labelId="status-label"
                  label="Estado"
                  value={field.value ?? ''}
                  onChange={(event) => field.onChange(event.target.value)}
                >
                  <MenuItem value="">
                    <em>Usar predeterminado</em>
                  </MenuItem>
                  {STATUS_OPTIONS.map(option => (
                    <MenuItem key={option.value} value={option.value}>
                      {option.label}
                    </MenuItem>
                  ))}
                </Select>
              </FormControl>
            )}
          />
        )}
      </Stack>

      <Divider />

      <Stack spacing={2}>
        <Stack direction="row" alignItems="center" justifyContent="space-between">
          <Typography variant="subtitle1" fontWeight={600}>Input list</Typography>
          <Button
            variant="text"
            startIcon={<AddCircleOutline />}
            onClick={() => append(createEmptyRow(fields.length))}
          >
            Añadir canal
          </Button>
        </Stack>

        {fields.length === 0 && (
          <Typography variant="body2" color="text.secondary">
            Agrega canales para capturar el setup de microfonía y ruteo.
          </Typography>
        )}

        {fields.map((fieldItem, index) => {
          const rowErrors = errors.inputListRows?.[index];
          const rowValues = watchedInputRows?.[index];
          const currentMicId = normalizeAssetId(rowValues?.micId);
          const currentPreampId = normalizeAssetId(rowValues?.preampId);
          const micOptions = ensureOptionForCurrentValue(
            micAssets.filter(asset => asset.assetId === currentMicId || !selectedMicIds.has(asset.assetId)),
            currentMicId,
          );
          const preampOptions = ensureOptionForCurrentValue(
            preampAssets.filter(asset => asset.assetId === currentPreampId || !selectedPreampIds.has(asset.assetId)),
            currentPreampId,
          );
          const micHelperText = inventoryQuery.isPending
            ? 'Cargando inventario…'
            : inventoryQuery.isError
              ? 'Error cargando inventario'
              : micOptions.length === 0
                ? 'No hay micrófonos disponibles'
                : undefined;
          const preampHelperText = inventoryQuery.isPending
            ? 'Cargando inventario…'
            : inventoryQuery.isError
              ? 'Error cargando inventario'
              : preampOptions.length === 0
                ? 'No hay preamps disponibles'
                : undefined;
          return (
            <Paper key={fieldItem.id} variant="outlined" sx={{ p: 2 }}>
              <Stack spacing={2}>
                <Stack direction="row" alignItems="flex-start" spacing={1}>
                  <Controller
                    name={`inputListRows.${index}.channelNumber` as const}
                    control={control}
                    render={({ field }) => (
                      <TextField
                        {...field}
                        label="Canal"
                        type="number"
                        InputLabelProps={{ shrink: true }}
                        sx={{ flex: 1 }}
                        error={!!rowErrors?.channelNumber}
                        helperText={rowErrors?.channelNumber?.message}
                      />
                    )}
                  />
                  <Tooltip title="Eliminar canal">
                    <IconButton aria-label="Eliminar canal" onClick={() => remove(index)}>
                      <DeleteOutline />
                    </IconButton>
                  </Tooltip>
                </Stack>

                <Controller
                  name={`inputListRows.${index}.trackName` as const}
                  control={control}
                  render={({ field }) => (
                    <TextField {...field} label="Track" placeholder="Voz, Guitarra L, etc." fullWidth />
                  )}
                />

                <Controller
                  name={`inputListRows.${index}.instrument` as const}
                  control={control}
                  render={({ field }) => (
                    <TextField {...field} label="Instrumento/Fuente" placeholder="SM57 sobre snare" fullWidth />
                  )}
                />

                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                  <Controller
                    name={`inputListRows.${index}.micId` as const}
                    control={control}
                    render={({ field }) => (
                      <TextField
                        {...field}
                        select
                        label="Mic"
                        value={field.value ?? ''}
                        onChange={(event) => field.onChange(event.target.value)}
                        SelectProps={{ displayEmpty: true }}
                        helperText={micHelperText}
                        fullWidth
                      >
                        <MenuItem value="">
                          <em>Sin asignar</em>
                        </MenuItem>
                        {micOptions.map(asset => (
                          <MenuItem key={asset.assetId} value={asset.assetId}>
                            {asset.name} ({asset.assetId})
                          </MenuItem>
                        ))}
                      </TextField>
                    )}
                  />
                  <Controller
                    name={`inputListRows.${index}.preampId` as const}
                    control={control}
                    render={({ field }) => (
                      <TextField
                        {...field}
                        select
                        label="Preamp"
                        value={field.value ?? ''}
                        onChange={(event) => field.onChange(event.target.value)}
                        SelectProps={{ displayEmpty: true }}
                        helperText={preampHelperText}
                        fullWidth
                      >
                        <MenuItem value="">
                          <em>Sin asignar</em>
                        </MenuItem>
                        {preampOptions.map(asset => (
                          <MenuItem key={asset.assetId} value={asset.assetId}>
                            {asset.name} ({asset.assetId})
                          </MenuItem>
                        ))}
                      </TextField>
                    )}
                  />
                </Stack>

                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                  <Controller
                    name={`inputListRows.${index}.standId` as const}
                    control={control}
                    render={({ field }) => (
                      <TextField {...field} label="Stand" fullWidth />
                    )}
                  />
                  <Controller
                    name={`inputListRows.${index}.cableId` as const}
                    control={control}
                    render={({ field }) => (
                      <TextField {...field} label="Cable" fullWidth />
                    )}
                  />
                  <Controller
                    name={`inputListRows.${index}.insertOutboardId` as const}
                    control={control}
                    render={({ field }) => (
                      <TextField {...field} label="Insert/Outboard" fullWidth />
                    )}
                  />
                </Stack>

                <Controller
                  name={`inputListRows.${index}.converterChannel` as const}
                  control={control}
                  render={({ field }) => (
                    <TextField {...field} label="Canal convertidor / Patch" fullWidth />
                  )}
                />

                <Stack direction="row" spacing={2} sx={{ flexWrap: 'wrap' }}>
                  <Controller
                    name={`inputListRows.${index}.phantom` as const}
                    control={control}
                    render={({ field }) => (
                      <FormControlLabel
                        control={<Checkbox checked={!!field.value} onChange={(event) => field.onChange(event.target.checked)} />}
                        label="+48V"
                      />
                    )}
                  />
                  <Controller
                    name={`inputListRows.${index}.polarity` as const}
                    control={control}
                    render={({ field }) => (
                      <FormControlLabel
                        control={<Checkbox checked={!!field.value} onChange={(event) => field.onChange(event.target.checked)} />}
                        label="Ø invertida"
                      />
                    )}
                  />
                  <Controller
                    name={`inputListRows.${index}.hpf` as const}
                    control={control}
                    render={({ field }) => (
                      <FormControlLabel
                        control={<Checkbox checked={!!field.value} onChange={(event) => field.onChange(event.target.checked)} />}
                        label="HPF"
                      />
                    )}
                  />
                  <Controller
                    name={`inputListRows.${index}.pad` as const}
                    control={control}
                    render={({ field }) => (
                      <FormControlLabel
                        control={<Checkbox checked={!!field.value} onChange={(event) => field.onChange(event.target.checked)} />}
                        label="Pad"
                      />
                    )}
                  />
                </Stack>

                <Controller
                  name={`inputListRows.${index}.notes` as const}
                  control={control}
                  render={({ field }) => (
                    <TextField {...field} label="Notas del canal" multiline minRows={2} fullWidth />
                  )}
                />
              </Stack>
            </Paper>
          );
        })}
      </Stack>
    </Stack>
  );
}

function CreateSessionDialog({
  open,
  onClose,
  rooms,
  bandChoices,
  bandChoicesLoading,
}: {
  open: boolean;
  onClose: () => void;
  rooms: RoomDTO[];
  bandChoices: BandChoiceDTO[];
  bandChoicesLoading: boolean;
}) {
  const qc = useQueryClient();
  const { control, handleSubmit, reset, formState: { errors } } = useForm<SessionFormValues>({
    resolver: zodResolver(sessionSchema),
    defaultValues: useMemo(() => buildDefaultSessionFormValues(), []),
  });

  useEffect(() => {
    if (open) {
      reset(buildDefaultSessionFormValues());
    }
  }, [open, reset]);

  const createMutation = useMutation({
    mutationFn: (body: SessionCreate) => Sessions.create(body),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['sessions'] });
      reset(buildDefaultSessionFormValues());
      onClose();
    },
  });

  const submit = (values: SessionFormValues) => {
    const payload = toSessionCreatePayload(values);
    createMutation.mutate(payload);
  };

  return (
    <Dialog open={open} onClose={onClose} maxWidth="md" fullWidth>
      <DialogTitle>Nueva sesión</DialogTitle>
      <Box component="form" onSubmit={handleSubmit(submit)}>
        <DialogContent dividers>
          <SessionFormFields
            control={control}
            errors={errors}
            rooms={rooms}
            bandChoices={bandChoices}
            bandChoicesLoading={bandChoicesLoading}
            showStatus
          />
        </DialogContent>
        <DialogActions>
          <Button onClick={onClose} disabled={createMutation.isPending}>Cancelar</Button>
          <Button type="submit" variant="contained" disabled={createMutation.isPending}>
            {createMutation.isPending ? 'Creando…' : 'Crear sesión'}
          </Button>
        </DialogActions>
      </Box>
    </Dialog>
  );
}

function EditSessionDialog({
  open,
  onClose,
  rooms,
  bandChoices,
  bandChoicesLoading,
  sessionId,
}: {
  open: boolean;
  onClose: () => void;
  rooms: RoomDTO[];
  bandChoices: BandChoiceDTO[];
  bandChoicesLoading: boolean;
  sessionId: string | null;
}) {
  const qc = useQueryClient();
  const { control, handleSubmit, reset, formState: { errors } } = useForm<SessionFormValues>({
    resolver: zodResolver(sessionSchema),
    defaultValues: useMemo(() => buildDefaultSessionFormValues(), []),
  });

  const sessionQuery = useQuery({
    queryKey: ['session', sessionId],
    queryFn: () => Sessions.detail(sessionId!),
    enabled: open && !!sessionId,
  });

  useEffect(() => {
    if (sessionQuery.data) {
      reset(sessionDtoToFormValues(sessionQuery.data));
    }
  }, [sessionQuery.data, reset]);

  useEffect(() => {
    if (!open) {
      reset(buildDefaultSessionFormValues());
    }
  }, [open, reset]);

  const updateMutation = useMutation({
    mutationFn: ({ id, body }: { id: string; body: SessionUpdate }) => Sessions.update(id, body),
    onSuccess: (_data, variables) => {
      qc.invalidateQueries({ queryKey: ['sessions'] });
      qc.invalidateQueries({ queryKey: ['session', variables.id] });
      onClose();
    },
  });

  const submit = (values: SessionFormValues) => {
    if (!sessionId) return;
    updateMutation.mutate({ id: sessionId, body: toSessionUpdatePayload(values) });
  };

  const isLoading = sessionQuery.isPending && !sessionQuery.data;

  return (
    <Dialog open={open} onClose={onClose} maxWidth="md" fullWidth>
      <DialogTitle>Editar sesión</DialogTitle>
      <Box component="form" onSubmit={handleSubmit(submit)}>
        <DialogContent dividers>
          {isLoading ? (
            <Box sx={{ display: 'flex', justifyContent: 'center', py: 4 }}>
              <CircularProgress />
            </Box>
          ) : (
            <SessionFormFields
              control={control}
              errors={errors}
              rooms={rooms}
              bandChoices={bandChoices}
              bandChoicesLoading={bandChoicesLoading}
              showStatus
            />
          )}
        </DialogContent>
        <DialogActions>
          <Button onClick={onClose} disabled={updateMutation.isPending}>Cancelar</Button>
          <Button
            type="submit"
            variant="contained"
            disabled={updateMutation.isPending || isLoading || !sessionId}
          >
            {updateMutation.isPending ? 'Guardando…' : 'Guardar cambios'}
          </Button>
        </DialogActions>
      </Box>
    </Dialog>
  );
}

function StatusChip({ value }: { value: string }) {
  const option = STATUS_OPTIONS.find(opt => opt.value === value);
  return (
    <Chip
      size="small"
      label={option?.label ?? value}
      color={option?.color ?? 'default'}
      sx={{ fontWeight: 600 }}
    />
  );
}

export default function SessionsPage() {
  const [page, setPage] = useState(0);
  const [pageSize, setPageSize] = useState(10);
  const [createOpen, setCreateOpen] = useState(false);
  const [editId, setEditId] = useState<string | null>(null);

  const roomsQuery = useQuery({ queryKey: ['rooms', 'for-sessions'], queryFn: Rooms.list });
  const rooms = roomsQuery.data ?? [];

  const sessionOptionsQuery = useQuery({ queryKey: ['sessions', 'options'], queryFn: Sessions.options });
  const bandChoices = sessionOptionsQuery.data?.bands ?? [];

  const roomNamesById = useMemo(() => {
    const map = new Map<string, string>();
    rooms.forEach(room => {
      map.set(room.roomId, room.rName);
    });
    return map;
  }, [rooms]);

  const sessionsQuery = useQuery<Page<SessionDTO>>({
    queryKey: ['sessions', page, pageSize],
    queryFn: () => Sessions.list({ page: page + 1, pageSize }),
  });

  const sessions = sessionsQuery.data ?? { items: [], page: 1, pageSize, total: 0 };
  const rows: SessionDTO[] = sessions.items;

  return (
    <Stack spacing={2}>
      <Stack direction="row" alignItems="center" justifyContent="space-between">
        <Typography variant="h5">Sesiones</Typography>
        <Button variant="contained" onClick={() => setCreateOpen(true)}>Nueva sesión</Button>
      </Stack>
      <Paper variant="outlined">
        <TableContainer>
          <Table size="small">
            <TableHead>
              <TableRow>
                <TableCell>Horario</TableCell>
                <TableCell>Servicio</TableCell>
                <TableCell>Booking</TableCell>
                <TableCell>Ingeniero</TableCell>
                <TableCell>Salas</TableCell>
                <TableCell>Estado</TableCell>
                <TableCell width={160}>Acciones</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {rows.map((session) => (
                <TableRow key={session.sessionId} hover>
                  <TableCell>{formatDateRange(session.sStartAt, session.sEndAt)}</TableCell>
                  <TableCell>{session.sService ?? '—'}</TableCell>
                  <TableCell>{session.sBookingRef ?? '—'}</TableCell>
                  <TableCell>{session.sEngineerRef ?? '—'}</TableCell>
                  <TableCell>
                    {session.sRoomIds && session.sRoomIds.length > 0
                      ? session.sRoomIds.map((id) => roomNamesById.get(id) ?? id).join(', ')
                      : '—'}
                  </TableCell>
                  <TableCell>
                    <StatusChip value={session.sStatus} />
                  </TableCell>
                  <TableCell>
                    <Button size="small" variant="outlined" onClick={() => setEditId(session.sessionId)}>
                      Editar
                    </Button>
                  </TableCell>
                </TableRow>
              ))}
              {rows.length === 0 && (
                <TableRow>
                  <TableCell colSpan={7}>
                    <Typography variant="body2" color="text.secondary" align="center" sx={{ py: 2 }}>
                      {sessionsQuery.isLoading ? 'Cargando sesiones…' : 'No hay sesiones registradas todavía.'}
                    </Typography>
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </TableContainer>
        <TablePagination
          component="div"
          count={sessions.total}
          page={page}
          onPageChange={(_event, value) => setPage(value)}
          rowsPerPage={pageSize}
          onRowsPerPageChange={(event) => {
            setPageSize(parseInt(event.target.value, 10));
            setPage(0);
          }}
          rowsPerPageOptions={[10, 25, 50]}
        />
      </Paper>

      <CreateSessionDialog
        open={createOpen}
        onClose={() => setCreateOpen(false)}
        rooms={rooms}
        bandChoices={bandChoices}
        bandChoicesLoading={sessionOptionsQuery.isLoading}
      />
      <EditSessionDialog
        open={!!editId}
        onClose={() => setEditId(null)}
        rooms={rooms}
        bandChoices={bandChoices}
        bandChoicesLoading={sessionOptionsQuery.isLoading}
        sessionId={editId}
      />
    </Stack>
  );
}
