import { useEffect, useMemo, useState, type ChangeEvent, type FormEvent, type KeyboardEvent } from 'react';
import { useMutation, useQuery, useQueryClient, type UseQueryResult } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControl,
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
  type ChipProps,
  type SelectChangeEvent,
} from '@mui/material';
import RefreshIcon from '@mui/icons-material/Refresh';
import AddIcon from '@mui/icons-material/Add';
import OpenInNewIcon from '@mui/icons-material/OpenInNew';
import { DateTime } from 'luxon';
import { useNavigate } from 'react-router-dom';
import { Bookings, type BookingUpdatePayload } from '../api/bookings';
import { Parties } from '../api/parties';
import type { BookingDTO, BookingResourceDTO, PartyDTO } from '../api/types';

type StatusValue = 'Tentative' | 'Confirmed' | 'InProgress' | 'Completed' | 'Cancelled' | 'NoShow';

const STATUS_VARIANTS: { value: StatusValue; label: string; color: ChipProps['color'] }[] = [
  { value: 'Tentative', label: 'Tentativa', color: 'default' },
  { value: 'Confirmed', label: 'Confirmada', color: 'primary' },
  { value: 'InProgress', label: 'En preparación', color: 'info' },
  { value: 'Completed', label: 'Completada', color: 'success' },
  { value: 'Cancelled', label: 'Cancelada', color: 'default' },
  { value: 'NoShow', label: 'No asistencia', color: 'warning' },
];
const STATUS_VALUES: StatusValue[] = STATUS_VARIANTS.map((item) => item.value);
const isStatusValue = (value: string): value is StatusValue =>
  STATUS_VALUES.some((status) => status === value);

const normalizeStatusValue = (value: string | null | undefined): StatusValue => {
  const trimmed = value?.trim() ?? '';
  return isStatusValue(trimmed) ? trimmed : 'Tentative';
};

const STATUS_LOOKUP = STATUS_VARIANTS.reduce<Record<string, { label: string; color: ChipProps['color'] }>>((acc, item) => {
  acc[item.value] = { label: item.label, color: item.color };
  return acc;
}, {});

const TZ = import.meta.env?.['VITE_TZ'] ?? 'America/Guayaquil';
const ROWS_PER_PAGE_OPTIONS = [5, 10, 25] as const;
const ORDERS_PAGE_OVERVIEW_SUMMARY =
  'Revisa horario, servicio, booking, recursos y estado desde una sola tabla.';

const parseRowsPerPage = (value: string, fallback = 10): number => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed)) return fallback;
  return ROWS_PER_PAGE_OPTIONS.some((option) => option === parsed) ? parsed : fallback;
};

const formatDisplayedRowsLabel = ({
  from,
  to,
  count,
}: {
  from: number;
  to: number;
  count: number;
}) => `${from}-${to} de ${count === -1 ? `más de ${to}` : count}`;

function formatScheduleRange(start: string, end: string) {
  const s = DateTime.fromISO(start, { zone: TZ });
  const e = DateTime.fromISO(end, { zone: TZ });
  if (!s.isValid || !e.isValid) return '—';
  const datePart = s.toLocaleString(DateTime.DATE_MED_WITH_WEEKDAY);
  const timeRange = `${s.toLocaleString(DateTime.TIME_SIMPLE)} → ${e.toLocaleString(DateTime.TIME_SIMPLE)}`;
  return `${datePart}, ${timeRange}`;
}

function filterResources(resources: BookingResourceDTO[] | undefined, predicate: (role: string) => boolean) {
  if (!resources) return [];
  return resources
    .filter((resource) => predicate(resource.brRole?.toLowerCase() ?? ''))
    .map((resource) => resource.brRoomName);
}

function dedupeStrings(values: (string | null | undefined)[]) {
  const seen = new Set<string>();
  const result: string[] = [];
  values.forEach((value) => {
    if (!value) return;
    const trimmed = value.trim();
    const normalized = trimmed.toLocaleLowerCase();
    if (!trimmed || seen.has(normalized)) return;
    seen.add(normalized);
    result.push(trimmed);
  });
  return result;
}

const normalizeComparableString = (value: string | null | undefined) => value?.trim().toLocaleLowerCase() ?? '';
const getSharedSummaryValue = (values: readonly string[]) => {
  if (values.length < 2) return '';

  const trimmedValues = values.map((value) => value.trim());
  const [firstValue] = trimmedValues;
  if (!firstValue || firstValue === '—') return '';

  const firstComparableValue = normalizeComparableString(firstValue);
  return trimmedValues.every((value) => normalizeComparableString(value) === firstComparableValue)
    ? firstValue
    : '';
};

function formatNaturalLanguageList(values: readonly string[], conjunction: 'y' | 'o') {
  if (values.length === 0) return '';
  if (values.length === 1) return values[0] ?? '';
  if (values.length === 2) return `${values[0]} ${conjunction} ${values[1]}`;
  return `${values.slice(0, -1).join(', ')} ${conjunction} ${values[values.length - 1]}`;
}

function buildCombinedSharedContextSummary(
  contexts: ReadonlyArray<{
    value: string;
    singularLabel: string;
    pluralLabel: string;
  }>,
) {
  const visibleContexts = contexts.filter((context) => context.value !== '');

  if (visibleContexts.length < 2) return '';

  const summaryDetails = formatNaturalLanguageList(
    visibleContexts.map((context) => `${context.singularLabel}: ${context.value}`),
    'y',
  );
  const hiddenColumns = formatNaturalLanguageList(
    visibleContexts.map((context) => context.pluralLabel),
    'o',
  );

  return `Mostrando ${summaryDetails}. Las columnas volverán cuando ya no coincidan ${hiddenColumns}.`;
}

const hasDisplayValue = (value: string) => {
  const trimmed = value.trim();
  return trimmed !== '' && trimmed !== '—';
};

function buildBookingSecondarySummary({
  bookingPrimary,
  partyNames,
  serviceOrderId,
}: {
  bookingPrimary: string;
  partyNames: string[];
  serviceOrderId?: number | null;
}) {
  const normalizedPrimary = normalizeComparableString(bookingPrimary);
  const secondaryParts = partyNames.filter((name) => normalizeComparableString(name) !== normalizedPrimary);

  if (serviceOrderId) {
    secondaryParts.push(`SO #${serviceOrderId}`);
  }

  return secondaryParts.join(' · ');
}

interface OrderRow {
  bookingId: number;
  schedule: string;
  service: string;
  isRecording: boolean;
  bookingPrimary: string;
  bookingSecondary: string | null;
  engineers: string;
  rooms: string;
  status: string;
}

export default function OrdersPage() {
  const navigate = useNavigate();
  const qc = useQueryClient();
  const [page, setPage] = useState(0);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [selectedBookingId, setSelectedBookingId] = useState<number | null>(null);

  const bookingsQuery: UseQueryResult<BookingDTO[], Error> = useQuery<BookingDTO[], Error>({
    queryKey: ['bookings'],
    queryFn: () => Bookings.list(),
  });
  const partiesQuery: UseQueryResult<PartyDTO[], Error> = useQuery<PartyDTO[], Error>({
    queryKey: ['parties'],
    queryFn: Parties.list,
  });
  const bookings = useMemo<BookingDTO[]>(() => bookingsQuery.data ?? [], [bookingsQuery.data]);
  const parties = useMemo<PartyDTO[]>(() => partiesQuery.data ?? [], [partiesQuery.data]);

  const partyLookup = useMemo(() => {
    const map = new Map<number, PartyDTO>();
    parties.forEach((party) => map.set(party.partyId, party));
    return map;
  }, [parties]);

  const selectedBooking = useMemo(
    () => bookings.find((booking) => booking.bookingId === selectedBookingId) ?? null,
    [bookings, selectedBookingId],
  );

  const rows = useMemo<OrderRow[]>(() => {
    return bookings.map((booking) => {
      const engineers = filterResources(booking.resources, (role) => role.includes('engineer') || role.includes('ing'));
      const rooms = filterResources(booking.resources, (role) => role.includes('room') || role.includes('sala'));
      const party = booking.partyId ? partyLookup.get(booking.partyId) : undefined;
      const partyNames = dedupeStrings([booking.customerName, booking.partyDisplayName, party?.displayName]);
      const serviceTitle = booking.serviceType ?? booking.title ?? '—';
      const isRecording = serviceTitle.toLowerCase().includes('grab');
      const bookingPrimary =
        booking.serviceOrderTitle ??
        booking.customerName ??
        booking.partyDisplayName ??
        party?.displayName ??
        `Booking #${booking.bookingId}`;
      const bookingSecondarySummary = buildBookingSecondarySummary({
        bookingPrimary,
        partyNames,
        serviceOrderId: booking.serviceOrderId,
      });

      return {
        bookingId: booking.bookingId,
        schedule: formatScheduleRange(booking.startsAt ?? '', booking.endsAt ?? ''),
        service: serviceTitle,
        isRecording,
        bookingPrimary,
        bookingSecondary: bookingSecondarySummary.length > 0 ? bookingSecondarySummary : null,
        engineers: engineers.length ? engineers.join(', ') : '—',
        rooms: rooms.length ? rooms.join(', ') : '—',
        status: booking.status,
      };
    });
  }, [bookings, partyLookup]);

  const totalRows = rows.length;
  const maxPage = Math.max(0, Math.ceil(totalRows / rowsPerPage) - 1);
  const showInitialLoadingState = bookingsQuery.isLoading && bookingsQuery.data == null;
  const showFirstSessionEmptyState = !bookingsQuery.isLoading && !bookingsQuery.error && totalRows === 0;
  const singleRow = !bookingsQuery.isLoading && !bookingsQuery.error && totalRows === 1
    ? (rows[0] ?? null)
    : null;
  const showSingleSessionSummary = singleRow != null;
  const showRefreshAction = Boolean(bookingsQuery.error) || totalRows > 0;

  useEffect(() => {
    if (page > maxPage) {
      setPage(maxPage);
    }
  }, [page, maxPage]);

  const paginatedRows = useMemo(() => {
    const start = page * rowsPerPage;
    const end = start + rowsPerPage;
    return rows.slice(start, end);
  }, [rows, page, rowsPerPage]);
  const showPagination = totalRows > rowsPerPage;
  const showLiveSessionsColumn = paginatedRows.some((row) => row.isRecording);
  const sharedServiceSummary = useMemo(
    () => getSharedSummaryValue(rows.map((row) => row.service)),
    [rows],
  );
  const showServiceColumn = sharedServiceSummary === '';
  const sharedEngineerSummary = useMemo(
    () => getSharedSummaryValue(rows.map((row) => row.engineers)),
    [rows],
  );
  const showEngineerColumn = sharedEngineerSummary === '';
  const sharedRoomsSummary = useMemo(
    () => getSharedSummaryValue(rows.map((row) => row.rooms)),
    [rows],
  );
  const showRoomsColumn = sharedRoomsSummary === '';
  const combinedSharedContextSummary = useMemo(
    () =>
      buildCombinedSharedContextSummary([
        {
          pluralLabel: 'servicios',
          singularLabel: 'un solo servicio',
          value: sharedServiceSummary,
        },
        {
          pluralLabel: 'ingenieros',
          singularLabel: 'un solo ingeniero',
          value: sharedEngineerSummary,
        },
        {
          pluralLabel: 'salas',
          singularLabel: 'una sola sala',
          value: sharedRoomsSummary,
        },
      ]),
    [sharedEngineerSummary, sharedRoomsSummary, sharedServiceSummary],
  );
  const visibleTableColumnCount = 3
    + (showServiceColumn ? 1 : 0)
    + (showEngineerColumn ? 1 : 0)
    + (showRoomsColumn ? 1 : 0)
    + (showLiveSessionsColumn ? 1 : 0);
  const rowActionSummary = rows.some((row) => row.isRecording)
    ? 'Haz clic en una fila para editar la sesión. Live Sessions aparece solo en sesiones de grabación.'
    : 'Haz clic en una fila para editar la sesión y revisar horario, servicio, recursos y estado.';
  const pageSummary = totalRows === 0
    ? ORDERS_PAGE_OVERVIEW_SUMMARY
    : showSingleSessionSummary
      ? 'Revisa la primera sesión desde un resumen simple. La tabla volverá cuando exista una segunda.'
      : rowActionSummary;

  const handleChangePage = (_: unknown, newPage: number) => setPage(newPage);
  const handleChangeRowsPerPage = (event: React.ChangeEvent<HTMLInputElement>) => {
    setRowsPerPage(parseRowsPerPage(event.target.value, rowsPerPage));
    setPage(0);
  };

  const handleCreateSession = () => {
    navigate('/estudio/calendario');
  };

  const updateMutation = useMutation<BookingDTO, Error, { id: number; payload: BookingUpdatePayload }>({
    mutationFn: ({ id, payload }: { id: number; payload: BookingUpdatePayload }) => Bookings.update(id, payload),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['bookings'] });
      setSelectedBookingId(null);
    },
  });

  const handleEditClick = (bookingId: number) => {
    setSelectedBookingId(bookingId);
  };

  const handleDialogClose = () => {
    setSelectedBookingId(null);
    updateMutation.reset();
  };

  const handleSave = async (payload: BookingUpdatePayload) => {
    if (!selectedBooking) return;
    await updateMutation.mutateAsync({ id: selectedBooking.bookingId, payload });
  };

  const mutationError = updateMutation.error?.message ?? null;

  const renderStatus = (status: string) => {
    const trimmedStatus = status.trim();
    const fallbackLabel = trimmedStatus === '' ? 'Desconocido' : trimmedStatus;
    const config = STATUS_LOOKUP[status] ?? { label: fallbackLabel, color: 'default' as ChipProps['color'] };
    return <Chip label={config.label} color={config.color} size="small" />;
  };

  const handleRowKeyDown = (event: KeyboardEvent<HTMLTableRowElement>, bookingId: number) => {
    if (event.target !== event.currentTarget) return;
    if (event.key !== 'Enter' && event.key !== ' ') return;
    event.preventDefault();
    handleEditClick(bookingId);
  };

  return (
    <Stack gap={3}>
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" gap={2} alignItems={{ md: 'center' }}>
        <Box>
          <Typography variant="h5" fontWeight={600}>Sesiones</Typography>
          <Typography variant="body2" color="text.secondary">
            {pageSummary}
          </Typography>
        </Box>
        <Stack direction="row" gap={1} justifyContent={{ xs: 'flex-start', md: 'flex-end' }}>
          {showRefreshAction && (
            <Tooltip title="Actualizar lista">
              <span>
                <IconButton
                  onClick={() => {
                    void bookingsQuery.refetch();
                  }}
                  disabled={bookingsQuery.isFetching}
                  color="primary"
                  aria-label="Actualizar lista de sesiones"
                >
                  <RefreshIcon />
                </IconButton>
              </span>
            </Tooltip>
          )}
          <Button variant="contained" startIcon={<AddIcon />} onClick={handleCreateSession}>
            Nueva sesión
          </Button>
        </Stack>
      </Stack>

      {bookingsQuery.error && (
        <Alert severity="error">{bookingsQuery.error.message}</Alert>
      )}

      <Paper variant="outlined">
        {showInitialLoadingState ? (
          <Stack spacing={1} sx={{ p: 3 }}>
            <Typography variant="h6" fontWeight={700}>
              Cargando sesiones…
            </Typography>
            <Typography variant="body2" color="text.secondary">
              La tabla aparecerá cuando termine esta primera carga para que puedas comparar horario, servicio, booking,
              recursos y estado desde una sola vista.
            </Typography>
          </Stack>
        ) : showFirstSessionEmptyState ? (
          <Stack spacing={1} sx={{ p: 3 }}>
            <Typography variant="h6" fontWeight={700}>
              Primeras sesiones
            </Typography>
            <Typography variant="body2" color="text.secondary">
              Todavía no hay sesiones registradas. Usa Nueva sesión para cargar la primera y volver a esta vista cuando
              necesites revisar horario, servicio, booking, recursos y estado en una sola tabla.
            </Typography>
            <Typography variant="body2" color="text.secondary">
              La tabla y la paginación aparecerán cuando exista al menos una sesión para comparar.
            </Typography>
          </Stack>
        ) : showSingleSessionSummary && singleRow ? (
          <Stack spacing={2} sx={{ p: 3 }}>
            <Stack spacing={0.75}>
              <Typography variant="h6" fontWeight={700}>
                Primera sesión registrada
              </Typography>
              <Typography variant="body2" color="text.secondary">
                Revísala aquí sin tabla ni paginación. Cuando exista la segunda, volverá la vista comparativa para
                revisar horario, servicio, booking, recursos y estado lado a lado.
              </Typography>
            </Stack>
            <Stack
              spacing={0.75}
              sx={{
                border: '1px solid',
                borderColor: 'divider',
                borderRadius: 2,
                px: 2,
                py: 1.5,
                maxWidth: 720,
              }}
            >
              <Typography variant="body2">
                <Box component="span" sx={{ fontWeight: 600 }}>Horario:</Box> {singleRow.schedule}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>Servicio:</Box> {singleRow.service}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>Booking:</Box> {singleRow.bookingPrimary}
              </Typography>
              {singleRow.bookingSecondary && (
                <Typography variant="body2" color="text.secondary">
                  <Box component="span" sx={{ fontWeight: 600 }}>Detalle:</Box> {singleRow.bookingSecondary}
                </Typography>
              )}
              {hasDisplayValue(singleRow.engineers) && (
                <Typography variant="body2" color="text.secondary">
                  <Box component="span" sx={{ fontWeight: 600 }}>Ingeniero:</Box> {singleRow.engineers}
                </Typography>
              )}
              {hasDisplayValue(singleRow.rooms) && (
                <Typography variant="body2" color="text.secondary">
                  <Box component="span" sx={{ fontWeight: 600 }}>Salas:</Box> {singleRow.rooms}
                </Typography>
              )}
              <Stack direction="row" spacing={1} alignItems="center">
                <Typography variant="body2" color="text.secondary">
                  <Box component="span" sx={{ fontWeight: 600 }}>Estado:</Box>
                </Typography>
                {renderStatus(singleRow.status ?? '')}
              </Stack>
            </Stack>
            <Button
              variant="outlined"
              onClick={() => handleEditClick(singleRow.bookingId)}
              sx={{ alignSelf: 'flex-start' }}
            >
              Editar sesión
            </Button>
          </Stack>
        ) : (
          <>
            {(combinedSharedContextSummary || sharedServiceSummary || sharedEngineerSummary || sharedRoomsSummary) && (
              <Stack spacing={0.5} sx={{ px: 3, pt: 2 }}>
                {combinedSharedContextSummary ? (
                  <Typography variant="caption" color="text.secondary">
                    {combinedSharedContextSummary}
                  </Typography>
                ) : (
                  <>
                    {sharedServiceSummary && (
                      <Typography variant="caption" color="text.secondary">
                        {`Mostrando un solo servicio: ${sharedServiceSummary}. La columna volverá cuando esta vista mezcle servicios distintos.`}
                      </Typography>
                    )}
                    {sharedEngineerSummary && (
                      <Typography variant="caption" color="text.secondary">
                        {`Mostrando un solo ingeniero: ${sharedEngineerSummary}. La columna volverá cuando esta vista mezcle ingenieros distintos.`}
                      </Typography>
                    )}
                    {sharedRoomsSummary && (
                      <Typography variant="caption" color="text.secondary">
                        {`Mostrando una sola sala: ${sharedRoomsSummary}. La columna volverá cuando esta vista mezcle salas distintas.`}
                      </Typography>
                    )}
                  </>
                )}
              </Stack>
            )}
            <TableContainer>
              <Table size="small">
                <TableHead>
                  <TableRow>
                    <TableCell>Horario</TableCell>
                    {showServiceColumn && <TableCell>Servicio</TableCell>}
                    <TableCell>Booking</TableCell>
                    {showEngineerColumn && <TableCell>Ingeniero</TableCell>}
                    {showRoomsColumn && <TableCell>Salas</TableCell>}
                    <TableCell>Estado</TableCell>
                    {showLiveSessionsColumn && <TableCell align="right">Live Sessions</TableCell>}
                  </TableRow>
                </TableHead>
                <TableBody>
                  {bookingsQuery.isLoading && (
                    <TableRow>
                      <TableCell colSpan={visibleTableColumnCount} align="center">
                        Cargando sesiones…
                      </TableCell>
                    </TableRow>
                  )}

                  {!bookingsQuery.isLoading && paginatedRows.map((row) => (
                    <TableRow
                      hover
                      key={row.bookingId}
                      onClick={() => handleEditClick(row.bookingId)}
                      onKeyDown={(event) => handleRowKeyDown(event, row.bookingId)}
                      tabIndex={0}
                      aria-label={`Editar sesión ${row.bookingId}`}
                      data-testid={`orders-row-${row.bookingId}`}
                      sx={{ cursor: 'pointer' }}
                    >
                      <TableCell sx={{ minWidth: 240 }}>
                        <Typography variant="body2" fontWeight={600}>
                          #{row.bookingId}
                        </Typography>
                        <Typography variant="body2" color="text.secondary">
                          {row.schedule}
                        </Typography>
                      </TableCell>
                      {showServiceColumn && <TableCell>{row.service}</TableCell>}
                      <TableCell sx={{ minWidth: 220 }}>
                        <Typography variant="body2" fontWeight={600}>{row.bookingPrimary}</Typography>
                        {row.bookingSecondary && (
                          <Typography variant="body2" color="text.secondary">{row.bookingSecondary}</Typography>
                        )}
                      </TableCell>
                      {showEngineerColumn && <TableCell>{row.engineers}</TableCell>}
                      {showRoomsColumn && <TableCell>{row.rooms}</TableCell>}
                      <TableCell>{renderStatus(row.status ?? '')}</TableCell>
                      {showLiveSessionsColumn && (
                        <TableCell align="right">
                          {row.isRecording ? (
                            <Tooltip title="Abrir Live Sessions">
                              <IconButton
                                onClick={(event) => {
                                  event.stopPropagation();
                                  navigate('/estudio/live-sessions');
                                }}
                                aria-label={`Abrir Live Sessions para sesión ${row.bookingId}`}
                              >
                                <OpenInNewIcon fontSize="small" />
                              </IconButton>
                            </Tooltip>
                          ) : null}
                        </TableCell>
                      )}
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </TableContainer>
            {showPagination ? (
              <TablePagination
                component="div"
                count={totalRows}
                page={page}
                onPageChange={handleChangePage}
                rowsPerPage={rowsPerPage}
                onRowsPerPageChange={handleChangeRowsPerPage}
                rowsPerPageOptions={ROWS_PER_PAGE_OPTIONS}
                labelRowsPerPage="Filas por página"
                labelDisplayedRows={formatDisplayedRowsLabel}
              />
            ) : null}
          </>
        )}
      </Paper>

      <OrderEditDialog
        booking={selectedBooking}
        open={Boolean(selectedBooking)}
        onClose={handleDialogClose}
        onSubmit={handleSave}
        saving={updateMutation.isPending}
        errorMessage={mutationError}
      />
    </Stack>
  );
}

interface OrderEditDialogProps {
  booking: BookingDTO | null;
  open: boolean;
  onClose: () => void;
  onSubmit: (payload: BookingUpdatePayload) => Promise<void>;
  saving: boolean;
  errorMessage: string | null;
}

function OrderEditDialog({ booking, open, onClose, onSubmit, saving, errorMessage }: OrderEditDialogProps) {
  const [form, setForm] = useState({
    title: '',
    serviceType: '',
    status: normalizeStatusValue('Tentative'),
    notes: '',
  });

  useEffect(() => {
    if (!booking) return;
    setForm({
      title: booking.title ?? '',
      serviceType: booking.serviceType ?? '',
      status: normalizeStatusValue(booking.status),
      notes: booking.notes ?? '',
    });
  }, [booking]);

  if (!booking) {
    return null;
  }

  const handleFieldChange = (field: 'title' | 'serviceType' | 'notes') =>
    (event: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
      setForm((prev) => ({ ...prev, [field]: event.target.value }));
    };

  const handleStatusChange = (event: SelectChangeEvent) => {
    setForm((prev) => ({ ...prev, status: normalizeStatusValue(event.target.value) }));
  };

  const handleSubmit = async (event: FormEvent) => {
    event.preventDefault();
    const payload: BookingUpdatePayload = {
      ubTitle: form.title,
      ubServiceType: form.serviceType,
      ubStatus: form.status,
      ubNotes: form.notes,
    };
    try {
      await onSubmit(payload);
    } catch {
      // The mutation hook surfaces the error; keep dialog open.
    }
  };

  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="sm">
      <form
        onSubmit={(event) => {
          void handleSubmit(event);
        }}
      >
        <DialogTitle>Editar sesión #{booking.bookingId}</DialogTitle>
        <DialogContent>
          <Stack gap={2} sx={{ mt: 1 }}>
            <TextField
              label="Título"
              value={form.title}
              onChange={handleFieldChange('title')}
              required
              disabled={saving}
            />
            <TextField
              label="Servicio"
              value={form.serviceType}
              onChange={handleFieldChange('serviceType')}
              disabled={saving}
            />
            <FormControl fullWidth disabled={saving}>
              <InputLabel id="booking-status-label">Estado</InputLabel>
              <Select
                labelId="booking-status-label"
                label="Estado"
                value={form.status}
                onChange={handleStatusChange}
              >
                {STATUS_VARIANTS.map((option) => (
                  <MenuItem key={option.value} value={option.value}>
                    {option.label}
                  </MenuItem>
                ))}
              </Select>
            </FormControl>
            <TextField
              label="Notas internas"
              value={form.notes}
              onChange={handleFieldChange('notes')}
              multiline
              minRows={3}
              disabled={saving}
            />
            {errorMessage && <Alert severity="error">{errorMessage}</Alert>}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={onClose} disabled={saving}>Cancelar</Button>
          <Button type="submit" variant="contained" disabled={saving}>
            Guardar cambios
          </Button>
        </DialogActions>
      </form>
    </Dialog>
  );
}
