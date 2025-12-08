import { useEffect, useMemo, useState, type ChangeEvent, type FormEvent } from 'react';
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

const STATUS_LOOKUP = STATUS_VARIANTS.reduce<Record<string, { label: string; color: ChipProps['color'] }>>((acc, item) => {
  acc[item.value] = { label: item.label, color: item.color };
  return acc;
}, {});

const TZ = import.meta.env['VITE_TZ'] ?? 'America/Guayaquil';

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
    if (!trimmed || seen.has(trimmed)) return;
    seen.add(trimmed);
    result.push(trimmed);
  });
  return result;
}

export default function OrdersPage() {
  const navigate = useNavigate();
  const qc = useQueryClient();
  const [page, setPage] = useState(0);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [selectedBookingId, setSelectedBookingId] = useState<number | null>(null);

  const bookingsQuery: UseQueryResult<BookingDTO[], Error> = useQuery<BookingDTO[], Error>({
    queryKey: ['bookings'],
    queryFn: Bookings.list,
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

  const rows = useMemo(() => {
    return bookings.map((booking) => {
      const engineers = filterResources(booking.resources, (role) => role.includes('engineer') || role.includes('ing'));
      const rooms = filterResources(booking.resources, (role) => role.includes('room') || role.includes('sala'));
      const party = booking.partyId ? partyLookup.get(booking.partyId) : undefined;
      const partyNames = dedupeStrings([booking.customerName, booking.partyDisplayName, party?.displayName]);
      const bookingSecondaryParts = [...partyNames];
      if (booking.serviceOrderId) {
        bookingSecondaryParts.push(`SO #${booking.serviceOrderId}`);
      }
      const serviceTitle = booking.serviceType ?? booking.title ?? '—';
      const isRecording = serviceTitle.toLowerCase().includes('grab');
      const bookingPrimary =
        booking.serviceOrderTitle ??
        booking.customerName ??
        booking.partyDisplayName ??
        party?.displayName ??
        `Booking #${booking.bookingId}`;
      const bookingSecondaryJoined = bookingSecondaryParts.join(' · ');

      return {
        bookingId: booking.bookingId,
        schedule: formatScheduleRange(booking.startsAt, booking.endsAt),
        service: serviceTitle,
        isRecording,
        bookingPrimary,
        bookingSecondary: bookingSecondaryJoined.length > 0 ? bookingSecondaryJoined : null,
        engineers: engineers.length ? engineers.join(', ') : '—',
        rooms: rooms.length ? rooms.join(', ') : '—',
        status: booking.status,
      };
    });
  }, [bookings, partyLookup]);

  const totalRows = rows.length;
  const maxPage = Math.max(0, Math.ceil(totalRows / rowsPerPage) - 1);

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

  const handleChangePage = (_: unknown, newPage: number) => setPage(newPage);
  const handleChangeRowsPerPage = (event: React.ChangeEvent<HTMLInputElement>) => {
    setRowsPerPage(parseInt(event.target.value, 10));
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

  return (
    <Stack gap={3}>
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" gap={2} alignItems={{ md: 'center' }}>
        <Box>
          <Typography variant="h5" fontWeight={600}>Sesiones</Typography>
          <Typography variant="body2" color="text.secondary">
            Visualiza órdenes de estudio, su horario, recursos asignados y estado operacional.
          </Typography>
        </Box>
        <Stack direction="row" gap={1} justifyContent={{ xs: 'flex-start', md: 'flex-end' }}>
          <Tooltip title="Actualizar lista">
            <span>
              <IconButton
                onClick={() => {
                  void bookingsQuery.refetch();
                }}
                disabled={bookingsQuery.isFetching}
                color="primary"
              >
                <RefreshIcon />
              </IconButton>
            </span>
          </Tooltip>
          <Button variant="contained" startIcon={<AddIcon />} onClick={handleCreateSession}>
            Nueva sesión
          </Button>
        </Stack>
      </Stack>

      {bookingsQuery.error && (
        <Alert severity="error">{bookingsQuery.error.message}</Alert>
      )}

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
                <TableCell align="right">Acciones</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {bookingsQuery.isLoading && (
                <TableRow>
                  <TableCell colSpan={7} align="center">
                    Cargando sesiones…
                  </TableCell>
                </TableRow>
              )}

              {!bookingsQuery.isLoading && paginatedRows.length === 0 && (
                <TableRow>
                  <TableCell colSpan={7} align="center">
                    No hay sesiones registradas todavía.
                  </TableCell>
                </TableRow>
              )}

              {!bookingsQuery.isLoading && paginatedRows.map((row) => (
                <TableRow hover key={row.bookingId}>
                  <TableCell sx={{ minWidth: 240 }}>
                    <Typography variant="body2" fontWeight={600}>
                      #{row.bookingId}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      {row.schedule}
                    </Typography>
                  </TableCell>
                  <TableCell>{row.service}</TableCell>
                  <TableCell sx={{ minWidth: 220 }}>
                    <Typography variant="body2" fontWeight={600}>{row.bookingPrimary}</Typography>
                    {row.bookingSecondary && (
                      <Typography variant="body2" color="text.secondary">{row.bookingSecondary}</Typography>
                    )}
                  </TableCell>
                  <TableCell>{row.engineers}</TableCell>
                  <TableCell>{row.rooms}</TableCell>
                  <TableCell>{renderStatus(row.status)}</TableCell>
                  <TableCell align="right">
                    <Stack direction="row" spacing={1} justifyContent="flex-end">
                      {row.isRecording && (
                        <Button
                          variant="outlined"
                          size="small"
                          onClick={() => navigate('/estudio/live-sessions')}
                        >
                          Crear input list
                        </Button>
                      )}
                      <Button variant="text" size="small" onClick={() => handleEditClick(row.bookingId)}>
                        Editar
                      </Button>
                    </Stack>
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </TableContainer>
        <TablePagination
          component="div"
          count={totalRows}
          page={page}
          onPageChange={handleChangePage}
          rowsPerPage={rowsPerPage}
          onRowsPerPageChange={handleChangeRowsPerPage}
          rowsPerPageOptions={[5, 10, 25]}
          labelRowsPerPage="Rows per page"
        />
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
    status: 'Tentative' as StatusValue,
    notes: '',
  });

  useEffect(() => {
    if (!booking) return;
    setForm({
      title: booking.title ?? '',
      serviceType: booking.serviceType ?? '',
      status: (booking.status as StatusValue) ?? 'Tentative',
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
    setForm((prev) => ({ ...prev, status: event.target.value as StatusValue }));
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
