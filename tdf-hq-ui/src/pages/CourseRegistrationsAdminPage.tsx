import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  CircularProgress,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Divider,
  Grid,
  IconButton,
  MenuItem,
  Paper,
  Stack,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import RefreshIcon from '@mui/icons-material/Refresh';
import DoneIcon from '@mui/icons-material/Done';
import CancelIcon from '@mui/icons-material/Cancel';
import PendingIcon from '@mui/icons-material/HourglassBottom';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Courses,
  type CourseEmailEventDTO,
  type CourseRegistrationDTO,
} from '../api/courses';
import { useSearchParams } from 'react-router-dom';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';

type StatusFilter = 'all' | 'pending_payment' | 'paid' | 'cancelled';
const statusFilters: StatusFilter[] = ['all', 'pending_payment', 'paid', 'cancelled'];

const parseStatusFilter = (value: string | null): StatusFilter =>
  value && statusFilters.includes(value as StatusFilter) ? (value as StatusFilter) : 'all';

const parsePositiveLimit = (value: string | null, fallback = 200): number => {
  const trimmed = value?.trim() ?? '';
  if (!/^\d+$/.test(trimmed)) return fallback;
  const parsed = Number(trimmed);
  return Number.isInteger(parsed) && parsed > 0 ? parsed : fallback;
};

const formatDate = (iso: string) => new Date(iso).toLocaleString();

const statusChip = (status: string) => {
  const normalized = status.toLowerCase();
  if (normalized === 'paid') return <Chip label="Paid" color="success" size="small" />;
  if (normalized === 'cancelled') return <Chip label="Cancelled" color="error" size="small" />;
  return <Chip label="Pending payment" color="warning" size="small" />;
};

const eventStatusColor = (
  status: string,
): 'default' | 'success' | 'warning' | 'error' | 'info' => {
  const normalized = status.toLowerCase();
  if (normalized === 'sent') return 'success';
  if (normalized === 'failed') return 'error';
  if (normalized === 'skipped') return 'warning';
  if (normalized === 'queued') return 'info';
  return 'default';
};

const eventTypeLabel = (eventType: string) =>
  eventType
    .trim()
    .toLowerCase()
    .replace(/_/g, ' ')
    .replace(/\b\w/g, (m) => m.toUpperCase());

const actionButtons = (
  reg: CourseRegistrationDTO,
  onUpdate: (status: 'pending_payment' | 'paid' | 'cancelled') => void,
  isUpdating: boolean,
) => {
  return (
    <Stack direction="row" spacing={1}>
      <Tooltip title="Marcar pagado">
        <span>
          <IconButton
            size="small"
            color="success"
            disabled={isUpdating || reg.crStatus === 'paid'}
            onClick={() => onUpdate('paid')}
          >
            <DoneIcon fontSize="small" />
          </IconButton>
        </span>
      </Tooltip>
      <Tooltip title="Marcar pendiente">
        <span>
          <IconButton
            size="small"
            color="warning"
            disabled={isUpdating || reg.crStatus === 'pending_payment'}
            onClick={() => onUpdate('pending_payment')}
          >
            <PendingIcon fontSize="small" />
          </IconButton>
        </span>
      </Tooltip>
      <Tooltip title="Cancelar">
        <span>
          <IconButton
            size="small"
            color="error"
            disabled={isUpdating || reg.crStatus === 'cancelled'}
            onClick={() => onUpdate('cancelled')}
          >
            <CancelIcon fontSize="small" />
          </IconButton>
        </span>
      </Tooltip>
    </Stack>
  );
};

export default function CourseRegistrationsAdminPage() {
  const qc = useQueryClient();
  const [searchParams, setSearchParams] = useSearchParams();
  const initialSlug = searchParams.get('slug') ?? 'produccion-musical-feb-2026';
  const initialStatus = parseStatusFilter(searchParams.get('status'));
  const initialLimit = parsePositiveLimit(searchParams.get('limit'));
  const [slug, setSlug] = useState(initialSlug);
  const [status, setStatus] = useState<StatusFilter>(initialStatus);
  const [limit, setLimit] = useState(initialLimit);
  const [copyMessage, setCopyMessage] = useState<string | null>(null);
  const [selectedRegForEmails, setSelectedRegForEmails] =
    useState<CourseRegistrationDTO | null>(null);

  const queryKey = useMemo(
    () => ['admin', 'course-registrations', { slug, status, limit }],
    [slug, status, limit],
  );

  const regsQuery = useQuery({
    queryKey,
    queryFn: () =>
      Courses.listRegistrations({
        slug: slug.trim() || undefined,
        status: status === 'all' ? undefined : status,
        limit,
      }),
  });

  const selectedRegistrationId = selectedRegForEmails?.crId;
  const emailEventsQuery = useQuery<CourseEmailEventDTO[]>({
    queryKey: ['admin', 'course-registration-email-events', selectedRegistrationId],
    enabled: selectedRegistrationId != null,
    queryFn: () => {
      if (selectedRegistrationId == null) return Promise.resolve([]);
      return Courses.listRegistrationEmails(selectedRegistrationId, 200);
    },
    staleTime: 30_000,
  });

  const statusCounts = useMemo(() => {
    const base = { total: 0, pending_payment: 0, paid: 0, cancelled: 0 };
    if (!regsQuery.data) return base;
    return regsQuery.data.reduce(
      (acc, reg) => {
        acc.total += 1;
        if (reg.crStatus === 'pending_payment' || reg.crStatus === 'paid' || reg.crStatus === 'cancelled') {
          acc[reg.crStatus] += 1;
        }
        return acc;
      },
      { ...base },
    );
  }, [regsQuery.data]);

  const updateStatusMutation = useMutation({
    mutationFn: (args: { id: number; newStatus: Exclude<StatusFilter, 'all'> }) =>
      Courses.updateStatus(slug, args.id, { status: args.newStatus }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['admin', 'course-registrations'] });
    },
  });

  const handleRefresh = () => {
    void qc.invalidateQueries({ queryKey: ['admin', 'course-registrations'] });
  };

  const handleCopyCsv = async () => {
    if (!regsQuery.data?.length) return;
    const header = ['id', 'slug', 'nombre', 'email', 'estado', 'creado'];
    const rows = regsQuery.data.map((reg) => [
      reg.crId,
      reg.crCourseSlug,
      reg.crFullName ?? '',
      reg.crEmail ?? '',
      reg.crStatus,
      reg.crCreatedAt,
    ]);
    const csv = [header, ...rows]
      .map((r) => r.map((cell) => `"${String(cell ?? '').replace(/"/g, '""')}"`).join(','))
      .join('\n');
    try {
      await navigator.clipboard.writeText(csv);
      setCopyMessage(`Copiado CSV (${rows.length} filas)`);
      setTimeout(() => setCopyMessage(null), 2000);
    } catch {
      setCopyMessage('No se pudo copiar el CSV');
      setTimeout(() => setCopyMessage(null), 2000);
    }
  };

  useEffect(() => {
    const params = new URLSearchParams();
    if (slug.trim()) params.set('slug', slug.trim());
    if (status !== 'all') params.set('status', status);
    if (limit && limit !== 200) params.set('limit', String(limit));
    setSearchParams(params, { replace: true });
  }, [slug, status, limit, setSearchParams]);

  return (
    <Stack spacing={3}>
      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Typography variant="h4" fontWeight={700}>
          Inscripciones de cursos
        </Typography>
        <Stack direction="row" spacing={1}>
          <Chip label={`Total: ${statusCounts.total}`} size="small" />
          <Chip label={`Pagadas: ${statusCounts.paid}`} size="small" color="success" variant="outlined" />
          <Chip label={`Pendientes: ${statusCounts.pending_payment}`} size="small" color="warning" variant="outlined" />
          <Chip label={`Canceladas: ${statusCounts.cancelled}`} size="small" color="error" variant="outlined" />
          <Tooltip title="Refrescar">
            <IconButton onClick={handleRefresh} disabled={regsQuery.isFetching}>
              <RefreshIcon />
            </IconButton>
          </Tooltip>
        </Stack>
      </Stack>
      <Paper sx={{ p: 3, borderRadius: 3 }}>
        <Grid container spacing={2}>
          <Grid item xs={12} md={4}>
            <TextField
              label="Slug"
              value={slug}
              onChange={(e) => setSlug(e.target.value)}
              fullWidth
              size="small"
            />
          </Grid>
          <Grid item xs={12} md={3}>
            <TextField
              select
              label="Estado"
              value={status}
              onChange={(e) => setStatus(parseStatusFilter(e.target.value))}
              fullWidth
              size="small"
            >
              <MenuItem value="all">Todos</MenuItem>
              <MenuItem value="pending_payment">Pending payment</MenuItem>
              <MenuItem value="paid">Paid</MenuItem>
              <MenuItem value="cancelled">Cancelled</MenuItem>
            </TextField>
          </Grid>
          <Grid item xs={12} md={2}>
            <TextField
              label="Límite"
              type="number"
              inputProps={{ min: 1 }}
              value={limit}
              onChange={(e) => setLimit(parsePositiveLimit(e.target.value, 100))}
              fullWidth
              size="small"
            />
          </Grid>
          <Grid item xs={12} md={3} display="flex" alignItems="center" justifyContent="flex-end">
            <Button variant="contained" onClick={handleRefresh} disabled={regsQuery.isFetching}>
              Aplicar filtros
            </Button>
          </Grid>
        </Grid>
        <Stack direction="row" spacing={1} alignItems="center" sx={{ mt: 2 }} flexWrap="wrap" useFlexGap>
          <Typography variant="caption" color="text.secondary">
            Leyenda de estados:
          </Typography>
          <Chip label="Paid" size="small" color="success" />
          <Chip label="Pending payment" size="small" color="warning" />
          <Chip label="Cancelled" size="small" color="error" />
          <Button
            size="small"
            startIcon={<ContentCopyIcon fontSize="small" />}
            onClick={() => void handleCopyCsv()}
            disabled={!regsQuery.data?.length}
          >
            Copiar CSV filtrado
          </Button>
          {copyMessage && (
            <Typography variant="caption" color="text.secondary">
              {copyMessage}
            </Typography>
          )}
        </Stack>
      </Paper>

      <Paper sx={{ p: 3, borderRadius: 3 }}>
        {regsQuery.isError && (
          <Typography color="error">
            No se pudieron cargar las inscripciones: {regsQuery.error instanceof Error ? regsQuery.error.message : 'Error'}
          </Typography>
        )}
        {regsQuery.isLoading && <Typography>Cargando inscripciones…</Typography>}
        {!regsQuery.isLoading && regsQuery.data?.length === 0 && (
          <Typography color="text.secondary">No hay inscripciones para estos filtros.</Typography>
        )}
        {regsQuery.data?.length ? (
          <Stack divider={<Divider flexItem />} spacing={2}>
            {regsQuery.data.map((reg) => (
              <Box key={reg.crId} sx={{ display: 'flex', gap: 2, alignItems: 'center', flexWrap: 'wrap' }}>
                <Box sx={{ minWidth: 220 }}>
                  <Typography variant="subtitle1" fontWeight={700}>
                    {reg.crFullName ?? 'Sin nombre'}
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    {reg.crEmail ?? 'Sin correo'}
                  </Typography>
                  {reg.crPhoneE164 && (
                    <Typography variant="body2" color="text.secondary">
                      {reg.crPhoneE164}
                    </Typography>
                  )}
                </Box>
                <Box sx={{ minWidth: 160 }}>
                  <Typography variant="body2">Slug: {reg.crCourseSlug}</Typography>
                  <Typography variant="body2" color="text.secondary">
                    Fuente: {reg.crSource}
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    Creado: {formatDate(reg.crCreatedAt)}
                  </Typography>
                </Box>
                <Box>{statusChip(reg.crStatus)}</Box>
                <Button
                  size="small"
                  variant="outlined"
                  onClick={() => setSelectedRegForEmails(reg)}
                >
                  Ver correos
                </Button>
                <Box sx={{ flexGrow: 1 }} />
                {actionButtons(reg, (newStatus) => updateStatusMutation.mutate({ id: reg.crId, newStatus }), updateStatusMutation.isPending)}
              </Box>
            ))}
          </Stack>
        ) : null}
      </Paper>

      <Dialog
        open={Boolean(selectedRegForEmails)}
        onClose={() => setSelectedRegForEmails(null)}
        fullWidth
        maxWidth="md"
      >
        <DialogTitle>Historial de correos</DialogTitle>
        <DialogContent dividers>
          {selectedRegForEmails && (
            <Stack spacing={1.5}>
              <Typography variant="subtitle2">
                {selectedRegForEmails.crFullName ?? 'Sin nombre'} · {selectedRegForEmails.crEmail ?? 'Sin correo'}
              </Typography>
              <Typography variant="caption" color="text.secondary">
                Historial persistente por inscripción (base de datos).
              </Typography>

              {emailEventsQuery.isLoading && (
                <Stack direction="row" spacing={1} alignItems="center">
                  <CircularProgress size={18} />
                  <Typography variant="body2">Cargando historial…</Typography>
                </Stack>
              )}

              {emailEventsQuery.isError && (
                <Alert severity="error">
                  No se pudo cargar el historial: {emailEventsQuery.error instanceof Error ? emailEventsQuery.error.message : 'Error'}
                </Alert>
              )}

              {!emailEventsQuery.isLoading && !emailEventsQuery.isError && (emailEventsQuery.data?.length ?? 0) === 0 && (
                <Alert severity="info">No hay correos registrados para esta inscripción.</Alert>
              )}

              {!emailEventsQuery.isLoading && !emailEventsQuery.isError && (emailEventsQuery.data?.length ?? 0) > 0 && (
                <Stack spacing={1}>
                  {(emailEventsQuery.data ?? []).map((entry) => (
                    <Paper key={entry.ceId} variant="outlined" sx={{ p: 1.5 }}>
                      <Stack direction="row" spacing={1} alignItems="center" sx={{ mb: 0.75 }} flexWrap="wrap" useFlexGap>
                        <Chip size="small" label={entry.ceStatus} color={eventStatusColor(entry.ceStatus)} />
                        <Chip size="small" label={eventTypeLabel(entry.ceEventType)} variant="outlined" />
                        <Typography variant="caption" color="text.secondary">
                          {formatDate(entry.ceCreatedAt)}
                        </Typography>
                      </Stack>
                      {entry.ceMessage && (
                        <Typography
                          variant="body2"
                          sx={{ fontFamily: 'monospace', whiteSpace: 'pre-wrap', wordBreak: 'break-word' }}
                        >
                          {entry.ceMessage}
                        </Typography>
                      )}
                    </Paper>
                  ))}
                </Stack>
              )}
            </Stack>
          )}
        </DialogContent>
        <DialogActions>
          <Button onClick={() => void emailEventsQuery.refetch()} disabled={emailEventsQuery.isFetching}>
            Actualizar
          </Button>
          <Button onClick={() => setSelectedRegForEmails(null)}>Cerrar</Button>
        </DialogActions>
      </Dialog>
    </Stack>
  );
}
