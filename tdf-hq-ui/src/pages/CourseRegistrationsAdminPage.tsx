import { useMemo, useState } from 'react';
import {
  Box,
  Button,
  Chip,
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
import { Courses, type CourseRegistrationDTO } from '../api/courses';

type StatusFilter = 'all' | 'pending_payment' | 'paid' | 'cancelled';

const formatDate = (iso: string) => new Date(iso).toLocaleString();

const statusChip = (status: string) => {
  const normalized = status.toLowerCase();
  if (normalized === 'paid') return <Chip label="Paid" color="success" size="small" />;
  if (normalized === 'cancelled') return <Chip label="Cancelled" color="error" size="small" />;
  return <Chip label="Pending payment" color="warning" size="small" />;
};

const actionButtons = (
  reg: CourseRegistrationDTO,
  onUpdate: (status: StatusFilter) => void,
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
  const [slug, setSlug] = useState('produccion-musical-dic-2025');
  const [status, setStatus] = useState<StatusFilter>('all');
  const [limit, setLimit] = useState(200);

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

  const updateStatusMutation = useMutation({
    mutationFn: (args: { id: number; newStatus: StatusFilter }) =>
      Courses.updateStatus(slug, args.id, { status: args.newStatus }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['admin', 'course-registrations'] });
    },
  });

  const handleRefresh = () => {
    void qc.invalidateQueries({ queryKey: ['admin', 'course-registrations'] });
  };

  return (
    <Stack spacing={3}>
      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Typography variant="h4" fontWeight={700}>
          Inscripciones de cursos
        </Typography>
        <Stack direction="row" spacing={1}>
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
              onChange={(e) => setStatus(e.target.value as StatusFilter)}
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
              onChange={(e) => setLimit(Number(e.target.value) || 100)}
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
      </Paper>

      <Paper sx={{ p: 3, borderRadius: 3 }}>
        {regsQuery.isError && (
          <Typography color="error">
            No se pudieron cargar las inscripciones: {regsQuery.error instanceof Error ? regsQuery.error.message : 'Error'}
          </Typography>
        )}
        {regsQuery.isLoading && <Typography>Cargando inscripciones…</Typography>}
        {!regsQuery.isLoading && regsQuery.data && regsQuery.data.length === 0 && (
          <Typography color="text.secondary">No hay inscripciones para estos filtros.</Typography>
        )}
        {regsQuery.data && regsQuery.data.length > 0 && (
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
                <Box sx={{ flexGrow: 1 }} />
                {actionButtons(reg, (newStatus) => updateStatusMutation.mutate({ id: reg.crId, newStatus }), updateStatusMutation.isPending)}
              </Box>
            ))}
          </Stack>
        )}
      </Paper>
    </Stack>
  );
}
