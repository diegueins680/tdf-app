import { useMemo, useState } from 'react';
import {
  Box,
  Button,
  Chip,
  LinearProgress,
  MenuItem,
  Paper,
  Select,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  TextField,
  Typography,
} from '@mui/material';
import EventNoteIcon from '@mui/icons-material/EventNote';
import CalendarMonthIcon from '@mui/icons-material/CalendarMonth';
import { useQuery } from '@tanstack/react-query';
import { Sessions } from '../api/sessions';
import type { SessionDTO } from '../api/types';
import { Link as RouterLink } from 'react-router-dom';

const STATUS_LABELS: Record<string, { label: string; color: 'default' | 'primary' | 'success' | 'warning' | 'info' | 'error' }> = {
  InPrep: { label: 'En preparación', color: 'info' },
  InSession: { label: 'En sesión', color: 'success' },
  Break: { label: 'Break', color: 'warning' },
  Editing: { label: 'Edición', color: 'default' },
  Approved: { label: 'Aprobada', color: 'success' },
  Delivered: { label: 'Entregada', color: 'success' },
  Closed: { label: 'Cerrada', color: 'default' },
};

function formatDate(indicator: string) {
  const date = new Date(indicator);
  if (Number.isNaN(date.getTime())) return indicator;
  return date.toLocaleString();
}

function roomLabel(roomIds: string[] | undefined) {
  if (!roomIds?.length) return '—';
  return roomIds.join(', ');
}

export default function LessonsPage() {
  const [search, setSearch] = useState('');
  const [status, setStatus] = useState<string>('all');

  const sessionsQuery = useQuery({
    queryKey: ['lessons', { page: 1 }],
    queryFn: () => Sessions.list({ page: 1, pageSize: 200 }),
  });

  const sessions = sessionsQuery.data?.items ?? [];

  const filtered = useMemo(() => {
    const term = search.trim().toLowerCase();
    return sessions.filter((session) => {
      const matchesStatus = status === 'all' || session.sStatus === status;
      if (!matchesStatus) return false;
      if (!term) return true;
      const haystack = [
        session.sService,
        session.sEngineerRef,
        session.sAssistantRef ?? '',
        session.sBookingRef ?? '',
        session.sNotes ?? '',
      ].join(' ').toLowerCase();
      return haystack.includes(term);
    });
  }, [sessions, search, status]);

  const stats = useMemo(() => {
    const total = sessions.length;
    const byStatus = sessions.reduce<Record<string, number>>((acc, item) => {
      acc[item.sStatus] = (acc[item.sStatus] ?? 0) + 1;
      return acc;
    }, {});
    return { total, byStatus };
  }, [sessions]);

  return (
    <Stack spacing={3}>
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ xs: 'stretch', sm: 'center' }} justifyContent="space-between">
        <Box>
          <Typography variant="h5">Clases y sesiones</Typography>
          <Typography variant="body2" color="text.secondary">
            Monitorea las clases programadas, identifica docentes y verifica la ocupación de salas.
          </Typography>
        </Box>
        <Stack direction="row" spacing={1} justifyContent="flex-end">
          <Button
            variant="outlined"
            component={RouterLink}
            to="/sessions"
            startIcon={<CalendarMonthIcon />}
          >
            Abrir gestor avanzado
          </Button>
        </Stack>
      </Stack>

      <Paper variant="outlined" sx={{ p: 2 }}>
        <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} alignItems={{ xs: 'flex-start', md: 'center' }} justifyContent="space-between">
          <Stack direction="row" spacing={3} alignItems="center">
            <Box>
              <Typography variant="h6">{stats.total}</Typography>
              <Typography variant="body2" color="text.secondary">Sesiones registradas</Typography>
            </Box>
            <Stack direction="row" spacing={1} flexWrap="wrap">
              {Object.entries(stats.byStatus).map(([key, value]) => (
                <Chip
                  key={key}
                  icon={<EventNoteIcon fontSize="small" />}
                  label={`${STATUS_LABELS[key]?.label ?? key}: ${value}`}
                  color={STATUS_LABELS[key]?.color ?? 'default'}
                  variant="outlined"
                  size="small"
                />
              ))}
            </Stack>
          </Stack>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'stretch', sm: 'center' }}>
            <TextField
              value={search}
              onChange={(event) => setSearch(event.target.value)}
              size="small"
              label="Buscar"
              placeholder="Servicio, ingeniero, booking…"
            />
            <Select
              size="small"
              value={status}
              onChange={(event) => setStatus(event.target.value)}
            >
              <MenuItem value="all">Todos los estados</MenuItem>
              {Object.entries(STATUS_LABELS).map(([value, entry]) => (
                <MenuItem key={value} value={value}>{entry.label}</MenuItem>
              ))}
            </Select>
          </Stack>
        </Stack>
      </Paper>

      <Paper variant="outlined">
        {sessionsQuery.isFetching && <LinearProgress />}
        <TableContainer>
          <Table size="small">
            <TableHead>
              <TableRow>
                <TableCell>Servicio</TableCell>
                <TableCell>Inicio</TableCell>
                <TableCell>Fin</TableCell>
                <TableCell>Docente / Ing.</TableCell>
                <TableCell>Salas</TableCell>
                <TableCell>Estado</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {filtered.map((session: SessionDTO) => {
                const statusInfo = STATUS_LABELS[session.sStatus] ?? { label: session.sStatus, color: 'default' as const };
                return (
                  <TableRow key={session.sessionId} hover>
                    <TableCell sx={{ minWidth: 180 }}>{session.sService || '—'}</TableCell>
                    <TableCell>{formatDate(session.sStartAt)}</TableCell>
                    <TableCell>{formatDate(session.sEndAt)}</TableCell>
                    <TableCell>
                      <Stack spacing={0.5}>
                        <Typography variant="body2">{session.sEngineerRef || 'Sin asignar'}</Typography>
                        {session.sAssistantRef && <Typography variant="caption" color="text.secondary">Asistente: {session.sAssistantRef}</Typography>}
                      </Stack>
                    </TableCell>
                    <TableCell>{roomLabel(session.sRoomIds)}</TableCell>
                    <TableCell>
                      <Chip label={statusInfo.label} color={statusInfo.color} size="small" />
                    </TableCell>
                  </TableRow>
                );
              })}
              {filtered.length === 0 && (
                <TableRow>
                  <TableCell colSpan={6} align="center" sx={{ py: 4 }}>
                    No encontramos sesiones con los filtros actuales.
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </TableContainer>
      </Paper>
    </Stack>
  );
}
