import { useMemo, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import {
  Alert,
  Button,
  Chip,
  CircularProgress,
  IconButton,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import RefreshIcon from '@mui/icons-material/Refresh';
import { Admin, type UserActivity } from '../api/admin';
import LazyPaginatedList from '../components/LazyPaginatedList';
import { formatTimestampForDisplay } from '../utils/dateTime';

interface ActorActivitySummary {
  key: string;
  actorName: string;
  usernames: string[];
  roles: string[];
  count: number;
  lastAt: string;
  actions: string[];
}

const parseActivityLimit = (value: string, fallback = 200): number => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed)) return fallback;
  return Math.min(1000, Math.max(1, parsed));
};

const humanizeToken = (value: string): string => {
  const cleaned = value.trim().replace(/[_-]+/g, ' ');
  if (!cleaned) return 'Sin dato';
  return cleaned.replace(/\S+/g, (part) => part.charAt(0).toUpperCase() + part.slice(1));
};

const formatMetadata = (metadata: unknown): string => {
  if (metadata == null) return '';
  if (typeof metadata === 'string') return metadata;
  if (typeof metadata === 'number' || typeof metadata === 'boolean') return String(metadata);
  try {
    return JSON.stringify(metadata, null, 2);
  } catch {
    return '';
  }
};

const summarizeActivityByActor = (items: readonly UserActivity[]): ActorActivitySummary[] => {
  const summaries = new Map<string, ActorActivitySummary>();

  items.forEach((item) => {
    const key = item.actorPartyId == null ? 'system' : `party-${item.actorPartyId}`;
    const existing = summaries.get(key);
    const actionLabel = `${humanizeToken(item.action)} · ${item.entity}`;

    if (!existing) {
      summaries.set(key, {
        key,
        actorName: item.actorName,
        usernames: item.actorUsernames ?? [],
        roles: item.actorRoles ?? [],
        count: 1,
        lastAt: item.createdAt,
        actions: [actionLabel],
      });
      return;
    }

    existing.count += 1;
    if (new Date(item.createdAt).getTime() > new Date(existing.lastAt).getTime()) {
      existing.lastAt = item.createdAt;
    }
    if (!existing.actions.includes(actionLabel)) {
      existing.actions.push(actionLabel);
    }
  });

  return Array.from(summaries.values()).sort(
    (a, b) => new Date(b.lastAt).getTime() - new Date(a.lastAt).getTime(),
  );
};

export default function UserActivityPage() {
  const [limit, setLimit] = useState(200);

  const activityQuery = useQuery<UserActivity[]>({
    queryKey: ['admin', 'activity', limit],
    queryFn: () => Admin.getActivity(limit),
    refetchInterval: 15000,
  });

  const activities = useMemo(() => activityQuery.data ?? [], [activityQuery.data]);
  const summaries = useMemo(() => summarizeActivityByActor(activities), [activities]);
  const hasActivity = activities.length > 0;

  return (
    <Stack spacing={3}>
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between">
        <Stack spacing={0.25}>
          <Typography variant="h5" component="h1">
            Actividad de usuarios
          </Typography>
          <Typography variant="body2" color="text.secondary">
            {hasActivity ? `${activities.length} acciones recientes · ${summaries.length} actores` : 'Sin actividad registrada'}
          </Typography>
        </Stack>
        <Stack direction="row" spacing={1} alignItems="center">
          <TextField
            type="number"
            label="Límite"
            value={limit}
            onChange={(event) => setLimit(parseActivityLimit(event.target.value))}
            size="small"
            sx={{ width: 112 }}
            inputProps={{ min: 1, max: 1000 }}
          />
          <Tooltip title="Refrescar actividad">
            <span>
              <IconButton
                aria-label="Refrescar actividad"
                onClick={() => void activityQuery.refetch()}
                disabled={activityQuery.isFetching}
              >
                <RefreshIcon />
              </IconButton>
            </span>
          </Tooltip>
        </Stack>
      </Stack>

      {activityQuery.isError && (
        <Alert
          severity="error"
          action={(
            <Button
              color="inherit"
              size="small"
              onClick={() => void activityQuery.refetch()}
              disabled={activityQuery.isFetching}
            >
              Reintentar
            </Button>
          )}
        >
          No se pudo cargar la actividad: {activityQuery.error instanceof Error ? activityQuery.error.message : 'error desconocido'}
        </Alert>
      )}

      {!activityQuery.isLoading && !activityQuery.isError && !hasActivity && (
        <Alert severity="info" variant="outlined" data-testid="user-activity-empty-state">
          Todavía no hay actividad registrada.
        </Alert>
      )}

      {(activityQuery.isLoading || hasActivity) && (
        <Paper>
          <TableContainer>
            <Table size="small" aria-label="Resumen de actividad por usuario">
              <TableHead>
                <TableRow>
                  <TableCell>Usuario</TableCell>
                  <TableCell sx={{ width: 180 }}>Última acción</TableCell>
                  <TableCell sx={{ width: 110 }} align="right">Acciones</TableCell>
                  <TableCell>Tipos</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {activityQuery.isLoading && (
                  <TableRow>
                    <TableCell colSpan={4} align="center">
                      <CircularProgress size={24} />
                    </TableCell>
                  </TableRow>
                )}
                {summaries.map((summary) => (
                  <TableRow key={summary.key} hover>
                    <TableCell>
                      <Stack spacing={0.5}>
                        <Typography variant="body2" fontWeight={700}>{summary.actorName}</Typography>
                        {summary.usernames.length > 0 && (
                          <Typography variant="caption" color="text.secondary">
                            {summary.usernames.join(', ')}
                          </Typography>
                        )}
                        {summary.roles.length > 0 && (
                          <Stack direction="row" spacing={0.5} flexWrap="wrap" useFlexGap>
                            {summary.roles.slice(0, 4).map((role) => (
                              <Chip key={role} label={role} size="small" variant="outlined" />
                            ))}
                          </Stack>
                        )}
                      </Stack>
                    </TableCell>
                    <TableCell>{formatTimestampForDisplay(summary.lastAt)}</TableCell>
                    <TableCell align="right">{summary.count}</TableCell>
                    <TableCell>
                      <Stack direction="row" spacing={0.5} flexWrap="wrap" useFlexGap>
                        {summary.actions.slice(0, 4).map((action) => (
                          <Chip key={action} label={action} size="small" />
                        ))}
                      </Stack>
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </TableContainer>
        </Paper>
      )}

      {(activityQuery.isLoading || hasActivity) && (
        <Paper>
          <LazyPaginatedList
            items={activities}
            pagination={{ itemLabel: 'acciones', initialRowsPerPage: 25, resetKey: limit }}
            renderItems={(visibleActivities) => (
              <TableContainer>
                <Table size="small" aria-label="Actividad reciente">
                  <TableHead>
                    <TableRow>
                      <TableCell sx={{ width: 180 }}>Fecha y hora</TableCell>
                      <TableCell sx={{ width: 220 }}>Usuario</TableCell>
                      <TableCell sx={{ width: 170 }}>Acción</TableCell>
                      <TableCell sx={{ width: 220 }}>Objeto</TableCell>
                      <TableCell>Metadatos</TableCell>
                    </TableRow>
                  </TableHead>
                  <TableBody>
                    {activityQuery.isLoading && (
                      <TableRow>
                        <TableCell colSpan={5} align="center">
                          <CircularProgress size={24} />
                        </TableCell>
                      </TableRow>
                    )}
                    {visibleActivities.map((activity) => {
                      const metadata = formatMetadata(activity.metadata);
                      return (
                        <TableRow key={activity.id} hover>
                          <TableCell>{formatTimestampForDisplay(activity.createdAt)}</TableCell>
                          <TableCell>
                            <Stack spacing={0.25}>
                              <Typography variant="body2" fontWeight={700}>{activity.actorName}</Typography>
                              {activity.actorUsernames.length > 0 && (
                                <Typography variant="caption" color="text.secondary">
                                  {activity.actorUsernames.join(', ')}
                                </Typography>
                              )}
                            </Stack>
                          </TableCell>
                          <TableCell>
                            <Chip label={humanizeToken(activity.action)} size="small" color="info" />
                          </TableCell>
                          <TableCell>
                            <Typography variant="body2" sx={{ fontFamily: 'monospace', wordBreak: 'break-word' }}>
                              {activity.entity}#{activity.entityId}
                            </Typography>
                          </TableCell>
                          <TableCell>
                            {metadata ? (
                              <Typography
                                variant="body2"
                                sx={{ fontFamily: 'monospace', whiteSpace: 'pre-wrap', wordBreak: 'break-word' }}
                              >
                                {metadata}
                              </Typography>
                            ) : (
                              <Typography variant="body2" color="text.secondary">-</Typography>
                            )}
                          </TableCell>
                        </TableRow>
                      );
                    })}
                  </TableBody>
                </Table>
              </TableContainer>
            )}
          />
        </Paper>
      )}
    </Stack>
  );
}
