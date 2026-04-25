import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
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
  Typography,
  Tooltip,
} from '@mui/material';
import RefreshIcon from '@mui/icons-material/Refresh';
import DeleteIcon from '@mui/icons-material/Delete';
import { Admin } from '../api/admin';
import { formatTimestampForDisplay } from '../utils/dateTime';

interface LogEntry {
  logTimestamp: string;
  logLevel: 'info' | 'warning' | 'error';
  logMessage: string;
}

const LOG_LEVEL_PRESENTATION: Record<string, { label: string; color: 'default' | 'warning' | 'error' | 'info' }> = {
  error: { label: 'Error', color: 'error' },
  info: { label: 'Info', color: 'info' },
  warning: { label: 'Advertencia', color: 'warning' },
};

const parseLogLimit = (value: string, fallback = 100): number => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed)) return fallback;
  return Math.min(1000, Math.max(1, parsed));
};

const getLevelPresentation = (level: string) => {
  const normalizedLevel = level.trim().toLocaleLowerCase('en-US');
  return LOG_LEVEL_PRESENTATION[normalizedLevel] ?? {
    color: 'default' as const,
    label: level.trim() || 'Desconocido',
  };
};

const getSharedLogLevelSummary = (logs: readonly LogEntry[]) => {
  if (logs.length < 2) return '';

  const normalizedLevels = logs
    .map((log) => log.logLevel.trim())
    .filter((level) => level !== '');

  if (normalizedLevels.length !== logs.length) return '';

  const [firstLevel] = normalizedLevels;
  const comparableLevel = firstLevel?.toLocaleLowerCase('en-US') ?? '';

  return normalizedLevels.every((level) => level.toLocaleLowerCase('en-US') === comparableLevel)
    ? getLevelPresentation(firstLevel ?? '').label
    : '';
};

export default function LogsPage() {
  const [limit, setLimit] = useState(100);
  const qc = useQueryClient();

  const logsQuery = useQuery<LogEntry[]>({
    queryKey: ['admin', 'logs', limit],
    queryFn: () => Admin.getLogs(limit),
    refetchInterval: 5000, // Auto-refresh every 5 seconds
  });

  const clearLogsMutation = useMutation({
    mutationFn: Admin.clearLogs,
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['admin', 'logs'] });
    },
  });

  const logs = logsQuery.data ?? [];
  const hasLogs = logs.length > 0;
  const sharedLevelSummary = getSharedLogLevelSummary(logs);
  const showLevelColumn = sharedLevelSummary === '';
  const showLogsTable = logsQuery.isLoading || hasLogs;
  const showLimitControl = logsQuery.isLoading || logsQuery.isError || hasLogs;
  const showRefreshAction = logsQuery.isError;
  const showAutoRefreshHint = logsQuery.isLoading || hasLogs;
  const visibleTableColumnCount = 2 + (showLevelColumn ? 1 : 0);

  return (
    <Stack spacing={3}>
      <Stack direction="row" spacing={2} alignItems="center" justifyContent="space-between">
        <Stack spacing={0.25}>
          <Typography variant="h5" component="h1">
            Logs del servidor
          </Typography>
          {showAutoRefreshHint && !logsQuery.isError && (
            <Typography variant="body2" color="text.secondary">
              Actualizacion automatica cada 5 segundos.
            </Typography>
          )}
        </Stack>
        <Stack direction="row" spacing={1} alignItems="center">
          {showLimitControl && (
            <TextField
              type="number"
              label="Limite"
              value={limit}
              onChange={(e) => setLimit(parseLogLimit(e.target.value))}
              size="small"
              sx={{ width: 100 }}
              inputProps={{ min: 1, max: 1000 }}
            />
          )}
          {showRefreshAction && (
            <Tooltip title="Refrescar logs">
              <span>
                <IconButton aria-label="Refrescar logs" onClick={() => void logsQuery.refetch()} disabled={logsQuery.isFetching}>
                  <RefreshIcon />
                </IconButton>
              </span>
            </Tooltip>
          )}
          {hasLogs && (
            <Tooltip title="Vaciar logs">
              <span>
                <IconButton
                  aria-label="Vaciar logs"
                  onClick={() => clearLogsMutation.mutate()}
                  disabled={clearLogsMutation.isPending}
                  color="error"
                >
                  <DeleteIcon />
                </IconButton>
              </span>
            </Tooltip>
          )}
        </Stack>
      </Stack>

      {logsQuery.isError && (
        <Alert severity="error">
          Failed to load logs: {logsQuery.error instanceof Error ? logsQuery.error.message : 'Unknown error'}
        </Alert>
      )}

      {clearLogsMutation.isError && (
        <Alert severity="error">
          Failed to clear logs: {clearLogsMutation.error instanceof Error ? clearLogsMutation.error.message : 'Unknown error'}
        </Alert>
      )}

      {clearLogsMutation.isSuccess && (
        <Alert severity="success" onClose={() => clearLogsMutation.reset()}>
          Logs vaciados correctamente
        </Alert>
      )}

      {!logsQuery.isLoading && !logsQuery.isError && !hasLogs && (
        <Alert severity="info" variant="outlined" data-testid="server-logs-empty-state">
          Todavia no hay logs disponibles. Esta vista se actualiza automaticamente y mostrara filtros cuando exista el primer registro.
        </Alert>
      )}

      {showLogsTable && (
        <Paper>
          {sharedLevelSummary && (
            <Typography
              variant="caption"
              color="text.secondary"
              sx={{ display: 'block', px: 2, pt: 2 }}
              data-testid="server-logs-shared-level-summary"
            >
              {`Mostrando un solo nivel: ${sharedLevelSummary}. La columna volverá cuando esta vista mezcle niveles distintos.`}
            </Typography>
          )}
          <TableContainer>
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell sx={{ width: 180 }}>Fecha y hora</TableCell>
                  {showLevelColumn && <TableCell sx={{ width: 100 }}>Nivel</TableCell>}
                  <TableCell>Mensaje</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {logsQuery.isLoading && (
                  <TableRow>
                    <TableCell colSpan={visibleTableColumnCount} align="center">
                      <CircularProgress size={24} />
                    </TableCell>
                  </TableRow>
                )}
                {logs.map((log, idx) => {
                  const levelPresentation = getLevelPresentation(log.logLevel);

                  return (
                    <TableRow key={idx} hover>
                      <TableCell>
                        <Typography variant="body2" sx={{ fontFamily: 'monospace', fontSize: '0.875rem' }}>
                          {formatTimestampForDisplay(log.logTimestamp)}
                        </Typography>
                      </TableCell>
                      {showLevelColumn && (
                        <TableCell>
                          <Chip label={levelPresentation.label} size="small" color={levelPresentation.color} />
                        </TableCell>
                      )}
                      <TableCell>
                        <Typography
                          variant="body2"
                          sx={{
                            fontFamily: 'monospace',
                            fontSize: '0.875rem',
                            whiteSpace: 'pre-wrap',
                            wordBreak: 'break-word',
                          }}
                        >
                          {log.logMessage}
                        </Typography>
                      </TableCell>
                    </TableRow>
                  );
                })}
              </TableBody>
            </Table>
          </TableContainer>
        </Paper>
      )}
    </Stack>
  );
}
