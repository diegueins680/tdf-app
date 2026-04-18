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

const parseLogLimit = (value: string, fallback = 100): number => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed)) return fallback;
  return Math.min(1000, Math.max(1, parsed));
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

  const getLevelColor = (level: string): 'default' | 'warning' | 'error' | 'info' => {
    switch (level) {
      case 'error':
        return 'error';
      case 'warning':
        return 'warning';
      case 'info':
        return 'info';
      default:
        return 'default';
    }
  };

  const logs = logsQuery.data ?? [];
  const hasLogs = logs.length > 0;
  const showLogsTable = logsQuery.isLoading || hasLogs;

  return (
    <Stack spacing={3}>
      <Stack direction="row" spacing={2} alignItems="center" justifyContent="space-between">
        <Typography variant="h5" component="h1">
          Logs del servidor
        </Typography>
        <Stack direction="row" spacing={1}>
          <TextField
            type="number"
            label="Limite"
            value={limit}
            onChange={(e) => setLimit(parseLogLimit(e.target.value))}
            size="small"
            sx={{ width: 100 }}
            inputProps={{ min: 1, max: 1000 }}
          />
          <Tooltip title="Refrescar logs">
            <span>
              <IconButton aria-label="Refrescar logs" onClick={() => void logsQuery.refetch()} disabled={logsQuery.isFetching}>
                <RefreshIcon />
              </IconButton>
            </span>
          </Tooltip>
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
          Todavia no hay logs disponibles. Esta vista mostrara eventos del servidor cuando exista el primer registro.
        </Alert>
      )}

      {showLogsTable && (
        <Paper>
          <TableContainer>
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell sx={{ width: 180 }}>Timestamp</TableCell>
                  <TableCell sx={{ width: 100 }}>Level</TableCell>
                  <TableCell>Message</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {logsQuery.isLoading && (
                  <TableRow>
                    <TableCell colSpan={3} align="center">
                      <CircularProgress size={24} />
                    </TableCell>
                  </TableRow>
                )}
                {logs.map((log, idx) => (
                  <TableRow key={idx} hover>
                    <TableCell>
                      <Typography variant="body2" sx={{ fontFamily: 'monospace', fontSize: '0.875rem' }}>
                        {formatTimestampForDisplay(log.logTimestamp)}
                      </Typography>
                    </TableCell>
                    <TableCell>
                      <Chip label={log.logLevel} size="small" color={getLevelColor(log.logLevel)} />
                    </TableCell>
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
                ))}
              </TableBody>
            </Table>
          </TableContainer>
        </Paper>
      )}
    </Stack>
  );
}
