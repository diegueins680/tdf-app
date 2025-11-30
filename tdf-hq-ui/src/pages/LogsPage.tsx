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

interface LogEntry {
  logTimestamp: string;
  logLevel: 'info' | 'warning' | 'error';
  logMessage: string;
}

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

  const formatTimestamp = (timestamp: string) => {
    try {
      const date = new Date(timestamp);
      return date.toLocaleString();
    } catch {
      return timestamp;
    }
  };

  return (
    <Stack spacing={3}>
      <Stack direction="row" spacing={2} alignItems="center" justifyContent="space-between">
        <Typography variant="h5" component="h1">
          Server Logs
        </Typography>
        <Stack direction="row" spacing={1}>
          <TextField
            type="number"
            label="Limit"
            value={limit}
            onChange={(e) => setLimit(parseInt(e.target.value) || 100)}
            size="small"
            sx={{ width: 100 }}
            inputProps={{ min: 1, max: 1000 }}
          />
          <Tooltip title="Refresh logs">
            <IconButton onClick={() => void logsQuery.refetch()} disabled={logsQuery.isFetching}>
              <RefreshIcon />
            </IconButton>
          </Tooltip>
          <Tooltip title="Clear all logs">
            <IconButton
              onClick={() => clearLogsMutation.mutate()}
              disabled={clearLogsMutation.isPending}
              color="error"
            >
              <DeleteIcon />
            </IconButton>
          </Tooltip>
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
          Logs cleared successfully
        </Alert>
      )}

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
              {logsQuery.data && logsQuery.data.length === 0 && (
                <TableRow>
                  <TableCell colSpan={3} align="center">
                    <Typography variant="body2" color="text.secondary">
                      No logs available
                    </Typography>
                  </TableCell>
                </TableRow>
              )}
              {logsQuery.data?.map((log, idx) => (
                <TableRow key={idx} hover>
                  <TableCell>
                    <Typography variant="body2" sx={{ fontFamily: 'monospace', fontSize: '0.875rem' }}>
                      {formatTimestamp(log.logTimestamp)}
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
    </Stack>
  );
}
