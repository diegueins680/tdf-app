import { useSyncExternalStore } from 'react';
import { Box, LinearProgress } from '@mui/material';

import { getPendingApiRequestCount, subscribeToApiActivity } from '../api/client';

const getServerSnapshot = () => 0;

export default function ApiActivityIndicator() {
  const pendingRequests = useSyncExternalStore(
    subscribeToApiActivity,
    getPendingApiRequestCount,
    getServerSnapshot,
  );
  const loading = pendingRequests > 0;

  return (
    <Box
      role="status"
      aria-live="polite"
      aria-busy={loading ? true : undefined}
      sx={(theme) => ({
        height: 4,
        flexShrink: 0,
        overflow: 'hidden',
        bgcolor: loading ? 'action.hover' : 'transparent',
        transition: theme.transitions.create('background-color', {
          duration: theme.transitions.duration.shortest,
        }),
      })}
    >
      {loading && (
        <LinearProgress
          aria-label="Cargando datos"
          sx={{
            height: 4,
          }}
        />
      )}
    </Box>
  );
}
