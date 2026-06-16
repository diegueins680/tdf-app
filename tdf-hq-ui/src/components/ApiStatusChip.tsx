import { useQuery } from '@tanstack/react-query';
import { Chip, CircularProgress } from '@mui/material';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import ErrorOutlineIcon from '@mui/icons-material/ErrorOutline';
import { Meta } from '../api/meta';

export default function ApiStatusChip() {
  const { data, isError, isFetching } = useQuery({
    queryKey: ['meta', 'health-indicator'],
    queryFn: Meta.health,
    refetchInterval: 60_000,
  });

  const hasStatus = data?.status != null;
  const checkingInitialStatus = isFetching && !hasStatus;

  if (checkingInitialStatus) {
    return (
      <Chip
        role="status"
        aria-live="polite"
        aria-busy="true"
        icon={<CircularProgress size={14} color="inherit" aria-label="Verificando API" />}
        label="API: verificando..."
        color="info"
        size="small"
        variant="outlined"
      />
    );
  }

  const healthy = !isError && (data?.status ?? '').toLowerCase() === 'ok';
  const refreshingStatus = isFetching && hasStatus;

  return (
    <Chip
      role="status"
      aria-live="polite"
      aria-busy={refreshingStatus ? true : undefined}
      icon={
        refreshingStatus
          ? <CircularProgress size={14} color="inherit" aria-label="Actualizando API" />
          : healthy ? <CheckCircleIcon fontSize="small" /> : <ErrorOutlineIcon fontSize="small" />
      }
      label={`API: ${healthy ? 'online' : 'offline'}`}
      color={healthy ? 'success' : 'warning'}
      size="small"
      variant={healthy ? 'filled' : 'outlined'}
    />
  );
}
