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

  if (isFetching) {
    return (
      <Chip
        role="status"
        aria-live="polite"
        aria-busy="true"
        icon={<CircularProgress size={14} color="inherit" aria-label="Verificando API" />}
        label="API: verificando..."
        size="small"
        sx={{ bgcolor: 'rgba(255,255,255,0.1)', color: '#f8fafc' }}
      />
    );
  }

  const healthy = !isError && (data?.status ?? '').toLowerCase() === 'ok';
  return (
    <Chip
      role="status"
      aria-live="polite"
      icon={healthy ? <CheckCircleIcon fontSize="small" /> : <ErrorOutlineIcon fontSize="small" />}
      label={`API: ${healthy ? 'online' : 'offline'}`}
      color={healthy ? 'success' : 'warning'}
      size="small"
      variant={healthy ? 'filled' : 'outlined'}
    />
  );
}
