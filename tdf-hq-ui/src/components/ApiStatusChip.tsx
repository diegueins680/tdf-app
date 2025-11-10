import { useQuery } from '@tanstack/react-query';
import { Chip, CircularProgress } from '@mui/material';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import ErrorOutlineIcon from '@mui/icons-material/ErrorOutline';
import { Meta } from '../api/meta';

export default function ApiStatusChip() {
  const { data, isLoading } = useQuery({
    queryKey: ['meta', 'health-indicator'],
    queryFn: Meta.health,
    refetchInterval: 60_000,
  });

  if (isLoading) {
    return (
      <Chip
        icon={<CircularProgress size={14} color="inherit" />}
        label="API: verificando..."
        sx={{ bgcolor: 'rgba(255,255,255,0.1)', color: '#f8fafc' }}
      />
    );
  }

  const healthy = (data?.status || '').toLowerCase() === 'ok';
  return (
    <Chip
      icon={healthy ? <CheckCircleIcon fontSize="small" /> : <ErrorOutlineIcon fontSize="small" />}
      label={`API: ${healthy ? 'online' : 'offline'}`}
      color={healthy ? 'success' : 'warning'}
      variant={healthy ? 'filled' : 'outlined'}
    />
  );
}
