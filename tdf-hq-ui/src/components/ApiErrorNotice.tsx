import ReplayIcon from '@mui/icons-material/Replay';
import { Alert, AlertTitle, Button, Stack, Typography } from '@mui/material';
import type { ReactNode } from 'react';
import { API_BASE_URL } from '../api/client';

interface Props {
  error: unknown;
  title?: string;
  onRetry?: () => void;
  helper?: ReactNode;
  showCorsHint?: boolean;
}

const describeError = (error: unknown) => {
  if (error instanceof Error) return error.message;
  if (typeof error === 'string') return error;
  return 'Error desconocido.';
};

const buildCorsHint = (showCorsHint?: boolean) => {
  if (!showCorsHint) return null;
  if (typeof window === 'undefined') return null;
  if (!API_BASE_URL) return null;
  try {
    const apiUrl = new URL(API_BASE_URL, window.location.origin);
    const appOrigin = window.location.origin;
    if (apiUrl.origin !== appOrigin) {
      console.warn('Cross-origin API request detected', { apiOrigin: apiUrl.origin, appOrigin });
      return 'No pudimos conectar con el servicio en este momento. Intenta nuevamente en unos minutos.';
    }
  } catch {
    return null;
  }
  return null;
};

export default function ApiErrorNotice({ error, title, onRetry, helper, showCorsHint }: Props) {
  const corsHint = buildCorsHint(showCorsHint);
  return (
    <Alert
      severity="error"
      action={
        onRetry ? (
          <Button size="small" startIcon={<ReplayIcon />} onClick={onRetry}>
            Reintentar
          </Button>
        ) : undefined
      }
    >
      <Stack spacing={0.5}>
        {title && <AlertTitle sx={{ mb: 0 }}>{title}</AlertTitle>}
        <Typography variant="body2">{describeError(error)}</Typography>
        {helper}
        {corsHint && (
          <Typography variant="caption" color="text.secondary">
            {corsHint}
          </Typography>
        )}
      </Stack>
    </Alert>
  );
}
