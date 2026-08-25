import { logger } from '../utils/logger';
import { Alert, AlertTitle, Button, CircularProgress, Stack, Typography } from '@mui/material';
import type { KeyboardEvent, MouseEvent, ReactNode } from 'react';
import { useEffect, useRef, useState } from 'react';
import { API_BASE_URL } from '../api/client';

interface Props {
  error: unknown;
  title?: string;
  onRetry?: () => void | Promise<unknown>;
  helper?: ReactNode;
  showCorsHint?: boolean;
}

interface LoadingProps {
  title?: string;
  message?: ReactNode;
  helper?: ReactNode;
}

type ApiLoadingNoticeContract = Readonly<{
  loadingSpinnerSizePx: number;
  titleFontWeight: number;
}>;

// Invariant: loading notices stay compact and keep their optional title visually
// stronger than helper text while remaining valid numeric CSS values.
export const API_LOADING_NOTICE_CONTRACTS = {
  loadingSpinnerSizePx: 2 * 10 - 2,
  titleFontWeight: 7 * 100,
} as const satisfies ApiLoadingNoticeContract;

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
      logger.warn('Cross-origin API request detected', { apiOrigin: apiUrl.origin, appOrigin });
      return 'No pudimos conectar con el servicio en este momento. Intenta nuevamente en unos minutos.';
    }
  } catch {
    return null;
  }
  return null;
};

export function ApiLoadingNotice({
  title = 'Cargando',
  message = 'Actualizando la vista.',
  helper,
}: LoadingProps) {
  // Precondition is captured by LoadingProps; postcondition is a polite status
  // region rendered with API_LOADING_NOTICE_CONTRACTS.
  const loadingLabel = title || 'Cargando';

  return (
    <Stack
      direction="row"
      spacing={1.25}
      alignItems="flex-start"
      role="status"
      aria-live="polite"
      aria-busy="true"
      sx={{
        border: 1,
        borderColor: 'divider',
        borderRadius: 1,
        bgcolor: 'action.hover',
        p: 1.5,
      }}
    >
      <CircularProgress
        size={API_LOADING_NOTICE_CONTRACTS.loadingSpinnerSizePx}
        aria-label={loadingLabel}
        sx={{ mt: 0.25, flex: '0 0 auto' }}
      />
      <Stack spacing={0.25}>
        {title && (
          <Typography variant="body2" fontWeight={API_LOADING_NOTICE_CONTRACTS.titleFontWeight}>
            {title}
          </Typography>
        )}
        {message && (
          <Typography variant="body2" color="text.secondary">
            {message}
          </Typography>
        )}
        {helper}
      </Stack>
    </Stack>
  );
}

export default function ApiErrorNotice({ error, title, onRetry, helper, showCorsHint }: Props) {
  const corsHint = buildCorsHint(showCorsHint);
  const retryAriaLabel = title ? `Reintentar: ${title}` : 'Reintentar solicitud fallida';
  const noticeRef = useRef<HTMLDivElement | null>(null);
  const retryInFlightRef = useRef(false);
  const isMountedRef = useRef(true);
  const [isRetrying, setIsRetrying] = useState(false);

  useEffect(() => () => {
    isMountedRef.current = false;
  }, []);

  const focusNotice = () => {
    if (noticeRef.current?.isConnected) {
      noticeRef.current.focus();
    }
  };

  const runRetry = async () => {
    if (!onRetry || retryInFlightRef.current) return;

    retryInFlightRef.current = true;
    setIsRetrying(true);
    focusNotice();

    try {
      await onRetry();
    } catch (retryError) {
      logger.warn('Retry action failed', { error: describeError(retryError) });
    } finally {
      retryInFlightRef.current = false;
      if (isMountedRef.current) {
        setIsRetrying(false);
        queueMicrotask(focusNotice);
      }
    }
  };

  const focus = {
    afterRetryClick: (_event: MouseEvent<HTMLButtonElement>) => {
      void runRetry();
    },
    afterRetryKeyDown: (event: KeyboardEvent<HTMLButtonElement>) => {
      if (event.key !== 'Enter' && event.key !== ' ') return;
      event.preventDefault();
      void runRetry();
    },
  };

  return (
    <Alert
      ref={noticeRef}
      tabIndex={-1}
      severity="error"
      action={
        onRetry ? (
          <Button
            size="small"
            type="button"
            aria-label={retryAriaLabel}
            aria-busy={isRetrying ? true : undefined}
            disabled={isRetrying}
            onClick={focus.afterRetryClick}
            onKeyDown={focus.afterRetryKeyDown}
            startIcon={isRetrying ? <CircularProgress size={14} color="inherit" /> : undefined}
          >
            {isRetrying ? 'Reintentando...' : 'Reintentar'}
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
