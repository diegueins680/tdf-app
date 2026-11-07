import type { ErrorInfo, ReactNode } from 'react';
import { Component } from 'react';
import { Alert, Box, Button, Stack, Typography } from '@mui/material';
import { reportClientError } from '../analytics/errorReporting';

interface AppErrorBoundaryProps {
  children: ReactNode;
  /** Changing this (e.g. the route path) clears a previous failure. */
  resetKey?: string;
}

interface AppErrorBoundaryState {
  error: Error | null;
}

export default class AppErrorBoundary extends Component<AppErrorBoundaryProps, AppErrorBoundaryState> {
  override state: AppErrorBoundaryState = { error: null };

  static getDerivedStateFromError(error: Error): AppErrorBoundaryState {
    return { error };
  }

  override componentDidCatch(error: Error, info: ErrorInfo) {
    reportClientError('app_render', error, {
      component_stack: (info.componentStack ?? '').split('\n').slice(0, 6).join(' | ').slice(0, 300),
    });
  }

  override componentDidUpdate(previous: AppErrorBoundaryProps) {
    // Navigating away from a failed view (back button, nav link) must render
    // the new route instead of keeping the error screen forever.
    if (this.state.error && previous.resetKey !== this.props.resetKey) {
      this.setState({ error: null });
    }
  }

  private handleReload = () => {
    window.location.reload();
  };

  private handleGoHome = () => {
    window.location.href = '/inicio';
  };

  override render() {
    if (!this.state.error) {
      return this.props.children;
    }

    return (
      <Box
        sx={{
          minHeight: '100vh',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          bgcolor: 'background.default',
          p: 2,
        }}
      >
        <Alert
          severity="error"
          action={(
            <Stack direction="row" spacing={1}>
              <Button color="inherit" size="small" onClick={this.handleGoHome}>
                Ir al inicio
              </Button>
              <Button color="inherit" size="small" onClick={this.handleReload}>
                Recargar
              </Button>
            </Stack>
          )}
          sx={{ width: '100%', maxWidth: 640 }}
        >
          <Stack spacing={0.5}>
            <Typography fontWeight={800}>No pudimos cargar esta vista.</Typography>
            <Typography variant="body2">
              Recarga la página para usar la versión más reciente de TDF Records.
            </Typography>
            <Typography variant="body2" color="text.secondary">
              Si el problema persiste, vuelve al inicio o contacta a soporte.
            </Typography>
          </Stack>
        </Alert>
      </Box>
    );
  }
}
