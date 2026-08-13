import type { ErrorInfo, ReactNode } from 'react';
import { Component } from 'react';
import { Alert, Button, Stack, Typography } from '@mui/material';

interface RouteErrorBoundaryProps {
  children: ReactNode;
}

interface RouteErrorBoundaryState {
  error: Error | null;
}

export default class RouteErrorBoundary extends Component<RouteErrorBoundaryProps, RouteErrorBoundaryState> {
  override state: RouteErrorBoundaryState = { error: null };

  static getDerivedStateFromError(error: Error): RouteErrorBoundaryState {
    return { error };
  }

  override componentDidCatch(error: Error, info: ErrorInfo) {
    console.error('Route render failure', error, info.componentStack);

    // Report to error tracking service
    try {
      // PostHog error capture (already integrated in the app)
      if (typeof window !== 'undefined' && (window as any).posthog) {
        (window as any).posthog.captureException(error, {
          componentStack: info.componentStack,
          url: window.location.href,
        });
      }
    } catch {
      // Silently fail — don't let error reporting crash the error handler
    }
  }

  private handleRetry = () => {
    this.setState({ error: null });
  };

  private handleGoHome = () => {
    window.location.href = '/inicio';
  };

  override render() {
    if (!this.state.error) {
      return this.props.children;
    }

    return (
      <Alert
        severity="error"
        sx={{ my: 2 }}
        action={(
          <Stack direction="row" spacing={1}>
            <Button color="inherit" size="small" onClick={this.handleRetry}>
              Reintentar
            </Button>
            <Button color="inherit" size="small" onClick={this.handleGoHome}>
              Ir al inicio
            </Button>
          </Stack>
        )}
      >
        <Stack spacing={0.5}>
          <Typography fontWeight={700}>Algo salió al cargar esta sección.</Typography>
          <Typography variant="body2">
            Puedes reintentar o volver al inicio.
          </Typography>
        </Stack>
      </Alert>
    );
  }
}
