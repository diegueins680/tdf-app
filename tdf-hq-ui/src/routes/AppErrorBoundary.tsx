import type { ErrorInfo, ReactNode } from 'react';
import { Component } from 'react';
import { Alert, Box, Button, Stack, Typography } from '@mui/material';

interface AppErrorBoundaryProps {
  children: ReactNode;
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
    console.error('Unhandled route render failure', error, info.componentStack);
  }

  private handleReload = () => {
    window.location.reload();
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
            <Button color="inherit" size="small" onClick={this.handleReload}>
              Recargar
            </Button>
          )}
          sx={{ width: '100%', maxWidth: 640 }}
        >
          <Stack spacing={0.5}>
            <Typography fontWeight={800}>No pudimos cargar esta vista.</Typography>
            <Typography variant="body2">
              Recarga la pagina para tomar la version mas reciente de TDF Records.
            </Typography>
          </Stack>
        </Alert>
      </Box>
    );
  }
}
