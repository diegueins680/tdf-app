import { Alert, Button, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { useSession } from '../session/SessionContext';

interface SessionGateProps {
  children: React.ReactNode;
  message?: string;
  requireToken?: boolean;
}

/**
 * Simple guard to pause protected sections when there is no session/token.
 * Shows a login prompt with a retry action.
 */
export function SessionGate({ children, message, requireToken = true }: SessionGateProps) {
  const { session } = useSession();
  const hasToken = Boolean(session?.apiToken);
  if (requireToken && !hasToken) {
    return (
      <Alert
        severity="info"
        action={
          <Button component={RouterLink} to="/login" size="small" variant="contained">
            Iniciar sesión
          </Button>
        }
      >
        <Stack spacing={0.5}>
          <Typography variant="body2">
            {message ?? 'Inicia sesión para cargar esta sección.'}
          </Typography>
        </Stack>
      </Alert>
    );
  }
  return <>{children}</>;
}
