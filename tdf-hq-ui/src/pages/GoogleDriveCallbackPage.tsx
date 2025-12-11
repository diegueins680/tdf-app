import { Alert, Box, Button, CircularProgress, Stack, Typography } from '@mui/material';
import { useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { useGoogleDriveCallback } from '../hooks/useGoogleDriveAuth';

export default function GoogleDriveCallbackPage() {
  const navigate = useNavigate();
  const result = useGoogleDriveCallback();

  useEffect(() => {
    if (result.ok && result.state) {
      const target = decodeURIComponent(result.state);
      navigate(target, { replace: true });
    } else if (result.ok) {
      navigate('/', { replace: true });
    }
  }, [navigate, result]);

  if (!result.ok && !result.message) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="60vh">
        <Stack spacing={2} alignItems="center">
          <CircularProgress />
          <Typography variant="body2" color="text.secondary">
            Conectando con Google Drive...
          </Typography>
        </Stack>
      </Box>
    );
  }

  if (!result.ok) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="60vh">
        <Stack spacing={2} alignItems="center">
          <Alert severity="error">
            No pudimos autorizar Google Drive: {result.message}
            <br />
            Verifica el redirect configurado en Google: {import.meta.env['VITE_GOOGLE_DRIVE_REDIRECT_URI'] ?? 'no definido'}
          </Alert>
          <Button variant="contained" onClick={() => navigate('/', { replace: true })}>
            Volver
          </Button>
        </Stack>
      </Box>
    );
  }

  return null;
}
