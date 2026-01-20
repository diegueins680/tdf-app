import { Alert, Box, Button, CircularProgress, Stack, Typography } from '@mui/material';
import { useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { useInstagramCallback } from '../hooks/useInstagramAuth';

export default function InstagramCallbackPage() {
  const navigate = useNavigate();
  const result = useInstagramCallback();

  useEffect(() => {
    if (!result.ok) return;
    const target = result.returnTo?.trim() || '/social/instagram';
    navigate(target, { replace: true });
  }, [navigate, result]);

  if (!result.ok && !result.message) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="60vh">
        <Stack spacing={2} alignItems="center">
          <CircularProgress />
          <Typography variant="body2" color="text.secondary">
            Conectando con Instagram...
          </Typography>
        </Stack>
      </Box>
    );
  }

  if (!result.ok) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="60vh">
        <Stack spacing={2} alignItems="center" maxWidth={520} textAlign="center">
          <Alert severity="error">
            No pudimos autorizar Instagram: {result.message}
            <br />
            Revisa el redirect configurado en Meta:{' '}
            {import.meta.env['VITE_INSTAGRAM_REDIRECT_URI'] ?? 'no definido'}
          </Alert>
          <Button variant="contained" onClick={() => navigate('/social/instagram', { replace: true })}>
            Volver
          </Button>
        </Stack>
      </Box>
    );
  }

  return null;
}
