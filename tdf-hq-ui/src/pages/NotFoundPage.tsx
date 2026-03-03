import { useState } from 'react';
import { Alert, Box, Button, Stack, Typography } from '@mui/material';
import { useLocation, useNavigate } from 'react-router-dom';
import { useSession } from '../session/SessionContext';

export default function NotFoundPage() {
  const navigate = useNavigate();
  const location = useLocation();
  const { session } = useSession();
  const [copyStatus, setCopyStatus] = useState<'idle' | 'success' | 'error'>('idle');
  const fallbackPath = session ? '/inicio' : '/login';

  const handleBack = () => {
    if (typeof window !== 'undefined' && window.history.length > 1) {
      navigate(-1);
      return;
    }
    navigate(fallbackPath, { replace: true });
  };

  const handleCopyPath = async () => {
    const rawPath = `${location.pathname}${location.search}${location.hash}`;
    try {
      await navigator.clipboard.writeText(rawPath);
      setCopyStatus('success');
    } catch {
      setCopyStatus('error');
    }
  };

  return (
    <Box
      sx={{
        minHeight: '60vh',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        textAlign: 'center',
        gap: 2,
      }}
    >
      <Stack spacing={2} alignItems="center" maxWidth={460}>
        <Typography variant="overline" color="text.secondary" sx={{ letterSpacing: 1.2 }}>
          ERROR 404
        </Typography>
        <Typography variant="h4" fontWeight={700}>
          Página no encontrada
        </Typography>
        <Typography variant="body1" color="text.secondary">
          No encontramos la ruta <strong>{location.pathname}</strong>. Revisa el enlace o vuelve a una sección conocida.
        </Typography>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5} justifyContent="center">
          <Button variant="contained" onClick={() => navigate(fallbackPath, { replace: true })}>
            Ir al {session ? 'inicio' : 'login'}
          </Button>
          <Button variant="outlined" onClick={handleBack}>
            Volver
          </Button>
          <Button variant="text" onClick={() => { void handleCopyPath(); }}>
            Copiar ruta
          </Button>
        </Stack>
        {copyStatus === 'success' && (
          <Alert severity="success" sx={{ width: '100%' }}>
            Ruta copiada al portapapeles.
          </Alert>
        )}
        {copyStatus === 'error' && (
          <Alert severity="warning" sx={{ width: '100%' }}>
            No se pudo copiar la ruta. Puedes copiarla manualmente.
          </Alert>
        )}
        <Stack direction="row" spacing={2} justifyContent="center">
          <Button variant="text" onClick={() => navigate('/docs')}>Ver docs</Button>
          <Button variant="text" onClick={() => navigate('/feedback')}>Enviar feedback</Button>
        </Stack>
      </Stack>
    </Box>
  );
}
