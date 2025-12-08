import { Box, Button, Stack, Typography } from '@mui/material';
import { useLocation, useNavigate } from 'react-router-dom';
import { useSession } from '../session/SessionContext';

export default function NotFoundPage() {
  const navigate = useNavigate();
  const location = useLocation();
  const { session } = useSession();

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
        <Typography variant="h4" fontWeight={700}>
          Página no encontrada
        </Typography>
        <Typography variant="body1" color="text.secondary">
          No encontramos la ruta <strong>{location.pathname}</strong>. Revisa el enlace o vuelve a una sección conocida.
        </Typography>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5} justifyContent="center">
          <Button variant="contained" onClick={() => navigate(session ? '/inicio' : '/login', { replace: true })}>
            Ir al {session ? 'inicio' : 'login'}
          </Button>
          <Button variant="outlined" onClick={() => navigate(-1)}>
            Volver
          </Button>
        </Stack>
        <Stack direction="row" spacing={2} justifyContent="center">
          <Button variant="text" onClick={() => navigate('/docs')}>Ver docs</Button>
          <Button variant="text" onClick={() => navigate('/feedback')}>Enviar feedback</Button>
        </Stack>
      </Stack>
    </Box>
  );
}
