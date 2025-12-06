import { Box, Button, Stack, Typography } from '@mui/material';
import { useLocation, useNavigate } from 'react-router-dom';

export default function NotFoundPage() {
  const navigate = useNavigate();
  const location = useLocation();

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
          <Button variant="contained" onClick={() => navigate('/inicio', { replace: true })}>
            Ir al inicio
          </Button>
          <Button variant="outlined" onClick={() => navigate('/login', { replace: true })}>
            Ir al login
          </Button>
        </Stack>
      </Stack>
    </Box>
  );
}
