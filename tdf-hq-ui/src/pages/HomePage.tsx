import { ArrowForward } from '@mui/icons-material';
import { Box, Button, Chip, Grid, Paper, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';

export default function HomePage() {
  return (
    <Stack spacing={3}>
      <Paper
        elevation={1}
        sx={{
          p: { xs: 3, md: 4 },
          borderRadius: 3,
          background: 'linear-gradient(135deg, rgba(15,23,42,0.9), rgba(34,211,238,0.12))',
          color: '#e2e8f0',
          border: '1px solid rgba(255,255,255,0.08)',
        }}
      >
        <Stack spacing={1.5}>
          <Typography variant="h4" fontWeight={700}>
            Inicio
          </Typography>
          <Typography variant="body1" color="rgba(226,232,240,0.8)">
            Resumen ejecutivo y accesos rápidos.
          </Typography>
          <Typography variant="body2" color="rgba(226,232,240,0.6)">
            Si necesitas priorizar esta sección, comparte los requisitos con el equipo de producto.
          </Typography>
        </Stack>
      </Paper>

      <Grid container spacing={3}>
        <Grid item xs={12} md={7}>
          <Paper elevation={1} sx={{ p: { xs: 3, md: 4 }, borderRadius: 3 }}>
            <Stack spacing={1.5}>
              <Typography variant="h6" fontWeight={700}>
                Qué hay de nuevo
              </Typography>
              <Typography variant="body1" color="text.secondary">
                Reúne tu equipo y prioriza: esta vista está lista para recibir métricas, accesos rápidos y automatizaciones
                clave de la operación diaria.
              </Typography>
              <Typography variant="body2" color="text.disabled">
                Comparte con producto los KPIs y accesos que necesitas aquí.
              </Typography>
            </Stack>
          </Paper>
        </Grid>

        <Grid item xs={12} md={5}>
          <Paper
            elevation={2}
            sx={{
              p: { xs: 3, md: 4 },
              borderRadius: 3,
              background: 'linear-gradient(145deg, #0ea5e9 0%, #1e293b 90%)',
              color: '#f8fafc',
              display: 'flex',
              flexDirection: 'column',
              gap: 2,
              boxShadow: '0 10px 30px rgba(14,165,233,0.25)',
            }}
          >
            <Stack direction="row" spacing={1} alignItems="center">
              <Chip size="small" label="Fans primero" sx={{ bgcolor: 'rgba(255,255,255,0.14)', color: '#e0f2fe' }} />
              <Typography variant="subtitle2" sx={{ color: 'rgba(248,250,252,0.8)' }}>
                Comunidad
              </Typography>
            </Stack>
            <Typography variant="h5" fontWeight={800} lineHeight={1.25}>
              Fan Hub
            </Typography>
            <Typography variant="body1" sx={{ color: 'rgba(248,250,252,0.85)' }}>
              Fans son ciudadanos de primera clase: centraliza perfiles, follows y lanzamientos en un solo lugar.
            </Typography>
            <Stack spacing={0.5} sx={{ color: 'rgba(248,250,252,0.78)' }}>
              <Typography variant="body2">• Actualiza tu perfil y preferencias</Typography>
              <Typography variant="body2">• Sigue artistas y recibe sus lanzamientos</Typography>
              <Typography variant="body2">• Publica actualizaciones y playlists oficiales</Typography>
            </Stack>
            <Box sx={{ display: 'flex', justifyContent: 'flex-end' }}>
              <Button
                component={RouterLink}
                to="/fans"
                variant="contained"
                endIcon={<ArrowForward />}
                sx={{
                  bgcolor: '#0b90d9',
                  color: '#0a0f1c',
                  fontWeight: 700,
                  px: 2.5,
                  boxShadow: '0 10px 20px rgba(0,0,0,0.25)',
                  '&:hover': { bgcolor: '#08a8ff' },
                }}
              >
                Ir a Fan Hub
              </Button>
            </Box>
          </Paper>
        </Grid>
      </Grid>
    </Stack>
  );
}
