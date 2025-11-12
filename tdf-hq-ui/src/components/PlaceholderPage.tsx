import { Box, Paper, Stack, Typography } from '@mui/material';

interface PlaceholderPageProps {
  title: string;
  description?: string;
}

export default function PlaceholderPage({ title, description }: PlaceholderPageProps) {
  return (
    <Paper elevation={1} sx={{ p: 4 }}>
      <Stack spacing={2}>
        <Box>
          <Typography variant="h4" fontWeight={600} gutterBottom>
            {title}
          </Typography>
          <Typography variant="body1" color="text.secondary">
            {description ?? 'Estamos trabajando en esta vista. Próximamente encontrarás la funcionalidad completa aquí.'}
          </Typography>
        </Box>
        <Typography variant="body2" color="text.disabled">
          Si necesitas priorizar esta sección, comparte los requisitos con el equipo de producto.
        </Typography>
      </Stack>
    </Paper>
  );
}
