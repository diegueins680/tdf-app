import { useParams, useNavigate } from 'react-router-dom';
import { Container, Typography, Box, Button, Card, CardContent, Stack } from '@mui/material';
import SchoolIcon from '@mui/icons-material/School';
import ReceiptIcon from '@mui/icons-material/Receipt';
import EventNoteIcon from '@mui/icons-material/EventNote';

export default function StudentProfilePage() {
  const { id } = useParams<{ id: string }>();
  const navigate = useNavigate();

  if (!id) {
    return (
      <Container sx={{ py: 4 }}>
        <Typography color="error">No se especificó ID de estudiante</Typography>
      </Container>
    );
  }

  return (
    <Container sx={{ py: 4 }}>
      <Box mb={3}>
        <Typography variant="h4" gutterBottom>
          Perfil del Estudiante
        </Typography>
        <Typography color="text.secondary">
          ID: {id}
        </Typography>
      </Box>

      <Stack spacing={2}>
        <Card>
          <CardContent>
            <Typography variant="h6" gutterBottom>
              <SchoolIcon sx={{ mr: 1, verticalAlign: 'middle' }} />
              Acciones rápidas
            </Typography>
            <Stack direction="row" spacing={2} mt={2}>
              <Button
                variant="contained"
                startIcon={<EventNoteIcon />}
                onClick={() => navigate(`/students/${id}/lessons`)}
              >
                Ver Clases
              </Button>
              <Button
                variant="outlined"
                startIcon={<ReceiptIcon />}
                onClick={() => navigate('/finanzas/cobros')}
              >
                Ver Pagos
              </Button>
            </Stack>
          </CardContent>
        </Card>

        <Card>
          <CardContent>
            <Typography variant="h6" gutterBottom>
              Información General
            </Typography>
            <Typography color="text.secondary">
              Los datos del estudiante, saldos de paquetes, consumos y make-ups estarán disponibles próximamente.
            </Typography>
          </CardContent>
        </Card>
      </Stack>
    </Container>
  );
}
