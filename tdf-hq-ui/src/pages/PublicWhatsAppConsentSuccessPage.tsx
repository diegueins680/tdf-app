import { Button, Paper, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';

export default function PublicWhatsAppConsentSuccessPage() {
  return (
    <Stack spacing={3}>
      <Paper variant="outlined" sx={{ p: 4, borderRadius: 2 }}>
        <Stack spacing={2}>
          <Typography variant="h4" fontWeight={800}>
            Consentimiento registrado
          </Typography>
          <Typography variant="body1">
            Gracias. Hemos registrado tu consentimiento para recibir mensajes por WhatsApp de TDF Records.
          </Typography>
          <Typography variant="body2" color="text.secondary">
            Revisa tu WhatsApp para la confirmacion. Si deseas darte de baja, responde STOP o usa el boton de
            baja en la pagina de consentimiento.
          </Typography>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
            <Button variant="contained" component={RouterLink} to="/whatsapp/consentimiento">
              Volver a consentimiento
            </Button>
            <Button variant="outlined" component={RouterLink} to="/records">
              Ir a TDF Records
            </Button>
          </Stack>
        </Stack>
      </Paper>
    </Stack>
  );
}
