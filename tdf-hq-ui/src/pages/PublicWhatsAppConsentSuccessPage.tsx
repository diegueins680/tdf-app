import { Button, Paper, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { WHATSAPP_OPTIN_URL } from '../config/appConfig';

export default function PublicWhatsAppConsentSuccessPage() {
  return (
    <Stack spacing={3}>
      <Paper variant="outlined" sx={{ p: 4, borderRadius: 2 }}>
        <Stack spacing={2}>
          <Typography variant="h4" fontWeight={800}>
            Revisa tu WhatsApp
          </Typography>
          <Typography variant="body1">
            Si el número es válido, recibirás un mensaje de TDF Records. Responde SI a ese mensaje para activar
            tu suscripción; sin tu respuesta no te enviaremos más mensajes.
          </Typography>
          <Typography variant="body2" color="text.secondary">
            Si deseas darte de baja, responde STOP o usa el botón de baja en la página de consentimiento.
          </Typography>
          {WHATSAPP_OPTIN_URL && (
            <Button variant="contained" color="success" href={WHATSAPP_OPTIN_URL} target="_blank" rel="noopener noreferrer">
              Enviar SI por WhatsApp
            </Button>
          )}
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
            <Button variant="outlined" component={RouterLink} to="/whatsapp/consentimiento">
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
