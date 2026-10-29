import { Box, Button, Stack, Typography } from '@mui/material';

const CAMPAIGN_URL = 'https://api.tdfrecords.net/campaign-platform/';

export default function MusicCampaignsPage() {
  return (
    <Stack spacing={2}>
      <Stack direction="row" alignItems="center" justifyContent="space-between" gap={2}>
        <Typography variant="h4" component="h1">Campañas musicales</Typography>
        <Button component="a" href={CAMPAIGN_URL} target="_blank" rel="noopener noreferrer">
          Abrir en otra pestaña
        </Button>
      </Stack>
      <Typography color="text.secondary">
        Prepara tus piezas, conecta las cuentas que administras y aprueba el calendario.
        Las campañas se ejecutan desde TDF aunque cierres esta ventana.
      </Typography>
      <Box
        component="iframe"
        title="Panel de campañas musicales de TDF"
        src={CAMPAIGN_URL}
        referrerPolicy="no-referrer"
        sx={{ width: '100%', minHeight: '80vh', border: 0, borderRadius: 2, bgcolor: '#160f12' }}
      />
    </Stack>
  );
}
