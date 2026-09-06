import { Card, CardContent, Link, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';

export default function PublicReputationRightsPage({ appeal = false }: { appeal?: boolean }) {
  const title = appeal ? 'Apelaciones de reputación' : 'Privacidad y reputación';
  const body = appeal
    ? 'Puedes reportar o apelar una señal, categoría, badge o resultado agregado. No revelamos la identidad de otros evaluadores.'
    : 'Puedes solicitar acceso, exportación, corrección o eliminación de tus datos de reputación. Las posiciones individuales y la identidad de evaluadores no son públicas.';
  return <Stack spacing={3} sx={{ maxWidth: 760, mx: 'auto', py: 4 }}>
    <Typography variant="h3" fontWeight={800}>{title}</Typography>
    <Typography color="text.secondary">{body}</Typography>
    <Card><CardContent><Stack spacing={2}>
      <Typography>Para iniciar una solicitud, escríbenos a <Link href="mailto:privacidad@tdfrecords.com">privacidad@tdfrecords.com</Link>.</Typography>
      <Typography>Incluye el correo de tu cuenta, el tipo de solicitud y el contexto. Te responderemos sin exponer datos de terceros.</Typography>
      {appeal ? <Link component={RouterLink} to="/feedback">Enviar reporte o apelación</Link> : <Link component={RouterLink} to="/apelaciones">Reportar o apelar un resultado</Link>}
    </Stack></CardContent></Card>
  </Stack>;
}
