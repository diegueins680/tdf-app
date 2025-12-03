import { Box, Card, CardContent, Divider, Link, Stack, Typography } from '@mui/material';

export default function SecurityPage() {
  return (
    <Stack spacing={3}>
      <Stack spacing={0.5}>
        <Typography variant="overline" color="text.secondary">
          Seguridad y privacidad
        </Typography>
        <Typography variant="h4" fontWeight={800}>
          Protección de datos y términos de uso
        </Typography>
        <Typography color="text.secondary">
          Resumen de cómo manejamos la información, los derechos de los usuarios y los canales para
          reportar incidentes o solicitar eliminación de datos.
        </Typography>
      </Stack>

      <Card>
        <CardContent>
          <Stack spacing={2}>
            <Typography variant="h6" fontWeight={700} id="privacidad">
              Política de Privacidad
            </Typography>
            <Typography>
              Recopilamos datos de contacto, actividad de agenda y contenido necesario para operar TDF HQ.
              No vendemos datos a terceros. Solo compartimos información con proveedores esenciales
              (infraestructura, correo, pagos) bajo acuerdos de confidencialidad.
            </Typography>
            <Typography>
              Puedes solicitar acceso, rectificación o eliminación de tus datos escribiendo a{' '}
              <Link href="mailto:seguridad@tdfrecords.com">seguridad@tdfrecords.com</Link>. Para tokens de
              integraciones (Google, etc.) solo almacenamos lo mínimo para proveer la funcionalidad y puedes
              revocar acceso en cualquier momento.
            </Typography>
          </Stack>
        </CardContent>
      </Card>

      <Card>
        <CardContent>
          <Stack spacing={2}>
            <Typography variant="h6" fontWeight={700} id="terminos">
              Términos del Servicio
            </Typography>
            <Typography>
              El uso de TDF HQ está limitado a personal autorizado de TDF Records y clientes invitados.
              Está prohibido el uso malicioso, el acceso no autorizado y el intento de extraer datos fuera
              de las funcionalidades previstas.
            </Typography>
            <Typography>
              Nos reservamos el derecho de suspender cuentas por abuso, incumplimiento de estos términos o
              riesgo de seguridad. Las integraciones de terceros (ej. Google Calendar) se rigen también por
              sus propios términos.
            </Typography>
          </Stack>
        </CardContent>
      </Card>

      <Card>
        <CardContent>
          <Stack spacing={2}>
            <Typography variant="h6" fontWeight={700}>
              Contacto y reportes
            </Typography>
            <Typography>
              Para reportar vulnerabilidades o incidentes de seguridad:{' '}
              <Link href="mailto:seguridad@tdfrecords.com">seguridad@tdfrecords.com</Link>.
            </Typography>
            <Typography>
              Para solicitudes de datos personales (acceso, rectificación, eliminación):{' '}
              <Link href="mailto:privacidad@tdfrecords.com">privacidad@tdfrecords.com</Link>.
            </Typography>
            <Divider />
            <Box>
              <Typography variant="subtitle2" color="text.secondary">
                Última actualización:
              </Typography>
              <Typography>02 de diciembre de 2025</Typography>
            </Box>
          </Stack>
        </CardContent>
      </Card>
    </Stack>
  );
}
