import { Alert, Button, Paper, Stack, Typography } from '@mui/material';

export default function AdminDiagnosticsPage() {
  const missingEnv =
    typeof window !== 'undefined'
      ? ((window as typeof window & { __MISSING_ENV__?: string[] }).__MISSING_ENV__ ?? [])
      : [];
  const calendarId = typeof window !== 'undefined' ? window.localStorage.getItem('calendar-sync.calendarId') ?? '—' : '—';
  const lastSyncAt = typeof window !== 'undefined' ? window.localStorage.getItem('calendar-sync.lastSyncAt') ?? '—' : '—';

  return (
    <Stack spacing={2}>
      <Typography variant="h4" fontWeight={800}>
        Diagnóstico
      </Typography>
      <Paper variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
        <Typography variant="h6">Variables de entorno críticas</Typography>
        {missingEnv.length === 0 ? (
          <Alert severity="success" sx={{ mt: 1 }}>
            No faltan variables críticas detectadas en el cliente.
          </Alert>
        ) : (
          <Alert severity="warning" sx={{ mt: 1 }}>
            Faltan: {missingEnv.join(', ')}
          </Alert>
        )}
      </Paper>
      <Paper variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
        <Typography variant="h6">Sincronización de calendario</Typography>
        <Typography variant="body2" color="text.secondary">
          Calendar ID: {calendarId}
        </Typography>
        <Typography variant="body2" color="text.secondary">
          Última sincronización: {lastSyncAt}
        </Typography>
        <Button
          variant="outlined"
          size="small"
          onClick={() => {
            if (typeof window !== 'undefined') {
              window.location.href = '/calendario/sincronizar';
            }
          }}
          sx={{ mt: 1 }}
        >
          Abrir página de sincronización
        </Button>
      </Paper>
    </Stack>
  );
}
