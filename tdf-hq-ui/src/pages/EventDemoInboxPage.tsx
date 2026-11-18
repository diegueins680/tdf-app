import { useQuery } from '@tanstack/react-query';
import { Alert, CircularProgress, Paper, Stack, Typography } from '@mui/material';
import { get } from '../api/client';
import { useSession } from '../session/SessionContext';

interface DemoRequest { inquiryId: number; name?: string; email?: string; message?: string; channel?: string; status: string }

export default function EventDemoInboxPage() {
  const {session} = useSession();
  const result = useQuery({
    queryKey: ['event-demo-inbox', session?.partyId ?? session?.username],
    queryFn: ({signal}) => get<DemoRequest[]>('/admin/event-demo-requests', {signal}),
    enabled: Boolean(session),
    retry: false,
  });
  return (
    <Stack spacing={2}>
      <Typography variant="h4" component="h1">Solicitudes de demos para eventos</Typography>
      {result.isPending && <CircularProgress aria-label="Cargando solicitudes" />}
      {result.isError && <Alert severity="error">No se pudo abrir la bandeja de demos.</Alert>}
      {result.isSuccess && result.data.length === 0 && <Typography>Todavía no hay solicitudes.</Typography>}
      {result.data?.map((row) => (
        <Paper key={row.inquiryId} variant="outlined" sx={{p: 2}}>
          <Typography component="h2" fontWeight={700}>{row.name}</Typography>
          <Typography>{row.email}</Typography>
          <Typography sx={{whiteSpace:'pre-wrap'}}>{row.message}</Typography>
          <Typography color="text.secondary">{row.channel} · {row.status}</Typography>
        </Paper>
      ))}
    </Stack>
  );
}
