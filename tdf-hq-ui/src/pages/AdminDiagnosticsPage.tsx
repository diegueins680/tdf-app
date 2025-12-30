import { useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import {
  Alert,
  Button,
  Chip,
  CircularProgress,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Typography,
} from '@mui/material';
import { SocialInboxAPI, type SocialMessage } from '../api/socialInbox';

type MessageStats = {
  incoming: SocialMessage[];
  replied: SocialMessage[];
  pending: SocialMessage[];
  failed: SocialMessage[];
};

const buildStats = (messages: SocialMessage[] | undefined): MessageStats => {
  const incoming = (messages ?? []).filter((msg) => msg.direction === 'incoming');
  const replied = incoming.filter((msg) => Boolean(msg.repliedAt));
  const failed = incoming.filter((msg) => !msg.repliedAt && Boolean(msg.replyError));
  const pending = incoming.filter((msg) => !msg.repliedAt && !msg.replyError);
  return { incoming, replied, pending, failed };
};

const formatTimestamp = (value?: string | null) => {
  if (!value) return '—';
  try {
    return new Date(value).toLocaleString();
  } catch {
    return value;
  }
};

const formatBody = (value?: string | null) => {
  const trimmed = value?.trim();
  return trimmed && trimmed.length > 0 ? trimmed : '—';
};

export default function AdminDiagnosticsPage() {
  const missingEnv =
    typeof window !== 'undefined'
      ? ((window as typeof window & { __MISSING_ENV__?: string[] }).__MISSING_ENV__ ?? [])
      : [];
  const calendarId = typeof window !== 'undefined' ? window.localStorage.getItem('calendar-sync.calendarId') ?? '—' : '—';
  const lastSyncAt = typeof window !== 'undefined' ? window.localStorage.getItem('calendar-sync.lastSyncAt') ?? '—' : '—';
  const instagramQuery = useQuery({
    queryKey: ['social-inbox', 'instagram'],
    queryFn: () => SocialInboxAPI.listInstagramMessages({ direction: 'incoming' }),
  });
  const whatsappQuery = useQuery({
    queryKey: ['social-inbox', 'whatsapp'],
    queryFn: () => SocialInboxAPI.listWhatsAppMessages({ direction: 'incoming' }),
  });
  const instagramStats = useMemo(() => buildStats(instagramQuery.data), [instagramQuery.data]);
  const whatsappStats = useMemo(() => buildStats(whatsappQuery.data), [whatsappQuery.data]);
  const socialChannels = [
    { label: 'Instagram', stats: instagramStats, loading: instagramQuery.isLoading },
    { label: 'WhatsApp', stats: whatsappStats, loading: whatsappQuery.isLoading },
  ];
  const refetchSocialMessages = () => {
    void instagramQuery.refetch();
    void whatsappQuery.refetch();
  };

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
      {(instagramQuery.isError || whatsappQuery.isError) && (
        <Stack spacing={1}>
          {instagramQuery.isError && (
            <Alert severity="error">
              Instagram: {instagramQuery.error instanceof Error ? instagramQuery.error.message : 'Error inesperado.'}
            </Alert>
          )}
          {whatsappQuery.isError && (
            <Alert severity="error">
              WhatsApp: {whatsappQuery.error instanceof Error ? whatsappQuery.error.message : 'Error inesperado.'}
            </Alert>
          )}
        </Stack>
      )}
      <Paper variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
        <Stack spacing={2}>
          <Stack direction="row" spacing={2} alignItems="center" justifyContent="space-between">
            <Typography variant="h6">Auto respuestas sociales</Typography>
            <Button
              variant="outlined"
              size="small"
              onClick={refetchSocialMessages}
              disabled={instagramQuery.isFetching || whatsappQuery.isFetching}
            >
              Actualizar mensajes
            </Button>
          </Stack>
          <Stack direction={{ xs: 'column', lg: 'row' }} spacing={2}>
            {socialChannels.map(({ label, stats, loading }) => (
              <Paper key={label} variant="outlined" sx={{ p: 2, flex: 1, minWidth: 0 }}>
                <Stack spacing={1.5}>
                  <Stack direction="row" spacing={1} alignItems="center" justifyContent="space-between">
                    <Typography variant="subtitle1" fontWeight={700}>
                      {label}
                    </Typography>
                    <Chip label={`Entrantes: ${stats.incoming.length}`} size="small" variant="outlined" />
                  </Stack>
                  <Stack direction="row" spacing={1} flexWrap="wrap">
                    <Chip label={`Respondidos: ${stats.replied.length}`} size="small" color="success" />
                    <Chip label={`Pendientes: ${stats.pending.length}`} size="small" color="warning" />
                    <Chip
                      label={`Fallidos: ${stats.failed.length}`}
                      size="small"
                      color={stats.failed.length > 0 ? 'error' : 'default'}
                      variant={stats.failed.length > 0 ? 'filled' : 'outlined'}
                    />
                  </Stack>
                  <TableContainer sx={{ maxHeight: 320 }}>
                    <Table size="small" stickyHeader>
                      <TableHead>
                        <TableRow>
                          <TableCell sx={{ width: 170 }}>Respondido</TableCell>
                          <TableCell sx={{ width: 180 }}>Remitente</TableCell>
                          <TableCell>Mensaje</TableCell>
                          <TableCell>Respuesta</TableCell>
                        </TableRow>
                      </TableHead>
                      <TableBody>
                        {loading && (
                          <TableRow>
                            <TableCell colSpan={4} align="center">
                              <CircularProgress size={22} />
                            </TableCell>
                          </TableRow>
                        )}
                        {!loading && stats.replied.length === 0 && (
                          <TableRow>
                            <TableCell colSpan={4} align="center">
                              <Typography variant="body2" color="text.secondary">
                                Sin mensajes respondidos.
                              </Typography>
                            </TableCell>
                          </TableRow>
                        )}
                        {!loading &&
                          stats.replied.map((msg) => {
                            const senderLabel = msg.senderName ? `${msg.senderName} · ${msg.senderId}` : msg.senderId;
                            return (
                              <TableRow key={msg.externalId} hover>
                                <TableCell>
                                  <Typography variant="body2">{formatTimestamp(msg.repliedAt)}</Typography>
                                </TableCell>
                                <TableCell>
                                  <Typography variant="body2" sx={{ fontFamily: 'monospace', fontSize: '0.875rem' }}>
                                    {senderLabel}
                                  </Typography>
                                </TableCell>
                                <TableCell>
                                  <Typography
                                    variant="body2"
                                    sx={{ whiteSpace: 'normal', wordBreak: 'break-word' }}
                                  >
                                    {formatBody(msg.text)}
                                  </Typography>
                                </TableCell>
                                <TableCell>
                                  <Typography
                                    variant="body2"
                                    sx={{ whiteSpace: 'normal', wordBreak: 'break-word' }}
                                  >
                                    {formatBody(msg.replyText)}
                                  </Typography>
                                </TableCell>
                              </TableRow>
                            );
                          })}
                      </TableBody>
                    </Table>
                  </TableContainer>
                </Stack>
              </Paper>
            ))}
          </Stack>
        </Stack>
      </Paper>
    </Stack>
  );
}
