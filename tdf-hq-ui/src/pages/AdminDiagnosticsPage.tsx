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
import { Link as RouterLink } from 'react-router-dom';
import { SocialInboxAPI, type SocialMessage } from '../api/socialInbox';

interface MessageStats {
  incoming: SocialMessage[];
  replied: SocialMessage[];
  pending: SocialMessage[];
  failed: SocialMessage[];
}

const buildStats = (messages: SocialMessage[] | undefined): MessageStats => {
  const incoming = (messages ?? []).filter((msg) => msg.direction === 'incoming');
  const replied = incoming.filter((msg) => Boolean(msg.repliedAt));
  const failed = incoming.filter((msg) => !msg.repliedAt && Boolean(msg.replyError));
  const pending = incoming.filter((msg) => !msg.repliedAt && !msg.replyError);
  return { incoming, replied, pending, failed };
};

const formatTimestamp = (value?: string | null) => {
  if (!value) return '—';
  const parsed = new Date(value);
  if (Number.isNaN(parsed.getTime())) return value;
  return parsed.toLocaleString();
};

const formatBody = (value?: string | null) => {
  const trimmed = value?.trim();
  return trimmed && trimmed.length > 0 ? trimmed : '—';
};

const parseJson = (value?: string | null) => {
  if (!value) return null;
  try {
    return JSON.parse(value) as unknown;
  } catch {
    return null;
  }
};

const coerceText = (value: unknown) => (typeof value === 'string' ? value : undefined);

const extractSenderNameFromMetadata = (metadata?: string | null) => {
  const parsed = parseJson(metadata);
  if (!parsed || typeof parsed !== 'object') return null;
  const root = parsed as Record<string, unknown>;

  const directName = coerceText(root['sender_name']) ?? coerceText(root['senderName']);
  if (directName?.trim()) return directName.trim();

  const from = root['from'];
  if (from && typeof from === 'object') {
    const fromObj = from as Record<string, unknown>;
    const fromName = coerceText(fromObj['name']) ?? coerceText(fromObj['username']);
    if (fromName?.trim()) return fromName.trim();
  }

  const contacts = root['contacts'];
  if (Array.isArray(contacts) && contacts.length > 0) {
    const first = contacts[0];
    if (first && typeof first === 'object') {
      const firstObj = first as Record<string, unknown>;
      const profile = firstObj['profile'];
      if (profile && typeof profile === 'object') {
        const profileName = coerceText((profile as Record<string, unknown>)['name']);
        if (profileName?.trim()) return profileName.trim();
      }
    }
  }

  return null;
};

const resolveSenderName = (msg: SocialMessage) => {
  const senderName = msg.senderName?.trim();
  if (senderName) return senderName;

  const metadataName = extractSenderNameFromMetadata(msg.metadata);
  if (metadataName) return metadataName;

  return 'Sin nombre';
};

const getRepliedHistoryGuidance = (stats: MessageStats) => {
  if (stats.incoming.length === 0) {
    return 'Todavía no hay mensajes entrantes en este canal.';
  }

  return 'El historial detallado aparecerá aquí cuando exista al menos un mensaje respondido.';
};

const getAwaitingReplyHistorySummary = (labels: readonly string[]) =>
  `Todavía no hay mensajes respondidos en ${formatChannelList(labels)}. El historial detallado aparecerá por canal cuando exista la primera respuesta.`;

const CALENDAR_SYNC_PATH = '/configuracion/integraciones/calendario';

const formatChannelList = (labels: readonly string[]) => {
  if (labels.length <= 1) return labels[0] ?? '';
  if (labels.length === 2) return `${labels[0]} y ${labels[1]}`;
  return `${labels.slice(0, -1).join(', ')} y ${labels[labels.length - 1]}`;
};

export default function AdminDiagnosticsPage() {
  const missingEnv =
    typeof window !== 'undefined'
      ? ((window as typeof window & { __MISSING_ENV__?: string[] }).__MISSING_ENV__ ?? [])
      : [];
  const calendarId = typeof window !== 'undefined' ? window.localStorage.getItem('calendar-sync.calendarId') : null;
  const lastSyncAt = typeof window !== 'undefined' ? window.localStorage.getItem('calendar-sync.lastSyncAt') : null;
  const hasCalendarSyncState = Boolean(calendarId || lastSyncAt);
  const instagramQuery = useQuery({
    queryKey: ['social-inbox', 'instagram'],
    queryFn: () => SocialInboxAPI.listInstagramMessages({ direction: 'incoming' }),
  });
  const facebookQuery = useQuery({
    queryKey: ['social-inbox', 'facebook'],
    queryFn: () => SocialInboxAPI.listFacebookMessages({ direction: 'incoming' }),
  });
  const whatsappQuery = useQuery({
    queryKey: ['social-inbox', 'whatsapp'],
    queryFn: () => SocialInboxAPI.listWhatsAppMessages({ direction: 'incoming' }),
  });
  const instagramStats = useMemo(() => buildStats(instagramQuery.data), [instagramQuery.data]);
  const facebookStats = useMemo(() => buildStats(facebookQuery.data), [facebookQuery.data]);
  const whatsappStats = useMemo(() => buildStats(whatsappQuery.data), [whatsappQuery.data]);
  const socialChannels = [
    { label: 'Instagram', stats: instagramStats, loading: instagramQuery.isLoading },
    { label: 'Facebook', stats: facebookStats, loading: facebookQuery.isLoading },
    { label: 'WhatsApp', stats: whatsappStats, loading: whatsappQuery.isLoading },
  ];
  const showGlobalSocialQuietGuidance =
    !instagramQuery.isError
    && !facebookQuery.isError
    && !whatsappQuery.isError
    && socialChannels.every(({ loading, stats }) => !loading && stats.incoming.length === 0);
  const hasSocialQueryError = instagramQuery.isError || facebookQuery.isError || whatsappQuery.isError;
  const visibleSocialChannels = socialChannels.filter(({ loading, stats }) => loading || stats.incoming.length > 0);
  const quietSocialChannelLabels = showGlobalSocialQuietGuidance || hasSocialQueryError
    ? []
    : socialChannels
      .filter(({ loading, stats }) => !loading && stats.incoming.length === 0)
      .map(({ label }) => label);
  const awaitingReplyHistoryChannelLabels = showGlobalSocialQuietGuidance || hasSocialQueryError
    ? []
    : socialChannels
      .filter(({ loading, stats }) => !loading && stats.incoming.length > 0 && stats.replied.length === 0)
      .map(({ label }) => label);
  const showSharedAwaitingReplyHistorySummary = awaitingReplyHistoryChannelLabels.length > 1;
  const showSocialChannelCards = !showGlobalSocialQuietGuidance && visibleSocialChannels.length > 0;
  const showSocialRefreshAction = !showGlobalSocialQuietGuidance;
  const refetchSocialMessages = () => {
    void instagramQuery.refetch();
    void facebookQuery.refetch();
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
        {hasCalendarSyncState ? (
          <>
            <Typography variant="body2" color="text.secondary">
              Calendar ID: {calendarId ?? '—'}
            </Typography>
            <Typography variant="body2" color="text.secondary">
              Última sincronización: {lastSyncAt ?? '—'}
            </Typography>
          </>
        ) : (
          <Alert
            severity="info"
            variant="outlined"
            sx={{ mt: 1 }}
            data-testid="admin-diagnostics-calendar-empty"
            action={(
              <Button color="inherit" size="small" component={RouterLink} to={CALENDAR_SYNC_PATH}>
                Conectar calendario
              </Button>
            )}
          >
            Todavía no hay calendario configurado. Conecta Google Calendar para activar el diagnóstico de sincronización.
          </Alert>
        )}
        {hasCalendarSyncState && (
          <Button
            variant="outlined"
            size="small"
            component={RouterLink}
            to={CALENDAR_SYNC_PATH}
            sx={{ mt: 1 }}
          >
            Abrir sincronización
          </Button>
        )}
      </Paper>
      {(instagramQuery.isError || facebookQuery.isError || whatsappQuery.isError) && (
        <Stack spacing={1}>
          {instagramQuery.isError && (
            <Alert severity="error">
              Instagram: {instagramQuery.error instanceof Error ? instagramQuery.error.message : 'Error inesperado.'}
            </Alert>
          )}
          {facebookQuery.isError && (
            <Alert severity="error">
              Facebook: {facebookQuery.error instanceof Error ? facebookQuery.error.message : 'Error inesperado.'}
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
            {showSocialRefreshAction && (
              <Button
                variant="outlined"
                size="small"
                onClick={refetchSocialMessages}
                disabled={instagramQuery.isFetching || facebookQuery.isFetching || whatsappQuery.isFetching}
              >
                Actualizar mensajes
              </Button>
            )}
          </Stack>
          {showGlobalSocialQuietGuidance && (
            <Alert severity="info" variant="outlined" data-testid="admin-diagnostics-social-quiet-summary">
              Todavía no hay mensajes entrantes en Instagram, Facebook ni WhatsApp.
              Cuando llegue el primero, aquí verás el historial respondido por canal.
            </Alert>
          )}
          {quietSocialChannelLabels.length > 0 && (
            <Alert severity="info" variant="outlined" data-testid="admin-diagnostics-social-partial-quiet-summary">
              Sin mensajes entrantes en {formatChannelList(quietSocialChannelLabels)}.
            </Alert>
          )}
          {showSharedAwaitingReplyHistorySummary && (
            <Alert severity="info" variant="outlined" data-testid="admin-diagnostics-social-awaiting-reply-summary">
              {getAwaitingReplyHistorySummary(awaitingReplyHistoryChannelLabels)}
            </Alert>
          )}
          {showSocialChannelCards && (
            <Stack direction={{ xs: 'column', lg: 'row' }} spacing={2}>
              {visibleSocialChannels.map(({ label, stats, loading }) => (
                <Paper
                  key={label}
                  variant="outlined"
                  sx={{ p: 2, flex: 1, minWidth: 0 }}
                  data-testid="admin-diagnostics-social-channel-card"
                >
                <Stack spacing={1.5}>
                  <Stack direction="row" spacing={1} alignItems="center" justifyContent="space-between">
                    <Typography variant="subtitle1" fontWeight={700}>
                      {label}
                    </Typography>
                    <Chip label={`Entrantes: ${stats.incoming.length}`} size="small" variant="outlined" />
                  </Stack>
                  <Stack direction="row" spacing={1} flexWrap="wrap">
                    {stats.replied.length > 0 && (
                      <Chip label={`Respondidos: ${stats.replied.length}`} size="small" color="success" />
                    )}
                    {stats.pending.length > 0 && (
                      <Chip label={`Pendientes: ${stats.pending.length}`} size="small" color="warning" />
                    )}
                    {stats.failed.length > 0 && (
                      <Chip label={`Fallidos: ${stats.failed.length}`} size="small" color="error" />
                    )}
                  </Stack>
                  {loading ? (
                    <Stack alignItems="center" justifyContent="center" sx={{ minHeight: 120 }}>
                      <CircularProgress size={22} />
                    </Stack>
                  ) : stats.replied.length === 0 ? (
                    showGlobalSocialQuietGuidance || showSharedAwaitingReplyHistorySummary ? null : (
                    <Typography variant="body2" color="text.secondary">
                      {getRepliedHistoryGuidance(stats)}
                    </Typography>
                    )
                  ) : (
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
                          {stats.replied.map((msg) => {
                            const senderLabel = resolveSenderName(msg);
                            return (
                              <TableRow key={msg.externalId} hover>
                                <TableCell>
                                  <Typography variant="body2">{formatTimestamp(msg.repliedAt)}</Typography>
                                </TableCell>
                                <TableCell>
                                  <Typography variant="body2" sx={{ fontSize: '0.9rem', fontWeight: 700 }}>
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
                  )}
                </Stack>
                </Paper>
              ))}
            </Stack>
          )}
        </Stack>
      </Paper>
    </Stack>
  );
}
