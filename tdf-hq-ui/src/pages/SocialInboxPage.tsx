import { useEffect, useMemo, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import AutoFixHighIcon from '@mui/icons-material/AutoFixHigh';
import CloseIcon from '@mui/icons-material/Close';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import SendIcon from '@mui/icons-material/Send';
import {
  Alert,
  Box,
  Button,
  Chip,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Divider,
  IconButton,
  MenuItem,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  TextField,
  Tooltip,
  Typography,
  useMediaQuery,
} from '@mui/material';
import { useTheme } from '@mui/material/styles';
import { SocialInboxAPI, type SocialChannel, type SocialMessage } from '../api/socialInbox';

interface MessageStats {
  incoming: SocialMessage[];
  replied: SocialMessage[];
  pending: SocialMessage[];
  failed: SocialMessage[];
}

type FilterKey = 'all' | 'pending' | 'replied' | 'failed';

const FILTERS: { id: FilterKey; label: string }[] = [
  { id: 'all', label: 'Todos' },
  { id: 'pending', label: 'Pendientes' },
  { id: 'replied', label: 'Respondidos' },
  { id: 'failed', label: 'Fallidos' },
];

const buildStats = (messages: SocialMessage[] | undefined): MessageStats => {
  const incoming = (messages ?? []).filter((msg) => msg.direction === 'incoming');
  const replied = incoming.filter((msg) => Boolean(msg.repliedAt));
  const failed = incoming.filter((msg) => !msg.repliedAt && Boolean(msg.replyError));
  const pending = incoming.filter((msg) => !msg.repliedAt && !msg.replyError);
  return { incoming, replied, pending, failed };
};

const selectMessages = (stats: MessageStats, filter: FilterKey) => {
  switch (filter) {
    case 'pending':
      return stats.pending;
    case 'replied':
      return stats.replied;
    case 'failed':
      return stats.failed;
    default:
      return stats.incoming;
  }
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

const channelToLabel = (channel: SocialChannel) => {
  switch (channel) {
    case 'instagram':
      return 'Instagram';
    case 'facebook':
      return 'Facebook';
    case 'whatsapp':
      return 'WhatsApp';
    default:
      return channel;
  }
};

const copyText = async (value: string) => {
  try {
    await navigator.clipboard.writeText(value);
    return true;
  } catch (err) {
    console.warn('No se pudo copiar al portapapeles', err);
    return false;
  }
};

interface SelectedMessage {
  channel: SocialChannel;
  message: SocialMessage;
}

interface SocialMessageDialogProps {
  selection: SelectedMessage | null;
  onClose: () => void;
  onRefresh: () => void;
}

const SocialMessageDialog = ({ selection, onClose, onRefresh }: SocialMessageDialogProps) => {
  const open = Boolean(selection);
  const channel = selection?.channel;
  const msg = selection?.message;
  const theme = useTheme();
  const fullScreen = useMediaQuery(theme.breakpoints.down('md'));

  const [hint, setHint] = useState('');
  const [replyDraft, setReplyDraft] = useState('');
  const [aiLoading, setAiLoading] = useState(false);
  const [sendLoading, setSendLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [notice, setNotice] = useState<string | null>(null);
  const [optimisticRepliedAt, setOptimisticRepliedAt] = useState<string | null>(null);
  const [optimisticReplyText, setOptimisticReplyText] = useState<string | null>(null);
  const [optimisticReplyError, setOptimisticReplyError] = useState<string | null>(null);

  useEffect(() => {
    if (!open || !msg) return;
    setHint('');
    setReplyDraft((msg.replyText ?? '').trim());
    setAiLoading(false);
    setSendLoading(false);
    setError(null);
    setNotice(null);
    setOptimisticRepliedAt(null);
    setOptimisticReplyText(null);
    setOptimisticReplyError(null);
  }, [open, msg?.externalId]);

  const senderLabel = useMemo(() => {
    if (!msg) return '';
    return msg.senderName ? `${msg.senderName} · ${msg.senderId}` : msg.senderId;
  }, [msg]);

  const canGenerate = Boolean(channel && msg && (msg.text ?? '').trim().length > 0 && !aiLoading && !sendLoading);
  const canSend = Boolean(channel && msg && replyDraft.trim().length > 0 && !sendLoading);

  const handleGenerate = async () => {
    if (!channel || !msg) return;
    const body = msg.text ?? '';
    if (!body.trim()) return;
    setAiLoading(true);
    setError(null);
    setNotice(null);
    try {
      const suggestion = await SocialInboxAPI.suggestReply(channel, body, hint);
      setReplyDraft(suggestion);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'No se pudo generar una respuesta.');
    } finally {
      setAiLoading(false);
    }
  };

  const handleSend = async () => {
    if (!channel || !msg) return;
    setSendLoading(true);
    setError(null);
    setNotice(null);
    try {
      await SocialInboxAPI.sendReply(channel, {
        senderId: msg.senderId,
        message: replyDraft,
        externalId: msg.externalId,
      });
      setOptimisticRepliedAt(new Date().toISOString());
      setOptimisticReplyText(replyDraft.trim());
      setOptimisticReplyError(null);
      setNotice('Respuesta enviada.');
      onRefresh();
    } catch (err) {
      const message = err instanceof Error ? err.message : 'No se pudo enviar la respuesta.';
      setOptimisticRepliedAt(null);
      setOptimisticReplyText(null);
      setOptimisticReplyError(message);
      setError(message);
    } finally {
      setSendLoading(false);
    }
  };

  const handleCopySender = async () => {
    if (!msg) return;
    const ok = await copyText(msg.senderId);
    if (ok) setNotice('Remitente copiado.');
  };

  const handleCopyExternal = async () => {
    if (!msg) return;
    const ok = await copyText(msg.externalId);
    if (ok) setNotice('ID copiado.');
  };

  const handleCopyReply = async () => {
    const body = replyDraft.trim();
    if (!body) return;
    const ok = await copyText(body);
    if (ok) setNotice('Respuesta copiada.');
  };

  const repliedAtValue = optimisticRepliedAt ?? msg?.repliedAt;
  const replyTextValue = optimisticReplyText ?? msg?.replyText;
  const replyErrorValue = optimisticReplyError ?? msg?.replyError;

  return (
    <Dialog open={open} onClose={onClose} fullScreen={fullScreen} fullWidth maxWidth="lg">
      <DialogTitle sx={{ pr: 6 }}>
        <Stack direction="row" spacing={1.5} alignItems="center" justifyContent="space-between">
          <Stack spacing={0.25} sx={{ minWidth: 0 }}>
            <Typography variant="h6" fontWeight={800} noWrap>
              {channel ? channelToLabel(channel) : 'Mensaje'}
            </Typography>
            <Typography variant="caption" color="text.secondary" noWrap>
              {msg ? `${formatTimestamp(msg.createdAt)} · ${msg.repliedAt ? 'Respondido' : msg.replyError ? 'Fallido' : 'Pendiente'}` : ''}
            </Typography>
          </Stack>
          <IconButton aria-label="Cerrar" onClick={onClose}>
            <CloseIcon />
          </IconButton>
        </Stack>
      </DialogTitle>
      <DialogContent dividers sx={{ p: 3 }}>
        {!msg ? (
          <Typography color="text.secondary">Selecciona un mensaje.</Typography>
        ) : (
          <Stack spacing={2.5}>
            <Paper variant="outlined" sx={{ p: 2 }}>
              <Stack spacing={1}>
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5} justifyContent="space-between">
                  <Stack spacing={0.25} sx={{ minWidth: 0 }}>
                    <Typography variant="overline" color="text.secondary">
                      Remitente
                    </Typography>
                    <Stack direction="row" spacing={1} alignItems="center" sx={{ minWidth: 0 }}>
                      <Typography
                        variant="body2"
                        sx={{ fontFamily: 'monospace', fontSize: '0.9rem', overflow: 'hidden', textOverflow: 'ellipsis' }}
                        noWrap
                      >
                        {senderLabel}
                      </Typography>
                      <Tooltip title="Copiar remitente">
                        <IconButton size="small" onClick={() => void handleCopySender()}>
                          <ContentCopyIcon fontSize="inherit" />
                        </IconButton>
                      </Tooltip>
                    </Stack>
                  </Stack>
                  <Stack spacing={0.25} alignItems={{ xs: 'flex-start', sm: 'flex-end' }}>
                    <Typography variant="overline" color="text.secondary">
                      ID
                    </Typography>
                    <Stack direction="row" spacing={1} alignItems="center">
                      <Typography variant="body2" sx={{ fontFamily: 'monospace', fontSize: '0.9rem' }}>
                        {msg.externalId}
                      </Typography>
                      <Tooltip title="Copiar ID">
                        <IconButton size="small" onClick={() => void handleCopyExternal()}>
                          <ContentCopyIcon fontSize="inherit" />
                        </IconButton>
                      </Tooltip>
                    </Stack>
                  </Stack>
                </Stack>

                <Divider />

                <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                  <Box sx={{ flex: 1, minWidth: 0 }}>
                    <Typography variant="overline" color="text.secondary">
                      Mensaje
                    </Typography>
                    <Paper
                      variant="outlined"
                      sx={{
                        p: 1.5,
                        bgcolor: 'rgba(148,163,184,0.08)',
                        borderStyle: 'dashed',
                        mt: 0.5,
                      }}
                    >
                      <Typography variant="body2" sx={{ whiteSpace: 'pre-wrap', wordBreak: 'break-word' }}>
                        {formatBody(msg.text)}
                      </Typography>
                    </Paper>
                  </Box>
                  <Box sx={{ flex: 1, minWidth: 0 }}>
                    <Typography variant="overline" color="text.secondary">
                      Estado
                    </Typography>
                    <Stack spacing={1} sx={{ mt: 0.5 }}>
                      {replyErrorValue && <Alert severity="error">Último intento falló: {replyErrorValue}</Alert>}
                      {repliedAtValue && (
                        <Alert severity="success">
                          Respondido: {formatTimestamp(repliedAtValue)}
                          {replyTextValue ? ` · ${replyTextValue}` : ''}
                        </Alert>
                      )}
                      {!repliedAtValue && !replyErrorValue && (
                        <Alert severity="info" variant="outlined">
                          Pendiente de respuesta.
                        </Alert>
                      )}
                    </Stack>
                  </Box>
                </Stack>
              </Stack>
            </Paper>

            <Paper variant="outlined" sx={{ p: 2 }}>
              <Stack spacing={1.5}>
                <Stack direction="row" spacing={1} alignItems="center" justifyContent="space-between">
                  <Typography variant="subtitle1" fontWeight={800}>
                    Responder
                  </Typography>
                  <Stack direction="row" spacing={1}>
                    <Button
                      variant="outlined"
                      size="small"
                      startIcon={<ContentCopyIcon />}
                      onClick={() => void handleCopyReply()}
                      disabled={!replyDraft.trim()}
                    >
                      Copiar
                    </Button>
                    <Button variant="outlined" size="small" onClick={() => setReplyDraft('')} disabled={!replyDraft.trim()}>
                      Limpiar
                    </Button>
                  </Stack>
                </Stack>

                {notice && (
                  <Alert severity="success" onClose={() => setNotice(null)}>
                    {notice}
                  </Alert>
                )}
                {error && (
                  <Alert severity="error" onClose={() => setError(null)}>
                    {error}
                  </Alert>
                )}

                <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
                  <TextField
                    label="Instrucciones para IA (opcional)"
                    placeholder="ej. Responder breve y ofrecer link de inscripción."
                    value={hint}
                    onChange={(e) => setHint(e.target.value)}
                    disabled={aiLoading || sendLoading}
                    fullWidth
                  />
                  <Button
                    variant="contained"
                    startIcon={<AutoFixHighIcon />}
                    onClick={() => void handleGenerate()}
                    disabled={!canGenerate}
                    sx={{ alignSelf: { xs: 'stretch', md: 'flex-start' }, minWidth: 200 }}
                  >
                    {aiLoading ? 'Generando…' : 'Generar con IA'}
                  </Button>
                </Stack>

                <TextField
                  label="Respuesta"
                  placeholder="Escribe la respuesta..."
                  value={replyDraft}
                  onChange={(e) => setReplyDraft(e.target.value)}
                  disabled={sendLoading}
                  multiline
                  minRows={6}
                  fullWidth
                />
              </Stack>
            </Paper>
          </Stack>
        )}
      </DialogContent>
      <DialogActions sx={{ px: 3, py: 2 }}>
        <Button onClick={onClose} variant="outlined">
          Cerrar
        </Button>
        <Button
          onClick={() => void handleSend()}
          variant="contained"
          startIcon={<SendIcon />}
          disabled={!canSend}
        >
          {sendLoading ? 'Enviando…' : 'Enviar'}
        </Button>
      </DialogActions>
    </Dialog>
  );
};

interface ChannelPanelProps {
  label: string;
  channel: SocialChannel;
  stats: MessageStats;
  messages: SocialMessage[];
  loading: boolean;
  onSelect: (selection: SelectedMessage) => void;
}

const ChannelPanel = ({ label, channel, stats, messages, loading, onSelect }: ChannelPanelProps) => (
  <Paper variant="outlined" sx={{ p: 2, flex: 1, minWidth: 0 }}>
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
      <TableContainer sx={{ maxHeight: 440 }}>
        <Table size="small" stickyHeader>
          <TableHead>
            <TableRow>
              <TableCell sx={{ width: 160 }}>Recibido</TableCell>
              <TableCell sx={{ width: 160 }}>Respondido</TableCell>
              <TableCell sx={{ width: 200 }}>Remitente</TableCell>
              <TableCell>Mensaje</TableCell>
              <TableCell>Respuesta / Error</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {loading && (
              <TableRow>
                <TableCell colSpan={5} align="center">
                  <CircularProgress size={22} />
                </TableCell>
              </TableRow>
            )}
            {!loading && messages.length === 0 && (
              <TableRow>
                <TableCell colSpan={5} align="center">
                  <Typography variant="body2" color="text.secondary">
                    Sin mensajes para este filtro.
                  </Typography>
                </TableCell>
              </TableRow>
            )}
            {!loading &&
              messages.map((msg) => {
                const senderLabel = msg.senderName ? `${msg.senderName} · ${msg.senderId}` : msg.senderId;
                return (
                  <TableRow
                    key={msg.externalId}
                    hover
                    onClick={() => onSelect({ channel, message: msg })}
                    sx={{ cursor: 'pointer' }}
                  >
                    <TableCell>
                      <Typography variant="body2">{formatTimestamp(msg.createdAt)}</Typography>
                    </TableCell>
                    <TableCell>
                      <Typography variant="body2">{formatTimestamp(msg.repliedAt)}</Typography>
                    </TableCell>
                    <TableCell>
                      <Typography variant="body2" sx={{ fontFamily: 'monospace', fontSize: '0.875rem' }}>
                        {senderLabel}
                      </Typography>
                    </TableCell>
                    <TableCell>
                      <Typography variant="body2" sx={{ whiteSpace: 'normal', wordBreak: 'break-word' }}>
                        {formatBody(msg.text)}
                      </Typography>
                    </TableCell>
                    <TableCell>
                      <Typography variant="body2" sx={{ whiteSpace: 'normal', wordBreak: 'break-word' }}>
                        {formatBody(msg.replyError ?? msg.replyText)}
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
);

export default function SocialInboxPage() {
  const [filter, setFilter] = useState<FilterKey>('pending');
  const [limit, setLimit] = useState(100);
  const [selection, setSelection] = useState<SelectedMessage | null>(null);
  const direction = 'incoming' as const;
  const repliedOnly = filter === 'replied';
  const instagramQuery = useQuery({
    queryKey: ['social-inbox', 'instagram', limit, direction, repliedOnly],
    queryFn: () => SocialInboxAPI.listInstagramMessages({ limit, direction, repliedOnly }),
  });
  const facebookQuery = useQuery({
    queryKey: ['social-inbox', 'facebook', limit, direction, repliedOnly],
    queryFn: () => SocialInboxAPI.listFacebookMessages({ limit, direction, repliedOnly }),
  });
  const whatsappQuery = useQuery({
    queryKey: ['social-inbox', 'whatsapp', limit, direction, repliedOnly],
    queryFn: () => SocialInboxAPI.listWhatsAppMessages({ limit, direction, repliedOnly }),
  });
  const instagramStats = useMemo(() => buildStats(instagramQuery.data), [instagramQuery.data]);
  const facebookStats = useMemo(() => buildStats(facebookQuery.data), [facebookQuery.data]);
  const whatsappStats = useMemo(() => buildStats(whatsappQuery.data), [whatsappQuery.data]);
  const instagramMessages = useMemo(() => selectMessages(instagramStats, filter), [instagramStats, filter]);
  const facebookMessages = useMemo(() => selectMessages(facebookStats, filter), [facebookStats, filter]);
  const whatsappMessages = useMemo(() => selectMessages(whatsappStats, filter), [whatsappStats, filter]);
  const refetch = () => {
    void instagramQuery.refetch();
    void facebookQuery.refetch();
    void whatsappQuery.refetch();
  };

  return (
    <Stack spacing={3}>
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ xs: 'flex-start', sm: 'center' }}>
        <Stack spacing={0.5}>
          <Typography variant="h4" fontWeight={800}>
            Inbox social
          </Typography>
          <Typography variant="body2" color="text.secondary">
            Auto respuestas registradas por el cron diario.
          </Typography>
        </Stack>
        <Stack direction="row" spacing={1} alignItems="center">
          <TextField
            select
            label="Limite"
            size="small"
            value={limit}
            onChange={(e) => setLimit(parseInt(e.target.value, 10) || 100)}
            sx={{ minWidth: 120 }}
          >
            {[50, 100, 200].map((value) => (
              <MenuItem key={value} value={value}>
                {value}
              </MenuItem>
            ))}
          </TextField>
          <Button
            variant="outlined"
            size="small"
            onClick={refetch}
            disabled={instagramQuery.isFetching || facebookQuery.isFetching || whatsappQuery.isFetching}
          >
            Actualizar
          </Button>
        </Stack>
      </Stack>
      <Paper variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
        <Stack spacing={1.5}>
          <Typography variant="subtitle2" color="text.secondary">
            Filtro
          </Typography>
          <Stack direction="row" spacing={1} flexWrap="wrap">
            {FILTERS.map((item) => (
              <Chip
                key={item.id}
                label={item.label}
                onClick={() => setFilter(item.id)}
                color={filter === item.id ? 'primary' : 'default'}
                variant={filter === item.id ? 'filled' : 'outlined'}
              />
            ))}
          </Stack>
          {repliedOnly && (
            <Typography variant="caption" color="text.secondary">
              Solo respondidos (filtrado en servidor).
            </Typography>
          )}
        </Stack>
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
      <Stack direction={{ xs: 'column', lg: 'row' }} spacing={2}>
        <ChannelPanel
          label="Instagram"
          channel="instagram"
          stats={instagramStats}
          messages={instagramMessages}
          loading={instagramQuery.isLoading}
          onSelect={(next) => setSelection(next)}
        />
        <ChannelPanel
          label="Facebook"
          channel="facebook"
          stats={facebookStats}
          messages={facebookMessages}
          loading={facebookQuery.isLoading}
          onSelect={(next) => setSelection(next)}
        />
        <ChannelPanel
          label="WhatsApp"
          channel="whatsapp"
          stats={whatsappStats}
          messages={whatsappMessages}
          loading={whatsappQuery.isLoading}
          onSelect={(next) => setSelection(next)}
        />
      </Stack>
      <SocialMessageDialog
        selection={selection}
        onClose={() => setSelection(null)}
        onRefresh={refetch}
      />
    </Stack>
  );
}
