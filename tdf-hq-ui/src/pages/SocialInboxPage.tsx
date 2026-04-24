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
import { Link as RouterLink, useLocation } from 'react-router-dom';
import { SocialInboxAPI, type SocialChannel, type SocialMessage } from '../api/socialInbox';
import { assertNever } from '../utils/assertNever';
import {
  getMetaReviewAssetSelection,
  getInstagramOAuthProvider,
  getInstagramRequestedScopes,
  getStoredInstagramResult,
  type MetaReviewAssetSelection,
} from '../services/instagramAuth';

interface MessageStats {
  incoming: SocialMessage[];
  replied: SocialMessage[];
  pending: SocialMessage[];
  failed: SocialMessage[];
}

type FilterKey = 'all' | 'pending' | 'replied' | 'failed';
type RealFilterKey = Exclude<FilterKey, 'all'>;
const LIMIT_OPTIONS = [50, 100, 200] as const;
const DEFAULT_LIMIT = 100;
const reviewSelectedAssetEmptyStateTitle = 'Waiting for the first inbound message.';
const reviewSelectedAssetEmptyStateMessage =
  'Send one inbound test message to the selected asset and wait a few seconds. This review inbox updates automatically; status filters and channel panels appear after the first inbound message arrives.';

const parseInboxLimit = (value: string, fallback = DEFAULT_LIMIT): number => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed)) return fallback;
  return LIMIT_OPTIONS.some((limit) => limit === parsed) ? parsed : fallback;
};

const FILTERS: { id: FilterKey; label: string; reviewLabel: string }[] = [
  { id: 'all', label: 'Todos', reviewLabel: 'All' },
  { id: 'pending', label: 'Pendientes', reviewLabel: 'Pending' },
  { id: 'replied', label: 'Respondidos', reviewLabel: 'Replied' },
  { id: 'failed', label: 'Fallidos', reviewLabel: 'Failed' },
];

const getFilterLabel = (filter: FilterKey, reviewMode: boolean) => {
  const match = FILTERS.find((item) => item.id === filter);
  if (!match) return filter;
  return reviewMode ? match.reviewLabel : match.label;
};

const buildStats = (messages: SocialMessage[] | undefined): MessageStats => {
  const incoming = (messages ?? []).filter((msg) => msg.direction === 'incoming');
  const replied = incoming.filter((msg) => Boolean(msg.repliedAt));
  const failed = incoming.filter((msg) => !msg.repliedAt && Boolean(msg.replyError));
  const pending = incoming.filter((msg) => !msg.repliedAt && !msg.replyError);
  return { incoming, replied, pending, failed };
};

const selectMessages = (stats: MessageStats, filter: FilterKey) => {
  switch (filter) {
    case 'all':
      return stats.incoming;
    case 'pending':
      return stats.pending;
    case 'replied':
      return stats.replied;
    case 'failed':
      return stats.failed;
  }

  return assertNever(filter, 'message filter');
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

const looksLikeOpaqueSenderValue = (value: string) => /^[A-Za-z0-9+/=_-]{48,}$/.test(value);

const normalizeSenderLabel = (value?: string | null) => {
  const trimmed = value?.trim();
  if (!trimmed) return null;
  if (looksLikeOpaqueSenderValue(trimmed)) return null;
  if (trimmed.length > 70 && !trimmed.includes(' ')) return null;
  return trimmed;
};

const compactIdentifier = (value: string) => {
  const trimmed = value.trim();
  if (trimmed.length <= 22) return trimmed;
  return `${trimmed.slice(0, 10)}…${trimmed.slice(-6)}`;
};

const resolveSenderName = (msg: SocialMessage) => {
  const senderName = normalizeSenderLabel(msg.senderName);
  if (senderName) return senderName;

  const metaName = normalizeSenderLabel(extractSenderNameFromMetadata(msg.metadata));
  if (metaName) return metaName;

  const senderId = msg.senderId?.trim();
  if (senderId) return `ID ${compactIdentifier(senderId)}`;

  return 'Sin nombre';
};

interface ParsedAttachment {
  kind: string;
  label: string;
  url?: string;
}

const coerceText = (value: unknown) => (typeof value === 'string' ? value : undefined);

interface MetaMessageAsset {
  recipientId?: string;
  entryId?: string;
  sourceId?: string;
  sourceType?: string;
}

const extractMetaMessageAsset = (metadata?: string | null): MetaMessageAsset | null => {
  const parsed = parseJson(metadata);
  if (!parsed || typeof parsed !== 'object') return null;
  const root = parsed as Record<string, unknown>;
  const recipientId = coerceText(root['recipient_id']);
  const entryId = coerceText(root['entry_id']);
  const sourceId = coerceText(root['source_id']);
  const sourceType = coerceText(root['source_type']);
  if (!recipientId && !entryId && !sourceId && !sourceType) return null;
  return { recipientId, entryId, sourceId, sourceType };
};

const normalizePhoneForWa = (raw: string) => raw.replace(/[^\d]/g, '');

const resolveNativeClientUrl = (channel: SocialChannel, senderId: string) => {
  switch (channel) {
    case 'instagram':
      return 'https://www.instagram.com/direct/inbox/';
    case 'facebook':
      return 'https://www.facebook.com/messages';
    case 'whatsapp': {
      const normalized = normalizePhoneForWa(senderId);
      return normalized ? `https://wa.me/${normalized}` : 'https://web.whatsapp.com/';
    }
  }

  return assertNever(channel, 'social channel');
};

const extractAttachments = (metadata?: string | null): ParsedAttachment[] => {
  const parsed = parseJson(metadata);
  if (!parsed || typeof parsed !== 'object') return [];

  const attachmentsRaw = (parsed as Record<string, unknown>)['attachments'];
  const items: unknown[] = Array.isArray(attachmentsRaw)
    ? attachmentsRaw
    : attachmentsRaw &&
        typeof attachmentsRaw === 'object' &&
        Array.isArray((attachmentsRaw as Record<string, unknown>)['data'])
      ? ((attachmentsRaw as Record<string, unknown>)['data'] as unknown[])
      : [];

  return items
    .map<ParsedAttachment | null>((item) => {
      if (!item || typeof item !== 'object') return null;
      const obj = item as Record<string, unknown>;

      // Webhook-style: { type, payload: { url, ... } }
      const type = coerceText(obj['type']);
      const payload =
        obj['payload'] && typeof obj['payload'] === 'object' ? (obj['payload'] as Record<string, unknown>) : null;
      const payloadUrl = payload ? coerceText(payload['url']) : undefined;

      // Graph-style: { mime_type, name, image_data: { url }, video_data: { url }, file_url }
      const mimeType = coerceText(obj['mime_type']);
      const name = coerceText(obj['name']);
      const fileUrl = coerceText(obj['file_url']) ?? coerceText(obj['url']);
      const imageData =
        obj['image_data'] && typeof obj['image_data'] === 'object'
          ? (obj['image_data'] as Record<string, unknown>)
          : null;
      const videoData =
        obj['video_data'] && typeof obj['video_data'] === 'object'
          ? (obj['video_data'] as Record<string, unknown>)
          : null;
      const imageUrl = imageData ? coerceText(imageData['url']) ?? coerceText(imageData['preview_url']) : undefined;
      const videoUrl = videoData ? coerceText(videoData['url']) : undefined;

      const url = payloadUrl ?? imageUrl ?? videoUrl ?? fileUrl;
      const kind = type ?? mimeType ?? 'attachment';
      const label = name ?? type ?? mimeType ?? 'Adjunto';
      return { kind, label, url } satisfies ParsedAttachment;
    })
    .filter((value): value is ParsedAttachment => value !== null);
};

const guessIsImage = (attachment: ParsedAttachment) => {
  const kind = attachment.kind.toLowerCase();
  if (kind.includes('image')) return true;
  const url = attachment.url?.toLowerCase();
  if (!url) return false;
  return ['.png', '.jpg', '.jpeg', '.gif', '.webp', '.bmp', '.avif'].some((ext) => url.includes(ext));
};

const isInstagramCdnAsset = (url?: string) => {
  if (!url) return false;
  try {
    const parsed = new URL(url);
    const host = parsed.hostname.toLowerCase();
    return host.endsWith('cdninstagram.com');
  } catch {
    return false;
  }
};

interface ReplyErrorSummary {
  headline: string;
  guidance?: string;
  technical: string;
}

interface InboxErrorSummary {
  headline: string;
  guidance?: string;
  technical: string;
}

const summarizeReplyError = (value: string | null | undefined, reviewMode: boolean): ReplyErrorSummary | null => {
  const technical = value?.replace(/\s+/g, ' ').trim();
  if (!technical) return null;
  const lower = technical.toLowerCase();

  if (lower.includes('instagram_manage_messages') && lower.includes('advanced access')) {
    return {
      headline: reviewMode
        ? 'Delivery blocked: Meta app lacks Advanced Access for Instagram messaging.'
        : 'Envío bloqueado: la app de Meta no tiene Advanced Access para mensajería de Instagram.',
      guidance: reviewMode
        ? 'Grant Advanced Access to instagram_manage_messages, then reconnect the Instagram asset.'
        : 'Activa Advanced Access para instagram_manage_messages y reconecta el asset de Instagram.',
      technical,
    };
  }

  if (lower.includes('recipient user does not have role on app') || lower.includes('2534048')) {
    return {
      headline: reviewMode
        ? 'Delivery blocked: recipient account has no role/tester access on this Meta app.'
        : 'Envío bloqueado: la cuenta destinataria no tiene rol/tester en esta app de Meta.',
      guidance: reviewMode
        ? 'Add the account as tester/developer for review environments, or use a production-enabled account.'
        : 'Agrega la cuenta como tester/developer (entorno review) o usa una cuenta habilitada para producción.',
      technical,
    };
  }

  if (lower.includes('application does not have the capability to make this api call') || lower.includes('"code":3')) {
    return {
      headline: reviewMode
        ? 'Delivery blocked: app permissions/capabilities are missing for this channel.'
        : 'Envío bloqueado: faltan permisos/capacidades de la app para este canal.',
      guidance: reviewMode
        ? 'Validate app mode, granted permissions, and the selected Page/IG asset in Meta settings.'
        : 'Verifica modo de app, permisos otorgados y la Página/asset IG seleccionado en Meta.',
      technical,
    };
  }

  return {
    headline: reviewMode ? 'Message delivery failed.' : 'Falló el envío del mensaje.',
    guidance: reviewMode
      ? 'Review channel credentials, selected asset, and provider permissions.'
      : 'Revisa credenciales del canal, asset seleccionado y permisos del proveedor.',
    technical,
  };
};

const summarizeInboxFetchError = (value: string | null | undefined, reviewMode: boolean): InboxErrorSummary | null => {
  const technical = value?.replace(/\s+/g, ' ').trim();
  if (!technical) return null;
  const lower = technical.toLowerCase();

  if (lower.includes('no se pudo contactar la api')) {
    return {
      headline: reviewMode
        ? 'Cannot load channel messages: backend is unreachable.'
        : 'No se pueden cargar mensajes del canal: backend no disponible.',
      guidance: reviewMode
        ? 'Check API availability, network/CORS, and current VITE_API_BASE.'
        : 'Verifica disponibilidad de la API, red/CORS y VITE_API_BASE.',
      technical,
    };
  }

  if (lower.includes('403') || lower.includes('forbidden')) {
    return {
      headline: reviewMode
        ? 'Cannot load messages: insufficient permissions (403).'
        : 'No se pueden cargar mensajes: permisos insuficientes (403).',
      guidance: reviewMode
        ? 'Review channel credentials and app/page permissions.'
        : 'Revisa credenciales del canal y permisos de app/página.',
      technical,
    };
  }

  if (lower.includes('401') || lower.includes('unauthorized')) {
    return {
      headline: reviewMode
        ? 'Cannot load messages: session/token is invalid (401).'
        : 'No se pueden cargar mensajes: sesión/token inválido (401).',
      guidance: reviewMode
        ? 'Re-authenticate the integration and refresh tokens.'
        : 'Vuelve a autenticar la integración y renueva los tokens.',
      technical,
    };
  }

  return {
    headline: reviewMode ? 'Unable to load channel messages.' : 'No se pudieron cargar mensajes del canal.',
    guidance: reviewMode ? 'Review integration status and try refreshing.' : 'Revisa el estado de integración y vuelve a cargar.',
    technical,
  };
};

const channelToLabel = (channel: SocialChannel) => {
  switch (channel) {
    case 'instagram':
      return 'Instagram';
    case 'facebook':
      return 'Facebook';
    case 'whatsapp':
      return 'WhatsApp';
  }

  return assertNever(channel, 'social channel');
};

const renderAssetField = (name: string, value?: string | null) =>
  value ? `${name}=${value.length > 28 ? compactIdentifier(value) : value}` : null;

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
  reviewMode: boolean;
  activeAsset: MetaReviewAssetSelection | null;
  onClose: () => void;
  onRefresh: () => void;
}

export const SocialMessageDialog = ({ selection, reviewMode, activeAsset, onClose, onRefresh }: SocialMessageDialogProps) => {
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
  const [providerMessageId, setProviderMessageId] = useState<string | null>(null);
  const [showReplyErrorDetails, setShowReplyErrorDetails] = useState(false);
  const [showSendErrorDetails, setShowSendErrorDetails] = useState(false);
  const [failedAttachmentUrls, setFailedAttachmentUrls] = useState<string[]>([]);

  useEffect(() => {
    if (!open || !msg) return;
    setHint('');
    setReplyDraft(msg.repliedAt ? '' : (msg.replyText ?? '').trim());
    setAiLoading(false);
    setSendLoading(false);
    setError(null);
    setNotice(null);
    setOptimisticRepliedAt(null);
    setOptimisticReplyText(null);
    setOptimisticReplyError(null);
    setProviderMessageId(null);
    setShowReplyErrorDetails(false);
    setShowSendErrorDetails(false);
    setFailedAttachmentUrls([]);
  }, [open, msg]);

  const senderLabel = useMemo(() => {
    if (!msg) return '';
    return resolveSenderName(msg);
  }, [msg]);

  const rawBody = (msg?.text ?? '').trim();
  const showBody = rawBody.length > 0 && rawBody.toLowerCase() !== '[attachment]';
  const repliedAtValue = optimisticRepliedAt ?? msg?.repliedAt;
  const replyTextValue = optimisticReplyText ?? msg?.replyText;
  const replyErrorValue = optimisticReplyError ?? msg?.replyError;
  const hasDeliveredReply = Boolean(repliedAtValue);
  const showAiDraftControls = showBody && !hasDeliveredReply;
  const canGenerate = Boolean(channel && msg && showAiDraftControls && !aiLoading && !sendLoading);
  const canSend = Boolean(channel && msg && replyDraft.trim().length > 0 && !sendLoading);
  const hasReplyDraft = replyDraft.trim().length > 0;

  const extractProviderMessageId = (payload: unknown): string | null => {
    if (!payload || typeof payload !== 'object') return null;
    const obj = payload as Record<string, unknown>;
    const direct =
      coerceText(obj['message_id']) ??
      coerceText(obj['messageId']) ??
      coerceText(obj['id']);
    if (direct) return direct;
    const messages = obj['messages'];
    if (Array.isArray(messages) && messages.length > 0) {
      const first = messages[0];
      if (first && typeof first === 'object') {
        const messageId =
          coerceText((first as Record<string, unknown>)['id']) ??
          coerceText((first as Record<string, unknown>)['message_id']);
        if (messageId) return messageId;
      }
    }
    return null;
  };

  const handleGenerate = async () => {
    if (!channel || !msg) return;
    if (!showAiDraftControls) return;
    setAiLoading(true);
    setError(null);
    setNotice(null);
    try {
      const suggestion = await SocialInboxAPI.suggestReply(channel, rawBody, hint);
      setReplyDraft(suggestion);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'No se pudo generar una respuesta.');
    } finally {
      setAiLoading(false);
    }
  };

  const handleSend = async () => {
    if (!channel || !msg) return;
    const outgoingMessage = replyDraft.trim();
    if (!outgoingMessage) return;
    setSendLoading(true);
    setError(null);
    setNotice(null);
    try {
      const response = await SocialInboxAPI.sendReply(channel, {
        senderId: msg.senderId,
        message: outgoingMessage,
        externalId: msg.externalId,
      });
      const outboundId = extractProviderMessageId(response?.response);
      setProviderMessageId(outboundId);
      setOptimisticRepliedAt(new Date().toISOString());
      setOptimisticReplyText(outgoingMessage);
      setOptimisticReplyError(null);
      setReplyDraft('');
      setNotice(reviewMode ? 'Message sent from app UI.' : 'Respuesta enviada.');
      onRefresh();
    } catch (err) {
      const message = err instanceof Error ? err.message : 'No se pudo enviar la respuesta.';
      setOptimisticRepliedAt(null);
      setOptimisticReplyText(null);
      setOptimisticReplyError(message);
      setProviderMessageId(null);
      setError(message);
    } finally {
      setSendLoading(false);
    }
  };

  const handleCopySender = async () => {
    if (!msg) return;
    const ok = await copyText(msg.senderId);
    if (ok) setNotice(reviewMode ? 'Sender copied.' : 'Remitente copiado.');
  };

  const handleCopyExternal = async () => {
    if (!msg) return;
    const ok = await copyText(msg.externalId);
    if (ok) setNotice(reviewMode ? 'Message ID copied.' : 'ID copiado.');
  };

  const handleCopyReply = async () => {
    const body = replyDraft.trim();
    if (!body) return;
    const ok = await copyText(body);
    if (ok) setNotice(reviewMode ? 'Reply copied.' : 'Respuesta copiada.');
  };

  const replyInputLabel = hasDeliveredReply
    ? reviewMode
      ? 'Follow-up message'
      : 'Seguimiento'
    : reviewMode
      ? 'Outgoing message'
      : 'Respuesta';
  const replyInputPlaceholder = hasDeliveredReply
    ? reviewMode
      ? 'Type a follow-up only if another app message is needed...'
      : 'Escribe un seguimiento solo si hace falta otro mensaje...'
    : reviewMode
      ? 'Type the message to send...'
      : 'Escribe la respuesta...';
  const replyInputHelper = hasDeliveredReply
    ? reviewMode
      ? 'Already replied. Send a follow-up only if the review run needs a second app message.'
      : 'Ya respondido. Envia un seguimiento solo si hace falta otro mensaje.'
    : undefined;
  const replyErrorSummary = useMemo(
    () => summarizeReplyError(replyErrorValue, reviewMode),
    [replyErrorValue, reviewMode],
  );
  const sendErrorSummary = useMemo(() => summarizeReplyError(error, reviewMode), [error, reviewMode]);
  const attachments = useMemo(() => extractAttachments(msg?.metadata), [msg?.metadata]);
  const messageAsset = useMemo(() => extractMetaMessageAsset(msg?.metadata), [msg?.metadata]);
  const nativeClientUrl = msg && channel ? resolveNativeClientUrl(channel, msg.senderId) : '';
  const markAttachmentFailed = (url: string) => {
    setFailedAttachmentUrls((prev) => (prev.includes(url) ? prev : [...prev, url]));
  };

  return (
    <Dialog open={open} onClose={onClose} fullScreen={fullScreen} fullWidth maxWidth="lg">
      <DialogTitle sx={{ pr: 6 }}>
        <Stack direction="row" spacing={1.5} alignItems="center" justifyContent="space-between">
          <Stack spacing={0.25} sx={{ minWidth: 0 }}>
            <Typography variant="h6" fontWeight={800} noWrap>
              {channel ? channelToLabel(channel) : 'Mensaje'}
            </Typography>
            <Typography variant="caption" color="text.secondary" noWrap>
              {msg
                ? `${formatTimestamp(msg.createdAt)} · ${
                    msg.repliedAt
                      ? reviewMode
                        ? 'Replied'
                        : 'Respondido'
                      : msg.replyError
                        ? reviewMode
                          ? 'Failed'
                          : 'Fallido'
                        : reviewMode
                          ? 'Pending'
                          : 'Pendiente'
                  }`
                : ''}
            </Typography>
          </Stack>
          <IconButton aria-label={reviewMode ? 'Close' : 'Cerrar'} onClick={onClose}>
            <CloseIcon />
          </IconButton>
        </Stack>
      </DialogTitle>
      <DialogContent dividers sx={{ p: 3 }}>
        {!msg ? (
          <Typography color="text.secondary">{reviewMode ? 'Select a message.' : 'Selecciona un mensaje.'}</Typography>
        ) : (
          <Stack spacing={2.5}>
            {reviewMode && !hasDeliveredReply && (
              <Alert severity="info" variant="outlined">
                Step 2 of 3: keep this dialog visible, click <strong>Send</strong>, then show the same delivered text in the native client.
              </Alert>
            )}
            <Paper variant="outlined" sx={{ p: 2 }}>
              <Stack spacing={1}>
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5} justifyContent="space-between">
                  <Stack spacing={0.25} sx={{ minWidth: 0 }}>
                    <Typography variant="overline" color="text.secondary">
                      {reviewMode ? 'Sender' : 'Remitente'}
                    </Typography>
                    <Stack direction="row" spacing={1} alignItems="center" sx={{ minWidth: 0 }}>
                      <Typography
                        variant="body2"
                        sx={{ fontSize: '0.95rem', fontWeight: 700, overflow: 'hidden', textOverflow: 'ellipsis' }}
                        noWrap
                      >
                        {senderLabel}
                      </Typography>
                      <Tooltip title={reviewMode ? 'Copy sender' : 'Copiar remitente'}>
                        <IconButton
                          size="small"
                          onClick={() => void handleCopySender()}
                          aria-label={reviewMode ? 'Copy sender' : 'Copiar remitente'}
                        >
                          <ContentCopyIcon fontSize="inherit" />
                        </IconButton>
                      </Tooltip>
                    </Stack>
                  </Stack>
                  <Stack spacing={0.25} alignItems={{ xs: 'flex-start', sm: 'flex-end' }}>
                    <Typography variant="overline" color="text.secondary">
                      {reviewMode ? 'Message ID' : 'ID'}
                    </Typography>
                    <Stack direction="row" spacing={1} alignItems="center">
                      <Typography variant="body2" sx={{ fontFamily: 'monospace', fontSize: '0.9rem' }}>
                        {msg.externalId}
                      </Typography>
                      <Tooltip title={reviewMode ? 'Copy ID' : 'Copiar ID'}>
                        <IconButton
                          size="small"
                          onClick={() => void handleCopyExternal()}
                          aria-label={reviewMode ? 'Copy ID' : 'Copiar ID'}
                        >
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
                      {reviewMode ? 'Inbound message' : 'Mensaje'}
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
                      {showBody ? (
                        <Typography variant="body2" sx={{ whiteSpace: 'pre-wrap', wordBreak: 'break-word' }}>
                          {formatBody(msg.text)}
                        </Typography>
                      ) : attachments.length > 0 ? (
                        <Stack spacing={1}>
                          <Typography variant="body2" color="text.secondary">
                            {reviewMode ? 'Attachments' : 'Adjuntos'}
                          </Typography>
                          {attachments.map((att, idx) => (
                            <Paper
                              key={`${att.kind}-${att.url ?? att.label ?? idx}`}
                              variant="outlined"
                              sx={{ p: 1, bgcolor: 'rgba(15,23,42,0.02)' }}
                            >
                              <Stack spacing={0.75}>
                                <Typography variant="caption" color="text.secondary">
                                  {att.label ?? 'Adjunto'}
                                </Typography>
                                {att.url ? (
                                  guessIsImage(att) &&
                                  !failedAttachmentUrls.includes(att.url) &&
                                  !(channel === 'instagram' && isInstagramCdnAsset(att.url)) ? (
                                    <Box
                                      component="img"
                                      src={att.url}
                                      alt={att.label ?? 'Adjunto'}
                                      onError={() => markAttachmentFailed(att.url!)}
                                      sx={{
                                        width: '100%',
                                        maxHeight: 320,
                                        objectFit: 'contain',
                                        borderRadius: 1,
                                        border: '1px solid rgba(15,23,42,0.08)',
                                      }}
                                    />
                                  ) : (
                                    <Stack spacing={0.75}>
                                      {guessIsImage(att) && (
                                        <Alert severity="info" variant="outlined">
                                          {channel === 'instagram' && isInstagramCdnAsset(att.url)
                                            ? reviewMode
                                              ? 'Instagram CDN preview is restricted (403). Open the asset in a new tab.'
                                              : 'La vista previa del CDN de Instagram está restringida (403). Abre el archivo en una nueva pestaña.'
                                            : reviewMode
                                              ? 'Preview unavailable. Open the attachment in a new tab.'
                                              : 'Vista previa no disponible. Abre el adjunto en una nueva pestaña.'}
                                        </Alert>
                                      )}
                                      <Typography
                                        variant="body2"
                                        component="a"
                                        href={att.url}
                                        target="_blank"
                                        rel="noreferrer"
                                        sx={{ wordBreak: 'break-all', color: 'primary.main', textDecoration: 'none' }}
                                      >
                                        {att.url}
                                      </Typography>
                                    </Stack>
                                  )
                                ) : (
                                  <Typography variant="body2" color="text.secondary">
                                    {reviewMode ? '(No URL available)' : '(Sin URL disponible)'}
                                  </Typography>
                                )}
                              </Stack>
                            </Paper>
                          ))}
                        </Stack>
                      ) : (
                        <Typography variant="body2" color="text.secondary">
                          —
                        </Typography>
                      )}
                    </Paper>
                  </Box>
                  <Box sx={{ flex: 1, minWidth: 0 }}>
                    <Typography variant="overline" color="text.secondary">
                      {reviewMode ? 'Delivery status' : 'Estado'}
                    </Typography>
                    <Stack spacing={1} sx={{ mt: 0.5 }}>
                      {replyErrorValue && (
                        <Alert
                          severity="error"
                          action={
                            <Button
                              color="inherit"
                              size="small"
                              onClick={() => setShowReplyErrorDetails((prev) => !prev)}
                            >
                              {showReplyErrorDetails
                                ? reviewMode
                                  ? 'Hide details'
                                  : 'Ocultar detalle'
                                : reviewMode
                                  ? 'Details'
                                  : 'Detalle'}
                            </Button>
                          }
                        >
                          <Stack spacing={0.5}>
                            <Typography variant="body2" fontWeight={700}>
                              {replyErrorSummary?.headline ?? (reviewMode ? 'Last attempt failed.' : 'Último intento falló.')}
                            </Typography>
                            {replyErrorSummary?.guidance && (
                              <Typography variant="body2">{replyErrorSummary.guidance}</Typography>
                            )}
                            {showReplyErrorDetails && (
                              <Typography variant="caption" sx={{ wordBreak: 'break-word', opacity: 0.85 }}>
                                {replyErrorSummary?.technical ?? replyErrorValue}
                              </Typography>
                            )}
                          </Stack>
                        </Alert>
                      )}
                      {repliedAtValue && (
                        <Alert severity="success">
                          {reviewMode ? 'Sent from app UI:' : 'Respondido:'} {formatTimestamp(repliedAtValue)}
                          {replyTextValue ? ` · ${replyTextValue}` : ''}
                        </Alert>
                      )}
                      {providerMessageId && (
                        <Alert severity="info" variant="outlined">
                          {reviewMode ? 'Provider message ID:' : 'ID de mensaje en proveedor:'} {providerMessageId}
                        </Alert>
                      )}
                      {messageAsset && (
                        <Alert severity="info" variant="outlined">
                          {reviewMode ? 'Conversation asset metadata:' : 'Metadata del asset de conversación:'}{' '}
                          {[
                            renderAssetField('recipient_id', messageAsset.recipientId),
                            renderAssetField('entry_id', messageAsset.entryId),
                            renderAssetField('source_id', messageAsset.sourceId),
                            renderAssetField('source_type', messageAsset.sourceType),
                          ]
                            .filter((value): value is string => Boolean(value))
                            .join(' · ')}
                        </Alert>
                      )}
                      {reviewMode && activeAsset && (
                        <Alert severity="info" variant="outlined">
                          Active selected professional/business asset: {activeAsset.pageName} (Page ID: {activeAsset.pageId}
                          {activeAsset.instagramUserId ? ` · IG User ID: ${activeAsset.instagramUserId}` : ''})
                        </Alert>
                      )}
                      {!repliedAtValue && !replyErrorValue && (
                        <Alert severity="info" variant="outlined">
                          {reviewMode ? 'Pending reply.' : 'Pendiente de respuesta.'}
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
                    {reviewMode ? 'Reply from app UI' : 'Responder'}
                  </Typography>
                  {hasReplyDraft && (
                    <Stack direction="row" spacing={1}>
                      <Button
                        variant="outlined"
                        size="small"
                        startIcon={<ContentCopyIcon />}
                        onClick={() => void handleCopyReply()}
                      >
                        {reviewMode ? 'Copy' : 'Copiar'}
                      </Button>
                      <Button variant="outlined" size="small" onClick={() => setReplyDraft('')}>
                        {reviewMode ? 'Clear' : 'Limpiar'}
                      </Button>
                    </Stack>
                  )}
                </Stack>

                {reviewMode && !hasDeliveredReply && (
                  <Alert severity="info" variant="outlined">
                    {showAiDraftControls
                      ? 'Explain each button while recording: AI draft (optional), message textarea, and Send action.'
                      : 'Explain the attachment, message textarea, and Send action while recording. AI draft is hidden because this message has no text body.'}
                  </Alert>
                )}

                {notice && (
                  <Alert severity="success" onClose={() => setNotice(null)}>
                    {notice}
                  </Alert>
                )}
                {reviewMode && replyTextValue && (
                  <Alert
                    severity="success"
                    action={
                      nativeClientUrl ? (
                        <Button
                          color="inherit"
                          size="small"
                          component="a"
                          href={nativeClientUrl}
                          target="_blank"
                          rel="noreferrer"
                        >
                          Open native client
                        </Button>
                      ) : undefined
                    }
                  >
                    Step 3 of 3: show this exact text in the native client (Instagram/Messenger/WhatsApp): “{replyTextValue}”.
                  </Alert>
                )}
                {error && (
                  <Alert
                    severity="error"
                    onClose={() => setError(null)}
                    action={
                      <Button
                        color="inherit"
                        size="small"
                        onClick={() => setShowSendErrorDetails((prev) => !prev)}
                      >
                        {showSendErrorDetails
                          ? reviewMode
                            ? 'Hide details'
                            : 'Ocultar detalle'
                          : reviewMode
                            ? 'Details'
                            : 'Detalle'}
                      </Button>
                    }
                  >
                    <Stack spacing={0.5}>
                      <Typography variant="body2" fontWeight={700}>
                        {sendErrorSummary?.headline ?? error}
                      </Typography>
                      {sendErrorSummary?.guidance && (
                        <Typography variant="body2">{sendErrorSummary.guidance}</Typography>
                      )}
                      {showSendErrorDetails && (
                        <Typography variant="caption" sx={{ wordBreak: 'break-word', opacity: 0.85 }}>
                          {sendErrorSummary?.technical ?? error}
                        </Typography>
                      )}
                    </Stack>
                  </Alert>
                )}

                {showAiDraftControls ? (
                  <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
                    <TextField
                      label={reviewMode ? 'AI instructions (optional)' : 'Instrucciones para IA (opcional)'}
                      placeholder={
                        reviewMode
                          ? 'ex. Keep it concise and offer signup link.'
                          : 'ej. Responder breve y ofrecer link de inscripción.'
                      }
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
                      {aiLoading ? (reviewMode ? 'Generating…' : 'Generando…') : reviewMode ? 'Generate with AI' : 'Generar con IA'}
                    </Button>
                  </Stack>
                ) : !reviewMode && !hasDeliveredReply ? (
                  <Alert severity="info" variant="outlined">
                    La IA se oculta porque este mensaje no tiene texto. Revisa el adjunto, escribe la respuesta y enviala.
                  </Alert>
                ) : null}

                <TextField
                  label={replyInputLabel}
                  placeholder={replyInputPlaceholder}
                  helperText={replyInputHelper}
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
        <Tooltip
          title={
            reviewMode
              ? 'Click to send from app UI. Keep this on screen before switching to native client.'
              : 'Enviar respuesta'
          }
        >
          <span>
            <Button
              onClick={() => void handleSend()}
              variant="contained"
              startIcon={<SendIcon />}
              disabled={!canSend}
            >
              {sendLoading ? (reviewMode ? 'Sending…' : 'Enviando…') : reviewMode ? 'Send message' : 'Enviar'}
            </Button>
          </span>
        </Tooltip>
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
  reviewMode: boolean;
  showStatusChips: boolean;
  onSelect: (selection: SelectedMessage) => void;
}

const ChannelPanel = ({
  label,
  channel,
  stats,
  messages,
  loading,
  reviewMode,
  showStatusChips,
  onSelect,
}: ChannelPanelProps) => {
  const visibleStatusChips = [
    {
      key: 'replied',
      label: reviewMode ? 'Replied' : 'Respondidos',
      count: stats.replied.length,
      color: 'success' as const,
    },
    {
      key: 'pending',
      label: reviewMode ? 'Pending' : 'Pendientes',
      count: stats.pending.length,
      color: 'warning' as const,
    },
    {
      key: 'failed',
      label: reviewMode ? 'Failed' : 'Fallidos',
      count: stats.failed.length,
      color: 'error' as const,
    },
  ].filter((chip) => chip.count > 0);
  const showRepliedAtColumn = loading || messages.some((msg) => Boolean(msg.repliedAt));
  const showReplyOutcomeColumn = loading || messages.some(
    (msg) => Boolean(msg.replyError?.trim()) || Boolean(msg.replyText?.trim()),
  );
  const visibleColumnCount = 3 + (showRepliedAtColumn ? 1 : 0) + (showReplyOutcomeColumn ? 1 : 0);

  return (
    <Paper variant="outlined" sx={{ p: 2, flex: 1, minWidth: 0 }}>
      <Stack spacing={1.5}>
        <Stack direction="row" spacing={1} alignItems="center" justifyContent="space-between">
          <Typography variant="subtitle1" fontWeight={700}>
            {label}
          </Typography>
          <Chip label={`${reviewMode ? 'Inbound' : 'Entrantes'}: ${stats.incoming.length}`} size="small" variant="outlined" />
        </Stack>
        {showStatusChips && visibleStatusChips.length > 0 && (
          <Stack direction="row" spacing={1} flexWrap="wrap">
            {visibleStatusChips.map((chip) => (
              <Chip key={chip.key} label={`${chip.label}: ${chip.count}`} size="small" color={chip.color} />
            ))}
          </Stack>
        )}
        <TableContainer sx={{ maxHeight: 440 }}>
          <Table size="small" stickyHeader>
            <TableHead>
              <TableRow>
                <TableCell sx={{ width: 160 }}>{reviewMode ? 'Received' : 'Recibido'}</TableCell>
                {showRepliedAtColumn && (
                  <TableCell sx={{ width: 160 }}>{reviewMode ? 'Replied' : 'Respondido'}</TableCell>
                )}
                <TableCell sx={{ width: 200 }}>{reviewMode ? 'Sender' : 'Remitente'}</TableCell>
                <TableCell>{reviewMode ? 'Message' : 'Mensaje'}</TableCell>
                {showReplyOutcomeColumn && (
                  <TableCell>{reviewMode ? 'Reply / Error' : 'Respuesta / Error'}</TableCell>
                )}
              </TableRow>
            </TableHead>
            <TableBody>
              {loading && (
                <TableRow>
                  <TableCell colSpan={visibleColumnCount} align="center">
                    <CircularProgress size={22} />
                  </TableCell>
                </TableRow>
              )}
              {!loading && messages.length === 0 && (
                <TableRow>
                  <TableCell colSpan={visibleColumnCount} align="center">
                    <Typography variant="body2" color="text.secondary">
                      {reviewMode ? 'No messages for this filter.' : 'Sin mensajes para este filtro.'}
                    </Typography>
                  </TableCell>
                </TableRow>
              )}
              {!loading &&
                messages.map((msg) => {
                  const senderLabel = resolveSenderName(msg);
                  const attachments = extractAttachments(msg.metadata);
                  const rawBody = (msg.text ?? '').trim();
                  const previewText =
                    rawBody && rawBody.toLowerCase() !== '[attachment]'
                      ? rawBody
                      : attachments.length > 0
                        ? 'Adjunto'
                        : '';
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
                      {showRepliedAtColumn && (
                        <TableCell>
                          <Typography variant="body2">{formatTimestamp(msg.repliedAt)}</Typography>
                        </TableCell>
                      )}
                      <TableCell>
                        <Typography variant="body2" sx={{ fontSize: '0.9rem', fontWeight: 700 }}>
                          {senderLabel}
                        </Typography>
                      </TableCell>
                      <TableCell>
                        <Typography variant="body2" sx={{ whiteSpace: 'normal', wordBreak: 'break-word' }}>
                          {previewText ? formatBody(previewText) : '—'}
                        </Typography>
                      </TableCell>
                      {showReplyOutcomeColumn && (
                        <TableCell>
                          <Typography variant="body2" sx={{ whiteSpace: 'normal', wordBreak: 'break-word' }}>
                            {msg.replyError
                              ? (summarizeReplyError(msg.replyError, reviewMode)?.headline ?? formatBody(msg.replyError))
                              : formatBody(msg.replyText)}
                          </Typography>
                        </TableCell>
                      )}
                    </TableRow>
                  );
                })}
            </TableBody>
          </Table>
        </TableContainer>
      </Stack>
    </Paper>
  );
};

export default function SocialInboxPage() {
  const location = useLocation();
  const reviewMode = useMemo(() => new URLSearchParams(location.search).get('review') === '1', [location.search]);
  const [filter, setFilter] = useState<FilterKey>('pending');
  const [limit, setLimit] = useState(DEFAULT_LIMIT);
  const [selection, setSelection] = useState<SelectedMessage | null>(null);
  const [activeAsset, setActiveAsset] = useState<MetaReviewAssetSelection | null>(() => getMetaReviewAssetSelection());
  const [expandedFetchErrorChannels, setExpandedFetchErrorChannels] = useState<SocialChannel[]>([]);
  const reviewProvider = useMemo(() => getInstagramOAuthProvider(), []);
  const reviewScopes = useMemo(() => getInstagramRequestedScopes(), []);

  useEffect(() => {
    if (!reviewMode) return;
    const stored = getMetaReviewAssetSelection();
    if (stored) {
      setActiveAsset(stored);
      return;
    }
    const fallback = getStoredInstagramResult()?.pages?.[0];
    if (!fallback) {
      setActiveAsset(null);
      return;
    }
    setActiveAsset({
      pageId: fallback.pageId,
      pageName: fallback.pageName,
      instagramUserId: fallback.instagramUserId ?? null,
      instagramUsername: fallback.instagramUsername ?? null,
      selectedAt: Date.now(),
    });
  }, [reviewMode]);

  const direction = 'incoming' as const;
  const repliedOnly = filter === 'replied';
  const instagramQuery = useQuery({
    queryKey: ['social-inbox', 'instagram', limit, direction, repliedOnly],
    queryFn: () => SocialInboxAPI.listInstagramMessages({ limit, direction, repliedOnly }),
    refetchInterval: reviewMode ? 5000 : false,
  });
  const facebookQuery = useQuery({
    queryKey: ['social-inbox', 'facebook', limit, direction, repliedOnly],
    queryFn: () => SocialInboxAPI.listFacebookMessages({ limit, direction, repliedOnly }),
    refetchInterval: reviewMode ? 5000 : false,
  });
  const whatsappQuery = useQuery({
    queryKey: ['social-inbox', 'whatsapp', limit, direction, repliedOnly],
    queryFn: () => SocialInboxAPI.listWhatsAppMessages({ limit, direction, repliedOnly }),
    refetchInterval: reviewMode ? 5000 : false,
  });
  const instagramStats = useMemo(() => buildStats(instagramQuery.data), [instagramQuery.data]);
  const facebookStats = useMemo(() => buildStats(facebookQuery.data), [facebookQuery.data]);
  const whatsappStats = useMemo(() => buildStats(whatsappQuery.data), [whatsappQuery.data]);
  const filterCounts = useMemo<Record<FilterKey, number>>(
    () => ({
      all: instagramStats.incoming.length + facebookStats.incoming.length + whatsappStats.incoming.length,
      pending: instagramStats.pending.length + facebookStats.pending.length + whatsappStats.pending.length,
      replied: instagramStats.replied.length + facebookStats.replied.length + whatsappStats.replied.length,
      failed: instagramStats.failed.length + facebookStats.failed.length + whatsappStats.failed.length,
    }),
    [facebookStats, instagramStats, whatsappStats],
  );
  const nonEmptyRealFilters = useMemo(
    () =>
      FILTERS
        .map((item) => item.id)
        .filter((item): item is RealFilterKey => item !== 'all' && filterCounts[item] > 0),
    [filterCounts],
  );
  const activeFilter =
    filter !== 'all' && filterCounts.all > 0 && filterCounts[filter] === 0 && nonEmptyRealFilters.length > 0
      ? 'all'
      : filter;
  const visibleFilters = useMemo(() => {
    if (filterCounts.all === 0) return FILTERS;
    return FILTERS.filter((item) => item.id === 'all' || item.id === activeFilter || filterCounts[item.id] > 0);
  }, [activeFilter, filterCounts]);
  const hasHiddenFilters = filterCounts.all > 0 && visibleFilters.length < FILTERS.length;
  const singleVisibleFilter = useMemo<RealFilterKey | null>(() => {
    if (filterCounts.all === 0) return null;
    return nonEmptyRealFilters.length === 1 ? (nonEmptyRealFilters[0] ?? null) : null;
  }, [filterCounts.all, nonEmptyRealFilters]);
  const displayFilter = singleVisibleFilter ?? activeFilter;
  const showSingleFilterSummary = Boolean(singleVisibleFilter);
  const singleVisibleFilterLabel = singleVisibleFilter ? getFilterLabel(singleVisibleFilter, reviewMode) : '';
  const showChannelStatusChips = displayFilter === 'all' && !showSingleFilterSummary;
  const instagramMessages = useMemo(() => selectMessages(instagramStats, displayFilter), [instagramStats, displayFilter]);
  const facebookMessages = useMemo(() => selectMessages(facebookStats, displayFilter), [facebookStats, displayFilter]);
  const whatsappMessages = useMemo(() => selectMessages(whatsappStats, displayFilter), [whatsappStats, displayFilter]);
  const channelPanels = useMemo(
    () => [
      {
        label: 'Instagram',
        channel: 'instagram' as const,
        stats: instagramStats,
        messages: instagramMessages,
        loading: instagramQuery.isLoading,
        hasError: instagramQuery.isError,
      },
      {
        label: 'Facebook',
        channel: 'facebook' as const,
        stats: facebookStats,
        messages: facebookMessages,
        loading: facebookQuery.isLoading,
        hasError: facebookQuery.isError,
      },
      {
        label: 'WhatsApp',
        channel: 'whatsapp' as const,
        stats: whatsappStats,
        messages: whatsappMessages,
        loading: whatsappQuery.isLoading,
        hasError: whatsappQuery.isError,
      },
    ],
    [
      facebookMessages,
      facebookQuery.isError,
      facebookQuery.isLoading,
      facebookStats,
      instagramMessages,
      instagramQuery.isError,
      instagramQuery.isLoading,
      instagramStats,
      whatsappMessages,
      whatsappQuery.isError,
      whatsappQuery.isLoading,
      whatsappStats,
    ],
  );
  const hasVisibleChannelMessages = channelPanels.some((panel) => panel.messages.length > 0);
  const visibleChannelPanels = hasVisibleChannelMessages
    ? channelPanels.filter((panel) => panel.loading || panel.messages.length > 0)
    : channelPanels;
  const hiddenEmptyChannelLabels = hasVisibleChannelMessages
    ? channelPanels
        .filter((panel) => !panel.loading && !panel.hasError && panel.messages.length === 0)
        .map((panel) => panel.label)
    : [];
  const allChannelsLoaded = !instagramQuery.isLoading && !facebookQuery.isLoading && !whatsappQuery.isLoading;
  const hasChannelLoadErrors = instagramQuery.isError || facebookQuery.isError || whatsappQuery.isError;
  const hasAnyInboundMessage = filterCounts.all > 0;
  const hasEmptyInbox = !repliedOnly && allChannelsLoaded && !hasChannelLoadErrors && filterCounts.all === 0;
  const showChannelErrorOnlyState = allChannelsLoaded && hasChannelLoadErrors && filterCounts.all === 0;
  const showReviewSetupOnlyState = reviewMode && !activeAsset && hasEmptyInbox;
  const showUnifiedEmptyState = hasEmptyInbox && !showReviewSetupOnlyState;
  const showReviewMessageProofGuidance = reviewMode && Boolean(activeAsset) && hasAnyInboundMessage;
  const viewHitsCurrentLimit = channelPanels.some((panel) => panel.stats.incoming.length >= limit);
  const showLimitControl = limit !== DEFAULT_LIMIT || (!showUnifiedEmptyState && viewHitsCurrentLimit);
  const showEmptyStateRefresh = !reviewMode && showUnifiedEmptyState;
  const showManualRefresh = !reviewMode && !showUnifiedEmptyState;
  const showHeaderControls = showLimitControl || showManualRefresh;
  const activeFilterLabel = getFilterLabel(displayFilter, reviewMode);
  const showStatusFilterEmptyState =
    allChannelsLoaded
    && !hasChannelLoadErrors
    && displayFilter !== 'all'
    && filterCounts.all > 0
    && filterCounts[displayFilter] === 0;
  const refetch = () => {
    void instagramQuery.refetch();
    void facebookQuery.refetch();
    void whatsappQuery.refetch();
  };

  const hasExpandedChannelError = (channel: SocialChannel) => expandedFetchErrorChannels.includes(channel);
  const toggleExpandedChannelError = (channel: SocialChannel) => {
    setExpandedFetchErrorChannels((prev) =>
      prev.includes(channel) ? prev.filter((entry) => entry !== channel) : [...prev, channel],
    );
  };

  const renderChannelLoadError = (channel: SocialChannel, rawError: unknown) => {
    if (!rawError) return null;
    const rawMessage =
      rawError instanceof Error
        ? rawError.message
        : reviewMode
          ? 'Unexpected error.'
          : 'Error inesperado.';
    const summary = summarizeInboxFetchError(rawMessage, reviewMode);
    const expanded = hasExpandedChannelError(channel);
    return (
      <Alert
        severity="error"
        action={
          <Button color="inherit" size="small" onClick={() => toggleExpandedChannelError(channel)}>
            {expanded ? (reviewMode ? 'Hide details' : 'Ocultar detalle') : (reviewMode ? 'Details' : 'Detalle')}
          </Button>
        }
      >
        <Stack spacing={0.5}>
          <Typography variant="body2" fontWeight={700}>
            {channelToLabel(channel)}: {summary?.headline ?? rawMessage}
          </Typography>
          {summary?.guidance && <Typography variant="body2">{summary.guidance}</Typography>}
          {expanded && (
            <Typography variant="caption" sx={{ wordBreak: 'break-word', opacity: 0.85 }}>
              {summary?.technical ?? rawMessage}
            </Typography>
          )}
        </Stack>
      </Alert>
    );
  };

  return (
    <Stack spacing={3}>
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ xs: 'flex-start', sm: 'center' }}>
        <Stack spacing={0.5}>
          <Typography variant="h4" fontWeight={800}>
            {reviewMode ? 'Meta App Review: Messaging Inbox' : 'Inbox social'}
          </Typography>
          <Typography variant="body2" color="text.secondary">
            {reviewMode
              ? activeAsset
                ? hasAnyInboundMessage
                  ? 'Step 2/3: send a live reply from app UI, then show the same message in native client.'
                  : 'Step 1/3 complete: send one inbound test message to the selected professional/business account.'
                : 'Step 1/3: select the exact Page + professional/business account for this review run.'
              : 'Auto respuestas registradas por el cron diario.'}
          </Typography>
        </Stack>
        {showHeaderControls && (
          <Stack direction="row" spacing={1} alignItems="center">
            {showLimitControl && (
              <TextField
                select
                label={reviewMode ? 'Limit' : 'Limite'}
                size="small"
                value={limit}
                onChange={(e) => setLimit(parseInboxLimit(e.target.value))}
                sx={{ minWidth: 120 }}
              >
                {LIMIT_OPTIONS.map((value) => (
                  <MenuItem key={value} value={value}>
                    {value}
                  </MenuItem>
                ))}
              </TextField>
            )}
            {showManualRefresh && (
              <Button
                variant="outlined"
                size="small"
                onClick={refetch}
                disabled={instagramQuery.isFetching || facebookQuery.isFetching || whatsappQuery.isFetching}
              >
                Actualizar
              </Button>
            )}
          </Stack>
        )}
      </Stack>
      {reviewMode && (
        <Paper variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
          <Stack spacing={1.25}>
            <Alert severity="info" variant="outlined">
              <Stack spacing={0.5}>
                <Typography variant="body2" fontWeight={700}>
                  Review run: {reviewProvider === 'instagram' ? 'Instagram Login' : 'Facebook Login'}
                </Typography>
                <Typography variant="body2">Requested scopes: {reviewScopes.join(', ')}</Typography>
                {showReviewMessageProofGuidance && (
                  <>
                    <Typography variant="body2">
                      Proof order: open the inbound thread, send the reply from TDF HQ, show the same message in the native
                      Instagram client, delete or unsend it there, then wait for the inbox auto-refresh.
                    </Typography>
                    <Typography variant="caption" color="text.secondary">
                      Keep this panel visible while recording. It already shows the selected account, inbound message,
                      send action, native-client confirmation, and deleted-message refresh. App Review mode auto-refreshes
                      every 5 seconds.
                    </Typography>
                  </>
                )}
              </Stack>
            </Alert>
            {activeAsset ? (
              <Alert severity="success" variant="outlined">
                Selected professional/business Instagram messaging asset: {activeAsset.pageName} (Page ID: {activeAsset.pageId}
                {activeAsset.instagramUsername ? ` · @${activeAsset.instagramUsername}` : ''}
                {activeAsset.instagramUserId ? ` · IG User ID: ${activeAsset.instagramUserId}` : ''})
              </Alert>
            ) : (
              <Alert severity="warning" variant="outlined">
                No asset selected yet. Go to Instagram setup and select the exact Page + professional/business account first.
              </Alert>
            )}
            <Button component={RouterLink} to="/social/instagram?review=1" variant="outlined" sx={{ alignSelf: 'flex-start' }}>
              {activeAsset ? 'Change selected asset' : 'Select asset in Instagram setup'}
            </Button>
          </Stack>
        </Paper>
      )}
      {showReviewSetupOnlyState ? null : showUnifiedEmptyState ? (
        <Alert
          severity="info"
          variant="outlined"
          action={
            showEmptyStateRefresh ? (
              <Button
                color="inherit"
                size="small"
                onClick={refetch}
                disabled={instagramQuery.isFetching || facebookQuery.isFetching || whatsappQuery.isFetching}
              >
                Actualizar inbox
              </Button>
            ) : undefined
          }
        >
          <Stack spacing={0.5}>
            <Typography variant="body2" fontWeight={700}>
              {reviewMode
                ? activeAsset
                  ? reviewSelectedAssetEmptyStateTitle
                  : 'No inbound messages yet.'
                : 'Todavia no hay mensajes entrantes.'}
            </Typography>
            <Typography variant="body2">
              {reviewMode
                ? activeAsset
                  ? reviewSelectedAssetEmptyStateMessage
                  : 'Select the review asset, send one test message, and wait a few seconds. The inbox updates automatically; status filters and channel panels appear after the first inbound message arrives.'
                : 'Cuando llegue el primer mensaje entrante, aparecera aqui y se activaran los filtros por estado. Usa Actualizar inbox si esperabas uno ahora.'}
            </Typography>
          </Stack>
        </Alert>
      ) : showChannelErrorOnlyState ? null : (
        <>
          <Paper variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
            <Stack spacing={1.5}>
              <Typography variant="subtitle2" color="text.secondary">
                {reviewMode ? 'Filter' : 'Filtro'}
              </Typography>
              {showSingleFilterSummary ? (
                <Stack
                  spacing={0.5}
                  sx={{
                    minHeight: 40,
                    justifyContent: 'center',
                    px: 1.5,
                    py: 1.25,
                    border: '1px solid',
                    borderColor: 'divider',
                    borderRadius: 1,
                  }}
                >
                  <Typography variant="caption" color="text.secondary">
                    {reviewMode ? 'Status available' : 'Estado disponible'}
                  </Typography>
                  <Typography variant="body2" fontWeight={600}>
                    {singleVisibleFilterLabel}
                  </Typography>
                  <Typography variant="caption" color="text.secondary">
                    {reviewMode
                      ? 'No need to filter it: it is the only inbound status in this view.'
                      : 'No hace falta filtrarlo: es el unico estado entrante presente en esta vista.'}
                  </Typography>
                </Stack>
              ) : (
                <>
                  <Stack direction="row" spacing={1} flexWrap="wrap">
                    {visibleFilters.map((item) => {
                      const label = getFilterLabel(item.id, reviewMode);
                      const count = filterCounts[item.id];
                      const showCount = item.id === 'all' || count > 0;
                      return (
                        <Chip
                          key={item.id}
                          label={`${label}${showCount ? ` (${count})` : ''}`}
                          onClick={() => setFilter(item.id)}
                          color={displayFilter === item.id ? 'primary' : 'default'}
                          variant={displayFilter === item.id ? 'filled' : 'outlined'}
                          aria-label={reviewMode ? `Filter inbox by ${label}` : `Filtrar inbox por ${label}`}
                          aria-pressed={displayFilter === item.id}
                        />
                      );
                    })}
                  </Stack>
                  {hasHiddenFilters && (
                    <Typography variant="caption" color="text.secondary">
                      {reviewMode
                        ? 'Only statuses with inbound messages in this view are shown.'
                        : 'Solo aparecen estados con mensajes entrantes en esta vista.'}
                    </Typography>
                  )}
                  {repliedOnly && (
                    <Typography variant="caption" color="text.secondary">
                      {reviewMode ? 'Replied only (server-side filter).' : 'Solo respondidos (filtrado en servidor).'}
                    </Typography>
                  )}
                </>
              )}
            </Stack>
          </Paper>
          {showStatusFilterEmptyState ? (
            <Alert severity="info" variant="outlined">
              <Typography variant="body2">
                {reviewMode
                  ? `No messages match ${activeFilterLabel} in this view. Use All or a status with a count to see existing inbound messages.`
                  : `No hay mensajes con estado ${activeFilterLabel} en esta vista. Cambia a Todos o a un estado con contador para ver los mensajes entrantes existentes.`}
              </Typography>
            </Alert>
          ) : hiddenEmptyChannelLabels.length > 0 && (
            <Typography variant="body2" color="text.secondary">
              {reviewMode
                ? `Showing only channels with messages in this view. No messages right now: ${hiddenEmptyChannelLabels.join(', ')}.`
                : `Mostrando solo canales con mensajes en esta vista. Sin mensajes ahora: ${hiddenEmptyChannelLabels.join(', ')}.`}
            </Typography>
          )}
          {!showStatusFilterEmptyState && (
            <Stack direction={{ xs: 'column', lg: 'row' }} spacing={2}>
              {visibleChannelPanels.map((panel) => (
                <ChannelPanel
                  key={panel.channel}
                  label={panel.label}
                  channel={panel.channel}
                  stats={panel.stats}
                  messages={panel.messages}
                  loading={panel.loading}
                  reviewMode={reviewMode}
                  showStatusChips={showChannelStatusChips}
                  onSelect={(next) => setSelection(next)}
                />
              ))}
            </Stack>
          )}
        </>
      )}
      {(instagramQuery.isError || facebookQuery.isError || whatsappQuery.isError) && (
        <Stack spacing={1}>
          {instagramQuery.isError && renderChannelLoadError('instagram', instagramQuery.error)}
          {facebookQuery.isError && renderChannelLoadError('facebook', facebookQuery.error)}
          {whatsappQuery.isError && renderChannelLoadError('whatsapp', whatsappQuery.error)}
        </Stack>
      )}
      <SocialMessageDialog
        selection={selection}
        reviewMode={reviewMode}
        activeAsset={activeAsset}
        onClose={() => setSelection(null)}
        onRefresh={refetch}
      />
    </Stack>
  );
}
