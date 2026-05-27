import { useEffect, useMemo, useRef, useState, type KeyboardEvent } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Link as RouterLink } from 'react-router-dom';
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
  Paper,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { Admin, type AdminUser, type UserCommunicationHistory, type WhatsAppMessageAdmin } from '../api/admin';
import LazyPaginatedList from './LazyPaginatedList';

interface AdminUserCommunicationDialogProps {
  open: boolean;
  user: AdminUser | null;
  onClose: () => void;
}

type SendMode = 'reply' | 'notify';

type FeedbackState =
  | { kind: 'success'; message: string }
  | { kind: 'error'; message: string }
  | null;

const COPY = {
  closeAction: 'Cerrar',
  composerLabelNotification: 'Notificación',
  composerLabelReply: 'Respuesta',
  composerPlaceholder: 'Escribe el mensaje que se enviará por WhatsApp.',
  composerTitle: 'Responder o notificar',
  directionIncoming: 'Entrante',
  directionOutgoing: 'Saliente',
  emailLabel: 'Correo:',
  fallbackHistoryError: 'No se pudo cargar el historial.',
  lastStatusLabel: 'Último estado:',
  loadingHistory: 'Cargando historial...',
  messageCountLabel: 'Mensajes:',
  noConfigured: 'No configurado',
  noContent: 'Sin contenido.',
  noDate: 'Sin fecha',
  noMessages: 'No hay mensajes registrados para este usuario.',
  notificationSent: 'Notificación enviada.',
  notifyAction: 'Notificar',
  notifySendingAction: 'Enviando...',
  profileAction: 'Ver perfil',
  replyAction: 'Responder',
  replyCancelAction: 'Cancelar respuesta',
  replyNoticePrefix: 'Responderás al mensaje del',
  replySendingAction: 'Enviando...',
  replySent: 'Respuesta enviada.',
  resendAction: 'Reenviar',
  resendSendingAction: 'Reenviando...',
  resent: 'Mensaje reenviado.',
  title: 'Comunicación WhatsApp',
  userLabel: 'Usuario:',
  unavailableUser: 'Usuario no disponible.',
  whatsappLabel: 'WhatsApp:',
  historyTitle: 'Historial',
} as const;

interface ElementFocusRef {
  current: HTMLElement | null;
}

interface TextareaFocusRef {
  current: HTMLTextAreaElement | null;
}

const ACTION_SPINNER_SIZE_PX = 16;

const focusSoon = (target: () => HTMLElement | null) => {
  globalThis.setTimeout(() => target()?.focus(), 0);
};

const isActivationKey = (key: string) => key === 'Enter' || key === ' ';

const activateOnKeyboard = (event: KeyboardEvent, action: () => void) => {
  if (!isActivationKey(event.key)) return;
  event.preventDefault();
  action();
};

const formatTimestamp = (value?: string | null) => {
  if (!value) return COPY.noDate;
  const parsed = new Date(value);
  if (Number.isNaN(parsed.getTime())) return value;
  return parsed.toLocaleString();
};

const normalizeBody = (value?: string | null) => {
  const trimmed = value?.trim();
  return trimmed && trimmed.length > 0 ? trimmed : COPY.noContent;
};

const labelDirection = (direction: string) => (
  direction === 'incoming' ? COPY.directionIncoming : COPY.directionOutgoing
);

const labelDeliveryStatus = (status: string) => {
  switch (status.toLowerCase()) {
    case 'received':
      return 'Recibido';
    case 'sent':
      return 'Enviado';
    case 'delivered':
      return 'Entregado';
    case 'read':
      return 'Leído';
    case 'failed':
      return 'Fallido';
    default:
      return status;
  }
};

const chipColorForStatus = (status: string): 'default' | 'success' | 'warning' | 'error' | 'info' => {
  switch (status.toLowerCase()) {
    case 'received':
    case 'read':
    case 'sent':
    case 'delivered':
      return 'success';
    case 'pending':
    case 'hold':
      return 'warning';
    case 'error':
    case 'failed':
      return 'error';
    default:
      return 'info';
  }
};

const isIncoming = (message: WhatsAppMessageAdmin) => message.direction === 'incoming';
const isOutgoing = (message: WhatsAppMessageAdmin) => message.direction === 'outgoing';

interface FeedbackNoticeProps {
  feedback: FeedbackState;
  feedbackRef: ElementFocusRef;
}

function FeedbackNotice(props: FeedbackNoticeProps) {
  const { feedback, feedbackRef } = props;
  if (!feedback) return null;

  return (
    <Box ref={feedbackRef} tabIndex={-1} sx={{ outline: 'none' }}>
      <Alert severity={feedback.kind} aria-live="polite">
        {feedback.message}
      </Alert>
    </Box>
  );
}

function HistoryLoadingNotice(props: { loading: boolean }) {
  if (!props.loading) return null;

  return (
    <Typography role="status" aria-live="polite" color="text.secondary">
      {COPY.loadingHistory}
    </Typography>
  );
}

function HistoryErrorNotice(props: { error: unknown }) {
  if (!props.error) return null;
  const message = props.error instanceof Error ? props.error.message : COPY.fallbackHistoryError;

  return <Alert severity="error">{message}</Alert>;
}

interface UserSummaryCardProps {
  actionsDisabled: boolean;
  history: UserCommunicationHistory;
  messageCount: number;
}

function UserSummaryCard(props: UserSummaryCardProps) {
  const { actionsDisabled, history, messageCount } = props;
  const whatsapp = history.whatsapp ?? history.primaryPhone ?? COPY.noConfigured;
  const email = history.primaryEmail ?? COPY.noConfigured;

  return (
    <Paper variant="outlined" sx={{ p: 2 }}>
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between">
        <Box>
          <Typography variant="h6" fontWeight={700}>
            {history.partyName}
          </Typography>
          <Typography variant="body2" color="text.secondary">
            {COPY.userLabel} {history.username}
          </Typography>
          <Typography variant="body2" color="text.secondary">
            {COPY.whatsappLabel} {whatsapp}
          </Typography>
          <Typography variant="body2" color="text.secondary">
            {COPY.emailLabel} {email}
          </Typography>
        </Box>
        <Stack direction="row" spacing={1} alignItems="flex-start" flexWrap="wrap">
          <Chip label={`${COPY.messageCountLabel} ${messageCount}`} size="small" variant="outlined" />
          <Button
            component={RouterLink}
            to={`/perfil/${history.partyId}`}
            variant="outlined"
            disabled={actionsDisabled}
          >
            {COPY.profileAction}
          </Button>
        </Stack>
      </Stack>
    </Paper>
  );
}

interface ReplyTargetNoticeProps {
  actionsDisabled: boolean;
  focusAfterClearReplyTarget: () => void;
  onReplyModeKeyDown: (event: KeyboardEvent) => void;
  replyTarget: WhatsAppMessageAdmin | null;
}

function ReplyTargetNotice(props: ReplyTargetNoticeProps) {
  const { actionsDisabled, focusAfterClearReplyTarget, onReplyModeKeyDown, replyTarget } = props;
  if (!replyTarget) return null;
  const focus = {
    afterClearReplyTarget: focusAfterClearReplyTarget,
  };

  return (
    <Alert
      severity="info"
      action={
        <Button
          color="inherit"
          size="small"
          disabled={actionsDisabled}
          aria-busy={actionsDisabled ? true : undefined}
          aria-keyshortcuts="Escape"
          onClick={focus.afterClearReplyTarget}
          onKeyDown={onReplyModeKeyDown}
        >
          {COPY.replyCancelAction}
        </Button>
      }
    >
      {COPY.replyNoticePrefix} {formatTimestamp(replyTarget.createdAt)}.
    </Alert>
  );
}

interface ReplyComposerProps {
  actionsDisabled: boolean;
  composerRef: TextareaFocusRef;
  draft: string;
  focusAfterClearReplyTarget: () => void;
  focusAfterNotify: () => void;
  focusAfterReply: () => void;
  onDraftChange: (value: string) => void;
  onReplyModeKeyDown: (event: KeyboardEvent) => void;
  pendingMode?: SendMode;
  replyTarget: WhatsAppMessageAdmin | null;
}

function ReplyComposer(props: ReplyComposerProps) {
  const {
    actionsDisabled,
    composerRef,
    draft,
    focusAfterClearReplyTarget,
    focusAfterNotify,
    focusAfterReply,
    onDraftChange,
    onReplyModeKeyDown,
    pendingMode,
    replyTarget,
  } = props;
  const draftIsEmpty = draft.trim().length === 0;
  const notifyIsSending = pendingMode === 'notify';
  const replyIsSending = pendingMode === 'reply';
  const focus = {
    afterClearReplyTarget: focusAfterClearReplyTarget,
    afterNotify: focusAfterNotify,
    afterReply: focusAfterReply,
  };

  return (
    <Paper variant="outlined" sx={{ p: 2 }} onKeyDown={onReplyModeKeyDown}>
      <Stack spacing={1.5}>
        <Typography variant="subtitle1" fontWeight={700}>
          {COPY.composerTitle}
        </Typography>
        <ReplyTargetNotice
          actionsDisabled={actionsDisabled}
          focusAfterClearReplyTarget={focusAfterClearReplyTarget}
          onReplyModeKeyDown={onReplyModeKeyDown}
          replyTarget={replyTarget}
        />
        <TextField
          label={replyTarget ? COPY.composerLabelReply : COPY.composerLabelNotification}
          multiline
          minRows={4}
          value={draft}
          onChange={(event) => onDraftChange(event.target.value)}
          placeholder={COPY.composerPlaceholder}
          inputRef={composerRef}
          fullWidth
        />
        <Stack direction="row" spacing={1} justifyContent="flex-end" flexWrap="wrap">
          <Button
            variant="outlined"
            disabled={actionsDisabled || draftIsEmpty}
            aria-busy={notifyIsSending ? true : undefined}
            startIcon={notifyIsSending ? <CircularProgress size={ACTION_SPINNER_SIZE_PX} color="inherit" /> : undefined}
            onClick={focus.afterNotify}
            onKeyDown={(event) => activateOnKeyboard(event, focus.afterNotify)}
          >
            {notifyIsSending ? COPY.notifySendingAction : COPY.notifyAction}
          </Button>
          {replyTarget && (
            <Button
              variant="contained"
              disabled={actionsDisabled || draftIsEmpty}
              aria-busy={replyIsSending ? true : undefined}
              startIcon={replyIsSending ? <CircularProgress size={ACTION_SPINNER_SIZE_PX} color="inherit" /> : undefined}
              onClick={focus.afterReply}
              onKeyDown={(event) => activateOnKeyboard(event, focus.afterReply)}
            >
              {replyIsSending ? COPY.replySendingAction : COPY.replyAction}
            </Button>
          )}
        </Stack>
      </Stack>
    </Paper>
  );
}

function EmptyHistoryNotice() {
  return (
    <Paper variant="outlined" sx={{ p: 2 }} data-testid="empty-history">
      <Typography color="text.secondary">{COPY.noMessages}</Typography>
    </Paper>
  );
}

function MessageHeader(props: { message: WhatsAppMessageAdmin }) {
  const { message } = props;

  return (
    <Stack direction={{ xs: 'column', md: 'row' }} spacing={1} justifyContent="space-between">
      <Stack direction="row" spacing={1} flexWrap="wrap">
        <Chip
          label={labelDirection(message.direction)}
          size="small"
          color={isIncoming(message) ? 'info' : 'success'}
        />
        <Chip
          label={labelDeliveryStatus(message.deliveryStatus)}
          size="small"
          color={chipColorForStatus(message.deliveryStatus)}
          variant="outlined"
        />
        <Chip
          label={message.replyStatus}
          size="small"
          color={chipColorForStatus(message.replyStatus)}
          variant="outlined"
        />
        {message.source && <Chip label={message.source} size="small" variant="outlined" />}
      </Stack>
      <Typography variant="body2" color="text.secondary">
        {formatTimestamp(message.createdAt)}
      </Typography>
    </Stack>
  );
}

interface MessageSenderBlockProps {
  message: WhatsAppMessageAdmin;
  partyName: string;
}

function MessageSenderBlock(props: MessageSenderBlockProps) {
  const { message, partyName } = props;

  return (
    <Box>
      <Typography variant="body2" color="text.secondary">
        {message.senderName ?? partyName} · {message.phoneE164 ?? message.senderId}
      </Typography>
      {message.contactEmail && (
        <Typography variant="body2" color="text.secondary">
          {message.contactEmail}
        </Typography>
      )}
    </Box>
  );
}

function MessageErrorNotice(props: { message: WhatsAppMessageAdmin }) {
  const error = props.message.deliveryError ?? props.message.replyError;
  if (!error) return null;

  return <Alert severity="error">{error}</Alert>;
}

function DeliveryUpdateNotice(props: { message: WhatsAppMessageAdmin }) {
  const { message } = props;
  if (!message.deliveryUpdatedAt) return null;

  return (
    <Typography variant="caption" color="text.secondary">
      {COPY.lastStatusLabel} {labelDeliveryStatus(message.deliveryStatus)} · {formatTimestamp(message.deliveryUpdatedAt)}
    </Typography>
  );
}

interface MessageActionsProps {
  actionsDisabled: boolean;
  focusAfterReplySelect: (message: WhatsAppMessageAdmin) => void;
  focusAfterResend: (message: WhatsAppMessageAdmin) => void;
  message: WhatsAppMessageAdmin;
  resending: boolean;
}

function MessageActions(props: MessageActionsProps) {
  const { actionsDisabled, focusAfterReplySelect, focusAfterResend, message, resending } = props;
  const focus = {
    afterReplySelect: () => focusAfterReplySelect(message),
    afterResend: () => focusAfterResend(message),
  };

  return (
    <Stack direction="row" spacing={1} justifyContent="flex-end">
      {isIncoming(message) && (
        <Button
          size="small"
          variant="outlined"
          disabled={actionsDisabled}
          onClick={focus.afterReplySelect}
          onKeyDown={(event) => activateOnKeyboard(event, focus.afterReplySelect)}
        >
          {COPY.replyAction}
        </Button>
      )}
      {isOutgoing(message) && (
        <Button
          size="small"
          variant="outlined"
          disabled={actionsDisabled}
          aria-busy={resending ? true : undefined}
          startIcon={resending ? <CircularProgress size={ACTION_SPINNER_SIZE_PX} color="inherit" /> : undefined}
          onClick={focus.afterResend}
          onKeyDown={(event) => activateOnKeyboard(event, focus.afterResend)}
        >
          {resending ? COPY.resendSendingAction : COPY.resendAction}
        </Button>
      )}
    </Stack>
  );
}

interface MessageCardProps {
  actionsDisabled: boolean;
  focusAfterReplySelect: (message: WhatsAppMessageAdmin) => void;
  focusAfterResend: (message: WhatsAppMessageAdmin) => void;
  message: WhatsAppMessageAdmin;
  partyName: string;
  resending: boolean;
}

function MessageCard(props: MessageCardProps) {
  const { actionsDisabled, focusAfterReplySelect, focusAfterResend, message, partyName, resending } = props;

  return (
    <Paper
      variant="outlined"
      sx={{
        p: 2,
        borderColor: isIncoming(message) ? 'rgba(14,165,233,0.35)' : 'rgba(34,197,94,0.35)',
      }}
    >
      <Stack spacing={1.25}>
        <MessageHeader message={message} />
        <MessageSenderBlock message={message} partyName={partyName} />
        <Typography whiteSpace="pre-wrap">{normalizeBody(message.text)}</Typography>
        <MessageErrorNotice message={message} />
        <DeliveryUpdateNotice message={message} />
        <Divider />
        <MessageActions
          actionsDisabled={actionsDisabled}
          focusAfterReplySelect={focusAfterReplySelect}
          focusAfterResend={focusAfterResend}
          message={message}
          resending={resending}
        />
      </Stack>
    </Paper>
  );
}

interface MessageHistoryProps {
  actionsDisabled: boolean;
  focusAfterReplySelect: (message: WhatsAppMessageAdmin) => void;
  focusAfterResend: (message: WhatsAppMessageAdmin) => void;
  messages: WhatsAppMessageAdmin[];
  partyName: string;
  resendingMessageId?: number;
}

function MessageHistory(props: MessageHistoryProps) {
  const {
    actionsDisabled,
    focusAfterReplySelect,
    focusAfterResend,
    messages,
    partyName,
    resendingMessageId,
  } = props;

  return (
    <Stack spacing={1.5}>
      <Typography variant="subtitle1" fontWeight={700}>
        {COPY.historyTitle}
      </Typography>
      {messages.length === 0 ? <EmptyHistoryNotice /> : null}
      <LazyPaginatedList
        items={messages}
        pagination={{ itemLabel: 'mensajes', initialRowsPerPage: 10 }}
        renderItems={(visibleMessages) => (
          <Stack spacing={1.5}>
            {visibleMessages.map((message) => (
              <MessageCard
                key={message.id}
                actionsDisabled={actionsDisabled}
                focusAfterReplySelect={focusAfterReplySelect}
                focusAfterResend={focusAfterResend}
                message={message}
                partyName={partyName}
                resending={resendingMessageId === message.id}
              />
            ))}
          </Stack>
        )}
      />
    </Stack>
  );
}

interface CommunicationContentProps {
  actionsDisabled: boolean;
  composerRef: TextareaFocusRef;
  draft: string;
  feedback: FeedbackState;
  feedbackRef: ElementFocusRef;
  focusAfterClearReplyTarget: () => void;
  focusAfterNotify: () => void;
  focusAfterReply: () => void;
  focusAfterReplySelect: (message: WhatsAppMessageAdmin) => void;
  focusAfterResend: (message: WhatsAppMessageAdmin) => void;
  history?: UserCommunicationHistory;
  historyError: unknown;
  historyLoading: boolean;
  messages: WhatsAppMessageAdmin[];
  onDraftChange: (value: string) => void;
  onReplyModeKeyDown: (event: KeyboardEvent) => void;
  pendingMode?: SendMode;
  replyTarget: WhatsAppMessageAdmin | null;
  resendingMessageId?: number;
}

function CommunicationContent(props: CommunicationContentProps) {
  const {
    actionsDisabled,
    composerRef,
    draft,
    feedback,
    feedbackRef,
    focusAfterClearReplyTarget,
    focusAfterNotify,
    focusAfterReply,
    focusAfterReplySelect,
    focusAfterResend,
    history,
    historyError,
    historyLoading,
    messages,
    onDraftChange,
    onReplyModeKeyDown,
    pendingMode,
    replyTarget,
    resendingMessageId,
  } = props;

  return (
    <Stack spacing={2.5} sx={{ mt: 0.5 }}>
      <FeedbackNotice feedback={feedback} feedbackRef={feedbackRef} />
      <HistoryLoadingNotice loading={historyLoading} />
      <HistoryErrorNotice error={historyError} />
      {history && (
        <Stack spacing={2.5}>
          <UserSummaryCard actionsDisabled={actionsDisabled} history={history} messageCount={messages.length} />
          <ReplyComposer
            actionsDisabled={actionsDisabled}
            composerRef={composerRef}
            draft={draft}
            focusAfterClearReplyTarget={focusAfterClearReplyTarget}
            focusAfterNotify={focusAfterNotify}
            focusAfterReply={focusAfterReply}
            onDraftChange={onDraftChange}
            onReplyModeKeyDown={onReplyModeKeyDown}
            pendingMode={pendingMode}
            replyTarget={replyTarget}
          />
          <MessageHistory
            actionsDisabled={actionsDisabled}
            focusAfterReplySelect={focusAfterReplySelect}
            focusAfterResend={focusAfterResend}
            messages={messages}
            partyName={history.partyName}
            resendingMessageId={resendingMessageId}
          />
        </Stack>
      )}
    </Stack>
  );
}

export default function AdminUserCommunicationDialog({
  open,
  user,
  onClose,
}: AdminUserCommunicationDialogProps) {
  const qc = useQueryClient();
  const composerRef = useRef(null) as TextareaFocusRef;
  const feedbackRef = useRef(null) as ElementFocusRef;
  const [draft, setDraft] = useState('');
  const [replyTarget, setReplyTarget] = useState(null as WhatsAppMessageAdmin | null);
  const [feedback, setFeedback] = useState(null as FeedbackState);

  useEffect(() => {
    if (!open) return;
    setDraft('');
    setReplyTarget(null);
    setFeedback(null);
  }, [open, user?.userId]);

  const historyQuery = useQuery({
    queryKey: ['admin', 'user-communications', user?.userId ?? 'none'],
    queryFn: () => Admin.getUserCommunicationHistory(user?.userId ?? 0),
    enabled: open && Boolean(user?.userId),
  });

  const sendMutation = useMutation({
    mutationFn: async (mode: SendMode) => {
      if (!user) throw new Error(COPY.unavailableUser);
      return Admin.sendUserWhatsApp(user.userId, {
        mode,
        message: draft,
        replyToMessageId: mode === 'reply' ? replyTarget?.id ?? null : null,
      });
    },
    onSuccess: async (response, mode) => {
      setFeedback({
        kind: response.status === 'ok' ? 'success' : 'error',
        message: response.message ?? (mode === 'reply' ? COPY.replySent : COPY.notificationSent),
      });
      if (response.status === 'ok') {
        setDraft('');
        setReplyTarget(null);
      }
      await qc.invalidateQueries({ queryKey: ['admin', 'user-communications', user?.userId ?? 'none'] });
    },
    onError: (error: Error) => {
      setFeedback({ kind: 'error', message: error.message });
    },
  });

  const resendMutation = useMutation({
    mutationFn: async (message: WhatsAppMessageAdmin) => Admin.resendWhatsAppMessage(message.id),
    onSuccess: async (response) => {
      setFeedback({
        kind: response.status === 'ok' ? 'success' : 'error',
        message: response.message ?? COPY.resent,
      });
      await qc.invalidateQueries({ queryKey: ['admin', 'user-communications', user?.userId ?? 'none'] });
    },
    onError: (error: Error) => {
      setFeedback({ kind: 'error', message: error.message });
    },
  });

  const messages = useMemo(() => historyQuery.data?.messages ?? [], [historyQuery.data]);
  const actionsDisabled = sendMutation.isPending || resendMutation.isPending;
  const pendingMode = sendMutation.isPending ? sendMutation.variables : undefined;
  const resendingMessageId = resendMutation.isPending ? resendMutation.variables?.id : undefined;

  const focusComposerSoon = () => focusSoon(() => composerRef.current);
  const focusFeedbackSoon = () => focusSoon(() => feedbackRef.current);

  const focusAfterReplySelect = (message: WhatsAppMessageAdmin) => {
    setReplyTarget(message);
    setFeedback(null);
    focusComposerSoon();
  };

  const focusAfterClearReplyTarget = () => {
    setReplyTarget(null);
    focusComposerSoon();
  };

  const handleReplyModeKeyDown = (event: KeyboardEvent) => {
    if (!replyTarget || event.key !== 'Escape') return;
    event.preventDefault();
    event.stopPropagation();
    focusAfterClearReplyTarget();
  };

  const focusAfterNotify = () => {
    sendMutation.mutate('notify', { onSettled: focusFeedbackSoon });
  };

  const focusAfterReply = () => {
    sendMutation.mutate('reply', { onSettled: focusFeedbackSoon });
  };

  const focusAfterResend = (message: WhatsAppMessageAdmin) => {
    resendMutation.mutate(message, { onSettled: focusFeedbackSoon });
  };

  const handleClose = () => {
    if (actionsDisabled) return;
    onClose();
  };

  const focusAfterClose = () => {
    handleClose();
  };
  const focus = {
    afterClose: focusAfterClose,
  };

  return (
    <Dialog open={open} onClose={handleClose} fullWidth maxWidth="lg">
      <DialogTitle>{COPY.title}</DialogTitle>
      <DialogContent>
        <CommunicationContent
          actionsDisabled={actionsDisabled}
          composerRef={composerRef}
          draft={draft}
          feedback={feedback}
          feedbackRef={feedbackRef}
          focusAfterClearReplyTarget={focusAfterClearReplyTarget}
          focusAfterNotify={focusAfterNotify}
          focusAfterReply={focusAfterReply}
          focusAfterReplySelect={focusAfterReplySelect}
          focusAfterResend={focusAfterResend}
          history={historyQuery.data}
          historyError={historyQuery.error}
          historyLoading={historyQuery.isLoading}
          messages={messages}
          onDraftChange={setDraft}
          onReplyModeKeyDown={handleReplyModeKeyDown}
          pendingMode={pendingMode}
          replyTarget={replyTarget}
          resendingMessageId={resendingMessageId}
        />
      </DialogContent>
      <DialogActions>
        <Button
          disabled={actionsDisabled}
          onClick={focus.afterClose}
          onKeyDown={(event) => activateOnKeyboard(event, focus.afterClose)}
        >
          {COPY.closeAction}
        </Button>
      </DialogActions>
    </Dialog>
  );
}
