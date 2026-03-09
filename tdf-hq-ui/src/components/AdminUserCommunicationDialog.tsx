import { useEffect, useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Link as RouterLink } from 'react-router-dom';
import {
  Alert,
  Box,
  Button,
  Chip,
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

interface AdminUserCommunicationDialogProps {
  open: boolean;
  user: AdminUser | null;
  onClose: () => void;
}

type FeedbackState =
  | { kind: 'success'; message: string }
  | { kind: 'error'; message: string }
  | null;

const formatTimestamp = (value?: string | null) => {
  if (!value) return 'Sin fecha';
  const parsed = new Date(value);
  if (Number.isNaN(parsed.getTime())) return value;
  return parsed.toLocaleString();
};

const normalizeBody = (value?: string | null) => {
  const trimmed = value?.trim();
  return trimmed && trimmed.length > 0 ? trimmed : 'Sin contenido.';
};

const labelDirection = (direction: string) => (direction === 'incoming' ? 'Entrante' : 'Saliente');

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

export default function AdminUserCommunicationDialog({
  open,
  user,
  onClose,
}: AdminUserCommunicationDialogProps) {
  const qc = useQueryClient();
  const [draft, setDraft] = useState('');
  const [replyTarget, setReplyTarget] = useState<WhatsAppMessageAdmin | null>(null);
  const [feedback, setFeedback] = useState<FeedbackState>(null);

  useEffect(() => {
    if (!open) return;
    setDraft('');
    setReplyTarget(null);
    setFeedback(null);
  }, [open, user?.userId]);

  const historyQuery = useQuery<UserCommunicationHistory>({
    queryKey: ['admin', 'user-communications', user?.userId ?? 'none'],
    queryFn: () => Admin.getUserCommunicationHistory(user?.userId ?? 0),
    enabled: open && Boolean(user?.userId),
  });

  const sendMutation = useMutation({
    mutationFn: async (mode: 'reply' | 'notify') => {
      if (!user) throw new Error('Usuario no disponible.');
      return Admin.sendUserWhatsApp(user.userId, {
        mode,
        message: draft,
        replyToMessageId: mode === 'reply' ? replyTarget?.id ?? null : null,
      });
    },
    onSuccess: async (response, mode) => {
      setFeedback({
        kind: response.status === 'ok' ? 'success' : 'error',
        message: response.message ?? (mode === 'reply' ? 'Respuesta enviada.' : 'Notificación enviada.'),
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
        message: response.message ?? 'Mensaje reenviado.',
      });
      await qc.invalidateQueries({ queryKey: ['admin', 'user-communications', user?.userId ?? 'none'] });
    },
    onError: (error: Error) => {
      setFeedback({ kind: 'error', message: error.message });
    },
  });

  const messages = useMemo(() => historyQuery.data?.messages ?? [], [historyQuery.data]);

  const handleReplySelect = (message: WhatsAppMessageAdmin) => {
    setReplyTarget(message);
    setFeedback(null);
  };

  const handleClose = () => {
    if (sendMutation.isPending || resendMutation.isPending) return;
    onClose();
  };

  return (
    <Dialog open={open} onClose={handleClose} fullWidth maxWidth="lg">
      <DialogTitle>Comunicación WhatsApp</DialogTitle>
      <DialogContent>
        <Stack spacing={2.5} sx={{ mt: 0.5 }}>
          {feedback && <Alert severity={feedback.kind}>{feedback.message}</Alert>}
          {historyQuery.isLoading && <Typography>Cargando historial…</Typography>}
          {historyQuery.isError && (
            <Alert severity="error">
              {historyQuery.error instanceof Error ? historyQuery.error.message : 'No se pudo cargar el historial.'}
            </Alert>
          )}
          {historyQuery.data && (
            <>
              <Paper variant="outlined" sx={{ p: 2 }}>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between">
                  <Box>
                    <Typography variant="h6" fontWeight={700}>
                      {historyQuery.data.partyName}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      Usuario: {historyQuery.data.username}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      WhatsApp: {historyQuery.data.whatsapp ?? historyQuery.data.primaryPhone ?? 'No configurado'}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      Correo: {historyQuery.data.primaryEmail ?? 'No configurado'}
                    </Typography>
                  </Box>
                  <Stack direction="row" spacing={1} alignItems="flex-start" flexWrap="wrap">
                    <Chip label={`Mensajes: ${messages.length}`} size="small" variant="outlined" />
                    <Button component={RouterLink} to={`/perfil/${historyQuery.data.partyId}`} variant="outlined">
                      Ver perfil
                    </Button>
                  </Stack>
                </Stack>
              </Paper>

              <Paper variant="outlined" sx={{ p: 2 }}>
                <Stack spacing={1.5}>
                  <Typography variant="subtitle1" fontWeight={700}>
                    Responder o notificar
                  </Typography>
                  {replyTarget && (
                    <Alert
                      severity="info"
                      action={
                        <Button color="inherit" size="small" onClick={() => setReplyTarget(null)}>
                          Limpiar
                        </Button>
                      }
                    >
                      Responderás al mensaje del {formatTimestamp(replyTarget.createdAt)}.
                    </Alert>
                  )}
                  <TextField
                    label={replyTarget ? 'Respuesta' : 'Notificación'}
                    multiline
                    minRows={4}
                    value={draft}
                    onChange={(event) => setDraft(event.target.value)}
                    placeholder="Escribe el mensaje que se enviará por WhatsApp."
                    fullWidth
                  />
                  <Stack direction="row" spacing={1} justifyContent="flex-end" flexWrap="wrap">
                    <Button
                      variant="outlined"
                      disabled={sendMutation.isPending || !draft.trim()}
                      onClick={() => sendMutation.mutate('notify')}
                    >
                      Notificar
                    </Button>
                    <Button
                      variant="contained"
                      disabled={sendMutation.isPending || !draft.trim() || !replyTarget}
                      onClick={() => sendMutation.mutate('reply')}
                    >
                      Responder
                    </Button>
                  </Stack>
                </Stack>
              </Paper>

              <Stack spacing={1.5}>
                <Typography variant="subtitle1" fontWeight={700}>
                  Historial
                </Typography>
                {messages.length === 0 && (
                  <Paper variant="outlined" sx={{ p: 2 }}>
                    <Typography color="text.secondary">
                      No hay mensajes registrados para este usuario.
                    </Typography>
                  </Paper>
                )}
                {messages.map((message) => (
                  <Paper
                    key={message.id}
                    variant="outlined"
                    sx={{
                      p: 2,
                      borderColor: isIncoming(message) ? 'rgba(14,165,233,0.35)' : 'rgba(34,197,94,0.35)',
                    }}
                  >
                    <Stack spacing={1.25}>
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

                      <Box>
                        <Typography variant="body2" color="text.secondary">
                          {message.senderName ?? historyQuery.data.partyName} · {message.phoneE164 ?? message.senderId}
                        </Typography>
                        {message.contactEmail && (
                          <Typography variant="body2" color="text.secondary">
                            {message.contactEmail}
                          </Typography>
                        )}
                  </Box>

                      <Typography whiteSpace="pre-wrap">{normalizeBody(message.text)}</Typography>

                      {(message.deliveryError ?? message.replyError) && (
                        <Alert severity="error">
                          {message.deliveryError ?? message.replyError}
                        </Alert>
                      )}

                      {message.deliveryUpdatedAt && (
                        <Typography variant="caption" color="text.secondary">
                          Último estado: {labelDeliveryStatus(message.deliveryStatus)} · {formatTimestamp(message.deliveryUpdatedAt)}
                        </Typography>
                      )}

                      <Divider />

                      <Stack direction="row" spacing={1} justifyContent="flex-end">
                        {isIncoming(message) && (
                          <Button size="small" variant="outlined" onClick={() => handleReplySelect(message)}>
                            Responder
                          </Button>
                        )}
                        {isOutgoing(message) && (
                          <Button
                            size="small"
                            variant="outlined"
                            disabled={resendMutation.isPending}
                            onClick={() => resendMutation.mutate(message)}
                          >
                            Reenviar
                          </Button>
                        )}
                      </Stack>
                    </Stack>
                  </Paper>
                ))}
              </Stack>
            </>
          )}
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={handleClose} disabled={sendMutation.isPending || resendMutation.isPending}>
          Cerrar
        </Button>
      </DialogActions>
    </Dialog>
  );
}
