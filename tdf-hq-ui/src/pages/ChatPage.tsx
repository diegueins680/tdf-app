import { useEffect, useMemo, useRef, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Divider,
  IconButton,
  List,
  ListItemButton,
  ListItemText,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import SendIcon from '@mui/icons-material/Send';
import AddCommentIcon from '@mui/icons-material/AddComment';
import RefreshIcon from '@mui/icons-material/Refresh';
import { ChatAPI } from '../api/chat';
import type { ChatMessageDTO, ChatThreadDTO } from '../api/types';
import { useSession } from '../session/SessionContext';

function formatTimestamp(value?: string | null) {
  if (!value) return '';
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return '';
  return date.toLocaleString();
}

function truncate(value: string, max: number) {
  const trimmed = value.trim();
  if (trimmed.length <= max) return trimmed;
  return `${trimmed.slice(0, max - 1)}…`;
}

export default function ChatPage() {
  const qc = useQueryClient();
  const { session } = useSession();
  const myPartyId = session?.partyId ?? null;
  const [selectedThreadId, setSelectedThreadId] = useState<number | null>(null);
  const [draft, setDraft] = useState('');
  const [newChatOpen, setNewChatOpen] = useState(false);
  const [newChatPartyId, setNewChatPartyId] = useState('');
  const [newChatError, setNewChatError] = useState<string | null>(null);
  const [sendError, setSendError] = useState<string | null>(null);
  const messagesEndRef = useRef<HTMLDivElement | null>(null);

  const threadsQuery = useQuery({
    queryKey: ['chat-threads'],
    queryFn: ChatAPI.listThreads,
    refetchInterval: 10_000,
  });

  const threads = useMemo(() => threadsQuery.data ?? [], [threadsQuery.data]);

  useEffect(() => {
    if (selectedThreadId) return;
    const first = threads.at(0);
    if (first) setSelectedThreadId(first.ctThreadId);
  }, [selectedThreadId, threads]);

  const selectedThread = useMemo<ChatThreadDTO | null>(() => {
    if (!selectedThreadId) return null;
    return threads.find((t) => t.ctThreadId === selectedThreadId) ?? null;
  }, [selectedThreadId, threads]);

  const messagesQuery = useQuery({
    queryKey: ['chat-messages', selectedThreadId],
    queryFn: () => ChatAPI.listMessages(selectedThreadId as number, { limit: 80 }),
    enabled: Boolean(selectedThreadId),
    refetchInterval: 3_000,
  });

  const messages = useMemo<ChatMessageDTO[]>(
    () => messagesQuery.data ?? [],
    [messagesQuery.data],
  );

  useEffect(() => {
    if (!selectedThreadId) return;
    messagesEndRef.current?.scrollIntoView({ block: 'end', behavior: 'smooth' });
  }, [messages.length, selectedThreadId]);

  const createThreadMutation = useMutation<ChatThreadDTO, Error, number>({
    mutationFn: (otherPartyId) => ChatAPI.getOrCreateDmThread(otherPartyId),
    onSuccess: async (thread) => {
      setNewChatOpen(false);
      setNewChatPartyId('');
      setNewChatError(null);
      setSelectedThreadId(thread.ctThreadId);
      await qc.invalidateQueries({ queryKey: ['chat-threads'] });
    },
    onError: (err) => setNewChatError(err.message),
  });

  const sendMutation = useMutation<ChatMessageDTO, Error, string>({
    mutationFn: (body) => ChatAPI.sendMessage(selectedThreadId as number, body),
    onSuccess: async () => {
      setDraft('');
      setSendError(null);
      await qc.invalidateQueries({ queryKey: ['chat-messages', selectedThreadId] });
      await qc.invalidateQueries({ queryKey: ['chat-threads'] });
    },
    onError: (err) => setSendError(err.message),
  });

  const handleStartChat = () => {
    setNewChatOpen(true);
    setNewChatError(null);
  };

  const handleCreateChat = () => {
    const numeric = Number(newChatPartyId.trim());
    if (!Number.isFinite(numeric) || numeric <= 0) {
      setNewChatError('Ingresa un Party ID válido.');
      return;
    }
    createThreadMutation.mutate(numeric);
  };

  const handleSend = () => {
    if (!selectedThreadId) {
      setSendError('Selecciona una conversación.');
      return;
    }
    const body = draft.trim();
    if (!body) {
      setSendError('Escribe un mensaje.');
      return;
    }
    sendMutation.mutate(body);
  };

  const threadsLoading = threadsQuery.isLoading;
  const threadsError = threadsQuery.isError ? (threadsQuery.error as Error).message : null;
  const messagesLoading = messagesQuery.isLoading;
  const messagesError = messagesQuery.isError ? (messagesQuery.error as Error).message : null;

  return (
    <Box>
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} alignItems={{ xs: 'stretch', md: 'center' }} sx={{ mb: 2 }}>
        <Box sx={{ flex: 1 }}>
          <Typography variant="h4" fontWeight={800}>Chat</Typography>
          <Typography color="text.secondary">
            Mensajería 1:1 entre usuarios (Party IDs).
          </Typography>
        </Box>
        <Stack direction="row" spacing={1}>
          <Button variant="outlined" startIcon={<AddCommentIcon />} onClick={handleStartChat}>
            Nuevo chat
          </Button>
          <Button
            variant="text"
            startIcon={<RefreshIcon />}
            onClick={() => void qc.invalidateQueries({ queryKey: ['chat-threads'] })}
          >
            Refrescar
          </Button>
        </Stack>
      </Stack>

      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} sx={{ minHeight: 520 }}>
        <Card sx={{ width: { xs: '100%', md: 360 }, flexShrink: 0 }}>
          <CardContent sx={{ p: 0 }}>
            <Box sx={{ px: 2, py: 1.5 }}>
              <Typography fontWeight={700}>Conversaciones</Typography>
            </Box>
            <Divider />
            {threadsLoading ? (
              <Box sx={{ p: 2, display: 'flex', justifyContent: 'center' }}>
                <CircularProgress size={24} />
              </Box>
            ) : threadsError ? (
              <Box sx={{ p: 2 }}>
                <Alert severity="error">{threadsError}</Alert>
              </Box>
            ) : threads.length === 0 ? (
              <Box sx={{ p: 2 }}>
                <Alert severity="info">No tienes conversaciones todavía. Crea una con “Nuevo chat”.</Alert>
              </Box>
            ) : (
              <List dense disablePadding sx={{ maxHeight: { xs: 360, md: 560 }, overflowY: 'auto' }}>
                {threads.map((t) => (
                  <ListItemButton
                    key={t.ctThreadId}
                    selected={t.ctThreadId === selectedThreadId}
                    onClick={() => setSelectedThreadId(t.ctThreadId)}
                    sx={{ px: 2, py: 1.25 }}
                  >
                    <ListItemText
                      primary={
                        <Stack direction="row" spacing={1} alignItems="baseline" justifyContent="space-between">
                          <Typography fontWeight={700} sx={{ mr: 1 }} noWrap>
                            {t.ctOtherDisplayName || `Party #${t.ctOtherPartyId}`}
                          </Typography>
                          <Typography variant="caption" color="text.secondary" noWrap>
                            {formatTimestamp(t.ctLastMessageAt || t.ctUpdatedAt)}
                          </Typography>
                        </Stack>
                      }
                      secondary={
                        t.ctLastMessage
                          ? truncate(t.ctLastMessage, 64)
                          : 'Sin mensajes aún'
                      }
                      secondaryTypographyProps={{ noWrap: true }}
                    />
                  </ListItemButton>
                ))}
              </List>
            )}
          </CardContent>
        </Card>

        <Card sx={{ flex: 1, minWidth: 0 }}>
          <CardContent sx={{ display: 'flex', flexDirection: 'column', height: { xs: 'auto', md: 600 } }}>
            <Box sx={{ mb: 1 }}>
              <Typography fontWeight={800} noWrap>
                {selectedThread ? selectedThread.ctOtherDisplayName : 'Selecciona una conversación'}
              </Typography>
              {selectedThread && (
                <Typography variant="caption" color="text.secondary">
                  Thread #{selectedThread.ctThreadId} · Party #{selectedThread.ctOtherPartyId}
                </Typography>
              )}
            </Box>
            <Divider sx={{ mb: 2 }} />

            <Box sx={{ flex: 1, overflowY: 'auto', pr: 1, minHeight: 240 }}>
              {messagesLoading ? (
                <Box sx={{ display: 'flex', justifyContent: 'center', py: 4 }}>
                  <CircularProgress size={24} />
                </Box>
              ) : messagesError ? (
                <Alert severity="error">{messagesError}</Alert>
              ) : selectedThreadId && messages.length === 0 ? (
                <Alert severity="info">Aún no hay mensajes. Escribe el primero.</Alert>
              ) : (
                <Stack spacing={1.5}>
                  {messages.map((m) => {
                    const mine = myPartyId !== null && m.cmSenderPartyId === myPartyId;
                    return (
                      <Box
                        key={m.cmId}
                        sx={{
                          display: 'flex',
                          flexDirection: 'column',
                          alignItems: mine ? 'flex-end' : 'flex-start',
                        }}
                      >
                        <Box
                          sx={{
                            px: 2,
                            py: 1.25,
                            borderRadius: 2,
                            maxWidth: '78%',
                            bgcolor: mine ? 'primary.main' : 'rgba(148,163,184,0.14)',
                            color: mine ? 'primary.contrastText' : 'text.primary',
                            border: mine ? '1px solid rgba(0,0,0,0.08)' : '1px solid rgba(148,163,184,0.25)',
                            whiteSpace: 'pre-wrap',
                            wordBreak: 'break-word',
                          }}
                        >
                          <Typography variant="body2">{m.cmBody}</Typography>
                        </Box>
                        <Typography
                          variant="caption"
                          color="text.secondary"
                          sx={{ mt: 0.25, textAlign: mine ? 'right' : 'left' }}
                        >
                          {formatTimestamp(m.cmCreatedAt)}
                        </Typography>
                      </Box>
                    );
                  })}
                  <div ref={messagesEndRef} />
                </Stack>
              )}
            </Box>

            <Divider sx={{ my: 2 }} />
            {sendError && (
              <Alert severity="error" sx={{ mb: 1 }}>
                {sendError}
              </Alert>
            )}
            <Stack direction="row" spacing={1} alignItems="flex-end">
              <TextField
                fullWidth
                multiline
                minRows={1}
                maxRows={4}
                placeholder={selectedThreadId ? 'Escribe un mensaje…' : 'Selecciona una conversación…'}
                value={draft}
                disabled={!selectedThreadId || sendMutation.isPending}
                onChange={(e) => setDraft(e.target.value)}
                onKeyDown={(e) => {
                  if (e.key === 'Enter' && !e.shiftKey) {
                    e.preventDefault();
                    handleSend();
                  }
                }}
              />
              <IconButton
                color="primary"
                onClick={handleSend}
                disabled={!selectedThreadId || sendMutation.isPending || !draft.trim()}
              >
                <SendIcon />
              </IconButton>
            </Stack>
          </CardContent>
        </Card>
      </Stack>

      <Dialog open={newChatOpen} onClose={() => setNewChatOpen(false)} maxWidth="xs" fullWidth>
        <DialogTitle>Nuevo chat</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            <Typography variant="body2" color="text.secondary">
              Ingresa el Party ID del usuario con quien quieres chatear. Por defecto solo puedes chatear con amigos mutuos.
            </Typography>
            <TextField
              autoFocus
              label="Party ID"
              value={newChatPartyId}
              onChange={(e) => setNewChatPartyId(e.target.value)}
              inputMode="numeric"
              placeholder="Ej: 123"
              disabled={createThreadMutation.isPending}
            />
            {newChatError && <Alert severity="error">{newChatError}</Alert>}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setNewChatOpen(false)} disabled={createThreadMutation.isPending}>Cancelar</Button>
          <Button variant="contained" onClick={handleCreateChat} disabled={createThreadMutation.isPending}>
            Crear
          </Button>
        </DialogActions>
      </Dialog>
    </Box>
  );
}

