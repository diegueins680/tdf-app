import { useEffect, useMemo, useRef, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
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
import ChatBubbleOutlineIcon from '@mui/icons-material/ChatBubbleOutline';
import SendIcon from '@mui/icons-material/Send';
import AddCommentIcon from '@mui/icons-material/AddComment';
import RefreshIcon from '@mui/icons-material/Refresh';
import FiberManualRecordIcon from '@mui/icons-material/FiberManualRecord';
import { useLocation, useNavigate } from 'react-router-dom';
import { ChatAPI } from '../api/chat';
import { Meta } from '../api/meta';
import { Parties } from '../api/parties';
import { SocialAPI } from '../api/social';
import type { ChatMessageDTO, ChatThreadDTO } from '../api/types';
import { useSession } from '../session/SessionContext';
import { countUnreadThreads, isThreadUnread, loadChatReadMap, markThreadSeen, subscribeToChatReadState } from '../utils/chatReadState';

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

interface FriendOption {
  partyId: number;
  label: string;
  subtitle: string;
}

const SELECTED_THREAD_STORAGE_KEY = 'tdf-chat-selected-thread-v1';

const loadSelectedThreadId = (): number | null => {
  if (typeof window === 'undefined') return null;
  try {
    const raw = window.localStorage.getItem(SELECTED_THREAD_STORAGE_KEY);
    if (!raw) return null;
    const numeric = Number(raw);
    if (!Number.isFinite(numeric) || numeric <= 0) return null;
    return numeric;
  } catch {
    return null;
  }
};

const storeSelectedThreadId = (threadId: number | null) => {
  if (typeof window === 'undefined') return;
  try {
    if (!threadId) {
      window.localStorage.removeItem(SELECTED_THREAD_STORAGE_KEY);
      return;
    }
    window.localStorage.setItem(SELECTED_THREAD_STORAGE_KEY, String(threadId));
  } catch {
    // ignore storage issues
  }
};

export default function ChatPage() {
  const qc = useQueryClient();
  const { session } = useSession();
  const location = useLocation();
  const navigate = useNavigate();
  const myPartyId = session?.partyId ?? null;
  const [selectedThreadId, setSelectedThreadId] = useState<number | null>(() => loadSelectedThreadId());
  const [draftByThread, setDraftByThread] = useState<Record<string, string>>({});
  const [newChatOpen, setNewChatOpen] = useState(false);
  const [newChatInput, setNewChatInput] = useState('');
  const [newChatSelection, setNewChatSelection] = useState<FriendOption | null>(null);
  const [newChatError, setNewChatError] = useState<string | null>(null);
  const [bannerError, setBannerError] = useState<string | null>(null);
  const [sendError, setSendError] = useState<string | null>(null);
  const messagesEndRef = useRef<HTMLDivElement | null>(null);
  const requestParamConsumed = useRef(false);
  const [readVersion, setReadVersion] = useState(0);

  useEffect(() => subscribeToChatReadState(() => setReadVersion((v) => v + 1)), []);

  const healthQuery = useQuery({
    queryKey: ['health'],
    queryFn: Meta.health,
    staleTime: 30_000,
    refetchInterval: 30_000,
  });

  const friendsQuery = useQuery({
    queryKey: ['social-friends'],
    queryFn: SocialAPI.listFriends,
    enabled: Boolean(session?.partyId),
    staleTime: 15_000,
  });

  const partiesQuery = useQuery({
    queryKey: ['parties'],
    queryFn: Parties.list,
    staleTime: 5 * 60_000,
  });

  const threadsQuery = useQuery({
    queryKey: ['chat-threads'],
    queryFn: ChatAPI.listThreads,
    refetchInterval: 10_000,
  });

  const threads = useMemo(() => threadsQuery.data ?? [], [threadsQuery.data]);
  const readMap = useMemo(() => {
    void readVersion;
    return loadChatReadMap();
  }, [readVersion]);
  const unreadCount = useMemo(() => countUnreadThreads(threads, readMap), [readMap, threads]);

  const friendOptions = useMemo<FriendOption[]>(() => {
    const partiesById = new Map((partiesQuery.data ?? []).map((p) => [p.partyId, p]));
    const ids = new Set<number>();
    (friendsQuery.data ?? []).forEach((row) => ids.add(row.pfFollowingId));
    return Array.from(ids)
      .map((partyId) => {
        const party = partiesById.get(partyId);
        const label = party?.displayName ?? party?.legalName ?? `Party #${partyId}`;
        return { partyId, label, subtitle: `ID #${partyId}` };
      })
      .sort((a, b) => a.label.localeCompare(b.label));
  }, [friendsQuery.data, partiesQuery.data]);

  useEffect(() => {
    const first = threads.at(0);
    if (!first) return;
    if (selectedThreadId && threads.some((t) => t.ctThreadId === selectedThreadId)) return;
    setSelectedThreadId(first.ctThreadId);
  }, [selectedThreadId, threads]);

  const selectedThread = useMemo<ChatThreadDTO | null>(() => {
    if (!selectedThreadId) return null;
    return threads.find((t) => t.ctThreadId === selectedThreadId) ?? null;
  }, [selectedThreadId, threads]);

  const messagesQuery = useQuery({
    queryKey: ['chat-messages', selectedThreadId],
    queryFn: () => ChatAPI.listMessages(selectedThreadId!, { limit: 80 }),
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

  useEffect(() => {
    storeSelectedThreadId(selectedThreadId);
  }, [selectedThreadId]);

  useEffect(() => {
    if (!selectedThreadId) return;
    if (!messagesQuery.data) return;
    const lastMessageAt = messages.at(-1)?.cmCreatedAt ?? selectedThread?.ctLastMessageAt ?? selectedThread?.ctUpdatedAt;
    if (!lastMessageAt) return;
    markThreadSeen(selectedThreadId, lastMessageAt);
  }, [messages, messagesQuery.data, selectedThread?.ctLastMessageAt, selectedThread?.ctUpdatedAt, selectedThreadId]);

  const createThreadMutation = useMutation<ChatThreadDTO, Error, number>({
    mutationFn: (otherPartyId) => ChatAPI.getOrCreateDmThread(otherPartyId),
    onSuccess: async (thread) => {
      setNewChatOpen(false);
      setNewChatInput('');
      setNewChatSelection(null);
      setNewChatError(null);
      setBannerError(null);
      setSelectedThreadId(thread.ctThreadId);
      await qc.invalidateQueries({ queryKey: ['chat-threads'] });
    },
    onError: (err) => setNewChatError(err.message),
  });

  useEffect(() => {
    if (requestParamConsumed.current) return;
    if (typeof window === 'undefined') return;
    const params = new URLSearchParams(location.search);
    const rawParty = params.get('partyId');
    if (!rawParty) return;
    requestParamConsumed.current = true;
    navigate('/chat', { replace: true });
    const numeric = Number(rawParty);
    if (!Number.isFinite(numeric) || numeric <= 0) {
      setBannerError('El enlace de chat tiene un Party ID inválido.');
      return;
    }
    createThreadMutation.mutate(numeric, {
      onError: (err) => setBannerError(err.message),
    });
  }, [createThreadMutation, location.search, navigate]);

  const sendMutation = useMutation<ChatMessageDTO, Error, string>({
    mutationFn: (body) => ChatAPI.sendMessage(selectedThreadId!, body),
    onSuccess: async () => {
      setDraftByThread((prev) => {
        if (!selectedThreadId) return prev;
        return { ...prev, [String(selectedThreadId)]: '' };
      });
      setSendError(null);
      setBannerError(null);
      await qc.invalidateQueries({ queryKey: ['chat-messages', selectedThreadId] });
      await qc.invalidateQueries({ queryKey: ['chat-threads'] });
    },
    onError: (err) => setSendError(err.message),
  });

  const draft = useMemo(() => {
    if (!selectedThreadId) return '';
    return draftByThread[String(selectedThreadId)] ?? '';
  }, [draftByThread, selectedThreadId]);

  const updateDraft = (next: string) => {
    if (!selectedThreadId) return;
    setDraftByThread((prev) => ({ ...prev, [String(selectedThreadId)]: next }));
  };

  const handleStartChat = () => {
    setNewChatOpen(true);
    setNewChatError(null);
    setNewChatInput('');
    setNewChatSelection(null);
  };

  const handleCreateChat = () => {
    const numeric = newChatSelection?.partyId ?? Number(newChatInput.trim());
    if (!Number.isFinite(numeric) || numeric <= 0) {
      setNewChatError('Selecciona un contacto o ingresa un ID válido.');
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
    if (sendMutation.isPending) return;
    sendMutation.mutate(body);
  };

  const threadsLoading = threadsQuery.isLoading;
  const threadsError = threadsQuery.isError ? threadsQuery.error.message : null;
  const messagesLoading = messagesQuery.isLoading;
  const messagesError = messagesQuery.isError ? messagesQuery.error.message : null;

  const healthChip = useMemo(() => {
    if (healthQuery.isLoading) return <Chip size="small" label="API…" variant="outlined" />;
    if (healthQuery.isError) return <Chip size="small" label="API offline" color="error" variant="outlined" />;
    const status = healthQuery.data?.status ?? 'unknown';
    if (status === 'ok') return <Chip size="small" label="API online" color="success" variant="outlined" />;
    return <Chip size="small" label="API degradada" color="warning" variant="outlined" />;
  }, [healthQuery.data?.status, healthQuery.isError, healthQuery.isLoading]);

  return (
    <Box>
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} alignItems={{ xs: 'stretch', md: 'center' }} sx={{ mb: 2 }}>
        <Box sx={{ flex: 1 }}>
          <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
            <Typography variant="h4" fontWeight={800}>Chat</Typography>
            {healthChip}
            {unreadCount > 0 && <Chip size="small" label={`${unreadCount} sin leer`} color="error" variant="outlined" />}
          </Stack>
          <Typography color="text.secondary">
            Mensajería 1:1 con tus amigos mutuos.
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
          <Button variant="text" startIcon={<ChatBubbleOutlineIcon />} onClick={() => navigate('/social')}>
            Conexiones
          </Button>
        </Stack>
      </Stack>

      {bannerError && (
        <Alert
          severity="error"
          sx={{ mb: 2 }}
          action={(
            <Button color="inherit" size="small" onClick={() => setBannerError(null)}>
              Cerrar
            </Button>
          )}
        >
          {bannerError}
        </Alert>
      )}

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
                <Alert
                  severity="error"
                  action={(
                    <Button
                      color="inherit"
                      size="small"
                      onClick={() => void qc.invalidateQueries({ queryKey: ['chat-threads'] })}
                    >
                      Reintentar
                    </Button>
                  )}
                >
                  {threadsError}
                </Alert>
              </Box>
            ) : threads.length === 0 ? (
              <Box sx={{ p: 2 }}>
                <Alert severity="info">No tienes conversaciones todavía. Crea una con “Nuevo chat”.</Alert>
              </Box>
            ) : (
              <List dense disablePadding sx={{ maxHeight: { xs: 360, md: 560 }, overflowY: 'auto' }}>
                {threads.map((t) => {
                  const unread = isThreadUnread(t, readMap);
                  return (
                  <ListItemButton
                    key={t.ctThreadId}
                    selected={t.ctThreadId === selectedThreadId}
                    onClick={() => {
                      setSelectedThreadId(t.ctThreadId);
                      setSendError(null);
                    }}
                    sx={{ px: 2, py: 1.25 }}
                  >
                    <ListItemText
                      primary={
                        <Stack direction="row" spacing={1} alignItems="baseline" justifyContent="space-between">
                          <Typography fontWeight={700} sx={{ mr: 1 }} noWrap>
                            {t.ctOtherDisplayName.trim() ? t.ctOtherDisplayName : `Party #${t.ctOtherPartyId}`}
                            {unread && (
                              <FiberManualRecordIcon
                                sx={{ ml: 1, fontSize: 10, color: 'error.main', verticalAlign: 'middle' }}
                              />
                            )}
                          </Typography>
                          <Typography variant="caption" color="text.secondary" noWrap>
                            {formatTimestamp(t.ctLastMessageAt ?? t.ctUpdatedAt)}
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
                  );
                })}
              </List>
            )}
          </CardContent>
        </Card>

        <Card sx={{ flex: 1, minWidth: 0 }}>
          <CardContent sx={{ display: 'flex', flexDirection: 'column', height: { xs: 'auto', md: 600 } }}>
            <Box sx={{ mb: 1 }}>
              <Typography fontWeight={800} noWrap>
                {selectedThread
                  ? (selectedThread.ctOtherDisplayName.trim()
                      ? selectedThread.ctOtherDisplayName
                      : `Party #${selectedThread.ctOtherPartyId}`)
                  : 'Selecciona una conversación'}
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
                <Alert
                  severity="error"
                  action={(
                    <Button color="inherit" size="small" onClick={() => void messagesQuery.refetch()}>
                      Reintentar
                    </Button>
                  )}
                >
                  {messagesError}
                </Alert>
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
                onChange={(e) => updateDraft(e.target.value)}
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
              Busca un amigo mutuo para iniciar un chat. Si no aparece, revisa <strong>Conexiones</strong>.
            </Typography>
            <Autocomplete
              options={friendOptions}
              value={newChatSelection}
              onChange={(_e, value) => setNewChatSelection(value)}
              inputValue={newChatInput}
              onInputChange={(_e, value) => setNewChatInput(value)}
              getOptionLabel={(option) => option.label}
              renderOption={(props, option) => (
                <li {...props} key={option.partyId}>
                  <Stack sx={{ width: '100%' }}>
                    <Typography fontWeight={700} noWrap>
                      {option.label}
                    </Typography>
                    <Typography variant="caption" color="text.secondary" noWrap>
                      {option.subtitle}
                    </Typography>
                  </Stack>
                </li>
              )}
              renderInput={(params) => (
                <TextField
                  {...params}
                  label="Amigo"
                  placeholder={friendOptions.length ? 'Buscar por nombre…' : 'Agrega amigos en Conexiones'}
                  disabled={createThreadMutation.isPending}
                />
              )}
              noOptionsText={friendsQuery.isLoading ? 'Cargando amigos…' : 'No tienes amigos mutuos aún.'}
              loading={friendsQuery.isLoading || partiesQuery.isLoading}
            />
            {friendOptions.length === 0 && !friendsQuery.isLoading && (
              <Alert
                severity="info"
                action={<Button color="inherit" size="small" onClick={() => navigate('/social')}>Ir</Button>}
              >
                Agrega un amigo mutuo para chatear.
              </Alert>
            )}
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
