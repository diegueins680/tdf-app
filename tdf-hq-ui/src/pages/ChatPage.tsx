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
import LazyPaginatedList from '../components/LazyPaginatedList';
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
  if (max >= trimmed.length) return trimmed;
  return `${trimmed.slice(0, max - 1)}…`;
}

interface FriendOption {
  partyId: number;
  label: string;
  subtitle: string;
}

type DraftByThread = Record<string, string>;

const SELECTED_THREAD_STORAGE_KEY = 'tdf-chat-selected-thread-v1';

type ChatPageDisplayContract = Readonly<{
  threadLastMessagePreviewChars: number;
}>;

// Invariant: thread previews fit one list row and remain shorter than the full
// message bubble rendered in the active conversation pane.
const CHAT_PAGE_DISPLAY_CONTRACTS = {
  threadLastMessagePreviewChars: 8 * 8,
} as const satisfies ChatPageDisplayContract;

const parsePositiveInt = (raw: string | null | undefined): number | null => {
  const trimmed = raw?.trim() ?? '';
  if (!/^\d+$/.test(trimmed)) return null;
  const parsed = Number(trimmed);
  return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : null;
};

const loadSelectedThreadId = (): number | null => {
  if (typeof window === 'undefined') return null;
  try {
    return parsePositiveInt(window.localStorage.getItem(SELECTED_THREAD_STORAGE_KEY));
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

interface ThreadListItemProps {
  thread: ChatThreadDTO;
  selected: boolean;
  unread: boolean;
  onSelect: (threadId: number) => void;
}

function ThreadListItem({ thread, selected, unread, onSelect }: ThreadListItemProps) {
  const displayName = thread.ctOtherDisplayName.trim() ? thread.ctOtherDisplayName : `Party #${thread.ctOtherPartyId}`;

  return (
    <ListItemButton
      selected={selected}
      tabIndex={0}
      onClick={(event) => {
        event.currentTarget.focus();
        onSelect(thread.ctThreadId);
      }}
      sx={{ px: 2, py: 1.25 }}
    >
      <ListItemText
        primary={
          <Stack direction="row" spacing={1} alignItems="baseline" justifyContent="space-between">
            <Typography fontWeight={700} sx={{ mr: 1 }} noWrap>
              {displayName}
              {unread && <FiberManualRecordIcon sx={{ ml: 1, fontSize: 10, color: 'error.main', verticalAlign: 'middle' }} />}
            </Typography>
            <Typography variant="caption" color="text.secondary" noWrap>
              {formatTimestamp(thread.ctLastMessageAt ?? thread.ctUpdatedAt)}
            </Typography>
          </Stack>
        }
        secondary={thread.ctLastMessage
          ? truncate(thread.ctLastMessage, CHAT_PAGE_DISPLAY_CONTRACTS.threadLastMessagePreviewChars)
          : 'Sin mensajes aún'}
        secondaryTypographyProps={{ noWrap: true }}
      />
    </ListItemButton>
  );
}

interface MessageBubbleProps {
  message: ChatMessageDTO;
  myPartyId: number | null;
}

function MessageBubble({ message, myPartyId }: MessageBubbleProps) {
  const mine = myPartyId !== null && message.cmSenderPartyId === myPartyId;

  return (
    <Box
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
        <Typography variant="body2">{message.cmBody}</Typography>
      </Box>
      <Typography variant="caption" color="text.secondary" sx={{ mt: 0.25, textAlign: mine ? 'right' : 'left' }}>
        {formatTimestamp(message.cmCreatedAt)}
      </Typography>
    </Box>
  );
}

export default function ChatPage() {
  const qc = useQueryClient();
  const { session } = useSession();
  const location = useLocation();
  const navigate = useNavigate();
  const myPartyId = session?.partyId ?? null;
  const [selectedThreadId, setSelectedThreadId] = useState(() => loadSelectedThreadId());
  const [draftByThread, setDraftByThread] = useState({} as DraftByThread);
  const [newChatOpen, setNewChatOpen] = useState(false);
  const [newChatInput, setNewChatInput] = useState('');
  const [newChatSelection, setNewChatSelection] = useState(null as FriendOption | null);
  const [newChatError, setNewChatError] = useState(null as string | null);
  const [bannerError, setBannerError] = useState(null as string | null);
  const [sendError, setSendError] = useState(null as string | null);
  const messagesEndRef = useRef(null as HTMLDivElement | null);
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

  const friendOptions: FriendOption[] = useMemo(() => {
    const partiesById = new Map((partiesQuery.data ?? []).map((p) => [p.partyId, p]));
    const ids = new Set((friendsQuery.data ?? []).map((row) => row.pfFollowingId));
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

  const selectedThread: ChatThreadDTO | null = useMemo(() => {
    if (!selectedThreadId) return null;
    return threads.find((t) => t.ctThreadId === selectedThreadId) ?? null;
  }, [selectedThreadId, threads]);

  const messagesQuery = useQuery({
    queryKey: ['chat-messages', selectedThreadId],
    queryFn: () => ChatAPI.listMessages(selectedThreadId!, { limit: 80 }),
    enabled: Boolean(selectedThreadId),
    refetchInterval: 3_000,
  });

  const messages: ChatMessageDTO[] = useMemo(
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

  const createThreadMutation = useMutation({
    mutationFn: (otherPartyId: number) => ChatAPI.getOrCreateDmThread(otherPartyId),
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
    const numeric = parsePositiveInt(rawParty);
    if (numeric == null) {
      setBannerError('El enlace de chat tiene un Party ID inválido.');
      return;
    }
    createThreadMutation.mutate(numeric, {
      onError: (err) => setBannerError(err.message),
    });
  }, [createThreadMutation, location.search, navigate]);

  const sendMutation = useMutation({
    mutationFn: (body: string) => ChatAPI.sendMessage(selectedThreadId!, body),
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
  const composerAriaLabel = selectedThreadId
    ? 'Escribe un mensaje'
    : 'Selecciona una conversación para escribir un mensaje';

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
    const numeric = newChatSelection?.partyId ?? parsePositiveInt(newChatInput);
    if (numeric == null) {
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
          <Button
            variant="outlined"
            startIcon={<AddCommentIcon />}
            tabIndex={0}
            onClick={(event) => {
              event.currentTarget.focus();
              handleStartChat();
            }}
          >
            Nuevo chat
          </Button>
          <Button
            variant="text"
            startIcon={<RefreshIcon />}
            tabIndex={0}
            onClick={(event) => {
              event.currentTarget.focus();
              void qc.invalidateQueries({ queryKey: ['chat-threads'] });
            }}
          >
            Refrescar
          </Button>
          <Button
            variant="text"
            startIcon={<ChatBubbleOutlineIcon />}
            tabIndex={0}
            onClick={(event) => {
              event.currentTarget.focus();
              navigate('/social');
            }}
          >
            Conexiones
          </Button>
        </Stack>
      </Stack>

      {bannerError && (
        <Alert
          severity="error"
          sx={{ mb: 2 }}
          action={(
            <Button
              color="inherit"
              size="small"
              tabIndex={0}
              onClick={(event) => {
                event.currentTarget.focus();
                setBannerError(null);
              }}
            >
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
                      tabIndex={0}
                      onClick={(event) => {
                        event.currentTarget.focus();
                        void qc.invalidateQueries({ queryKey: ['chat-threads'] });
                      }}
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
              <LazyPaginatedList
                items={threads}
                loading={threadsQuery.isFetching}
                pagination={{ itemLabel: 'conversaciones', initialRowsPerPage: 10 }}
                renderItems={(visibleThreads) => (
                  <List dense disablePadding>
                    {visibleThreads.map((t) => {
                      const unread = isThreadUnread(t, readMap);
                      return (
                        <ThreadListItem
                          key={t.ctThreadId}
                          thread={t}
                          selected={t.ctThreadId === selectedThreadId}
                          unread={unread}
                          onSelect={(threadId) => {
                            setSelectedThreadId(threadId);
                            setSendError(null);
                          }}
                        />
                      );
                    })}
                  </List>
                )}
              />
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
                    <Button
                      color="inherit"
                      size="small"
                      tabIndex={0}
                      onClick={(event) => {
                        event.currentTarget.focus();
                        void messagesQuery.refetch();
                      }}
                    >
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
                  {messages.map((message) => (
                    <MessageBubble key={message.cmId} message={message} myPartyId={myPartyId} />
                  ))}
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
                aria-label={composerAriaLabel}
                placeholder={selectedThreadId ? 'Escribe un mensaje…' : 'Selecciona una conversación…'}
                value={draft}
                disabled={!selectedThreadId || sendMutation.isPending}
                inputProps={{ 'aria-label': composerAriaLabel }}
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
                aria-label="Enviar mensaje"
                tabIndex={0}
                onClick={(event) => {
                  event.currentTarget.focus();
                  handleSend();
                }}
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
                action={
                  <Button
                    color="inherit"
                    size="small"
                    tabIndex={0}
                    onClick={(event) => {
                      event.currentTarget.focus();
                      navigate('/social');
                    }}
                  >
                    Ir
                  </Button>
                }
              >
                Agrega un amigo mutuo para chatear.
              </Alert>
            )}
            {newChatError && <Alert severity="error">{newChatError}</Alert>}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button
            tabIndex={0}
            onClick={(event) => {
              event.currentTarget.focus();
              setNewChatOpen(false);
            }}
            disabled={createThreadMutation.isPending}
          >
            Cancelar
          </Button>
          <Button
            variant="contained"
            tabIndex={0}
            onClick={(event) => {
              event.currentTarget.focus();
              handleCreateChat();
            }}
            disabled={createThreadMutation.isPending}
          >
            Crear
          </Button>
        </DialogActions>
      </Dialog>
    </Box>
  );
}
