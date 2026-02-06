import { useMemo, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import {
  Alert,
  Button,
  Chip,
  CircularProgress,
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
  Typography,
} from '@mui/material';
import { SocialInboxAPI, type SocialMessage } from '../api/socialInbox';

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

interface ChannelPanelProps {
  label: string;
  stats: MessageStats;
  messages: SocialMessage[];
  loading: boolean;
}

const ChannelPanel = ({ label, stats, messages, loading }: ChannelPanelProps) => (
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
                  <TableRow key={msg.externalId} hover>
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
  const direction = 'incoming' as const;
  const repliedOnly = filter === 'replied';
  const instagramQuery = useQuery({
    queryKey: ['social-inbox', 'instagram', limit, direction, repliedOnly],
    queryFn: () => SocialInboxAPI.listInstagramMessages({ limit, direction, repliedOnly }),
  });
  const whatsappQuery = useQuery({
    queryKey: ['social-inbox', 'whatsapp', limit, direction, repliedOnly],
    queryFn: () => SocialInboxAPI.listWhatsAppMessages({ limit, direction, repliedOnly }),
  });
  const instagramStats = useMemo(() => buildStats(instagramQuery.data), [instagramQuery.data]);
  const whatsappStats = useMemo(() => buildStats(whatsappQuery.data), [whatsappQuery.data]);
  const instagramMessages = useMemo(() => selectMessages(instagramStats, filter), [instagramStats, filter]);
  const whatsappMessages = useMemo(() => selectMessages(whatsappStats, filter), [whatsappStats, filter]);
  const refetch = () => {
    void instagramQuery.refetch();
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
            disabled={instagramQuery.isFetching || whatsappQuery.isFetching}
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
      <Stack direction={{ xs: 'column', lg: 'row' }} spacing={2}>
        <ChannelPanel
          label="Instagram"
          stats={instagramStats}
          messages={instagramMessages}
          loading={instagramQuery.isLoading}
        />
        <ChannelPanel
          label="WhatsApp"
          stats={whatsappStats}
          messages={whatsappMessages}
          loading={whatsappQuery.isLoading}
        />
      </Stack>
    </Stack>
  );
}
