import { useMemo } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Avatar,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Stack,
  Tooltip,
  Typography,
} from '@mui/material';
import { Link as RouterLink, useParams } from 'react-router-dom';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import PersonAddAltIcon from '@mui/icons-material/PersonAddAlt';
import PersonOffIcon from '@mui/icons-material/PersonOff';
import ChatBubbleOutlineIcon from '@mui/icons-material/ChatBubbleOutline';
import { Parties } from '../api/parties';
import { SocialAPI } from '../api/social';
import { RadioAPI } from '../api/radio';
import type { RadioPresenceDTO } from '../api/types';
import { useSession } from '../session/SessionContext';

const parsePositiveIntParam = (value: string | undefined): number | null => {
  const raw = value?.trim();
  if (!raw || !/^\d+$/.test(raw)) return null;
  const parsed = Number.parseInt(raw, 10);
  return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : null;
};

export default function PublicProfilePage() {
  const { partyId } = useParams();
  const parsedId = parsePositiveIntParam(partyId);
  const enabled = parsedId !== null;
  const qc = useQueryClient();
  const { session } = useSession();
  const isSelf = session?.partyId === parsedId;

  const partyQuery = useQuery({
    queryKey: ['public-party', parsedId],
    queryFn: () => Parties.getOne(parsedId ?? 0),
    enabled,
  });

  const friendsQuery = useQuery({
    queryKey: ['friends'],
    queryFn: SocialAPI.listFriends,
    enabled: Boolean(session?.partyId),
  });
  const followersQuery = useQuery({
    queryKey: ['social-followers'],
    queryFn: SocialAPI.listFollowers,
    enabled: Boolean(session?.partyId),
  });
  const followingQuery = useQuery({
    queryKey: ['social-following'],
    queryFn: SocialAPI.listFollowing,
    enabled: Boolean(session?.partyId),
  });

  const presenceQuery = useQuery({
    queryKey: ['radio-presence', parsedId],
    queryFn: () => RadioAPI.getPresence(parsedId ?? 0),
    enabled,
    refetchInterval: 30_000,
  });

  const isFriend = useMemo(
    () => friendsQuery.data?.some((f) => f.pfFollowingId === parsedId) ?? false,
    [friendsQuery.data, parsedId],
  );
  const youFollow = useMemo(
    () => followingQuery.data?.some((f) => f.pfFollowingId === parsedId) ?? false,
    [followingQuery.data, parsedId],
  );
  const followsYou = useMemo(
    () => followersQuery.data?.some((f) => f.pfFollowerId === parsedId) ?? false,
    [followersQuery.data, parsedId],
  );

  const friendMutation = useMutation<void, Error, void>({
    mutationFn: async () => {
      if (parsedId === null) {
        throw new Error('ID de perfil inválido.');
      }
      if (isFriend) {
        await SocialAPI.removeFriend(parsedId);
      } else {
        await SocialAPI.addFriend(parsedId);
      }
    },
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['friends'] });
      void qc.invalidateQueries({ queryKey: ['social-followers'] });
      void qc.invalidateQueries({ queryKey: ['social-following'] });
    },
  });

  const presence = presenceQuery.data ?? null;

  const handleTuneIn = (p: RadioPresenceDTO) => {
    window.dispatchEvent(
      new CustomEvent('tdf-radio-load-stream', {
        detail: {
          streamUrl: p.rpStreamUrl,
          stationName: p.rpStationName ?? partyQuery.data?.displayName ?? 'Stream compartido',
          stationId: p.rpStationId ?? `party-${p.rpPartyId}`,
        },
      }),
    );
  };

  if (!enabled) {
    return (
      <Box p={3}>
        <Typography variant="h6">Perfil no encontrado.</Typography>
      </Box>
    );
  }

  if (partyQuery.isLoading) {
    return (
      <Box p={3} display="flex" alignItems="center" gap={1.5}>
        <CircularProgress size={20} />
        <Typography>Cargando perfil...</Typography>
      </Box>
    );
  }

  if (partyQuery.error || !partyQuery.data) {
    return (
      <Box p={3}>
        <Typography variant="h6">No pudimos cargar este perfil.</Typography>
      </Box>
    );
  }

  const party = partyQuery.data;

  return (
    <Box p={{ xs: 2, md: 4 }}>
      <Card sx={{ maxWidth: 720, mx: 'auto', borderRadius: 3, overflow: 'hidden' }}>
        <CardContent>
          <Stack spacing={2}>
          <Stack direction="row" spacing={2} alignItems="center">
            <Avatar sx={{ width: 72, height: 72, bgcolor: '#1d4ed8' }}>
              {party.displayName?.[0]?.toUpperCase() ?? '?'}
            </Avatar>
            <Box flex={1}>
              <Typography variant="h5" fontWeight={800}>
                {party.displayName}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                {party.legalName ?? party.displayName}
              </Typography>
              {session?.partyId && (
                <Stack direction="row" spacing={1} sx={{ mt: 1, flexWrap: 'wrap' }}>
                  <Chip label={isFriend ? 'Amigos mutuos' : youFollow ? 'Sigues a esta persona' : 'No sigues aún'} size="small" color={isFriend ? 'success' : youFollow ? 'info' : 'default'} />
                  {followsYou && <Chip label="Te sigue" size="small" color="secondary" />}
                  {!followsYou && !youFollow && <Chip label="Sin conexión" size="small" />}
                </Stack>
              )}
            </Box>
              {!isSelf && session?.partyId && (
                <Stack direction="row" spacing={1} alignItems="center">
                  <Button
                    variant={isFriend ? 'outlined' : 'contained'}
                    startIcon={isFriend ? <PersonOffIcon /> : <PersonAddAltIcon />}
                    onClick={() => friendMutation.mutate()}
                    disabled={friendMutation.status === 'pending'}
                  >
                    {isFriend ? 'Eliminar amigo' : 'Agregar amigo'}
                  </Button>
                  <Tooltip title={isFriend ? 'Abrir chat' : 'Disponible solo para amigos mutuos'} disableInteractive>
                    <span>
                      <Button
                        variant="outlined"
                        startIcon={<ChatBubbleOutlineIcon />}
                        component={RouterLink}
                        to={`/chat?partyId=${parsedId}`}
                        disabled={!isFriend}
                      >
                        Chatear
                      </Button>
                    </span>
                  </Tooltip>
                </Stack>
              )}
            </Stack>
            {presence && (
              <Card variant="outlined" sx={{ borderRadius: 2, p: 2, bgcolor: 'rgba(148,163,184,0.08)' }}>
                <Stack direction="row" spacing={2} alignItems="center" justifyContent="space-between">
                  <Box>
                    <Typography variant="caption" color="text.secondary">
                      Escuchando ahora
                    </Typography>
                    <Typography variant="subtitle1" fontWeight={700}>
                      {presence.rpStationName ?? presence.rpStreamUrl}
                    </Typography>
                    <Typography variant="caption" color="text.secondary">
                      {new Date(presence.rpUpdatedAt).toLocaleTimeString()}
                    </Typography>
                  </Box>
                  <Button
                    variant="contained"
                    startIcon={<PlayArrowIcon />}
                    onClick={() => handleTuneIn(presence)}
                    data-no-drag
                  >
                    Escuchar
                  </Button>
                </Stack>
              </Card>
            )}
            {!presence && (
              <Typography variant="body2" color="text.secondary">
                Esta persona no está escuchando la radio en este momento.
              </Typography>
            )}
            {session?.partyId && (
              <Typography variant="body2" color="text.secondary">
                Estado: {isFriend ? 'Conexión mutua' : youFollow ? 'Lo sigues' : followsYou ? 'Te sigue' : 'Sin conexión'}.
              </Typography>
            )}
          </Stack>
        </CardContent>
      </Card>
    </Box>
  );
}
