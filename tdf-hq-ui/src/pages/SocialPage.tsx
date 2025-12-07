import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Divider,
  Stack,
  Tab,
  Tabs,
  TextField,
  Typography,
} from '@mui/material';
import PersonAddAltIcon from '@mui/icons-material/PersonAddAlt';
import PersonOffIcon from '@mui/icons-material/PersonOff';
import RefreshIcon from '@mui/icons-material/Refresh';
import { Parties } from '../api/parties';
import { SocialAPI } from '../api/social';
import type { PartyDTO, PartyFollowDTO } from '../api/types';
import { useSession } from '../session/SessionContext';

type TabKey = 'friends' | 'following' | 'followers';

function usePartiesMap() {
  const partiesQuery = useQuery({
    queryKey: ['parties'],
    queryFn: () => Parties.list(),
    staleTime: 5 * 60 * 1000,
  });
  const byId = useMemo(() => {
    const map = new Map<number, PartyDTO>();
    (partiesQuery.data ?? []).forEach((p) => {
      map.set(p.partyId, p);
    });
    return map;
  }, [partiesQuery.data]);
  return { partiesQuery, byId };
}

function formatParty(byId: Map<number, PartyDTO>, partyId: number) {
  const party = byId.get(partyId);
  if (!party) return `Party #${partyId}`;
  return party.displayName || party.legalName || `Party #${partyId}`;
}

export default function SocialPage() {
  const qc = useQueryClient();
  const { session } = useSession();
  const [activeTab, setActiveTab] = useState<TabKey>('friends');
  const [addId, setAddId] = useState('');

  const { partiesQuery, byId } = usePartiesMap();

  const followersQuery = useQuery({
    queryKey: ['social-followers'],
    queryFn: SocialAPI.listFollowers,
  });
  const followingQuery = useQuery({
    queryKey: ['social-following'],
    queryFn: SocialAPI.listFollowing,
  });
  const friendsQuery = useQuery({
    queryKey: ['social-friends'],
    queryFn: SocialAPI.listFriends,
  });

  const invalidateAll = () => {
    void qc.invalidateQueries({ queryKey: ['social-followers'] });
    void qc.invalidateQueries({ queryKey: ['social-following'] });
    void qc.invalidateQueries({ queryKey: ['social-friends'] });
  };

  const addMutation = useMutation({
    mutationFn: async () => {
      const numeric = Number(addId.trim());
      if (!Number.isFinite(numeric) || numeric <= 0) throw new Error('Ingresa un ID válido.');
      await SocialAPI.addFriend(numeric);
    },
    onSuccess: () => {
      setAddId('');
      invalidateAll();
    },
  });

  const removeMutation = useMutation({
    mutationFn: (targetId: number) => SocialAPI.removeFriend(targetId),
    onSuccess: invalidateAll,
  });

  const tabData: Record<TabKey, { data?: PartyFollowDTO[]; empty: string }> = {
    friends: { data: friendsQuery.data, empty: 'Aún no tienes amigos mutuos.' },
    following: { data: followingQuery.data, empty: 'No sigues a nadie todavía.' },
    followers: { data: followersQuery.data, empty: 'Aún no tienes seguidores.' },
  };

  const activeData = tabData[activeTab].data ?? [];

  return (
    <Box>
      <Stack spacing={2} sx={{ mb: 2 }}>
        <Stack direction="row" alignItems="center" spacing={1}>
          <Typography variant="h4" fontWeight={800}>Red social</Typography>
          <Chip label={session?.partyId ? `Tu ID: ${session.partyId}` : 'Sin sesión'} size="small" />
        </Stack>
        <Typography color="text.secondary">
          Administra seguidores, seguidos y amigos mutuos. Usa el ID de perfil para agregar amigos.
        </Typography>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'stretch', sm: 'center' }}>
          <TextField
            label="Agregar amigo por ID"
            placeholder="Ej: 123"
            size="small"
            value={addId}
            onChange={(e) => setAddId(e.target.value)}
            sx={{ maxWidth: 220 }}
          />
          <Button
            variant="contained"
            startIcon={<PersonAddAltIcon />}
            onClick={() => addMutation.mutate()}
            disabled={addMutation.status === 'pending'}
          >
            Agregar
          </Button>
          <Button
            variant="outlined"
            startIcon={<RefreshIcon />}
            onClick={invalidateAll}
          >
            Refrescar
          </Button>
        </Stack>
        {addMutation.error && <Alert severity="error">{addMutation.error.message}</Alert>}
      </Stack>

      <Card>
        <CardContent>
          <Tabs
            value={activeTab}
            onChange={(_e, val) => setActiveTab(val)}
            sx={{ mb: 2 }}
          >
            <Tab label={`Amigos (${friendsQuery.data?.length ?? 0})`} value="friends" />
            <Tab label={`Siguiendo (${followingQuery.data?.length ?? 0})`} value="following" />
            <Tab label={`Seguidores (${followersQuery.data?.length ?? 0})`} value="followers" />
          </Tabs>

          {(followersQuery.isLoading || followingQuery.isLoading || friendsQuery.isLoading || partiesQuery.isLoading) ? (
            <Stack direction="row" alignItems="center" spacing={1.5} sx={{ py: 2 }}>
              <CircularProgress size={20} />
              <Typography>Cargando red social...</Typography>
            </Stack>
          ) : (
            <>
              {activeData.length === 0 ? (
                <Alert severity="info">{tabData[activeTab].empty}</Alert>
              ) : (
                <Stack divider={<Divider flexItem />} spacing={1}>
                  {activeData.map((row) => {
                    const targetId = activeTab === 'followers' ? row.pfFollowerId : row.pfFollowingId;
                    const label = formatParty(byId, targetId);
                    const since = new Date(row.pfStartedAt).toLocaleString();
                    const isFriend = friendsQuery.data?.some((f) => f.pfFollowingId === targetId) ?? false;
                    return (
                      <Stack key={`${activeTab}-${row.pfFollowerId}-${row.pfFollowingId}`} direction="row" justifyContent="space-between" alignItems="center">
                        <Box>
                          <Typography variant="subtitle1" fontWeight={700}>{label}</Typography>
                          <Typography variant="body2" color="text.secondary">
                            ID #{targetId} · Desde {since}
                          </Typography>
                          {row.pfViaNfc && (
                            <Chip label="Intercambio NFC" size="small" color="info" sx={{ mt: 0.5 }} />
                          )}
                        </Box>
                        <Stack direction="row" spacing={1}>
                          {activeTab !== 'followers' && (
                            <Button
                              variant="outlined"
                              size="small"
                              startIcon={<PersonOffIcon />}
                              onClick={() => removeMutation.mutate(targetId)}
                              disabled={removeMutation.status === 'pending'}
                            >
                              {isFriend ? 'Eliminar amigo' : 'Dejar de seguir'}
                            </Button>
                          )}
                        </Stack>
                      </Stack>
                    );
                  })}
                </Stack>
              )}
            </>
          )}
        </CardContent>
      </Card>
    </Box>
  );
}
