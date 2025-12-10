import { useEffect, useMemo, useState } from 'react';
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
import QrCodeScannerIcon from '@mui/icons-material/QrCodeScanner';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import LinkIcon from '@mui/icons-material/Link';
import OpenInNewIcon from '@mui/icons-material/OpenInNew';
import { Parties } from '../api/parties';
import { SocialAPI } from '../api/social';
import type { PartyDTO, PartyFollowDTO } from '../api/types';
import { useSession } from '../session/SessionContext';
import { buildVCardSharePayload, parseVCardPayload } from '../utils/vcard';

type TabKey = 'friends' | 'following' | 'followers';

function usePartiesMap() {
  const partiesQuery = useQuery({
    queryKey: ['parties'],
    queryFn: () => Parties.list(),
    staleTime: 5 * 60 * 1000,
  });
  const byId = useMemo(() => {
    const map = new Map<number, PartyDTO>();
    (partiesQuery.data ?? []).forEach((p) => map.set(p.partyId, p));
    return map;
  }, [partiesQuery.data]);
  return { partiesQuery, byId };
}

function formatParty(byId: Map<number, PartyDTO>, partyId: number) {
  const party = byId.get(partyId);
  if (!party) return `Party #${partyId}`;
  return party.displayName ?? party.legalName ?? `Party #${partyId}`;
}

export default function SocialPage() {
  const qc = useQueryClient();
  const { session } = useSession();
  const [activeTab, setActiveTab] = useState<TabKey>('friends');
  const [addId, setAddId] = useState('');
  const [shareName, setShareName] = useState('');
  const [shareEmail, setShareEmail] = useState('');
  const [sharePhone, setSharePhone] = useState('');
  const [shareQr, setShareQr] = useState<string | null>(null);
  const [shareQrError, setShareQrError] = useState<string | null>(null);
  const [payloadInput, setPayloadInput] = useState('');
  const [copyMessage, setCopyMessage] = useState<string | null>(null);

  const { partiesQuery, byId } = usePartiesMap();
  const myParty = session?.partyId ? byId.get(session.partyId) : null;

  useEffect(() => {
    if (!myParty) return;
    setShareName((prev) => prev || myParty.displayName || myParty.legalName || '');
    setShareEmail((prev) => prev || myParty.primaryEmail || '');
    setSharePhone((prev) => prev || myParty.primaryPhone || myParty.whatsapp || '');
  }, [myParty]);

  const sharePayload = useMemo(
    () => buildVCardSharePayload({
      name: shareName,
      email: shareEmail,
      phone: sharePhone,
      partyId: session?.partyId ?? undefined,
    }),
    [shareEmail, shareName, sharePhone, session?.partyId],
  );

  useEffect(() => {
    setShareQrError(null);
    try {
      const url = `https://api.qrserver.com/v1/create-qr-code/?size=240x240&data=${encodeURIComponent(sharePayload)}`;
      setShareQr(url);
    } catch (err) {
      console.error('No se pudo generar el QR de vCard', err);
      setShareQrError('No pudimos generar el QR.');
    }
  }, [sharePayload]);

  const parsedPayload = useMemo(() => {
    const trimmed = payloadInput.trim();
    if (!trimmed) return null;
    const parsed = parseVCardPayload(trimmed);
    if (parsed) return parsed;
    const numeric = Number(trimmed);
    if (Number.isFinite(numeric) && numeric > 0) {
      return {
        kind: 'vcard-exchange',
        partyId: numeric,
        name: null,
        email: null,
        phone: null,
        ts: Date.now(),
      };
    }
    return null;
  }, [payloadInput]);

  const profileUrl = useMemo(() => {
    if (typeof window === 'undefined' || !session?.partyId) return null;
    return `${window.location.origin}/perfil/${session.partyId}`;
  }, [session?.partyId]);

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
  const suggestionsQuery = useQuery({
    queryKey: ['social-suggestions'],
    queryFn: SocialAPI.listSuggestions,
  });

  const invalidateAll = () => {
    void qc.invalidateQueries({ queryKey: ['social-followers'] });
    void qc.invalidateQueries({ queryKey: ['social-following'] });
    void qc.invalidateQueries({ queryKey: ['social-friends'] });
    void qc.invalidateQueries({ queryKey: ['social-suggestions'] });
  };

  const addMutation = useMutation<void, Error, number | undefined>({
    mutationFn: async (targetId) => {
      const numeric = targetId ?? Number(addId.trim());
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

  const exchangeMutation = useMutation<void, Error>({
    mutationFn: async () => {
      const target = parsedPayload?.partyId;
      if (!target) throw new Error('Ingresa un payload válido con partyId.');
      await SocialAPI.exchangeVCard(target);
    },
    onSuccess: () => {
      setPayloadInput('');
      invalidateAll();
    },
  });

  const tabData: Record<TabKey, { data?: PartyFollowDTO[]; empty: string }> = {
    friends: { data: friendsQuery.data, empty: 'Aún no tienes amigos mutuos.' },
    following: { data: followingQuery.data, empty: 'No sigues a nadie todavía.' },
    followers: { data: followersQuery.data, empty: 'Aún no tienes seguidores.' },
  };

  const activeData = tabData[activeTab].data ?? [];

  const handleCopy = async (value: string | null) => {
    if (!value) return;
    if (typeof navigator === 'undefined' || !navigator.clipboard?.writeText) {
      setCopyMessage('Portapapeles no disponible');
      return;
    }
    try {
      await navigator.clipboard.writeText(value);
      setCopyMessage('Copiado');
      window.setTimeout(() => setCopyMessage(null), 1800);
    } catch (err) {
      console.warn('No se pudo copiar', err);
      setCopyMessage('No se pudo copiar');
    }
  };

  return (
    <Box>
      <Stack spacing={2} sx={{ mb: 2 }}>
        <Stack direction="row" alignItems="center" spacing={1}>
          <Typography variant="h4" fontWeight={800}>Conexiones</Typography>
          <Chip label={session?.partyId ? `Tu ID: ${session.partyId}` : 'Sin sesión'} size="small" />
        </Stack>
        <Typography color="text.secondary">
          Administra seguidores, seguidos y amigos mutuos. Usa el ID de perfil para agregar amigos.
        </Typography>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'stretch', sm: 'center' }}>
          <Autocomplete
            sx={{ minWidth: 260 }}
            size="small"
            options={partiesQuery.data ?? []}
            loading={partiesQuery.isLoading}
            getOptionLabel={(option) => option.displayName ?? option.legalName ?? `Party #${option.partyId}`}
            value={(() => {
              const numeric = Number(addId);
              if (!Number.isFinite(numeric)) return null;
              return (partiesQuery.data ?? []).find((p) => p.partyId === numeric) ?? null;
            })()}
            onChange={(_, value) => setAddId(value ? String(value.partyId) : '')}
            inputValue={addId}
            onInputChange={(_, value) => setAddId(value)}
            renderInput={(params) => (
              <TextField
                {...params}
                label="Agregar amigo por contacto o ID"
                placeholder="Busca un contacto o ingresa su ID"
              />
            )}
            noOptionsText={partiesQuery.isFetching ? 'Cargando contactos…' : 'Sin coincidencias'}
          />
          <Button
            variant="contained"
            startIcon={<PersonAddAltIcon />}
            onClick={() => addMutation.mutate(undefined)}
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

      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} sx={{ mb: 3 }}>
        <Card sx={{ flex: 1 }}>
          <CardContent>
            <Stack spacing={2}>
              <Stack direction="row" alignItems="center" justifyContent="space-between" flexWrap="wrap" gap={1}>
                <Typography variant="h6" fontWeight={800}>Comparte tu contacto</Typography>
                {profileUrl && (
                  <Button
                    size="small"
                    variant="outlined"
                    startIcon={<OpenInNewIcon />}
                    component="a"
                    href={profileUrl}
                    target="_blank"
                    rel="noreferrer"
                  >
                    Ver perfil público
                  </Button>
                )}
              </Stack>
              <Typography color="text.secondary">
                Genera un QR con tus datos básicos para intercambiar contactos rápidamente.
              </Typography>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                <TextField
                  label="Nombre para mostrar"
                  value={shareName}
                  onChange={(e) => setShareName(e.target.value)}
                  fullWidth
                />
                <TextField
                  label="Correo"
                  value={shareEmail}
                  onChange={(e) => setShareEmail(e.target.value)}
                  fullWidth
                />
              </Stack>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                <TextField
                  label="Teléfono / WhatsApp"
                  value={sharePhone}
                  onChange={(e) => setSharePhone(e.target.value)}
                  fullWidth
                />
                <TextField
                  label="Party ID"
                  value={session?.partyId ? String(session.partyId) : ''}
                  disabled
                  fullWidth
                />
              </Stack>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems="center">
                <Box
                  sx={{
                    width: 168,
                    height: 168,
                    borderRadius: 2,
                    border: '1px dashed',
                    borderColor: 'divider',
                    bgcolor: 'background.paper',
                    display: 'flex',
                    alignItems: 'center',
                    justifyContent: 'center',
                    p: 1,
                  }}
                >
                  {shareQr ? (
                    <Box component="img" src={shareQr} alt="QR de contacto" sx={{ width: '100%', height: '100%' }} />
                  ) : shareQrError ? (
                    <Typography variant="caption" color="error">{shareQrError}</Typography>
                  ) : (
                    <CircularProgress size={20} />
                  )}
                </Box>
                <Stack spacing={1} flex={1}>
                  <Typography variant="body2" color="text.secondary">
                    Escanea este QR o comparte el payload para agregar contactos rápidamente.
                  </Typography>
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                    <Button
                      variant="outlined"
                      startIcon={<ContentCopyIcon />}
                      onClick={() => handleCopy(sharePayload)}
                    >
                      Copiar payload
                    </Button>
                    {profileUrl && (
                      <Button
                        variant="text"
                        startIcon={<LinkIcon />}
                        onClick={() => handleCopy(profileUrl)}
                      >
                        Copiar link de perfil
                      </Button>
                    )}
                  </Stack>
                  {copyMessage && (
                    <Typography variant="caption" color="text.secondary">
                      {copyMessage}
                    </Typography>
                  )}
                </Stack>
              </Stack>
            </Stack>
          </CardContent>
        </Card>

        <Card sx={{ width: { md: 360, xs: '100%' } }}>
          <CardContent>
            <Stack spacing={2}>
              <Typography variant="h6" fontWeight={800}>Registrar intercambio</Typography>
              <Typography color="text.secondary">
                Pega el texto leído desde un QR (o un party ID) y lo enviaremos como intercambio vCard.
              </Typography>
              <TextField
                label="Payload escaneado o ID"
                value={payloadInput}
                onChange={(e) => setPayloadInput(e.target.value)}
                multiline
                minRows={4}
              />
              {parsedPayload ? (
                <Stack
                  spacing={0.5}
                  sx={{
                    bgcolor: 'rgba(148,163,184,0.08)',
                    borderRadius: 1.5,
                    p: 1.25,
                    border: '1px solid',
                    borderColor: 'divider',
                  }}
                >
                  {parsedPayload.partyId && (
                    <Typography variant="body2" fontWeight={700}>
                      ID destino: {parsedPayload.partyId}
                    </Typography>
                  )}
                  {parsedPayload.name && <Typography variant="body2">Nombre: {parsedPayload.name}</Typography>}
                  {parsedPayload.email && <Typography variant="body2">Correo: {parsedPayload.email}</Typography>}
                  {parsedPayload.phone && <Typography variant="body2">Teléfono: {parsedPayload.phone}</Typography>}
                </Stack>
              ) : (
                <Typography variant="body2" color="text.secondary">
                  Esperando un payload válido...
                </Typography>
              )}
              <Button
                variant="contained"
                startIcon={<QrCodeScannerIcon />}
                onClick={() => exchangeMutation.mutate()}
                disabled={!parsedPayload?.partyId || exchangeMutation.status === 'pending'}
              >
                {exchangeMutation.status === 'pending' ? 'Enviando...' : 'Registrar intercambio'}
              </Button>
              {exchangeMutation.error && <Alert severity="error">{exchangeMutation.error.message}</Alert>}
            </Stack>
          </CardContent>
        </Card>
      </Stack>

      <Card sx={{ mb: 2 }}>
        <CardContent>
          <Stack spacing={1.5}>
            <Stack direction="row" alignItems="center" justifyContent="space-between">
              <Typography variant="h6" fontWeight={800}>Sugerencias de amigos</Typography>
              <Button
                size="small"
                variant="text"
                startIcon={<RefreshIcon />}
                onClick={() => suggestionsQuery.refetch()}
                disabled={suggestionsQuery.isFetching}
              >
                Actualizar
              </Button>
            </Stack>
            {suggestionsQuery.error ? (
              <Alert severity="error">No pudimos cargar sugerencias. Intenta de nuevo.</Alert>
            ) : suggestionsQuery.isLoading ? (
              <Stack direction="row" spacing={1.5} alignItems="center">
                <CircularProgress size={18} />
                <Typography color="text.secondary">Buscando conexiones...</Typography>
              </Stack>
            ) : (suggestionsQuery.data?.length ?? 0) === 0 ? (
              <Alert severity="info">No tenemos sugerencias todavía. Conecta con más personas y vuelve a intentar.</Alert>
            ) : (
              <Stack divider={<Divider flexItem />} spacing={1}>
                {suggestionsQuery.data?.map((suggestion) => {
                  const label = formatParty(byId, suggestion.sfPartyId);
                  return (
                    <Stack key={`suggestion-${suggestion.sfPartyId}`} direction="row" justifyContent="space-between" alignItems="center">
                      <Box>
                        <Typography variant="subtitle1" fontWeight={700}>{label}</Typography>
                        <Typography variant="body2" color="text.secondary">
                          ID #{suggestion.sfPartyId}
                        </Typography>
                        <Chip
                          label={`${suggestion.sfMutualCount} conexión${suggestion.sfMutualCount === 1 ? '' : 'es'} en común`}
                          size="small"
                          color="info"
                          sx={{ mt: 0.5 }}
                        />
                      </Box>
                      <Button
                        variant="contained"
                        size="small"
                        startIcon={<PersonAddAltIcon />}
                        onClick={() => addMutation.mutate(suggestion.sfPartyId)}
                        disabled={addMutation.status === 'pending'}
                      >
                        Conectar
                      </Button>
                    </Stack>
                  );
                })}
              </Stack>
            )}
          </Stack>
        </CardContent>
      </Card>

      <Card>
        <CardContent>
          <Tabs
            value={activeTab}
            onChange={(_e, val: TabKey) => setActiveTab(val)}
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
                          <Button
                            variant="text"
                            size="small"
                            startIcon={<OpenInNewIcon />}
                            component="a"
                            href={`/perfil/${targetId}`}
                            target="_blank"
                            rel="noreferrer"
                          >
                            Ver perfil
                          </Button>
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
