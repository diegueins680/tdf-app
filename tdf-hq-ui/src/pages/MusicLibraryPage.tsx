import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import KeyboardArrowDownIcon from '@mui/icons-material/KeyboardArrowDown';
import KeyboardArrowUpIcon from '@mui/icons-material/KeyboardArrowUp';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Divider,
  IconButton,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { useState } from 'react';
import { Link as RouterLink } from 'react-router-dom';

import {
  musicReleases,
  type MusicLibraryTrack,
  type MusicPlaylist,
  type MusicPlaylistItem,
} from '../api/musicReleases';
import { useDocumentTitle } from '../hooks/useDocumentTitle';
import { PLAYER_LOAD_TRACK_EVENT, type PlayerTrack } from '../player/types';
import { musicSourceQuality } from '../player/sourceMetadata';
import { useSession } from '../session/SessionContext';

const loudnessFor = (metadata: Record<string, unknown>): number | null =>
  typeof metadata['loudness_lufs'] === 'number' ? metadata['loudness_lufs'] : null;

const resolveTrack = async (entry: MusicLibraryTrack): Promise<PlayerTrack> => {
  if (!entry.available || !entry.trackId || !entry.releaseId || !entry.releaseVersionId) {
    throw new Error(`${entry.title} ya no está disponible.`);
  }
  const sources = await Promise.all(entry.sources.map(async (source) => {
    const access = await musicReleases.getAssetAccess(source.assetId);
    return {
      url: access.url,
      assetId: source.assetId,
      expiresAt: access.expiresAt,
      quality: musicSourceQuality(source.technicalMetadata, access.mediaType),
      mediaType: access.mediaType,
      preview: source.role === 'preview_audio',
    };
  }));
  if (sources.length === 0) throw new Error(`${entry.title} no tiene audio autorizado.`);
  const previewOnly = entry.sources.some((source) => source.role === 'preview_audio')
    && !entry.sources.some((source) => source.role === 'stream_audio');
  return {
    id: entry.trackId,
    recordingId: entry.recordingId,
    releaseId: entry.releaseId,
    releaseVersionId: entry.releaseVersionId,
    title: entry.title,
    artist: entry.displayArtist ?? 'Artista',
    durationMs: entry.durationMs,
    previewEndMs: previewOnly ? 30000 : null,
    loudnessLufs: entry.sources.map((source) => loudnessFor(source.technicalMetadata)).find((value) => value !== null) ?? null,
    sources,
  };
};

export default function MusicLibraryPage() {
  useDocumentTitle('Mi biblioteca musical');
  const { session } = useSession();
  const listenerKey = session?.partyId ?? session?.username ?? null;
  const queryClient = useQueryClient();
  const [name, setName] = useState('');
  const [visibility, setVisibility] = useState<MusicPlaylist['visibility']>('private');
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const favorites = useQuery({ queryKey: ['music-favorites', listenerKey], queryFn: musicReleases.listFavorites, enabled: Boolean(session), retry: false });
  const playlists = useQuery({ queryKey: ['music-playlists', listenerKey], queryFn: musicReleases.listPlaylists, enabled: Boolean(session), retry: false });
  const history = useQuery({ queryKey: ['music-history', listenerKey], queryFn: musicReleases.playbackHistory, enabled: Boolean(session), retry: false });

  const refreshLibrary = () => queryClient.invalidateQueries({ queryKey: ['music-playlists'] });

  const playEntries = async (entries: MusicLibraryTrack[], selectedRecordingId?: string, startPositionMs?: number) => {
    setBusy(true); setError(null);
    try {
      const playable = entries.filter((entry) => entry.available && entry.sources.length > 0);
      const resolved = await Promise.allSettled(playable.map(resolveTrack));
      const queue = resolved.flatMap((result) => result.status === 'fulfilled' ? [result.value] : []);
      const selectedEntry = selectedRecordingId
        ? playable.find((entry) => entry.recordingId === selectedRecordingId)
        : playable[0];
      const selected = queue.find((track) => track.recordingId === selectedEntry?.recordingId) ?? queue[0];
      if (!selected) throw new Error('No hay pistas autorizadas disponibles en esta colección.');
      window.dispatchEvent(new CustomEvent(PLAYER_LOAD_TRACK_EVENT, {
        detail: { track: selected, queue, autoplay: true, startPositionMs },
      }));
    } catch (reason) { setError(reason instanceof Error ? reason.message : 'No se pudo cargar la colección.'); }
    finally { setBusy(false); }
  };

  const createPlaylist = async () => {
    if (!name.trim()) return;
    setBusy(true); setError(null);
    try {
      await musicReleases.createPlaylist(name.trim(), visibility);
      setName(''); await refreshLibrary();
    } catch (reason) { setError(reason instanceof Error ? reason.message : 'No se pudo crear la playlist.'); }
    finally { setBusy(false); }
  };

  const moveItem = async (playlist: MusicPlaylist, item: MusicPlaylistItem, delta: number) => {
    const position = item.position + delta;
    if (position < 0 || position >= playlist.items.length) return;
    setBusy(true); setError(null);
    try { await musicReleases.movePlaylistItem(playlist.id, item.id, position); await refreshLibrary(); }
    catch (reason) { setError(reason instanceof Error ? reason.message : 'No se pudo reordenar la playlist.'); }
    finally { setBusy(false); }
  };

  const removeItem = async (playlistId: string, itemId: string) => {
    setBusy(true); setError(null);
    try { await musicReleases.removePlaylistItem(playlistId, itemId); await refreshLibrary(); }
    catch (reason) { setError(reason instanceof Error ? reason.message : 'No se pudo quitar la pista.'); }
    finally { setBusy(false); }
  };

  const deletePlaylist = async (playlist: MusicPlaylist) => {
    if (!window.confirm(`¿Eliminar la playlist “${playlist.name}”? Esta acción no elimina las grabaciones.`)) return;
    setBusy(true); setError(null);
    try { await musicReleases.deletePlaylist(playlist.id); await refreshLibrary(); }
    catch (reason) { setError(reason instanceof Error ? reason.message : 'No se pudo eliminar la playlist.'); }
    finally { setBusy(false); }
  };

  const loading = favorites.isLoading || playlists.isLoading || history.isLoading;
  const queryFailed = favorites.isError || playlists.isError || history.isError;
  return <Stack spacing={3} sx={{ maxWidth: 980, mx: 'auto', py: 4, px: 2 }}>
    <Box><Typography component="h1" variant="h3" fontWeight={900}>Mi biblioteca musical</Typography>
      <Typography color="text.secondary">Favoritos, playlists e historial sincronizados con tu cuenta.</Typography></Box>
    <Button component={RouterLink} to="/musica" sx={{ alignSelf: 'flex-start' }}>Explorar catálogo</Button>
    {loading && <CircularProgress aria-label="Cargando biblioteca" />}
    {queryFailed && <Alert severity="warning">La biblioteca musical no está habilitada o no pudo cargarse.</Alert>}
    {error && <Alert severity="error" onClose={() => setError(null)}>{error}</Alert>}

    <Card><CardContent><Stack spacing={2}>
      <Typography variant="h5" fontWeight={800}>Favoritos</Typography>
      <Button startIcon={<PlayArrowIcon />} disabled={busy || !favorites.data?.some((item) => item.available)}
        onClick={() => void playEntries(favorites.data ?? [])} sx={{ alignSelf: 'flex-start' }}>Reproducir favoritos</Button>
      {(favorites.data ?? []).map((item) => <Stack key={item.recordingId} direction="row" spacing={1} alignItems="center">
        <Box flex={1}><Typography>{item.title}</Typography><Typography variant="caption" color="text.secondary">{item.displayArtist ?? 'No disponible'}</Typography></Box>
        {!item.available && <Chip label="No disponible" size="small" />}
        <IconButton aria-label={`Quitar ${item.title} de favoritos`} disabled={busy} onClick={() => void musicReleases.unfavorite(item.recordingId).then(() => queryClient.invalidateQueries({ queryKey: ['music-favorites'] }))}><DeleteOutlineIcon /></IconButton>
      </Stack>)}
      {favorites.data?.length === 0 && <Typography color="text.secondary">Aún no guardaste favoritos.</Typography>}
    </Stack></CardContent></Card>

    <Card><CardContent><Stack spacing={2}>
      <Typography variant="h5" fontWeight={800}>Playlists</Typography>
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
        <TextField label="Nueva playlist" value={name} onChange={(event) => setName(event.target.value)} fullWidth inputProps={{ maxLength: 200 }} />
        <TextField select label="Visibilidad" value={visibility} onChange={(event) => setVisibility(event.target.value as MusicPlaylist['visibility'])}>
          <MenuItem value="private">Privada</MenuItem><MenuItem value="unlisted">No listada</MenuItem><MenuItem value="public">Pública</MenuItem>
        </TextField>
        <Button variant="contained" disabled={busy || !name.trim()} onClick={() => void createPlaylist()}>Crear</Button>
      </Stack>
      {(playlists.data ?? []).map((playlist) => <Stack key={playlist.id} spacing={1.5} sx={{ p: 2, border: '1px solid', borderColor: 'divider', borderRadius: 2 }}>
        <Stack direction="row" spacing={1} alignItems="center"><Box flex={1}><Typography fontWeight={800}>{playlist.name}</Typography><Typography variant="caption">{playlist.visibility} · {playlist.items.length} pistas</Typography></Box>
          <Button startIcon={<PlayArrowIcon />} disabled={busy || !playlist.items.some((item) => item.available)} onClick={() => void playEntries(playlist.items)}>Reproducir</Button>
          <IconButton aria-label={`Eliminar playlist ${playlist.name}`} disabled={busy} onClick={() => void deletePlaylist(playlist)}><DeleteOutlineIcon /></IconButton>
        </Stack>
        <Divider />
        {playlist.items.map((item, index) => <Stack key={item.id} direction="row" spacing={0.5} alignItems="center">
          <Typography width={28}>{index + 1}</Typography><Box flex={1}><Typography>{item.title}</Typography><Typography variant="caption" color="text.secondary">{item.displayArtist ?? 'No disponible'}</Typography></Box>
          {!item.available && <Chip label="No disponible" size="small" />}
          <IconButton aria-label={`Subir ${item.title}`} disabled={busy || index === 0} onClick={() => void moveItem(playlist, item, -1)}><KeyboardArrowUpIcon /></IconButton>
          <IconButton aria-label={`Bajar ${item.title}`} disabled={busy || index === playlist.items.length - 1} onClick={() => void moveItem(playlist, item, 1)}><KeyboardArrowDownIcon /></IconButton>
          <IconButton aria-label={`Quitar ${item.title}`} disabled={busy} onClick={() => void removeItem(playlist.id, item.id)}><DeleteOutlineIcon /></IconButton>
        </Stack>)}
      </Stack>)}
    </Stack></CardContent></Card>

    <Card><CardContent><Stack spacing={2}>
      <Typography variant="h5" fontWeight={800}>Historial</Typography>
      {(history.data ?? []).map((item) => <Stack key={`${item.recordingId}-${item.lastPlayedAt}`} direction="row" spacing={1} alignItems="center">
        <Box flex={1}><Typography>{item.title}</Typography><Typography variant="caption" color="text.secondary">{item.playCount} reproducciones · {new Date(item.lastPlayedAt).toLocaleString('es-EC')}</Typography></Box>
        {!item.available && <Chip label="No disponible" size="small" />}
        <Button startIcon={<PlayArrowIcon />} disabled={busy || !item.available} onClick={() => void playEntries([item], item.recordingId, item.positionMs)}>Continuar</Button>
      </Stack>)}
      {history.data?.length === 0 && <Typography color="text.secondary">El historial aparecerá después de reproducir música.</Typography>}
    </Stack></CardContent></Card>
  </Stack>;
}
