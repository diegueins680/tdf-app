import FavoriteBorderIcon from '@mui/icons-material/FavoriteBorder';
import DownloadIcon from '@mui/icons-material/Download';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import QueueMusicIcon from '@mui/icons-material/QueueMusic';
import PlaylistAddIcon from '@mui/icons-material/PlaylistAdd';
import ShoppingCartIcon from '@mui/icons-material/ShoppingCart';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Checkbox,
  Chip,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Divider,
  FormControlLabel,
  IconButton,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { useEffect, useMemo, useRef, useState } from 'react';
import { Link as RouterLink, useLocation, useNavigate, useParams } from 'react-router-dom';

import { musicReleases, type MusicPublicRelease } from '../api/musicReleases';
import { useDocumentTitle } from '../hooks/useDocumentTitle';
import { PLAYER_LOAD_TRACK_EVENT, type PlayerTrack } from '../player/types';
import { musicSourceQuality } from '../player/sourceMetadata';
import { useSession } from '../session/SessionContext';
import { buildLoginRedirectPath } from '../utils/loginRouting';

const loudnessFor = (metadata: Record<string, unknown>): number | null =>
  typeof metadata['loudness_lufs'] === 'number' ? metadata['loudness_lufs'] : null;

type PublicOffer = MusicPublicRelease['availability'][number];

const openAuthorizedDownload = (accessUrl: string) => {
  const anchor = document.createElement('a');
  anchor.href = accessUrl;
  anchor.rel = 'noopener noreferrer';
  anchor.target = '_blank';
  document.body.appendChild(anchor);
  anchor.click();
  anchor.remove();
};

const formatMinorMoney = (minor: number, currency: string) => {
  try { return new Intl.NumberFormat('es-EC', { style: 'currency', currency }).format(minor / 100); }
  catch { return `${currency} ${(minor / 100).toFixed(2)}`; }
};

const installSocialMetadata = (release: MusicPublicRelease, artworkUrl: string | null) => {
  const canonicalUrl = new URL(`/musica/${release.slug}`, window.location.origin).toString();
  const definitions: [string, string, string][] = [
    ['property', 'og:type', 'music.album'], ['property', 'og:title', `${release.title} — ${release.displayArtist}`],
    ['property', 'og:url', canonicalUrl], ['name', 'twitter:card', 'summary_large_image'],
  ];
  if (artworkUrl) definitions.push(['property', 'og:image', artworkUrl]);
  const nodes: HTMLElement[] = definitions.map(([attribute, key, content]) => {
    const node = document.createElement('meta');
    node.setAttribute(attribute, key); node.content = content; node.dataset['tdfMusicRelease'] = 'true';
    document.head.appendChild(node); return node;
  });
  const canonical = document.createElement('link'); canonical.rel = 'canonical'; canonical.href = canonicalUrl;
  canonical.dataset['tdfMusicRelease'] = 'true'; document.head.appendChild(canonical); nodes.push(canonical);
  const structured = document.createElement('script'); structured.type = 'application/ld+json';
  structured.dataset['tdfMusicRelease'] = 'true';
  structured.text = JSON.stringify({
    '@context': 'https://schema.org', '@type': release.kind === 'single' ? 'MusicRecording' : 'MusicAlbum',
    name: release.title, byArtist: { '@type': 'MusicGroup', name: release.displayArtist }, url: canonicalUrl,
    image: artworkUrl ?? undefined, numTracks: release.tracks.length,
  });
  document.head.appendChild(structured);
  return () => [...nodes, structured].forEach((node) => node.remove());
};

export default function MusicReleasePublicPage() {
  const { slug = '' } = useParams();
  const { session } = useSession();
  const listenerKey = session?.partyId ?? session?.username ?? null;
  const navigate = useNavigate();
  const location = useLocation();
  const [artworkUrl, setArtworkUrl] = useState<string | null>(null);
  const [playError, setPlayError] = useState<string | null>(null);
  const [loadingTrack, setLoadingTrack] = useState<string | null>(null);
  const [favoriteBusy, setFavoriteBusy] = useState(false);
  const [selectedOffer, setSelectedOffer] = useState<PublicOffer | null>(null);
  const [purchaseTermsAccepted, setPurchaseTermsAccepted] = useState(false);
  const [purchaseBusy, setPurchaseBusy] = useState(false);
  const [paypalReady, setPaypalReady] = useState(false);
  const [paypalPurchase, setPaypalPurchase] = useState<{ purchaseId: string; orderId: string } | null>(null);
  const [playlistTrack, setPlaylistTrack] = useState<{ recordingId: string; title: string } | null>(null);
  const [reportOpen, setReportOpen] = useState(false);
  const [reportReason, setReportReason] = useState<'copyright' | 'master_rights' | 'composition_rights' | 'impersonation' | 'metadata' | 'other'>('copyright');
  const [reportDescription, setReportDescription] = useState('');
  const paypalButtonRef = useRef<HTMLDivElement | null>(null);
  const paypalClientId = import.meta.env?.VITE_PAYPAL_CLIENT_ID?.trim() ?? '';
  const release = useQuery({
    queryKey: ['music-release-public', slug], queryFn: () => musicReleases.getPublic(slug), retry: false,
  });
  const favorites = useQuery({
    queryKey: ['music-favorites', listenerKey], queryFn: musicReleases.listFavorites, enabled: Boolean(session), retry: false,
  });
  const favoriteIds = useMemo(() => new Set((favorites.data ?? []).map((item) => item.recordingId)), [favorites.data]);
  const favoriteUnavailable = Boolean(session) && (favoriteBusy || favorites.isFetching || !favorites.isSuccess);
  const playlists = useQuery({
    queryKey: ['music-playlists', listenerKey], queryFn: musicReleases.listPlaylists, enabled: Boolean(session), retry: false,
  });
  useDocumentTitle(release.data ? `${release.data.title} — ${release.data.displayArtist}` : 'Lanzamiento');

  useEffect(() => {
    const cover = release.data?.coverAssets.find((asset) => asset.role === 'cover_display') ?? release.data?.coverAssets[0];
    if (!cover) { setArtworkUrl(null); return undefined; }
    let active = true;
    void musicReleases.getAssetAccess(cover.assetId).then((access) => {
      if (active) setArtworkUrl(access.url);
    }).catch(() => { if (active) setArtworkUrl(null); });
    return () => { active = false; };
  }, [release.data]);

  useEffect(() => release.data ? installSocialMetadata(release.data, artworkUrl) : undefined, [artworkUrl, release.data]);

  useEffect(() => {
    if (selectedOffer?.downloadPolicy !== 'purchase' || !paypalClientId || typeof window === 'undefined') return undefined;
    if (window.paypal) { setPaypalReady(true); return undefined; }
    const script = document.createElement('script');
    script.src = `https://www.paypal.com/sdk/js?client-id=${encodeURIComponent(paypalClientId)}&currency=${encodeURIComponent(selectedOffer.currency ?? 'USD')}`;
    script.async = true;
    script.onload = () => setPaypalReady(true);
    script.onerror = () => setPlayError('PayPal no pudo cargarse. No se confirmó ningún pago.');
    document.body.appendChild(script);
    return () => script.remove();
  }, [paypalClientId, selectedOffer]);

  useEffect(() => {
    if (!paypalPurchase || !paypalReady || !window.paypal || !paypalButtonRef.current || !release.data) return undefined;
    paypalButtonRef.current.innerHTML = '';
    const buttons = window.paypal.Buttons({
      createOrder: () => paypalPurchase.orderId,
      onApprove: async (data) => {
        if (data.orderID !== paypalPurchase.orderId) {
          setPlayError('PayPal devolvió una referencia distinta. No se capturó ningún pago.'); return;
        }
        setPurchaseBusy(true);
        try {
          const paid = await musicReleases.capturePaypalOrder(paypalPurchase.purchaseId, paypalPurchase.orderId, crypto.randomUUID());
          if (paid.state !== 'paid') throw new Error('El proveedor respondió, pero el servidor aún no confirmó el pago.');
          const entitlements = await musicReleases.listEntitlements();
          const entitlement = entitlements.find((item) => item.releaseVersionId === paid.releaseVersionId && item.status === 'active' && item.sourceKind === 'purchase');
          if (!entitlement) throw new Error('El pago fue verificado, pero el permiso de descarga aún no está disponible.');
          const access = await musicReleases.authorizeDownload(entitlement.id, crypto.randomUUID());
          openAuthorizedDownload(access.url);
          setSelectedOffer(null); setPaypalPurchase(null); setPurchaseTermsAccepted(false);
        } catch (reason) {
          setPlayError(reason instanceof Error ? reason.message : 'El servidor no pudo verificar el pago.');
        } finally { setPurchaseBusy(false); }
      },
      onCancel: () => setPlayError('Cancelaste PayPal; la orden permanece sin pago confirmado.'),
      onError: () => setPlayError('PayPal no completó la operación; no se confirmó ningún pago.'),
    });
    void buttons.render(paypalButtonRef.current);
    return () => buttons.close?.();
  }, [paypalPurchase, paypalReady, release.data]);

  const resolveTrack = async (track: MusicPublicRelease['tracks'][number]): Promise<PlayerTrack> => {
    const attempts = await Promise.allSettled(track.sources.map(async (source) => {
      const access = await musicReleases.getAssetAccess(source.assetId);
      return {
        url: access.url, assetId: source.assetId, expiresAt: access.expiresAt,
        quality: musicSourceQuality(source.technicalMetadata, access.mediaType), mediaType: access.mediaType,
        preview: source.role === 'preview_audio',
      };
    }));
    const resolved = attempts.flatMap((attempt) => attempt.status === 'fulfilled' ? [attempt.value] : []);
    if (resolved.length === 0) throw new Error('Esta pista no tiene una fuente autorizada disponible.');
    const preview = resolved.every((source) => source.preview);
    const authorizedAssetIds = new Set(resolved.map((source) => source.assetId));
    const loudness = track.sources.filter((source) => authorizedAssetIds.has(source.assetId))
      .map((source) => loudnessFor(source.technicalMetadata)).find((value) => value !== null) ?? null;
    return {
      id: track.trackId, recordingId: track.recordingId, releaseId: release.data?.id,
      releaseVersionId: release.data?.versionId, title: track.title, artist: track.displayArtist,
      artworkUrl, durationMs: track.durationMs, previewEndMs: preview ? 30000 : null,
      loudnessLufs: loudness, sources: resolved,
    };
  };

  const play = async (trackId: string) => {
    if (!release.data) return;
    setLoadingTrack(trackId); setPlayError(null);
    try {
      const attempts = await Promise.allSettled(release.data.tracks.map(resolveTrack));
      const queue = attempts.flatMap((attempt) => attempt.status === 'fulfilled' ? [attempt.value] : []);
      const selected = queue.find((track) => track.id === trackId);
      if (!selected) throw new Error('La pista ya no está disponible.');
      window.dispatchEvent(new CustomEvent(PLAYER_LOAD_TRACK_EVENT, { detail: { track: selected, queue, autoplay: true } }));
    } catch (reason) {
      setPlayError(reason instanceof Error ? reason.message : 'No se pudo iniciar la reproducción.');
    } finally { setLoadingTrack(null); }
  };

  const toggleFavorite = async (recordingId: string) => {
    if (!session) {
      navigate(buildLoginRedirectPath(location.pathname)); return;
    }
    if (favoriteUnavailable) return;
    setFavoriteBusy(true);
    try {
      if (favoriteIds.has(recordingId)) await musicReleases.unfavorite(recordingId);
      else await musicReleases.favorite(recordingId);
      await favorites.refetch({ throwOnError: true });
    } catch (reason) { setPlayError(reason instanceof Error ? reason.message : 'No se pudo guardar el favorito.'); }
    finally { setFavoriteBusy(false); }
  };

  const freeDownload = async (offer: PublicOffer) => {
    if (!session) { navigate(buildLoginRedirectPath(location.pathname)); return; }
    setPurchaseBusy(true); setPlayError(null);
    try {
      const access = await musicReleases.freeDownload(offer.ruleId, crypto.randomUUID());
      openAuthorizedDownload(access.url);
    } catch (reason) {
      setPlayError(reason instanceof Error ? reason.message : 'No se pudo autorizar la descarga.');
    } finally { setPurchaseBusy(false); }
  };

  const addToPlaylist = async (playlistId: string) => {
    if (!playlistTrack) return;
    const playlist = playlists.data?.find((entry) => entry.id === playlistId);
    if (!playlist) return;
    setPurchaseBusy(true); setPlayError(null);
    try {
      await musicReleases.addPlaylistItem(playlistId, playlistTrack.recordingId, playlist.items.length);
      await playlists.refetch(); setPlaylistTrack(null);
    } catch (reason) { setPlayError(reason instanceof Error ? reason.message : 'No se pudo añadir la pista.'); }
    finally { setPurchaseBusy(false); }
  };

  const startPurchase = async () => {
    if (!selectedOffer || !purchaseTermsAccepted) return;
    if (!session) { navigate(buildLoginRedirectPath(location.pathname)); return; }
    if (!paypalClientId) { setPlayError('PayPal no está configurado en este navegador.'); return; }
    setPurchaseBusy(true); setPlayError(null);
    try {
      const purchase = await musicReleases.createPurchase(selectedOffer.ruleId, crypto.randomUUID());
      const provider = await musicReleases.createPaypalOrder(purchase.id, crypto.randomUUID());
      setPaypalPurchase({ purchaseId: purchase.id, orderId: provider.paypalOrderId });
    } catch (reason) {
      setPlayError(reason instanceof Error ? reason.message : 'No se pudo iniciar la compra.');
    } finally { setPurchaseBusy(false); }
  };

  const reportInfringement = async () => {
    if (!session) { navigate(buildLoginRedirectPath(location.pathname)); return; }
    if (!reportDescription.trim() || !release.data) return;
    setPurchaseBusy(true); setPlayError(null);
    try {
      await musicReleases.reportInfringement(release.data.id, reportReason, reportDescription.trim(), crypto.randomUUID());
      setReportOpen(false); setReportDescription('');
    } catch (reason) { setPlayError(reason instanceof Error ? reason.message : 'No se pudo registrar el reporte.'); }
    finally { setPurchaseBusy(false); }
  };

  const releaseDate = useMemo(() => release.data?.releaseAtUtc ?? release.data?.originalReleaseDate, [release.data]);
  if (release.isLoading) return <Box display="grid" minHeight="45vh" sx={{ placeItems: 'center' }}><CircularProgress /></Box>;
  if (release.isError || !release.data) return <Alert severity="warning">Este lanzamiento no está publicado, sigue bajo embargo o fue retirado.</Alert>;

  return <Stack spacing={3} sx={{ maxWidth: 980, mx: 'auto', py: 4, px: 2 }}>
    {playError && <Alert severity="error" onClose={() => setPlayError(null)}>{playError}</Alert>}
    {session && favorites.isError && <Alert severity="warning">No se pudieron cargar tus favoritos. Recarga la página para reintentar.</Alert>}
    <Stack direction={{ xs: 'column', md: 'row' }} spacing={4} alignItems={{ md: 'center' }}>
      <Box component={artworkUrl ? 'img' : 'div'} src={artworkUrl ?? undefined} alt={artworkUrl ? `Portada de ${release.data.title}` : undefined}
        sx={{ width: { xs: '100%', md: 360 }, aspectRatio: '1', objectFit: 'cover', borderRadius: 3, bgcolor: 'action.hover', boxShadow: 6 }} />
      <Stack spacing={1.5} flex={1}>
        <Chip label={release.data.kind.toUpperCase()} sx={{ alignSelf: 'flex-start' }} />
        <Typography variant="h2" component="h1" fontWeight={800}>{release.data.title}</Typography>
        <Typography variant="h5" color="text.secondary">{release.data.displayArtist}</Typography>
        <Typography color="text.secondary">{[release.data.labelName, releaseDate].filter(Boolean).join(' · ')}</Typography>
        <Button variant="contained" size="large" startIcon={<PlayArrowIcon />} onClick={() => void play(release.data.tracks[0]?.trackId ?? '')}
          disabled={release.data.tracks.length === 0 || loadingTrack !== null} sx={{ alignSelf: 'flex-start' }}>
          Reproducir release
        </Button>
        {session && <Button component={RouterLink} to="/musica/biblioteca" sx={{ alignSelf: 'flex-start' }}>Abrir mi biblioteca</Button>}
      </Stack>
    </Stack>
    <Card><CardContent><Stack divider={<Divider flexItem />}>
      {release.data.tracks.map((track) => {
        const offer = release.data.availability.find((rule) => rule.trackId === track.trackId)
          ?? release.data.availability.find((rule) => rule.trackId === null);
        return <Stack key={track.trackId} direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ sm: 'center' }} sx={{ py: 1.5 }}>
          <Typography width={28} textAlign="right" color="text.secondary">{track.trackNumber}</Typography>
          <Box flex={1} minWidth={0}><Typography fontWeight={600} noWrap>{track.title}</Typography>
            <Typography variant="body2" color="text.secondary" noWrap>{track.displayArtist}{track.explicitContent === 'explicit' ? ' · E' : ''}</Typography></Box>
          <Typography variant="body2" color="text.secondary">{Math.floor(track.durationMs / 60000)}:{String(Math.floor(track.durationMs / 1000) % 60).padStart(2, '0')}</Typography>
          <IconButton aria-label={favoriteIds.has(track.recordingId) ? `Quitar ${track.title} de favoritos` : `Guardar ${track.title} en favoritos`} disabled={favoriteUnavailable} onClick={() => void toggleFavorite(track.recordingId)} color={favoriteIds.has(track.recordingId) ? 'primary' : 'default'}><FavoriteBorderIcon /></IconButton>
          <IconButton aria-label={`Añadir ${track.title} a una playlist`} onClick={() => {
            if (!session) navigate(buildLoginRedirectPath(location.pathname));
            else setPlaylistTrack({ recordingId: track.recordingId, title: track.title });
          }}><PlaylistAddIcon /></IconButton>
          <Button startIcon={loadingTrack === track.trackId ? <CircularProgress size={16} /> : <QueueMusicIcon />} onClick={() => void play(track.trackId)} disabled={loadingTrack !== null}>Reproducir</Button>
          {offer?.downloadPolicy === 'free' && <Button startIcon={<DownloadIcon />} onClick={() => void freeDownload(offer)} disabled={purchaseBusy}>Descargar</Button>}
          {offer?.downloadPolicy === 'purchase' && offer.priceMinor !== null && offer.currency && <Button startIcon={<ShoppingCartIcon />} onClick={() => { setSelectedOffer(offer); setPaypalPurchase(null); setPurchaseTermsAccepted(false); }} disabled={purchaseBusy}>Comprar {formatMinorMoney(offer.priceMinor, offer.currency)}</Button>}
        </Stack>;
      })}
    </Stack></CardContent></Card>
    <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
      <Typography variant="caption" color="text.secondary" flex={1}>Las reproducciones operativas no constituyen contabilidad certificada de regalías.</Typography>
      <Button color="warning" size="small" onClick={() => {
        if (!session) navigate(buildLoginRedirectPath(location.pathname));
        else setReportOpen(true);
      }}>Reportar una infracción</Button>
    </Stack>
    <Dialog open={selectedOffer !== null} onClose={() => { if (!purchaseBusy) { setSelectedOffer(null); setPaypalPurchase(null); } }} maxWidth="xs" fullWidth>
      <DialogTitle>Compra y descarga protegida</DialogTitle>
      <DialogContent><Stack spacing={2} sx={{ pt: 1 }}>
        {selectedOffer?.priceMinor !== null && selectedOffer?.currency && <Typography variant="h5">{formatMinorMoney(selectedOffer.priceMinor, selectedOffer.currency)}</Typography>}
        <Alert severity="info">TDF solo habilita la descarga después de verificar en servidor el proveedor, importe, moneda y referencia de esta orden.</Alert>
        <FormControlLabel control={<Checkbox checked={purchaseTermsAccepted} onChange={(event) => setPurchaseTermsAccepted(event.target.checked)} disabled={purchaseBusy || paypalPurchase !== null} />} label="Acepto los términos vigentes de la compra y descarga digital." />
        {paypalPurchase && <Box ref={paypalButtonRef} sx={{ minHeight: 48 }} />}
      </Stack></DialogContent>
      <DialogActions>
        <Button onClick={() => { setSelectedOffer(null); setPaypalPurchase(null); }} disabled={purchaseBusy}>Cancelar</Button>
        {!paypalPurchase && <Button variant="contained" onClick={() => void startPurchase()} disabled={!purchaseTermsAccepted || purchaseBusy || !paypalReady}>{purchaseBusy ? 'Preparando…' : 'Continuar con PayPal'}</Button>}
      </DialogActions>
    </Dialog>
    <Dialog open={playlistTrack !== null} onClose={() => { if (!purchaseBusy) setPlaylistTrack(null); }} maxWidth="xs" fullWidth>
      <DialogTitle>Añadir “{playlistTrack?.title}”</DialogTitle>
      <DialogContent><Stack spacing={1} sx={{ pt: 1 }}>
        {(playlists.data ?? []).map((playlist) => <Button key={playlist.id} variant="outlined" disabled={purchaseBusy}
          onClick={() => void addToPlaylist(playlist.id)}>{playlist.name} · {playlist.items.length} pistas</Button>)}
        {playlists.data?.length === 0 && <Alert severity="info">Crea primero una playlist en tu biblioteca.</Alert>}
      </Stack></DialogContent>
      <DialogActions><Button component={RouterLink} to="/musica/biblioteca">Administrar playlists</Button><Button onClick={() => setPlaylistTrack(null)} disabled={purchaseBusy}>Cerrar</Button></DialogActions>
    </Dialog>
    <Dialog open={reportOpen} onClose={() => { if (!purchaseBusy) setReportOpen(false); }} maxWidth="sm" fullWidth>
      <DialogTitle>Reportar una posible infracción</DialogTitle>
      <DialogContent><Stack spacing={2} sx={{ pt: 1 }}>
        <Alert severity="info">El reporte queda asociado a tu cuenta y se conserva como evidencia auditable. No incluyas datos personales innecesarios.</Alert>
        <TextField select label="Motivo" value={reportReason} onChange={(event) => setReportReason(event.target.value as typeof reportReason)}>
          <MenuItem value="copyright">Copyright</MenuItem><MenuItem value="master_rights">Derechos de máster</MenuItem><MenuItem value="composition_rights">Derechos de composición</MenuItem><MenuItem value="impersonation">Suplantación</MenuItem><MenuItem value="metadata">Metadatos engañosos</MenuItem><MenuItem value="other">Otro</MenuItem>
        </TextField>
        <TextField label="Descripción y referencias verificables" value={reportDescription} onChange={(event) => setReportDescription(event.target.value)} multiline minRows={5} inputProps={{ maxLength: 10000 }} required />
      </Stack></DialogContent>
      <DialogActions><Button onClick={() => setReportOpen(false)} disabled={purchaseBusy}>Cancelar</Button><Button color="warning" variant="contained" disabled={purchaseBusy || !reportDescription.trim()} onClick={() => void reportInfringement()}>Enviar reporte</Button></DialogActions>
    </Dialog>
  </Stack>;
}
