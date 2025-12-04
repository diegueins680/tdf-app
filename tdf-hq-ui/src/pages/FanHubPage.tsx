import { useEffect, useMemo, useRef, useState, type ReactNode, type ChangeEvent } from 'react';
import {
  Alert,
  Avatar,
  Box,
  Button,
  Card,
  CardContent,
  CardMedia,
  CardActionArea,
  Chip,
  CircularProgress,
  Grid,
  IconButton,
  Link,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import FavoriteIcon from '@mui/icons-material/Favorite';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import YouTubeIcon from '@mui/icons-material/YouTube';
import UploadFileIcon from '@mui/icons-material/UploadFile';
import EditIcon from '@mui/icons-material/Edit';
import type { ArtistProfileUpsert, FanProfileUpdate, ArtistReleaseDTO } from '../api/types';
import { Fans } from '../api/fans';
import { useSession } from '../session/SessionContext';
import { Link as RouterLink } from 'react-router-dom';
import { useCmsContent } from '../hooks/useCmsContent';
import StreamingPlayer from '../components/StreamingPlayer';
import { buildReleaseStreamingSources } from '../utils/media';

function StatPill({ label, value }: { label: string; value: number }) {
  return (
    <Box
      sx={{
        px: 2,
        py: 1,
        borderRadius: 2,
        bgcolor: 'rgba(148,163,184,0.14)',
        border: '1px solid rgba(148,163,184,0.3)',
        minWidth: 140,
      }}
    >
      <Typography variant="caption" color="text.secondary">
        {label}
      </Typography>
      <Typography variant="h6" fontWeight={800}>
        {value}
      </Typography>
    </Box>
  );
}

export default function FanHubPage({ focusArtist }: { focusArtist?: boolean }) {
  const { session } = useSession();
  const qc = useQueryClient();
  const viewerId = session?.partyId ?? null;
  const avatarInputRef = useRef<HTMLInputElement | null>(null);
  const isFan = useMemo(() => {
    const roles = session?.roles ?? [];
    return roles.some((role) => {
      const r = role.toLowerCase();
      return r === 'fan' || r === 'customer';
    });
  }, [session?.roles]);
  const canEditArtist = useMemo(
    () => Boolean(
      session?.partyId &&
        (session.roles?.some((r) => {
          const role = r.toLowerCase();
          return role === 'artist' || role === 'artista' || role === 'admin';
        }) ?? false),
    ),
    [session?.partyId, session?.roles],
  );
  interface FanCmsPayload {
    heroTitle?: string;
    heroSubtitle?: string;
  }
  const cmsQuery = useCmsContent('fan-hub', 'es');
  const cmsPayload = useMemo<FanCmsPayload>(() => {
    const payload = cmsQuery.data?.ccdPayload as FanCmsPayload | undefined;
    return payload ?? {};
  }, [cmsQuery.data]);
  const artistSectionRef = useRef<HTMLDivElement | null>(null);

  const artistsQuery = useQuery({
    queryKey: ['fan-artists'],
    queryFn: Fans.listArtists,
  });

  const profileQuery = useQuery({
    queryKey: ['fan-profile', viewerId],
    queryFn: Fans.getProfile,
    enabled: Boolean(viewerId && isFan),
  });

  const followsQuery = useQuery({
    queryKey: ['fan-follows', viewerId],
    queryFn: Fans.listFollows,
    enabled: Boolean(viewerId && isFan),
  });
  const artistProfileQuery = useQuery({
    queryKey: ['artist-profile', viewerId],
    queryFn: Fans.getMyArtistProfile,
    enabled: Boolean(viewerId) && canEditArtist,
  });

  const [profileDraft, setProfileDraft] = useState<FanProfileUpdate>({
    fpuDisplayName: '',
    fpuBio: '',
    fpuCity: '',
    fpuFavoriteGenres: '',
    fpuAvatarUrl: '',
  });
  const [artistDraft, setArtistDraft] = useState<ArtistProfileUpsert>({
    apuArtistId: session?.partyId ?? 0,
    apuDisplayName: '',
    apuSlug: '',
    apuBio: '',
    apuCity: '',
    apuHeroImageUrl: '',
    apuSpotifyArtistId: '',
    apuSpotifyUrl: '',
    apuYoutubeChannelId: '',
    apuYoutubeUrl: '',
    apuWebsiteUrl: '',
    apuFeaturedVideoUrl: '',
    apuGenres: '',
    apuHighlights: '',
  });
  const [heroImageFileName, setHeroImageFileName] = useState<string>('');
  const [heroImageError, setHeroImageError] = useState<string | null>(null);
  const [expandedFeatured, setExpandedFeatured] = useState<Set<number>>(() => new Set());

  useEffect(() => {
    if (profileQuery.data) {
      setProfileDraft({
        fpuDisplayName: profileQuery.data.fpDisplayName ?? '',
        fpuBio: profileQuery.data.fpBio ?? '',
        fpuCity: profileQuery.data.fpCity ?? '',
        fpuFavoriteGenres: profileQuery.data.fpFavoriteGenres ?? '',
        fpuAvatarUrl: profileQuery.data.fpAvatarUrl ?? '',
      });
    }
  }, [profileQuery.data]);
  useEffect(() => {
    if (artistProfileQuery.data && session?.partyId) {
      const dto = artistProfileQuery.data;
      setArtistDraft({
        apuArtistId: session.partyId,
        apuDisplayName: dto.apDisplayName ?? '',
        apuSlug: dto.apSlug ?? '',
        apuBio: dto.apBio ?? '',
        apuCity: dto.apCity ?? '',
        apuHeroImageUrl: dto.apHeroImageUrl ?? '',
        apuSpotifyArtistId: dto.apSpotifyArtistId ?? '',
        apuSpotifyUrl: dto.apSpotifyUrl ?? '',
        apuYoutubeChannelId: dto.apYoutubeChannelId ?? '',
        apuYoutubeUrl: dto.apYoutubeUrl ?? '',
        apuWebsiteUrl: dto.apWebsiteUrl ?? '',
        apuFeaturedVideoUrl: dto.apFeaturedVideoUrl ?? '',
        apuGenres: dto.apGenres ?? '',
        apuHighlights: dto.apHighlights ?? '',
      });
      setHeroImageFileName(dto.apHeroImageUrl ? 'Imagen existente' : '');
      setHeroImageError(null);
    }
  }, [artistProfileQuery.data, session?.partyId]);
  useEffect(() => {
    const partyId = session?.partyId;
    if (!partyId) return;
    setArtistDraft((prev) => ({ ...prev, apuArtistId: partyId }));
  }, [session?.partyId]);

  useEffect(() => {
    if (focusArtist && artistSectionRef.current) {
      artistSectionRef.current.scrollIntoView({ behavior: 'smooth', block: 'start' });
    }
  }, [focusArtist]);

  const updateProfileMutation = useMutation({
    mutationFn: Fans.updateProfile,
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['fan-profile', viewerId] });
    },
  });
  const updateArtistProfileMutation = useMutation({
    mutationFn: Fans.updateMyArtistProfile,
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['artist-profile', viewerId] });
    },
  });

  const followMutation = useMutation({
    mutationFn: (artistId: number) => Fans.follow(artistId),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['fan-follows', viewerId] });
      void qc.invalidateQueries({ queryKey: ['fan-artists'] });
    },
  });

  const unfollowMutation = useMutation({
    mutationFn: (artistId: number) => Fans.unfollow(artistId),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['fan-follows', viewerId] });
      void qc.invalidateQueries({ queryKey: ['fan-artists'] });
    },
  });

  const follows = useMemo(() => followsQuery.data ?? [], [followsQuery.data]);
  const followedArtistIds = useMemo(
    () => follows.map((follow) => follow.ffArtistId).sort((a, b) => a - b),
    [follows],
  );

  type ReleaseFeedItem = ArtistReleaseDTO & { artistName: string };
  const releaseFeedQuery = useQuery({
    queryKey: ['fan-release-feed', followedArtistIds],
    enabled: isFan && follows.length > 0,
    queryFn: async () => {
      const perArtist = await Promise.all(
        follows.map(async (follow) => {
          const releases = await Fans.getReleases(follow.ffArtistId);
          return releases.map((release) => ({
            ...release,
            artistName: follow.ffArtistName,
          }));
        }),
      );
      const parseDate = (value?: string | null) => {
        const ts = value ? Date.parse(value) : Number.NaN;
        return Number.isNaN(ts) ? 0 : ts;
      };
      const flat = perArtist.flat() as ReleaseFeedItem[];
      return flat.sort((a, b) => parseDate(b.arReleaseDate) - parseDate(a.arReleaseDate));
    },
  });

  const releaseFeed = releaseFeedQuery.data ?? [];

  const handleFollowToggle = (artistId: number, currentlyFollowing: boolean) => {
    if (!viewerId) return;
    if (currentlyFollowing) {
      unfollowMutation.mutate(artistId);
    } else {
      followMutation.mutate(artistId);
    }
  };

  const normalizeField = (value?: string | null) => {
    const trimmed = value?.trim();
    return trimmed && trimmed.length > 0 ? trimmed : null;
  };

  const handleSaveProfile = () => {
    updateProfileMutation.mutate(profileDraft);
  };

  const handleAvatarFileChange = (event: ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    if (!file) return;
    const maxBytes = 3 * 1024 * 1024; // 3 MB
    if (file.size > maxBytes) {
      alert('El archivo supera 3 MB. Usa una imagen más liviana.'); // small UX guard; avoid bigger error plumbing here
      return;
    }
    const reader = new FileReader();
    reader.onload = () => {
      if (typeof reader.result === 'string') {
        setProfileDraft((prev) => ({ ...prev, fpuAvatarUrl: reader.result as string }));
      }
    };
    reader.readAsDataURL(file);
  };


  const handleHeroImageFileChange = (file: File | null) => {
    if (!file) {
      setArtistDraft((prev) => ({ ...prev, apuHeroImageUrl: '' }));
      setHeroImageFileName('');
      return;
    }
    const maxBytes = 6 * 1024 * 1024;
    if (file.size > maxBytes) {
      setHeroImageError('El archivo supera 6 MB. Usa una imagen más liviana.');
      return;
    }
    const reader = new FileReader();
    reader.onload = () => {
      if (typeof reader.result === 'string') {
        const dataUrl = reader.result;
        setArtistDraft((prev) => ({ ...prev, apuHeroImageUrl: dataUrl }));
        setHeroImageFileName(file.name);
        setHeroImageError(null);
      } else {
        setHeroImageError('No pudimos leer la imagen seleccionada.');
        setArtistDraft((prev) => ({ ...prev, apuHeroImageUrl: '' }));
      }
    };
    reader.onerror = () => setHeroImageError('No pudimos leer la imagen seleccionada.');
    reader.readAsDataURL(file);
  };

  const handleSaveArtistProfile = () => {
    if (!session?.partyId) return;
    const payload: ArtistProfileUpsert = {
      apuArtistId: session.partyId,
      apuDisplayName: normalizeField(artistDraft.apuDisplayName),
      apuSlug: normalizeField(artistDraft.apuSlug),
      apuBio: normalizeField(artistDraft.apuBio),
      apuCity: normalizeField(artistDraft.apuCity),
      apuHeroImageUrl: normalizeField(artistDraft.apuHeroImageUrl),
      apuSpotifyArtistId: normalizeField(artistDraft.apuSpotifyArtistId),
      apuSpotifyUrl: normalizeField(artistDraft.apuSpotifyUrl),
      apuYoutubeChannelId: normalizeField(artistDraft.apuYoutubeChannelId),
      apuYoutubeUrl: normalizeField(artistDraft.apuYoutubeUrl),
      apuWebsiteUrl: normalizeField(artistDraft.apuWebsiteUrl),
      apuFeaturedVideoUrl: normalizeField(artistDraft.apuFeaturedVideoUrl),
      apuGenres: normalizeField(artistDraft.apuGenres),
      apuHighlights: normalizeField(artistDraft.apuHighlights),
    };
    updateArtistProfileMutation.mutate(payload);
  };

  const artists = useMemo(() => artistsQuery.data ?? [], [artistsQuery.data]);
  const suggestedArtists = useMemo(() => {
    if (!artists.length) return [];
    const followed = new Set(follows.map((f) => f.ffArtistId));
    return artists
      .filter((artist) => !followed.has(artist.apArtistId))
      .sort((a, b) => (b.apFollowerCount ?? 0) - (a.apFollowerCount ?? 0))
      .slice(0, 3);
  }, [artists, follows]);

  const errorMessage =
    artistsQuery.error instanceof Error
      ? artistsQuery.error.message
      : 'No se pudo cargar la información de artistas.';

  const isLoading = artistsQuery.isLoading;
  const hasError = artistsQuery.error;
  const formatReleaseDate = (value?: string | null) => {
    if (!value) return 'Sin fecha';
    const parsed = Date.parse(value);
    if (Number.isNaN(parsed)) return 'Sin fecha';
    return new Intl.DateTimeFormat('es-CO', { month: 'short', day: 'numeric' }).format(new Date(parsed));
  };

  return (
    <Box sx={{ minHeight: '100vh', bgcolor: 'background.default', py: 6, px: { xs: 2, md: 6 } }}>
      <Stack spacing={3} maxWidth="lg" sx={{ mx: 'auto' }}>
        <Stack spacing={1}>
          <Typography variant="h3" fontWeight={700}>
            {cmsPayload?.heroTitle ?? 'Fan Hub — Conecta con tus artistas'}
          </Typography>
          <Typography variant="body1" color="text.secondary">
            {cmsPayload?.heroSubtitle ?? 'Sigue a tus artistas favoritos, recibe lanzamientos y escucha sus playlists oficiales en Spotify y YouTube.'}
          </Typography>
          {!session && (
            <Typography variant="body2">
              ¿Quieres guardar tus artistas?{' '}
              <Link component={RouterLink} to="/login" underline="hover">
                Inicia sesión o crea una cuenta
              </Link>
              .
            </Typography>
          )}
        </Stack>

        <Grid container spacing={2}>
          <Grid item xs={12} md={8}>
            <Card sx={{ p: 3, height: '100%', display: 'flex', flexDirection: 'column', gap: 2 }}>
              <Stack direction="row" justifyContent="space-between" alignItems="center">
                <Typography variant="h6">Novedades de tus artistas</Typography>
                {isFan && <Chip label={`${releaseFeed.length} lanzamientos`} size="small" />}
              </Stack>
              <Typography variant="body2" color="text.secondary">
                Reproduce lanzamientos sin salir del hub: si hay enlaces de Spotify o YouTube los cargamos en el reproductor
                embebido.
              </Typography>
              {!isFan && (
                <Alert severity="info">Inicia sesión con rol Fan/Customer para ver lanzamientos personalizados.</Alert>
              )}
              {isFan && releaseFeedQuery.isLoading && (
                <Box display="flex" justifyContent="center" py={3}>
                  <CircularProgress size={20} />
                </Box>
              )}
              {isFan && !releaseFeedQuery.isLoading && releaseFeed.length === 0 && (
                <Alert severity="info">Sigue al menos un artista para ver drops recientes aquí.</Alert>
              )}
              {isFan && releaseFeed.length > 0 && (
                <Stack spacing={1.5}>
                  {releaseFeed.slice(0, 4).map((release) => {
                    const releaseSources = buildReleaseStreamingSources(release);
                    return (
                      <Box
                        key={`${release.arArtistId}-${release.arReleaseId}`}
                        sx={{
                          p: 2,
                          borderRadius: 2,
                          border: '1px solid',
                          borderColor: 'divider',
                          bgcolor: 'background.paper',
                        }}
                      >
                        <Stack direction="row" justifyContent="space-between" alignItems="center">
                          <Box>
                            <Typography variant="subtitle1" fontWeight={700}>
                              {release.arTitle}
                            </Typography>
                            <Typography variant="body2" color="text.secondary">
                              {release.artistName}
                            </Typography>
                          </Box>
                          <Chip label={formatReleaseDate(release.arReleaseDate)} size="small" />
                        </Stack>
                        {release.arDescription && (
                          <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                            {release.arDescription.length > 140
                              ? `${release.arDescription.slice(0, 140)}…`
                              : release.arDescription}
                          </Typography>
                        )}
                        {releaseSources.length > 0 && (
                          <Box sx={{ mt: 1.5 }}>
                            <StreamingPlayer
                              title={release.arTitle}
                              artist={release.artistName}
                              posterUrl={release.arCoverImageUrl}
                              sources={releaseSources}
                              variant="compact"
                            />
                          </Box>
                        )}
                        <Stack direction="row" spacing={1} sx={{ mt: 1 }}>
                          <Button
                            variant="contained"
                            size="small"
                            component="a"
                            href={release.arSpotifyUrl ?? undefined}
                            target="_blank"
                            rel="noopener noreferrer"
                            disabled={!release.arSpotifyUrl}
                          >
                            Escuchar en Spotify
                          </Button>
                          <Button
                            variant="outlined"
                            size="small"
                            component="a"
                            href={release.arYoutubeUrl ?? undefined}
                            target="_blank"
                            rel="noopener noreferrer"
                            disabled={!release.arYoutubeUrl}
                          >
                            Ver en YouTube
                          </Button>
                        </Stack>
                      </Box>
                    );
                  })}
                </Stack>
              )}
            </Card>
          </Grid>
          <Grid item xs={12} md={4}>
            <Stack spacing={2} height="100%">
              <Card sx={{ p: 3 }}>
                <Stack spacing={1.5}>
                  <Typography variant="h6">Panel rápido</Typography>
                  <Stack direction="row" spacing={1} flexWrap="wrap">
                    <StatPill label="Artistas que sigues" value={follows.length} />
                    {artistProfileQuery.data && (
                      <StatPill label="Fans de tu perfil" value={artistProfileQuery.data.apFollowerCount} />
                    )}
                  </Stack>
                  <Typography variant="body2" color="text.secondary">
                    Actualiza tu bio y enlaces para subir la tasa de follow. ¿Tienes un nuevo clip? Añádelo como video destacado.
                  </Typography>
                  <Button component={RouterLink} to="/records" variant="outlined">
                    Ver Sessions y releases
                  </Button>
                </Stack>
              </Card>
              <Card sx={{ p: 3 }}>
                <Stack spacing={1.5}>
                  <Typography variant="h6">Sugerencias para seguir</Typography>
                  {suggestedArtists.length === 0 && (
                    <Typography variant="body2" color="text.secondary">
                      Ya sigues a todos los artistas activos en el hub.
                    </Typography>
                  )}
                  {suggestedArtists.length > 0 && (
                    <Stack spacing={1.25}>
                      {suggestedArtists.map((artist) => (
                        <Box
                          key={artist.apArtistId}
                          sx={{
                            p: 2,
                            borderRadius: 2,
                            border: '1px solid',
                            borderColor: 'divider',
                            bgcolor: 'background.paper',
                          }}
                        >
                          <Stack direction="row" justifyContent="space-between" alignItems="center" spacing={1.5}>
                            <Stack direction="row" spacing={1.5} alignItems="center">
                              <Avatar src={artist.apHeroImageUrl ?? undefined} alt={artist.apDisplayName} />
                              <Box>
                                <Typography variant="subtitle1" fontWeight={700}>
                                  {artist.apDisplayName}
                                </Typography>
                                {artist.apCity && (
                                  <Typography variant="caption" color="text.secondary">
                                    {artist.apCity}
                                  </Typography>
                                )}
                              </Box>
                            </Stack>
                            <Button
                              variant="contained"
                              size="small"
                              onClick={() => handleFollowToggle(artist.apArtistId, false)}
                              disabled={!viewerId || followMutation.isPending || unfollowMutation.isPending}
                            >
                              Seguir
                            </Button>
                          </Stack>
                          {artist.apGenres && (
                            <Stack direction="row" spacing={0.5} mt={1} flexWrap="wrap">
                              {artist.apGenres.split(',').slice(0, 3).map((genre) => (
                                <Chip key={genre.trim()} label={genre.trim()} size="small" variant="outlined" />
                              ))}
                            </Stack>
                          )}
                        </Box>
                      ))}
                    </Stack>
                  )}
                </Stack>
              </Card>
            </Stack>
          </Grid>
        </Grid>

        {!isFan && session && (
          <Alert severity="info">
            Este usuario no tiene rol de Fan/Customer, por lo que no cargamos el perfil fan. Agrega el rol Fan para evitar errores 403 en esta sección.
          </Alert>
        )}

        {isFan && (
          <ProfileSectionCard
            title="Tu perfil fan"
            description="Personaliza cómo te ve la comunidad cuando sigues a un artista."
            actions={
              <Button
                variant="contained"
                sx={{ alignSelf: 'flex-start' }}
                onClick={handleSaveProfile}
                disabled={updateProfileMutation.isPending}
              >
                {updateProfileMutation.isPending ? 'Guardando…' : 'Guardar perfil'}
              </Button>
            }
          >
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={3} alignItems="flex-start">
              <Box sx={{ position: 'relative', width: 88, height: 88 }}>
                <Avatar
                  src={profileDraft.fpuAvatarUrl ?? undefined}
                  alt={profileDraft.fpuDisplayName ?? session?.displayName ?? ''}
                  sx={{ width: 88, height: 88 }}
                />
                <IconButton
                  size="small"
                  onClick={() => avatarInputRef.current?.click()}
                  sx={{
                    position: 'absolute',
                    bottom: -6,
                    right: -6,
                    bgcolor: 'primary.main',
                    color: '#fff',
                    boxShadow: 2,
                    '&:hover': { bgcolor: 'primary.dark' },
                  }}
                  aria-label="Editar avatar"
                >
                  <EditIcon fontSize="small" />
                </IconButton>
                <input
                  type="file"
                  accept="image/*"
                  ref={avatarInputRef}
                  onChange={handleAvatarFileChange}
                  style={{ display: 'none' }}
                />
              </Box>
              <Stack flex={1} spacing={2}>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                  <TextField
                    label="Nombre público"
                    value={profileDraft.fpuDisplayName ?? ''}
                    onChange={(event) => setProfileDraft((prev) => ({ ...prev, fpuDisplayName: event.target.value }))}
                    fullWidth
                  />
                  <TextField
                    label="Ciudad"
                    value={profileDraft.fpuCity ?? ''}
                    onChange={(event) => setProfileDraft((prev) => ({ ...prev, fpuCity: event.target.value }))}
                    fullWidth
                  />
                </Stack>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                  <TextField
                    label="Géneros favoritos"
                    value={profileDraft.fpuFavoriteGenres ?? ''}
                    onChange={(event) =>
                      setProfileDraft((prev) => ({ ...prev, fpuFavoriteGenres: event.target.value }))
                    }
                    fullWidth
                  />
                  <TextField
                    label="Avatar (URL)"
                    value={profileDraft.fpuAvatarUrl ?? ''}
                    onChange={(event) => setProfileDraft((prev) => ({ ...prev, fpuAvatarUrl: event.target.value }))}
                    fullWidth
                  />
                </Stack>
                <TextField
                  label="Bio"
                  multiline
                  minRows={2}
                  value={profileDraft.fpuBio ?? ''}
                  onChange={(event) => setProfileDraft((prev) => ({ ...prev, fpuBio: event.target.value }))}
                  fullWidth
                />
              </Stack>
            </Stack>
          </ProfileSectionCard>
        )}

        {canEditArtist && (
          <div ref={artistSectionRef}>
            <ProfileSectionCard
              title="Perfil de artista"
              description="Cualquier usuario puede convertirse en artista y publicar su perfil."
              actions={
                <Stack direction="row" spacing={2} alignItems="center">
                  {artistProfileQuery.data && (
                    <Chip label={`${artistProfileQuery.data.apFollowerCount} fans`} color="secondary" />
                  )}
                  <Button
                    variant="contained"
                    onClick={handleSaveArtistProfile}
                    disabled={updateArtistProfileMutation.isPending || !session?.partyId}
                  >
                    {updateArtistProfileMutation.isPending ? 'Guardando…' : 'Actualizar perfil de artista'}
                  </Button>
                </Stack>
              }
            >
              {artistProfileQuery.isLoading && <CircularProgress size={20} />}
              {artistProfileQuery.isError && (
                <Alert severity="error">No pudimos cargar tu perfil de artista.</Alert>
              )}
              <Stack spacing={2}>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                  <TextField
                    label="Nombre artístico"
                    value={artistDraft.apuDisplayName ?? ''}
                    onChange={(event) => setArtistDraft((prev) => ({ ...prev, apuDisplayName: event.target.value }))}
                    fullWidth
                  />
                  <TextField
                    label="Slug público"
                    value={artistDraft.apuSlug ?? ''}
                    onChange={(event) => setArtistDraft((prev) => ({ ...prev, apuSlug: event.target.value }))}
                    fullWidth
                  />
                </Stack>
                <TextField
                  label="Ciudad"
                  value={artistDraft.apuCity ?? ''}
                  onChange={(event) => setArtistDraft((prev) => ({ ...prev, apuCity: event.target.value }))}
                  fullWidth
                />
                <TextField
                  label="Bio"
                  multiline
                  minRows={3}
                  value={artistDraft.apuBio ?? ''}
                  onChange={(event) => setArtistDraft((prev) => ({ ...prev, apuBio: event.target.value }))}
                  fullWidth
                />
                <Stack spacing={1}>
                  <Typography variant="body2" fontWeight={700}>
                    Imagen principal
                  </Typography>
                  <Stack direction={{ xs: 'column', md: 'row' }} spacing={1.5} alignItems="center">
                    <Button
                      component="label"
                      startIcon={<UploadFileIcon />}
                      variant="outlined"
                      sx={{ alignSelf: 'flex-start' }}
                    >
                      Seleccionar imagen
                      <input
                        type="file"
                        accept="image/*"
                        hidden
                        onChange={(e) => handleHeroImageFileChange(e.target.files?.[0] ?? null)}
                      />
                    </Button>
                    {heroImageFileName && (
                      <Typography variant="body2" color="text.secondary">
                        {heroImageFileName}
                      </Typography>
                    )}
                    {artistDraft.apuHeroImageUrl && (
                      <Button
                        variant="text"
                        color="inherit"
                        onClick={() => {
                          setArtistDraft((prev) => ({ ...prev, apuHeroImageUrl: '' }));
                          setHeroImageFileName('');
                        }}
                      >
                        Quitar
                      </Button>
                    )}
                  </Stack>
                  {artistDraft.apuHeroImageUrl && (
                    <Card
                      variant="outlined"
                      sx={{ maxWidth: 420, borderRadius: 2, borderColor: 'divider', overflow: 'hidden' }}
                    >
                      <CardMedia component="img" height="180" image={artistDraft.apuHeroImageUrl} alt="Vista previa" />
                    </Card>
                  )}
                  {heroImageError && <Alert severity="warning">{heroImageError}</Alert>}
                  <Typography variant="caption" color="text.secondary">
                    Se guardará embebida (data URL). Usa imágenes livianas (&lt; 6 MB).
                  </Typography>
                </Stack>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                  <TextField
                    label="Spotify URL"
                    value={artistDraft.apuSpotifyUrl ?? ''}
                    onChange={(event) => setArtistDraft((prev) => ({ ...prev, apuSpotifyUrl: event.target.value }))}
                    fullWidth
                  />
                  <TextField
                    label="Spotify Artist ID"
                    value={artistDraft.apuSpotifyArtistId ?? ''}
                    onChange={(event) =>
                      setArtistDraft((prev) => ({ ...prev, apuSpotifyArtistId: event.target.value }))
                    }
                    fullWidth
                  />
                </Stack>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                  <TextField
                    label="YouTube URL"
                    value={artistDraft.apuYoutubeUrl ?? ''}
                    onChange={(event) => setArtistDraft((prev) => ({ ...prev, apuYoutubeUrl: event.target.value }))}
                    fullWidth
                  />
                  <TextField
                    label="YouTube Channel ID"
                    value={artistDraft.apuYoutubeChannelId ?? ''}
                    onChange={(event) =>
                      setArtistDraft((prev) => ({ ...prev, apuYoutubeChannelId: event.target.value }))
                    }
                    fullWidth
                  />
                </Stack>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                  <TextField
                    label="Sitio web"
                    value={artistDraft.apuWebsiteUrl ?? ''}
                    onChange={(event) => setArtistDraft((prev) => ({ ...prev, apuWebsiteUrl: event.target.value }))}
                    fullWidth
                  />
                  <TextField
                    label="Video destacado"
                    value={artistDraft.apuFeaturedVideoUrl ?? ''}
                    onChange={(event) =>
                      setArtistDraft((prev) => ({ ...prev, apuFeaturedVideoUrl: event.target.value }))
                    }
                    fullWidth
                  />
                </Stack>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                  <TextField
                    label="Géneros (separados por coma)"
                    value={artistDraft.apuGenres ?? ''}
                    onChange={(event) => setArtistDraft((prev) => ({ ...prev, apuGenres: event.target.value }))}
                    fullWidth
                  />
                  <TextField
                    label="Highlights"
                    value={artistDraft.apuHighlights ?? ''}
                    onChange={(event) => setArtistDraft((prev) => ({ ...prev, apuHighlights: event.target.value }))}
                    fullWidth
                  />
                </Stack>
              </Stack>
            </ProfileSectionCard>
          </div>
        )}

        {viewerId && follows.length > 0 && (
          <Card sx={{ p: 3 }}>
            <Typography variant="h6" gutterBottom>Artistas que sigues</Typography>
            <Stack direction="row" spacing={2} flexWrap="wrap">
              {follows.map((follow) => (
                <Chip
                  key={follow.ffArtistId}
                  label={follow.ffArtistName}
                  color="primary"
                  icon={<FavoriteIcon />}
                />
              ))}
            </Stack>
          </Card>
        )}

        {hasError && <Alert severity="error">{errorMessage}</Alert>}
        {isLoading && (
          <Box display="flex" justifyContent="center" py={6}>
            <CircularProgress />
          </Box>
        )}

        {!isLoading && artists.length === 0 && (
          <Alert severity="info">Pronto encontrarás artistas disponibles para seguir.</Alert>
        )}

        <Grid container spacing={3}>
          {artists.map((artist) => {
            const spotifyUrl = artist.apSpotifyUrl ?? (artist.apSpotifyArtistId ? `https://open.spotify.com/artist/${artist.apSpotifyArtistId}` : null);
            const youtubeUrl =
              artist.apFeaturedVideoUrl ??
              artist.apYoutubeUrl ??
              (artist.apYoutubeChannelId ? `https://www.youtube.com/channel/${artist.apYoutubeChannelId}` : null);
            const isFollowing = follows.some((follow) => follow.ffArtistId === artist.apArtistId);
            const spotifyButtonProps = spotifyUrl
              ? { component: 'a', href: spotifyUrl, target: '_blank', rel: 'noopener noreferrer' }
              : {};
            const youtubeButtonProps = youtubeUrl
              ? { component: 'a', href: youtubeUrl, target: '_blank', rel: 'noopener noreferrer' }
              : {};
            const featuredSources = artist.apFeaturedVideoUrl
              ? [
                  {
                    url: artist.apFeaturedVideoUrl,
                    provider: 'youtube' as const,
                    label: 'YouTube',
                    posterUrl: artist.apHeroImageUrl,
                  },
                ]
              : [];
            const isFeaturedOpen = expandedFeatured.has(artist.apArtistId);
            return (
              <Grid item xs={12} md={6} key={artist.apArtistId}>
                <Card sx={{ height: '100%', display: 'flex', flexDirection: 'column', overflow: 'hidden' }}>
                  <CardActionArea
                    onClick={() => {
                      if (artist.apSlug) {
                        window.open(`/artista/${artist.apSlug}`, '_blank');
                      } else {
                        window.open(`/artista/${artist.apArtistId}`, '_blank');
                      }
                    }}
                  >
                    {artist.apHeroImageUrl && (
                      <CardMedia component="img" height="220" image={artist.apHeroImageUrl} alt={artist.apDisplayName} />
                    )}
                  </CardActionArea>
                  <CardContent sx={{ flex: 1, display: 'flex', flexDirection: 'column', gap: 1.5 }}>
                    <Stack
                      direction={{ xs: 'column', md: 'row' }}
                      spacing={2}
                      flexWrap="wrap"
                      alignItems="flex-start"
                      rowGap={2}
                    >
                      <Box flex={1} minWidth={0}>
                        <Stack direction="row" justifyContent="space-between" alignItems="center">
                          <Typography variant="h5">{artist.apDisplayName}</Typography>
                          <Chip label={`${artist.apFollowerCount} fans`} size="small" />
                        </Stack>
                        {artist.apCity && (
                          <Typography variant="body2" color="text.secondary">
                            {artist.apCity}
                          </Typography>
                        )}
                        {artist.apBio && (
                          <Typography variant="body2" color="text.secondary">
                            {artist.apBio.length > 180 ? `${artist.apBio.slice(0, 180)}…` : artist.apBio}
                          </Typography>
                        )}
                        <Stack direction="row" spacing={1} flexWrap="wrap">
                          {artist.apGenres
                            ?.split(',')
                            ?.map((genre) => (
                              <Chip key={genre.trim()} label={genre.trim()} size="small" variant="outlined" />
                            ))}
                        </Stack>
                        <Stack direction="row" spacing={1} mt={2}>
                          <Button
                            {...spotifyButtonProps}
                            variant="contained"
                            size="small"
                            startIcon={<PlayArrowIcon />}
                            disabled={!spotifyUrl}
                          >
                            Spotify
                          </Button>
                          <Button
                            {...youtubeButtonProps}
                            variant="outlined"
                            size="small"
                            startIcon={<YouTubeIcon />}
                            disabled={!youtubeUrl}
                          >
                            YouTube
                          </Button>
                          <Button
                            variant={isFollowing ? 'outlined' : 'contained'}
                            color={isFollowing ? 'inherit' : 'secondary'}
                            size="small"
                            onClick={() => handleFollowToggle(artist.apArtistId, isFollowing)}
                            disabled={!viewerId || followMutation.isPending || unfollowMutation.isPending}
                          >
                            {isFollowing ? 'Siguiendo' : 'Seguir'}
                          </Button>
                        </Stack>
                      </Box>
                      {featuredSources.length > 0 && (
                        <Box
                          sx={{
                            minWidth: { xs: '100%', md: 260 },
                            flexGrow: 1,
                            flexBasis: { xs: '100%', md: 280 },
                            maxWidth: '100%',
                          }}
                        >
                          <Stack spacing={1}>
                            <Button
                              size="small"
                              variant={isFeaturedOpen ? 'outlined' : 'contained'}
                              onClick={() =>
                                setExpandedFeatured((prev) => {
                                  const next = new Set(prev);
                                  if (next.has(artist.apArtistId)) next.delete(artist.apArtistId);
                                  else next.add(artist.apArtistId);
                                  return next;
                                })
                              }
                            >
                              {isFeaturedOpen ? 'Ocultar video' : 'Ver video destacado'}
                            </Button>
                            {isFeaturedOpen && (
                              <StreamingPlayer
                                title={`${artist.apDisplayName} — Destacado`}
                                artist={artist.apDisplayName}
                                posterUrl={artist.apHeroImageUrl}
                                sources={featuredSources}
                                variant="compact"
                              />
                            )}
                          </Stack>
                        </Box>
                      )}
                    </Stack>
                  </CardContent>
                </Card>
              </Grid>
            );
          })}
        </Grid>
      </Stack>
    </Box>
  );
}
function ProfileSectionCard({
  title,
  description,
  actions,
  children,
}: {
  title: string;
  description?: string;
  actions?: ReactNode;
  children: ReactNode;
}) {
  return (
    <Card sx={{ p: 3 }}>
      <Stack spacing={2}>
        <Box>
          <Typography variant="h6">{title}</Typography>
          {description && (
            <Typography variant="body2" color="text.secondary">
              {description}
            </Typography>
          )}
        </Box>
        {children}
        {actions}
      </Stack>
    </Card>
  );
}
