import { useEffect, useMemo, useState, type ReactNode } from 'react';
import {
  Alert,
  Avatar,
  Box,
  Button,
  Card,
  CardContent,
  CardMedia,
  Chip,
  CircularProgress,
  Grid,
  Link,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import FavoriteIcon from '@mui/icons-material/Favorite';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import YouTubeIcon from '@mui/icons-material/YouTube';
import type { ArtistProfileUpsert, FanProfileUpdate } from '../api/types';
import { Fans } from '../api/fans';
import { useSession } from '../session/SessionContext';
import { Link as RouterLink } from 'react-router-dom';

export default function FanHubPage() {
  const { session } = useSession();
  const qc = useQueryClient();
  const viewerId = session?.partyId ?? null;
  const isFan = useMemo(
    () => Boolean(session?.roles?.some((role) => role === 'fan' || role === 'customer')),
    [session?.roles],
  );
  const canEditArtist = useMemo(() => Boolean(session?.partyId), [session?.partyId]);

  const artistsQuery = useQuery({
    queryKey: ['fan-artists'],
    queryFn: Fans.listArtists,
  });

  const profileQuery = useQuery({
    queryKey: ['fan-profile', viewerId],
    queryFn: Fans.getProfile,
    enabled: Boolean(viewerId) && isFan,
  });

  const followsQuery = useQuery({
    queryKey: ['fan-follows', viewerId],
    queryFn: Fans.listFollows,
    enabled: Boolean(viewerId) && isFan,
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
    }
  }, [artistProfileQuery.data, session?.partyId]);
  useEffect(() => {
    const partyId = session?.partyId;
    if (!partyId) return;
    setArtistDraft((prev) => ({ ...prev, apuArtistId: partyId }));
  }, [session?.partyId]);

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

  const follows = followsQuery.data ?? [];

  const handleFollowToggle = (artistId: number, currentlyFollowing: boolean) => {
    if (!isFan) return;
    if (currentlyFollowing) {
      unfollowMutation.mutate(artistId);
    } else {
      followMutation.mutate(artistId);
    }
  };

  const handleSaveProfile = () => {
    updateProfileMutation.mutate(profileDraft);
  };
  const normalizeField = (value?: string | null) => {
    const trimmed = value?.trim();
    return trimmed && trimmed.length > 0 ? trimmed : null;
  };

  const handleSaveArtistProfile = () => {
    if (!session?.partyId) return;
    const payload: ArtistProfileUpsert = {
      apuArtistId: session.partyId,
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

  const artists = artistsQuery.data ?? [];
  const errorMessage =
    artistsQuery.error instanceof Error
      ? artistsQuery.error.message
      : 'No se pudo cargar la información de artistas.';

  const isLoading = artistsQuery.isLoading;
  const hasError = artistsQuery.error;

  return (
    <Box sx={{ minHeight: '100vh', bgcolor: 'background.default', py: 6, px: { xs: 2, md: 6 } }}>
      <Stack spacing={3} maxWidth="lg" sx={{ mx: 'auto' }}>
        <Stack spacing={1}>
          <Typography variant="h3" fontWeight={700}>
            Fan Hub — Conecta con tus artistas
          </Typography>
          <Typography variant="body1" color="text.secondary">
            Sigue a tus artistas favoritos, recibe lanzamientos y escucha sus playlists oficiales en Spotify y YouTube.
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
              <Avatar
                src={profileDraft.fpuAvatarUrl ?? undefined}
                alt={profileDraft.fpuDisplayName ?? session?.displayName ?? ''}
                sx={{ width: 80, height: 80 }}
              />
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
                  label="Slug público"
                  value={artistDraft.apuSlug ?? ''}
                  onChange={(event) => setArtistDraft((prev) => ({ ...prev, apuSlug: event.target.value }))}
                  fullWidth
                />
                <TextField
                  label="Ciudad"
                  value={artistDraft.apuCity ?? ''}
                  onChange={(event) => setArtistDraft((prev) => ({ ...prev, apuCity: event.target.value }))}
                  fullWidth
                />
              </Stack>
              <TextField
                label="Bio"
                multiline
                minRows={3}
                value={artistDraft.apuBio ?? ''}
                onChange={(event) => setArtistDraft((prev) => ({ ...prev, apuBio: event.target.value }))}
                fullWidth
              />
              <TextField
                label="Imagen principal (URL)"
                value={artistDraft.apuHeroImageUrl ?? ''}
                onChange={(event) => setArtistDraft((prev) => ({ ...prev, apuHeroImageUrl: event.target.value }))}
                fullWidth
              />
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
        )}

        {isFan && follows.length > 0 && (
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
            const youtubeUrl = artist.apYoutubeUrl ?? (artist.apYoutubeChannelId ? `https://www.youtube.com/channel/${artist.apYoutubeChannelId}` : null);
            const isFollowing = follows.some((follow) => follow.ffArtistId === artist.apArtistId);
            const spotifyButtonProps = spotifyUrl
              ? { component: 'a', href: spotifyUrl, target: '_blank', rel: 'noopener noreferrer' }
              : {};
            const youtubeButtonProps = youtubeUrl
              ? { component: 'a', href: youtubeUrl, target: '_blank', rel: 'noopener noreferrer' }
              : {};
            return (
              <Grid item xs={12} md={6} key={artist.apArtistId}>
                <Card sx={{ height: '100%', display: 'flex', flexDirection: 'column' }}>
                  {artist.apHeroImageUrl && (
                    <CardMedia component="img" height="220" image={artist.apHeroImageUrl} alt={artist.apDisplayName} />
                  )}
                  <CardContent sx={{ flex: 1, display: 'flex', flexDirection: 'column', gap: 1.5 }}>
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
                      {isFan && (
                        <Button
                          variant={isFollowing ? 'outlined' : 'contained'}
                          color={isFollowing ? 'inherit' : 'secondary'}
                          size="small"
                          onClick={() => handleFollowToggle(artist.apArtistId, isFollowing)}
                          disabled={followMutation.isPending || unfollowMutation.isPending}
                        >
                          {isFollowing ? 'Siguiendo' : 'Seguir'}
                        </Button>
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
