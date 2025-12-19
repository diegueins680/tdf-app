import { useMemo } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
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
  Divider,
  Grid,
  Link,
  Stack,
  Typography,
} from '@mui/material';
import FavoriteIcon from '@mui/icons-material/Favorite';
import FavoriteBorderIcon from '@mui/icons-material/FavoriteBorder';
import LaunchIcon from '@mui/icons-material/Launch';
import MusicNoteIcon from '@mui/icons-material/MusicNote';
import { Link as RouterLink, useParams } from 'react-router-dom';
import { Fans } from '../api/fans';
import { useSession } from '../session/SessionContext';

const isNumericSegment = (value: string) => /^\d+$/.test(value);

export default function ArtistPublicPage() {
  const { slugOrId } = useParams();
  const qc = useQueryClient();
  const { session } = useSession();
  const viewerId = session?.partyId ?? null;
  const hasToken = Boolean(session?.apiToken);

  const segment = (slugOrId ?? '').trim();
  const numericId = useMemo(() => (isNumericSegment(segment) ? Number.parseInt(segment, 10) : null), [segment]);

  const artistsQuery = useQuery({
    queryKey: ['fan-artists'],
    queryFn: Fans.listArtists,
    staleTime: 5 * 60 * 1000,
  });

  const artistFromList = useMemo(() => {
    const artists = artistsQuery.data ?? [];
    if (numericId) {
      return artists.find((artist) => artist.apArtistId === numericId) ?? null;
    }
    const needle = segment.toLowerCase();
    return artists.find((artist) => (artist.apSlug ?? '').toLowerCase() === needle) ?? null;
  }, [artistsQuery.data, numericId, segment]);

  const artistId = numericId ?? artistFromList?.apArtistId ?? null;

  const artistQuery = useQuery({
    queryKey: ['public-artist', artistId],
    queryFn: () => Fans.getArtist(artistId!),
    enabled: Boolean(artistId),
    retry: false,
  });

  const releasesQuery = useQuery({
    queryKey: ['public-artist-releases', artistId],
    queryFn: () => Fans.getReleases(artistId!),
    enabled: Boolean(artistId),
    retry: false,
  });

  const followsQuery = useQuery({
    queryKey: ['fan-follows', viewerId],
    queryFn: Fans.listFollows,
    enabled: Boolean(viewerId && hasToken),
  });

  const isFollowing = useMemo(() => {
    if (!artistId) return false;
    return followsQuery.data?.some((follow) => follow.ffArtistId === artistId) ?? false;
  }, [artistId, followsQuery.data]);

  const followMutation = useMutation({
    mutationFn: async () => {
      if (!artistId) return;
      if (isFollowing) {
        await Fans.unfollow(artistId);
      } else {
        await Fans.follow(artistId);
      }
    },
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['fan-follows', viewerId] });
      void qc.invalidateQueries({ queryKey: ['fan-artists'] });
    },
  });

  const artist = artistQuery.data ?? artistFromList;
  const releases = releasesQuery.data ?? [];

  const profileLink = useMemo(() => {
    if (!artist) return null;
    if (artist.apSlug) return `/artista/${artist.apSlug}`;
    return `/artista/${artist.apArtistId}`;
  }, [artist]);

  if (!segment) {
    return (
      <Box py={4}>
        <Alert severity="warning">Link inválido.</Alert>
      </Box>
    );
  }

  if (!numericId && artistsQuery.isLoading) {
    return (
      <Box py={4} display="flex" alignItems="center" gap={1.5}>
        <CircularProgress size={20} />
        <Typography>Cargando artista...</Typography>
      </Box>
    );
  }

  if (!artistId && !artistsQuery.isLoading) {
    return (
      <Box py={4}>
        <Alert
          severity="info"
          action={
            <Button component={RouterLink} to="/fans" color="inherit" size="small">
              Ver artistas
            </Button>
          }
        >
          No encontramos este artista.
        </Alert>
      </Box>
    );
  }

  if (artistQuery.isLoading && !artist) {
    return (
      <Box py={4} display="flex" alignItems="center" gap={1.5}>
        <CircularProgress size={20} />
        <Typography>Cargando perfil...</Typography>
      </Box>
    );
  }

  if (!artist) {
    return (
      <Box py={4}>
        <Alert severity="warning">No pudimos cargar este perfil.</Alert>
      </Box>
    );
  }

  const heroImage = artist.apHeroImageUrl ?? null;
  const spotifyUrl =
    artist.apSpotifyUrl ?? (artist.apSpotifyArtistId ? `https://open.spotify.com/artist/${artist.apSpotifyArtistId}` : null);
  const youtubeUrl =
    artist.apYoutubeUrl ??
    (artist.apYoutubeChannelId ? `https://www.youtube.com/channel/${artist.apYoutubeChannelId}` : null);

  const canClaim = artist.apHasUserAccount === false;
  const isSelf = viewerId === artist.apArtistId;

  return (
    <Box sx={{ maxWidth: 1040, mx: 'auto' }}>
      <Card sx={{ borderRadius: 4, overflow: 'hidden' }} variant="outlined">
        <Box
          sx={{
            position: 'relative',
            minHeight: { xs: 220, md: 280 },
            bgcolor: heroImage ? 'transparent' : '#0b1224',
            backgroundImage: heroImage ? `url(${heroImage})` : undefined,
            backgroundSize: 'cover',
            backgroundPosition: 'center',
          }}
        >
          <Box
            sx={{
              position: 'absolute',
              inset: 0,
              background:
                'linear-gradient(135deg, rgba(2,6,23,0.78) 0%, rgba(2,6,23,0.45) 45%, rgba(2,6,23,0.78) 100%)',
            }}
          />
          <Stack
            spacing={1.5}
            sx={{
              position: 'relative',
              p: { xs: 2.5, md: 4 },
              height: '100%',
              justifyContent: 'flex-end',
              color: '#e2e8f0',
            }}
          >
            <Stack direction="row" spacing={2} alignItems="center" flexWrap="wrap">
              <Avatar sx={{ width: 64, height: 64, bgcolor: 'rgba(59,130,246,0.35)', border: '1px solid rgba(148,163,184,0.35)' }}>
                {artist.apDisplayName?.[0]?.toUpperCase() ?? <MusicNoteIcon />}
              </Avatar>
              <Box sx={{ flex: 1, minWidth: 0 }}>
                <Typography variant="h4" fontWeight={900} sx={{ lineHeight: 1.1 }}>
                  {artist.apDisplayName}
                </Typography>
                <Stack direction="row" spacing={1} flexWrap="wrap" sx={{ mt: 1 }}>
                  {artist.apCity && <Chip size="small" label={artist.apCity} sx={{ bgcolor: 'rgba(148,163,184,0.16)', color: '#e2e8f0' }} />}
                  <Chip size="small" label={`${artist.apFollowerCount ?? 0} fans`} sx={{ bgcolor: 'rgba(148,163,184,0.16)', color: '#e2e8f0' }} />
                  {artist.apGenres && (
                    <Chip
                      size="small"
                      label={artist.apGenres}
                      sx={{ bgcolor: 'rgba(148,163,184,0.16)', color: '#e2e8f0' }}
                    />
                  )}
                </Stack>
              </Box>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'stretch', sm: 'center' }}>
                {!hasToken ? (
                  <Button
                    variant="contained"
                    color="secondary"
                    component={RouterLink}
                    to={`/login?${new URLSearchParams({ redirect: profileLink ?? '/fans' }).toString()}`}
                    startIcon={<FavoriteBorderIcon />}
                    sx={{ textTransform: 'none' }}
                  >
                    Inicia sesión para seguir
                  </Button>
                ) : (
                  <Button
                    variant={isFollowing ? 'outlined' : 'contained'}
                    color="secondary"
                    onClick={() => followMutation.mutate()}
                    startIcon={isFollowing ? <FavoriteIcon /> : <FavoriteBorderIcon />}
                    disabled={followMutation.isPending}
                    sx={{ textTransform: 'none' }}
                  >
                    {isFollowing ? 'Siguiendo' : 'Seguir'}
                  </Button>
                )}
                {isSelf && (
                  <Button variant="outlined" component={RouterLink} to="/mi-artista" startIcon={<LaunchIcon />} sx={{ textTransform: 'none' }}>
                    Editar perfil
                  </Button>
                )}
              </Stack>
            </Stack>
          </Stack>
        </Box>
        <CardContent sx={{ p: { xs: 2.5, md: 4 } }}>
          <Stack spacing={2.5}>
            {canClaim && (
              <Alert
                severity="info"
                action={
                  <Button
                    component={RouterLink}
                    to={`/artista/crear?${new URLSearchParams({ claimArtistId: String(artist.apArtistId) }).toString()}`}
                    color="inherit"
                    size="small"
                  >
                    Reclamar este perfil
                  </Button>
                }
              >
                Este perfil aún no está reclamado. Si eres el artista, crea tu cuenta y reclámalo.
              </Alert>
            )}

            <Stack direction={{ xs: 'column', md: 'row' }} spacing={2.5} alignItems="flex-start">
              <Box sx={{ flex: 1, minWidth: 0 }}>
                <Typography variant="h6" fontWeight={800}>
                  Bio
                </Typography>
                <Typography variant="body2" color="text.secondary" sx={{ whiteSpace: 'pre-wrap', mt: 1 }}>
                  {artist.apBio ?? 'Este artista aún no ha agregado una bio.'}
                </Typography>
              </Box>
              <Box sx={{ width: { xs: '100%', md: 340 } }}>
                <Typography variant="h6" fontWeight={800}>
                  Links
                </Typography>
                <Stack spacing={1} sx={{ mt: 1 }}>
                  {spotifyUrl && (
                    <Link href={spotifyUrl} target="_blank" rel="noopener noreferrer" underline="hover">
                      Spotify <LaunchIcon fontSize="inherit" />
                    </Link>
                  )}
                  {youtubeUrl && (
                    <Link href={youtubeUrl} target="_blank" rel="noopener noreferrer" underline="hover">
                      YouTube <LaunchIcon fontSize="inherit" />
                    </Link>
                  )}
                  {artist.apWebsiteUrl && (
                    <Link href={artist.apWebsiteUrl} target="_blank" rel="noopener noreferrer" underline="hover">
                      Sitio web <LaunchIcon fontSize="inherit" />
                    </Link>
                  )}
                  {!spotifyUrl && !youtubeUrl && !artist.apWebsiteUrl && (
                    <Typography variant="body2" color="text.secondary">
                      Sin links todavía.
                    </Typography>
                  )}
                </Stack>
              </Box>
            </Stack>

            <Divider />

            <Box>
              <Stack direction="row" justifyContent="space-between" alignItems="center" flexWrap="wrap" gap={1}>
                <Typography variant="h6" fontWeight={800}>
                  Releases
                </Typography>
                {profileLink && (
                  <Button
                    size="small"
                    variant="text"
                    component={RouterLink}
                    to={profileLink}
                    startIcon={<LaunchIcon />}
                    sx={{ textTransform: 'none' }}
                  >
                    Link público
                  </Button>
                )}
              </Stack>

              {releasesQuery.isLoading && (
                <Box display="flex" alignItems="center" gap={1.5} py={2}>
                  <CircularProgress size={18} />
                  <Typography variant="body2" color="text.secondary">
                    Cargando releases...
                  </Typography>
                </Box>
              )}

              {!releasesQuery.isLoading && releases.length === 0 && (
                <Typography variant="body2" color="text.secondary" sx={{ mt: 1 }}>
                  No hay releases publicados todavía.
                </Typography>
              )}

              <Grid container spacing={2} sx={{ mt: 0.5 }}>
                {releases.map((release) => (
                  <Grid key={release.arReleaseId} item xs={12} sm={6} md={4}>
                    <Card variant="outlined" sx={{ borderRadius: 3, height: '100%' }}>
                      {release.arCoverImageUrl && (
                        <CardMedia component="img" height="180" image={release.arCoverImageUrl} alt={release.arTitle} loading="lazy" />
                      )}
                      <CardContent>
                        <Stack spacing={1}>
                          <Typography fontWeight={800}>{release.arTitle}</Typography>
                          {release.arReleaseDate && (
                            <Typography variant="caption" color="text.secondary">
                              {new Date(release.arReleaseDate).toLocaleDateString('es-EC', { year: 'numeric', month: 'short', day: 'numeric' })}
                            </Typography>
                          )}
                          {release.arDescription && (
                            <Typography variant="body2" color="text.secondary">
                              {release.arDescription.length > 140 ? `${release.arDescription.slice(0, 140)}…` : release.arDescription}
                            </Typography>
                          )}
                          <Stack direction="row" spacing={1} flexWrap="wrap">
                            {release.arSpotifyUrl && (
                              <Button size="small" component="a" href={release.arSpotifyUrl} target="_blank" rel="noopener noreferrer">
                                Spotify
                              </Button>
                            )}
                            {release.arYoutubeUrl && (
                              <Button size="small" component="a" href={release.arYoutubeUrl} target="_blank" rel="noopener noreferrer">
                                YouTube
                              </Button>
                            )}
                          </Stack>
                        </Stack>
                      </CardContent>
                    </Card>
                  </Grid>
                ))}
              </Grid>
            </Box>
          </Stack>
        </CardContent>
      </Card>
    </Box>
  );
}

