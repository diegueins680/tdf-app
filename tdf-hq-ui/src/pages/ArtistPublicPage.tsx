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
import GroupsIcon from '@mui/icons-material/Groups';
import LaunchIcon from '@mui/icons-material/Launch';
import MusicNoteIcon from '@mui/icons-material/MusicNote';
import { Link as RouterLink, useParams } from 'react-router-dom';
import { Fans } from '../api/fans';
import type { ArtistReleaseDTO } from '../api/types';
import { useSession } from '../session/SessionContext';
import { getArtistHeroImage } from '../utils/artistFallbacks';
import ArtistFansList from '../components/ArtistFansList';
import LazyPaginatedList from '../components/LazyPaginatedList';
import { formatDateForUser } from '../utils/formatters';

interface ReleaseCardProps {
  release: ArtistReleaseDTO;
}

type ArtistPublicPageDisplayContract = Readonly<{
  releaseDescriptionPreviewChars: number;
}>;

// Invariant: public release cards show a bounded description preview so the
// card grid remains comparable while preserving the original release content.
const ARTIST_PUBLIC_PAGE_DISPLAY_CONTRACTS = {
  releaseDescriptionPreviewChars: 100 + 4 * 10,
} as const satisfies ArtistPublicPageDisplayContract;

const parseJsonObject = (raw?: string | null): Record<string, unknown> => {
  if (!raw) return {};
  try {
    const value: unknown = JSON.parse(raw);
    return value && typeof value === 'object' && !Array.isArray(value) ? value as Record<string, unknown> : {};
  } catch {
    return {};
  }
};

const parseOfficialLinks = (raw?: string | null) => Object.entries(parseJsonObject(raw))
  .filter((entry): entry is [string, string] => typeof entry[1] === 'string' && /^https?:\/\//i.test(entry[1]));

const parseDiscography = (raw?: string | null) => {
  if (!raw) return [];
  try {
    const value: unknown = JSON.parse(raw);
    if (Array.isArray(value)) {
      return value.flatMap((item) => {
        if (typeof item === 'string') return [{ title: item, detail: '' }];
        if (!item || typeof item !== 'object') return [];
        const record = item as Record<string, unknown>;
        if (typeof record['title'] !== 'string') return [];
        const detail = [record['type'], record['firstReleaseDate']]
          .filter((part) => typeof part === 'string')
          .join(' · ');
        return [{ title: record['title'], detail }];
      });
    }
  } catch {
    // Legacy profiles may contain plain text rather than structured releases.
  }
  return [{ title: raw, detail: '' }];
};

const responsiveSourceSet = (raw: string | null | undefined, format: 'avif' | 'webp') => {
  const values = parseJsonObject(raw)[format];
  if (!Array.isArray(values)) return null;
  const sourceSet = values.flatMap((value) => {
    if (!value || typeof value !== 'object') return [];
    const record = value as Record<string, unknown>;
    return typeof record['url'] === 'string' && typeof record['width'] === 'number'
      ? [`${record['url']} ${record['width']}w`]
      : [];
  }).join(', ');
  return sourceSet || null;
};

function ReleaseCard({ release }: ReleaseCardProps) {
  const releaseDate = release.arReleaseDate
    ? formatDateForUser(release.arReleaseDate, {
        year: 'numeric',
        month: 'short',
        day: 'numeric',
      })
    : null;
  const descriptionMaxLength = ARTIST_PUBLIC_PAGE_DISPLAY_CONTRACTS.releaseDescriptionPreviewChars;
  const description = release.arDescription && release.arDescription.length > descriptionMaxLength
    ? `${release.arDescription.slice(0, descriptionMaxLength)}…`
    : release.arDescription;

  return (
    <Card variant="outlined" sx={{ borderRadius: 3, height: '100%' }}>
      {release.arCoverImageUrl && (
        <CardMedia component="img" height="180" image={release.arCoverImageUrl} alt={release.arTitle} loading="lazy" />
      )}
      <CardContent>
        <Stack spacing={1}>
          <Typography fontWeight={800}>{release.arTitle}</Typography>
          {releaseDate && (
            <Typography variant="caption" color="text.secondary">
              {releaseDate}
            </Typography>
          )}
          {description && (
            <Typography variant="body2" color="text.secondary">
              {description}
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
  );
}

export default function ArtistPublicPage() {
  const { slugOrId } = useParams();
  const qc = useQueryClient();
  const { session } = useSession();
  const viewerId = session?.partyId ?? null;
  const hasToken = Boolean(session);

  const segment = (slugOrId ?? '').trim();

  const artistQuery = useQuery({
    queryKey: ['public-artist', segment],
    queryFn: () => Fans.getPublicArtist(segment),
    enabled: Boolean(segment),
    retry: false,
  });

  const artistId = artistQuery.data?.apArtistId ?? null;

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
      void qc.invalidateQueries({ queryKey: ['public-artist', segment] });
    },
  });

  const artist = artistQuery.data ?? null;
  const releases = releasesQuery.data ?? [];

  const profileLink = useMemo(() => {
    if (!artist) return null;
    if (artist.apSlug) return `/a/${artist.apSlug}`;
    return `/a/${artist.apArtistId}`;
  }, [artist]);

  if (!segment) {
    return (
      <Box py={4}>
        <Alert severity="warning">Link inválido.</Alert>
      </Box>
    );
  }

  if (artistQuery.isError) {
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

  const heroImage = getArtistHeroImage(artist.apHeroLandscapeUrl ?? artist.apHeroImageUrl, artist.apSlug);
  const avatarImage = artist.apHeroSquareUrl ?? heroImage;
  const spotifyUrl =
    artist.apSpotifyUrl ?? (artist.apSpotifyArtistId ? `https://open.spotify.com/artist/${artist.apSpotifyArtistId}` : null);
  const youtubeUrl =
    artist.apYoutubeUrl ??
    (artist.apYoutubeChannelId ? `https://www.youtube.com/channel/${artist.apYoutubeChannelId}` : null);
  const officialLinks = parseOfficialLinks(artist.apSocialLinks);
  const discography = parseDiscography(artist.apDiscography);
  const avifSourceSet = responsiveSourceSet(artist.apHeroResponsiveUrls, 'avif');
  const webpSourceSet = responsiveSourceSet(artist.apHeroResponsiveUrls, 'webp');

  const canClaim = artist.apHasUserAccount === false;
  const isSelf = viewerId === artist.apArtistId;

  return (
    <Box sx={{ maxWidth: 1040, mx: 'auto' }}>
      <Card sx={{ borderRadius: 4, overflow: 'hidden' }} variant="outlined">
        <Box
          sx={{
            position: 'relative',
            minHeight: { xs: 220, md: 280 },
            bgcolor: '#0b1224',
          }}
        >
          {heroImage && (
            <Box component="picture" sx={{ position: 'absolute', inset: 0 }}>
              {avifSourceSet && <source type="image/avif" srcSet={avifSourceSet} sizes="(max-width: 1040px) 100vw, 1040px" />}
              {webpSourceSet && <source type="image/webp" srcSet={webpSourceSet} sizes="(max-width: 1040px) 100vw, 1040px" />}
              <Box
                component="img"
                src={heroImage}
                alt=""
                aria-hidden="true"
                sx={{ width: '100%', height: '100%', objectFit: 'cover', objectPosition: artist.apHeroFocalPoint ?? 'center' }}
              />
            </Box>
          )}
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
              <Avatar
                src={avatarImage ?? undefined}
                alt={artist.apDisplayName}
                sx={{ width: 64, height: 64, bgcolor: 'rgba(59,130,246,0.35)', border: '1px solid rgba(148,163,184,0.35)' }}
              >
                {artist.apDisplayName?.[0]?.toUpperCase() ?? <MusicNoteIcon />}
              </Avatar>
              <Box sx={{ flex: 1, minWidth: 0 }}>
                <Typography variant="h4" fontWeight={900} sx={{ lineHeight: 1.1 }}>
                  {artist.apDisplayName}
                </Typography>
                <Stack direction="row" spacing={1} flexWrap="wrap" sx={{ mt: 1 }}>
                  {[artist.apCity, artist.apCountry].some((value) => Boolean(value)) && (
                    <Chip
                      size="small"
                      label={[artist.apCity, artist.apCountry].filter(Boolean).join(', ')}
                      sx={{ bgcolor: 'rgba(148,163,184,0.16)', color: '#e2e8f0' }}
                    />
                  )}
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
                    tabIndex={0}
                    onClick={(event) => {
                      event.currentTarget.focus();
                      followMutation.mutate();
                    }}
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
                <Button
                  variant="outlined"
                  color="primary"
                  component={RouterLink}
                  to={`/fans/clubs/${artist.apArtistId}`}
                  startIcon={<GroupsIcon />}
                  sx={{ textTransform: 'none' }}
                >
                  Club de Fans
                </Button>
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
                  {artist.apInstagramUrl && (
                    <Link href={artist.apInstagramUrl} target="_blank" rel="noopener noreferrer" underline="hover">
                      Instagram <LaunchIcon fontSize="inherit" />
                    </Link>
                  )}
                  {officialLinks.map(([label, url]) => (
                    <Link key={`${label}:${url}`} href={url} target="_blank" rel="noopener noreferrer" underline="hover">
                      {label === 'bandcamp' ? 'Bandcamp' : label} <LaunchIcon fontSize="inherit" />
                    </Link>
                  ))}
                  {!spotifyUrl && !youtubeUrl && !artist.apWebsiteUrl && !artist.apInstagramUrl && officialLinks.length === 0 && (
                    <Typography variant="body2" color="text.secondary">
                      Sin links todavía.
                    </Typography>
                  )}
                </Stack>
              </Box>
            </Stack>

            {[artist.apHighlights, artist.apAchievements].some((value) => Boolean(value)) && (
              <>
                <Divider />
                <Box sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr', md: '1fr 1fr' }, gap: 2.5 }}>
                  {artist.apHighlights && (
                    <Box>
                      <Typography variant="h6" fontWeight={800}>Highlights</Typography>
                      <Typography variant="body2" color="text.secondary" sx={{ whiteSpace: 'pre-wrap', mt: 1 }}>
                        {artist.apHighlights}
                      </Typography>
                    </Box>
                  )}
                  {artist.apAchievements && (
                    <Box>
                      <Typography variant="h6" fontWeight={800}>Logros</Typography>
                      <Typography variant="body2" color="text.secondary" sx={{ whiteSpace: 'pre-wrap', mt: 1 }}>
                        {artist.apAchievements}
                      </Typography>
                    </Box>
                  )}
                </Box>
              </>
            )}

            {discography.length > 0 && (
              <>
                <Divider />
                <Box>
                  <Typography variant="h6" fontWeight={800}>Discografía destacada</Typography>
                  <Stack component="ul" spacing={0.75} sx={{ pl: 2.5, mt: 1, mb: 0 }}>
                    {discography.map((release, index) => (
                      <Typography component="li" variant="body2" color="text.secondary" key={`${release.title}:${index}`}>
                        <Box component="span" sx={{ color: 'text.primary', fontWeight: 600 }}>{release.title}</Box>
                        {release.detail ? ` · ${release.detail}` : ''}
                      </Typography>
                    ))}
                  </Stack>
                </Box>
              </>
            )}

            {artist.apFollowerCount > 0 && (
              <>
                <Divider />
                <Box>
                  <Typography variant="h6" fontWeight={800} gutterBottom>
                    Fans ({artist.apFollowerCount})
                  </Typography>
                  <ArtistFansList artistId={artist.apArtistId} />
                </Box>
              </>
            )}

            {artist.apSpotifyArtistId && (
              <>
                <Divider />
                <Box>
                  <Typography variant="h6" fontWeight={800} gutterBottom>
                    Escucha en Spotify
                  </Typography>
                  <Box
                    component="iframe"
                    src={`https://open.spotify.com/embed/artist/${artist.apSpotifyArtistId}?utm_source=generator`}
                    width="100%"
                    height="352"
                    frameBorder="0"
                    allowFullScreen
                    allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture"
                    loading="lazy"
                    sx={{ borderRadius: 3, display: 'block' }}
                  />
                </Box>
              </>
            )}

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

              {releases.length > 0 && (
                <LazyPaginatedList
                  items={releases}
                  loading={releasesQuery.isFetching}
                  pagination={{ itemLabel: 'releases', initialRowsPerPage: 6 }}
                  renderItems={(visibleReleases) => (
                    <Grid container spacing={2} sx={{ mt: 0.5 }}>
                      {visibleReleases.map((release) => (
                        <Grid key={release.arReleaseId} item xs={12} sm={6} md={4}>
                          <ReleaseCard release={release} />
                        </Grid>
                      ))}
                    </Grid>
                  )}
                />
              )}
            </Box>
          </Stack>
        </CardContent>
      </Card>
    </Box>
  );
}
