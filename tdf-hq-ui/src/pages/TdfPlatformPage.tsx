import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Container,
  Divider,
  Grid,
  Stack,
  Typography,
} from '@mui/material';
import AccountCircleIcon from '@mui/icons-material/AccountCircle';
import AutoAwesomeIcon from '@mui/icons-material/AutoAwesome';
import EventAvailableIcon from '@mui/icons-material/EventAvailable';
import GroupsIcon from '@mui/icons-material/Groups';
import MicExternalOnIcon from '@mui/icons-material/MicExternalOn';
import MusicNoteIcon from '@mui/icons-material/MusicNote';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import SchoolIcon from '@mui/icons-material/School';
import StorefrontIcon from '@mui/icons-material/Storefront';
import { useQuery } from '@tanstack/react-query';
import { type ReactNode, useMemo } from 'react';
import { useTranslation } from 'react-i18next';
import { Link as RouterLink } from 'react-router-dom';

import { Fans } from '../api/fans';
import type { ArtistProfileDTO } from '../api/types';
import { STUDIO_MAP_URL } from '../config/appConfig';
import { recordings, releases } from '../constants/recordsContent';
import { getArtistHeroImage } from '../utils/artistFallbacks';

const GENERAL_SIGNUP_PATH = '/login?signup=1&redirect=/fans';
const FAN_SIGNUP_PATH = '/login?signup=1&roles=Fan&redirect=/fans';
const ARTIST_SIGNUP_PATH = '/login?signup=1&intent=artist&redirect=/mi-artista';
const HERO_VIDEO_SRC = '/videos/music-hero.mp4';

type ServiceCardItem = {
  title: string;
  description: string;
  icon: ReactNode;
  path: string;
};

const serviceCards: readonly ServiceCardItem[] = [
  {
    title: 'Estudio y reservas',
    description: 'Grabación, mezcla, mastering, ensayos, podcast y sesiones privadas desde el ecosistema TDF.',
    icon: <MicExternalOnIcon />,
    path: '/reservar',
  },
  {
    title: 'DJ Booth',
    description: 'Practica, prepara sets y conecta tu proceso creativo con una comunidad que escucha.',
    icon: <MusicNoteIcon />,
    path: '/dj-booth',
  },
  {
    title: 'Escuela y cursos',
    description: 'Programas presenciales para aprender producción, performance, grabación y herramientas de industria.',
    icon: <SchoolIcon />,
    path: '/curso/produccion-musical-jun-2026',
  },
  {
    title: 'Live sessions',
    description: 'Experiencias en vivo para convertir ensayos, lanzamientos y encuentros en contenido compartible.',
    icon: <PlayArrowIcon />,
    path: '/live-sessions/registro',
  },
  {
    title: 'Marketplace',
    description: 'Un canal para descubrir activos, experiencias y servicios ligados al circuito musical.',
    icon: <StorefrontIcon />,
    path: '/marketplace',
  },
  {
    title: 'Comunidad',
    description: 'Perfiles, seguidores, clubes de fans, lanzamientos y señales reales de relación artista-audiencia.',
    icon: <GroupsIcon />,
    path: '/fans',
  },
];

const fanBenefits = [
  'Acceso a material exclusivo de los artistas que sigues.',
  'Descubrimiento de lanzamientos, sesiones y experiencias desde un solo lugar.',
  'Conexión con miembros que siguen a los mismos artistas.',
  'Participación directa en una comunidad musical que también crea valor para el artista.',
];

const artistBenefits = [
  'Un perfil público para presentar tu música, links, bio, videos y ciudad.',
  'Canal directo para conocer mejor a tus fans y fortalecer la relación con ellos.',
  'Visibilidad dentro del hub, clubes de fans y futuros flujos del sello descentralizado.',
  'Herramientas para publicar actividad, activar experiencias y convertir comunidad en tracción.',
];

const fallbackImages = [
  releases[0]?.cover,
  recordings[0]?.image,
  recordings[1]?.image,
  releases[1]?.cover,
].filter(Boolean) as string[];

const formatFollowerCount = (value: number) =>
  value === 1 ? '1 seguidor' : `${value.toLocaleString('es-EC')} seguidores`;

const artistImageFor = (artist: ArtistProfileDTO, index: number) =>
  getArtistHeroImage(artist.apHeroImageUrl, artist.apSlug) ??
  fallbackImages[index % Math.max(fallbackImages.length, 1)] ??
  'https://images.unsplash.com/photo-1511671782779-c97d3d27a1d4?auto=format&fit=crop&w=1200&q=80';

const artistPathFor = (artist: ArtistProfileDTO) =>
  artist.apSlug ? `/artista/${artist.apSlug}` : `/artista/${artist.apArtistId}`;

function Eyebrow({ children }: { children: string }) {
  return (
    <Chip
      label={children}
      sx={{
        width: 'fit-content',
        bgcolor: 'rgba(214,182,107,0.16)',
        border: '1px solid rgba(214,182,107,0.32)',
        color: '#f4d58a',
        fontWeight: 800,
      }}
    />
  );
}

function SectionHeader({
  eyebrow,
  title,
  body,
}: {
  eyebrow: string;
  title: string;
  body: string;
}) {
  return (
    <Stack spacing={1.25} sx={{ maxWidth: 780 }}>
      <Eyebrow>{eyebrow}</Eyebrow>
      <Typography variant="h3" sx={{ fontWeight: 900, color: '#f8fafc', lineHeight: 1.08 }}>
        {title}
      </Typography>
      <Typography variant="body1" sx={{ color: 'rgba(226,232,240,0.76)', fontSize: '1.05rem' }}>
        {body}
      </Typography>
    </Stack>
  );
}

function ValueCard({
  title,
  description,
  action,
  to,
  accent,
}: {
  title: string;
  description: string;
  action: string;
  to: string;
  accent: string;
}) {
  return (
    <Card
      variant="outlined"
      sx={{
        height: '100%',
        borderRadius: 2,
        bgcolor: 'rgba(9,12,18,0.84)',
        borderColor: 'rgba(148,163,184,0.22)',
        color: '#e2e8f0',
      }}
    >
      <CardContent>
        <Stack spacing={2}>
          <Box sx={{ width: 48, height: 4, borderRadius: 999, bgcolor: accent }} />
          <Typography variant="h5" sx={{ fontWeight: 850 }}>
            {title}
          </Typography>
          <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.72)' }}>
            {description}
          </Typography>
          <Button
            disabled={false}
            component={RouterLink}
            to={to}
            variant="outlined"
            sx={{ alignSelf: 'flex-start', textTransform: 'none' }}
          >
            {action}
          </Button>
        </Stack>
      </CardContent>
    </Card>
  );
}

function EmptyListNotice({ message }: { message: string }) {
  return (
    <Alert
      severity="info"
      sx={{ bgcolor: 'rgba(15,23,42,0.88)', color: '#e2e8f0', border: '1px solid rgba(148,163,184,0.2)' }}
    >
      {message}
    </Alert>
  );
}

function BenefitList({ items, emptyMessage }: { items: readonly string[]; emptyMessage: string }) {
  if (items.length === 0) {
    return <EmptyListNotice message={emptyMessage} />;
  }

  return (
    <Stack spacing={1.2}>
      {items.map((item) => (
        <Stack key={item} direction="row" spacing={1.25} alignItems="flex-start">
          <Box
            sx={{
              width: 8,
              height: 8,
              borderRadius: '50%',
              bgcolor: '#22d3ee',
              mt: 0.9,
              flex: '0 0 auto',
            }}
          />
          <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.78)' }}>
            {item}
          </Typography>
        </Stack>
      ))}
    </Stack>
  );
}

function ArtistCarousel({
  artists,
  loading,
  error,
  emptyMessage,
  profileAction,
}: {
  artists: ArtistProfileDTO[];
  loading: boolean;
  error: boolean;
  emptyMessage: string;
  profileAction: string;
}) {
  if (loading) {
    return (
      <Stack direction="row" spacing={1.5} alignItems="center" sx={{ color: 'rgba(226,232,240,0.78)' }}>
        <CircularProgress size={20} color="inherit" />
        <Typography>Cargando artistas destacados...</Typography>
      </Stack>
    );
  }

  if (error) {
    return (
      <Alert severity="info" sx={{ bgcolor: 'rgba(15,23,42,0.88)', color: '#e2e8f0', border: '1px solid rgba(148,163,184,0.2)' }}>
        No pudimos cargar el ranking de artistas ahora mismo. El hub sigue disponible para explorar lanzamientos y crear cuenta.
      </Alert>
    );
  }

  if (artists.length === 0) {
    return <EmptyListNotice message={emptyMessage} />;
  }

  return (
    <Box
      sx={{
        display: 'grid',
        gridAutoFlow: 'column',
        gridAutoColumns: { xs: '82%', sm: '44%', md: '31%' },
        gap: 2,
        overflowX: 'auto',
        pb: 1.5,
        scrollSnapType: 'x mandatory',
        scrollbarColor: 'rgba(214,182,107,0.6) rgba(148,163,184,0.16)',
      }}
      aria-label="Carrusel de artistas destacados"
    >
      {artists.map((artist, index) => (
        <Card
          key={artist.apArtistId}
          variant="outlined"
          sx={{
            scrollSnapAlign: 'start',
            minHeight: 420,
            borderRadius: 2,
            overflow: 'hidden',
            bgcolor: 'rgba(9,12,18,0.92)',
            borderColor: 'rgba(148,163,184,0.22)',
            color: '#e2e8f0',
          }}
        >
          <Box
            component="img"
            src={artistImageFor(artist, index)}
            alt={`Imagen de ${artist.apDisplayName}`}
            sx={{
              width: '100%',
              height: 220,
              objectFit: 'cover',
              display: 'block',
              bgcolor: '#111827',
            }}
          />
          <CardContent>
            <Stack spacing={1.5}>
              <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
                <Chip
                  label={`#${index + 1}`}
                  size="small"
                  sx={{ bgcolor: 'rgba(214,182,107,0.18)', color: '#f4d58a', fontWeight: 800 }}
                />
                <Chip
                  label={formatFollowerCount(artist.apFollowerCount)}
                  size="small"
                  sx={{ bgcolor: 'rgba(34,211,238,0.14)', color: '#67e8f9' }}
                />
              </Stack>
              <Typography variant="h5" sx={{ fontWeight: 850 }}>
                {artist.apDisplayName}
              </Typography>
              <Typography
                variant="body2"
                sx={{
                  color: 'rgba(226,232,240,0.72)',
                  minHeight: 42,
                  display: '-webkit-box',
                  WebkitLineClamp: 2,
                  WebkitBoxOrient: 'vertical',
                  overflow: 'hidden',
                }}
              >
                {artist.apBio || artist.apGenres || 'Perfil activo dentro de la comunidad TDF.'}
              </Typography>
              <Button
                disabled={false}
                component={RouterLink}
                to={artistPathFor(artist)}
                variant="text"
                sx={{ alignSelf: 'flex-start', color: '#f4d58a', textTransform: 'none' }}
              >
                {profileAction}
              </Button>
            </Stack>
          </CardContent>
        </Card>
      ))}
    </Box>
  );
}

export default function TdfPlatformPage() {
  const { t } = useTranslation();
  const artistsQuery = useQuery({
    queryKey: ['tdf-platform', 'public-artists'],
    queryFn: Fans.listPublicArtists,
    staleTime: 5 * 60 * 1000,
  });

  const topArtists = useMemo(() => {
    const artists = artistsQuery.data ?? [];
    return [...artists]
      .sort((a, b) => {
        const followerDelta = (b.apFollowerCount ?? 0) - (a.apFollowerCount ?? 0);
        if (followerDelta !== 0) return followerDelta;
        return a.apDisplayName.localeCompare(b.apDisplayName);
      })
      .slice(0, 12);
  }, [artistsQuery.data]);

  const totalFollowers = useMemo(
    () => (artistsQuery.data ?? []).reduce((sum, artist) => sum + Math.max(0, artist.apFollowerCount ?? 0), 0),
    [artistsQuery.data],
  );

  const copy = {
    createAccount: t('tdfPlatform.cta.createAccount'),
    fanProfile: t('tdfPlatform.cta.fanProfile'),
    artistProfile: t('tdfPlatform.cta.artistProfile'),
    createFanProfile: t('tdfPlatform.cta.createFanProfile'),
    createArtistProfile: t('tdfPlatform.cta.createArtistProfile'),
    viewArtistProfile: t('tdfPlatform.cta.viewArtistProfile'),
    explore: t('tdfPlatform.cta.explore'),
    reserveExperience: t('tdfPlatform.cta.reserveExperience'),
    viewLocation: t('tdfPlatform.cta.viewLocation'),
    viewReleases: t('tdfPlatform.cta.viewReleases'),
    artistsEmpty: t('tdfPlatform.empty.artists'),
    servicesEmpty: t('tdfPlatform.empty.services'),
    fanBenefitsEmpty: t('tdfPlatform.empty.fanBenefits'),
    artistBenefitsEmpty: t('tdfPlatform.empty.artistBenefits'),
    startEyebrow: t('tdfPlatform.sections.startEyebrow'),
  };

  return (
    <Box sx={{ minHeight: '100vh', bgcolor: '#07090f', color: '#e2e8f0' }}>
      <Box
        component="section"
        sx={{
          position: 'relative',
          minHeight: { xs: 720, md: 760 },
          overflow: 'hidden',
          display: 'flex',
          alignItems: 'center',
          borderBottom: '1px solid rgba(148,163,184,0.14)',
        }}
      >
        <Box
          component="video"
          src={HERO_VIDEO_SRC}
          autoPlay
          muted
          loop
          playsInline
          aria-hidden="true"
          sx={{
            position: 'absolute',
            inset: 0,
            width: '100%',
            height: '100%',
            objectFit: 'cover',
            opacity: 0.5,
          }}
        />
        <Box
          sx={{
            position: 'absolute',
            inset: 0,
            background:
              'linear-gradient(90deg, rgba(7,9,15,0.96) 0%, rgba(7,9,15,0.86) 42%, rgba(7,9,15,0.46) 100%), linear-gradient(0deg, rgba(7,9,15,0.88) 0%, transparent 42%)',
          }}
        />
        <Container maxWidth="lg" sx={{ position: 'relative', py: { xs: 8, md: 12 } }}>
          <Stack spacing={3.5} sx={{ maxWidth: 820 }}>
            <Eyebrow>Comunidad musical · Quito · Plataforma</Eyebrow>
            <Stack spacing={2}>
              <Typography
                component="h1"
                sx={{
                  fontSize: { xs: '3.4rem', sm: '4.6rem', md: '6.2rem' },
                  lineHeight: 0.94,
                  fontWeight: 950,
                  color: '#f8fafc',
                }}
              >
                TDF Records
              </Typography>
              <Typography
                variant="h5"
                sx={{ color: 'rgba(241,245,249,0.86)', lineHeight: 1.45, maxWidth: 760 }}
              >
                La plataforma de una comunidad de músicos y entusiastas de la música, construida para convertirse en un sello descentralizado donde artistas y seguidores estrechan su relación y aportan más valor de lado y lado.
              </Typography>
            </Stack>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
              <Button
                disabled={false}
                component={RouterLink}
                to={GENERAL_SIGNUP_PATH}
                variant="contained"
                size="large"
                startIcon={<AccountCircleIcon />}
                sx={{
                  bgcolor: '#d6b66b',
                  color: '#0b0f16',
                  fontWeight: 900,
                  textTransform: 'none',
                  px: 3,
                  '&:hover': { bgcolor: '#f4d58a' },
                }}
              >
                {copy.createAccount}
              </Button>
              <Button
                disabled={false}
                component={RouterLink}
                to={FAN_SIGNUP_PATH}
                variant="outlined"
                size="large"
                sx={{ color: '#f8fafc', borderColor: 'rgba(248,250,252,0.34)', textTransform: 'none' }}
              >
                {copy.fanProfile}
              </Button>
              <Button
                disabled={false}
                component={RouterLink}
                to={ARTIST_SIGNUP_PATH}
                variant="text"
                size="large"
                sx={{ color: '#67e8f9', textTransform: 'none' }}
              >
                {copy.artistProfile}
              </Button>
            </Stack>
            <Stack direction="row" spacing={1} flexWrap="wrap">
              <Chip label="Fans con acceso exclusivo" sx={{ bgcolor: 'rgba(34,211,238,0.12)', color: '#a5f3fc' }} />
              <Chip label="Artistas con datos de comunidad" sx={{ bgcolor: 'rgba(251,113,133,0.13)', color: '#fecdd3' }} />
              <Chip label="Estudio, escuela y live sessions" sx={{ bgcolor: 'rgba(214,182,107,0.16)', color: '#f4d58a' }} />
            </Stack>
          </Stack>
        </Container>
      </Box>

      <Box component="section" sx={{ py: { xs: 7, md: 10 }, bgcolor: '#090c12' }}>
        <Container maxWidth="lg">
          <Grid container spacing={4} alignItems="stretch">
            <Grid item xs={12} md={6}>
              <ValueCard
                title="Para fans"
                description="Crea tu perfil para seguir artistas, recibir contenido exclusivo y conectar con otros miembros que comparten tus gustos."
                action={copy.createFanProfile}
                to={FAN_SIGNUP_PATH}
                accent="#22d3ee"
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <ValueCard
                title="Para artistas"
                description="Crea o reclama tu perfil para publicar tu mundo, conocer mejor a tus fans y activar experiencias directas con tu comunidad."
                action={copy.createArtistProfile}
                to={ARTIST_SIGNUP_PATH}
                accent="#fb7185"
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <Stack spacing={2.5}>
                <SectionHeader
                  eyebrow="Fans"
                  title="Más que escuchar: participar."
                  body="El perfil fan convierte la curiosidad en una relación continua con artistas, lanzamientos y comunidad."
                />
                <BenefitList items={fanBenefits} emptyMessage={copy.fanBenefitsEmpty} />
              </Stack>
            </Grid>
            <Grid item xs={12} md={6}>
              <Stack spacing={2.5}>
                <SectionHeader
                  eyebrow="Artistas"
                  title="Más que publicar: conocer a tu audiencia."
                  body="El perfil de artista da una base propia para reunir señales, contar tu historia y mover a tus seguidores hacia experiencias reales."
                />
                <BenefitList items={artistBenefits} emptyMessage={copy.artistBenefitsEmpty} />
              </Stack>
            </Grid>
          </Grid>
        </Container>
      </Box>

      <Box component="section" sx={{ py: { xs: 7, md: 10 }, borderTop: '1px solid rgba(148,163,184,0.14)' }}>
        <Container maxWidth="lg">
          <Stack spacing={4}>
            <SectionHeader
              eyebrow="Todo TDF"
              title="Una misma plataforma para estudio, sello, escuela y comunidad."
              body="TDF conecta servicios musicales, contenido, perfiles y experiencias para que el valor no se quede solo en una publicación o una sesión aislada."
            />
            <Grid container spacing={2}>
              {serviceCards.length === 0 ? (
                <Grid item xs={12}>
                  <EmptyListNotice message={copy.servicesEmpty} />
                </Grid>
              ) : (
                serviceCards.map((item) => (
                  <Grid item xs={12} sm={6} md={4} key={item.title}>
                    <Card
                      variant="outlined"
                      sx={{
                        height: '100%',
                        borderRadius: 2,
                        bgcolor: 'rgba(15,23,42,0.62)',
                        borderColor: 'rgba(148,163,184,0.2)',
                        color: '#e2e8f0',
                      }}
                    >
                      <CardContent>
                        <Stack spacing={1.4}>
                          <Box sx={{ color: '#f4d58a', display: 'flex' }}>{item.icon}</Box>
                          <Typography variant="h6" sx={{ fontWeight: 850 }}>
                            {item.title}
                          </Typography>
                          <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.72)' }}>
                            {item.description}
                          </Typography>
                          <Button
                            disabled={false}
                            component={RouterLink}
                            to={item.path}
                            variant="text"
                            sx={{ alignSelf: 'flex-start', color: '#67e8f9', textTransform: 'none', px: 0 }}
                          >
                            {copy.explore}
                          </Button>
                        </Stack>
                      </CardContent>
                    </Card>
                  </Grid>
                ))
              )}
            </Grid>
          </Stack>
        </Container>
      </Box>

      <Box component="section" sx={{ py: { xs: 7, md: 10 }, bgcolor: '#090c12' }}>
        <Container maxWidth="lg">
          <Stack spacing={4}>
            <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" spacing={2}>
              <SectionHeader
                eyebrow="Top comunidad"
                title="Artistas con más tracción dentro del hub."
                body="Este carrusel se alimenta de artistas publicados en la plataforma y se ordena por seguidores reales; las señales de reacciones se integran dentro de los clubes y experiencias de comunidad."
              />
              <Stack direction="row" spacing={1} flexWrap="wrap" alignSelf={{ xs: 'flex-start', md: 'flex-end' }}>
                <Chip
                  icon={<GroupsIcon />}
                  label={`${totalFollowers.toLocaleString('es-EC')} seguidores`}
                  sx={{ bgcolor: 'rgba(34,211,238,0.12)', color: '#a5f3fc' }}
                />
                <Chip
                  icon={<AutoAwesomeIcon />}
                  label={`${topArtists.length} destacados`}
                  sx={{ bgcolor: 'rgba(214,182,107,0.16)', color: '#f4d58a' }}
                />
              </Stack>
            </Stack>
            <ArtistCarousel
              artists={topArtists}
              loading={artistsQuery.isLoading}
              error={artistsQuery.isError}
              emptyMessage={copy.artistsEmpty}
              profileAction={copy.viewArtistProfile}
            />
          </Stack>
        </Container>
      </Box>

      <Box component="section" sx={{ py: { xs: 7, md: 10 }, borderTop: '1px solid rgba(148,163,184,0.14)' }}>
        <Container maxWidth="lg">
          <Grid container spacing={4} alignItems="center">
            <Grid item xs={12} md={6}>
              <Stack spacing={3}>
                <SectionHeader
                  eyebrow="Lugar fisico + plataforma"
                  title="TDF también vive en el estudio."
                  body="La comunidad digital se conecta con experiencias presenciales: reservas, clases, sesiones en vivo, DJ Booth y encuentros alrededor de la música."
                />
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
                  <Button
                    disabled={false}
                    component={RouterLink}
                    to="/reservar"
                    variant="contained"
                    startIcon={<EventAvailableIcon />}
                    sx={{ textTransform: 'none' }}
                  >
                    {copy.reserveExperience}
                  </Button>
                  <Button
                    disabled={false}
                    component="a"
                    href={STUDIO_MAP_URL}
                    target="_blank"
                    rel="noreferrer"
                    variant="outlined"
                    sx={{ textTransform: 'none' }}
                  >
                    {copy.viewLocation}
                  </Button>
                </Stack>
              </Stack>
            </Grid>
            <Grid item xs={12} md={6}>
              <Box
                component="img"
                src={recordings[1]?.image ?? recordings[0]?.image}
                alt="Sesión musical en estudio"
                sx={{
                  width: '100%',
                  aspectRatio: '4 / 3',
                  objectFit: 'cover',
                  borderRadius: 2,
                  border: '1px solid rgba(148,163,184,0.22)',
                  display: 'block',
                }}
              />
            </Grid>
          </Grid>
        </Container>
      </Box>

      <Box component="section" sx={{ py: { xs: 7, md: 10 }, bgcolor: '#0c1018' }}>
        <Container maxWidth="md">
          <Stack spacing={3} alignItems="center" textAlign="center">
            <Eyebrow>{copy.startEyebrow}</Eyebrow>
            <Typography variant="h3" sx={{ color: '#f8fafc', fontWeight: 950 }}>
              Crea tu usuario y elige tu ruta dentro de TDF.
            </Typography>
            <Typography sx={{ color: 'rgba(226,232,240,0.76)' }}>
              Puedes iniciar como fan, artista o ambos. La plataforma se adapta a la relación que quieres construir con la música.
            </Typography>
            <Divider flexItem sx={{ borderColor: 'rgba(148,163,184,0.16)' }} />
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
              <Button
                disabled={false}
                component={RouterLink}
                to={GENERAL_SIGNUP_PATH}
                variant="contained"
                size="large"
                sx={{ bgcolor: '#d6b66b', color: '#0b0f16', fontWeight: 900, textTransform: 'none', '&:hover': { bgcolor: '#f4d58a' } }}
              >
                {copy.createAccount}
              </Button>
              <Button
                disabled={false}
                component={RouterLink}
                to="/records"
                variant="outlined"
                size="large"
                sx={{ textTransform: 'none' }}
              >
                {copy.viewReleases}
              </Button>
            </Stack>
          </Stack>
        </Container>
      </Box>
    </Box>
  );
}
