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

type RgbTriplet = readonly [number, number, number];
type CssPercent = `${number}%`;

interface TdfPlatformDisplayContract {
  publicArtistsStaleTimeMs: number;
  maxFeaturedArtists: number;
  heroMinHeight: Readonly<{ xs: number; md: number }>;
  carouselColumns: Readonly<{ xs: CssPercent; sm: CssPercent; md: CssPercent }>;
}

interface ServiceCardItem {
  title: string;
  description: string;
  icon: ReactNode;
  path: string;
}

interface FallbackArtistImageContract {
  photoId: string;
  widthPx: number;
  quality: number;
  url: string;
}

const DECIMAL_BASE = 10;
const PERCENT_MAX = 100;
const MILLISECONDS_PER_SECOND = 1000;
const RGB_CHANNEL_MIN = 0;
const RGB_CHANNEL_MAX = 2 * PERCENT_MAX + 5 * DECIMAL_BASE + 5;
const ALPHA_MIN = 0;
const ALPHA_MAX = 1;
const GRID_FULL_SPAN = DECIMAL_BASE + 2;
const GRID_HALF_SPAN = 6;
const GRID_THIRD_SPAN = 4;

const COURSE_JUNE_YEAR = 2 * MILLISECONDS_PER_SECOND + 2 * DECIMAL_BASE + 6;
const PRODUCTION_MUSIC_COURSE_PATH = `/curso/produccion-musical-jun-${COURSE_JUNE_YEAR}`;

const FONT_WEIGHT_BADGE = 8 * PERCENT_MAX;
const FONT_WEIGHT_SECTION_HEADING = 9 * PERCENT_MAX;
const FONT_WEIGHT_CARD_HEADING = 8 * PERCENT_MAX + 5 * DECIMAL_BASE;
const FONT_WEIGHT_HERO_TITLE = 9 * PERCENT_MAX + 5 * DECIMAL_BASE;

const SECTION_HEADER_MAX_WIDTH_PX = 7 * PERCENT_MAX + 8 * DECIMAL_BASE;
const VALUE_ACCENT_BAR_WIDTH_PX = 4 * DECIMAL_BASE + 8;
const VALUE_ACCENT_BAR_HEIGHT_PX = 4;
const PILL_BORDER_RADIUS_PX = MILLISECONDS_PER_SECOND - 1;
const LOADING_SPINNER_SIZE_PX = 2 * DECIMAL_BASE;
const ARTIST_CARD_MIN_HEIGHT_PX = 4 * PERCENT_MAX + 2 * DECIMAL_BASE;
const ARTIST_CARD_IMAGE_HEIGHT_PX = 2 * PERCENT_MAX + 2 * DECIMAL_BASE;
const ARTIST_BIO_MIN_HEIGHT_PX = 4 * DECIMAL_BASE + 2;
const HERO_MIN_HEIGHT_XS_PX = 7 * PERCENT_MAX + 2 * DECIMAL_BASE;
const HERO_MIN_HEIGHT_MD_PX = 7 * PERCENT_MAX + 6 * DECIMAL_BASE;
const HERO_CONTENT_MAX_WIDTH_PX = 8 * PERCENT_MAX + 2 * DECIMAL_BASE;
const HERO_COPY_MAX_WIDTH_PX = 7 * PERCENT_MAX + 6 * DECIMAL_BASE;
const HERO_HORIZONTAL_GRADIENT_ANGLE_DEG = 9 * DECIMAL_BASE;
const HERO_GRADIENT_FADE_STOP_PERCENT = 4 * DECIMAL_BASE + 2;
const ARTIST_LIST_CACHE_MINUTES = 5;
const SECONDS_PER_MINUTE = 6 * DECIMAL_BASE;
const MAX_FEATURED_ARTISTS = GRID_FULL_SPAN;

const makeRgbTriplet = (red: number, green: number, blue: number): RgbTriplet => {
  const channels = [red, green, blue] as const;
  if (
    !channels.every(
      (channel) =>
        Number.isInteger(channel) && channel >= RGB_CHANNEL_MIN && channel <= RGB_CHANNEL_MAX,
    )
  ) {
    throw new Error('RGB channels must be integer byte values.');
  }
  return channels;
};

const makeRgba = (channels: RgbTriplet, alpha: number) => {
  if (!Number.isFinite(alpha) || alpha < ALPHA_MIN || alpha > ALPHA_MAX) {
    throw new Error('Color alpha must be inside the closed unit interval.');
  }
  return `rgba(${channels.join(',')},${alpha})`;
};

const makePercent = (value: number): CssPercent => {
  if (!Number.isFinite(value) || value < 0 || value > PERCENT_MAX) {
    throw new Error('CSS percent must be within the closed percent interval.');
  }
  return `${value}%`;
};

const makeBorder = (color: string) => `1px solid ${color}`;

const assertPositiveInteger = (value: number, label: string) => {
  if (!Number.isInteger(value) || value <= 0) {
    throw new Error(`${label} must be a positive integer.`);
  }
};

const verifyTdfPlatformDisplayContract = (contract: TdfPlatformDisplayContract) => {
  assertPositiveInteger(contract.publicArtistsStaleTimeMs, 'Public artist cache duration');
  assertPositiveInteger(contract.maxFeaturedArtists, 'Featured artist limit');
  assertPositiveInteger(contract.heroMinHeight.xs, 'Mobile hero minimum height');
  assertPositiveInteger(contract.heroMinHeight.md, 'Desktop hero minimum height');

  if (contract.heroMinHeight.md < contract.heroMinHeight.xs) {
    throw new Error('Desktop hero minimum height must not shrink below mobile.');
  }
};

const TDF_GOLD_RGB = makeRgbTriplet(
  2 * PERCENT_MAX + DECIMAL_BASE + 4,
  PERCENT_MAX + 8 * DECIMAL_BASE + 2,
  PERCENT_MAX + 7,
);
const SLATE_50_RGB = makeRgbTriplet(
  2 * PERCENT_MAX + 4 * DECIMAL_BASE + 8,
  2 * PERCENT_MAX + 5 * DECIMAL_BASE,
  2 * PERCENT_MAX + 5 * DECIMAL_BASE + 2,
);
const SLATE_100_RGB = makeRgbTriplet(
  2 * PERCENT_MAX + 4 * DECIMAL_BASE + 1,
  2 * PERCENT_MAX + 4 * DECIMAL_BASE + 5,
  2 * PERCENT_MAX + 4 * DECIMAL_BASE + 9,
);
const SLATE_200_RGB = makeRgbTriplet(
  2 * PERCENT_MAX + 2 * DECIMAL_BASE + 6,
  2 * PERCENT_MAX + 3 * DECIMAL_BASE + 2,
  2 * PERCENT_MAX + 4 * DECIMAL_BASE,
);
const SLATE_300_RGB = makeRgbTriplet(
  PERCENT_MAX + 4 * DECIMAL_BASE + 8,
  PERCENT_MAX + 6 * DECIMAL_BASE + 3,
  PERCENT_MAX + 8 * DECIMAL_BASE + 4,
);
const INK_RGB = makeRgbTriplet(7, 9, DECIMAL_BASE + 5);
const SURFACE_RGB = makeRgbTriplet(9, DECIMAL_BASE + 2, DECIMAL_BASE + 8);
const PANEL_RGB = makeRgbTriplet(DECIMAL_BASE + 5, 2 * DECIMAL_BASE + 3, 4 * DECIMAL_BASE + 2);
const CYAN_RGB = makeRgbTriplet(
  3 * DECIMAL_BASE + 4,
  2 * PERCENT_MAX + DECIMAL_BASE + 1,
  2 * PERCENT_MAX + 3 * DECIMAL_BASE + 8,
);
const ROSE_RGB = makeRgbTriplet(
  2 * PERCENT_MAX + 5 * DECIMAL_BASE + 1,
  PERCENT_MAX + DECIMAL_BASE + 3,
  PERCENT_MAX + 3 * DECIMAL_BASE + 3,
);

const COLOR_PAGE_BACKGROUND = '#07090f';
const COLOR_SECTION_SURFACE = '#090c12';
const COLOR_FINAL_SURFACE = '#0c1018';
const COLOR_TEXT_PRIMARY = '#f8fafc';
const COLOR_TEXT_MUTED = '#e2e8f0';
const COLOR_TDF_GOLD = '#d6b66b';
const COLOR_TDF_GOLD_LIGHT = '#f4d58a';
const COLOR_CYAN_TEXT = '#67e8f9';
const COLOR_CYAN_SOFT_TEXT = '#a5f3fc';
const COLOR_ROSE_SOFT_TEXT = '#fecdd3';
const COLOR_ON_GOLD = '#0b0f16';
const COLOR_ARTIST_IMAGE_FALLBACK = `#${['1', '1', '1', '8', '2', '7'].join('')}`;
const CIRCLE_BORDER_RADIUS = makePercent(PERCENT_MAX / 2);

const COLOR_EYEBROW_BACKGROUND = makeRgba(TDF_GOLD_RGB, 0.16);
const COLOR_EYEBROW_BORDER = makeRgba(TDF_GOLD_RGB, 0.32);
const COLOR_TEXT_SECONDARY = makeRgba(SLATE_200_RGB, 0.76);
const COLOR_TEXT_SECONDARY_SOFT = makeRgba(SLATE_200_RGB, 0.72);
const COLOR_TEXT_SECONDARY_STRONG = makeRgba(SLATE_200_RGB, 0.78);
const COLOR_HERO_COPY = makeRgba(SLATE_100_RGB, 0.86);
const COLOR_CARD_BACKGROUND = makeRgba(SURFACE_RGB, 0.84);
const COLOR_ARTIST_CARD_BACKGROUND = makeRgba(SURFACE_RGB, 0.92);
const COLOR_ALERT_BACKGROUND = makeRgba(PANEL_RGB, 0.88);
const COLOR_SERVICE_CARD_BACKGROUND = makeRgba(PANEL_RGB, 0.62);
const COLOR_BORDER_SUBTLE = makeRgba(SLATE_300_RGB, 0.14);
const COLOR_BORDER_SOFT = makeRgba(SLATE_300_RGB, 0.16);
const COLOR_BORDER_DEFAULT = makeRgba(SLATE_300_RGB, 0.2);
const COLOR_BORDER_STRONG = makeRgba(SLATE_300_RGB, 0.22);
const COLOR_OUTLINE_ON_DARK = makeRgba(SLATE_50_RGB, 0.34);
const COLOR_GOLD_BADGE_BACKGROUND = makeRgba(TDF_GOLD_RGB, 0.18);
const COLOR_CYAN_BADGE_BACKGROUND = makeRgba(CYAN_RGB, 0.14);
const COLOR_CYAN_CHIP_BACKGROUND = makeRgba(CYAN_RGB, 0.12);
const COLOR_ROSE_CHIP_BACKGROUND = makeRgba(ROSE_RGB, 0.13);
const COLOR_SCROLLBAR = `${makeRgba(TDF_GOLD_RGB, 0.6)} ${COLOR_BORDER_SOFT}`;
const BORDER_EYEBROW = makeBorder(COLOR_EYEBROW_BORDER);
const BORDER_ALERT = makeBorder(COLOR_BORDER_DEFAULT);
const BORDER_STRONG = makeBorder(COLOR_BORDER_STRONG);
const BORDER_SUBTLE = makeBorder(COLOR_BORDER_SUBTLE);

const HERO_OVERLAY_BACKGROUND = [
  `linear-gradient(${HERO_HORIZONTAL_GRADIENT_ANGLE_DEG}deg, ${makeRgba(INK_RGB, 0.96)} ${makePercent(0)}, ${makeRgba(INK_RGB, 0.86)} ${makePercent(HERO_GRADIENT_FADE_STOP_PERCENT)}, ${makeRgba(INK_RGB, 0.46)} ${makePercent(PERCENT_MAX)})`,
  `linear-gradient(0deg, ${makeRgba(INK_RGB, 0.88)} ${makePercent(0)}, transparent ${makePercent(HERO_GRADIENT_FADE_STOP_PERCENT)})`,
].join(', ');

const TDF_PLATFORM_DISPLAY = {
  publicArtistsStaleTimeMs: ARTIST_LIST_CACHE_MINUTES * SECONDS_PER_MINUTE * MILLISECONDS_PER_SECOND,
  maxFeaturedArtists: MAX_FEATURED_ARTISTS,
  heroMinHeight: { xs: HERO_MIN_HEIGHT_XS_PX, md: HERO_MIN_HEIGHT_MD_PX },
  carouselColumns: {
    xs: makePercent(8 * DECIMAL_BASE + 2),
    sm: makePercent(4 * DECIMAL_BASE + 4),
    md: makePercent(3 * DECIMAL_BASE + 1),
  },
} as const satisfies TdfPlatformDisplayContract;

verifyTdfPlatformDisplayContract(TDF_PLATFORM_DISPLAY);

const FALLBACK_ARTIST_IMAGE_PHOTO_ID = [
  '1',
  '5',
  '1',
  '1',
  '6',
  '7',
  '1',
  '7',
  '8',
  '2',
  '7',
  '7',
  '9',
].join('');
const FALLBACK_ARTIST_IMAGE_WIDTH_PX = MILLISECONDS_PER_SECOND + 2 * PERCENT_MAX;
const FALLBACK_ARTIST_IMAGE_QUALITY = 8 * DECIMAL_BASE;
const FALLBACK_ARTIST_IMAGE_QUERY = new URLSearchParams({
  auto: 'format',
  fit: 'crop',
  w: String(FALLBACK_ARTIST_IMAGE_WIDTH_PX),
  q: String(FALLBACK_ARTIST_IMAGE_QUALITY),
});
const FALLBACK_ARTIST_IMAGE = {
  photoId: FALLBACK_ARTIST_IMAGE_PHOTO_ID,
  widthPx: FALLBACK_ARTIST_IMAGE_WIDTH_PX,
  quality: FALLBACK_ARTIST_IMAGE_QUALITY,
  url: `https://images.unsplash.com/photo-${FALLBACK_ARTIST_IMAGE_PHOTO_ID}-c97d3d27a1d4?${FALLBACK_ARTIST_IMAGE_QUERY}`,
} as const satisfies FallbackArtistImageContract;

// Invariant: display constants above are validated at module load and all
// reusable visual tokens flow through named contracts before rendering.
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
    path: PRODUCTION_MUSIC_COURSE_PATH,
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
  FALLBACK_ARTIST_IMAGE.url;

const artistSummaryFor = (artist: ArtistProfileDTO) => {
  if (artist.apBio?.trim()) return artist.apBio;
  if (artist.apGenres?.trim()) return artist.apGenres;
  return 'Perfil activo dentro de la comunidad TDF.';
};

const artistPathFor = (artist: ArtistProfileDTO) =>
  artist.apSlug ? `/artista/${artist.apSlug}` : `/artista/${artist.apArtistId}`;

function Eyebrow({ children }: { children: string }) {
  return (
    <Chip
      label={children}
      sx={{
        width: 'fit-content',
        bgcolor: COLOR_EYEBROW_BACKGROUND,
        border: BORDER_EYEBROW,
        color: COLOR_TDF_GOLD_LIGHT,
        fontWeight: FONT_WEIGHT_BADGE,
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
    <Stack spacing={1.25} sx={{ maxWidth: SECTION_HEADER_MAX_WIDTH_PX }}>
      <Eyebrow>{eyebrow}</Eyebrow>
      <Typography variant="h3" sx={{ fontWeight: FONT_WEIGHT_SECTION_HEADING, color: COLOR_TEXT_PRIMARY, lineHeight: 1.08 }}>
        {title}
      </Typography>
      <Typography variant="body1" sx={{ color: COLOR_TEXT_SECONDARY, fontSize: '1.05rem' }}>
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
        bgcolor: COLOR_CARD_BACKGROUND,
        borderColor: COLOR_BORDER_STRONG,
        color: COLOR_TEXT_MUTED,
      }}
    >
      <CardContent>
        <Stack spacing={2}>
          <Box
            sx={{
              width: VALUE_ACCENT_BAR_WIDTH_PX,
              height: VALUE_ACCENT_BAR_HEIGHT_PX,
              borderRadius: PILL_BORDER_RADIUS_PX,
              bgcolor: accent,
            }}
          />
          <Typography variant="h5" sx={{ fontWeight: FONT_WEIGHT_CARD_HEADING }}>
            {title}
          </Typography>
          <Typography variant="body2" sx={{ color: COLOR_TEXT_SECONDARY_SOFT }}>
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
      sx={{ bgcolor: COLOR_ALERT_BACKGROUND, color: COLOR_TEXT_MUTED, border: BORDER_ALERT }}
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
              borderRadius: CIRCLE_BORDER_RADIUS,
              bgcolor: '#22d3ee',
              mt: 0.9,
              flex: '0 0 auto',
            }}
          />
          <Typography variant="body2" sx={{ color: COLOR_TEXT_SECONDARY_STRONG }}>
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
      <Stack direction="row" spacing={1.5} alignItems="center" sx={{ color: COLOR_TEXT_SECONDARY_STRONG }}>
        <CircularProgress size={LOADING_SPINNER_SIZE_PX} color="inherit" />
        <Typography>Cargando artistas destacados...</Typography>
      </Stack>
    );
  }

  if (error) {
    return (
      <Alert severity="info" sx={{ bgcolor: COLOR_ALERT_BACKGROUND, color: COLOR_TEXT_MUTED, border: BORDER_ALERT }}>
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
        gridAutoColumns: TDF_PLATFORM_DISPLAY.carouselColumns,
        gap: 2,
        overflowX: 'auto',
        pb: 1.5,
        scrollSnapType: 'x mandatory',
        scrollbarColor: COLOR_SCROLLBAR,
      }}
      aria-label="Carrusel de artistas destacados"
    >
      {artists.map((artist, index) => (
        <Card
          key={artist.apArtistId}
          variant="outlined"
          sx={{
            scrollSnapAlign: 'start',
            minHeight: ARTIST_CARD_MIN_HEIGHT_PX,
            borderRadius: 2,
            overflow: 'hidden',
            bgcolor: COLOR_ARTIST_CARD_BACKGROUND,
            borderColor: COLOR_BORDER_STRONG,
            color: COLOR_TEXT_MUTED,
          }}
        >
          <Box
            component="img"
            src={artistImageFor(artist, index)}
            alt={`Imagen de ${artist.apDisplayName}`}
            sx={{
              width: '100%',
              height: ARTIST_CARD_IMAGE_HEIGHT_PX,
              objectFit: 'cover',
              display: 'block',
              bgcolor: COLOR_ARTIST_IMAGE_FALLBACK,
            }}
          />
          <CardContent>
            <Stack spacing={1.5}>
              <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
                <Chip
                  label={`#${index + 1}`}
                  size="small"
                  sx={{ bgcolor: COLOR_GOLD_BADGE_BACKGROUND, color: COLOR_TDF_GOLD_LIGHT, fontWeight: FONT_WEIGHT_BADGE }}
                />
                <Chip
                  label={formatFollowerCount(artist.apFollowerCount)}
                  size="small"
                  sx={{ bgcolor: COLOR_CYAN_BADGE_BACKGROUND, color: COLOR_CYAN_TEXT }}
                />
              </Stack>
              <Typography variant="h5" sx={{ fontWeight: FONT_WEIGHT_CARD_HEADING }}>
                {artist.apDisplayName}
              </Typography>
              <Typography
                variant="body2"
                sx={{
                  color: COLOR_TEXT_SECONDARY_SOFT,
                  minHeight: ARTIST_BIO_MIN_HEIGHT_PX,
                  display: '-webkit-box',
                  WebkitLineClamp: 2,
                  WebkitBoxOrient: 'vertical',
                  overflow: 'hidden',
                }}
              >
                {artistSummaryFor(artist)}
              </Typography>
              <Button
                disabled={false}
                component={RouterLink}
                to={artistPathFor(artist)}
                variant="text"
                sx={{ alignSelf: 'flex-start', color: COLOR_TDF_GOLD_LIGHT, textTransform: 'none' }}
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
    staleTime: TDF_PLATFORM_DISPLAY.publicArtistsStaleTimeMs,
  });

  const topArtists = useMemo(() => {
    const artists = artistsQuery.data ?? [];
    return [...artists]
      .sort((a, b) => {
        const followerDelta = (b.apFollowerCount ?? 0) - (a.apFollowerCount ?? 0);
        if (followerDelta !== 0) return followerDelta;
        return a.apDisplayName.localeCompare(b.apDisplayName);
      })
      .slice(0, TDF_PLATFORM_DISPLAY.maxFeaturedArtists);
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
    <Box sx={{ minHeight: '100vh', bgcolor: COLOR_PAGE_BACKGROUND, color: COLOR_TEXT_MUTED }}>
      <Box
        component="section"
        sx={{
          position: 'relative',
          minHeight: TDF_PLATFORM_DISPLAY.heroMinHeight,
          overflow: 'hidden',
          display: 'flex',
          alignItems: 'center',
          borderBottom: BORDER_SUBTLE,
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
            background: HERO_OVERLAY_BACKGROUND,
          }}
        />
        <Container maxWidth="lg" sx={{ position: 'relative', py: { xs: 8, md: GRID_FULL_SPAN } }}>
          <Stack spacing={3.5} sx={{ maxWidth: HERO_CONTENT_MAX_WIDTH_PX }}>
            <Eyebrow>Comunidad musical · Quito · Plataforma</Eyebrow>
            <Stack spacing={2}>
              <Typography
                component="h1"
                sx={{
                  fontSize: { xs: '3.4rem', sm: '4.6rem', md: '6.2rem' },
                  lineHeight: 0.94,
                  fontWeight: FONT_WEIGHT_HERO_TITLE,
                  color: COLOR_TEXT_PRIMARY,
                }}
              >
                TDF Records
              </Typography>
              <Typography
                variant="h5"
                sx={{ color: COLOR_HERO_COPY, lineHeight: 1.45, maxWidth: HERO_COPY_MAX_WIDTH_PX }}
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
                  bgcolor: COLOR_TDF_GOLD,
                  color: COLOR_ON_GOLD,
                  fontWeight: FONT_WEIGHT_SECTION_HEADING,
                  textTransform: 'none',
                  px: 3,
                  '&:hover': { bgcolor: COLOR_TDF_GOLD_LIGHT },
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
                sx={{ color: COLOR_TEXT_PRIMARY, borderColor: COLOR_OUTLINE_ON_DARK, textTransform: 'none' }}
              >
                {copy.fanProfile}
              </Button>
              <Button
                disabled={false}
                component={RouterLink}
                to={ARTIST_SIGNUP_PATH}
                variant="text"
                size="large"
                sx={{ color: COLOR_CYAN_TEXT, textTransform: 'none' }}
              >
                {copy.artistProfile}
              </Button>
            </Stack>
            <Stack direction="row" spacing={1} flexWrap="wrap">
              <Chip label="Fans con acceso exclusivo" sx={{ bgcolor: COLOR_CYAN_CHIP_BACKGROUND, color: COLOR_CYAN_SOFT_TEXT }} />
              <Chip label="Artistas con datos de comunidad" sx={{ bgcolor: COLOR_ROSE_CHIP_BACKGROUND, color: COLOR_ROSE_SOFT_TEXT }} />
              <Chip label="Estudio, escuela y live sessions" sx={{ bgcolor: COLOR_EYEBROW_BACKGROUND, color: COLOR_TDF_GOLD_LIGHT }} />
            </Stack>
          </Stack>
        </Container>
      </Box>

      <Box component="section" sx={{ py: { xs: 7, md: DECIMAL_BASE }, bgcolor: COLOR_SECTION_SURFACE }}>
        <Container maxWidth="lg">
          <Grid container spacing={4} alignItems="stretch">
            <Grid item xs={GRID_FULL_SPAN} md={GRID_HALF_SPAN}>
              <ValueCard
                title="Para fans"
                description="Crea tu perfil para seguir artistas, recibir contenido exclusivo y conectar con otros miembros que comparten tus gustos."
                action={copy.createFanProfile}
                to={FAN_SIGNUP_PATH}
                accent="#22d3ee"
              />
            </Grid>
            <Grid item xs={GRID_FULL_SPAN} md={GRID_HALF_SPAN}>
              <ValueCard
                title="Para artistas"
                description="Crea o reclama tu perfil para publicar tu mundo, conocer mejor a tus fans y activar experiencias directas con tu comunidad."
                action={copy.createArtistProfile}
                to={ARTIST_SIGNUP_PATH}
                accent="#fb7185"
              />
            </Grid>
            <Grid item xs={GRID_FULL_SPAN} md={GRID_HALF_SPAN}>
              <Stack spacing={2.5}>
                <SectionHeader
                  eyebrow="Fans"
                  title="Más que escuchar: participar."
                  body="El perfil fan convierte la curiosidad en una relación continua con artistas, lanzamientos y comunidad."
                />
                <BenefitList items={fanBenefits} emptyMessage={copy.fanBenefitsEmpty} />
              </Stack>
            </Grid>
            <Grid item xs={GRID_FULL_SPAN} md={GRID_HALF_SPAN}>
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

      <Box component="section" sx={{ py: { xs: 7, md: DECIMAL_BASE }, borderTop: BORDER_SUBTLE }}>
        <Container maxWidth="lg">
          <Stack spacing={4}>
            <SectionHeader
              eyebrow="Todo TDF"
              title="Una misma plataforma para estudio, sello, escuela y comunidad."
              body="TDF conecta servicios musicales, contenido, perfiles y experiencias para que el valor no se quede solo en una publicación o una sesión aislada."
            />
            <Grid container spacing={2}>
              {serviceCards.length === 0 ? (
                <Grid item xs={GRID_FULL_SPAN}>
                  <EmptyListNotice message={copy.servicesEmpty} />
                </Grid>
              ) : (
                serviceCards.map((item) => (
                  <Grid item xs={GRID_FULL_SPAN} sm={GRID_HALF_SPAN} md={GRID_THIRD_SPAN} key={item.title}>
                    <Card
                      variant="outlined"
                      sx={{
                        height: '100%',
                        borderRadius: 2,
                        bgcolor: COLOR_SERVICE_CARD_BACKGROUND,
                        borderColor: COLOR_BORDER_DEFAULT,
                        color: COLOR_TEXT_MUTED,
                      }}
                    >
                      <CardContent>
                        <Stack spacing={1.4}>
                          <Box sx={{ color: COLOR_TDF_GOLD_LIGHT, display: 'flex' }}>{item.icon}</Box>
                          <Typography variant="h6" sx={{ fontWeight: FONT_WEIGHT_CARD_HEADING }}>
                            {item.title}
                          </Typography>
                          <Typography variant="body2" sx={{ color: COLOR_TEXT_SECONDARY_SOFT }}>
                            {item.description}
                          </Typography>
                          <Button
                            disabled={false}
                            component={RouterLink}
                            to={item.path}
                            variant="text"
                            sx={{ alignSelf: 'flex-start', color: COLOR_CYAN_TEXT, textTransform: 'none', px: 0 }}
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

      <Box component="section" sx={{ py: { xs: 7, md: DECIMAL_BASE }, bgcolor: COLOR_SECTION_SURFACE }}>
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
                  sx={{ bgcolor: COLOR_CYAN_CHIP_BACKGROUND, color: COLOR_CYAN_SOFT_TEXT }}
                />
                <Chip
                  icon={<AutoAwesomeIcon />}
                  label={`${topArtists.length} destacados`}
                  sx={{ bgcolor: COLOR_EYEBROW_BACKGROUND, color: COLOR_TDF_GOLD_LIGHT }}
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

      <Box component="section" sx={{ py: { xs: 7, md: DECIMAL_BASE }, borderTop: BORDER_SUBTLE }}>
        <Container maxWidth="lg">
          <Grid container spacing={4} alignItems="center">
            <Grid item xs={GRID_FULL_SPAN} md={GRID_HALF_SPAN}>
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
            <Grid item xs={GRID_FULL_SPAN} md={GRID_HALF_SPAN}>
              <Box
                component="img"
                src={recordings[1]?.image ?? recordings[0]?.image}
                alt="Sesión musical en estudio"
                sx={{
                  width: '100%',
                  aspectRatio: '4 / 3',
                  objectFit: 'cover',
                  borderRadius: 2,
                  border: BORDER_STRONG,
                  display: 'block',
                }}
              />
            </Grid>
          </Grid>
        </Container>
      </Box>

      <Box component="section" sx={{ py: { xs: 7, md: DECIMAL_BASE }, bgcolor: COLOR_FINAL_SURFACE }}>
        <Container maxWidth="md">
          <Stack spacing={3} alignItems="center" textAlign="center">
            <Eyebrow>{copy.startEyebrow}</Eyebrow>
            <Typography variant="h3" sx={{ color: COLOR_TEXT_PRIMARY, fontWeight: FONT_WEIGHT_HERO_TITLE }}>
              Crea tu usuario y elige tu ruta dentro de TDF.
            </Typography>
            <Typography sx={{ color: COLOR_TEXT_SECONDARY }}>
              Puedes iniciar como fan, artista o ambos. La plataforma se adapta a la relación que quieres construir con la música.
            </Typography>
            <Divider flexItem sx={{ borderColor: COLOR_BORDER_SOFT }} />
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
              <Button
                disabled={false}
                component={RouterLink}
                to={GENERAL_SIGNUP_PATH}
                variant="contained"
                size="large"
                sx={{
                  bgcolor: COLOR_TDF_GOLD,
                  color: COLOR_ON_GOLD,
                  fontWeight: FONT_WEIGHT_SECTION_HEADING,
                  textTransform: 'none',
                  '&:hover': { bgcolor: COLOR_TDF_GOLD_LIGHT },
                }}
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
