import { Box, Button, Chip, Stack, Typography } from '@mui/material';
import EventAvailableIcon from '@mui/icons-material/EventAvailable';
import InstagramIcon from '@mui/icons-material/Instagram';
import MusicNoteIcon from '@mui/icons-material/MusicNote';
import SchoolIcon from '@mui/icons-material/School';
import { Link as RouterLink } from 'react-router-dom';
import { COURSE_COHORTS, COURSE_DEFAULTS } from '../config/appConfig';

interface InstagramLink {
  label: string;
  to: string;
}

const MONTH_LABELS: Record<string, string> = {
  ene: 'Ene',
  feb: 'Feb',
  mar: 'Mar',
  abr: 'Abr',
  may: 'May',
  jun: 'Jun',
  jul: 'Jul',
  ago: 'Ago',
  sep: 'Sep',
  oct: 'Oct',
  nov: 'Nov',
  dic: 'Dic',
};

const withInstagramParams = (path: string): string => {
  const [pathAndSearch, hash] = path.split('#');
  const [pathname, rawSearch] = (pathAndSearch ?? path).split('?');
  const params = new URLSearchParams(rawSearch ?? '');
  if (!params.has('utm_source')) params.set('utm_source', 'instagram');
  if (!params.has('utm_medium')) params.set('utm_medium', 'social');
  if (!params.has('utm_campaign')) params.set('utm_campaign', 'instagram_public_links');
  const query = params.toString();
  const normalizedPathname = pathname === undefined || pathname === '' ? '/' : pathname;
  return `${normalizedPathname}${query ? `?${query}` : ''}${hash ? `#${hash}` : ''}`;
};

const formatCourseSlugLabel = (slug: string): string => {
  if (slug === 'produccion-musical') return 'Producción musical';
  const productionMatch = /^produccion-musical-([a-z]{3})-(\d{4})$/.exec(slug);
  if (productionMatch) {
    const month = productionMatch[1] ?? '';
    const year = productionMatch[2] ?? '';
    return `Producción musical ${MONTH_LABELS[month] ?? month} ${year}`;
  }
  return slug
    .split('-')
    .filter(Boolean)
    .map((part) => part.charAt(0).toUpperCase() + part.slice(1))
    .join(' ');
};

const buildCourseLinks = (): InstagramLink[] => {
  const configuredSlugs = Array.from(
    new Set([COURSE_DEFAULTS.slug, ...COURSE_COHORTS].map((slug) => slug.trim()).filter(Boolean)),
  );
  const directCourseLinks = configuredSlugs.slice(0, 3).map((slug) => ({
    label: formatCourseSlugLabel(slug),
    to: withInstagramParams(`/curso/${encodeURIComponent(slug)}`),
  }));
  return [
    { label: 'Cursos de producción', to: withInstagramParams('/curso/produccion-musical') },
    { label: 'Clases de prueba', to: withInstagramParams('/trials') },
    ...directCourseLinks,
  ];
};

const serviceLinks: InstagramLink[] = [
  { label: 'Reservar servicios', to: withInstagramParams('/reservar') },
  { label: 'DJ Booth', to: withInstagramParams('/dj-booth') },
];

const courseLinks = buildCourseLinks();

export default function InstagramEntryLinks() {
  return (
    <Box
      aria-label="Opciones para visitantes desde Instagram"
      sx={{
        mb: { xs: 2, md: 3 },
        p: { xs: 2, md: 2.5 },
        border: '1px solid',
        borderColor: 'divider',
        borderRadius: 2,
        bgcolor: 'background.paper',
        background: (theme) =>
          theme.palette.mode === 'dark'
            ? 'linear-gradient(135deg, rgba(225,29,72,0.12), rgba(20,184,166,0.1)), linear-gradient(0deg, rgba(18,18,26,0.96), rgba(18,18,26,0.96))'
            : 'linear-gradient(135deg, rgba(225,29,72,0.07), rgba(20,184,166,0.07)), linear-gradient(0deg, rgba(255,255,255,0.96), rgba(255,255,255,0.96))',
      }}
    >
      <Stack spacing={1.5}>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5} alignItems={{ xs: 'flex-start', sm: 'center' }}>
          <Chip
            icon={<InstagramIcon />}
            label="TDF desde Instagram"
            size="small"
            sx={{ bgcolor: 'rgba(225,29,72,0.1)', color: 'text.primary', fontWeight: 700 }}
          />
          <Typography variant="body2" color="text.secondary" sx={{ maxWidth: 720 }}>
            Reservas, cursos y clases en un solo lugar.
          </Typography>
        </Stack>
        <Stack direction={{ xs: 'column', md: 'row' }} spacing={1} flexWrap="wrap" useFlexGap>
          {serviceLinks.map((link, index) => (
            <Button
              key={link.to}
              component={RouterLink}
              to={link.to}
              variant={index === 0 ? 'contained' : 'outlined'}
              startIcon={index === 0 ? <EventAvailableIcon /> : <MusicNoteIcon />}
              sx={{ textTransform: 'none', alignSelf: { xs: 'stretch', md: 'flex-start' } }}
            >
              {link.label}
            </Button>
          ))}
          {courseLinks.map((link, index) => (
            <Button
              key={link.to}
              component={RouterLink}
              to={link.to}
              variant="outlined"
              startIcon={<SchoolIcon />}
              sx={{ textTransform: 'none', alignSelf: { xs: 'stretch', md: 'flex-start' } }}
            >
              {index === 0 ? 'Inscribirme: cursos' : link.label}
            </Button>
          ))}
        </Stack>
      </Stack>
    </Box>
  );
}
