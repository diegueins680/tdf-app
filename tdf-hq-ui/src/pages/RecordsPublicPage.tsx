import {
  Avatar,
  Box,
  Button,
  Card,
  CardContent,
  CardMedia,
  Chip,
  Container,
  Grid,
  Link as MuiLink,
  Stack,
  Typography,
} from '@mui/material';
import { recordings, releases, sessionVideos } from '../constants/recordsContent';

const SectionTitle = ({ title, kicker }: { title: string; kicker?: string }) => (
  <Stack spacing={1} direction="row" alignItems="center" sx={{ mb: 2 }}>
    {kicker && (
      <Chip
        label={kicker}
        size="small"
        sx={{ bgcolor: 'rgba(255,255,255,0.12)', color: 'primary.contrastText', fontWeight: 600 }}
      />
    )}
    <Typography variant="h4" sx={{ fontWeight: 800, letterSpacing: '-0.02em' }}>
      {title}
    </Typography>
  </Stack>
);

const GradientCard = ({
  title,
  children,
}: {
  title: string;
  children: React.ReactNode;
}) => (
  <Box
    sx={{
      p: 3,
      borderRadius: 3,
      bgcolor: 'rgba(255,255,255,0.02)',
      border: '1px solid rgba(255,255,255,0.08)',
      boxShadow: '0 30px 60px rgba(0,0,0,0.35)',
    }}
  >
    <Typography variant="h6" sx={{ fontWeight: 700, mb: 2 }}>
      {title}
    </Typography>
    {children}
  </Box>
);

const RecordingsGrid = () => (
  <Grid container spacing={3}>
    {recordings.map((item) => (
      <Grid item key={item.title} xs={12} md={4}>
        <Card
          sx={{
            height: '100%',
            bgcolor: 'rgba(255,255,255,0.03)',
            border: '1px solid rgba(255,255,255,0.08)',
            borderRadius: 3,
            overflow: 'hidden',
            display: 'flex',
            flexDirection: 'column',
          }}
        >
          <CardMedia
            component="div"
            sx={{
              pt: '60%',
              backgroundImage: `linear-gradient(180deg, rgba(0,0,0,0.1), rgba(0,0,0,0.35)), url(${item.image})`,
              backgroundSize: 'cover',
              backgroundPosition: 'center',
            }}
          />
          <CardContent sx={{ flexGrow: 1, display: 'flex', flexDirection: 'column', gap: 1.5 }}>
            <Stack direction="row" spacing={1} alignItems="center">
              <Chip label={item.vibe} size="small" sx={{ bgcolor: 'rgba(255,255,255,0.08)' }} />
              <Typography variant="caption" sx={{ color: 'text.secondary' }}>
                {item.recordedAt}
              </Typography>
            </Stack>
            <Typography variant="h6" sx={{ fontWeight: 800, letterSpacing: '-0.01em' }}>
              {item.title}
            </Typography>
            <Typography variant="subtitle2" sx={{ color: 'text.secondary', fontWeight: 700 }}>
              {item.artist}
            </Typography>
            <Typography variant="body2" sx={{ color: 'text.secondary' }}>
              {item.description}
            </Typography>
          </CardContent>
        </Card>
      </Grid>
    ))}
  </Grid>
);

const ReleasesGrid = () => (
  <Grid container spacing={3}>
    {releases.map((release) => (
      <Grid item xs={12} md={6} key={release.title}>
        <Card
          sx={{
            display: 'flex',
            flexDirection: { xs: 'column', sm: 'row' },
            gap: 2,
            bgcolor: 'rgba(255,255,255,0.03)',
            border: '1px solid rgba(255,255,255,0.08)',
            borderRadius: 3,
            overflow: 'hidden',
          }}
        >
          <Box
            sx={{
              width: { xs: '100%', sm: 200 },
              minHeight: 200,
              backgroundImage: `linear-gradient(180deg, rgba(0,0,0,0.1), rgba(0,0,0,0.35)), url(${release.cover})`,
              backgroundSize: 'cover',
              backgroundPosition: 'center',
              borderRight: { sm: '1px solid rgba(255,255,255,0.06)' },
            }}
          />
          <CardContent sx={{ flex: 1, display: 'flex', flexDirection: 'column', gap: 1.5 }}>
            <Stack direction="row" spacing={1} alignItems="center">
              <Chip label="Release" size="small" sx={{ bgcolor: 'rgba(255,255,255,0.08)' }} />
              <Typography variant="caption" sx={{ color: 'text.secondary' }}>
                {release.releasedOn}
              </Typography>
            </Stack>
            <Typography variant="h6" sx={{ fontWeight: 800 }}>
              {release.title}
            </Typography>
            <Typography variant="subtitle2" sx={{ color: 'text.secondary', fontWeight: 700 }}>
              {release.artist}
            </Typography>
            <Typography variant="body2" sx={{ color: 'text.secondary' }}>
              {release.blurb}
            </Typography>
            <Stack direction="row" spacing={1} flexWrap="wrap">
              {release.links.map((link) => (
                <Button
                  key={link.platform}
                  component={MuiLink}
                  href={link.url}
                  target="_blank"
                  rel="noopener noreferrer"
                  variant="contained"
                  size="small"
                  sx={{
                    textTransform: 'none',
                    bgcolor: link.accent,
                    color: '#0b0d12',
                    fontWeight: 700,
                    '&:hover': { opacity: 0.9, bgcolor: link.accent },
                  }}
                >
                  {link.platform}
                </Button>
              ))}
            </Stack>
          </CardContent>
        </Card>
      </Grid>
    ))}
  </Grid>
);

const SessionsGrid = () => (
  <Grid container spacing={3}>
    {sessionVideos.map((video) => (
      <Grid item xs={12} md={4} key={video.youtubeId}>
        <Card
          sx={{
            height: '100%',
            bgcolor: 'rgba(255,255,255,0.03)',
            border: '1px solid rgba(255,255,255,0.08)',
            borderRadius: 3,
            overflow: 'hidden',
            display: 'flex',
            flexDirection: 'column',
          }}
        >
          <Box sx={{ position: 'relative', pt: '56.25%', backgroundColor: '#0f1117' }}>
            <Box
              component="iframe"
              src={`https://www.youtube.com/embed/${video.youtubeId}`}
              title={video.title}
              allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
              allowFullScreen
              sx={{
                position: 'absolute',
                inset: 0,
                width: '100%',
                height: '100%',
                border: 0,
              }}
            />
          </Box>
          <CardContent sx={{ flexGrow: 1, display: 'flex', flexDirection: 'column', gap: 1 }}>
            <Stack direction="row" spacing={1} alignItems="center">
              <Chip label="TDF Session" size="small" sx={{ bgcolor: 'rgba(255,255,255,0.08)' }} />
              <Typography variant="caption" sx={{ color: 'text.secondary' }}>
                {video.duration}
              </Typography>
            </Stack>
            <Typography variant="h6" sx={{ fontWeight: 800 }}>
              {video.title}
            </Typography>
            <Typography variant="subtitle2" sx={{ color: 'text.secondary', fontWeight: 700 }}>
              {video.guests}
            </Typography>
            <Typography variant="body2" sx={{ color: 'text.secondary' }}>
              {video.description}
            </Typography>
          </CardContent>
        </Card>
      </Grid>
    ))}
  </Grid>
);

export default function RecordsPublicPage() {
  return (
    <Box
      sx={{
        minHeight: '100vh',
        bgcolor: '#07090f',
        color: '#e5e7eb',
        background:
          'radial-gradient(circle at 20% 20%, rgba(120,119,198,0.18), transparent 25%), radial-gradient(circle at 80% 0%, rgba(45,212,191,0.16), transparent 25%)',
      }}
    >
      <Box
        sx={{
          position: 'relative',
          overflow: 'hidden',
          borderBottom: '1px solid rgba(255,255,255,0.06)',
          background:
            'linear-gradient(135deg, rgba(12,18,28,0.9), rgba(12,18,28,0.6)), url(https://images.unsplash.com/photo-1483412033650-1015ddeb83d1?auto=format&fit=crop&w=1800&q=80) center/cover',
        }}
      >
        <Container maxWidth="lg" sx={{ py: { xs: 8, md: 12 } }}>
          <Stack spacing={3} maxWidth="md">
            <Chip
              label="TDF Records — Public CMS"
              sx={{
                width: 'fit-content',
                bgcolor: 'rgba(255,255,255,0.08)',
                color: '#e5e7eb',
                fontWeight: 700,
              }}
            />
            <Typography
              variant="h2"
              sx={{ fontWeight: 900, letterSpacing: '-0.03em', lineHeight: 1.05 }}
            >
              Historias desde el estudio, releases y TDF Sessions en un solo lugar.
            </Typography>
            <Typography variant="h6" sx={{ color: 'rgba(226,232,240,0.82)', maxWidth: 640 }}>
              Mantén al día la página pública de TDF Records con fotos, lanzamientos y videos curados
              desde este CMS mínimo. Edita el contenido en un solo archivo y publícalo en minutos.
            </Typography>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
              <Button
                variant="contained"
                size="large"
                href="https://wa.me/573135205493"
                sx={{
                  bgcolor: '#7c3aed',
                  color: '#f8fafc',
                  fontWeight: 800,
                  px: 3,
                  textTransform: 'none',
                  '&:hover': { bgcolor: '#6d28d9' },
                }}
              >
                Reservar sesión
              </Button>
              <Button
                variant="outlined"
                size="large"
                href="#releases"
                sx={{
                  borderColor: 'rgba(255,255,255,0.3)',
                  color: '#e5e7eb',
                  textTransform: 'none',
                }}
              >
                Ver lanzamientos
              </Button>
            </Stack>
          </Stack>
          <Stack
            direction={{ xs: 'column', md: 'row' }}
            spacing={3}
            sx={{ mt: 6, flexWrap: 'wrap' }}
          >
            <GradientCard title="Fotos del estudio">
              <Typography variant="body2" sx={{ color: 'text.secondary', mb: 1 }}>
                Actualiza imágenes en <code>src/constants/recordsContent.ts</code>.
              </Typography>
              <Stack direction="row" spacing={1} alignItems="center">
                {recordings.slice(0, 3).map((item) => (
                  <Avatar
                    key={item.title}
                    alt={item.title}
                    src={item.image}
                    sx={{ width: 40, height: 40, border: '2px solid rgba(255,255,255,0.2)' }}
                  />
                ))}
              </Stack>
            </GradientCard>
            <GradientCard title="TDF Sessions">
              <Stack spacing={1}>
                {sessionVideos.slice(0, 2).map((video) => (
                  <Stack key={video.youtubeId} direction="row" spacing={1} alignItems="center">
                    <Chip label="Video" size="small" sx={{ bgcolor: 'rgba(255,255,255,0.08)' }} />
                    <Typography variant="body2" sx={{ fontWeight: 700 }}>
                      {video.title}
                    </Typography>
                  </Stack>
                ))}
              </Stack>
            </GradientCard>
            <GradientCard title="Releases">
              <Stack spacing={1}>
                {releases.map((release) => (
                  <Typography key={release.title} variant="body2" sx={{ fontWeight: 700 }}>
                    {release.title} · {release.artist}
                  </Typography>
                ))}
              </Stack>
            </GradientCard>
          </Stack>
        </Container>
      </Box>

      <Container maxWidth="lg" sx={{ py: { xs: 6, md: 10 } }}>
        <Box sx={{ mb: 6 }}>
          <SectionTitle title="Grabaciones recientes" kicker="Estudio" />
          <Typography variant="body1" sx={{ color: 'text.secondary', maxWidth: 760, mb: 3 }}>
            Mantén este grid con las sesiones más frescas: toma la foto hero, fecha y un párrafo
            corto sobre cómo se grabó. Ideal para mostrar el sonido de TDF.
          </Typography>
          <RecordingsGrid />
        </Box>

        <Box id="releases" sx={{ mb: 6 }}>
          <SectionTitle title="Lanzamientos TDF Records" kicker="Label" />
          <Typography variant="body1" sx={{ color: 'text.secondary', maxWidth: 760, mb: 3 }}>
            Cada release incluye los links oficiales. Edita los enlaces a plataformas en el mismo
            archivo de contenido.
          </Typography>
          <ReleasesGrid />
        </Box>

        <Box sx={{ mb: 6 }}>
          <SectionTitle title="TDF Sessions" kicker="YouTube" />
          <Typography variant="body1" sx={{ color: 'text.secondary', maxWidth: 760, mb: 3 }}>
            Inserta el ID de YouTube en la lista de sesiones para embeberlo aquí. Ideal para
            compartir en la página pública sin esfuerzo.
          </Typography>
          <SessionsGrid />
        </Box>

        <GradientCard title="Cómo actualizar este CMS">
          <Typography variant="body2" sx={{ color: 'text.secondary', mb: 1 }}>
            Edita el archivo <code>src/constants/recordsContent.ts</code> para cambiar fotos,
            descripciones, links y videos. Al desplegar, la página pública en /records se actualiza
            automáticamente.
          </Typography>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} sx={{ mt: 2 }}>
            <Button
              variant="contained"
              href="https://github.com/diegueins680/tdf-app/tree/main/tdf-hq-ui/src/constants/recordsContent.ts"
              target="_blank"
              rel="noopener noreferrer"
              sx={{ textTransform: 'none', fontWeight: 800 }}
            >
              Abrir contenido
            </Button>
            <Button
              variant="outlined"
              href="mailto:bookings@tdfrecords.studio"
              sx={{ textTransform: 'none', borderColor: 'rgba(255,255,255,0.3)', color: '#e5e7eb' }}
            >
              Hablar con el estudio
            </Button>
          </Stack>
        </GradientCard>
      </Container>
    </Box>
  );
}
