import { Box, Button, Container, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import { getAnalyticsClient } from '../analytics/posthog';

/** Public exploration has no dependency on a profile, location or catalog response. */
export default function PublicWelcome() {
  const { t } = useTranslation();
  const track = (destination: '/tdf' | '/records' | '/buscar') => {
    try {
      getAnalyticsClient().capture('public_discovery_cta_clicked', {
        route: '/inicio', destination,
      });
    } catch {
      // Optional measurement must not interrupt public navigation.
    }
  };

  return (
    <Box component="section" aria-labelledby="public-welcome-title" sx={{ bgcolor: '#111b24', color: '#f8fafc' }}>
      <Container maxWidth="lg" sx={{ py: { xs: 4, md: 7 } }}>
        <Stack direction={{ xs: 'column', md: 'row' }} spacing={{ xs: 3, md: 6 }} alignItems="center">
          <Stack spacing={2.5} sx={{ flex: 1, minWidth: 0 }}>
            <Typography variant="overline" sx={{ color: '#e6c785', letterSpacing: 2 }}>
              {t('directorySearch.welcomeEyebrow')}
            </Typography>
            <Typography id="public-welcome-title" component="h1" sx={{ fontSize: { xs: '2.6rem', md: '3.8rem' }, fontWeight: 800, lineHeight: 1.08 }}>
              {t('directorySearch.welcomeTitle')}
            </Typography>
            <Typography sx={{ fontSize: '1.15rem', lineHeight: 1.7, color: '#e2e8f0' }}>
              {t('directorySearch.welcomeBody')}
            </Typography>
            <Typography sx={{ color: '#e6c785' }}>{t('directorySearch.welcomePrivacy')}</Typography>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
              <Button component={RouterLink} to="/tdf" onClick={() => track('/tdf')} variant="contained" size="large" sx={{ minHeight: 48, bgcolor: '#e6c785', color: '#111b24', '&:hover': { bgcolor: '#f0d9a8' } }}>
                {t('directorySearch.welcomeStudio')}
              </Button>
              <Button component={RouterLink} to="/records" onClick={() => track('/records')} variant="outlined" size="large" sx={{ minHeight: 48, color: '#f8fafc', borderColor: '#94a3b8' }}>
                {t('directorySearch.welcomeWork')}
              </Button>
            </Stack>
            <Button component={RouterLink} to="/buscar" onClick={() => track('/buscar')} sx={{ alignSelf: 'flex-start', color: '#a5e8ee', minHeight: 44 }}>
              {t('directorySearch.welcomeSearch')}
            </Button>
          </Stack>
          <Box component="figure" sx={{ flex: 1, m: 0, width: '100%', maxWidth: { xs: 440, md: 460 } }}>
            <Box component="img" src="/assets/tdf-ui/esteban-munoz.jpg" alt={t('directorySearch.welcomeImageAlt')} width={640} height={640} decoding="async" sx={{ display: 'block', width: '100%', height: 'auto', borderRadius: 3 }} />
            <Typography component="figcaption" sx={{ mt: 1.5, color: '#cbd5e1', lineHeight: 1.6 }}>
              {t('directorySearch.welcomeImageCaption')}
            </Typography>
          </Box>
        </Stack>
      </Container>
    </Box>
  );
}
