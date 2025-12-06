import { useEffect, useMemo } from 'react';
import { useSearchParams } from 'react-router-dom';
import { Alert, Box, Chip, Container, Paper, Stack, Typography } from '@mui/material';
import PublicBrandBar from '../components/PublicBrandBar';
import { LiveSessionIntakeForm } from './LiveSessionIntakePage';
import { useSession } from '../session/SessionContext';

export default function LiveSessionPublicPage() {
  const [sp] = useSearchParams();
  const tokenFromQuery = sp.get('token') ?? sp.get('t') ?? '';
  const fallbackToken = import.meta.env.VITE_LIVE_SESSIONS_PUBLIC_TOKEN ?? '';
  const token = useMemo(() => tokenFromQuery || fallbackToken, [tokenFromQuery, fallbackToken]);
  const { session, login, setApiToken } = useSession();

  useEffect(() => {
    if (!token) return;
    if (!session) {
      login(
        {
          username: 'live-session-intake',
          displayName: 'Live Sessions Intake',
          roles: [],
          apiToken: token,
        },
        { remember: false },
      );
    } else if (!session.apiToken) {
      setApiToken(token);
    }
  }, [token, session, login, setApiToken]);

  return (
    <Box
      sx={{
        minHeight: '100vh',
        background: 'radial-gradient(circle at 20% 20%, rgba(56,189,248,0.12), transparent 25%), radial-gradient(circle at 80% 0%, rgba(167,139,250,0.12), transparent 26%), linear-gradient(135deg, #0b1224, #0f172a)',
        color: '#e2e8f0',
        py: { xs: 4, md: 6 },
      }}
    >
      <Container maxWidth="lg">
        <Stack spacing={3}>
          <Box sx={{ display: 'flex', justifyContent: 'center' }}>
            <PublicBrandBar tagline="Live Sessions · Intake" compact />
          </Box>
          <Paper
            elevation={0}
            sx={{
              p: { xs: 3, md: 4 },
              bgcolor: 'rgba(15,23,42,0.82)',
              border: '1px solid rgba(255,255,255,0.08)',
              backdropFilter: 'blur(12px)',
              color: '#e2e8f0',
            }}
          >
            <Stack spacing={2}>
              <Stack spacing={1}>
                <Chip
                  label="Aplicación banda"
                  sx={{
                    bgcolor: 'rgba(255,255,255,0.08)',
                    color: '#cbd5f5',
                    border: '1px solid rgba(255,255,255,0.14)',
                    alignSelf: 'flex-start',
                  }}
                  size="small"
                />
                <Typography variant="h4" fontWeight={800}>
                  Sube tu banda a las TDF Live Sessions
                </Typography>
                <Typography variant="body1" color="rgba(226,232,240,0.85)">
                  Comparte quiénes tocan, tus redes y la disponibilidad tentativa. Si ya trabajamos juntos, busca los nombres para autocompletar los datos.
                </Typography>
              </Stack>
              {!token && (
                <Alert severity="warning" sx={{ bgcolor: 'rgba(251,191,36,0.12)', color: '#fef08a' }}>
                  Agrega el token de intake en la URL (?token=xxx) o define VITE_LIVE_SESSIONS_PUBLIC_TOKEN para poder enviar el formulario.
                </Alert>
              )}
              <LiveSessionIntakeForm variant="public" requireTerms />
            </Stack>
          </Paper>
        </Stack>
      </Container>
    </Box>
  );
}
