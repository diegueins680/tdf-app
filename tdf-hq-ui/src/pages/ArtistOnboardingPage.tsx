import { Alert, Box, Button, Card, CardContent, Chip, Stack, Typography } from '@mui/material';
import MusicNoteIcon from '@mui/icons-material/MusicNote';
import EditIcon from '@mui/icons-material/Edit';
import LinkIcon from '@mui/icons-material/Link';
import BoltIcon from '@mui/icons-material/Bolt';
import { useMemo, useState } from 'react';
import { Link as RouterLink, useSearchParams, useNavigate } from 'react-router-dom';
import { useSession, getActiveSession, SESSION_STORAGE_KEY } from '../session/SessionContext';
import { parsePositiveSafeInt } from '../utils/ids';
import { Fans } from '../api/fans';
import { get } from '../api/client';
import type { SessionResponseDTO } from '../api/session';

const buildArtistSignupLink = (claimArtistId: number | null) => {
  const params = new URLSearchParams();
  params.set('signup', '1');
  params.set('intent', 'artist_profile');
  const normalizedClaimArtistId = parsePositiveSafeInt(claimArtistId);
  if (normalizedClaimArtistId !== null) {
    params.set('claimArtistId', String(normalizedClaimArtistId));
  }
  return `/login?${params.toString()}`;
};

const buildArtistLoginLink = (claimArtistId: number | null) => {
  const params = new URLSearchParams();
  params.set('redirect', claimArtistId === null ? '/mi-artista' : `/artista/crear?claimArtistId=${claimArtistId}`);
  return `/login?${params.toString()}`;
};

export default function ArtistOnboardingPage() {
  const { session, login, logout } = useSession();
  const navigate = useNavigate();
  const [activating, setActivating] = useState(false);
  const [activationError, setActivationError] = useState<string | null>(null);

  const activateProfile = async () => {
    if (!session?.partyId || activating || claimArtistId !== null) return;
    const partyId = session.partyId;
    setActivating(true);
    setActivationError(null);
    try {
      await Fans.activateMyArtistProfile();
      const refreshed = await get<SessionResponseDTO>('/session');
      if (getActiveSession()?.partyId !== partyId || refreshed.partyId !== partyId) return;
      login({ ...refreshed, apiToken: session.apiToken }, {
        remember: !window.sessionStorage.getItem(SESSION_STORAGE_KEY),
      });
      navigate('/mi-artista');
    } catch (error) {
      setActivationError(error instanceof Error ? error.message : 'No pudimos crear tu perfil. Inténtalo de nuevo.');
    } finally {
      setActivating(false);
    }
  };
  const [searchParams] = useSearchParams();

  const claimArtistId = useMemo(() => {
    const raw = searchParams.get('claimArtistId') ?? searchParams.get('claim');
    return parsePositiveSafeInt(raw);
  }, [searchParams]);

  const continueClaim = () => {
    if (claimArtistId === null) return;
    logout();
    navigate(buildArtistSignupLink(claimArtistId));
  };

  const hasArtistRole = useMemo(() => {
    const roles = session?.roles ?? [];
    return roles.some((role) => {
      const r = role.toLowerCase();
      return r === 'artist' || r === 'artista' || r === 'admin';
    });
  }, [session?.roles]);

  return (
    <Box sx={{ maxWidth: 980, mx: 'auto' }}>
      <Stack spacing={3}>
        <Box
          sx={{
            borderRadius: 4,
            p: { xs: 3, md: 4 },
            color: '#e2e8f0',
            overflow: 'hidden',
            position: 'relative',
            background:
              'linear-gradient(135deg, rgba(15,23,42,1) 0%, rgba(2,6,23,1) 55%, rgba(15,23,42,1) 100%)',
            border: '1px solid rgba(148,163,184,0.18)',
          }}
        >
          <Box
            sx={{
              position: 'absolute',
              inset: 0,
              background:
                'radial-gradient(40% 60% at 12% 30%, rgba(56,189,248,0.18), transparent 62%), radial-gradient(42% 46% at 78% 18%, rgba(167,139,250,0.18), transparent 58%), radial-gradient(40% 46% at 58% 92%, rgba(34,197,94,0.12), transparent 60%)',
              pointerEvents: 'none',
            }}
          />
          <Stack spacing={1.25} sx={{ position: 'relative' }}>
            <Chip
              icon={<MusicNoteIcon />}
              label="TDF · Artistas"
              sx={{
                width: 'fit-content',
                bgcolor: 'rgba(148,163,184,0.14)',
                borderColor: 'rgba(148,163,184,0.28)',
                color: '#e2e8f0',
                fontWeight: 700,
              }}
              variant="outlined"
            />
            <Typography variant="h3" fontWeight={900}>
              Crea tu perfil de artista
            </Typography>
            <Typography sx={{ color: 'rgba(226,232,240,0.82)' }}>
              En minutos: registra tu cuenta, completa tu bio y comparte tu link público.
            </Typography>
            {claimArtistId && (
              <Alert severity="info" sx={{ mt: 2, bgcolor: 'rgba(30,41,59,0.65)', color: '#e2e8f0' }}>
                Vamos a ayudarte a reclamar el perfil #{claimArtistId}.
              </Alert>
            )}
          </Stack>
        </Box>

        {session?.partyId && claimArtistId !== null && (
          <Alert severity="info">
            Para reclamar este perfil, registra su cuenta con el correo asociado al artista.
            Al continuar se cerrará tu sesión actual y se conservará el perfil seleccionado.
          </Alert>
        )}

        {session?.partyId && claimArtistId === null && (
          <Alert
            severity={hasArtistRole ? 'success' : 'info'}
            action={
              hasArtistRole ? (
                <Button color="inherit" size="small" component={RouterLink} to="/mi-artista">
                  Ir a mi perfil
                </Button>
              ) : (
                <Button color="inherit" size="small" onClick={() => void activateProfile()} disabled={activating}>
                  {activating ? 'Creando…' : 'Crear mi perfil'}
                </Button>
              )
            }
          >
            {hasArtistRole
              ? 'Ya tienes una sesión activa. Puedes editar tu perfil de artista ahora.'
              : 'Tu cuenta está activa. Crea tu perfil de artista ahora, sin esperar aprobación.'}
          </Alert>
        )}

        {activationError && <Alert severity="error">{activationError}</Alert>}

        <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
          <Card variant="outlined" sx={{ flex: 1, borderRadius: 3 }}>
            <CardContent>
              <Stack spacing={2}>
                <Stack direction="row" spacing={1.5} alignItems="center">
                  <BoltIcon color="warning" />
                  <Typography variant="h6" fontWeight={800}>
                    Paso a paso
                  </Typography>
                </Stack>
                <Stack direction="row" spacing={1} flexWrap="wrap">
                  <Chip icon={<MusicNoteIcon />} label="1) Crear cuenta" variant="outlined" />
                  <Chip icon={<EditIcon />} label="2) Completar perfil" variant="outlined" />
                  <Chip icon={<LinkIcon />} label="3) Compartir link" variant="outlined" />
                </Stack>
                <Typography variant="body2" color="text.secondary">
                  Te llevamos directo al portal para completar tu perfil y publicarlo en tu URL.
                </Typography>
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.25}>
                  {session?.partyId && claimArtistId !== null ? (
                    <Button variant="contained" size="large" onClick={continueClaim}>
                      Cerrar sesión y reclamar perfil
                    </Button>
                  ) : session?.partyId ? (
                    <Button variant="contained" size="large" disabled={activating}
                      onClick={() => hasArtistRole ? navigate('/mi-artista') : void activateProfile()}>
                      {activating ? 'Creando…' : hasArtistRole ? 'Editar mi perfil de artista' : 'Crear mi perfil de artista'}
                    </Button>
                  ) : (
                    <Button variant="contained" size="large" component={RouterLink}
                      to={buildArtistSignupLink(claimArtistId)} sx={{ textTransform: 'none' }}>
                      Crear mi perfil de artista
                    </Button>
                  )}
                  {!session?.partyId && <Button
                    variant="outlined"
                    size="large"
                    component={RouterLink}
                    to={buildArtistLoginLink(claimArtistId)}
                    sx={{ textTransform: 'none' }}
                  >
                    Ya tengo cuenta
                  </Button>}
                </Stack>
              </Stack>
            </CardContent>
          </Card>

          <Card variant="outlined" sx={{ flex: 1, borderRadius: 3 }}>
            <CardContent>
              <Stack spacing={2}>
                <Typography variant="h6" fontWeight={800}>
                  ¿Qué vas a poder editar?
                </Typography>
                <Stack spacing={1}>
                  <Typography variant="body2" color="text.secondary">
                    - Nombre artístico y ciudad
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    - Bio, géneros, links (Spotify/YouTube/sitio)
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    - Portada principal (Drive) y video destacado
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    - URL pública (slug)
                  </Typography>
                </Stack>
                <Typography variant="body2" color="text.secondary">
                  Si tu perfil ya existe en TDF pero aún no tiene usuario, selecciona “Reclamar perfil” durante el registro.
                </Typography>
              </Stack>
            </CardContent>
          </Card>
        </Stack>
      </Stack>
    </Box>
  );
}
