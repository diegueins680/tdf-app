import { useMemo, useState, type FormEvent } from 'react';
import {
  Alert,
  Box,
  Button,
  Checkbox,
  Chip,
  Container,
  Fab,
  FormControlLabel,
  Link,
  Paper,
  Stack,
  Tab,
  Tabs,
  TextField,
  Typography,
  type ChipProps,
} from '@mui/material';
import { useMutation, useQuery } from '@tanstack/react-query';
import DarkModeIcon from '@mui/icons-material/DarkMode';
import LightModeIcon from '@mui/icons-material/LightMode';
import { Navigate, useNavigate } from 'react-router-dom';
import { useSession } from '../session/SessionContext';
import { Meta } from '../api/meta';
import { useThemeMode } from '../theme/AppThemeProvider';
import { loginRequest } from '../api/auth';

type LoginTab = 'password' | 'token';

export default function LoginPage() {
  const [username, setUsername] = useState('');
  const [password, setPassword] = useState('');
  const [tokenValue, setTokenValue] = useState('');
  const [tab, setTab] = useState<LoginTab>('password');
  const [rememberDevice, setRememberDevice] = useState(true);
  const [formError, setFormError] = useState<string | null>(null);

  const { session, login } = useSession();
  const navigate = useNavigate();
  const { mode, toggleMode } = useThemeMode();
  const loginMutation = useMutation({
    mutationFn: (payload: { username: string; password: string }) => loginRequest(payload),
  });

  const {
    data: health,
    isFetching: healthLoading,
    error: healthError,
  } = useQuery({
    queryKey: ['meta', 'health'],
    queryFn: Meta.health,
    retry: false,
    refetchInterval: 30000,
    refetchOnWindowFocus: false,
  });

  const apiStatus = useMemo<{ label: string; color: ChipProps['color'] }>(() => {
    if (healthLoading) {
      return { label: 'API: verificando...', color: 'default' };
    }
    if (health) {
      const healthy = (health.status ?? '').toLowerCase() === 'ok';
      return { label: `API: ${health.status}`, color: healthy ? 'success' : 'warning' };
    }
    if (healthError) {
      return { label: 'API: offline', color: 'error' };
    }
    return { label: 'API: offline', color: 'error' };
  }, [health, healthError, healthLoading]);

  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setFormError(null);

    const normalizedUsername =
      tab === 'password'
        ? username.trim()
        : (() => {
            const tokenSnippet = tokenValue.trim().slice(0, 8);
            const suffix = tokenSnippet === '' ? 'usuario' : tokenSnippet;
            return `token:${suffix}`;
          })();

    const displayName =
      normalizedUsername.charAt(0).toUpperCase() + normalizedUsername.slice(1);

    try {
      let apiToken: string | null = null;
      let roles: string[] = [];
      let modules: string[] | undefined;
      let partyId: number | undefined;

      if (tab === 'password') {
        if (!username.trim() || !password.trim()) {
          setFormError('Por favor completa usuario y contraseña.');
          return;
        }
        const response = await loginMutation.mutateAsync({
          username: username.trim(),
          password: password.trim(),
        });
        apiToken = response.token;
        roles = response.roles?.map((role) => role.toLowerCase()) ?? [];
        modules = response.modules;
        partyId = response.partyId;
      } else {
        if (!tokenValue.trim()) {
          setFormError('Ingresa tu token API.');
          return;
        }
        apiToken = tokenValue.trim();
        roles = ['token'];
      }

      login(
        {
          username: normalizedUsername,
          displayName,
          roles: roles.length ? roles : normalizedUsername.toLowerCase().includes('admin') ? ['admin'] : ['staff'],
          apiToken,
          modules,
          partyId,
        },
        { remember: rememberDevice },
      );
      navigate('/crm/contactos', { replace: true });
    } catch (error) {
      const message = error instanceof Error ? error.message : 'No se pudo iniciar sesión.';
      setFormError(message.trim() === '' ? 'No se pudo iniciar sesión.' : message);
    }
  };

  if (session) {
    return <Navigate to="/crm/contactos" replace />;
  }

  return (
    <Box
      component="main"
      sx={{
        minHeight: '100vh',
        bgcolor: (theme) => theme.palette.background.default,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        p: { xs: 2, sm: 4 },
      }}
    >
      <Container maxWidth="sm" sx={{ display: 'flex', justifyContent: 'center' }}>
        <Paper
          component="form"
          elevation={6}
          onSubmit={(event) => {
            void handleSubmit(event);
          }}
          sx={{
            width: '100%',
            maxWidth: 420,
            p: { xs: 3, sm: 4 },
            borderRadius: 4,
          }}
        >
          <Stack spacing={3}>
            <Stack spacing={1} textAlign="center">
              <Typography variant="overline" fontWeight={700} letterSpacing={6}>
                TDF RECORDS
              </Typography>
              <Typography variant="caption" color="text.secondary">
                RECORDS & STUDIO
              </Typography>
            </Stack>

            <Stack spacing={1}>
              <Typography variant="h5" fontWeight={600}>
                Iniciar sesión
              </Typography>
              <Typography variant="body2" color="text.secondary">
                Usa tus credenciales o un token emitido por TDF Records.
              </Typography>
            </Stack>

            <Tabs
              value={tab}
              onChange={(_, value) => setTab(value as LoginTab)}
              variant="fullWidth"
            >
              <Tab value="password" label="CONTRASEÑA" />
              <Tab value="token" label="TOKEN API" />
            </Tabs>

            {tab === 'password' ? (
              <Stack spacing={2}>
                <TextField
                  label="Usuario *"
                  type="text"
                  value={username}
                  onChange={(event) => setUsername(event.target.value)}
                  fullWidth
                  autoComplete="username"
                />

                <TextField
                  label="Contraseña *"
                  type="password"
                  value={password}
                  onChange={(event) => setPassword(event.target.value)}
                  fullWidth
                  autoComplete="current-password"
                />
              </Stack>
            ) : (
              <Stack spacing={2}>
                <TextField
                  label="Token API *"
                  value={tokenValue}
                  onChange={(event) => setTokenValue(event.target.value)}
                  fullWidth
                  placeholder="tdf_xxxx-xxxx"
                />
                <Typography variant="caption" color="text.secondary">
                  Inserta el token temporal asignado por el equipo de operaciones. Caduca en 24 horas.
                </Typography>
              </Stack>
            )}

            <FormControlLabel
              control={
                <Checkbox
                  checked={rememberDevice}
                  onChange={(event) => setRememberDevice(event.target.checked)}
                />
              }
              label="Recordarme en este dispositivo"
            />

            {formError && <Alert severity="warning">{formError}</Alert>}

            <Button variant="contained" type="submit" size="large" disabled={loginMutation.isPending}>
              {loginMutation.isPending ? 'Ingresando…' : 'Ingresar'}
            </Button>

            <Stack spacing={0.5} textAlign="center">
              <Typography variant="body2">
                ¿Olvidaste tu contraseña?{' '}
                <Link href="#" underline="hover">
                  Recuperar acceso
                </Link>
              </Typography>
              <Typography variant="body2">
                ¿No tienes cuenta?{' '}
                <Link href="#" underline="hover">
                  Crear cuenta
                </Link>
              </Typography>
              <Typography variant="body2">
                ¿Buscas una clase de prueba?{' '}
                <Link href="#" underline="hover">
                  Solicitar trial
                </Link>
              </Typography>
            </Stack>
          </Stack>
        </Paper>
      </Container>

      <Box
        sx={{
          position: 'fixed',
          right: 24,
          bottom: 24,
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'flex-end',
          gap: 1,
        }}
      >
        <Fab
          color="primary"
          size="medium"
          onClick={toggleMode}
          aria-label="Cambiar tema"
        >
          {mode === 'light' ? <DarkModeIcon /> : <LightModeIcon />}
        </Fab>
        <Chip label={apiStatus.label} color={apiStatus.color} size="small" />
      </Box>
    </Box>
  );
}
