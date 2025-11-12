import { useMemo, useState, type FormEvent } from 'react';
import {
  Alert,
  Box,
  Button,
  Checkbox,
  Chip,
  Container,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
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
import { loginRequest, requestPasswordReset } from '../api/auth';
import BrandLogo from '../components/BrandLogo';

type LoginTab = 'password' | 'token';

export default function LoginPage() {
  const [identifier, setIdentifier] = useState('');
  const [password, setPassword] = useState('');
  const [tokenValue, setTokenValue] = useState('');
  const [tab, setTab] = useState<LoginTab>('password');
  const [rememberDevice, setRememberDevice] = useState(true);
  const [formError, setFormError] = useState<string | null>(null);
  const [resetDialogOpen, setResetDialogOpen] = useState(false);
  const [resetEmail, setResetEmail] = useState('');
  const [resetFeedback, setResetFeedback] = useState<{ type: 'success' | 'error'; message: string } | null>(null);

  const { session, login } = useSession();
  const navigate = useNavigate();
  const { mode, toggleMode } = useThemeMode();
  const loginMutation = useMutation({
    mutationFn: (payload: { username: string; password: string }) => loginRequest(payload),
  });
  const resetMutation = useMutation({
    mutationFn: (email: string) => requestPasswordReset(email),
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

    const normalizedIdentifier =
      tab === 'password'
        ? identifier.trim()
        : (() => {
            const tokenSnippet = tokenValue.trim().slice(0, 8);
            const suffix = tokenSnippet === '' ? 'usuario' : tokenSnippet;
            return `token:${suffix}`;
          })();

    const displayName =
      normalizedIdentifier.charAt(0).toUpperCase() + normalizedIdentifier.slice(1);

    try {
      let apiToken: string | null = null;
      let roles: string[] = [];
      let modules: string[] | undefined;
      let partyId: number | undefined;

      if (tab === 'password') {
        if (!identifier.trim() || !password.trim()) {
          setFormError('Ingresa tu usuario o correo y la contraseña.');
          return;
        }
        const response = await loginMutation.mutateAsync({
          username: identifier.trim(),
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
          username: normalizedIdentifier,
          displayName,
          roles: roles.length ? roles : normalizedIdentifier.toLowerCase().includes('admin') ? ['admin'] : ['staff'],
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

  const openResetDialog = () => {
    setResetDialogOpen(true);
    setResetEmail(identifier.includes('@') ? identifier.trim() : '');
    setResetFeedback(null);
    resetMutation.reset();
  };

  const closeResetDialog = () => {
    setResetDialogOpen(false);
    setResetFeedback(null);
    resetMutation.reset();
  };

  const handleResetSubmit = async () => {
    const emailValue = resetEmail.trim();
    if (!emailValue) {
      setResetFeedback({ type: 'error', message: 'Ingresa el correo asociado a tu cuenta.' });
      return;
    }
    setResetFeedback(null);
    try {
      await resetMutation.mutateAsync(emailValue);
      setResetFeedback({
        type: 'success',
        message: 'Si el correo existe en TDF Records, te enviaremos un enlace para restablecer la contraseña.',
      });
    } catch (err) {
      setResetFeedback({
        type: 'error',
        message: err instanceof Error ? err.message : 'No pudimos procesar tu solicitud.',
      });
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
            <Stack spacing={1} alignItems="center">
              <BrandLogo size={64} aria-label="TDF Records" />
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
                  label="Usuario o correo *"
                  type="text"
                  value={identifier}
                  onChange={(event) => setIdentifier(event.target.value)}
                  fullWidth
                  autoComplete="username"
                  helperText="Puedes iniciar sesión con tu usuario o con el correo principal."
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
                <Link
                  component="button"
                  type="button"
                  underline="hover"
                  onClick={openResetDialog}
                  sx={{ cursor: 'pointer', p: 0 }}
                >
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
      <Dialog open={resetDialogOpen} onClose={closeResetDialog} fullWidth maxWidth="xs">
        <DialogTitle>Recuperar acceso</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ pt: 1 }}>
            <TextField
              label="Correo asociado a tu cuenta"
              type="email"
              value={resetEmail}
              onChange={(event) => setResetEmail(event.target.value)}
              fullWidth
              placeholder="tu.correo@tdf.com"
            />
            {resetFeedback && (
              <Alert severity={resetFeedback.type === 'success' ? 'success' : 'error'}>
                {resetFeedback.message}
              </Alert>
            )}
            <Typography variant="body2" color="text.secondary">
              Te enviaremos un enlace temporal para que puedas definir una nueva contraseña.
            </Typography>
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={closeResetDialog}>Cerrar</Button>
          <Button onClick={() => { void handleResetSubmit(); }} disabled={resetMutation.isPending}>
            {resetMutation.isPending ? 'Enviando…' : 'Enviar enlace'}
          </Button>
        </DialogActions>
      </Dialog>

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
