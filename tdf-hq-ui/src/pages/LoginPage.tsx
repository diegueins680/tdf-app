import { useMemo, useState, type FormEvent } from 'react';
import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Checkbox,
  Chip,
  CircularProgress,
  Container,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Fab,
  FormControl,
  FormControlLabel,
  FormHelperText,
  InputLabel,
  Link,
  MenuItem,
  OutlinedInput,
  Paper,
  Select,
  Stack,
  Tab,
  Tabs,
  TextField,
  Typography,
  type ChipProps,
} from '@mui/material';
import type { SelectChangeEvent } from '@mui/material/Select';
import { useMutation, useQuery } from '@tanstack/react-query';
import DarkModeIcon from '@mui/icons-material/DarkMode';
import LightModeIcon from '@mui/icons-material/LightMode';
import { Navigate, useNavigate, Link as RouterLink } from 'react-router-dom';
import { useSession } from '../session/SessionContext';
import { Meta } from '../api/meta';
import { useThemeMode } from '../theme/AppThemeProvider';
import { loginRequest, requestPasswordReset, signupRequest } from '../api/auth';
import { SELF_SIGNUP_ROLES, type SignupRole } from '../constants/roles';
import { Fans } from '../api/fans';
import type { ArtistProfileDTO } from '../api/types';
import BrandLogo from '../components/BrandLogo';
import { buildSignupPayload, deriveEffectiveRoles, normalizeSignupRoles } from '../utils/roles';

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
  const [signupDialogOpen, setSignupDialogOpen] = useState(false);
  const [signupForm, setSignupForm] = useState({
    firstName: '',
    lastName: '',
    email: '',
    phone: '',
    password: '',
  });
  const [signupRoles, setSignupRoles] = useState<SignupRole[]>([]);
  const [favoriteArtistIds, setFavoriteArtistIds] = useState<number[]>([]);
  const [claimArtistId, setClaimArtistId] = useState<number | null>(null);
  const [signupFeedback, setSignupFeedback] = useState<{ type: 'success' | 'error'; message: string } | null>(null);

  const { session, login } = useSession();
  const navigate = useNavigate();
  const { mode, toggleMode } = useThemeMode();
  const loginMutation = useMutation({
    mutationFn: (payload: { username: string; password: string }) => loginRequest(payload),
  });
  const resetMutation = useMutation({
    mutationFn: (email: string) => requestPasswordReset(email),
  });
  const signupMutation = useMutation({
    mutationFn: signupRequest,
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

  const fanArtistsQuery = useQuery({
    queryKey: ['signup', 'artists'],
    queryFn: Fans.listArtists,
    enabled: signupDialogOpen,
    staleTime: 5 * 60 * 1000,
  });
  const fanArtists = useMemo(() => fanArtistsQuery.data ?? [], [fanArtistsQuery.data]);
  const claimableArtists = useMemo(
    () => fanArtists.filter((artist) => artist.apHasUserAccount === false),
    [fanArtists],
  );
  const selectedClaim = useMemo(
    () => claimableArtists.find((artist) => artist.apArtistId === claimArtistId) ?? null,
    [claimArtistId, claimableArtists],
  );

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

      const baseRoles =
        roles.length > 0
          ? roles
          : normalizedIdentifier.toLowerCase().includes('admin')
            ? ['admin']
            : ['staff'];
      const normalized = Array.from(new Set(baseRoles.map((r) => r.toLowerCase())));
      if (!normalized.includes('fan')) {
        normalized.push('fan');
      }

      login(
        {
          username: normalizedIdentifier,
          displayName,
          roles: normalized,
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
    } catch (err) {
      console.warn('Password reset request failed (silently returning success)', err);
    } finally {
      setResetFeedback({
        type: 'success',
        message: 'Si el correo existe en TDF Records, te enviaremos un enlace para restablecer la contraseña.',
      });
    }
  };

  const openSignupDialog = () => {
    setSignupDialogOpen(true);
    setSignupFeedback(null);
    setSignupForm({
      firstName: '',
      lastName: '',
      email: '',
      phone: '',
      password: '',
    });
    setSignupRoles([]);
    setFavoriteArtistIds([]);
    setClaimArtistId(null);
    signupMutation.reset();
  };

  const closeSignupDialog = () => {
    setSignupDialogOpen(false);
    setSignupFeedback(null);
    setSignupRoles([]);
    setFavoriteArtistIds([]);
    setClaimArtistId(null);
    signupMutation.reset();
  };

  const handleSignupRoleChange = (event: SelectChangeEvent<SignupRole[]>) => {
    const nextRoles = normalizeSignupRoles(event.target.value);
    setSignupRoles(nextRoles);
    if (!nextRoles.includes('Fan')) {
      setFavoriteArtistIds([]);
    }
  };

  const handleSignupSubmit = async () => {
    const claimIsValid = claimArtistId ? claimableArtists.some((artist) => artist.apArtistId === claimArtistId) : true;
    if (!claimIsValid) {
      setSignupFeedback({ type: 'error', message: 'El perfil seleccionado ya no está disponible para reclamar.' });
      return;
    }

    const payload = buildSignupPayload(signupForm, signupRoles, favoriteArtistIds, claimArtistId ?? undefined);
    const selectedRoles = payload.roles ?? [];
    if (!payload.email || !payload.password || (!payload.firstName && !payload.lastName)) {
      setSignupFeedback({ type: 'error', message: 'Completa nombre, correo y una contraseña segura (8+ caracteres).' });
      return;
    }
    if (payload.password.length < 8) {
      setSignupFeedback({ type: 'error', message: 'La contraseña debe tener al menos 8 caracteres.' });
      return;
    }
    setSignupFeedback(null);
    try {
      const response = await signupMutation.mutateAsync(payload);
      const effectiveRoles = deriveEffectiveRoles(response.roles, selectedRoles);
      const shouldFollowArtists = selectedRoles.includes('Fan') && favoriteArtistIds.length > 0;
      const selectedFanArtistIds = favoriteArtistIds;
      login(
        {
          username: payload.email,
          displayName: `${payload.firstName} ${payload.lastName}`.trim() || payload.email,
          roles: effectiveRoles,
          apiToken: response.token,
          modules: response.modules,
          partyId: response.partyId,
        },
        { remember: true },
      );
      if (shouldFollowArtists) {
        void Promise.all(
          selectedFanArtistIds.map((artistId) =>
            Fans.follow(artistId).catch((followErr) => {
              console.warn('No se pudo seguir al artista después del registro', followErr);
            }),
          ),
        );
      }
      navigate('/crm/contactos', { replace: true });
    } catch (err) {
      setSignupFeedback({
        type: 'error',
        message: err instanceof Error ? err.message : 'No pudimos crear la cuenta. Intenta de nuevo.',
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
            <Typography variant="caption" color="text.secondary">
              Si lo activas, mantendremos tu sesión iniciada en este navegador.
            </Typography>

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
                <Link
                  component="button"
                  type="button"
                  underline="hover"
                  onClick={openSignupDialog}
                  sx={{ cursor: 'pointer', p: 0 }}
                >
                  Crear cuenta
                </Link>
              </Typography>
              <Typography variant="body2">
                ¿Buscas una clase de prueba?{' '}
                <Link component={RouterLink} to="/trials" underline="hover">
                  Solicitar trial
                </Link>
              </Typography>
              <Typography variant="body2">
                ¿Quieres explorar la música?{' '}
                <Link component={RouterLink} to="/fans" underline="hover">
                  Ir al Fan Hub
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
      <Dialog open={signupDialogOpen} onClose={closeSignupDialog} fullWidth maxWidth="sm">
        <DialogTitle>Crear cuenta</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ pt: 1 }}>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
              <TextField
                label="Nombre"
                value={signupForm.firstName}
                onChange={(event) => setSignupForm((prev) => ({ ...prev, firstName: event.target.value }))}
                fullWidth
              />
              <TextField
                label="Apellido"
                value={signupForm.lastName}
                onChange={(event) => setSignupForm((prev) => ({ ...prev, lastName: event.target.value }))}
                fullWidth
              />
            </Stack>
            <TextField
              label="Correo *"
              type="email"
              value={signupForm.email}
              onChange={(event) => setSignupForm((prev) => ({ ...prev, email: event.target.value }))}
              fullWidth
              placeholder="tu.correo@tdf.com"
            />
            <TextField
              label="Celular (opcional)"
              value={signupForm.phone}
              onChange={(event) => setSignupForm((prev) => ({ ...prev, phone: event.target.value }))}
              fullWidth
            />
            <FormControl fullWidth>
              <InputLabel id="signup-roles-label">Roles (opcional)</InputLabel>
              <Select<SignupRole[]>
                labelId="signup-roles-label"
                multiple
                value={signupRoles}
                onChange={handleSignupRoleChange}
                input={<OutlinedInput label="Roles (opcional)" />}
                renderValue={(selected) => (
                  <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
                    {selected.map((role) => (
                      <Chip key={role} label={role} size="small" />
                    ))}
                  </Box>
                )}
              >
                {SELF_SIGNUP_ROLES.map((role) => (
                  <MenuItem key={role} value={role}>
                    <Checkbox checked={signupRoles.includes(role)} />
                    <Typography variant="body2">{role}</Typography>
                  </MenuItem>
                ))}
              </Select>
              <FormHelperText>
                Elige los roles que necesitas (Fan, Artista, Promotor, Manager, A&R, Producer, etc.). Roles administrativos o financieros (como Admin o Accounting) no se pueden autoseleccionar.
                </FormHelperText>
              </FormControl>
            {(claimableArtists.length > 0 || claimArtistId) && (
              <Stack spacing={1}>
                <Typography variant="subtitle2">¿Tu perfil de artista ya existe en TDF?</Typography>
                <Autocomplete
                  options={claimableArtists}
                  getOptionLabel={(option) => option.apDisplayName}
                  value={selectedClaim}
                  loading={fanArtistsQuery.isFetching}
                  onChange={(_, option) => {
                    setClaimArtistId(option?.apArtistId ?? null);
                    if (option && !signupRoles.some((role) => role.toLowerCase().startsWith('artist'))) {
                      setSignupRoles((prev) => [...prev, 'Artista']);
                    }
                  }}
                  noOptionsText={fanArtistsQuery.isFetching ? 'Buscando artistas…' : 'No hay perfiles disponibles para reclamar'}
                  renderInput={(params) => (
                    <TextField
                      {...params}
                      label="Reclamar perfil de artista (opcional)"
                      helperText="Solo se muestran perfiles sin usuario. Al reclamarlo obtendrás acceso como Artista."
                      InputProps={{
                        ...params.InputProps,
                        endAdornment: (
                          <>
                            {fanArtistsQuery.isFetching ? <CircularProgress color="inherit" size={16} /> : null}
                            {params.InputProps.endAdornment}
                          </>
                        ),
                      }}
                    />
                  )}
                />
                {claimArtistId && !selectedClaim && (
                  <Alert severity="warning">El perfil elegido ya no está disponible para reclamar.</Alert>
                )}
              </Stack>
            )}
            {signupRoles.includes('Fan') && (
              <Stack spacing={1}>
                <Typography variant="subtitle2">¿De qué artistas o bandas eres fan?</Typography>
                <Autocomplete<ArtistProfileDTO, true, false, false>
                  multiple
                  options={fanArtists}
                  getOptionLabel={(option) => option.apDisplayName}
                  value={fanArtists.filter((artist) => favoriteArtistIds.includes(artist.apArtistId))}
                  loading={fanArtistsQuery.isFetching}
                  onChange={(_, selected) => {
                    setFavoriteArtistIds(selected.map((item) => item.apArtistId));
                  }}
                  renderInput={(params) => (
                    <TextField
                      {...params}
                      label="Artistas o bandas"
                      placeholder={fanArtistsQuery.isFetching ? 'Cargando artistas...' : 'Buscar y seleccionar'}
                      InputProps={{
                        ...params.InputProps,
                        endAdornment: (
                          <>
                            {fanArtistsQuery.isFetching ? <CircularProgress color="inherit" size={16} /> : null}
                            {params.InputProps.endAdornment}
                          </>
                        ),
                      }}
                    />
                  )}
                />
                {fanArtistsQuery.isError && (
                  <Alert severity="warning">
                    No pudimos cargar la lista de artistas en este momento. Puedes seguirlos después desde el Fan Hub.
                  </Alert>
                )}
              </Stack>
            )}
            <TextField
              label="Contraseña *"
              type="password"
              value={signupForm.password}
              onChange={(event) => setSignupForm((prev) => ({ ...prev, password: event.target.value }))}
              fullWidth
              helperText="Mínimo 8 caracteres."
            />
            {signupFeedback && (
              <Alert severity={signupFeedback.type === 'success' ? 'success' : 'error'}>
                {signupFeedback.message}
              </Alert>
            )}
            <Typography variant="body2" color="text.secondary">
              Al crear la cuenta aceptas los términos de servicio de TDF Records y recibes acceso inmediato al panel.
            </Typography>
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={closeSignupDialog}>Cancelar</Button>
          <Button onClick={() => { void handleSignupSubmit(); }} disabled={signupMutation.isPending}>
            {signupMutation.isPending ? 'Creando…' : 'Crear e ingresar'}
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
