import { useCallback, useEffect, useMemo, useRef, useState, type FormEvent } from 'react';
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
  useMediaQuery,
  type ChipProps,
} from '@mui/material';
import type { SelectChangeEvent } from '@mui/material/Select';
import { useMutation, useQuery } from '@tanstack/react-query';
import DarkModeIcon from '@mui/icons-material/DarkMode';
import LightModeIcon from '@mui/icons-material/LightMode';
import { Navigate, useNavigate, Link as RouterLink, useLocation } from 'react-router-dom';
import { useSession } from '../session/SessionContext';
import { Meta } from '../api/meta';
import { useThemeMode } from '../theme/AppThemeProvider';
import { googleLoginRequest, loginRequest, requestPasswordReset, signupRequest } from '../api/auth';
import { SELF_SIGNUP_ROLES, type SignupRole } from '../constants/roles';
import { Fans } from '../api/fans';
import type { ArtistProfileDTO } from '../api/types';
import { buildSignupPayload, deriveEffectiveRoles, normalizeSignupRoles } from '../utils/roles';

type LoginTab = 'password' | 'token';

const pickLandingPath = (roles: string[], modules?: string[]) => {
  const lowerRoles = roles.map((r) => r.toLowerCase());
  const lowerModules = (modules ?? []).map((m) => m.toLowerCase());
  const hasRole = (...needles: string[]) => needles.some((needle) => lowerRoles.some((role) => role.includes(needle)));
  const hasModule = (needle: string) => lowerModules.includes(needle);

  if (hasRole('admin') || hasModule('admin')) return '/configuracion/roles-permisos';
  if (hasRole('artist', 'artista')) return '/mi-artista';
  if (hasRole('teacher')) return '/mi-profesor';
  if (hasRole('fan', 'customer') && !hasModule('crm') && !hasModule('scheduling')) return '/fans';
  if (hasModule('scheduling')) return '/estudio/calendario';
  if (hasModule('crm')) return '/crm/contactos';
  if (hasModule('label')) return '/label/artistas';
  if (hasModule('ops')) return '/operacion/inventario';
  if (hasModule('invoicing')) return '/finanzas/pagos';
  return '/inicio';
};

declare global {
  interface Window {
    google?: {
      accounts?: {
        id?: {
          initialize: (options: Record<string, unknown>) => void;
          renderButton: (element: HTMLElement, options: Record<string, unknown>) => void;
          prompt: (callback?: (notification: unknown) => void) => void;
        };
      };
    };
  }
}

const GOOGLE_SCRIPT_SRC = 'https://accounts.google.com/gsi/client';
let googleScriptPromise: Promise<void> | null = null;

const loadGoogleScript = () => {
  if (googleScriptPromise) return googleScriptPromise;
  googleScriptPromise = new Promise<void>((resolve, reject) => {
    if (typeof window === 'undefined') {
      resolve();
      return;
    }
    const existing = document.querySelector<HTMLScriptElement>(`script[src="${GOOGLE_SCRIPT_SRC}"]`);
    if (existing) {
      existing.addEventListener('load', () => resolve(), { once: true });
      if (existing.dataset['loaded'] === 'true') {
        resolve();
      }
      return;
    }
    const script = document.createElement('script');
    script.src = GOOGLE_SCRIPT_SRC;
    script.async = true;
    script.defer = true;
    script.dataset['loaded'] = 'false';
    script.onload = () => {
      script.dataset['loaded'] = 'true';
      resolve();
    };
    script.onerror = () => reject(new Error('No se pudo cargar Google Sign-In.'));
    document.head.appendChild(script);
  });
  return googleScriptPromise;
};

const parseGoogleIdToken = (token: string): { email?: string; name?: string } | null => {
  try {
    const [, payload] = token.split('.');
    if (!payload) return null;
    const normalized = payload.replace(/-/g, '+').replace(/_/g, '/');
    const padded = normalized.padEnd(Math.ceil(normalized.length / 4) * 4, '=');
    const decoded = atob(padded);
    return JSON.parse(decoded) as { email?: string; name?: string };
  } catch (error) {
    console.warn('Failed to parse Google ID token', error);
    return null;
  }
};

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
  const [tokenPasteFeedback, setTokenPasteFeedback] = useState<string | null>(null);
  const [showApiToken, setShowApiToken] = useState(false);
  const { session, login } = useSession();
  const navigate = useNavigate();
  const location = useLocation();
  const params = new URLSearchParams(location.search);
  const tokenFromUrl = params.get('token');
  const lastSession = session;
  const passwordHint = 'Usa 8+ caracteres con mayúsculas, minúsculas y un número.';
  const googleClientId = import.meta.env['VITE_GOOGLE_CLIENT_ID'] ?? '';
  const googleButtonRef = useRef<HTMLDivElement | null>(null);
  const googleSignupButtonRef = useRef<HTMLDivElement | null>(null);
  const googleInitRef = useRef(false);
  const [googleButtonWidth, setGoogleButtonWidth] = useState(320);
  const [googleStatus, setGoogleStatus] = useState<string | null>(null);
  const [googleError, setGoogleError] = useState<string | null>(null);
  const textFieldSx = useMemo(
    () => ({
      '& .MuiInputLabel-root': {
        color: 'rgba(248,250,252,0.75)',
        '&.Mui-focused': { color: '#f8fafc' },
      },
      '& .MuiOutlinedInput-root': {
        bgcolor: 'rgba(255,255,255,0.05)',
        borderRadius: 2,
        color: '#f8fafc',
        '& fieldset': { borderColor: 'rgba(255,255,255,0.18)' },
        '&:hover fieldset': { borderColor: 'rgba(255,255,255,0.3)' },
        '&.Mui-focused fieldset': {
          borderColor: '#8ab4ff',
          boxShadow: '0 0 0 1px rgba(138,180,255,0.4)',
        },
      },
      '& .MuiFormHelperText-root': { color: 'rgba(248,250,252,0.78)' },
      '& .MuiInputBase-input::placeholder': { color: 'rgba(248,250,252,0.6)' },
    }),
    [],
  );

  const { mode, toggleMode } = useThemeMode();
  const isMobile = useMediaQuery('(max-width:600px)');
  const loginMutation = useMutation({
    mutationFn: (payload: { username: string; password: string }) => loginRequest(payload),
  });
  const googleLoginMutation = useMutation({
    mutationFn: (payload: { idToken: string }) => googleLoginRequest(payload),
  });
  const resetMutation = useMutation({
    mutationFn: (email: string) => requestPasswordReset(email),
  });
  const signupMutation = useMutation({
    mutationFn: signupRequest,
  });
  const redirectPath = useMemo(() => {
    const params = new URLSearchParams(location.search);
    const raw = params.get('redirect');
    if (raw?.startsWith('/')) return raw;
    return null;
  }, [location.search]);
  useEffect(() => {
    if (tokenFromUrl && tokenFromUrl.trim()) {
      setShowApiToken(true);
      setTab('token');
      setTokenValue(tokenFromUrl.trim());
      setTokenPasteFeedback('Token precargado desde el enlace.');
    }
  }, [tokenFromUrl]);

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
      const landingPath = pickLandingPath(normalized, modules);
      const targetPath = redirectPath ?? landingPath;

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
      navigate(targetPath, { replace: true });
    } catch (error) {
      const message = error instanceof Error ? error.message : 'No se pudo iniciar sesión.';
      setFormError(message.trim() === '' ? 'No se pudo iniciar sesión.' : message);
    }
  };

  const handlePasteToken = async () => {
    try {
      const text = await navigator.clipboard.readText();
      const trimmed = text.trim();
      if (!trimmed) {
        setTokenPasteFeedback('El portapapeles está vacío.');
        return;
      }
      setTokenValue(trimmed);
      setTokenPasteFeedback('Token pegado.');
    } catch (error) {
      console.warn('Clipboard read failed', error);
      setTokenPasteFeedback('No pudimos leer el portapapeles. Revisa permisos.');
    }
  };

  const handleGoogleCredential = useCallback(
    async (credentialResponse: { credential?: string }) => {
      const credential = credentialResponse?.credential;
      const parsed = credential ? parseGoogleIdToken(credential) : null;
      const fallbackName = parsed?.name ?? parsed?.email ?? 'Cuenta Google';
      const fallbackUsername = parsed?.email ?? 'google-user';
      if (!credential) {
        const message = 'No recibimos la credencial de Google.';
        if (signupDialogOpen) {
          setSignupFeedback({ type: 'error', message });
        } else {
          setFormError(message);
        }
        return;
      }
      try {
        setGoogleStatus('Conectando con Google…');
        setGoogleError(null);
        const response = await googleLoginMutation.mutateAsync({ idToken: credential });
        const normalizedRoles = response.roles?.map((role) => role.toLowerCase()) ?? [];
        const landingPath = pickLandingPath(normalizedRoles, response.modules);
        const targetPath = redirectPath ?? landingPath;
        login(
          {
            username: fallbackUsername,
            displayName: fallbackName,
            roles: normalizedRoles,
            apiToken: response.token,
            modules: response.modules,
            partyId: response.partyId,
          },
          { remember: true },
        );
        setSignupDialogOpen(false);
        setSignupFeedback(null);
        navigate(targetPath, { replace: true });
      } catch (err) {
        const message = err instanceof Error ? err.message : 'No pudimos iniciar sesión con Google.';
        setGoogleError(message);
        if (signupDialogOpen) {
          setSignupFeedback({ type: 'error', message });
        } else {
          setFormError(message);
        }
      } finally {
        setGoogleStatus(null);
      }
    },
    [googleLoginMutation, login, navigate, redirectPath, signupDialogOpen],
  );

  const loginWithToken = useCallback(() => {
    if (!tokenValue.trim()) {
      setFormError('Ingresa tu token API.');
      return;
    }
    const roles = ['token'];
    const landingPath = pickLandingPath(roles, []);
    const targetPath = redirectPath ?? landingPath;
    login(
      {
        username: 'token-user',
        displayName: 'API Token',
        roles,
        apiToken: tokenValue.trim(),
        modules: [],
        partyId: undefined,
      },
      { remember: rememberDevice },
    );
    navigate(targetPath, { replace: true });
  }, [login, navigate, redirectPath, rememberDevice, tokenValue]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    const clampWidth = (value: number) => Math.max(220, Math.min(Math.round(value), 420));
    const measureWidth = () => {
      const widths = [googleButtonRef.current, googleSignupButtonRef.current]
        .map((node) => node?.getBoundingClientRect().width ?? 0)
        .filter((value) => value > 0);
      const widest = widths.length > 0 ? Math.max(...widths) : 0;
      if (widest > 0) {
        setGoogleButtonWidth((prev) => {
          const next = clampWidth(widest);
          return Math.abs(prev - next) > 1 ? next : prev;
        });
      }
    };
    measureWidth();
    window.addEventListener('resize', measureWidth);
    return () => {
      window.removeEventListener('resize', measureWidth);
    };
  }, [signupDialogOpen]);

  useEffect(() => {
    if (!googleClientId) return;
    let cancelled = false;
    void (async () => {
      try {
        await loadGoogleScript();
        if (cancelled) return;
        const google = window.google?.accounts?.id;
        if (!google) return;
        if (!googleInitRef.current) {
          google.initialize({
            client_id: googleClientId,
            callback: handleGoogleCredential,
            ux_mode: 'popup',
            auto_select: false,
          });
          googleInitRef.current = true;
        }
        const renderGoogleButton = (target: HTMLElement, text: 'continue_with' | 'signup_with') => {
          target.innerHTML = '';
          google.renderButton(target, {
            theme: 'outline',
            size: isMobile ? 'medium' : 'large',
            width: googleButtonWidth,
            text,
            shape: 'pill',
          });
        };
        if (googleButtonRef.current) {
          renderGoogleButton(googleButtonRef.current, 'continue_with');
          google.prompt();
        }
        if (signupDialogOpen && googleSignupButtonRef.current) {
          renderGoogleButton(googleSignupButtonRef.current, 'signup_with');
        }
      } catch (error) {
        console.warn('Google Sign-In initialization failed', error);
      }
    })();
    return () => {
      cancelled = true;
    };
  }, [googleButtonWidth, googleClientId, handleGoogleCredential, isMobile, signupDialogOpen]);

  useEffect(() => {
    const params = new URLSearchParams(location.search);
    const wantsSignup = params.get('signup');
    const shouldOpenSignup = wantsSignup && wantsSignup.toLowerCase() !== 'false' && wantsSignup !== '0';
    if (shouldOpenSignup) {
      setSignupDialogOpen(true);
    }
    const wantsToken = params.get('token') ?? params.get('apiToken') ?? params.get('tab');
    if (wantsToken?.toLowerCase().includes('token')) {
      setTab('token');
    }
  }, [location.search]);

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
      const landingPath = pickLandingPath(effectiveRoles, response.modules);
      const targetPath = redirectPath ?? landingPath;
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
      navigate(targetPath, { replace: true });
    } catch (err) {
      setSignupFeedback({
        type: 'error',
        message: err instanceof Error ? err.message : 'No pudimos crear la cuenta. Intenta de nuevo.',
      });
    }
  };

  if (session) {
    const landing = redirectPath ?? pickLandingPath(session.roles ?? [], session.modules);
    return <Navigate to={landing} replace />;
  }

  return (
    <Box
      component="main"
      sx={{
        minHeight: '100vh',
        background: 'linear-gradient(135deg, #0b1224 0%, #0f172a 35%, #0b1224 100%)',
        position: 'relative',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        justifyContent: { xs: 'flex-start', sm: 'center' },
        px: { xs: 2.5, sm: 4 },
        pt: { xs: 4, sm: 6 },
        pb: { xs: `calc(env(safe-area-inset-bottom, 18px) + 20px)`, sm: 6 },
        isolation: 'isolate',
        '@supports (min-height: 100dvh)': {
          minHeight: { xs: '100dvh', sm: '100vh' },
        },
      }}
    >
      <Box
        sx={{
          position: 'absolute',
          inset: 0,
          pointerEvents: 'none',
          background:
            'radial-gradient(40% 40% at 18% 20%, rgba(56,189,248,0.14), transparent 60%), radial-gradient(32% 36% at 82% 10%, rgba(167,139,250,0.16), transparent 55%), radial-gradient(26% 30% at 48% 78%, rgba(52,211,153,0.12), transparent 60%)',
          filter: 'blur(0px)',
        }}
      />
      <Container maxWidth="sm" sx={{ display: 'flex', justifyContent: 'center' }}>
        <Paper
          component="form"
          elevation={6}
          onSubmit={(event) => {
            void handleSubmit(event);
          }}
          sx={{
            width: '100%',
            maxWidth: 440,
            p: { xs: 2.5, sm: 4 },
            borderRadius: { xs: 3, sm: 4 },
            boxShadow: '0 30px 90px rgba(0,0,0,0.55)',
            background: 'linear-gradient(145deg, rgba(15,23,42,0.95), rgba(18,24,38,0.9))',
            border: '1px solid rgba(255,255,255,0.08)',
            backdropFilter: 'blur(12px)',
            color: '#f8fafc',
          }}
        >
          <Stack spacing={isMobile ? 2.5 : 3}>
            <Stack spacing={1} alignItems="flex-start">
              <Chip
                label="TDF Records"
                size="small"
                sx={{
                  bgcolor: 'rgba(148,163,184,0.16)',
                  color: '#e2e8f0',
                  borderColor: 'rgba(148,163,184,0.45)',
                  letterSpacing: 0.8,
                  fontWeight: 700,
                  textTransform: 'uppercase',
                }}
                variant="outlined"
              />
              <Typography variant="h5" fontWeight={700}>
                Iniciar sesión
              </Typography>
              <Typography variant="body2" sx={{ color: 'rgba(248,250,252,0.82)' }}>
                Usa tus credenciales o un token emitido por TDF Records.
              </Typography>
              {lastSession && (
                <Stack direction="row" spacing={1} alignItems="center">
                  <Chip
                    label={`Continuar como ${lastSession.displayName}`}
                    color="primary"
                    onClick={() => {
                      const landing = redirectPath ?? pickLandingPath(lastSession.roles ?? [], lastSession.modules);
                      navigate(landing, { replace: true });
                    }}
                  />
                  {lastSession.apiToken && (
                    <Button
                      size="small"
                      variant="text"
                      onClick={() => {
                        setShowApiToken(true);
                        setTab('token');
                        setTokenValue(lastSession.apiToken ?? '');
                      }}
                    >
                      Usar token guardado
                    </Button>
                  )}
                </Stack>
              )}
            </Stack>


            <Stack spacing={2.25}>
              <Tabs value={tab} onChange={(_, value) => setTab(value as LoginTab)} variant="fullWidth">
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
                    inputProps={{ autoCapitalize: 'none', autoCorrect: 'off', spellCheck: false }}
                    helperText="Puedes iniciar sesión con tu usuario o con el correo principal."
                    sx={textFieldSx}
                  />

                  <TextField
                    label="Contraseña *"
                    type="password"
                    value={password}
                    onChange={(event) => setPassword(event.target.value)}
                    fullWidth
                    autoComplete="current-password"
                    inputProps={{ autoCapitalize: 'none', autoCorrect: 'off', spellCheck: false }}
                    sx={textFieldSx}
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
                    inputProps={{ autoCapitalize: 'none', autoCorrect: 'off', spellCheck: false }}
                    sx={textFieldSx}
                  />
                  <Typography variant="caption" color="text.secondary">
                    Inserta el token temporal asignado por el equipo de operaciones. Caduca en 24 horas.
                  </Typography>
                </Stack>
              )}
            </Stack>

            <Stack spacing={1} sx={{ mt: 1 }}>
              <Typography variant="subtitle2">Acceso rápido con token API</Typography>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                <TextField
                  label="Token API"
                  fullWidth
                  value={tokenValue}
                  onChange={(e) => setTokenValue(e.target.value)}
                  placeholder="tdf_xxxx-xxxx"
                  inputProps={{ autoCapitalize: 'none', autoCorrect: 'off', spellCheck: false }}
                  sx={{ flex: 1 }}
                />
                <Button variant="outlined" onClick={loginWithToken} size="medium">
                  Ingresar con token
                </Button>
              </Stack>
            </Stack>

            {googleClientId && (
              <Stack spacing={1} alignItems="center">
                <Typography variant="body2" sx={{ color: 'rgba(248,250,252,0.82)' }}>
                  O continúa con Google
                </Typography>
                <Box
                  ref={googleButtonRef}
                  sx={{ display: 'flex', justifyContent: 'center', minHeight: 44, width: '100%' }}
                />
                {googleStatus && (
                  <Typography variant="caption" color="text.secondary">
                    {googleStatus}
                  </Typography>
                )}
                {googleError && (
                  <Alert severity="warning" sx={{ width: '100%' }}>
                    {googleError}
                  </Alert>
                )}
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
            <Typography variant="caption" sx={{ color: 'rgba(248,250,252,0.7)' }}>
              Si lo activas, mantendremos tu sesión iniciada en este navegador.
            </Typography>

            {formError && <Alert severity="warning">{formError}</Alert>}

            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'stretch', sm: 'center' }}>
              <Button variant="contained" type="submit" size="large" disabled={loginMutation.isPending}>
                {loginMutation.isPending ? 'Ingresando…' : 'Ingresar'}
              </Button>
              <Button
                variant="outlined"
                size="large"
                onClick={openSignupDialog}
                sx={{ minWidth: 180 }}
              >
                Crear cuenta
              </Button>
            </Stack>

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
              {!googleClientId && (
                <Typography variant="caption" color="text.secondary">
                  Login con Google no disponible en este entorno.
                </Typography>
              )}
            </Stack>
          </Stack>
        </Paper>
      </Container>
      <Box sx={{ mt: { xs: 2.5, sm: 3 }, display: 'flex', justifyContent: 'center', width: '100%' }}>
        <Box id="login-radio-mini-slot" sx={{ width: '100%', maxWidth: 440 }} />
      </Box>
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
              sx={textFieldSx}
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
            {googleClientId && (
              <Stack spacing={1} alignItems="center">
                <Typography variant="body2" color="text.secondary">
                  Crear e ingresar con Google
                </Typography>
                <Box
                  ref={googleSignupButtonRef}
                  sx={{ display: 'flex', justifyContent: 'center', minHeight: 44, width: '100%' }}
                />
                {googleStatus && (
                  <Typography variant="caption" color="text.secondary">
                    {googleStatus}
                  </Typography>
                )}
                {googleError && (
                  <Alert severity="warning" sx={{ width: '100%' }}>
                    {googleError}
                  </Alert>
                )}
              </Stack>
            )}
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
              <TextField
                label="Nombre"
                value={signupForm.firstName}
                onChange={(event) => setSignupForm((prev) => ({ ...prev, firstName: event.target.value }))}
                fullWidth
                sx={textFieldSx}
              />
              <TextField
                label="Apellido"
                value={signupForm.lastName}
                onChange={(event) => setSignupForm((prev) => ({ ...prev, lastName: event.target.value }))}
                fullWidth
                sx={textFieldSx}
              />
            </Stack>
            <TextField
              label="Correo *"
              type="email"
              value={signupForm.email}
              onChange={(event) => setSignupForm((prev) => ({ ...prev, email: event.target.value }))}
              fullWidth
              placeholder="tu.correo@tdf.com"
              sx={textFieldSx}
            />
            <TextField
              label="Celular (opcional)"
              value={signupForm.phone}
              onChange={(event) => setSignupForm((prev) => ({ ...prev, phone: event.target.value }))}
              fullWidth
              sx={textFieldSx}
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
                      sx={textFieldSx}
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
                      sx={textFieldSx}
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
                helperText={passwordHint}
                sx={textFieldSx}
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
        sx={(theme) => ({
          position: 'fixed',
          right: { xs: 12, sm: 24 },
          bottom: `calc(${theme.spacing(10)} + env(safe-area-inset-bottom, 0px))`,
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'flex-end',
          gap: 0.75,
          zIndex: theme.zIndex.snackbar,
        })}
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
