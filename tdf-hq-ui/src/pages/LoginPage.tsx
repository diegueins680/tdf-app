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
  Fade,
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
  TextField,
  Typography,
  useMediaQuery,
  type ChipProps,
} from '@mui/material';
import type { SelectChangeEvent } from '@mui/material/Select';
import { useMutation, useQuery } from '@tanstack/react-query';
import DarkModeIcon from '@mui/icons-material/DarkMode';
import LightModeIcon from '@mui/icons-material/LightMode';
import AutoAwesomeIcon from '@mui/icons-material/AutoAwesome';
import FavoriteBorderIcon from '@mui/icons-material/FavoriteBorder';
import MusicNoteIcon from '@mui/icons-material/MusicNote';
import SchoolOutlinedIcon from '@mui/icons-material/SchoolOutlined';
import { Navigate, useNavigate, Link as RouterLink, useLocation } from 'react-router-dom';
import { useSession } from '../session/SessionContext';
import { Meta } from '../api/meta';
import { useThemeMode } from '../theme/AppThemeProvider';
import { googleLoginRequest, loginRequest, requestPasswordReset, signupRequest } from '../api/auth';
import { SELF_SIGNUP_ROLES, type SignupRole } from '../constants/roles';
import { Fans } from '../api/fans';
import type { ArtistProfileDTO } from '../api/types';
import { buildSignupPayload, deriveEffectiveRoles, normalizeSignupRoles } from '../utils/roles';

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

const LANDING_LABELS: Record<string, string> = {
  '/configuracion/roles-permisos': 'Roles y permisos',
  '/mi-artista': 'Mi artista',
  '/mi-profesor': 'Mi profesor',
  '/fans': 'Fan Hub',
  '/estudio/calendario': 'Calendario',
  '/crm/contactos': 'CRM',
  '/label/artistas': 'Label - Artistas',
  '/operacion/inventario': 'Inventario',
  '/finanzas/pagos': 'Finanzas',
  '/practicas': 'Practicas',
  '/inicio': 'Inicio',
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
    internshipStartAt: '',
    internshipEndAt: '',
    internshipRequiredHours: '',
    internshipSkills: '',
    internshipAreas: '',
  });
  const [signupRoles, setSignupRoles] = useState<SignupRole[]>([]);
  const [favoriteArtistIds, setFavoriteArtistIds] = useState<number[]>([]);
  const [claimArtistId, setClaimArtistId] = useState<number | null>(null);
  const [signupFeedback, setSignupFeedback] = useState<{ type: 'success' | 'error'; message: string } | null>(null);
  const { session, login } = useSession();
  const navigate = useNavigate();
  const location = useLocation();
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
  const dialogFieldSx = useMemo(
    () => ({
      '& .MuiInputLabel-root': {
        color: 'rgba(15,23,42,0.72)',
        '&.Mui-focused': { color: '#2563eb' },
      },
      '& .MuiOutlinedInput-root': {
        bgcolor: '#ffffff',
        color: '#0f172a',
        '& fieldset': { borderColor: 'rgba(15,23,42,0.25)' },
        '&:hover fieldset': { borderColor: 'rgba(15,23,42,0.45)' },
        '&.Mui-focused fieldset': {
          borderColor: '#2563eb',
          boxShadow: '0 0 0 1px rgba(37,99,235,0.35)',
        },
      },
      '& .MuiFormHelperText-root': { color: 'rgba(15,23,42,0.6)' },
      '& .MuiInputBase-input::placeholder': { color: 'rgba(15,23,42,0.45)', opacity: 1 },
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

  const signupPreset = useMemo(() => {
    const params = new URLSearchParams(location.search);
    const intent = (params.get('intent') ?? '').trim().toLowerCase();
    const openSignup = params.get('signup') === '1'
      || intent === 'artist'
      || intent === 'artista'
      || intent === 'intern'
      || intent === 'practicante'
      || intent === 'pasante';
    const rolesRaw = params.getAll('roles');
    const parsedRoles = rolesRaw.length > 0 ? normalizeSignupRoles(rolesRaw) : normalizeSignupRoles(params.get('roles') ?? '');
    const claimRaw = params.get('claimArtistId') ?? params.get('claim');
    const claimArtistId = claimRaw && /^\d+$/.test(claimRaw) ? Number.parseInt(claimRaw, 10) : null;

    const baseRoles: SignupRole[] =
      parsedRoles.length > 0
        ? parsedRoles
        : intent === 'artist' || intent === 'artista'
          ? ['Artista']
          : intent === 'intern' || intent === 'practicante' || intent === 'pasante'
            ? ['Intern']
            : [];
    const ensureArtistRole: SignupRole[] =
      claimArtistId
        ? baseRoles.some((r) => r.toLowerCase().includes('artist'))
          ? baseRoles
          : [...baseRoles, 'Artista']
        : baseRoles;

    return {
      openSignup,
      roles: ensureArtistRole,
      claimArtistId,
    };
  }, [location.search]);

  const appliedSignupPresetRef = useRef<string | null>(null);
  useEffect(() => {
    if (!signupPreset.openSignup) return;
    if (appliedSignupPresetRef.current === location.search) return;
    appliedSignupPresetRef.current = location.search;
    setSignupDialogOpen(true);
    setSignupFeedback(null);
    setSignupForm({
      firstName: '',
      lastName: '',
      email: '',
      phone: '',
      password: '',
      internshipStartAt: '',
      internshipEndAt: '',
      internshipRequiredHours: '',
      internshipSkills: '',
      internshipAreas: '',
    });
    setSignupRoles(signupPreset.roles);
    setFavoriteArtistIds([]);
    setClaimArtistId(signupPreset.claimArtistId);
    signupMutation.reset();
  }, [location.search, signupPreset.claimArtistId, signupPreset.openSignup, signupPreset.roles, signupMutation]);

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
  const wantsInternRole = useMemo(
    () => signupRoles.some((role) => role.toLowerCase() === 'intern'),
    [signupRoles],
  );
  const signupGuide = useMemo(() => {
    const normalizedRoles = signupRoles.map((role) => role.toLowerCase());
    const hasRoles = normalizedRoles.length > 0;
    const hasArtist = normalizedRoles.some((role) => role.includes('artist') || role.includes('artista'));
    const hasFan = normalizedRoles.includes('fan');
    const hasIntern = normalizedRoles.includes('intern');

    const steps: string[] = [];
    let title = 'Primeros pasos sugeridos';
    let note = '';

    if (hasArtist) {
      title = 'Ruta para artistas';
      steps.push('Completa tu bio, generos y links principales.');
      steps.push('Publica portada y video destacado.');
      steps.push('Comparte tu URL publica con tu audiencia.');
    } else if (hasIntern) {
      title = 'Ruta para practicas';
      steps.push('Completa fechas, horas y areas de interes.');
      steps.push('Revisa tu plan de rotaciones con el equipo.');
      steps.push('Comparte tus avances y objetivos.');
    } else if (hasFan) {
      title = 'Ruta para fans';
      steps.push('Sigue a tus artistas favoritos.');
      steps.push('Activa alertas de lanzamientos y eventos.');
      steps.push('Reserva sesiones o streams cuando quieras.');
    } else if (hasRoles) {
      steps.push('Accede al panel principal y configura tu perfil.');
      steps.push('Explora los modulos disponibles para tu rol.');
    } else {
      title = 'Elige tu ruta';
      steps.push('Selecciona un rol para personalizar tu panel.');
      steps.push('Puedes ajustar tus roles mas adelante.');
      note = 'Sin roles: Fan';
    }

    const landingRoles = hasRoles ? signupRoles : (['Fan'] as SignupRole[]);
    const landingPath = redirectPath ?? pickLandingPath(landingRoles);
    const landingLabel = LANDING_LABELS[landingPath] ?? landingPath;

    return { title, steps, landingLabel, note };
  }, [redirectPath, signupRoles]);

  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setFormError(null);

    const normalizedIdentifier = identifier.trim();
    const normalizedPassword = password.trim();
    if (!normalizedIdentifier || !normalizedPassword) {
      setFormError('Ingresa tu usuario o correo y la contraseña.');
      return;
    }

    const displayName =
      normalizedIdentifier.charAt(0).toUpperCase() + normalizedIdentifier.slice(1);

    try {
      const response = await loginMutation.mutateAsync({
        username: normalizedIdentifier,
        password: normalizedPassword,
      });
      const apiToken = response.token;
      const roles = response.roles?.map((role) => role.toLowerCase()) ?? [];
      const modules = response.modules;
      const partyId = response.partyId;

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

  const openSignupDialog = (roles: SignupRole[] = []) => {
    setSignupDialogOpen(true);
    setSignupFeedback(null);
    setSignupForm({
      firstName: '',
      lastName: '',
      email: '',
      phone: '',
      password: '',
      internshipStartAt: '',
      internshipEndAt: '',
      internshipRequiredHours: '',
      internshipSkills: '',
      internshipAreas: '',
    });
    setSignupRoles(roles);
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
      <Container maxWidth="lg" sx={{ display: 'flex', justifyContent: 'center' }}>
        <Stack
          direction={{ xs: 'column', md: 'row' }}
          spacing={{ xs: 2.5, md: 3 }}
          alignItems="stretch"
          sx={{ width: '100%', maxWidth: 980 }}
        >
          <Box sx={{ width: '100%', maxWidth: 440 }}>
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
                    Usa tus credenciales para acceder al panel.
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
                    </Stack>
                  )}
                </Stack>

                <Stack spacing={2.25}>
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
                    onClick={() => openSignupDialog()}
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
                      onClick={() => openSignupDialog()}
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
          </Box>
          <Fade in timeout={700}>
            <Box sx={{ width: '100%', maxWidth: 440 }}>
              <Paper
                elevation={0}
                sx={{
                  width: '100%',
                  p: { xs: 2.5, sm: 3 },
                  borderRadius: { xs: 3, sm: 4 },
                  background: 'linear-gradient(160deg, rgba(15,23,42,0.94), rgba(15,23,42,0.78))',
                  border: '1px solid rgba(255,255,255,0.08)',
                  color: '#e2e8f0',
                }}
              >
                <Stack spacing={2}>
                  <Stack spacing={0.75}>
                    <Chip
                      label="Onboarding rapido"
                      size="small"
                      sx={{
                        width: 'fit-content',
                        bgcolor: 'rgba(56,189,248,0.18)',
                        color: '#e2e8f0',
                        borderColor: 'rgba(56,189,248,0.35)',
                        fontWeight: 700,
                      }}
                      variant="outlined"
                    />
                    <Typography variant="h6" fontWeight={800}>
                      Empieza en minutos
                    </Typography>
                    <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.78)' }}>
                      Elige la ruta que mejor encaja contigo y te llevamos al panel ideal.
                    </Typography>
                  </Stack>
                  <Stack spacing={1.5}>
                    <Paper
                      variant="outlined"
                      sx={{
                        p: 2,
                        borderRadius: 3,
                        borderColor: 'rgba(148,163,184,0.24)',
                        bgcolor: 'rgba(15,23,42,0.65)',
                      }}
                    >
                      <Stack direction="row" spacing={1.5} alignItems="flex-start">
                        <Box
                          sx={{
                            width: 44,
                            height: 44,
                            borderRadius: 2,
                            display: 'flex',
                            alignItems: 'center',
                            justifyContent: 'center',
                            bgcolor: 'rgba(56,189,248,0.18)',
                            color: '#38bdf8',
                          }}
                        >
                          <MusicNoteIcon />
                        </Box>
                        <Stack spacing={0.5} sx={{ flex: 1 }}>
                          <Typography variant="subtitle1" fontWeight={700}>
                            Soy artista
                          </Typography>
                          <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.75)' }}>
                            Crea tu perfil, comparte tu link y presenta tu musica.
                          </Typography>
                          <Button
                            variant="outlined"
                            size="small"
                            onClick={() => navigate('/artist-onboarding')}
                            sx={{
                              alignSelf: 'flex-start',
                              textTransform: 'none',
                              borderColor: 'rgba(148,163,184,0.4)',
                              color: '#e2e8f0',
                            }}
                          >
                            Crear perfil
                          </Button>
                        </Stack>
                      </Stack>
                    </Paper>
                    <Paper
                      variant="outlined"
                      sx={{
                        p: 2,
                        borderRadius: 3,
                        borderColor: 'rgba(148,163,184,0.24)',
                        bgcolor: 'rgba(15,23,42,0.65)',
                      }}
                    >
                      <Stack direction="row" spacing={1.5} alignItems="flex-start">
                        <Box
                          sx={{
                            width: 44,
                            height: 44,
                            borderRadius: 2,
                            display: 'flex',
                            alignItems: 'center',
                            justifyContent: 'center',
                            bgcolor: 'rgba(244,114,182,0.18)',
                            color: '#f472b6',
                          }}
                        >
                          <FavoriteBorderIcon />
                        </Box>
                        <Stack spacing={0.5} sx={{ flex: 1 }}>
                          <Typography variant="subtitle1" fontWeight={700}>
                            Soy fan
                          </Typography>
                          <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.75)' }}>
                            Sigue artistas, recibe lanzamientos y reserva experiencias.
                          </Typography>
                          <Button
                            variant="outlined"
                            size="small"
                            onClick={() => openSignupDialog(['Fan'])}
                            sx={{
                              alignSelf: 'flex-start',
                              textTransform: 'none',
                              borderColor: 'rgba(148,163,184,0.4)',
                              color: '#e2e8f0',
                            }}
                          >
                            Crear cuenta fan
                          </Button>
                        </Stack>
                      </Stack>
                    </Paper>
                    <Paper
                      variant="outlined"
                      sx={{
                        p: 2,
                        borderRadius: 3,
                        borderColor: 'rgba(148,163,184,0.24)',
                        bgcolor: 'rgba(15,23,42,0.65)',
                      }}
                    >
                      <Stack direction="row" spacing={1.5} alignItems="flex-start">
                        <Box
                          sx={{
                            width: 44,
                            height: 44,
                            borderRadius: 2,
                            display: 'flex',
                            alignItems: 'center',
                            justifyContent: 'center',
                            bgcolor: 'rgba(52,211,153,0.18)',
                            color: '#34d399',
                          }}
                        >
                          <SchoolOutlinedIcon />
                        </Box>
                        <Stack spacing={0.5} sx={{ flex: 1 }}>
                          <Typography variant="subtitle1" fontWeight={700}>
                            Busco practicas
                          </Typography>
                          <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.75)' }}>
                            Postula, define tu plan y organiza tus rotaciones.
                          </Typography>
                          <Button
                            variant="outlined"
                            size="small"
                            onClick={() => openSignupDialog(['Intern'])}
                            sx={{
                              alignSelf: 'flex-start',
                              textTransform: 'none',
                              borderColor: 'rgba(148,163,184,0.4)',
                              color: '#e2e8f0',
                            }}
                          >
                            Postular practicas
                          </Button>
                        </Stack>
                      </Stack>
                    </Paper>
                  </Stack>
                  <Stack direction="row" spacing={1} flexWrap="wrap">
                    <Chip label="Menos de 3 minutos" size="small" variant="outlined" sx={{ color: '#cbd5f5' }} />
                    <Chip label="Roles editables" size="small" variant="outlined" sx={{ color: '#cbd5f5' }} />
                    <Chip label="Acceso inmediato" size="small" variant="outlined" sx={{ color: '#cbd5f5' }} />
                  </Stack>
                </Stack>
              </Paper>
            </Box>
          </Fade>
        </Stack>
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
              sx={dialogFieldSx}
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
                sx={dialogFieldSx}
              />
              <TextField
                label="Apellido"
                value={signupForm.lastName}
                onChange={(event) => setSignupForm((prev) => ({ ...prev, lastName: event.target.value }))}
                fullWidth
                sx={dialogFieldSx}
              />
            </Stack>
            <TextField
              label="Correo *"
              type="email"
              value={signupForm.email}
              onChange={(event) => setSignupForm((prev) => ({ ...prev, email: event.target.value }))}
              fullWidth
              placeholder="tu.correo@tdf.com"
              sx={dialogFieldSx}
            />
            <TextField
              label="Celular (opcional)"
              value={signupForm.phone}
              onChange={(event) => setSignupForm((prev) => ({ ...prev, phone: event.target.value }))}
              fullWidth
              sx={dialogFieldSx}
            />
            <FormControl fullWidth sx={dialogFieldSx}>
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
            {wantsInternRole && (
              <Stack spacing={1}>
                <Typography variant="subtitle2">Detalles de prácticas</Typography>
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
                  <TextField
                    label="Inicio de prácticas"
                    type="date"
                    value={signupForm.internshipStartAt}
                    onChange={(event) => setSignupForm((prev) => ({ ...prev, internshipStartAt: event.target.value }))}
                    fullWidth
                    InputLabelProps={{ shrink: true }}
                    sx={dialogFieldSx}
                  />
                  <TextField
                    label="Fin de prácticas"
                    type="date"
                    value={signupForm.internshipEndAt}
                    onChange={(event) => setSignupForm((prev) => ({ ...prev, internshipEndAt: event.target.value }))}
                    fullWidth
                    InputLabelProps={{ shrink: true }}
                    sx={dialogFieldSx}
                  />
                  <TextField
                    label="Horas requeridas"
                    type="number"
                    value={signupForm.internshipRequiredHours}
                    onChange={(event) => setSignupForm((prev) => ({ ...prev, internshipRequiredHours: event.target.value }))}
                    fullWidth
                    inputProps={{ min: 0, step: 1 }}
                    sx={dialogFieldSx}
                  />
                </Stack>
                <TextField
                  label="Habilidades / skills"
                  value={signupForm.internshipSkills}
                  onChange={(event) => setSignupForm((prev) => ({ ...prev, internshipSkills: event.target.value }))}
                  fullWidth
                  multiline
                  minRows={2}
                  sx={dialogFieldSx}
                />
                <TextField
                  label="Áreas de práctica de interés"
                  value={signupForm.internshipAreas}
                  onChange={(event) => setSignupForm((prev) => ({ ...prev, internshipAreas: event.target.value }))}
                  fullWidth
                  multiline
                  minRows={2}
                  sx={dialogFieldSx}
                />
              </Stack>
            )}
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
                      sx={dialogFieldSx}
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
                      sx={dialogFieldSx}
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
              sx={dialogFieldSx}
            />
            {signupFeedback && (
              <Alert severity={signupFeedback.type === 'success' ? 'success' : 'error'}>
                {signupFeedback.message}
              </Alert>
            )}
            <Paper
              variant="outlined"
              sx={{
                p: 2,
                borderRadius: 2,
                borderColor: 'rgba(37,99,235,0.28)',
                bgcolor: 'rgba(239,246,255,0.75)',
              }}
            >
              <Stack spacing={1}>
                <Stack direction="row" spacing={1} alignItems="center">
                  <AutoAwesomeIcon color="primary" fontSize="small" />
                  <Typography variant="subtitle2" color="text.primary">
                    {signupGuide.title}
                  </Typography>
                </Stack>
                <Stack spacing={0.5}>
                  {signupGuide.steps.map((step) => (
                    <Typography key={step} variant="body2" color="text.secondary">
                      - {step}
                    </Typography>
                  ))}
                </Stack>
                <Stack direction="row" spacing={1} flexWrap="wrap">
                  <Chip
                    label={`Destino: ${signupGuide.landingLabel}`}
                    size="small"
                    color="primary"
                    variant="outlined"
                  />
                  {signupGuide.note && (
                    <Chip label={signupGuide.note} size="small" variant="outlined" />
                  )}
                </Stack>
              </Stack>
            </Paper>
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
