import { useMemo, useState, type FormEvent } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  InputAdornment,
  IconButton,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import VisibilityIcon from '@mui/icons-material/Visibility';
import VisibilityOffIcon from '@mui/icons-material/VisibilityOff';
import { useMutation } from '@tanstack/react-query';
import { Link as RouterLink, useLocation, useNavigate } from 'react-router-dom';

import { confirmPasswordReset, type LoginResponseDTO } from '../api/auth';
import { loadSessionSnapshot } from '../api/session';
import { useSession, type SessionUser } from '../session/SessionContext';
import { pickLandingPath, readSafeRedirectPath } from '../utils/loginRouting';

const MIN_PASSWORD_LENGTH = 8;

const normalizeRoles = (roles: readonly string[] | undefined): string[] =>
  Array.from(new Set((roles ?? []).map((role) => role.toLowerCase())));

export default function ResetPasswordPage() {
  const location = useLocation();
  const navigate = useNavigate();
  const { login } = useSession();
  const token = useMemo(() => {
    const params = new URLSearchParams(location.search);
    return (params.get('token') ?? '').trim();
  }, [location.search]);
  const redirectPath = useMemo(() => readSafeRedirectPath(location.search), [location.search]);

  const [newPassword, setNewPassword] = useState('');
  const [confirmPassword, setConfirmPassword] = useState('');
  const [showNewPassword, setShowNewPassword] = useState(false);
  const [showConfirmPassword, setShowConfirmPassword] = useState(false);
  const [feedback, setFeedback] = useState<{ type: 'error'; message: string } | null>(null);

  const resetMutation = useMutation({
    mutationFn: (payload: { token: string; newPassword: string }) => confirmPasswordReset(payload),
  });

  const fieldSx = useMemo(
    () => ({
      '& .MuiInputLabel-root': {
        color: 'rgba(15,23,42,0.72)',
        '&.Mui-focused': { color: '#2563eb' },
      },
      '& .MuiOutlinedInput-root': {
        bgcolor: '#ffffff',
        color: '#0f172a',
        '& fieldset': { borderColor: 'rgba(15,23,42,0.25)' },
        '&:hover fieldset': { borderColor: 'rgba(15,23,42,0.42)' },
        '&.Mui-focused fieldset': {
          borderColor: '#2563eb',
          boxShadow: '0 0 0 1px rgba(37,99,235,0.3)',
        },
      },
      '& .MuiFormHelperText-root': { color: 'rgba(15,23,42,0.62)' },
    }),
    [],
  );

  const completeLogin = async (response: LoginResponseDTO) => {
    const fallbackSession: SessionUser = {
      username: 'usuario',
      displayName: 'Cuenta TDF',
      roles: normalizeRoles(response.roles),
      ...(response.token ? { apiToken: response.token } : {}),
      ...(response.modules ? { modules: response.modules } : {}),
      ...(response.partyId !== undefined ? { partyId: response.partyId } : {}),
    };

    try {
      const snapshot = await loadSessionSnapshot();
      if (!snapshot) {
        login(fallbackSession);
        navigate(redirectPath ?? pickLandingPath(fallbackSession.roles, fallbackSession.modules), {
          replace: true,
        });
        return;
      }

      const resolvedSession: SessionUser = {
        username: snapshot.username,
        displayName: snapshot.displayName,
        roles: normalizeRoles(snapshot.roles),
        ...(response.token ? { apiToken: response.token } : {}),
        modules: snapshot.modules,
        partyId: snapshot.partyId,
      };

      login(resolvedSession);
      navigate(redirectPath ?? pickLandingPath(resolvedSession.roles, resolvedSession.modules), {
        replace: true,
      });
    } catch {
      login(fallbackSession);
      navigate(redirectPath ?? pickLandingPath(fallbackSession.roles, fallbackSession.modules), {
        replace: true,
      });
    }
  };

  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();

    if (!token) {
      setFeedback({ type: 'error', message: 'Este enlace no incluye un token válido.' });
      return;
    }

    const trimmedPassword = newPassword.trim();
    if (trimmedPassword.length < MIN_PASSWORD_LENGTH) {
      setFeedback({
        type: 'error',
        message: `La nueva contraseña debe tener al menos ${MIN_PASSWORD_LENGTH} caracteres.`,
      });
      return;
    }

    if (trimmedPassword !== confirmPassword.trim()) {
      setFeedback({ type: 'error', message: 'Las contraseñas no coinciden.' });
      return;
    }

    setFeedback(null);
    try {
      const response = await resetMutation.mutateAsync({ token, newPassword: trimmedPassword });
      await completeLogin(response);
    } catch (error) {
      setFeedback({
        type: 'error',
        message:
          error instanceof Error && error.message.trim() !== ''
            ? error.message
            : 'No pudimos restablecer la contraseña.',
      });
    }
  };

  if (!token) {
    return (
      <Box sx={{ display: 'flex', justifyContent: 'center', py: { xs: 4, md: 8 } }}>
        <Card sx={{ width: '100%', maxWidth: 560, borderRadius: 4 }}>
          <CardContent>
            <Stack spacing={2.5}>
              <Typography variant="overline" color="text.secondary">
                Acceso
              </Typography>
              <Typography variant="h4" fontWeight={800}>
                Enlace incompleto
              </Typography>
              <Alert severity="error">
                El enlace de recuperación no trae token. Pide un nuevo correo desde la pantalla de login.
              </Alert>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
                <Button component={RouterLink} to="/login" variant="contained">
                  Ir a login
                </Button>
                <Button component={RouterLink} to="/login" variant="outlined">
                  Solicitar nuevo enlace
                </Button>
              </Stack>
            </Stack>
          </CardContent>
        </Card>
      </Box>
    );
  }

  return (
    <Box sx={{ display: 'flex', justifyContent: 'center', py: { xs: 2, md: 6 } }}>
      <Card sx={{ width: '100%', maxWidth: 640, borderRadius: 4, boxShadow: 8 }}>
        <CardContent sx={{ p: { xs: 3, md: 4 } }}>
          <Stack component="form" spacing={2.5} onSubmit={handleSubmit}>
            <Stack spacing={1}>
              <Typography variant="overline" color="text.secondary">
                Recuperar acceso
              </Typography>
              <Typography variant="h4" fontWeight={800}>
                Elige tu nueva contraseña
              </Typography>
              <Typography color="text.secondary">
                Cuando la guardes, te dejaremos con la sesión abierta para que entres directo a tu panel.
              </Typography>
            </Stack>

            <TextField
              label="Nueva contraseña"
              type={showNewPassword ? 'text' : 'password'}
              value={newPassword}
              onChange={(event) => setNewPassword(event.target.value)}
              autoComplete="new-password"
              fullWidth
              helperText="Usa al menos 8 caracteres."
              sx={fieldSx}
              InputProps={{
                endAdornment: (
                  <InputAdornment position="end">
                    <IconButton
                      edge="end"
                      onClick={() => setShowNewPassword((prev) => !prev)}
                      onMouseDown={(event) => event.preventDefault()}
                      aria-label={showNewPassword ? 'Ocultar contraseña' : 'Mostrar contraseña'}
                    >
                      {showNewPassword ? <VisibilityOffIcon /> : <VisibilityIcon />}
                    </IconButton>
                  </InputAdornment>
                ),
              }}
            />

            <TextField
              label="Confirmar contraseña"
              type={showConfirmPassword ? 'text' : 'password'}
              value={confirmPassword}
              onChange={(event) => setConfirmPassword(event.target.value)}
              autoComplete="new-password"
              fullWidth
              sx={fieldSx}
              InputProps={{
                endAdornment: (
                  <InputAdornment position="end">
                    <IconButton
                      edge="end"
                      onClick={() => setShowConfirmPassword((prev) => !prev)}
                      onMouseDown={(event) => event.preventDefault()}
                      aria-label={showConfirmPassword ? 'Ocultar contraseña' : 'Mostrar contraseña'}
                    >
                      {showConfirmPassword ? <VisibilityOffIcon /> : <VisibilityIcon />}
                    </IconButton>
                  </InputAdornment>
                ),
              }}
            />

            {feedback && <Alert severity="error">{feedback.message}</Alert>}

            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
              <Button type="submit" variant="contained" disabled={resetMutation.isPending}>
                {resetMutation.isPending ? 'Guardando…' : 'Guardar contraseña'}
              </Button>
              <Button component={RouterLink} to="/login" variant="outlined" disabled={resetMutation.isPending}>
                Volver a login
              </Button>
            </Stack>
          </Stack>
        </CardContent>
      </Card>
    </Box>
  );
}
