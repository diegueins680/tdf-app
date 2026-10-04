import { useTranslation } from 'react-i18next';
import { useEffect, useMemo, useState, type FormEvent } from 'react';
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
import { buildLoginRedirectPath, readSafeRedirectPath, resolvePostAuthPath } from '../utils/loginRouting';
import { authErrorMessage } from '../utils/authErrorMessage';
import { isValidAuthPassword } from '../utils/passwordPolicy';

const normalizeRoles = (roles: readonly string[] | undefined): string[] =>
  Array.from(new Set((roles ?? []).map((role) => role.toLowerCase())));

export default function ResetPasswordPage() {
  const { t, i18n } = useTranslation();
  const location = useLocation();
  const navigate = useNavigate();
  const { login } = useSession();
  const [token] = useState(() => {
    const params = new URLSearchParams(location.search);
    return (params.get('token') ?? '').trim();
  });
  const redirectPath = useMemo(() => readSafeRedirectPath(location.search), [location.search]);
  const loginPath = redirectPath ? buildLoginRedirectPath(redirectPath) : '/login';
  const recoveryPath = `${loginPath}${loginPath.includes('?') ? '&' : '?'}recover=1&lang=${i18n.resolvedLanguage?.startsWith('es') ? 'es' : 'en'}`;

  useEffect(() => {
    if (!token) return;
    const params = new URLSearchParams(window.location.search);
    params.delete('token');
    const query = params.toString();
    window.history.replaceState(
      window.history.state,
      '',
      `${window.location.pathname}${query ? `?${query}` : ''}${window.location.hash}`,
    );
  }, [token]);

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
      '& .MuiFormHelperText-root': { color: 'text.secondary' },
    }),
    [],
  );

  const completeLogin = async (response: LoginResponseDTO) => {
    const fallbackSession: SessionUser = {
      username: 'usuario',
      displayName: t('authEntry.tdfAccount'),
      roles: normalizeRoles(response.roles),
      ...(response.token ? { apiToken: response.token } : {}),
      ...(response.modules ? { modules: response.modules } : {}),
      ...(response.partyId !== undefined ? { partyId: response.partyId } : {}),
    };

    try {
      const snapshot = await loadSessionSnapshot();
      if (!snapshot) {
        login(fallbackSession);
        navigate(resolvePostAuthPath(null, fallbackSession.roles, fallbackSession.modules, redirectPath), {
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
      navigate(resolvePostAuthPath(null, resolvedSession.roles, resolvedSession.modules, redirectPath), {
        replace: true,
      });
    } catch {
      login(fallbackSession);
      navigate(resolvePostAuthPath(null, fallbackSession.roles, fallbackSession.modules, redirectPath), {
        replace: true,
      });
    }
  };

  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();

    if (!token) {
      setFeedback({ type: 'error', message: t('authEntry.invalidReset') });
      return;
    }

    const trimmedPassword = newPassword.trim();
    if (!isValidAuthPassword(trimmedPassword)) {
      setFeedback({
        type: 'error',
        message: t('authEntry.passwordHint'),
      });
      return;
    }

    if (trimmedPassword !== confirmPassword.trim()) {
      setFeedback({ type: 'error', message: t('authEntry.passwordMismatch') });
      return;
    }

    setFeedback(null);
    try {
      const response = await resetMutation.mutateAsync({ token, newPassword: trimmedPassword });
      await completeLogin(response);
    } catch (error) {
      setFeedback({
        type: 'error',
        message: authErrorMessage(error, t, 'authEntry.resetError'),
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
                {t('authEntry.access')}
              </Typography>
              <Typography variant="h4" fontWeight={800}>
                {t('authEntry.incompleteLink')}
              </Typography>
              <Alert severity="error">
                {t('authEntry.missingResetToken')}
              </Alert>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
                <Button component={RouterLink} to={loginPath} variant="contained">
                  {t('authEntry.loginLink')}
                </Button>
                <Button component={RouterLink} to={recoveryPath} variant="outlined">
                  {t('authEntry.requestNewLink')}
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
          <Stack component="form" spacing={2.5} onSubmit={(event) => { void handleSubmit(event); }}>
            <Stack spacing={1}>
              <Typography variant="overline" color="text.secondary">
                {t('authEntry.recover')}
              </Typography>
              <Typography variant="h4" fontWeight={800}>
                {t('authEntry.choosePassword')}
              </Typography>
              <Typography color="text.secondary">
                {t('authEntry.resetLoginHint')}
              </Typography>
            </Stack>

            <TextField
              label={t('authEntry.newPassword')}
              type={showNewPassword ? 'text' : 'password'}
              value={newPassword}
              onChange={(event) => setNewPassword(event.target.value)}
              autoComplete="new-password"
              fullWidth
              helperText={t('authEntry.passwordHint')}
              sx={fieldSx}
              InputProps={{
                endAdornment: (
                  <InputAdornment position="end">
                    <IconButton
                      edge="end"
                      onClick={() => setShowNewPassword((prev) => !prev)}
                      onMouseDown={(event) => event.preventDefault()}
                      aria-label={showNewPassword ? t('authEntry.hidePassword') : t('authEntry.showPassword')}
                    >
                      {showNewPassword ? <VisibilityOffIcon /> : <VisibilityIcon />}
                    </IconButton>
                  </InputAdornment>
                ),
              }}
            />

            <TextField
              label={t('authEntry.confirmPassword')}
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
                      aria-label={showConfirmPassword ? t('authEntry.hidePassword') : t('authEntry.showPassword')}
                    >
                      {showConfirmPassword ? <VisibilityOffIcon /> : <VisibilityIcon />}
                    </IconButton>
                  </InputAdornment>
                ),
              }}
            />

            {feedback && (
              <Stack spacing={1}>
                <Alert severity="error">{feedback.message}</Alert>
                <Button component={RouterLink} to={recoveryPath}>{t('authEntry.requestNewLink')}</Button>
              </Stack>
            )}

            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
              <Button type="submit" variant="contained" disabled={resetMutation.isPending}>
                {resetMutation.isPending ? t('authEntry.saving') : t('authEntry.savePassword')}
              </Button>
              <Button component={RouterLink} to={loginPath} variant="outlined" disabled={resetMutation.isPending}>
                {t('authEntry.backLogin')}
              </Button>
            </Stack>
          </Stack>
        </CardContent>
      </Card>
    </Box>
  );
}
