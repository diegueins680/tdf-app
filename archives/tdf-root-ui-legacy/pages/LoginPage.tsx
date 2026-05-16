import { useEffect, useMemo, useState } from 'react';
import { Link, useLocation, useNavigate } from 'react-router-dom';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Checkbox,
  FormControlLabel,
  Stack,
  Tab,
  Tabs,
  TextField,
  Typography,
} from '@mui/material';
import { useAuth } from '../auth/AuthProvider';
import { Logo } from '../components/Logo';

type LocationState = {
  from?: { pathname: string };
};

export default function LoginPage() {
  const { login, loginWithToken, isLoading } = useAuth();
  const navigate = useNavigate();
  const location = useLocation();
  const redirectTo = (location.state as LocationState | undefined)?.from?.pathname ?? '/parties';

  const [mode, setMode] = useState<'password' | 'token'>('password');
  const [username, setUsername] = useState('');
  const [password, setPassword] = useState('');
  const [rememberPassword, setRememberPassword] = useState(true);
  const [tokenValue, setTokenValue] = useState('');
  const [tokenLabel, setTokenLabel] = useState('');
  const [rememberToken, setRememberToken] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    setError(null);
  }, [mode]);

  const redirect = useMemo(() => redirectTo, [redirectTo]);

  const handleSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setError(null);
    try {
      if (mode === 'password') {
        await login(username.trim(), password, rememberPassword);
      } else {
        await loginWithToken(tokenValue.trim(), tokenLabel.trim() || 'API Token', rememberToken);
      }
      navigate(redirect, { replace: true });
    } catch (err) {
      const message = err instanceof Error ? err.message : 'Error al iniciar sesión';
      setError(message || 'Credenciales inválidas');
    }
  };

  return (
    <Box sx={{ minHeight: '100vh', display: 'flex', alignItems: 'center', justifyContent: 'center', bgcolor: 'grey.100', p: 2 }}>
      <Card sx={{ width: 360 }}>
        <CardContent>
          <Box sx={{ display: 'flex', justifyContent: 'center', pb: 2 }}>
            <Logo
              alt="TDF Records"
              style={{ width: '12rem', maxWidth: '100%', height: 'auto', maxHeight: 'none' }}
            />
          </Box>
          <Stack component="form" onSubmit={handleSubmit} spacing={2}>
            <Box>
              <Typography variant="h5" fontWeight={700}>Iniciar sesión</Typography>
              <Typography variant="body2" color="text.secondary">
                Usa tus credenciales o un token emitido por TDF Records
              </Typography>
            </Box>
            <Tabs
              value={mode}
              onChange={(_event, value) => setMode(value)}
              aria-label="Método de autenticación"
              variant="fullWidth"
            >
              <Tab label="Contraseña" value="password" />
              <Tab label="Token API" value="token" />
            </Tabs>
            {error && <Alert severity="error">{error}</Alert>}
            {mode === 'password' ? (
              <>
                <TextField
                  label="Usuario"
                  value={username}
                  onChange={(e) => setUsername(e.target.value)}
                  autoComplete="username"
                  disabled={isLoading}
                  required
                  autoFocus
                />
                <TextField
                  label="Contraseña"
                  type="password"
                  value={password}
                  onChange={(e) => setPassword(e.target.value)}
                  autoComplete="current-password"
                  disabled={isLoading}
                  required
                />
                <FormControlLabel
                  control={(
                    <Checkbox
                      checked={rememberPassword}
                      onChange={(event) => setRememberPassword(event.target.checked)}
                      disabled={isLoading}
                    />
                  )}
                  label="Recordarme en este dispositivo"
                />
              </>
            ) : (
              <>
                <TextField
                  label="Token API"
                  value={tokenValue}
                  onChange={(e) => setTokenValue(e.target.value)}
                  multiline
                  minRows={3}
                  disabled={isLoading}
                  required
                  helperText="Pega el token generado desde la consola de administradores"
                  autoFocus
                />
                <TextField
                  label="Etiqueta para mostrar"
                  value={tokenLabel}
                  onChange={(e) => setTokenLabel(e.target.value)}
                  disabled={isLoading}
                  placeholder="Ej: Automatización Studio"
                />
                <FormControlLabel
                  control={(
                    <Checkbox
                      checked={rememberToken}
                      onChange={(event) => setRememberToken(event.target.checked)}
                      disabled={isLoading}
                    />
                  )}
                  label="Guardar token de forma persistente"
                />
              </>
            )}
            <Button type="submit" variant="contained" size="large" disabled={isLoading}>
              {isLoading ? 'Ingresando…' : mode === 'password' ? 'Ingresar' : 'Conectar token'}
            </Button>
            <Stack spacing={1} pt={1}>
              <Typography variant="body2" color="text.secondary" textAlign="center">
                ¿Olvidaste tu contraseña?{' '}
                <Box component={Link} to="/forgot-password" sx={{ fontWeight: 600 }} color="primary.main">
                  Recuperar acceso
                </Box>
              </Typography>
              <Typography variant="body2" color="text.secondary" textAlign="center">
                ¿No tienes cuenta?{' '}
                <Box component={Link} to="/signup" sx={{ fontWeight: 600 }} color="primary.main">
                  Crear cuenta
                </Box>
              </Typography>
              <Typography variant="body2" color="text.secondary" textAlign="center">
                ¿Buscas una clase de prueba?{' '}
                <Box component={Link} to="/trial" sx={{ fontWeight: 600 }} color="primary.main">
                  Solicitar trial
                </Box>
              </Typography>
            </Stack>
          </Stack>
        </CardContent>
      </Card>
    </Box>
  );
}
