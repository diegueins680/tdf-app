import { useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  IconButton,
  InputAdornment,
  Stack,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import VisibilityIcon from '@mui/icons-material/Visibility';
import VisibilityOffIcon from '@mui/icons-material/VisibilityOff';
import { Link as RouterLink } from 'react-router-dom';
import { loginRequest } from '../api/auth';
import { useSession } from '../session/SessionContext';
import { deriveModulesFromRoles } from '../components/SidebarNav';

interface TokenDetails {
  username: string;
  partyId?: number;
  roles: string[];
  modules: string[];
}

const resolveApiBase = () => {
  const configured = import.meta.env.VITE_API_BASE ?? '';
  if (configured.trim()) return configured;
  if (typeof window !== 'undefined') return window.location.origin;
  return '';
};

export default function AdminTokenPage() {
  const { session } = useSession();
  const [username, setUsername] = useState(session?.username ?? '');
  const [password, setPassword] = useState('');
  const [showPassword, setShowPassword] = useState(false);
  const [token, setToken] = useState<string | null>(null);
  const [details, setDetails] = useState<TokenDetails | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [notice, setNotice] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  const modules = useMemo(() => {
    const provided = session?.modules ?? [];
    const fromRoles = deriveModulesFromRoles(session?.roles);
    const baseSet = new Set([...provided, ...fromRoles].map((m) => m.toLowerCase()));
    if (baseSet.has('packages')) {
      baseSet.add('ops');
      baseSet.add('label');
    }
    if (baseSet.has('ops')) {
      baseSet.add('packages');
    }
    return baseSet;
  }, [session?.modules, session?.roles]);
  const hasAdmin = modules.has('admin');
  const apiBase = useMemo(resolveApiBase, []);

  const handleCopy = async (value: string, label: string) => {
    try {
      await navigator.clipboard.writeText(value);
      setNotice(`${label} copiado al portapapeles.`);
      setError(null);
    } catch (err) {
      console.warn('No se pudo copiar', err);
      setNotice(null);
      setError('No se pudo copiar al portapapeles.');
    }
  };

  const handleRequestToken = async () => {
    const trimmedUser = username.trim();
    if (!trimmedUser || !password.trim()) {
      setError('Completa usuario y contraseña para generar el token.');
      return;
    }
    setLoading(true);
    setError(null);
    setNotice(null);
    try {
      const response = await loginRequest({ username: trimmedUser, password });
      setToken(response.token);
      setDetails({
        username: trimmedUser,
        partyId: response.partyId,
        roles: response.roles ?? [],
        modules: response.modules ?? [],
      });
      setNotice('Token generado correctamente.');
    } catch (err) {
      setToken(null);
      setDetails(null);
      setError(err instanceof Error ? err.message : 'No se pudo generar el token.');
    } finally {
      setLoading(false);
    }
  };

  const handleUseSessionToken = () => {
    if (!session?.apiToken) {
      setError('No hay un token activo en esta sesión.');
      return;
    }
    setToken(session.apiToken);
    setDetails({
      username: session.username,
      partyId: session.partyId,
      roles: session.roles ?? [],
      modules: session.modules ?? [],
    });
    setNotice('Token activo cargado.');
    setError(null);
  };

  const handleClear = () => {
    setPassword('');
    setToken(null);
    setDetails(null);
    setError(null);
    setNotice(null);
  };

  if (!hasAdmin) {
    return (
      <Stack spacing={2}>
        <Typography variant="h4" fontWeight={800}>
          Token API
        </Typography>
        <Alert severity="warning">
          Este recurso es solo para administradores.
        </Alert>
        <Button component={RouterLink} to="/inicio" variant="contained">
          Volver al inicio
        </Button>
      </Stack>
    );
  }

  return (
    <Box sx={{ maxWidth: 860, mx: 'auto' }}>
      <Stack spacing={2.5}>
        <Stack spacing={0.5}>
          <Typography variant="overline" color="text.secondary">
            Recursos · Admin
          </Typography>
          <Typography variant="h4" fontWeight={800}>
            Token API
          </Typography>
          <Typography color="text.secondary">
            Genera y copia bearer tokens para scripts internos o pruebas rápidas. No guardamos credenciales.
          </Typography>
        </Stack>

        <Card variant="outlined">
          <CardContent>
            <Stack spacing={2}>
              <Stack spacing={1}>
                <Typography variant="h6" fontWeight={700}>
                  Solicitar token
                </Typography>
                <Stack direction="row" spacing={1} flexWrap="wrap">
                  <Chip label={`API: ${apiBase || 'No configurado'}`} size="small" variant="outlined" />
                  <Chip label="POST /login" size="small" variant="outlined" />
                </Stack>
              </Stack>
              <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                <TextField
                  label="Usuario"
                  value={username}
                  onChange={(e) => setUsername(e.target.value)}
                  fullWidth
                  autoComplete="username"
                />
                <TextField
                  label="Contraseña"
                  value={password}
                  onChange={(e) => setPassword(e.target.value)}
                  fullWidth
                  type={showPassword ? 'text' : 'password'}
                  autoComplete="current-password"
                  InputProps={{
                    endAdornment: (
                      <InputAdornment position="end">
                        <Tooltip title={showPassword ? 'Ocultar contraseña' : 'Mostrar contraseña'}>
                          <IconButton
                            edge="end"
                            onClick={() => setShowPassword((prev) => !prev)}
                            aria-label={showPassword ? 'Ocultar contraseña' : 'Mostrar contraseña'}
                          >
                            {showPassword ? <VisibilityOffIcon /> : <VisibilityIcon />}
                          </IconButton>
                        </Tooltip>
                      </InputAdornment>
                    ),
                  }}
                />
              </Stack>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                <Button
                  variant="contained"
                  onClick={() => {
                    void handleRequestToken();
                  }}
                  disabled={loading}
                >
                  {loading ? 'Generando…' : 'Generar token'}
                </Button>
                <Button
                  variant="outlined"
                  onClick={handleUseSessionToken}
                  disabled={!session?.apiToken}
                >
                  Usar token activo
                </Button>
                <Button variant="text" onClick={handleClear}>
                  Limpiar
                </Button>
              </Stack>
            </Stack>
          </CardContent>
        </Card>

        {error && (
          <Alert severity="error">{error}</Alert>
        )}
        {notice && (
          <Alert severity="success">{notice}</Alert>
        )}

        {token && (
          <Card variant="outlined">
            <CardContent>
              <Stack spacing={2}>
                <Stack spacing={1}>
                  <Typography variant="h6" fontWeight={700}>
                    Token generado
                  </Typography>
                  <Stack direction="row" spacing={1} flexWrap="wrap">
                    {details?.username && (
                      <Chip label={`Usuario: ${details.username}`} size="small" variant="outlined" />
                    )}
                    {details?.partyId && (
                      <Chip label={`Party #${details.partyId}`} size="small" variant="outlined" />
                    )}
                  </Stack>
                </Stack>
                <TextField
                  label="Bearer token"
                  value={token}
                  fullWidth
                  multiline
                  minRows={2}
                  InputProps={{
                    readOnly: true,
                    sx: { fontFamily: 'monospace' },
                  }}
                  helperText="Úsalo como: Authorization: Bearer <token>"
                />
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                  <Button
                    variant="contained"
                    startIcon={<ContentCopyIcon />}
                    onClick={() => {
                      void handleCopy(token, 'Token');
                    }}
                  >
                    Copiar token
                  </Button>
                  <Button
                    variant="outlined"
                    startIcon={<ContentCopyIcon />}
                    onClick={() => {
                      void handleCopy(`Bearer ${token}`, 'Bearer');
                    }}
                  >
                    Copiar Bearer
                  </Button>
                </Stack>
                {details && (details.roles.length > 0 || details.modules.length > 0) && (
                  <Stack spacing={1}>
                    {details.roles.length > 0 && (
                      <Stack direction="row" spacing={1} flexWrap="wrap">
                        {details.roles.map((role) => (
                          <Chip key={`role-${role}`} label={role} size="small" />
                        ))}
                      </Stack>
                    )}
                    {details.modules.length > 0 && (
                      <Stack direction="row" spacing={1} flexWrap="wrap">
                        {details.modules.map((module) => (
                          <Chip key={`module-${module}`} label={module} size="small" variant="outlined" />
                        ))}
                      </Stack>
                    )}
                  </Stack>
                )}
              </Stack>
            </CardContent>
          </Card>
        )}
      </Stack>
    </Box>
  );
}
