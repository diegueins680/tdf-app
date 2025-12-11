import { useCallback, useEffect, useState } from 'react';
import { Link as RouterLink, useSearchParams } from 'react-router-dom';
import {
  Alert,
  Box,
  Button,
  Chip,
  Container,
  IconButton,
  InputAdornment,
  Paper,
  Stack,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import VisibilityIcon from '@mui/icons-material/Visibility';
import VisibilityOffIcon from '@mui/icons-material/VisibilityOff';
import PublicBrandBar from '../components/PublicBrandBar';
import { LiveSessionIntakeForm } from './LiveSessionIntakePage';
import { useSession } from '../session/SessionContext';

export default function LiveSessionPublicPage() {
  const [sp] = useSearchParams();
  const tokenFromQuery = sp.get('token') ?? sp.get('t') ?? '';
  const { session, login, setApiToken } = useSession();
  const [accessCode, setAccessCode] = useState(() => tokenFromQuery);
  const [codeStatus, setCodeStatus] = useState<'idle' | 'validating' | 'valid' | 'invalid'>('idle');
  const [validationMessage, setValidationMessage] = useState<string | null>(null);
  const [lastValidatedCode, setLastValidatedCode] = useState('');
  const canUseForm = codeStatus === 'valid';
  const [showCode, setShowCode] = useState(false);

  const validateAccessCode = useCallback(async (codeOverride?: string) => {
    const code = (codeOverride ?? accessCode).trim();
    if (!code) {
      setValidationMessage('Ingresa un código válido.');
      setCodeStatus('invalid');
      return;
    }
    setCodeStatus('validating');
    setValidationMessage(null);
    try {
      const base = import.meta.env.VITE_API_BASE ?? '';
      if (base) {
        const res = await fetch(`${base}/live-sessions/intake/ping`, {
          headers: { Authorization: `Bearer ${code}` },
        });
        if (!res.ok) {
          throw new Error('invalid');
        }
      }
      setCodeStatus('valid');
      setLastValidatedCode(code);
    } catch {
      setCodeStatus('invalid');
      setLastValidatedCode('');
      setValidationMessage('Código inválido o expirado. Solicita uno nuevo.');
    }
  }, [accessCode]);

  useEffect(() => {
    if (tokenFromQuery) {
      setAccessCode(tokenFromQuery);
      setCodeStatus('idle');
    }
  }, [tokenFromQuery]);

  useEffect(() => {
    if (tokenFromQuery && codeStatus === 'idle') {
      void validateAccessCode();
    }
  }, [codeStatus, tokenFromQuery, validateAccessCode]);

  useEffect(() => {
    const code = accessCode.trim();
    if (!code || codeStatus !== 'valid') return;
    if (!session) {
      login(
        {
          username: 'live-session-intake',
          displayName: 'Live Sessions Intake',
          roles: [],
          apiToken: code,
        },
        { remember: false },
      );
    } else if (!session.apiToken || session.apiToken !== code) {
      setApiToken(code);
    }
  }, [accessCode, codeStatus, session, login, setApiToken]);

  useEffect(() => {
    const code = accessCode.trim();
    if (!code) {
      setLastValidatedCode('');
      if (codeStatus !== 'idle') {
        setCodeStatus('idle');
      }
      return;
    }
    if (codeStatus === 'validating') return;
    if (codeStatus === 'valid' && code === lastValidatedCode) return;
    const handle = setTimeout(() => {
      void validateAccessCode();
    }, 450);
    return () => clearTimeout(handle);
  }, [accessCode, codeStatus, lastValidatedCode, validateAccessCode]);

  return (
    <Box
      sx={{
        minHeight: '100vh',
        background: 'radial-gradient(circle at 20% 20%, rgba(56,189,248,0.12), transparent 25%), radial-gradient(circle at 80% 0%, rgba(167,139,250,0.12), transparent 26%), linear-gradient(135deg, #0b1224, #0f172a)',
        color: '#e2e8f0',
        py: { xs: 4, md: 6 },
      }}
    >
      <Container maxWidth="lg">
        <Stack spacing={3}>
          <Box sx={{ display: 'flex', justifyContent: 'center' }}>
            <PublicBrandBar tagline="Live Sessions · Intake" compact />
          </Box>
          <Paper
            elevation={0}
            sx={{
              p: { xs: 3, md: 4 },
              bgcolor: 'rgba(15,23,42,0.82)',
              border: '1px solid rgba(255,255,255,0.08)',
              backdropFilter: 'blur(12px)',
              color: '#e2e8f0',
            }}
          >
            <Stack spacing={2}>
              <Stack spacing={1}>
                <Chip
                  label="Aplicación banda"
                  sx={{
                    bgcolor: 'rgba(255,255,255,0.08)',
                    color: '#cbd5f5',
                    border: '1px solid rgba(255,255,255,0.14)',
                    alignSelf: 'flex-start',
                  }}
                  size="small"
                />
                <Typography variant="h4" fontWeight={800}>
                  Sube tu banda a las TDF Live Sessions
                </Typography>
                <Typography variant="body1" color="rgba(226,232,240,0.85)">
                  Comparte quiénes tocan, tus redes y la disponibilidad tentativa. Si ya trabajamos juntos, busca los nombres para autocompletar los datos.
                </Typography>
              </Stack>
              <Stack spacing={1.5}>
                <Typography variant="subtitle2" color="rgba(226,232,240,0.85)">
                  Ingresa el código de acceso que te compartimos para autenticar el envío.
                </Typography>
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                  <TextField
                    label="Código de acceso"
                    type="password"
                    value={accessCode}
                    onChange={(e) => {
                      setAccessCode(e.target.value);
                      setCodeStatus('idle');
                      setValidationMessage(null);
                    }}
                    onBlur={() => {
                      void validateAccessCode();
                    }}
                    onPaste={(e) => {
                      const pasted = e.clipboardData.getData('text');
                      setAccessCode(pasted);
                      setCodeStatus('idle');
                      setValidationMessage(null);
                      setTimeout(() => {
                        void validateAccessCode(pasted);
                      }, 0);
                    }}
                    placeholder="Pega el código recibido"
                    fullWidth
                    InputLabelProps={{ sx: { color: '#cbd5f5' } }}
                    type={showCode ? 'text' : 'password'}
                    error={codeStatus === 'invalid'}
                    helperText={codeStatus === 'invalid' ? validationMessage ?? 'No pudimos validar el código.' : ' '}
                    InputProps={{
                      endAdornment: (
                        <InputAdornment position="end">
                          <Tooltip title={showCode ? 'Ocultar código' : 'Mostrar código'}>
                            <IconButton
                              edge="end"
                              onClick={() => setShowCode((prev) => !prev)}
                              aria-label={showCode ? 'Ocultar código' : 'Mostrar código'}
                            >
                              {showCode ? <VisibilityOffIcon /> : <VisibilityIcon />}
                            </IconButton>
                          </Tooltip>
                        </InputAdornment>
                      ),
                    }}
                    onFocus={() => {
                      if (codeStatus === 'invalid') setCodeStatus('idle');
                      setValidationMessage(null);
                    }}
                  />
                  <Button
                    variant="contained"
                    color="secondary"
                    onClick={() => {
                      void validateAccessCode();
                    }}
                    disabled={codeStatus === 'validating'}
                  >
                    {codeStatus === 'validating'
                      ? 'Validando…'
                      : codeStatus === 'valid'
                        ? 'Código validado'
                        : 'Validar código'}
                  </Button>
                  <Button
                    variant="outlined"
                    color="inherit"
                    component={RouterLink}
                    to="/feedback?topic=live-sessions"
                  >
                    Solicitar acceso
                  </Button>
                </Stack>
                {validationMessage && (
                  <Alert severity="error" sx={{ bgcolor: 'rgba(248,113,113,0.12)', color: '#fecdd3' }}>
                    {validationMessage}
                  </Alert>
                )}
                {codeStatus === 'valid' && (
                  <Alert severity="success" sx={{ bgcolor: 'rgba(34,197,94,0.12)', color: '#bbf7d0' }}>
                    Código verificado. Puedes completar y enviar el formulario.
                  </Alert>
                )}
              </Stack>
              <Box sx={{ opacity: canUseForm ? 1 : 0.35, pointerEvents: canUseForm ? 'auto' : 'none' }}>
                <LiveSessionIntakeForm variant="public" requireTerms />
              </Box>
            </Stack>
          </Paper>
        </Stack>
      </Container>
    </Box>
  );
}
