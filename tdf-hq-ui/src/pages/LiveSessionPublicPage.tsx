import { useCallback, useEffect, useRef, useState } from 'react';
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
import { resolveApiBase } from '../config/apiBase';

export default function LiveSessionPublicPage() {
  const [sp] = useSearchParams();
  const tokenFromQuery = sp.get('token') ?? sp.get('t') ?? '';
  const [accessCode, setAccessCode] = useState(() => tokenFromQuery);
  const [codeStatus, setCodeStatus] = useState<'idle' | 'validating' | 'valid' | 'invalid'>('idle');
  const [validationMessage, setValidationMessage] = useState<string | null>(null);
  const [lastValidatedCode, setLastValidatedCode] = useState('');
  const [verifiedPartyId, setVerifiedPartyId] = useState<number>();
  const canUseForm = codeStatus === 'valid' && accessCode.trim() === lastValidatedCode;
  const validationGeneration = useRef(0);
  const pendingValidation = useRef<AbortController | null>(null);
  const pendingDebounce = useRef<number | null>(null);
  const invalidateValidation = useCallback(() => {
    validationGeneration.current += 1;
    pendingValidation.current?.abort();
    if (pendingDebounce.current !== null) window.clearTimeout(pendingDebounce.current);
  }, []);
  useEffect(() => invalidateValidation, [invalidateValidation]);
  const [showCode, setShowCode] = useState(false);

  const validateAccessCode = useCallback(async (codeOverride?: string) => {
    const code = (codeOverride ?? accessCode).trim();
    invalidateValidation();
    const generation = validationGeneration.current;
    if (!code) {
      setValidationMessage('Ingresa un código válido.');
      setCodeStatus('invalid');
      return;
    }
    setCodeStatus('validating');
    setValidationMessage(null);
    const controller = new AbortController();
    pendingValidation.current = controller;
    const timeout = window.setTimeout(() => controller.abort(), 30_000);
    try {
      const res = await fetch(`${resolveApiBase()}/session`, {
        headers: { Authorization: `Bearer ${code}` },
        credentials: 'omit',
        signal: controller.signal,
      });
      if (!res.ok) throw new Error('invalid');
      const account: unknown = await res.json();
      if (!account || typeof account !== 'object' || !('partyId' in account)
        || !Number.isSafeInteger(account.partyId) || Number(account.partyId) <= 0) {
        throw new Error('invalid');
      }
      if (generation !== validationGeneration.current) return;
      setVerifiedPartyId(Number(account.partyId));
      setCodeStatus('valid');
      setLastValidatedCode(code);
    } catch {
      if (generation !== validationGeneration.current) return;
      setCodeStatus('invalid');
      setLastValidatedCode('');
      setValidationMessage('No pudimos validar el código. Comprueba el código y la conexión e inténtalo de nuevo.');
    } finally {
      window.clearTimeout(timeout);
      if (pendingValidation.current === controller) pendingValidation.current = null;
    }
  }, [accessCode, invalidateValidation]);

  useEffect(() => {
    invalidateValidation();
    setAccessCode(tokenFromQuery);
    setCodeStatus('idle');
  }, [tokenFromQuery, invalidateValidation]);

  useEffect(() => {
    const code = accessCode.trim();
    if (!code) {
      setLastValidatedCode('');
      setCodeStatus('idle');
      return;
    }
    const handle = window.setTimeout(() => { void validateAccessCode(code); }, 450);
    pendingDebounce.current = handle;
    return () => window.clearTimeout(handle);
  }, [accessCode, validateAccessCode]);

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
            <PublicBrandBar tagline="Sesiones en vivo · Formulario" compact />
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
                  label="Postulación de banda"
                  sx={{
                    bgcolor: 'rgba(255,255,255,0.08)',
                    color: '#cbd5f5',
                    border: '1px solid rgba(255,255,255,0.14)',
                    alignSelf: 'flex-start',
                  }}
                  size="small"
                />
                <Typography variant="h4" fontWeight={800}>
                  Postula tu banda a las sesiones en vivo de TDF
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
                    value={accessCode}
                    onChange={(e) => {
                      invalidateValidation();
                      setAccessCode(e.target.value);
                      setCodeStatus('idle');
                      setValidationMessage(null);
                    }}
                    onBlur={() => {
                      void validateAccessCode();
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
              <Box component="fieldset" disabled={!canUseForm} sx={{ minWidth: 0, m: 0, p: 0, border: 0 }}>
                <LiveSessionIntakeForm key={verifiedPartyId ?? 'unverified'} variant="public" draftOwner={verifiedPartyId} accessCode={canUseForm ? lastValidatedCode : undefined} />
              </Box>
            </Stack>
          </Paper>
        </Stack>
      </Container>
    </Box>
  );
}
