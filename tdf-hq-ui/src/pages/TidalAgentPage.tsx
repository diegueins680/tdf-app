import { useCallback, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Divider,
  FormControl,
  FormControlLabel,
  InputLabel,
  MenuItem,
  Select,
  Stack,
  Switch,
  TextField,
  Typography,
} from '@mui/material';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import { buildDefaultConfig, extractTidalCode, tidalAgentRequest } from '../utils/tidalAgent';

interface HistoryItem {
  prompt: string;
  code: string;
}

type TidalTarget = 'd1' | 'd2' | 'd3' | 'd4';
const TIDAL_TARGET_OPTIONS: readonly TidalTarget[] = ['d1', 'd2', 'd3', 'd4'];
const isTidalTarget = (value: string): value is TidalTarget =>
  TIDAL_TARGET_OPTIONS.some((target) => target === value);

export default function TidalAgentPage() {
  const config = useMemo(buildDefaultConfig, []);
  const [prompt, setPrompt] = useState('');
  const [raw, setRaw] = useState<string | null>(null);
  const [code, setCode] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [copied, setCopied] = useState(false);
  const [history, setHistory] = useState<HistoryItem[]>([]);
  const [showRaw, setShowRaw] = useState(false);
  const [historyMode, setHistoryMode] = useState<'full' | 'code'>('full');
  const [target, setTarget] = useState<TidalTarget>('d1');
  const [pinned, setPinned] = useState<HistoryItem | null>(null);

  const exportRecentHistory = useCallback(async () => {
    const recent = history.slice(0, 5);
    if (recent.length === 0) return;
    const payload = recent.map((item, idx) => `#${idx + 1}\n${item.code}`).join('\n\n');
    try {
      await navigator.clipboard.writeText(payload);
      setCopied(true);
      setTimeout(() => setCopied(false), 1800);
    } catch (err) {
      console.warn('No se pudo exportar el historial', err);
      setError('No se pudo exportar el historial.');
    }
  }, [history]);

  const handleSubmit = async () => {
    if (!prompt.trim()) {
      setError('Escribe un prompt.');
      return;
    }
    setLoading(true);
    setError(null);
    setCopied(false);
    try {
      const content = await tidalAgentRequest(prompt.trim(), config);
      setRaw(content);
      const extracted = extractTidalCode(content);
      if (!extracted) {
        setCode(null);
        setError('La respuesta no contiene código Tidal utilizable.');
      } else {
        setCode(extracted);
        setHistory((prev) => [{ prompt: prompt.trim(), code: extracted }, ...prev].slice(0, 5));
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : 'No se pudo generar código.');
    } finally {
      setLoading(false);
    }
  };

  const handleCopy = async () => {
    if (!code) return;
    try {
      await navigator.clipboard.writeText(code);
      setCopied(true);
      setTimeout(() => setCopied(false), 1800);
    } catch {
      setCopied(false);
      setError('No se pudo copiar al portapapeles.');
    }
  };

  return (
    <Box sx={{ maxWidth: 960, mx: 'auto' }}>
      <Stack spacing={2}>
        <Stack spacing={0.5}>
          <Typography variant="overline" color="text.secondary">
            Herramientas · Audio
          </Typography>
          <Typography variant="h4" fontWeight={800}>
            Tidal Agent
          </Typography>
          <Typography variant="body1" color="text.secondary">
            Convierte prompts en código TidalCycles. Se aplica un filtro para quedarnos solo con líneas seguras (d1–d4, hush, cps/bps).
          </Typography>
        </Stack>

        <Card variant="outlined">
          <CardContent>
            <Stack spacing={2}>
              <TextField
                label="Describe el patrón"
                placeholder="ej. groove techno con bombo a negras y hats sincopados"
                multiline
                minRows={3}
                value={prompt}
                onChange={(e) => setPrompt(e.target.value)}
                fullWidth
                disabled={loading}
              />
              <Stack direction="row" spacing={1}>
                <Button
                  variant="contained"
                  startIcon={<PlayArrowIcon />}
                  onClick={() => {
                    void handleSubmit();
                  }}
                  disabled={loading}
                >
                  {loading ? 'Generando…' : 'Generar código'}
                </Button>
                {history.length > 0 && (
                  <Button
                    variant="outlined"
                    startIcon={<ContentCopyIcon />}
                    onClick={() => {
                      void exportRecentHistory();
                    }}
                  >
                    Exportar últimos
                  </Button>
                )}
                {code && (
                  <Button
                    variant="outlined"
                    startIcon={<ContentCopyIcon />}
                    onClick={() => {
                      void handleCopy();
                    }}
                  >
                    {copied ? 'Copiado' : 'Copiar'}
                  </Button>
                )}
                {code && (
                  <Button
                    variant="outlined"
                    startIcon={<ContentCopyIcon />}
                    onClick={() => {
                      const wrapped = `${target} $ (${code})`;
                      void navigator.clipboard.writeText(wrapped);
                    }}
                  >
                    Copiar con {target} $
                  </Button>
                )}
              </Stack>
              <FormControl size="small" sx={{ maxWidth: 160 }}>
                <InputLabel id="tidal-target-label">Destino</InputLabel>
                <Select
                  labelId="tidal-target-label"
                  label="Destino"
                  value={target}
                  onChange={(e) => {
                    const next = e.target.value.trim();
                    setTarget(isTidalTarget(next) ? next : 'd1');
                  }}
                >
                  {TIDAL_TARGET_OPTIONS.map((targetOption) => (
                    <MenuItem key={targetOption} value={targetOption}>{targetOption}</MenuItem>
                  ))}
                </Select>
              </FormControl>

              {error && <Alert severity="error">{error}</Alert>}

              <Divider />

              <Stack spacing={1}>
                <Typography variant="subtitle2" color="text.secondary">
                  Código Tidal resultante
                </Typography>
                <TextField
                  value={code ?? ''}
                  placeholder="El resultado aparecerá aquí..."
                  multiline
                  minRows={6}
                  fullWidth
                  InputProps={{ readOnly: true }}
                />
              </Stack>

              {history.length > 0 && (
                <Stack spacing={1}>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <Typography variant="subtitle2" color="text.secondary">
                      Historial rápido
                    </Typography>
                    <Chip label={history.length} size="small" color="info" />
                  </Stack>
                  {pinned && (
                    <Card variant="outlined" sx={{ borderColor: 'primary.main' }}>
                      <CardContent sx={{ display: 'flex', flexDirection: 'column', gap: 0.75 }}>
                        <Stack direction="row" spacing={1} alignItems="center">
                          <Typography variant="subtitle2" color="text.secondary">
                            Fijado
                          </Typography>
                          <Chip label="Pinned" size="small" color="primary" />
                        </Stack>
                        <Typography variant="body2" color="text.secondary">
                          {pinned.prompt}
                        </Typography>
                        <Box
                          sx={{
                            bgcolor: 'rgba(148,163,184,0.08)',
                            border: '1px dashed',
                            borderColor: 'divider',
                            borderRadius: 1,
                            p: 1,
                            fontFamily: 'monospace',
                            fontSize: 12,
                            whiteSpace: 'pre-line',
                          }}
                        >
                          {pinned.code}
                        </Box>
                        <Stack direction="row" spacing={1}>
                          <Button
                            size="small"
                            startIcon={<ContentCopyIcon fontSize="small" />}
                            onClick={() => {
                              navigator.clipboard.writeText(pinned.code).catch((err) => console.warn(err));
                            }}
                          >
                            Copiar
                          </Button>
                          <Button size="small" onClick={() => setPinned(null)}>
                            Quitar pin
                          </Button>
                        </Stack>
                      </CardContent>
                    </Card>
                  )}
                  <Stack direction="row" spacing={1} alignItems="center">
                    <Button
                      size="small"
                      variant={historyMode === 'full' ? 'contained' : 'outlined'}
                      onClick={() => setHistoryMode('full')}
                    >
                      Prompt + código
                    </Button>
                    <Button
                      size="small"
                      variant={historyMode === 'code' ? 'contained' : 'outlined'}
                      onClick={() => setHistoryMode('code')}
                    >
                      Solo código
                    </Button>
                    <Button size="small" onClick={() => setHistory([])}>
                      Limpiar historial
                    </Button>
                  </Stack>
                  <Stack spacing={1}>
                    {history.map((item, idx) => (
                      <Card key={`${item.prompt}-${idx}`} variant="outlined">
                        <CardContent sx={{ display: 'flex', flexDirection: 'column', gap: 0.75 }}>
                          {historyMode === 'full' && <Typography variant="body2">{item.prompt}</Typography>}
                          <Box
                            sx={{
                              bgcolor: 'rgba(148,163,184,0.08)',
                              border: '1px dashed',
                              borderColor: 'divider',
                              borderRadius: 1,
                              p: 1,
                              fontFamily: 'monospace',
                              fontSize: 12,
                              whiteSpace: 'pre-line',
                            }}
                          >
                            {item.code}
                          </Box>
                          <Button
                            size="small"
                            startIcon={<ContentCopyIcon fontSize="small" />}
                            onClick={() => {
                              navigator.clipboard
                                .writeText(item.code)
                                .catch((err) => console.warn('No se pudo copiar el historial', err));
                            }}
                          >
                            Copiar
                          </Button>
                          <Button
                            size="small"
                            onClick={() => {
                              setPrompt(item.prompt);
                              setCode(item.code);
                            }}
                          >
                            Usar prompt
                          </Button>
                          <Button
                            size="small"
                            variant={pinned?.code === item.code ? 'contained' : 'outlined'}
                            onClick={() => {
                              setPinned((prev) => (prev?.code === item.code ? null : item));
                            }}
                          >
                            {pinned?.code === item.code ? 'Quitar pin' : 'Fijar'}
                          </Button>
                          <Button
                            size="small"
                            variant="outlined"
                            onClick={() => {
                              const wrapped = `${target} $ (${item.code})`;
                              navigator.clipboard
                                .writeText(wrapped)
                                .catch((err) => console.warn('No se pudo copiar con destino', err));
                              setShowRaw(false);
                            }}
                          >
                            Copiar con {target} $ y cerrar raw
                          </Button>
                        </CardContent>
                      </Card>
                    ))}
                  </Stack>
                </Stack>
              )}

              {raw && showRaw && (
                <Stack spacing={1}>
                  <Typography variant="subtitle2" color="text.secondary">
                    Respuesta completa del modelo (depuración)
                  </Typography>
                  <TextField value={raw} multiline minRows={4} fullWidth InputProps={{ readOnly: true }} />
                </Stack>
              )}

              {raw && (
                <FormControlLabel
                  control={<Switch checked={showRaw} onChange={(e) => setShowRaw(e.target.checked)} />}
                  label="Ver respuesta completa"
                />
              )}
            </Stack>
          </CardContent>
        </Card>

        <Alert severity="info" variant="outlined">
          Env vars: `VITE_API_BASE` en el frontend, `OPENAI_API_KEY` en el backend. Opcional `VITE_TIDAL_AGENT_MODEL`.
        </Alert>
      </Stack>
    </Box>
  );
}
