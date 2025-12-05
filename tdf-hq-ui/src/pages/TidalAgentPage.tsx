import { useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Divider,
  FormControlLabel,
  Stack,
  Switch,
  TextField,
  Typography,
} from '@mui/material';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import { buildDefaultConfig, extractTidalCode, tidalAgentRequest } from '../utils/tidalAgent';

type HistoryItem = { prompt: string; code: string };

export default function TidalAgentPage() {
  const { config, error: configError } = useMemo(buildDefaultConfig, []);
  const [prompt, setPrompt] = useState('');
  const [raw, setRaw] = useState<string | null>(null);
  const [code, setCode] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [copied, setCopied] = useState(false);
  const [history, setHistory] = useState<HistoryItem[]>([]);
  const [showRaw, setShowRaw] = useState(false);

  const handleSubmit = async () => {
    if (!config) return;
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

        {configError && <Alert severity="warning">{configError}</Alert>}

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
                disabled={loading || !config}
              />
              <Stack direction="row" spacing={1}>
                <Button
                  variant="contained"
                  startIcon={<PlayArrowIcon />}
                  onClick={handleSubmit}
                  disabled={loading || !config}
                >
                  {loading ? 'Generando…' : 'Generar código'}
                </Button>
                {code && (
                  <Button variant="outlined" startIcon={<ContentCopyIcon />} onClick={handleCopy}>
                    {copied ? 'Copiado' : 'Copiar'}
                  </Button>
                )}
              </Stack>

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
                  <Typography variant="subtitle2" color="text.secondary">
                    Historial rápido
                  </Typography>
                  <Stack spacing={1}>
                    {history.map((item, idx) => (
                      <Card key={`${item.prompt}-${idx}`} variant="outlined">
                        <CardContent sx={{ display: 'flex', flexDirection: 'column', gap: 0.75 }}>
                          <Typography variant="body2">{item.prompt}</Typography>
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
                            onClick={() => navigator.clipboard.writeText(item.code).catch(() => {})}
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
          Env vars: define `VITE_TIDAL_AGENT_API_KEY` (o `VITE_OPENAI_API_KEY`), opcional `VITE_TIDAL_AGENT_API_URL` y `VITE_TIDAL_AGENT_MODEL`.
        </Alert>
      </Stack>
    </Box>
  );
}
