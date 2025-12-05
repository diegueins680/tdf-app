import { useEffect, useMemo, useRef, useState } from 'react';
import {
  Box,
  Card,
  CardContent,
  Chip,
  Collapse,
  Divider,
  IconButton,
  LinearProgress,
  Stack,
  TextField,
  Typography,
  Tooltip,
  Button,
} from '@mui/material';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import PauseIcon from '@mui/icons-material/Pause';
import ExpandLessIcon from '@mui/icons-material/ExpandLess';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import RadioIcon from '@mui/icons-material/Radio';
import GraphicEqIcon from '@mui/icons-material/GraphicEq';
import VolumeOffIcon from '@mui/icons-material/VolumeOff';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import { generateTidalCode } from '../utils/tidalAgent';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';

interface Prompt {
  text: string;
  author?: string;
  createdAt?: string;
  code?: string;
}

interface Station {
  id: string;
  name: string;
  streamUrl: string;
  region?: string;
  mood: string;
  prompts: Prompt[];
}

const CURATED_STATIONS: Station[] = [
  {
    id: 'cosmic-cycles',
    name: 'Cosmic Cycles',
    mood: 'Downtempo / Ambient',
    streamUrl: 'https://icecast.radiofrance.fr/fip-webradio8.mp3', // placeholder stream
    prompts: [
      { text: 'Paisajes sonoros nocturnos con sintes lentos', author: 'Agente', createdAt: '2025-12-01' },
      { text: 'Texturas granulares inspiradas en lluvia en Quito', createdAt: '2025-12-02' },
    ],
  },
  {
    id: 'andina-bass',
    name: 'Andina Bass',
    mood: 'Afro / Bassline',
    streamUrl: 'https://streams.ilovemusic.de/iloveradio7.mp3', // placeholder stream
    prompts: [
      { text: 'Kick UKG con quenas procesadas en Tidal', author: 'User', createdAt: '2025-12-03' },
      { text: 'Dembow con delays quebrados y filtros vivos', author: 'Agente' },
    ],
  },
  {
    id: 'club-late',
    name: 'Club Late',
    mood: 'Techno / Hypnotic',
    streamUrl: 'https://stream.live.vc.bbcmedia.co.uk/bbc_radio_one', // placeholder stream
    prompts: [
      { text: 'Polirritmias 5/4 con hats abiertos', createdAt: '2025-12-03' },
      { text: 'Bajos en sinusoide sidechain con pads brillantes' },
    ],
  },
  {
    id: 'bbc-6music',
    name: 'BBC Radio 6 Music (UK)',
    region: 'UK / Alt',
    mood: 'Indie / Eclectic',
    streamUrl: 'https://stream.live.vc.bbcmedia.co.uk/bbc_6music',
    prompts: [],
  },
  {
    id: 'kexp',
    name: 'KEXP Seattle',
    region: 'US / Alt',
    mood: 'Indie / Live Sessions',
    streamUrl: 'https://kexp-mp3-128.streamguys1.com/kexp128.mp3',
    prompts: [],
  },
  {
    id: 'cafe-del-mar',
    name: 'Café del Mar',
    region: 'ES / Chill',
    mood: 'Balearic / Chill',
    streamUrl: 'https://s3.voscast.com:8151/stream',
    prompts: [],
  },
  {
    id: 'wnyc',
    name: 'WNYC FM',
    region: 'US / News',
    mood: 'News / Talk',
    streamUrl: 'https://fm939.wnyc.org/wnycfm-web',
    prompts: [],
  },
  {
    id: 'lofi-hiphop',
    name: 'Lofi Hip Hop Radio',
    region: 'Global',
    mood: 'Lofi / Beats',
    streamUrl: 'https://streams.radio.co/s8d88a3b0a/listen',
    prompts: [],
  },
];

function PromptList({ prompts }: { prompts: Prompt[] }) {
  if (prompts.length === 0) {
    return (
      <Typography variant="body2" color="text.secondary">
        No hay prompts aún.
      </Typography>
    );
  }
  return (
    <Stack spacing={1}>
      {prompts.map((p, idx) => (
        <Card key={`${p.text}-${idx}`} variant="outlined">
          <CardContent sx={{ py: 1.25, display: 'flex', flexDirection: 'column', gap: 0.75 }}>
            <Typography variant="body2" fontWeight={600}>
              {p.text}
            </Typography>
            <Typography variant="caption" color="text.secondary">
              {p.author ?? 'Agente Tidal'} {p.createdAt ? `· ${p.createdAt}` : ''}
            </Typography>
            {p.code && (
              <Box
                sx={{
                  bgcolor: 'rgba(148,163,184,0.08)',
                  border: '1px dashed',
                  borderColor: 'divider',
                  borderRadius: 1.5,
                  p: 1,
                  fontFamily: 'monospace',
                  fontSize: 12,
                  whiteSpace: 'pre-line',
                  position: 'relative',
                }}
              >
                {p.code}
                <Box sx={{ position: 'absolute', top: 4, right: 4 }}>
                  <Tooltip title="Copiar instrucciones Tidal">
                    <IconButton
                      size="small"
                      onClick={() => {
                        if (navigator?.clipboard?.writeText) {
                          navigator.clipboard
                            .writeText(p.code ?? '')
                            .catch((err) => console.warn('No se pudo copiar las instrucciones Tidal', err));
                        }
                      }}
                    >
                      <ContentCopyIcon fontSize="small" />
                    </IconButton>
                  </Tooltip>
                </Box>
              </Box>
            )}
          </CardContent>
        </Card>
      ))}
    </Stack>
  );
}

export default function RadioWidget() {
  const [customStations, setCustomStations] = useState<Station[]>([]);
  const defaultStation = useMemo<Station>(
    () =>
      CURATED_STATIONS[0] ?? {
        id: 'fallback',
        name: 'Radio',
        mood: 'Live',
        streamUrl: '',
        prompts: [],
      },
    [],
  );
  const audioRef = useRef<HTMLAudioElement | null>(null);
  const [expanded, setExpanded] = useState(false);
  const [activeId, setActiveId] = useState<string>(defaultStation.id);
  const [isPlaying, setIsPlaying] = useState(false);
  const [muted, setMuted] = useState(false);
  const [promptDraft, setPromptDraft] = useState('');
  const [promptState, setPromptState] = useState<Record<string, Prompt[]>>(() =>
    Object.fromEntries(CURATED_STATIONS.map((s) => [s.id, [...s.prompts]])),
  );
  const availableStations = useMemo<Station[]>(() => [...CURATED_STATIONS, ...customStations], [customStations]);
  const activeStation = useMemo<Station>(
    () => availableStations.find((s) => s.id === activeId) ?? defaultStation,
    [activeId, availableStations, defaultStation],
  );
  const stationPrompts = promptState[activeStation.id] ?? activeStation.prompts;

  // Hydrate prompts and radio settings from localStorage to keep user submissions/settings across reloads.
  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const rawStations = window.localStorage.getItem('radio-stations');
      if (rawStations) {
        const parsedStations: Station[] = JSON.parse(rawStations);
        setCustomStations(parsedStations);
      }
      const raw = window.localStorage.getItem('radio-prompts');
      if (!raw) return;
      const saved: Record<string, Prompt[]> = JSON.parse(raw);
      setPromptState((prev) => ({ ...prev, ...saved }));
    } catch {
      // ignore parse errors
    }
    try {
      const rawSettings = window.localStorage.getItem('radio-settings');
      if (rawSettings) {
        const parsed = JSON.parse(rawSettings) as { stationId?: string; playOnLoad?: boolean; muted?: boolean };
        if (parsed.stationId) setActiveId(parsed.stationId);
        if (parsed.muted !== undefined) setMuted(parsed.muted);
        if (parsed.playOnLoad) setIsPlaying(true);
      }
    } catch {
      // ignore parse errors
    }
  }, []);

  // Persist prompts and settings to localStorage when they change.
  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      window.localStorage.setItem('radio-prompts', JSON.stringify(promptState));
    } catch {
      // ignore quota errors
    }
  }, [promptState]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const settings = {
        stationId: activeStation.id,
        playOnLoad: isPlaying,
        muted,
      };
      window.localStorage.setItem('radio-settings', JSON.stringify(settings));
      window.localStorage.setItem('radio-stations', JSON.stringify(customStations));
    } catch {
      // ignore
    }
  }, [activeStation.id, isPlaying, muted, customStations]);

  useEffect(() => {
    const audio = audioRef.current;
    if (!audio) return;
    audio.src = activeStation.streamUrl;
    audio.muted = muted;
    if (isPlaying) {
      void audio.play().catch(() => setIsPlaying(false));
    }
  }, [activeStation, isPlaying, muted]);

  const togglePlay = () => {
    const audio = audioRef.current;
    if (!audio) return;
    if (isPlaying) {
      audio.pause();
      setIsPlaying(false);
    } else {
      if (muted) setMuted(false);
      void audio.play().then(() => setIsPlaying(true)).catch(() => setIsPlaying(false));
    }
  };

  const promptsWithCode = useMemo(() => {
    return stationPrompts.map((p) => ({
      ...p,
      code: p.code ?? generateTidalCode(p.text, activeStation.mood).code,
    }));
  }, [activeStation, stationPrompts]);

  const [newStationName, setNewStationName] = useState('');
  const [newStationUrl, setNewStationUrl] = useState('');
  const addCustomStation = () => {
    const name = newStationName.trim();
    const url = newStationUrl.trim();
    if (!name || !url) {
      setTestResult('Ingresa nombre y URL de la radio.');
      return;
    }
    const id = `custom-${Math.random().toString(36).slice(2, 8)}`;
    const station: Station = { id, name, streamUrl: url, mood: 'Custom', prompts: [] };
    setCustomStations((prev) => [...prev, station]);
    setPromptState((prev) => ({ ...prev, [id]: [] }));
    setActiveId(id);
    setNewStationName('');
    setNewStationUrl('');
  };

  const removeCustomStation = (id: string) => {
    setCustomStations((prev) => prev.filter((s) => s.id !== id));
    setPromptState((prev) => {
      const next = { ...prev };
      delete next[id];
      return next;
    });
    if (activeId === id) {
      setActiveId(defaultStation.id);
    }
  };

  const [testResult, setTestResult] = useState<string | null>(null);
  const testStream = async () => {
    const url = newStationUrl.trim();
    if (!url) {
      setTestResult('Ingresa una URL para probar.');
      return;
    }
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 6000);
    try {
      const res = await fetch(url, { method: 'HEAD', signal: controller.signal });
      if (res.ok) {
        setTestResult('Stream disponible ✅');
      } else {
        setTestResult('No pudimos validar el stream. Verifica la URL.');
      }
    } catch {
      setTestResult('No pudimos validar el stream. Verifica la URL.');
    } finally {
      clearTimeout(timeout);
    }
  };

  const handleAddPrompt = () => {
    const value = promptDraft.trim();
    if (!value) return;
    const newPrompt: Prompt = {
      text: value,
      author: 'Tú',
      createdAt: new Date().toISOString().slice(0, 10),
      code: generateTidalCode(value, activeStation.mood).code,
    };
    setPromptState((prev) => ({
      ...prev,
      [activeStation.id]: [newPrompt, ...(prev[activeStation.id] ?? [])],
    }));
    setPromptDraft('');
  };

  return (
    <Box
      sx={{
        position: 'fixed',
        bottom: { xs: 12, md: 20 },
        right: { xs: 12, md: 24 },
        zIndex: 1400,
        width: expanded ? { xs: '95%', sm: 420 } : { xs: 220, sm: 260 },
      }}
    >
      <Card elevation={6} sx={{ borderRadius: 3, overflow: 'hidden' }}>
        <CardContent sx={{ p: 2 }}>
          <Stack direction="row" alignItems="center" spacing={1}>
            <RadioIcon color="secondary" />
            <Box flex={1}>
              <Typography variant="subtitle1" fontWeight={800} noWrap>
                Radio Inteligente
              </Typography>
              <Typography variant="caption" color="text.secondary" noWrap>
                {activeStation.name} · {activeStation.mood}
              </Typography>
            </Box>
            <Tooltip title={isPlaying ? 'Pausar' : 'Reproducir'}>
              <IconButton onClick={togglePlay} color="primary">
                {isPlaying ? <PauseIcon /> : <PlayArrowIcon />}
              </IconButton>
            </Tooltip>
            <Tooltip title={muted ? 'Quitar silencio' : 'Silenciar'}>
              <IconButton onClick={() => setMuted((m) => !m)} color="inherit">
                {muted ? <VolumeOffIcon /> : <GraphicEqIcon />}
              </IconButton>
            </Tooltip>
            <IconButton onClick={() => setExpanded((p) => !p)} color="inherit">
              {expanded ? <ExpandMoreIcon /> : <ExpandLessIcon />}
            </IconButton>
          </Stack>
          <Collapse in={!expanded}>
            <Stack direction="row" spacing={1} mt={1} alignItems="center">
              <GraphicEqIcon fontSize="small" color="secondary" />
              <Typography variant="caption" color="text.secondary">
                Generada en vivo con prompts de usuarios
              </Typography>
            </Stack>
          </Collapse>
        </CardContent>
        <Collapse in={expanded}>
          <Divider />
          <CardContent sx={{ p: 2 }}>
            <Stack spacing={1.5}>
              <Typography variant="body2" color="text.secondary">
                Estaciones del mundo (usa el widget para escuchar):
              </Typography>
              <Stack direction="row" spacing={1} flexWrap="wrap">
                {availableStations.map((station) => (
                  <Chip
                    key={station.id}
                    label={station.region ? `${station.name} · ${station.region}` : station.name}
                    color={station.id === activeId ? 'primary' : 'default'}
                    onClick={() => setActiveId(station.id)}
                    variant={station.id === activeId ? 'filled' : 'outlined'}
                    onDelete={station.id.startsWith('custom-') ? () => removeCustomStation(station.id) : undefined}
                  />
                ))}
              </Stack>
              <Stack spacing={1}>
                <Typography variant="caption" color="text.secondary">
                  Pega una URL de radio (ej. onlineradiobox.com) y guárdala para escucharla aquí.
                </Typography>
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                  <TextField
                    size="small"
                    label="Nombre de la radio"
                    value={newStationName}
                    onChange={(e) => setNewStationName(e.target.value)}
                    fullWidth
                  />
                  <TextField
                    size="small"
                    label="URL del stream"
                    value={newStationUrl}
                    onChange={(e) => setNewStationUrl(e.target.value)}
                    placeholder="https://"
                    fullWidth
                  />
                  <Stack direction={{ xs: 'row', sm: 'column' }} spacing={1} sx={{ minWidth: { sm: 160 } }}>
                    <Button
                      variant="outlined"
                      onClick={() => {
                        void testStream();
                      }}
                    >
                      Probar stream
                    </Button>
                    <Button
                      variant="contained"
                      onClick={addCustomStation}
                      disabled={!newStationName.trim() || !newStationUrl.trim()}
                    >
                      Guardar
                    </Button>
                  </Stack>
                </Stack>
                {testResult && (
                  <Stack direction="row" spacing={1} alignItems="center">
                    <WarningAmberIcon fontSize="small" color={testResult.includes('✅') ? 'success' : 'warning'} />
                    <Typography variant="caption" color="text.secondary">
                      {testResult}
                    </Typography>
                  </Stack>
                )}
              </Stack>
              <LinearProgress
                variant="indeterminate"
                sx={{ height: 6, borderRadius: 999, bgcolor: 'rgba(148,163,184,0.2)' }}
              />
              <Typography variant="caption" color="text.secondary">
                Streams de muestra; agrega tus prompts para escuchar nuevas variaciones.
              </Typography>
              <Typography variant="subtitle2">Prompts en uso</Typography>
              <PromptList prompts={promptsWithCode} />
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                <TextField
                  fullWidth
                  size="small"
                  placeholder="Escribe tu prompt (ej. dembow con quenas y delay granular)"
                  value={promptDraft}
                  onChange={(e) => setPromptDraft(e.target.value)}
                />
                <Button variant="contained" onClick={handleAddPrompt} disabled={!promptDraft.trim()}>
                  Generar Tidal
                </Button>
              </Stack>
              <Box>
                <Typography variant="caption" color="text.secondary">
                  Tip: estos streams se generan con un agente que convierte prompts en código Tidal Cycles y los publica en tiempo real.
                </Typography>
              </Box>
            </Stack>
          </CardContent>
        </Collapse>
      </Card>
      {/* Audio is programmatically controlled; captions are not available for live streams. */}
      {/* eslint-disable-next-line jsx-a11y/media-has-caption */}
      <audio ref={audioRef} preload="none" aria-hidden="true" />
    </Box>
  );
}
