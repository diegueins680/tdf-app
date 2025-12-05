import { useEffect, useMemo, useRef, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Stack,
  TextField,
  Typography,
  Chip,
  Tooltip,
} from '@mui/material';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import StopIcon from '@mui/icons-material/Stop';
import GraphicEqIcon from '@mui/icons-material/GraphicEq';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import { generateTidalCode } from '../utils/tidalAgent';

function freqFromMidi(midi: number): number {
  return 440 * Math.pow(2, (midi - 69) / 12);
}

function hashPrompt(prompt: string): number {
  return prompt
    .toLowerCase()
    .split('')
    .reduce((acc, ch) => acc + ch.charCodeAt(0), 0);
}

export default function MusicMakerPage() {
  const [prompt, setPrompt] = useState('melodía minimal con pads y percusión suave');
  const [isPlaying, setIsPlaying] = useState(false);
  const [copied, setCopied] = useState(false);
  const intervalRef = useRef<number | null>(null);
  const audioCtxRef = useRef<AudioContext | null>(null);
  const masterGainRef = useRef<GainNode | null>(null);

  const tidal = useMemo(() => generateTidalCode(prompt), [prompt]);
  const tempo = tidal.tempo;
  const tags = tidal.tags;

  useEffect(() => {
    return () => {
      if (intervalRef.current) {
        clearInterval(intervalRef.current);
      }
      if (audioCtxRef.current) {
        audioCtxRef.current.close().catch(() => {});
      }
    };
  }, []);

  const playTone = (freq: number, duration: number) => {
    if (!audioCtxRef.current) return;
    const ctx = audioCtxRef.current;
    const now = ctx.currentTime;
    const osc = ctx.createOscillator();
    const gain = ctx.createGain();
    osc.frequency.value = freq;
    osc.type = 'sine';
    gain.gain.setValueAtTime(0.0001, now);
    gain.gain.exponentialRampToValueAtTime(0.25, now + 0.01);
    gain.gain.exponentialRampToValueAtTime(0.001, now + duration);
    osc.connect(gain).connect(masterGainRef.current ?? ctx.destination);
    osc.start(now);
    osc.stop(now + duration + 0.05);
  };

  const startPlayback = () => {
    if (isPlaying) return;
    if (!audioCtxRef.current) {
      const ctx = new AudioContext();
      const masterGain = ctx.createGain();
      masterGain.gain.value = 0.8;
      masterGain.connect(ctx.destination);
      audioCtxRef.current = ctx;
      masterGainRef.current = masterGain;
    }
    const ctx = audioCtxRef.current!;
    void ctx.resume();

    const seed = hashPrompt(prompt);
    const intervalMs = (60_000 / tempo) * 0.5; // eighth notes
    let step = 0;
    const scale = [0, 2, 3, 5, 7, 10, 12];
    const loop = () => {
      const idx = (seed + step * 7) % scale.length;
      const octave = 3 + ((seed + step) % 3);
      const degree = scale[idx] ?? 0;
      const midi = 48 + degree + octave * 2;
      const freq = freqFromMidi(midi);
      playTone(freq, 0.2);
      step += 1;
    };
    loop();
    intervalRef.current = window.setInterval(loop, intervalMs);
    setIsPlaying(true);
  };

  const stopPlayback = () => {
    if (intervalRef.current) {
      clearInterval(intervalRef.current);
      intervalRef.current = null;
    }
    setIsPlaying(false);
  };

  const handleCopy = async () => {
    try {
      await navigator.clipboard.writeText(tidal.code);
      setCopied(true);
      setTimeout(() => setCopied(false), 1500);
    } catch {
      setCopied(false);
    }
  };

  return (
    <Box sx={{ py: 4 }}>
      <Stack spacing={2} maxWidth={800} mx="auto">
        <Stack direction="row" spacing={1} alignItems="center">
          <GraphicEqIcon color="primary" />
          <Typography variant="h4" fontWeight={800}>
            Creador musical
          </Typography>
        </Stack>
        <Typography variant="body1" color="text.secondary">
          Escribe un prompt. Generamos instrucciones Tidal y las reproducimos con un motor de síntesis simple en tu navegador.
        </Typography>
        <TextField
          fullWidth
          label="Prompt musical"
          value={prompt}
          onChange={(e) => setPrompt(e.target.value)}
          multiline
          minRows={2}
        />
        <Stack direction="row" spacing={1}>
          <Button
            variant="contained"
            startIcon={<PlayArrowIcon />}
            onClick={startPlayback}
            disabled={isPlaying}
          >
            Generar y reproducir
          </Button>
          <Button
            variant="outlined"
            startIcon={<StopIcon />}
            onClick={stopPlayback}
            disabled={!isPlaying}
          >
            Detener
          </Button>
          <Chip label={`${Math.round(tempo)} BPM`} />
          <Chip label={tags.join(' · ')} variant="outlined" />
        </Stack>
        <Card variant="outlined">
          <CardContent>
            <Typography variant="subtitle2" gutterBottom>
              Instrucciones Tidal generadas
            </Typography>
            <Box
              component="pre"
              sx={{
                p: 2,
                bgcolor: 'grey.900',
                color: 'grey.50',
                borderRadius: 2,
                overflow: 'auto',
                fontSize: 13,
              }}
            >
              {tidal.code}
            </Box>
            <Stack direction="row" spacing={1} sx={{ mt: 1 }}>
              <Tooltip title="Copiar código Tidal">
                <Button
                  variant="outlined"
                  size="small"
                  startIcon={<ContentCopyIcon fontSize="small" />}
                  onClick={handleCopy}
                >
                  {copied ? 'Copiado' : 'Copiar'}
                </Button>
              </Tooltip>
            </Stack>
          </CardContent>
        </Card>
        <Alert severity="info">
          Esto es una vista previa rápida con síntesis básica en el navegador. Usa las instrucciones Tidal para producción en tu entorno de audio.
        </Alert>
      </Stack>
    </Box>
  );
}
