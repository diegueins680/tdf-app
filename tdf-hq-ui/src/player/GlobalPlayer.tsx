import { useId, useRef, useState } from 'react';
import { GLOBAL_PLAYER_HEIGHT_VAR, useDockedBarHeight } from '../utils/bottomDock';
import {
  Box,
  Button,
  Dialog,
  DialogContent,
  DialogTitle,
  Divider,
  Drawer,
  FormControl,
  FormControlLabel,
  IconButton,
  InputLabel,
  MenuItem,
  Select,
  Slider,
  Stack,
  Switch,
  Tooltip,
  Typography,
  useMediaQuery,
} from '@mui/material';
import CloseIcon from '@mui/icons-material/Close';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import DragHandleIcon from '@mui/icons-material/DragHandle';
import PauseIcon from '@mui/icons-material/Pause';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import QueueMusicIcon from '@mui/icons-material/QueueMusic';
import RepeatIcon from '@mui/icons-material/Repeat';
import RepeatOneIcon from '@mui/icons-material/RepeatOne';
import ShuffleIcon from '@mui/icons-material/Shuffle';
import SkipNextIcon from '@mui/icons-material/SkipNext';
import SkipPreviousIcon from '@mui/icons-material/SkipPrevious';
import VolumeOffIcon from '@mui/icons-material/VolumeOff';
import VolumeUpIcon from '@mui/icons-material/VolumeUp';
import TuneIcon from '@mui/icons-material/Tune';
import { usePlayer } from './PlayerProvider';
import type { PlayerQuality, RepeatMode } from './types';

const formatTime = (seconds: number): string => {
  if (!Number.isFinite(seconds) || seconds < 0) return '0:00';
  const whole = Math.floor(seconds);
  return `${Math.floor(whole / 60)}:${String(whole % 60).padStart(2, '0')}`;
};

const repeatLabel: Record<RepeatMode, string> = {
  off: 'Repetición desactivada',
  track: 'Repetir pista',
  queue: 'Repetir cola',
  release: 'Repetir lanzamiento',
};

const nextRepeatMode: Record<RepeatMode, RepeatMode> = {
  off: 'track',
  track: 'queue',
  queue: 'release',
  release: 'off',
};

export default function GlobalPlayer() {
  const player = usePlayer();
  const [queueOpen, setQueueOpen] = useState(false);
  const [optionsOpen, setOptionsOpen] = useState(false);
  const optionsId = useId();
  const closeOptionsRef = useRef<HTMLButtonElement>(null);
  const reducedMotion = useMediaQuery('(prefers-reduced-motion: reduce)');
  const track = player.currentTrack;
  const barRef = useRef<HTMLElement | null>(null);
  // Other docked bars and page CTAs stack above the player instead of under it.
  useDockedBarHeight(GLOBAL_PLAYER_HEIGHT_VAR, barRef, Boolean(track));
  if (!track) return null;

  const statusMessage = player.error
    ?? (player.status === 'buffering' ? 'Almacenando en búfer…'
      : player.status === 'loading' ? 'Cargando…'
        : player.status === 'unavailable' ? 'Contenido no disponible'
          : null);

  return (
    <>
      <Box
        ref={barRef}
        component="section"
        aria-label="Reproductor global"
        sx={{
          position: 'fixed',
          insetInline: 0,
          bottom: 0,
          zIndex: (theme) => theme.zIndex.modal - 1,
          bgcolor: 'background.paper',
          borderTop: '1px solid',
          borderColor: 'divider',
          boxShadow: '0 -8px 28px rgba(0,0,0,0.22)',
          px: { xs: 1, sm: 2 },
          py: 1,
          pb: 'max(8px, env(safe-area-inset-bottom))',
          '& .MuiIconButton-root': { minWidth: 44, minHeight: 44 },
          '& .Mui-focusVisible': { outline: '2px solid', outlineColor: 'primary.main', outlineOffset: 2 },
          '@media (prefers-reduced-motion: reduce)': { transition: 'none' },
        }}
      >
        <Stack
          direction="row"
          spacing={0}
          alignItems="center"
          sx={{
            display: { xs: 'grid', lg: 'flex' },
            gridTemplateColumns: { xs: 'minmax(0, 1fr) repeat(5, auto)', lg: 'none' },
            columnGap: { xs: 0.5, lg: 1 },
            rowGap: { xs: 0.5, lg: 0 },
          }}
        >
          {track.artworkUrl && (
            <Box
              component="img"
              src={track.artworkUrl}
              alt=""
              sx={{ width: 44, height: 44, borderRadius: 1, objectFit: 'cover', display: { xs: 'none', lg: 'block' } }}
            />
          )}
          <Box sx={{ minWidth: 0, width: { xs: 'auto', lg: 140, xl: 180 }, gridColumn: { xs: 1, lg: 'auto' }, gridRow: 1 }}>
            <Typography variant="subtitle2" noWrap>{track.title}</Typography>
            <Typography variant="caption" color="text.secondary" noWrap display="block">{track.artist}</Typography>
            {statusMessage && (
              <Typography variant="caption" color={player.error ? 'error' : 'text.secondary'} noWrap display="block" role="status">
                {statusMessage}
              </Typography>
            )}
          </Box>

          <Tooltip title="Pista anterior (Alt + ←)">
            <IconButton aria-label="Pista anterior" onClick={player.previous} size="small">
              <SkipPreviousIcon />
            </IconButton>
          </Tooltip>
          <Tooltip title={player.status === 'playing' ? 'Pausar (Espacio)' : 'Reproducir (Espacio)'}>
            <IconButton
              color="primary"
              aria-label={player.status === 'playing' ? 'Pausar' : 'Reproducir'}
              onClick={player.togglePlayback}
              sx={{ minWidth: 44, minHeight: 44 }}
            >
              {player.status === 'playing' ? <PauseIcon /> : <PlayArrowIcon />}
            </IconButton>
          </Tooltip>
          <Tooltip title="Pista siguiente (Alt + →)">
            <IconButton aria-label="Pista siguiente" onClick={player.next} size="small">
              <SkipNextIcon />
            </IconButton>
          </Tooltip>

          <Typography variant="caption" sx={{ display: { xs: 'none', lg: 'block' }, minWidth: 36, textAlign: 'right' }}>
            {formatTime(player.currentTime)}
          </Typography>
          <Slider
            aria-label="Posición de reproducción"
            value={Math.min(player.currentTime, player.duration || player.currentTime)}
            min={0}
            max={Math.max(player.duration, 1)}
            getAriaValueText={(value) => `${formatTime(value)} de ${formatTime(player.duration)}`}
            onChange={(_, value) => player.seek(Array.isArray(value) ? value[0] ?? 0 : value)}
            sx={{
              flex: 1,
              minWidth: { xs: 0, lg: 50 },
              width: { xs: '100%', lg: 'auto' },
              gridColumn: { xs: '1 / -1', lg: 'auto' },
              gridRow: { xs: 2, lg: 'auto' },
            }}
          />
          <Typography variant="caption" sx={{ display: { xs: 'none', lg: 'block' }, minWidth: 36 }}>
            {formatTime(player.duration)}
          </Typography>

          <Tooltip title={player.queue.shuffle ? 'Desactivar aleatorio' : 'Activar aleatorio'}>
            <IconButton
              aria-label={player.queue.shuffle ? 'Desactivar reproducción aleatoria' : 'Activar reproducción aleatoria'}
              aria-pressed={player.queue.shuffle}
              color={player.queue.shuffle ? 'primary' : 'default'}
              onClick={() => player.setShuffle(!player.queue.shuffle)}
              size="small"
              sx={{ display: { xs: 'none', lg: 'inline-flex' } }}
            >
              <ShuffleIcon />
            </IconButton>
          </Tooltip>
          <Tooltip title={repeatLabel[player.queue.repeat]}>
            <IconButton
              aria-label={repeatLabel[player.queue.repeat]}
              color={player.queue.repeat === 'off' ? 'default' : 'primary'}
              onClick={() => player.setRepeat(nextRepeatMode[player.queue.repeat])}
              size="small"
              sx={{ display: { xs: 'none', lg: 'inline-flex' } }}
            >
              {player.queue.repeat === 'track' ? <RepeatOneIcon /> : <RepeatIcon />}
            </IconButton>
          </Tooltip>
          <Tooltip title={player.muted ? 'Activar sonido (M)' : 'Silenciar (M)'}>
            <IconButton
              aria-label={player.muted ? 'Activar sonido' : 'Silenciar'}
              onClick={() => player.setMuted(!player.muted)}
              size="small"
              sx={{ display: { xs: 'none', lg: 'inline-flex' } }}
            >
              {player.muted ? <VolumeOffIcon /> : <VolumeUpIcon />}
            </IconButton>
          </Tooltip>
          <Slider
            aria-label="Volumen"
            value={player.volume}
            min={0}
            max={1}
            step={0.01}
            onChange={(_, value) => player.setVolume(Array.isArray(value) ? value[0] ?? 0 : value)}
            sx={{ width: 72, display: { xs: 'none', lg: 'block' } }}
          />
          <FormControl size="small" sx={{ minWidth: 90, display: { xs: 'none', lg: 'block' } }}>
            <Select
              inputProps={{ 'aria-label': 'Calidad de audio' }}
              value={player.quality}
              onChange={(event) => player.setQuality(event.target.value as PlayerQuality | 'auto')}
            >
              <MenuItem value="auto">Auto{player.resolvedQuality ? ` · ${player.resolvedQuality}` : ''}</MenuItem>
              <MenuItem value="low">Baja</MenuItem>
              <MenuItem value="medium">Media</MenuItem>
              <MenuItem value="high">Alta</MenuItem>
              <MenuItem value="lossless">Lossless</MenuItem>
            </Select>
          </FormControl>
          <Tooltip title="Opciones de reproducción">
            <IconButton
              aria-label="Opciones de reproducción"
              aria-haspopup="dialog"
              aria-expanded={optionsOpen}
              aria-controls={optionsOpen ? optionsId : undefined}
              onClick={() => setOptionsOpen(true)}
              sx={{ gridColumn: { xs: 5, lg: 'auto' }, gridRow: 1 }}
            ><TuneIcon /></IconButton>
          </Tooltip>
          <Tooltip title="Abrir cola">
            <IconButton
              aria-label={`Abrir cola, ${player.queue.tracks.length} pistas`}
              onClick={() => setQueueOpen(true)}
              sx={{ gridColumn: { xs: 6, lg: 'auto' }, gridRow: 1, minWidth: 44, minHeight: 44 }}
            >
              <QueueMusicIcon />
            </IconButton>
          </Tooltip>
        </Stack>
      </Box>

      <Dialog
        open={optionsOpen}
        onClose={() => setOptionsOpen(false)}
        aria-labelledby={`${optionsId}-title`}
        fullWidth
        maxWidth="xs"
        transitionDuration={reducedMotion ? 0 : undefined}
        slotProps={{ paper: { id: optionsId, sx: {
          m: 2, width: 'calc(100% - 32px)',
          '& .MuiIconButton-root': { minWidth: 44, minHeight: 44 },
          '& .Mui-focusVisible': { outline: '2px solid', outlineColor: 'primary.main', outlineOffset: 2 },
        } }, transition: { onEntered: () => closeOptionsRef.current?.focus() } }}
      >
        <DialogTitle id={`${optionsId}-title`} sx={{ pr: 7 }}>Opciones de reproducción</DialogTitle>
        <IconButton ref={closeOptionsRef} aria-label="Cerrar opciones" onClick={() => setOptionsOpen(false)} sx={{ position: 'absolute', right: 8, top: 8 }}>
          <CloseIcon />
        </IconButton>
        <DialogContent dividers>
          <Stack spacing={3} sx={{ pt: 1 }}>
            <FormControl fullWidth>
              <InputLabel id={`${optionsId}-quality`}>Calidad de audio</InputLabel>
              <Select labelId={`${optionsId}-quality`} label="Calidad de audio" value={player.quality}
                onChange={(event) => player.setQuality(event.target.value as PlayerQuality | 'auto')}>
                <MenuItem value="auto">Auto{player.resolvedQuality ? ` · ${player.resolvedQuality}` : ''}</MenuItem>
                <MenuItem value="low">Baja</MenuItem>
                <MenuItem value="medium">Media</MenuItem>
                <MenuItem value="high">Alta</MenuItem>
                <MenuItem value="lossless">Lossless</MenuItem>
              </Select>
            </FormControl>
            <FormControl fullWidth>
              <InputLabel id={`${optionsId}-repeat`}>Repetición</InputLabel>
              <Select labelId={`${optionsId}-repeat`} label="Repetición" value={player.queue.repeat}
                onChange={(event) => player.setRepeat(event.target.value as RepeatMode)}>
                {(['off', 'track', 'queue', 'release'] as const).map((mode) => (
                  <MenuItem key={mode} value={mode}>{repeatLabel[mode]}</MenuItem>
                ))}
              </Select>
            </FormControl>
            <FormControlLabel label="Reproducción aleatoria"
              control={<Switch checked={player.queue.shuffle} onChange={(_, checked) => player.setShuffle(checked)} />} />
            <Box>
              <Typography id={`${optionsId}-volume`} gutterBottom>Volumen</Typography>
              <Stack direction="row" spacing={2} alignItems="center">
                <IconButton aria-label={player.muted ? 'Activar sonido' : 'Silenciar'} onClick={() => player.setMuted(!player.muted)}>
                  {player.muted ? <VolumeOffIcon /> : <VolumeUpIcon />}
                </IconButton>
                <Slider aria-labelledby={`${optionsId}-volume`} min={0} max={1} step={0.01} value={player.volume}
                  getAriaValueText={(value) => `${Math.round(value * 100)} %`}
                  onChange={(_, value) => player.setVolume(Array.isArray(value) ? value[0] ?? 0 : value)} />
              </Stack>
            </Box>
          </Stack>
        </DialogContent>
      </Dialog>

      <Drawer anchor="right" open={queueOpen} onClose={() => setQueueOpen(false)} transitionDuration={reducedMotion ? 0 : undefined}>
        <Box sx={{ width: { xs: 'min(90vw, 360px)', sm: 400 }, p: 2 }} role="dialog" aria-label="Cola de reproducción">
          <Stack direction="row" alignItems="center" justifyContent="space-between">
            <Typography variant="h6">Cola</Typography>
            <IconButton aria-label="Cerrar cola" onClick={() => setQueueOpen(false)}><CloseIcon /></IconButton>
          </Stack>
          <Divider sx={{ my: 1 }} />
          <Stack component="ol" sx={{ listStyle: 'none', p: 0, m: 0 }} spacing={0.5}>
            {player.queue.tracks.map((queuedTrack, index) => (
              <Stack
                component="li"
                key={`${queuedTrack.id}-${index}`}
                direction="row"
                alignItems="center"
                spacing={1}
                sx={{ p: 1, borderRadius: 1, bgcolor: index === player.queue.currentIndex ? 'action.selected' : undefined }}
              >
                <DragHandleIcon color="disabled" aria-hidden="true" />
                <Button
                  variant="text"
                  onClick={() => player.selectTrack(index)}
                  sx={{ flex: 1, justifyContent: 'flex-start', textTransform: 'none', minWidth: 0 }}
                  aria-current={index === player.queue.currentIndex ? 'true' : undefined}
                >
                  <Box sx={{ minWidth: 0, textAlign: 'left' }}>
                    <Typography variant="body2" noWrap>{queuedTrack.title}</Typography>
                    <Typography variant="caption" color="text.secondary" noWrap display="block">{queuedTrack.artist}</Typography>
                  </Box>
                </Button>
                <Stack>
                  <IconButton
                    size="small"
                    aria-label={`Subir ${queuedTrack.title} en la cola`}
                    disabled={index === 0}
                    onClick={() => player.moveTrack(index, index - 1)}
                  >↑</IconButton>
                  <IconButton
                    size="small"
                    aria-label={`Bajar ${queuedTrack.title} en la cola`}
                    disabled={index === player.queue.tracks.length - 1}
                    onClick={() => player.moveTrack(index, index + 1)}
                  >↓</IconButton>
                </Stack>
                <IconButton aria-label={`Quitar ${queuedTrack.title} de la cola`} onClick={() => player.removeTrack(index)}>
                  <DeleteOutlineIcon />
                </IconButton>
              </Stack>
            ))}
          </Stack>
          {player.queue.tracks.length > 0 && (
            <Button color="error" onClick={player.clearQueue} sx={{ mt: 2 }}>Vaciar cola</Button>
          )}
        </Box>
      </Drawer>
    </>
  );
}
