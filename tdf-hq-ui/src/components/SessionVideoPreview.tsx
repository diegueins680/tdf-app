import { useEffect, useRef, useState } from 'react';
import { Alert, Box, Button, Stack, Typography } from '@mui/material';
import { resolveVideoSource } from '../utils/videoSource';
import { VideoWatch } from '../utils/videoWatch';
import { getAnalyticsClient } from '../analytics/posthog';

export interface SessionVideoPreviewProps { url: string; title: string; poster?: string | null; analyticsContext?: { artistId?: number; videoId: string } }

/** Shared by artist pages and Records. A page/iframe load is never counted as a video view. */
export default function SessionVideoPreview(props: SessionVideoPreviewProps) {
  // A new source remounts the player, stopping the previous media and clearing consent/errors.
  return <Player key={`${props.analyticsContext?.artistId ?? ''}:${resolveVideoSource(props.url)?.src ?? props.url}`} {...props} />;
}

function Player({ url, title, poster, analyticsContext }: SessionVideoPreviewProps) {
  const source = resolveVideoSource(url);
  const [open, setOpen] = useState(false);
  const [failed, setFailed] = useState(false);
  const videoRef = useRef<HTMLVideoElement>(null);
  const watch = useRef(new VideoWatch());
  const lastSent = useRef(0);
  const started = useRef(false);
  const capture = (event: string, properties: Record<string, unknown> = {}) => {
    if (analyticsContext) getAnalyticsClient().capture(event, { artist_id: analyticsContext.artistId, video_id: analyticsContext.videoId, provider: source?.provider, measurement: 'client_reported', ...properties });
  };
  const flush = () => {
    const seconds = Math.floor(watch.current.seconds - lastSent.current);
    if (seconds > 0) { capture('tdf_video_watch_sample', { watched_seconds: seconds }); lastSent.current += seconds; }
  };
  useEffect(() => {
    const video = videoRef.current;
    const tracker = watch.current;
    if (!video) return;
    const visibility = () => {
      if (document.visibilityState !== 'visible') tracker.stop();
      else if (!video.paused) tracker.start(video.currentTime, performance.now());
    };
    document.addEventListener('visibilitychange', visibility);
    return () => { document.removeEventListener('visibilitychange', visibility); video.pause(); tracker.stop(); };
  }, [open]);
  if (!source) return <Alert severity="info">Este enlace no admite reproducción en TDF. Añade una URL pública de YouTube, Vimeo, Instagram, TikTok o un archivo MP4/WebM.</Alert>;
  return <Stack spacing={1.5}>
    <Box sx={{ bgcolor: '#090909', borderRadius: 2, overflow: 'hidden', width: '100%', maxWidth: source.vertical ? 460 : undefined, mx: 'auto' }}>
      {!open ? <Box sx={{ position: 'relative', aspectRatio: source.vertical ? '9/16' : '16/9', minHeight: 210, display: 'grid', placeItems: 'center' }}>
        {poster && <Box component="img" src={poster} alt="" loading="lazy" sx={{ position: 'absolute', width: '100%', height: '100%', objectFit: 'contain', opacity: 0.65 }} />}
        <Button variant="contained" onClick={() => { capture('tdf_video_player_opened'); setOpen(true); }} sx={{ zIndex: 1 }}>Ver video aquí</Button>
      </Box> : source.provider === 'file' ? <Box component="video" ref={videoRef} controls playsInline preload="metadata" poster={poster ?? undefined} aria-label={title} src={source.src}
        onPlaying={event => { watch.current.start(event.currentTarget.currentTime, performance.now()); if (!started.current) { started.current = true; capture('tdf_video_play_started'); } }}
        onSeeking={() => watch.current.stop()}
        onSeeked={event => { if (!event.currentTarget.paused) watch.current.start(event.currentTarget.currentTime, performance.now()); }}
        onTimeUpdate={event => { if (!event.currentTarget.paused && !event.currentTarget.seeking) { watch.current.sample(event.currentTarget.currentTime, performance.now(), document.visibilityState === 'visible'); if (watch.current.seconds - lastSent.current >= 15) flush(); } }}
        onPause={() => { watch.current.stop(); flush(); }}
        onEnded={() => { watch.current.stop(); flush(); capture('tdf_video_ended'); }}
        onError={() => { setFailed(true); watch.current.stop(); flush(); }} sx={{ display: 'block', width: '100%', maxHeight: '75vh' }} />
        : <Box component="iframe" src={source.src} title={title} allow="encrypted-media; fullscreen; picture-in-picture" allowFullScreen referrerPolicy="strict-origin-when-cross-origin" onError={() => setFailed(true)} sx={{ display: 'block', width: '100%', aspectRatio: source.vertical ? '9/16' : '16/9', minHeight: 210, border: 0 }} />}
    </Box>
    <Typography variant="caption" color="text.secondary">{source.provider === 'file' ? 'Reproducción en TDF. No necesitas una cuenta para ver el video.' : `Al abrir el reproductor, te conectas con ${source.provider}. Su disponibilidad depende del permiso de inserción y las restricciones del proveedor.`}</Typography>
    {failed && <Alert severity="warning">No pudimos reproducir este video. Puedes reintentar o consultar la fuente.</Alert>}
    <Stack direction="row" spacing={2}>
      {open && <Button size="small" onClick={() => { watch.current.stop(); flush(); setOpen(false); setFailed(false); }}>Cerrar reproductor</Button>}
      {failed && <Button size="small" onClick={() => { setOpen(false); setFailed(false); }}>Reintentar</Button>}
      <Button size="small" component="a" href={source.url} target="_blank" rel="noopener noreferrer">Abrir fuente original</Button>
    </Stack>
  </Stack>;
}
