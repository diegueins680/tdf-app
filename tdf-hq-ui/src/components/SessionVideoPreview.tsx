import { useEffect, useRef, useState } from 'react';
import { useOptionalPlayer } from '../player/PlayerProvider';
import { PLAYER_TAKEOVER_EVENT } from '../player/types';
import { Alert, Box, Button, Stack, Typography } from '@mui/material';
import { resolveVideoSource } from '../utils/videoSource';
import { VideoWatch } from '../utils/videoWatch';
import { getAnalyticsClient } from '../analytics/posthog';
import { videoAttribution } from '../features/campaigns/videoAttribution';
import { safeVideoUrl } from '../features/campaigns/videoCampaign';

export function sessionVideoTarget(value: string): { kind: 'iframe' | 'video' | 'link'; url: string } | null {
  if (!value || !safeVideoUrl(value)) return null;
  const url = new URL(value);
  const host = url.hostname;
  if (['www.youtube.com', 'youtube.com', 'youtu.be'].includes(host)) {
    const id = host === 'youtu.be' ? url.pathname.slice(1) : url.searchParams.get('v')
      ?? /^\/(?:embed|shorts)\/([^/]+)$/.exec(url.pathname)?.[1];
    if (id && /^[\w-]{11}$/.test(id)) return { kind: 'iframe', url: `https://www.youtube-nocookie.com/embed/${id}` };
  }
  if (host === 'drive.google.com') {
    const id = /^\/file\/d\/([\w-]+)(?:\/|$)/.exec(url.pathname)?.[1];
    if (id) {
      const preview = new URL(`https://drive.google.com/file/d/${id}/preview`);
      const resourceKey = url.searchParams.get('resourcekey');
      if (resourceKey) preview.searchParams.set('resourcekey', resourceKey);
      return { kind: 'iframe', url: preview.toString() };
    }
  }
  if (/\.(mp4|webm)$/i.test(url.pathname)) return { kind: 'video', url: value };
  return { kind: 'link', url: value };
}



export interface SessionVideoPreviewProps { url: string; title: string; poster?: string | null; trackPublicPlayback?: boolean; analyticsContext?: { artistId?: number; videoId: string } }

/** Shared by artist pages and Records. A page/iframe load is never counted as a video view. */
export default function SessionVideoPreview(props: SessionVideoPreviewProps) {
  // A new source remounts the player, stopping the previous media and clearing consent/errors.
  return <Player key={`${props.analyticsContext?.artistId ?? ''}:${resolveVideoSource(props.url)?.src ?? props.url}`} {...props} />;
}

function Player({ url, title, poster, analyticsContext, trackPublicPlayback = false }: SessionVideoPreviewProps) {
  const player = useOptionalPlayer();
  const target = sessionVideoTarget(url);
  const source = resolveVideoSource(url) ?? (target?.kind === 'iframe' ? { provider: 'drive', src: target.url, url, vertical: false } : null);
  const [open, setOpen] = useState(false);
  const [failed, setFailed] = useState(false);
  const videoRef = useRef<HTMLVideoElement>(null);
  useEffect(() => {
    const retirePreview = () => { videoRef.current?.pause(); setOpen(false); };
    window.addEventListener(PLAYER_TAKEOVER_EVENT, retirePreview);
    return () => window.removeEventListener(PLAYER_TAKEOVER_EVENT, retirePreview);
  }, []);
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
  if (!source && target?.kind === 'link') return <Button component="a" href={url} target="_blank" rel="noopener noreferrer">Abrir video completo</Button>;
  if (!source) return <Alert severity="info">Este enlace no admite reproducción en TDF. Añade una URL pública de YouTube, Vimeo, Instagram, TikTok o un archivo MP4/WebM.</Alert>;
  return <Stack spacing={1.5}>
    <Box sx={{ bgcolor: '#090909', borderRadius: 2, overflow: 'hidden', width: '100%', maxWidth: source.vertical ? 460 : undefined, mx: 'auto' }}>
      {!open ? <Box sx={{ position: 'relative', aspectRatio: source.vertical ? '9/16' : '16/9', minHeight: 210, display: 'grid', placeItems: 'center' }}>
        {poster && <Box component="img" src={poster} alt="" loading="lazy" sx={{ position: 'absolute', width: '100%', height: '100%', objectFit: 'contain', opacity: 0.65 }} />}
        <Button variant="contained" onClick={() => { player?.pause(); capture('tdf_video_player_opened'); setOpen(true); }} sx={{ zIndex: 1 }}>Ver video aquí</Button>
      </Box> : source.provider === 'file' ? <Box component="video" ref={videoRef} controls playsInline preload="metadata" poster={poster ?? undefined} aria-label={title} src={source.src}
        onPlay={() => { player?.pause(); if (trackPublicPlayback) getAnalyticsClient().capture('video_campaign_play_started', { ...videoAttribution(window.location.search), player: 'file' }); }}
        onPlaying={event => { watch.current.start(event.currentTarget.currentTime, performance.now()); if (!started.current) { started.current = true; capture('tdf_video_play_started'); } }}
        onSeeking={() => watch.current.stop()}
        onSeeked={event => { if (!event.currentTarget.paused) watch.current.start(event.currentTarget.currentTime, performance.now()); }}
        onTimeUpdate={event => { if (!event.currentTarget.paused && !event.currentTarget.seeking) { watch.current.sample(event.currentTarget.currentTime, performance.now(), document.visibilityState === 'visible'); if (watch.current.seconds - lastSent.current >= 15) flush(); } }}
        onPause={() => { watch.current.stop(); flush(); }}
        onEnded={() => { watch.current.stop(); flush(); capture('tdf_video_ended'); if (trackPublicPlayback) getAnalyticsClient().capture('video_campaign_play_ended', { ...videoAttribution(window.location.search), player: 'file' }); }}
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
