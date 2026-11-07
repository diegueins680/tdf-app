import { useEffect, useRef, useState } from 'react';
import { useOptionalPlayer } from '../player/PlayerProvider';
import { PLAYER_TAKEOVER_EVENT } from '../player/types';
import { Alert, Box, Button, Stack } from '@mui/material';
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

export default function SessionVideoPreview({ url, title, trackPublicPlayback = false }: { url: string; title: string; trackPublicPlayback?: boolean }) {
  const player = useOptionalPlayer();
  const videoRef = useRef<HTMLVideoElement | null>(null);
  useEffect(() => {
    const retireVideo = () => videoRef.current?.pause();
    window.addEventListener(PLAYER_TAKEOVER_EVENT, retireVideo);
    return () => window.removeEventListener(PLAYER_TAKEOVER_EVENT, retireVideo);
  }, []);
  const [failedUrl, setFailedUrl] = useState<string | null>(null);
  const target = sessionVideoTarget(url);
  if (!target) return null;
  return <Stack spacing={1}>
    {target.kind === 'iframe' && <Box component="iframe" title={title} src={target.url} loading="lazy"
      allow="fullscreen; picture-in-picture" allowFullScreen referrerPolicy="strict-origin-when-cross-origin"
      sx={{ width: '100%', aspectRatio: '16 / 9', border: 0, borderRadius: 2 }} />}
    {target.kind === 'video' && <Box component="video" ref={videoRef} controls preload="metadata" aria-label={title}
      src={target.url} onError={() => setFailedUrl(url)}
      onPlay={() => { player?.pause(); if (trackPublicPlayback) getAnalyticsClient().capture('video_campaign_play_started', { ...videoAttribution(window.location.search), player: 'file' }); }}
      onEnded={() => { if (trackPublicPlayback) getAnalyticsClient().capture('video_campaign_play_ended', { ...videoAttribution(window.location.search), player: 'file' }); }}
      sx={{ width: '100%', maxHeight: 600, borderRadius: 2 }} />}
    {failedUrl === url && <Alert severity="warning">No se pudo reproducir el archivo. Abre el original y revisa su disponibilidad y permisos.</Alert>}
    <Button component="a" href={url} target="_blank" rel="noopener noreferrer">Abrir video completo</Button>
  </Stack>;
}
