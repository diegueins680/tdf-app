import { Box, Button, Stack } from '@mui/material';
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
    if (id) return { kind: 'iframe', url: `https://drive.google.com/file/d/${id}/preview` };
  }
  if (/\.(mp4|webm)$/i.test(url.pathname)) return { kind: 'video', url: value };
  return { kind: 'link', url: value };
}

export default function SessionVideoPreview({ url, title }: { url: string; title: string }) {
  const target = sessionVideoTarget(url);
  if (!target) return null;
  return <Stack spacing={1}>
    {target.kind === 'iframe' && <Box component="iframe" title={title} src={target.url} loading="lazy"
      allow="fullscreen; picture-in-picture" allowFullScreen referrerPolicy="strict-origin-when-cross-origin"
      sx={{ width: '100%', aspectRatio: '16 / 9', border: 0, borderRadius: 2 }} />}
    {target.kind === 'video' && <Box component="video" controls preload="metadata" aria-label={title}
      src={target.url} sx={{ width: '100%', maxHeight: 600, borderRadius: 2 }} />}
    <Button component="a" href={target.url} target="_blank" rel="noopener noreferrer">Abrir video completo</Button>
  </Stack>;
}
