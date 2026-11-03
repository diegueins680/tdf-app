export interface VideoSource { provider: 'youtube' | 'vimeo' | 'instagram' | 'tiktok' | 'file'; src: string; url: string; vertical: boolean }

/** Public source identity for a durable session link, never an arbitrary URL from a query. */
export function videoSourceKey(value: string): string | null {
  const source = resolveVideoSource(value);
  if (!source) return null;
  if (source.provider === 'file') {
    const id = /^\/campaign-platform\/public\/videos\/([0-9a-f]{32})\/media$/.exec(new URL(source.src).pathname)?.[1];
    return id ? `file:${id}` : null;
  }
  const parts = new URL(source.src).pathname.split('/').filter(Boolean);
  return `${source.provider}:${source.provider === 'instagram' ? parts[1] : parts.at(-1)}`;
}

/** Parse known public URLs; never render supplied HTML, follow shorteners or fetch oEmbed on the server. */
export function resolveVideoSource(value: string): VideoSource | null {
  let url: URL;
  try { url = new URL(value); } catch { return null; }
  if (url.protocol !== 'https:' || url.username || url.password || url.port || value.length > 2048) return null;
  const host = url.hostname.toLowerCase();
  const path = url.pathname;
  let id: string | undefined;
  if (host === 'youtu.be') id = path.slice(1);
  if (['youtube.com', 'www.youtube.com', 'm.youtube.com', 'www.youtube-nocookie.com'].includes(host)) {
    id = path === '/watch' ? url.searchParams.get('v') ?? undefined : (/^\/(?:embed|shorts|live)\/([\w-]{11})\/?$/.exec(path))?.[1];
  }
  if (id && /^[\w-]{11}$/.test(id)) return { provider: 'youtube', src: `https://www.youtube-nocookie.com/embed/${id}?autoplay=0&playsinline=1`, url: `https://www.youtube.com/watch?v=${id}`, vertical: path.startsWith('/shorts/') };
  if (['vimeo.com', 'www.vimeo.com', 'player.vimeo.com'].includes(host)) {
    const match = /^\/(?:video\/)?(\d{6,12})(?:\/([a-zA-Z0-9]+))?\/?$/.exec(path);
    if (!match) return null;
    const hash = match[2] ?? url.searchParams.get('h');
    if (hash && !/^[a-zA-Z0-9]{1,64}$/.test(hash)) return null;
    return { provider: 'vimeo', src: `https://player.vimeo.com/video/${match[1]}?autoplay=0&dnt=1${hash ? `&h=${hash}` : ''}`, url: url.href, vertical: false };
  }
  if (['instagram.com', 'www.instagram.com'].includes(host)) {
    const match = /^\/(p|reel|tv)\/([\w-]{5,64})\/?$/.exec(path);
    if (!match) return null;
    return { provider: 'instagram', src: `https://www.instagram.com/${match[1]}/${match[2]}/embed/`, url: `https://www.instagram.com/${match[1]}/${match[2]}/`, vertical: true };
  }
  if (['tiktok.com', 'www.tiktok.com'].includes(host)) {
    const match = /^\/@[\w.]+\/video\/(\d{15,25})\/?$/.exec(path);
    if (!match) return null;
    return { provider: 'tiktok', src: `https://www.tiktok.com/player/v1/${match[1]}?autoplay=0&controls=1&description=1`, url: url.href, vertical: true };
  }
  // Only public HTTPS file URLs. Unknown provider paths do not become arbitrary frames.
  if (/\.(?:mp4|webm)$/i.test(path) && !/^(localhost|.*\.localhost|.*\.local|\[.*\]|\d+(?:\.\d+){3})$/.test(host)) {
    return { provider: 'file', src: url.href, url: url.href, vertical: false };
  }
  if (['api.tdfrecords.net'].includes(host) && /^\/campaign-platform\/public\/videos\/[0-9a-f]{32}\/media$/.test(path)) {
    return { provider: 'file', src: url.href, url: url.href, vertical: false };
  }
  return null;
}
