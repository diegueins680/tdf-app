const pageHosts = new Set([
  'open.spotify.com', 'spotify.com', 'www.spotify.com',
  'youtube.com', 'www.youtube.com', 'm.youtube.com', 'youtu.be',
  'instagram.com', 'www.instagram.com', 'facebook.com', 'www.facebook.com',
]);

/** Resolve public Drive shares to image content and exclude known HTML pages. */
export function normalizeArtistImageUrl(value?: string | null): string | null {
  const trimmed = value?.trim();
  if (!trimmed) return null;
  // Older profiles can still contain embedded uploads. New saves require hosting.
  if (/^data:image\/[a-z0-9.+-]+;base64,[a-z0-9+/=]+$/i.test(trimmed)) return trimmed;
  try {
    const url = new URL(trimmed, 'https://www.tdfrecords.net');
    if (!['http:', 'https:'].includes(url.protocol) || url.username || url.password
      || pageHosts.has(url.hostname) || url.hostname === 'docs.google.com') return null;
    if (url.hostname === 'drive.google.com') {
      const id = /^\/file\/d\/([\w-]+)(?:\/|$)/.exec(url.pathname)?.[1] ?? url.searchParams.get('id');
      if (!id || !/^[\w-]+$/.test(id)) return null;
      const image = new URL('https://drive.google.com/thumbnail');
      image.searchParams.set('id', id);
      image.searchParams.set('sz', 'w1600');
      const resourceKey = url.searchParams.get('resourcekey');
      if (resourceKey) image.searchParams.set('resourcekey', resourceKey);
      return image.href;
    }
    return trimmed;
  } catch {
    return null;
  }
}

export function artistImageUrlError(value?: string | null): string | null {
  if (!value?.trim()) return null;
  if (!/^https?:\/\//i.test(value.trim()) || !normalizeArtistImageUrl(value)) {
    return 'Sube una imagen o pega su enlace público directo. Los enlaces a páginas de Spotify o redes sociales van en sus campos correspondientes.';
  }
  return null;
}

export function prepareArtistImageUrl(value?: string | null): string | null {
  const error = artistImageUrlError(value);
  if (error) throw new Error(error);
  return normalizeArtistImageUrl(value);
}
