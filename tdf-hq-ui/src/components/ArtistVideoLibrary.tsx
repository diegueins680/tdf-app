import { useQuery } from '@tanstack/react-query';
import { Alert, Box, Button, Typography } from '@mui/material';
import SessionVideoPreview from './SessionVideoPreview';
import { videoSourceKey } from '../utils/videoSource';

interface ArtistVideo { id: string; title: string; description: string; url: string; poster: string }
function sameVideo(a?: string | null, b?: string | null): boolean {
  if (!a || !b) return false;
  const key = videoSourceKey(a);
  return a === b || (key !== null && key === videoSourceKey(b));
}
export default function ArtistVideoLibrary({ artistId, featuredUrl, featuredPoster, artistName = 'este artista', requestedVideo }: { artistId: number; featuredUrl?: string | null; featuredPoster?: string | null; artistName?: string; requestedVideo?: string | null }) {
  const query = useQuery({
    queryKey: ['public-artist-videos', artistId],
    queryFn: async ({ signal }): Promise<ArtistVideo[]> => {
      const r = await fetch(`https://api.tdfrecords.net/campaign-platform/public/videos?artist_id=${artistId}`, { credentials: 'omit', signal });
      if (r.status === 404) return []; // Feature is independently reviewed/deployed; featured video still works.
      if (!r.ok) throw new Error('Videos temporalmente no disponibles.');
      const data: unknown = await r.json();
      if (!Array.isArray(data) || data.some(v => typeof v?.id !== 'string' || typeof v?.url !== 'string' || typeof v?.title !== 'string')) throw new Error('Respuesta de videos inválida.');
      return data as ArtistVideo[];
    },
    retry: 1,
  });
  const items = query.data ?? [];
  const requested = requestedVideo?.trim();
  const selection = requested ? items.find(v => v.id === requested || videoSourceKey(v.url) === requested) : items.find(v => sameVideo(v.url, featuredUrl));
  const featureMatches = featuredUrl && (!requested || videoSourceKey(featuredUrl) === requested);
  const selected = selection ?? (featureMatches ? { id: 'featured', url: featuredUrl, title: `Video de ${artistName}`, poster: featuredPoster, description: '' } : null);
  const selectedKey = selected?.id === 'featured' ? videoSourceKey(selected.url) : selected?.id;
  if (requested && !selected) return <Alert severity={query.isLoading ? 'info' : 'warning'} action={query.isError ? <Button onClick={() => { void query.refetch(); }}>Reintentar</Button> : undefined}>
    {query.isLoading ? 'Cargando la sesión seleccionada…' : 'No pudimos encontrar esta sesión en el perfil del artista. No se ha sustituido por otro video.'}
  </Alert>;
  return <>
    {selected && <Box component="section" aria-labelledby="artist-session">
      <Typography component="h2" variant="h6" id="artist-session" gutterBottom>{selected.title}</Typography>
      <SessionVideoPreview url={selected.url} title={selected.title} poster={selected.poster} analyticsContext={{ artistId, videoId: videoSourceKey(selected.url) ?? selected.id }} />
      {selected.description && <Typography variant="body2" sx={{ whiteSpace: 'pre-wrap', mt: 1 }}>{selected.description}</Typography>}
      {selectedKey && <Button component="a" href={`/a/${artistId}?video=${encodeURIComponent(selectedKey)}#artist-session`}>Enlace a esta sesión</Button>}
    </Box>}
    {query.isError && <Alert severity="info" action={<Button onClick={() => { void query.refetch(); }}>Reintentar</Button>}>No pudimos cargar los demás videos del artista.</Alert>}
    {items.filter(video => !sameVideo(video.url, selected?.url)).map(video => <Box component="section" key={video.id}>
    <Typography component="h2" variant="h6" gutterBottom>{video.title}</Typography>
    {video.url !== featuredUrl && <SessionVideoPreview url={video.url} title={video.title} poster={video.poster} analyticsContext={{ artistId, videoId: videoSourceKey(video.url) ?? video.id }} />}
    {video.description && <Typography variant="body2" sx={{ whiteSpace: 'pre-wrap', mt: 1 }}>{video.description}</Typography>}
    <Button component="a" href={`/a/${artistId}?video=${encodeURIComponent(video.id)}#artist-session`}>Enlace a esta sesión</Button>
  </Box>)}</>;
}
