import { useQuery } from '@tanstack/react-query';
import { Alert, Box, Button, Typography } from '@mui/material';
import SessionVideoPreview from './SessionVideoPreview';

interface ArtistVideo { id: string; title: string; description: string; url: string; poster: string }
export default function ArtistVideoLibrary({ artistId, featuredUrl }: { artistId: number; featuredUrl?: string | null }) {
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
  if (query.isError) return <Alert severity="info" action={<Button onClick={() => { void query.refetch(); }}>Reintentar</Button>}>No pudimos cargar los demás videos del artista.</Alert>;
  return <>{query.data?.filter(v => v.url !== featuredUrl).map(video => <Box component="section" key={video.id}>
    <Typography component="h2" variant="h6" gutterBottom>{video.title}</Typography>
    <SessionVideoPreview url={video.url} title={video.title} poster={video.poster} analyticsContext={{ artistId, videoId: video.id }} />
    {video.description && <Typography variant="body2" sx={{ whiteSpace: 'pre-wrap', mt: 1 }}>{video.description}</Typography>}
  </Box>)}</>;
}
