import { useState } from 'react';
import { Alert, Box } from '@mui/material';
import { artistImageUrlError, normalizeArtistImageUrl } from '../utils/artistImage';

export default function ArtistCoverPreview({ url }: { url?: string | null }) {
  return <Preview key={url} url={url} />;
}

function Preview({ url }: { url?: string | null }) {
  const [failed, setFailed] = useState(false);
  const error = artistImageUrlError(url);
  const image = normalizeArtistImageUrl(url);
  if (!url?.trim()) return null;
  if (error || failed || !image) return <Alert severity="warning">{error ?? 'No pudimos cargar esta imagen. Comprueba que el enlace sea público o vuelve a subir el archivo.'}</Alert>;
  return <Box component="img" src={image} alt="Vista previa de portada" onError={() => setFailed(true)}
    sx={{ display: 'block', width: '100%', maxWidth: 420, height: 180, objectFit: 'cover', borderRadius: 2 }} />;
}
