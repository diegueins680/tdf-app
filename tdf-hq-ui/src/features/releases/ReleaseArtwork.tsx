import { useState } from 'react';
import { Avatar, Box } from '@mui/material';

interface Props {
  sources: string[];
  title: string;
  artistName: string;
}

export default function ReleaseArtwork(props: Props) {
  // Reset failed-image state when catalog data arrives or a row's cover changes.
  return <ArtworkImage key={JSON.stringify(props.sources)} {...props} />;
}

function ArtworkImage({ sources, title, artistName }: Props) {
  const [index, setIndex] = useState(0);
  const src = sources[index];
  const sx = {
    width: 120, height: 120, flexShrink: 0, borderRadius: 2,
    fontWeight: 700, bgcolor: 'action.selected', color: 'text.primary',
    border: '1px solid', borderColor: 'divider',
  };
  if (src) {
    return <Box component="img" src={src} alt={`Portada de ${title}`}
      onError={() => setIndex((current) => current + 1)} sx={{ ...sx, objectFit: 'cover' }} />;
  }
  return (
    <Avatar
      variant="rounded"
      role="img"
      aria-label={`Sin portada disponible: ${title}`}
      title={`Sin portada disponible: ${title}`}
      sx={sx}
    >
      {artistName.trim().slice(0, 2).toUpperCase() || '♪'}
    </Avatar>
  );
}
