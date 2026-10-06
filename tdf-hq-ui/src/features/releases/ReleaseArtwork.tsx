import { useState } from 'react';
import { Avatar, Box } from '@mui/material';

interface Props {
  sources: string[];
  title: string;
  artistName: string;
}

export default function ReleaseArtwork({ sources, title, artistName }: Props) {
  const [attempt, setAttempt] = useState({ sources, index: 0 });
  const sourcesChanged = sources.length !== attempt.sources.length
    || sources.some((source, index) => source !== attempt.sources[index]);
  // Compare URL references/values without copying potentially multi-MB data URLs
  // into keys. Equal source lists retain failures across ordinary parent renders.
  if (sourcesChanged) setAttempt({ sources, index: 0 });
  const src = sources[sourcesChanged ? 0 : attempt.index];
  const sx = {
    width: 120, height: 120, flexShrink: 0, borderRadius: 2,
    fontWeight: 700, bgcolor: 'action.selected', color: 'text.primary',
    border: '1px solid', borderColor: 'divider',
  };
  if (src) {
    return <Box component="img" src={src} alt={`Portada de ${title}`}
      onError={() => setAttempt((current) => current.sources[current.index] === src
        ? { ...current, index: current.index + 1 } : current)} sx={{ ...sx, objectFit: 'cover' }} />;
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
