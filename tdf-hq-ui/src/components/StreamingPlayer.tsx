import { useMemo } from 'react';
import { Box, Stack, Typography, Button, Chip } from '@mui/material';
import type { NormalizedStreamingSource, StreamingSource } from '../utils/media';
import { normalizeStreamingSource } from '../utils/media';
import { useOptionalPlayer } from '../player/PlayerProvider';
import { assertNever } from '../utils/assertNever';
import { PLAYER_LOAD_TRACK_EVENT, type LoadPlayerTrackDetail } from '../player/types';

export type StreamingPlayerVariant = 'regular' | 'compact';

interface StreamingPlayerProps {
  title: string;
  artist?: string;
  sources: StreamingSource[];
  posterUrl?: string | null;
  variant?: StreamingPlayerVariant;
}

const playerArtistName = (artist?: string): string => {
  const trimmed = artist?.trim();
  if (trimmed === '') return 'Artista TDF';
  return trimmed ?? 'Artista TDF';
};

const renderPrimaryStream = (
  source: NormalizedStreamingSource,
  title: string,
  artist?: string,
  posterUrl?: string | null,
  pauseNative?: () => void,
) => {
  if (source.provider === 'youtube' || source.provider === 'spotify') {
    return <Button component="a" href={source.url} target="_blank" rel="noopener noreferrer"
      onClick={pauseNative} aria-label={`${source.label} (abre en nueva pestaña)`}>
      Escuchar en {source.label}
    </Button>;
  }

  if (source.provider === 'audio' || source.provider === 'video') {
    return (
      <Button
        variant="contained"
        startIcon={<span aria-hidden="true">▶</span>}
        onClick={() => {
          const detail: LoadPlayerTrackDetail = {
            track: {
              id: `legacy-audio:${source.url}`,
              title,
              artist: playerArtistName(artist),
              artworkUrl: posterUrl,
              sources: [{ url: source.url, quality: 'high', mediaType: source.provider === 'video' ? 'video/mp4' : 'audio/mpeg' }],
            },
            autoplay: true,
          };
          window.dispatchEvent(new CustomEvent(PLAYER_LOAD_TRACK_EVENT, { detail }));
        }}
      >
        Reproducir en el player global
      </Button>
    );
  }

  return assertNever(source.provider, 'stream provider');
};

const getBadgeColor = (provider: NormalizedStreamingSource['provider']) => {
  switch (provider) {
    case 'video':
      return 'primary';
    case 'youtube':
      return 'error';
    case 'spotify':
      return 'success';
    case 'audio':
      return 'default';
  }

  return assertNever(provider, 'stream provider');
};

export function StreamingPlayer({
  title,
  artist,
  sources,
  posterUrl,
  variant = 'regular',
}: StreamingPlayerProps) {
  const player = useOptionalPlayer();
  const normalizedSources = useMemo(
    () => sources.map((source) => normalizeStreamingSource(source)).filter(Boolean) as NormalizedStreamingSource[],
    [sources],
  );

  const primary = normalizedSources[0];
  const fallbacks = normalizedSources.slice(1);

  if (!primary) return null;

  const padding = variant === 'compact' ? 1.5 : 2;

  return (
    <Box
      sx={{
        p: padding,
        borderRadius: 2,
        border: '1px solid',
        borderColor: 'divider',
        bgcolor: 'background.paper',
        display: 'flex',
        flexDirection: 'column',
        gap: 1.5,
      }}
    >
      <Stack direction="row" alignItems="center" spacing={1} justifyContent="space-between">
        <Box>
          {artist && (
            <Typography variant="caption" color="text.secondary">
              {artist}
            </Typography>
          )}
          <Typography variant="subtitle1" fontWeight={700} lineHeight={1.2}>
            {title}
          </Typography>
        </Box>
        <Chip size="small" color={getBadgeColor(primary.provider)} label={primary.label} />
      </Stack>

      {renderPrimaryStream(primary, title, artist, posterUrl, player?.pause)}

      {fallbacks.length > 0 && (
        <Stack direction="row" spacing={1} flexWrap="wrap" alignItems="center">
          <Typography variant="caption" color="text.secondary">
            Fuentes alternativas:
          </Typography>
          {fallbacks.map((source) => (
            <Button
              key={`${source.provider}-${source.url}`}
              size="small"
              variant="text"
              component="a"
              href={source.url}
              onClick={() => player?.pause()}
              target="_blank"
              rel="noopener noreferrer"
              aria-label={`${source.label} (abre en nueva pestaña)`}
            >
              {source.label}
            </Button>
          ))}
        </Stack>
      )}
    </Box>
  );
}

export default StreamingPlayer;
