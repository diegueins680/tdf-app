import { useMemo } from 'react';
import { Box, Stack, Typography, Button, Chip } from '@mui/material';
import type { NormalizedStreamingSource, StreamingSource } from '../utils/media';
import { normalizeStreamingSource } from '../utils/media';
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
) => {
  if (source.provider === 'youtube') {
    return (
      <Box sx={{ position: 'relative', pt: '56.25%', borderRadius: 1.5, overflow: 'hidden' }}>
        <Box
          component="iframe"
          src={source.url}
          title={source.label}
          allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
          allowFullScreen
          loading="lazy"
          sx={{ position: 'absolute', inset: 0, width: '100%', height: '100%', border: 'none' }}
        />
      </Box>
    );
  }

  if (source.provider === 'spotify') {
    return (
      <Box
        component="iframe"
        src={source.url}
        title={source.label}
        allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture"
        loading="lazy"
        sx={{ border: 'none', borderRadius: 1.5, width: '100%', height: 152, bgcolor: 'background.default' }}
      />
    );
  }

  if (source.provider === 'video') {
    return (
      <Box
        component="video"
        src={source.url}
        controls
        preload="metadata"
        poster={posterUrl ?? undefined}
        playsInline
        sx={{ width: '100%', borderRadius: 1.5, bgcolor: 'background.default' }}
      >
        {/* Placeholder for future captions — no src until subtitles are available */}
        <track kind="captions" />
        Tu navegador no soporta la reproducción de video.
      </Box>
    );
  }

  if (source.provider === 'audio') {
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
              sources: [{ url: source.url, quality: 'high', mediaType: 'audio/mpeg' }],
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

      {renderPrimaryStream(primary, title, artist, posterUrl)}

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
