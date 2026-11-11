import ArtistProfileLink from '../../components/ArtistProfileLink';
import { Box, Card, CardActionArea, CardContent, CardMedia, Chip, Stack, Typography } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { Link as RouterLink } from 'react-router-dom';
import { musicReleases, type MusicPublicReleaseSummary } from '../../api/musicReleases';

/** A single presentation for authorized published releases across catalog, profile and feeds. */
export function PublishedReleaseCard({ release, compact = false, heading = 'h3' }: {
  release: MusicPublicReleaseSummary; compact?: boolean; heading?: 'h2' | 'h3';
}) {
  const cover = useQuery({
    queryKey: ['music-release-cover', release.coverAssetId],
    queryFn: () => musicReleases.getAssetAccess(release.coverAssetId!),
    enabled: !compact && Boolean(release.coverAssetId),
    retry: false,
  });
  return <Card variant="outlined" sx={{ height: '100%', borderRadius: 3 }}>
    <CardActionArea component={RouterLink} to={`/musica/${encodeURIComponent(release.slug)}`} sx={{ alignItems: 'stretch' }}>
      {!compact && (cover.data?.url
        ? <CardMedia component="img" image={cover.data.url} alt={`Portada de ${release.title}`} loading="lazy" sx={{ aspectRatio: '1', objectFit: 'cover' }} />
        : <Box sx={{ aspectRatio: '1', bgcolor: 'action.hover' }} aria-hidden="true" />)}
      <CardContent sx={{ pb: 1 }}>
        <Typography component={heading} variant={compact ? 'subtitle1' : 'h6'} fontWeight={800}>{release.title}</Typography>
      </CardContent>
    </CardActionArea>
    <CardContent sx={{ pt: 0 }}>
      <Stack spacing={1}>
        <Typography color="text.secondary">
          <ArtistProfileLink partyId={release.artistPartyId}>{release.displayArtist}</ArtistProfileLink>
        </Typography>
        <Chip label={`${release.kind.toUpperCase()} · TDF`} size="small" sx={{ alignSelf: 'flex-start' }} />
      </Stack>
    </CardContent>
  </Card>;
}
