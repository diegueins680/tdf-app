import { useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Card, Chip, Stack, Tooltip, Typography } from '@mui/material';
import ArtistProfileLink from '../../components/ArtistProfileLink';
import { Fans } from '../../api/fans';
import type { ArtistProfileDTO, FanFollowDTO } from '../../api/types';
import { recommendationCaption, resolveRecommendedArtists, type RecommendedArtist } from './artistRecommendations';

export const RECOMMENDED_ARTISTS_LIMIT = 6;

interface RecommendedArtistsProps {
  viewerId: number | null | undefined;
  /** True once the signed-in fan's follows are known and not empty. */
  personalized: boolean;
  artists: readonly ArtistProfileDTO[];
  follows: readonly FanFollowDTO[];
  /** Shown while there is nothing to personalize from (guest, or no follows yet). */
  starterArtists: readonly ArtistProfileDTO[];
}

/**
 * "Recomendados para ti". With follows, asks the server for artists followed by fans
 * with similar taste; otherwise shows the starter picks. A failed or empty answer hides
 * the card: recommendations are an extra, never a blocker.
 */
export function RecommendedArtists({ viewerId, personalized, artists, follows, starterArtists }: RecommendedArtistsProps) {
  // Follows are part of the key: following or unfollowing asks again instead of reusing the answer.
  const followKey = useMemo(() => follows.map((follow) => follow.ffArtistId).sort((a, b) => a - b).join(','), [follows]);
  const recommendationsQuery = useQuery({
    queryKey: ['fan-recommendations', viewerId, followKey],
    queryFn: () => Fans.listRecommendations(RECOMMENDED_ARTISTS_LIMIT),
    enabled: Boolean(viewerId && personalized),
    staleTime: 5 * 60 * 1000,
    retry: false,
  });
  const items = useMemo<RecommendedArtist[]>(() => {
    if (!personalized) return starterArtists.map((artist) => ({ artist, reason: 'popular', because: [] }));
    return resolveRecommendedArtists(recommendationsQuery.data ?? [], artists, follows);
  }, [artists, follows, personalized, recommendationsQuery.data, starterArtists]);

  if (items.length === 0) return null;
  const hasSimilarFans = items.some((item) => item.reason === 'similar_fans');

  return (
    <Card sx={{ p: 2, mb: 2 }} data-testid="recommended-artists">
      <Typography variant="h6" gutterBottom>Recomendados para ti</Typography>
      {hasSimilarFans && (
        <Typography variant="body2" color="text.secondary" sx={{ mb: 1 }}>
          Artistas que siguen otros fans con gustos parecidos a los tuyos.
        </Typography>
      )}
      <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
        {items.map((item) => {
          const chip = <Chip label={item.artist.apDisplayName} variant="outlined" />;
          return (
            <ArtistProfileLink key={item.artist.apArtistId} partyId={item.artist.apArtistId} slug={item.artist.apSlug}>
              {personalized ? <Tooltip describeChild title={recommendationCaption(item)}>{chip}</Tooltip> : chip}
            </ArtistProfileLink>
          );
        })}
      </Stack>
    </Card>
  );
}
