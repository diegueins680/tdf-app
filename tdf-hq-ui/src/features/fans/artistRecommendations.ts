import type { ArtistProfileDTO, ArtistRecommendationDTO, ArtistRecommendationReason, FanFollowDTO } from '../../api/types';

export interface RecommendedArtist {
  artist: ArtistProfileDTO;
  reason: ArtistRecommendationReason;
  /** Names of the followed artists behind a `similar_fans` recommendation. */
  because: string[];
}

/**
 * Join server recommendations with the catalog the page already shows.
 * An artist missing from the catalog or followed since the answer was read is dropped,
 * so a stale answer never offers an artist the fan cannot open or already follows.
 */
export function resolveRecommendedArtists(
  recommendations: readonly ArtistRecommendationDTO[],
  artists: readonly ArtistProfileDTO[],
  follows: readonly FanFollowDTO[],
): RecommendedArtist[] {
  const byId = new Map(artists.map((artist) => [artist.apArtistId, artist]));
  const followedNames = new Map(follows.map((follow) => [follow.ffArtistId, follow.ffArtistName]));
  const seen = new Set<number>();
  return recommendations.flatMap((recommendation) => {
    const artist = byId.get(recommendation.arecArtistId);
    if (!artist || followedNames.has(artist.apArtistId) || seen.has(artist.apArtistId)) return [];
    seen.add(artist.apArtistId);
    const because = recommendation.arecReason === 'similar_fans'
      ? recommendation.arecSeedArtistIds.flatMap((id) => {
        const name = followedNames.get(id)?.trim();
        return name ? [name] : [];
      })
      : [];
    return [{ artist, reason: recommendation.arecReason, because }];
  });
}

/** Why an artist is recommended, in the fan's words. */
export function recommendationCaption(recommendation: Pick<RecommendedArtist, 'reason' | 'because'>): string {
  if (recommendation.reason === 'popular') return 'Popular entre los fans';
  const [first, second] = recommendation.because;
  if (!first) return 'Lo siguen fans con gustos parecidos a los tuyos';
  if (!second) return `Porque sigues a ${first}`;
  return `Porque sigues a ${first} y ${second}`;
}
