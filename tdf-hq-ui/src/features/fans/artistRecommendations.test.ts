import type { ArtistProfileDTO, ArtistRecommendationDTO, FanFollowDTO } from '../../api/types';
import { recommendationCaption, resolveRecommendedArtists } from './artistRecommendations';

const artist = (apArtistId: number, apDisplayName: string): ArtistProfileDTO => (
  { apArtistId, apDisplayName, apGenreIds: [], apFollowerCount: 0, apHasUserAccount: true }
);
const follow = (ffArtistId: number, ffArtistName: string): FanFollowDTO => (
  { ffArtistId, ffArtistName, ffStartedAt: '2026-11-01' }
);
const similar = (arecArtistId: number, arecSeedArtistIds: number[]): ArtistRecommendationDTO => (
  { arecArtistId, arecReason: 'similar_fans', arecScore: 0.5, arecSeedArtistIds }
);
const popular = (arecArtistId: number): ArtistRecommendationDTO => (
  { arecArtistId, arecReason: 'popular', arecScore: 0, arecSeedArtistIds: [] }
);

describe('resolveRecommendedArtists', () => {
  const catalog = [artist(1, 'Seguida'), artist(2, 'Parecida'), artist(3, 'Popular')];
  const follows = [follow(1, 'Seguida'), follow(8, ' Otra seguida ')];

  it('keeps the server order and names the followed artists behind each match', () => {
    const resolved = resolveRecommendedArtists([similar(2, [8, 1, 99]), popular(3)], catalog, follows);
    expect(resolved.map((item) => item.artist.apDisplayName)).toEqual(['Parecida', 'Popular']);
    expect(resolved.map((item) => item.because)).toEqual([['Otra seguida', 'Seguida'], []]);
    expect(resolved.map((item) => item.reason)).toEqual(['similar_fans', 'popular']);
  });

  it('drops artists outside the catalog, already followed or repeated', () => {
    const resolved = resolveRecommendedArtists(
      [similar(77, [1]), similar(1, [8]), similar(2, [1]), popular(2)],
      catalog,
      follows,
    );
    expect(resolved.map((item) => item.artist.apArtistId)).toEqual([2]);
  });
});

describe('recommendationCaption', () => {
  it('explains each kind of recommendation', () => {
    expect(recommendationCaption({ reason: 'popular', because: [] })).toBe('Popular entre los fans');
    expect(recommendationCaption({ reason: 'similar_fans', because: [] }))
      .toBe('Lo siguen fans con gustos parecidos a los tuyos');
    expect(recommendationCaption({ reason: 'similar_fans', because: ['Uno'] })).toBe('Porque sigues a Uno');
    expect(recommendationCaption({ reason: 'similar_fans', because: ['Uno', 'Dos', 'Tres'] }))
      .toBe('Porque sigues a Uno y Dos');
  });
});
