import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string) => Promise<unknown>>();
const postMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();
const putMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();
const delMock = jest.fn<(path: string) => Promise<unknown>>();

type FansApiPathFixtureContract = Readonly<{
  leaderboardClubId: number;
  weeklyLeaderboardPeriod: string;
  discoveryFeedLimit: number;
  followedArtistId: number;
  artistSlug: string;
}>;

const LEADERBOARD_PATH_FIXTURE_CLUB_ID = 10 + 2;
const DISCOVERY_FEED_FIXTURE_LIMIT = 5 * 5;
const FOLLOWED_ARTIST_FIXTURE_ID = 4 * 10 + 2;

// Invariant: numeric path fixtures are positive integers so route segments and
// query parameters serialize without rounding, signs, or fractional suffixes.
const FANS_API_PATH_FIXTURES = {
  leaderboardClubId: LEADERBOARD_PATH_FIXTURE_CLUB_ID,
  weeklyLeaderboardPeriod: 'week',
  discoveryFeedLimit: DISCOVERY_FEED_FIXTURE_LIMIT,
  followedArtistId: FOLLOWED_ARTIST_FIXTURE_ID,
  artistSlug: 'los mentores',
} as const satisfies FansApiPathFixtureContract;

const EXPECTED_LEADERBOARD_PATH =
  `/fans/me/clubs/${FANS_API_PATH_FIXTURES.leaderboardClubId}/leaderboard`;
const EXPECTED_WEEKLY_LEADERBOARD_PATH =
  `${EXPECTED_LEADERBOARD_PATH}?period=${FANS_API_PATH_FIXTURES.weeklyLeaderboardPeriod}`;
const EXPECTED_DISCOVERY_FEED_PATH = '/fans/discovery';
const EXPECTED_LIMITED_DISCOVERY_FEED_PATH =
  `${EXPECTED_DISCOVERY_FEED_PATH}?limit=${FANS_API_PATH_FIXTURES.discoveryFeedLimit}`;
const EXPECTED_PUBLIC_ARTISTS_PATH = '/fans/artists';
const EXPECTED_ARTIST_SEARCH_PATH = '/artists/search';
const EXPECTED_FILTERED_ARTIST_SEARCH_PATH =
  `${EXPECTED_ARTIST_SEARCH_PATH}?q=neo+soul&genre=latin`;
const EXPECTED_PUBLIC_ARTIST_PATH =
  `/artists/${encodeURIComponent(FANS_API_PATH_FIXTURES.artistSlug)}/public`;
const EXPECTED_FOLLOWED_ARTIST_PATH =
  `/fans/me/follows/${FANS_API_PATH_FIXTURES.followedArtistId}`;

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
  put: putMock,
  del: delMock,
}));

const { Fans } = await import('./fans');

describe('Fans API optional query paths', () => {
  beforeEach(() => {
    getMock.mockReset();
    postMock.mockReset();
    putMock.mockReset();
    delMock.mockReset();
    getMock.mockResolvedValue([]);
  });

  it('builds leaderboard paths with a period suffix only when provided', async () => {
    await Fans.getLeaderboard(FANS_API_PATH_FIXTURES.leaderboardClubId);
    expect(getMock).toHaveBeenCalledWith(EXPECTED_LEADERBOARD_PATH);

    await Fans.getLeaderboard(
      FANS_API_PATH_FIXTURES.leaderboardClubId,
      FANS_API_PATH_FIXTURES.weeklyLeaderboardPeriod,
    );
    expect(getMock).toHaveBeenCalledWith(EXPECTED_WEEKLY_LEADERBOARD_PATH);
  });

  it('builds discovery feed paths with a limit suffix only when provided', async () => {
    await Fans.getDiscoveryFeed();
    expect(getMock).toHaveBeenCalledWith(EXPECTED_DISCOVERY_FEED_PATH);

    await Fans.getDiscoveryFeed(FANS_API_PATH_FIXTURES.discoveryFeedLimit);
    expect(getMock).toHaveBeenCalledWith(EXPECTED_LIMITED_DISCOVERY_FEED_PATH);
  });

  it('uses positive integer fixtures for path and query contracts', () => {
    const numericPathFixtures = [
      FANS_API_PATH_FIXTURES.leaderboardClubId,
      FANS_API_PATH_FIXTURES.discoveryFeedLimit,
      FANS_API_PATH_FIXTURES.followedArtistId,
    ];

    for (const value of numericPathFixtures) {
      expect(Number.isInteger(value)).toBe(true);
      expect(value).toBeGreaterThan(0);
    }
  });

  it('builds notification paths with unreadOnly only when requested', async () => {
    await Fans.listNotifications();
    expect(getMock).toHaveBeenCalledWith('/fans/me/notifications');

    await Fans.listNotifications(true);
    expect(getMock).toHaveBeenCalledWith('/fans/me/notifications?unreadOnly=true');
  });

  it('falls back to an empty notification state when production lacks notification routes', async () => {
    const unsupportedRouteError = Object.assign(new Error('Not found'), { status: 404 });
    getMock
      .mockRejectedValueOnce(unsupportedRouteError)
      .mockRejectedValueOnce(unsupportedRouteError);

    await expect(Fans.listNotifications()).resolves.toEqual([]);
    await expect(Fans.getNotificationCount()).resolves.toEqual({ ncUnread: 0 });
  });

  it('builds public artist search paths with filters only when provided', async () => {
    await Fans.searchArtists();
    expect(getMock).toHaveBeenCalledWith(EXPECTED_ARTIST_SEARCH_PATH);

    await Fans.searchArtists({ q: 'neo soul', genre: 'latin' });
    expect(getMock).toHaveBeenCalledWith(EXPECTED_FILTERED_ARTIST_SEARCH_PATH);
  });

  it('uses the available fan artist feed for public artist rankings', async () => {
    await Fans.listPublicArtists();
    expect(getMock).toHaveBeenCalledWith(EXPECTED_PUBLIC_ARTISTS_PATH);
  });

  it('builds direct public artist lookup paths with encoded refs', async () => {
    await Fans.getPublicArtist(FANS_API_PATH_FIXTURES.artistSlug);
    expect(getMock).toHaveBeenCalledWith(EXPECTED_PUBLIC_ARTIST_PATH);
  });

  it('uses fan profile and artist follow routes for fan onboarding', async () => {
    const profilePayload = {
      fpuDisplayName: 'Maria Caridad',
      fpuFavoriteGenres: 'DJ, house',
      fpuCity: 'Quito',
    };

    await Fans.getProfile();
    expect(getMock).toHaveBeenCalledWith('/fans/me/profile');

    await Fans.updateProfile(profilePayload);
    expect(putMock).toHaveBeenCalledWith('/fans/me/profile', profilePayload);

    await Fans.listFollows();
    expect(getMock).toHaveBeenCalledWith('/fans/me/follows');

    await Fans.follow(FANS_API_PATH_FIXTURES.followedArtistId);
    expect(postMock).toHaveBeenCalledWith(EXPECTED_FOLLOWED_ARTIST_PATH, {});

    await Fans.unfollow(FANS_API_PATH_FIXTURES.followedArtistId);
    expect(delMock).toHaveBeenCalledWith(EXPECTED_FOLLOWED_ARTIST_PATH);
  });

  it('uses the artist profile alias routes for profile creation and photo updates', async () => {
    const artistProfilePayload = {
      apuArtistId: FANS_API_PATH_FIXTURES.followedArtistId,
      apuDisplayName: 'Los Mentores',
    };
    const photoPayload = {
      apuHeroImageUrl: 'https://cdn.example.test/hero.jpg',
    };

    await Fans.createMyArtistProfile(artistProfilePayload);
    expect(postMock).toHaveBeenCalledWith('/artists/me/profile', artistProfilePayload);

    await Fans.updateMyArtistPhoto(photoPayload);
    expect(postMock).toHaveBeenCalledWith('/artists/me/photo', photoPayload);
  });
});
