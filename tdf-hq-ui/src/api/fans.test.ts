import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string) => Promise<unknown>>();
const postMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();
const putMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();
const delMock = jest.fn<(path: string) => Promise<unknown>>();

type FansApiPathFixtureContract = Readonly<{
  leaderboardClubId: number;
  weeklyLeaderboardPeriod: string;
  discoveryFeedLimit: number;
}>;

const LEADERBOARD_PATH_FIXTURE_CLUB_ID = 10 + 2;
const DISCOVERY_FEED_FIXTURE_LIMIT = 5 * 5;

// Invariant: numeric path fixtures are positive integers so route segments and
// query parameters serialize without rounding, signs, or fractional suffixes.
const FANS_API_PATH_FIXTURES = {
  leaderboardClubId: LEADERBOARD_PATH_FIXTURE_CLUB_ID,
  weeklyLeaderboardPeriod: 'week',
  discoveryFeedLimit: DISCOVERY_FEED_FIXTURE_LIMIT,
} as const satisfies FansApiPathFixtureContract;

const EXPECTED_LEADERBOARD_PATH =
  `/fans/me/clubs/${FANS_API_PATH_FIXTURES.leaderboardClubId}/leaderboard`;
const EXPECTED_WEEKLY_LEADERBOARD_PATH =
  `${EXPECTED_LEADERBOARD_PATH}?period=${FANS_API_PATH_FIXTURES.weeklyLeaderboardPeriod}`;
const EXPECTED_DISCOVERY_FEED_PATH = '/fans/discovery';
const EXPECTED_LIMITED_DISCOVERY_FEED_PATH =
  `${EXPECTED_DISCOVERY_FEED_PATH}?limit=${FANS_API_PATH_FIXTURES.discoveryFeedLimit}`;

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
});
