import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string) => Promise<unknown>>();
const postMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();
const putMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();
const delMock = jest.fn<(path: string) => Promise<unknown>>();

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
    await Fans.getLeaderboard(12);
    expect(getMock).toHaveBeenCalledWith('/fans/me/clubs/12/leaderboard');

    await Fans.getLeaderboard(12, 'week');
    expect(getMock).toHaveBeenCalledWith('/fans/me/clubs/12/leaderboard?period=week');
  });

  it('builds discovery feed paths with a limit suffix only when provided', async () => {
    await Fans.getDiscoveryFeed();
    expect(getMock).toHaveBeenCalledWith('/fans/discovery');

    await Fans.getDiscoveryFeed(25);
    expect(getMock).toHaveBeenCalledWith('/fans/discovery?limit=25');
  });

  it('builds notification paths with unreadOnly only when requested', async () => {
    await Fans.listNotifications();
    expect(getMock).toHaveBeenCalledWith('/fans/me/notifications');

    await Fans.listNotifications(true);
    expect(getMock).toHaveBeenCalledWith('/fans/me/notifications?unreadOnly=true');
  });
});
