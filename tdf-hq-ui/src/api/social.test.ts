import { jest } from '@jest/globals';

const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const postMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const delMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
  del: delMock,
}));

const { SocialAPI } = await import('./social');

describe('SocialAPI party id guards', () => {
  beforeEach(() => {
    getMock.mockReset();
    postMock.mockReset();
    delMock.mockReset();
  });

  it('rejects invalid party ids for add/remove/exchange calls', () => {
    expect(() => SocialAPI.addFriend(0)).toThrow('Party ID inválido para agregar amistad.');
    expect(() => SocialAPI.exchangeVCard(1.5)).toThrow('Party ID inválido para intercambio de vCard.');
    expect(() => SocialAPI.removeFriend(-3)).toThrow('Party ID inválido para eliminar amistad.');
    expect(postMock).not.toHaveBeenCalled();
    expect(delMock).not.toHaveBeenCalled();
  });

  it('calls expected endpoints for valid party ids', async () => {
    postMock.mockResolvedValueOnce([{ pfFollowerId: 1, pfFollowingId: 2, pfViaNfc: false, pfStartedAt: '2026-01-01T00:00:00Z' }]);
    postMock.mockResolvedValueOnce([{ pfFollowerId: 1, pfFollowingId: 2, pfViaNfc: false, pfStartedAt: '2026-01-01T00:00:00Z' }]);
    delMock.mockResolvedValueOnce(undefined);

    await SocialAPI.addFriend(22);
    await SocialAPI.exchangeVCard(22);
    await SocialAPI.removeFriend(22);

    expect(postMock).toHaveBeenNthCalledWith(1, '/social/friends/22', {});
    expect(postMock).toHaveBeenNthCalledWith(2, '/social/vcard-exchange', { vcerPartyId: 22 });
    expect(delMock).toHaveBeenCalledWith('/social/friends/22');
  });
});
