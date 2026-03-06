import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string) => Promise<unknown>>();
const postMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();
const postFormMock = jest.fn<(path: string, body: FormData) => Promise<unknown>>();
const putMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
  postForm: postFormMock,
  put: putMock,
}));

const { SocialEventsAPI } = await import('./socialEvents');

describe('SocialEventsAPI', () => {
  beforeEach(() => {
    getMock.mockReset();
    postMock.mockReset();
    postFormMock.mockReset();
    putMock.mockReset();
    getMock.mockResolvedValue([]);
    postMock.mockResolvedValue({});
    postFormMock.mockResolvedValue({});
    putMock.mockResolvedValue({});
  });

  it('trims list query params and skips blank filters', async () => {
    await SocialEventsAPI.listEvents({
      city: '  Quito  ',
      startAfter: '   ',
      eventType: ' concert ',
      eventStatus: '',
      artistId: ' 42 ',
      venueId: '   ',
    });

    expect(getMock).toHaveBeenCalledWith('/social-events/events?city=Quito&event_type=concert&artistId=42');
  });

  it('respondInvitation includes invitationToPartyId required by backend schema', async () => {
    getMock.mockResolvedValueOnce([
      {
        invitationId: ' 0012 ',
        invitationToPartyId: ' 99 ',
      },
    ]);

    await SocialEventsAPI.respondInvitation(' 7 ', ' 12 ', 'Accepted', 'Listo');

    expect(getMock).toHaveBeenCalledWith('/social-events/events/7/invitations');
    expect(putMock).toHaveBeenCalledWith(
      '/social-events/events/7/invitations/12',
      {
        invitationToPartyId: '99',
        invitationStatus: 'Accepted',
        invitationMessage: 'Listo',
      },
    );
  });

  it('respondInvitation updates with the canonical matched invitation id', async () => {
    getMock.mockResolvedValueOnce([
      {
        invitationId: '12',
        invitationToPartyId: '99',
      },
    ]);

    await SocialEventsAPI.respondInvitation('7', '0012', 'Accepted');

    expect(putMock).toHaveBeenCalledWith(
      '/social-events/events/7/invitations/12',
      expect.objectContaining({
        invitationToPartyId: '99',
        invitationStatus: 'Accepted',
      }),
    );
  });

  it('respondInvitation throws when invitation is not found', async () => {
    getMock.mockResolvedValueOnce([]);

    await expect(SocialEventsAPI.respondInvitation('7', '88', 'Declined')).rejects.toThrow(
      'Invitation 88 not found for event 7.',
    );
    expect(putMock).not.toHaveBeenCalled();
  });

  it('respondInvitation throws when invitationToPartyId is blank', async () => {
    getMock.mockResolvedValueOnce([
      {
        invitationId: '88',
        invitationToPartyId: '   ',
      },
    ]);

    await expect(SocialEventsAPI.respondInvitation('7', '88', 'Accepted')).rejects.toThrow(
      'Invitation 88 is missing invitationToPartyId.',
    );
    expect(putMock).not.toHaveBeenCalled();
  });

  it('respondInvitation throws when invitationToPartyId is missing', async () => {
    getMock.mockResolvedValueOnce([
      {
        invitationId: '88',
      },
    ]);

    await expect(SocialEventsAPI.respondInvitation('7', '88', 'Accepted')).rejects.toThrow(
      'Invitation 88 is missing invitationToPartyId.',
    );
    expect(putMock).not.toHaveBeenCalled();
  });

  it('respondInvitation validates required identifiers before requesting data', async () => {
    await expect(SocialEventsAPI.respondInvitation('   ', '88', 'Accepted')).rejects.toThrow(
      'eventId is required to respond to an invitation.',
    );
    await expect(SocialEventsAPI.respondInvitation('7', '   ', 'Accepted')).rejects.toThrow(
      'invitationId is required to respond to an invitation.',
    );

    expect(getMock).not.toHaveBeenCalled();
    expect(putMock).not.toHaveBeenCalled();
  });
});
