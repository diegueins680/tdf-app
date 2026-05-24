import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string) => Promise<unknown>>();
const postMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();
const postFormMock = jest.fn<(path: string, body: FormData) => Promise<unknown>>();
const putMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();
const delMock = jest.fn<(path: string) => Promise<unknown>>();

type RawPathSegment = string & { readonly __rawPathSegment: 'RawPathSegment' };
type EncodedPathSegment = string & { readonly __encodedPathSegment: 'EncodedPathSegment' };

const rawPathSegment = (value: string): RawPathSegment => {
  if (value.trim().length === 0) {
    throw new Error('Test path segment fixtures must be non-blank.');
  }
  if (value.includes('%')) {
    throw new Error('Test path segment fixtures must be raw, not pre-encoded.');
  }
  return value as RawPathSegment;
};

const encodeExpectedPathSegment = (value: RawPathSegment): EncodedPathSegment => {
  const encoded = encodeURIComponent(value);
  if (encoded.includes('/')) {
    throw new Error('Encoded path segment fixtures must not contain raw slashes.');
  }
  return encoded as EncodedPathSegment;
};

const WAITLIST_EVENT_ID_WITH_SPACE = rawPathSegment('event 7');
const WAITLIST_ENTRY_ID_WITH_SLASH = rawPathSegment('entry/a');
const WAITLIST_TIER_ID_WITH_SLASH = rawPathSegment('tier/3');
const PADDED_WAITLIST_TIER_ID_WITH_SLASH = ` ${WAITLIST_TIER_ID_WITH_SLASH} `;
const WAITLIST_EVENT_PATH = `/social-events/events/${encodeExpectedPathSegment(WAITLIST_EVENT_ID_WITH_SPACE)}/waitlist`;
const WAITLIST_ENTRY_PATH = `${WAITLIST_EVENT_PATH}/${encodeExpectedPathSegment(WAITLIST_ENTRY_ID_WITH_SLASH)}`;
const WAITLIST_TIER_QUERY_PATH = `${WAITLIST_EVENT_PATH}?tierId=${encodeExpectedPathSegment(WAITLIST_TIER_ID_WITH_SLASH)}`;

jest.unstable_mockModule('./client', () => ({
  del: delMock,
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
    delMock.mockReset();
    getMock.mockResolvedValue([]);
    postMock.mockResolvedValue({});
    postFormMock.mockResolvedValue({});
    putMock.mockResolvedValue({});
    delMock.mockResolvedValue({});
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

  it('removes waitlist entries through the shared API client', async () => {
    await SocialEventsAPI.removeFromWaitlist(WAITLIST_EVENT_ID_WITH_SPACE, WAITLIST_ENTRY_ID_WITH_SLASH);

    expect(delMock).toHaveBeenCalledWith(WAITLIST_ENTRY_PATH);
  });

  it('lists waitlist entries with a trimmed tier filter', async () => {
    await SocialEventsAPI.listWaitlist(WAITLIST_EVENT_ID_WITH_SPACE, PADDED_WAITLIST_TIER_ID_WITH_SLASH);

    expect(getMock).toHaveBeenCalledWith(WAITLIST_TIER_QUERY_PATH);
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
