import { jest } from '@jest/globals';

const envReadMock = jest.fn<(key: string) => string | undefined>(() => undefined);

jest.unstable_mockModule('../utils/env', () => ({
  env: {
    read: envReadMock,
  },
}));

const {
  completeOnboardingProgress,
  loadOnboardingProgress,
  loadSessionSnapshot,
  persistOnboardingIntent,
} = await import('./session');

const progressPayload = {
  eligible: true,
  signupCompletedAt: '2026-09-06T19:00:00Z',
  onboardingIntent: 'follow_artists',
  completedAt: null,
  firstValue: null,
  firstValueCompletedAt: null,
  updatedAt: '2026-09-06T19:00:00Z',
};

describe('session api', () => {
  const fetchMock = jest.fn<typeof fetch>();

  beforeEach(() => {
    fetchMock.mockReset();
    (globalThis as unknown as { fetch: typeof fetch }).fetch = fetchMock;
  });

  it('returns null when the backend reports no authenticated session', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: false,
      status: 401,
      text: jest.fn<() => Promise<string>>().mockResolvedValue('Invalid or inactive token'),
    } as unknown as Response);

    await expect(loadSessionSnapshot()).resolves.toBeNull();
  });

  it('accepts a public null session payload without treating it as an error', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: true,
      status: 200,
      json: jest.fn<() => Promise<null>>().mockResolvedValue(null),
    } as unknown as Response);

    await expect(loadSessionSnapshot()).resolves.toBeNull();
  });

  it('returns the authenticated session snapshot when present', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: true,
      status: 200,
      json: jest.fn<() => Promise<unknown>>().mockResolvedValue({
        username: 'alice',
        displayName: 'Alice',
        roles: ['Admin'],
        modules: ['CRM'],
        partyId: 42,
      }),
    } as unknown as Response);

    await expect(loadSessionSnapshot()).resolves.toEqual({
      username: 'alice',
      displayName: 'Alice',
      roles: ['Admin'],
      modules: ['CRM'],
      partyId: 42,
    });
  });

  it('records first value completion against the authenticated session', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: true,
      status: 200,
      json: jest.fn<() => Promise<unknown>>().mockResolvedValue({
        progress: {
          eligible: false,
          signupCompletedAt: '2026-09-06T19:00:00Z',
          onboardingIntent: 'follow_artists',
          completedAt: '2026-09-06T19:05:00Z',
          firstValue: 'artist_followed',
          firstValueCompletedAt: '2026-09-06T19:05:00Z',
          updatedAt: '2026-09-06T19:05:00Z',
        },
        newlyCompleted: true,
      }),
    } as unknown as Response);

    await expect(completeOnboardingProgress('artist_followed')).resolves.toMatchObject({
      newlyCompleted: true,
      progress: { firstValue: 'artist_followed' },
    });
    expect(fetchMock).toHaveBeenCalledWith(
      expect.stringContaining('/session/onboarding/complete'),
      expect.objectContaining({
        method: 'POST',
        credentials: 'include',
        body: JSON.stringify({ firstValue: 'artist_followed' }),
      }),
    );
  });

  it('loads account-bound onboarding eligibility', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: true,
      status: 200,
      json: jest.fn<() => Promise<unknown>>().mockResolvedValue(progressPayload),
    } as unknown as Response);

    await expect(loadOnboardingProgress()).resolves.toEqual(progressPayload);
    expect(fetchMock).toHaveBeenCalledWith(
      expect.stringContaining('/session/onboarding'),
      { credentials: 'include' },
    );
  });

  it('persists product intent without sending a security role', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: true,
      status: 200,
      json: jest.fn<() => Promise<unknown>>().mockResolvedValue(progressPayload),
    } as unknown as Response);

    await expect(persistOnboardingIntent('follow_artists', 'session-token')).resolves.toEqual(progressPayload);
    expect(fetchMock).toHaveBeenCalledWith(
      expect.stringContaining('/session/onboarding/intent'),
      expect.objectContaining({
        method: 'PUT',
        credentials: 'include',
        headers: {
          Authorization: 'Bearer session-token',
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ onboardingIntent: 'follow_artists' }),
      }),
    );
  });

  it('supports an explicit optional-onboarding exit without inventing a first value', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: true,
      status: 200,
      json: jest.fn<() => Promise<unknown>>().mockResolvedValue({
        progress: { ...progressPayload, eligible: false, completedAt: '2026-09-06T19:05:00Z' },
        newlyCompleted: true,
      }),
    } as unknown as Response);

    await expect(completeOnboardingProgress()).resolves.toMatchObject({ newlyCompleted: true });
    expect(fetchMock).toHaveBeenCalledWith(
      expect.stringContaining('/session/onboarding/complete'),
      expect.objectContaining({ body: '{}' }),
    );
  });

  it('surfaces a durable onboarding completion failure', async () => {
    fetchMock.mockResolvedValueOnce({
      ok: false,
      status: 401,
      statusText: 'Unauthorized',
      text: jest.fn<() => Promise<string>>().mockResolvedValue('Authentication required'),
    } as unknown as Response);

    await expect(completeOnboardingProgress('access_requested')).rejects.toThrow(
      'Authentication required',
    );
  });
});
