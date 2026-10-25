import { jest } from '@jest/globals';
import { act, type ReactNode } from 'react';
import { cleanup, renderHook, waitFor } from '@testing-library/react';
import type { FanProfileDTO, FanProfileUpdate } from '../../api/types';

// Retain the real provider, auth epoch, fan endpoints, API client and auth header.
// Only session bootstrap/analytics are unrelated to the profile dispatch boundary.
jest.unstable_mockModule('../../api/session', () => ({
  loadSessionSnapshot: jest.fn(async () => null),
  logoutSessionRequest: jest.fn(async () => undefined),
  reconcileOnboardingProgress: jest.fn(async () => ({ newlyCompleted: false, progress: {
    eligible: false, signupCompletedAt: null, onboardingIntent: null, completedAt: null,
    firstValue: null, firstValueCompletedAt: null, updatedAt: null,
  } })),
}));
jest.unstable_mockModule('../../analytics/posthog', () => ({
  getAnalyticsClient: () => ({ ready: false }),
}));

const { getAuthSessionEpoch, advanceAuthSessionEpoch } = await import('../../session/authEvents');
const { SessionProvider, useSession, getStoredSessionToken } = await import('../../session/SessionContext');
const { SessionQueryProvider } = await import('../../session/SessionQueryProvider');
const { useOwnedProfileDraft } = await import('./useOwnedProfileDraft');
const { useFanProfile } = await import('./useFanProfile');

const emptyDraft = { city: '' };
const loadedDraft = { city: 'Quito' };
const fetchMock = jest.fn<typeof fetch>();
const profileWrites: { bearer: string | null; body: FanProfileUpdate }[] = [];
const profileReads: (string | null)[] = [];
const originalFetch = globalThis.fetch;

function jsonResponse(value: unknown): Response {
  return {
    ok: true, status: 200, statusText: 'OK',
    headers: { get: (name: string) => name.toLowerCase() === 'content-type' ? 'application/json' : null },
    text: async () => JSON.stringify(value),
    json: async () => value,
  } as unknown as Response;
}

function profile(partyId: number): FanProfileDTO {
  return { fpArtistId: partyId, fpDisplayName: `Fan ${partyId}`, fpCity: `City ${partyId}`,
    fpBio: `Biography ${partyId}`, fpAvatarUrl: `https://images.example.test/${partyId}.jpg`, fpFavoriteGenreIds: [] };
}

function account(partyId: number, token: string) {
  return { partyId, username: `fan-${partyId}`, displayName: `Fan ${partyId}`, roles: ['Fan'], apiToken: token };
}

function renderProfile() {
  return renderHook(() => {
    const auth = useSession();
    const editor = useFanProfile({ enabled: Boolean(auth.session && !auth.loading), viewerId: auth.session?.partyId ?? null });
    return { auth, editor };
  }, { wrapper: ({ children }: { children: ReactNode }) => (
    <SessionProvider><SessionQueryProvider>{children}</SessionQueryProvider></SessionProvider>
  ) });
}

beforeEach(() => {
  window.localStorage.clear();
  window.sessionStorage.clear();
  profileWrites.length = 0;
  profileReads.length = 0;
  fetchMock.mockReset().mockImplementation(async (url, init) => {
    if (typeof url !== 'string' || !url.endsWith('/fans/me/profile')) throw new Error('Unexpected test request URL');
    const bearer = new Headers(init?.headers).get('Authorization');
    const partyId = bearer === 'Bearer synthetic-B' ? 84 : 42;
    if (init?.method === 'PUT') {
      if (typeof init.body !== 'string') throw new Error('Expected a JSON string profile body');
      profileWrites.push({ bearer, body: JSON.parse(init.body) as FanProfileUpdate });
    } else {
      profileReads.push(bearer);
    }
    return jsonResponse(profile(partyId));
  });
  globalThis.fetch = fetchMock;
});

afterEach(() => {
  cleanup();
  globalThis.fetch = originalFetch;
});

describe('profile ownership at the synchronous authentication boundary', () => {
  it('revokes a captured owner guard immediately when auth advances, before another React render', () => {
    const { result } = renderHook(() => useOwnedProfileDraft({ ownerId: 42, baseline: loadedDraft, empty: emptyDraft }));
    expect(result.current.ready).toBe(true);
    const capturedOwner = result.current.isCurrentOwner;
    const epoch = getAuthSessionEpoch();
    advanceAuthSessionEpoch();
    expect(getAuthSessionEpoch()).toBe(epoch + 1);
    expect(capturedOwner()).toBe(false);
  });

  it('rejects an old upload setter immediately after auth advances without waiting for a provider render', () => {
    const { result } = renderHook(() => useOwnedProfileDraft({ ownerId: 42, baseline: loadedDraft, empty: emptyDraft }));
    const capturedSetter = result.current.setDraft;
    advanceAuthSessionEpoch();
    act(() => { capturedSetter({ city: 'Old upload completion' }); });
    expect(result.current.draft.city).not.toBe('Old upload completion');
  });

  it('never dispatches account A profile fields using B credentials during a batched login transition', async () => {
    const { result } = renderProfile();
    await waitFor(() => expect(result.current.auth.loading).toBe(false));
    act(() => { result.current.auth.login(account(42, 'synthetic-A')); });
    await waitFor(() => expect(result.current.editor.profileReady).toBe(true));
    expect(result.current.editor.profileDraft.fpuDisplayName).toBe('Fan 42');
    const capturedSaveA = result.current.editor.saveProfile;

    // React batches the provider update while login changes the imperative token
    // immediately. Let the real React Query mutation microtask run in that gap.
    await act(async () => {
      result.current.auth.login(account(84, 'synthetic-B'));
      expect(getStoredSessionToken()).toBe('synthetic-B');
      capturedSaveA();
      await Promise.resolve();
      await Promise.resolve();
    });
    await waitFor(() => expect(result.current.editor.updateProfileMutation.isPending).toBe(false));
    expect(profileWrites).toEqual([]);

    await waitFor(() => expect(result.current.editor.profileDraft.fpuDisplayName).toBe('Fan 84'));
    act(() => { result.current.editor.setProfileDraft((draft) => ({ ...draft, fpuCity: 'B intended city' })); });
    act(() => { result.current.editor.saveProfile(); });
    await waitFor(() => expect(profileWrites).toHaveLength(1));
    expect(profileWrites[0]).toMatchObject({ bearer: 'Bearer synthetic-B', body: {
      fpuDisplayName: 'Fan 84', fpuCity: 'B intended city', fpuBio: 'Biography 84',
    } });
  });

  it('rejects a previous login save for the same Party and allows editing after a fresh baseline for the new login', async () => {
    const { result } = renderProfile();
    await waitFor(() => expect(result.current.auth.loading).toBe(false));
    act(() => { result.current.auth.login(account(42, 'synthetic-A')); });
    await waitFor(() => expect(result.current.editor.profileReady).toBe(true));
    const capturedSave = result.current.editor.saveProfile;
    act(() => { result.current.auth.login(account(42, 'synthetic-A-replacement')); });
    await act(async () => { capturedSave(); await Promise.resolve(); await Promise.resolve(); });
    await waitFor(() => expect(result.current.editor.updateProfileMutation.isPending).toBe(false));
    expect(profileWrites).toEqual([]);

    await waitFor(() => expect(profileReads).toContain('Bearer synthetic-A-replacement'));
    await waitFor(() => expect(result.current.editor.profileReady).toBe(true));
    act(() => { result.current.editor.setProfileDraft((draft) => ({ ...draft, fpuCity: 'New login intended city' })); });
    act(() => { result.current.editor.saveProfile(); });
    await waitFor(() => expect(profileWrites).toHaveLength(1));
    expect(profileWrites[0]).toMatchObject({ bearer: 'Bearer synthetic-A-replacement', body: {
      fpuDisplayName: 'Fan 42', fpuCity: 'New login intended city',
    } });
  });
});
