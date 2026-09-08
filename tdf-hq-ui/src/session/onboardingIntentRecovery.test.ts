import { jest } from '@jest/globals';

import {
  PENDING_ONBOARDING_INTENT_PREFIX,
  persistOnboardingIntentWithRetry,
  retryPendingOnboardingIntent,
} from './onboardingIntentRecovery';

const pendingKey = (partyId: number): string => `${PENDING_ONBOARDING_INTENT_PREFIX}${partyId}`;

describe('web onboarding intent recovery', () => {
  beforeEach(() => {
    window.localStorage.clear();
  });

  it('persists with the transient login token and clears after server acknowledgement', async () => {
    const persist = jest.fn(async () => undefined);

    await expect(persistOnboardingIntentWithRetry(
      42,
      'follow_artists',
      'transient-login-token',
      persist,
      () => true,
    )).resolves.toBe(true);

    expect(persist).toHaveBeenCalledWith('follow_artists', 'transient-login-token');
    expect(window.localStorage.length).toBe(0);
  });

  it('retains a failed synchronization and replays it without storing credentials', async () => {
    const offlinePersist = jest.fn().mockRejectedValue(new Error('offline'));

    await expect(persistOnboardingIntentWithRetry(
      42,
      'learning',
      'one-use-token',
      offlinePersist,
      () => true,
    )).resolves.toBe(false);
    expect(window.localStorage.length).toBe(1);
    expect(window.localStorage.key(0)).toBe(pendingKey(42));
    expect(window.localStorage.getItem(pendingKey(42))).toBe('learning');

    const retryPersist = jest.fn(async () => undefined);
    await expect(retryPendingOnboardingIntent(
      42,
      retryPersist,
      () => true,
    )).resolves.toBe(true);

    expect(retryPersist).toHaveBeenCalledWith('learning');
    expect(window.localStorage.length).toBe(0);
  });

  it('retains the initiating Party intent when the response arrives after an account switch', async () => {
    let resolvePersist: (() => void) | undefined;
    const persist = jest.fn(() => new Promise<void>((resolve) => {
      resolvePersist = resolve;
    }));
    let ownsParty = true;

    const pending = persistOnboardingIntentWithRetry(
      42,
      'internships',
      undefined,
      persist,
      () => ownsParty,
    );
    ownsParty = false;
    resolvePersist?.();

    await expect(pending).resolves.toBe(false);
    expect(window.localStorage.getItem(pendingKey(42))).toBe('internships');
  });

  it('does not clear a newer pending choice when an older request finishes', async () => {
    let resolvePersist: (() => void) | undefined;
    const persist = jest.fn(() => new Promise<void>((resolve) => {
      resolvePersist = resolve;
    }));

    const pending = persistOnboardingIntentWithRetry(
      42,
      'events',
      undefined,
      persist,
      () => true,
    );
    window.localStorage.setItem(pendingKey(42), 'artist_profile');
    resolvePersist?.();

    await expect(pending).resolves.toBe(true);
    expect(window.localStorage.getItem(pendingKey(42))).toBe('artist_profile');
  });

  it('isolates Party queues and removes invalid values without an API request', async () => {
    const persist = jest.fn();
    window.localStorage.setItem(pendingKey(42), 'admin');
    window.localStorage.setItem(pendingKey(43), 'professional_tools');

    await expect(retryPendingOnboardingIntent(42, persist, () => true)).resolves.toBe(false);

    expect(window.localStorage.getItem(pendingKey(42))).toBeNull();
    expect(window.localStorage.getItem(pendingKey(43))).toBe('professional_tools');
    expect(persist).not.toHaveBeenCalled();
  });

  it('rejects missing or malformed Party identities without writing or calling the API', async () => {
    const persist = jest.fn();

    await expect(persistOnboardingIntentWithRetry(
      null,
      'events',
      undefined,
      persist,
      () => true,
    )).resolves.toBe(false);
    await expect(persistOnboardingIntentWithRetry(
      'not-a-party',
      'events',
      undefined,
      persist,
      () => true,
    )).resolves.toBe(false);

    expect(window.localStorage.length).toBe(0);
    expect(persist).not.toHaveBeenCalled();
  });
});
