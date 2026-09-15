import { jest } from '@jest/globals';
import { fireEvent, waitFor } from '@testing-library/react';
import { act } from 'react';
import { createRoot } from 'react-dom/client';

const loadSessionSnapshotMock = jest.fn();
const logoutSessionRequestMock = jest.fn(async () => undefined);
const reconcileOnboardingProgressMock = jest.fn();
const analytics = {
  ready: true,
  capture: jest.fn(),
  identify: jest.fn(),
  reset: jest.fn(),
  page: jest.fn(),
};

jest.unstable_mockModule('../api/session', () => ({
  completeOnboardingProgress: jest.fn(),
  loadSessionSnapshot: (...args: unknown[]) => loadSessionSnapshotMock(...args),
  logoutSessionRequest: (...args: unknown[]) => logoutSessionRequestMock(...args),
  reconcileOnboardingProgress: (...args: unknown[]) => reconcileOnboardingProgressMock(...args),
}));

jest.unstable_mockModule('../analytics/posthog', () => ({
  getAnalyticsClient: () => analytics,
}));

jest.unstable_mockModule('../utils/logger', () => ({
  logger: { log: jest.fn(), warn: jest.fn(), error: jest.fn() },
}));

const { SessionProvider, useSession } = await import('./SessionContext');

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

const sessionSnapshot = (partyId: number) => ({
  username: `party-${partyId}`,
  displayName: `Party ${partyId}`,
  partyId,
  roles: ['customer'],
  modules: [],
  featureFlags: [],
  preferences: {
    locale: 'es',
    currency: 'USD',
    timeZone: 'America/Guayaquil',
  },
});

function Probe() {
  const { session, login, logout, setApiToken } = useSession();
  return (
    <>
      <button type="button" onClick={() => login({ ...sessionSnapshot(84), apiToken: 'party-84-token' })}>
        {session?.partyId ?? 'none'}
      </button>
      <button type="button" aria-label="Logout" onClick={logout} />
      <button type="button" aria-label="Rotate token" onClick={() => setApiToken('rotated-token')} />
    </>
  );
}

const completedReceipt = {
  newlyCompleted: true,
  progress: { eligible: false, completedAt: '2026-09-14T12:00:00Z', firstValue: 'event_saved' },
};
const unchangedReceipt = {
  newlyCompleted: false,
  progress: { eligible: true, completedAt: null, firstValue: null },
};

const renderProvider = async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);
  await act(async () => root.render(<SessionProvider><Probe /></SessionProvider>));
  return {
    container,
    cleanup: async () => {
      await act(async () => root.unmount());
      container.remove();
    },
  };
};

const reconnect = async () => {
  await act(async () => { window.dispatchEvent(new Event('online')); });
};

describe('SessionProvider onboarding reconciliation', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    window.localStorage.clear();
    window.sessionStorage.clear();
    loadSessionSnapshotMock.mockReset();
    reconcileOnboardingProgressMock.mockReset();
  });

  it('reconciles after session bootstrap and captures only the server-returned value', async () => {
    loadSessionSnapshotMock.mockResolvedValueOnce(sessionSnapshot(42));
    reconcileOnboardingProgressMock.mockResolvedValueOnce({
      newlyCompleted: true,
      progress: {
        eligible: false,
        completedAt: '2026-09-09T12:00:00Z',
        firstValue: 'event_saved',
      },
    });
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);

    try {
      await act(async () => {
        root.render(<SessionProvider><Probe /></SessionProvider>);
      });
      await waitFor(() => expect(container.textContent).toBe('42'));
      await waitFor(() => expect(analytics.capture).toHaveBeenCalledTimes(2));

      expect(reconcileOnboardingProgressMock).toHaveBeenCalledTimes(1);
      expect(reconcileOnboardingProgressMock).toHaveBeenCalledWith(undefined);
      expect(analytics.identify).toHaveBeenCalledWith('42');
      expect(analytics.capture).toHaveBeenCalledWith(
        'first_value_completed',
        expect.objectContaining({ platform: 'web', value: 'event_saved' }),
      );
    } finally {
      await act(async () => root.unmount());
      container.remove();
    }
  });

  it('ignores a reconciliation response after the active Party changes', async () => {
    let resolvePartyA: ((value: unknown) => void) | undefined;
    loadSessionSnapshotMock.mockResolvedValueOnce(sessionSnapshot(42));
    reconcileOnboardingProgressMock
      .mockReturnValueOnce(new Promise((resolve) => {
        resolvePartyA = resolve;
      }))
      .mockResolvedValueOnce({
        newlyCompleted: false,
        progress: { eligible: true, completedAt: null, firstValue: null },
      });
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);

    try {
      await act(async () => {
        root.render(<SessionProvider><Probe /></SessionProvider>);
      });
      await waitFor(() => expect(container.textContent).toBe('42'));
      await waitFor(() => expect(reconcileOnboardingProgressMock).toHaveBeenCalledTimes(1));

      fireEvent.click(container.querySelector('button')!);
      await waitFor(() => expect(container.textContent).toBe('84'));
      await waitFor(() => expect(reconcileOnboardingProgressMock).toHaveBeenCalledTimes(2));
      expect(reconcileOnboardingProgressMock).toHaveBeenLastCalledWith('party-84-token');

      await act(async () => {
        resolvePartyA?.({
          newlyCompleted: true,
          progress: {
            eligible: false,
            completedAt: '2026-09-09T12:00:00Z',
            firstValue: 'artist_followed',
          },
        });
      });

      expect(analytics.capture).not.toHaveBeenCalled();
      expect(analytics.identify).toHaveBeenLastCalledWith('84');
    } finally {
      await act(async () => root.unmount());
      container.remove();
    }
  });

  it('retries server reconciliation after an offline failure without local completion markers', async () => {
    loadSessionSnapshotMock.mockResolvedValueOnce(sessionSnapshot(42));
    reconcileOnboardingProgressMock
      .mockRejectedValueOnce(new Error('offline'))
      .mockResolvedValueOnce(completedReceipt)
      .mockResolvedValueOnce(unchangedReceipt);
    const { cleanup } = await renderProvider();
    try {
      expect(analytics.capture).not.toHaveBeenCalled();
      await reconnect();
      expect(reconcileOnboardingProgressMock).toHaveBeenCalledTimes(2);
      expect(analytics.capture).toHaveBeenCalledTimes(2);
      await reconnect();
      expect(reconcileOnboardingProgressMock).toHaveBeenCalledTimes(3);
      expect(analytics.capture).toHaveBeenCalledTimes(2);
    } finally {
      await cleanup();
    }
    await reconnect();
    expect(reconcileOnboardingProgressMock).toHaveBeenCalledTimes(3);
  });

  it('coalesces reconnects during a pending request and permits retries after settlement', async () => {
    let resolvePending: ((value: unknown) => void) | undefined;
    loadSessionSnapshotMock.mockResolvedValueOnce(sessionSnapshot(42));
    reconcileOnboardingProgressMock
      .mockReturnValueOnce(new Promise((resolve) => { resolvePending = resolve; }))
      .mockResolvedValueOnce(unchangedReceipt);
    const { cleanup } = await renderProvider();
    try {
      await reconnect();
      await reconnect();
      expect(reconcileOnboardingProgressMock).toHaveBeenCalledTimes(1);
      await act(async () => { resolvePending?.(completedReceipt); });
      expect(analytics.capture).toHaveBeenCalledTimes(2);
      await reconnect();
      expect(reconcileOnboardingProgressMock).toHaveBeenCalledTimes(2);
      expect(analytics.capture).toHaveBeenCalledTimes(2);
    } finally {
      await cleanup();
    }
  });

  it.each(['Logout', 'Rotate token'])('rejects a late receipt after %s', async (action) => {
    let resolveOld: ((value: unknown) => void) | undefined;
    loadSessionSnapshotMock.mockResolvedValueOnce(sessionSnapshot(42));
    reconcileOnboardingProgressMock
      .mockReturnValueOnce(new Promise((resolve) => { resolveOld = resolve; }))
      .mockResolvedValue(unchangedReceipt);
    const { container, cleanup } = await renderProvider();
    try {
      await act(async () => { fireEvent.click(container.querySelector(`[aria-label="${action}"]`)!); });
      await act(async () => { resolveOld?.(completedReceipt); });
      expect(analytics.capture).not.toHaveBeenCalled();
      await reconnect();
      if (action === 'Logout') {
        expect(container.textContent).toBe('none');
        expect(reconcileOnboardingProgressMock).toHaveBeenCalledTimes(1);
      } else {
        expect(container.textContent).toBe('42');
        expect(reconcileOnboardingProgressMock).toHaveBeenCalledTimes(3);
        expect(reconcileOnboardingProgressMock).toHaveBeenLastCalledWith('rotated-token');
      }
    } finally {
      await cleanup();
    }
  });

  it('suppresses a pending receipt and reconnect dispatch after unmount', async () => {
    let resolvePending: ((value: unknown) => void) | undefined;
    loadSessionSnapshotMock.mockResolvedValueOnce(sessionSnapshot(42));
    reconcileOnboardingProgressMock.mockReturnValueOnce(new Promise((resolve) => { resolvePending = resolve; }));
    const { cleanup } = await renderProvider();
    await cleanup();
    await act(async () => { resolvePending?.(completedReceipt); });
    await reconnect();
    expect(analytics.capture).not.toHaveBeenCalled();
    expect(reconcileOnboardingProgressMock).toHaveBeenCalledTimes(1);
  });

  it('does not reconcile an unauthenticated session on reconnect', async () => {
    loadSessionSnapshotMock.mockResolvedValueOnce(null);
    const { cleanup } = await renderProvider();
    try {
      await reconnect();
      expect(reconcileOnboardingProgressMock).not.toHaveBeenCalled();
      expect(analytics.capture).not.toHaveBeenCalled();
    } finally {
      await cleanup();
    }
  });
});
