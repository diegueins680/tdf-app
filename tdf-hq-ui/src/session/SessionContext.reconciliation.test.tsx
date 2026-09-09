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
  const { session, login } = useSession();
  return (
    <button type="button" onClick={() => login({ ...sessionSnapshot(84), apiToken: 'party-84-token' })}>
      {session?.partyId ?? 'none'}
    </button>
  );
}

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
});
