import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { jest } from '@jest/globals';

// A /session request that never settles (stalled mobile network). It honours
// the abort signal like fetch does.
const loadSessionSnapshotMock = jest.fn((options: { signal?: AbortSignal } = {}) => new Promise<null>((_, reject) => {
  options.signal?.addEventListener('abort', () => reject(new DOMException('aborted', 'AbortError')));
}));
const reportMock = jest.fn();

jest.unstable_mockModule('../api/session', () => ({
  completeOnboardingProgress: jest.fn(),
  loadSessionSnapshot: (options?: { signal?: AbortSignal }) => loadSessionSnapshotMock(options),
  logoutSessionRequest: jest.fn(async () => undefined),
  reconcileOnboardingProgress: jest.fn(async () => ({ newlyCompleted: false, progress: { eligible: false } })),
}));
jest.unstable_mockModule('../analytics/errorReporting', () => ({ reportClientError: reportMock }));
jest.unstable_mockModule('../analytics/posthog', () => ({
  getAnalyticsClient: () => ({ ready: true, capture: jest.fn(), identify: jest.fn(), reset: jest.fn(), page: jest.fn() }),
}));

const { SessionProvider, useSession, SESSION_BOOTSTRAP_TIMEOUT_MS } = await import('./SessionContext');

let loadingState: boolean | null = null;
function Probe() {
  loadingState = useSession().loading;
  return null;
}

it('stops loading and reports when session bootstrap stalls, instead of an endless spinner', async () => {
  (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  jest.useFakeTimers();
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);
  try {
    await act(async () => {
      root.render(<SessionProvider><Probe /></SessionProvider>);
    });
    expect(loadingState).toBe(true);
    await act(async () => {
      await jest.advanceTimersByTimeAsync(SESSION_BOOTSTRAP_TIMEOUT_MS - 1);
    });
    expect(loadingState).toBe(true);
    await act(async () => {
      await jest.advanceTimersByTimeAsync(2);
    });
    expect(loadingState).toBe(false);
    expect(reportMock).toHaveBeenCalledWith('session_bootstrap', expect.anything(), expect.objectContaining({ timed_out: true }));
  } finally {
    await act(async () => root.unmount());
    container.remove();
    jest.useRealTimers();
  }
});
