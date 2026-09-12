import React from 'react';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { jest } from '@jest/globals';

import { writeSessionPersonalData } from '../utils/sessionPersonalData';
import { AUTH_SESSION_EXPIRED_EVENT } from './authEvents';

const logoutSessionRequestMock = jest.fn(() => Promise.resolve());
const loadSessionSnapshotMock = jest.fn(() => new Promise<null>(() => undefined));
const analyticsIdentifyMock = jest.fn();
const analyticsResetMock = jest.fn();

jest.unstable_mockModule('../api/session', () => ({
  completeOnboardingProgress: jest.fn(),
  loadSessionSnapshot: () => loadSessionSnapshotMock(),
  logoutSessionRequest: () => logoutSessionRequestMock(),
  reconcileOnboardingProgress: jest.fn(async () => ({
    newlyCompleted: false,
    progress: { eligible: false },
  })),
}));

jest.unstable_mockModule('../analytics/posthog', () => ({
  getAnalyticsClient: () => ({
    ready: true,
    capture: jest.fn(),
    identify: analyticsIdentifyMock,
    reset: analyticsResetMock,
    page: jest.fn(),
  }),
}));

const { SessionProvider, useSession } = await import('./SessionContext');

let sessionControls: ReturnType<typeof useSession> | null = null;

function SessionHarness() {
  sessionControls = useSession();
  return null;
}

describe('SessionProvider personal-data boundary', () => {
  let container: HTMLDivElement;
  let root: Root;

  beforeEach(async () => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
    window.localStorage.clear();
    window.sessionStorage.clear();
    jest.clearAllMocks();
    container = document.createElement('div');
    document.body.appendChild(container);
    root = createRoot(container);
    await act(async () => {
      root.render(
        <SessionProvider>
          <SessionHarness />
        </SessionProvider>,
      );
    });
  });

  afterEach(async () => {
    await act(async () => root.unmount());
    container.remove();
    sessionControls = null;
  });

  it('keeps an interrupted anonymous checkout, then clears it across identity boundaries', async () => {
    writeSessionPersonalData('tdf-marketplace-buyer', 'anonymous checkout');

    await act(async () => {
      sessionControls?.login({ username: 'first', displayName: 'First', roles: [], partyId: 42 });
    });
    expect(window.sessionStorage.getItem('tdf-marketplace-buyer')).toBe('anonymous checkout');

    writeSessionPersonalData('tdf-public-booking-profile', 'first account booking');
    await act(async () => {
      sessionControls?.login({ username: 'second', displayName: 'Second', roles: [], partyId: 84 });
    });
    expect(window.sessionStorage.getItem('tdf-marketplace-buyer')).toBeNull();
    expect(window.sessionStorage.getItem('tdf-public-booking-profile')).toBeNull();

    writeSessionPersonalData('tdf-marketplace-buyer', 'second account checkout');
    await act(async () => sessionControls?.logout());
    expect(window.sessionStorage.getItem('tdf-marketplace-buyer')).toBeNull();
    expect(logoutSessionRequestMock).toHaveBeenCalledTimes(1);
  });

  it('clears current-tab personal data when the session expires', async () => {
    await act(async () => {
      sessionControls?.login({ username: 'first', displayName: 'First', roles: [], partyId: 42 });
    });
    writeSessionPersonalData('tdf-public-booking-profile', 'expired account booking');

    await act(async () => {
      window.dispatchEvent(new Event(AUTH_SESSION_EXPIRED_EVENT));
    });

    expect(window.sessionStorage.getItem('tdf-public-booking-profile')).toBeNull();
    expect(sessionControls?.session).toBeNull();
  });
});
