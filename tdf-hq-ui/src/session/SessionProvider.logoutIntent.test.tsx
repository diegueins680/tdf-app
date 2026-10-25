import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { fireEvent } from '@testing-library/react';

const snapshot = { username: 'fictional-fan', displayName: 'Fan', partyId: 42, roles: ['fan'] };
const loadSnapshot = jest.fn<() => Promise<typeof snapshot | null>>().mockResolvedValue(snapshot);
const logoutRequest = jest.fn<() => Promise<void>>();
jest.unstable_mockModule('../api/session', () => ({
  loadSessionSnapshot: loadSnapshot,
  logoutSessionRequest: logoutRequest,
  reconcileOnboardingProgress: jest.fn(async () => ({ newlyCompleted: false })),
}));
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => ({ ready: false }) }));
jest.unstable_mockModule('../utils/logger', () => ({ logger: { warn: jest.fn() } }));
const { SessionProvider, useSession, SESSION_STORAGE_KEY } = await import('./SessionContext');
const { getLogoutStatus, LOGOUT_INTENT_KEY } = await import('./logoutIntent');
(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

function Probe() {
  const { session, loading, login, logout } = useSession();
  return <><output>{loading ? 'loading' : session?.username ?? 'anonymous'}</output>
    <button onClick={logout}>logout</button><button onClick={() => login(snapshot)}>fresh login</button></>;
}
let root: ReturnType<typeof createRoot>;
let container: HTMLDivElement;
const mount = async () => { root = createRoot(container); await act(async () => root.render(<SessionProvider><Probe /></SessionProvider>)); };
beforeEach(async () => {
  localStorage.clear(); sessionStorage.clear(); loadSnapshot.mockClear();
  loadSnapshot.mockResolvedValue(snapshot);
  logoutRequest.mockReset().mockRejectedValue(new TypeError('offline'));
  container = document.createElement('div'); document.body.appendChild(container);
  await mount();
});
afterEach(async () => { await act(async () => root.unmount()); container.remove(); });

it('keeps a failed UI logout anonymous across remount despite a valid cookie and stale descriptor', async () => {
  expect(container.querySelector('output')?.textContent).toBe('fictional-fan');
  await act(async () => fireEvent.click(container.querySelector('button')!));
  expect(getLogoutStatus()).toBe('unconfirmed');
  expect(container.querySelector('output')?.textContent).toBe('anonymous');
  expect(JSON.parse(localStorage.getItem(LOGOUT_INTENT_KEY)!)).toEqual({ revision: expect.any(String), status: 'unconfirmed' });
  await act(async () => root.unmount());
  localStorage.setItem(SESSION_STORAGE_KEY, JSON.stringify(snapshot));
  loadSnapshot.mockClear(); await mount();
  expect(loadSnapshot).not.toHaveBeenCalled();
  expect(container.querySelector('output')?.textContent).toBe('anonymous');
  await act(async () => fireEvent.click(container.querySelectorAll('button')[1]!));
  expect(container.querySelector('output')?.textContent).toBe('fictional-fan');
  expect(getLogoutStatus()).toBe('cleared');
});

it('does not let late logout completion overwrite a new explicit login', async () => {
  let finish!: () => void;
  logoutRequest.mockImplementationOnce(() => new Promise<void>((resolve) => { finish = resolve; }));
  await act(async () => fireEvent.click(container.querySelector('button')!));
  expect(getLogoutStatus()).toBe('pending');
  await act(async () => fireEvent.click(container.querySelectorAll('button')[1]!));
  await act(async () => finish());
  expect(getLogoutStatus()).toBe('cleared');
  expect(container.querySelector('output')?.textContent).toBe('fictional-fan');
});

it('clears an existing session when another tab records a logout', async () => {
  localStorage.setItem(LOGOUT_INTENT_KEY, JSON.stringify({ revision: 'other-tab', status: 'unconfirmed' }));
  await act(async () => window.dispatchEvent(new StorageEvent('storage', { key: LOGOUT_INTENT_KEY })));
  expect(container.querySelector('output')?.textContent).toBe('anonymous');
});

it('cannot adopt an in-flight bootstrap snapshot after another tab logs out', async () => {
  await act(async () => root.unmount());
  let resolveSnapshot!: (value: typeof snapshot) => void;
  loadSnapshot.mockImplementationOnce(() => new Promise((resolve) => { resolveSnapshot = resolve; }));
  await mount();
  localStorage.setItem(LOGOUT_INTENT_KEY, JSON.stringify({ revision: 'tab-during-bootstrap', status: 'confirmed' }));
  await act(async () => window.dispatchEvent(new StorageEvent('storage', { key: LOGOUT_INTENT_KEY })));
  await act(async () => resolveSnapshot(snapshot));
  expect(container.querySelector('output')?.textContent).toBe('anonymous');
});
