import { jest } from '@jest/globals';
import { act, type ReactNode } from 'react';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';

jest.unstable_mockModule('../api/session', () => ({
  loadSessionSnapshot: jest.fn(async () => null),
  logoutSessionRequest: jest.fn(async () => undefined),
  reconcileOnboardingProgress: jest.fn(async () => ({ newlyCompleted: false, progress: {
    eligible: false, signupCompletedAt: null, onboardingIntent: null, completedAt: null,
    firstValue: null, firstValueCompletedAt: null, updatedAt: null,
  } })),
}));
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => ({ ready: false }) }));

// Keep the real session provider, synchronous credentials, ownership guard,
// preference API and HTTP client. Only the network and bootstrap are synthetic.
const { SessionProvider, useSession, getStoredSessionToken } = await import('./SessionContext');
const { SessionQueryProvider } = await import('./SessionQueryProvider');
const { default: Controls } = await import('../components/events/EventMemoryInvitations');
let auth!: ReturnType<typeof useSession>;
function Probe() { auth = useSession(); return <Controls eventId="141" organizer={false} />; }
const originalFetch = globalThis.fetch;
const writes: { authorization: string | null; body: unknown }[] = [];
function response(value: unknown): Response {
  return { ok: true, status: 200, statusText: 'OK', headers: new Headers({ 'Content-Type': 'application/json' }),
    text: async () => JSON.stringify(value), json: async () => value } as Response;
}
function account(partyId: number, apiToken: string) {
  return { partyId, apiToken, username: `synthetic-${partyId}`, displayName: 'Synthetic participant', roles: ['Fan'] };
}
const Wrapper = ({ children }: { children: ReactNode }) => <SessionProvider><SessionQueryProvider><MemoryRouter>{children}</MemoryRouter></SessionQueryProvider></SessionProvider>;

beforeEach(() => {
  window.localStorage.clear(); window.sessionStorage.clear(); writes.length = 0;
  globalThis.fetch = jest.fn<typeof fetch>(async (url, init) => {
    if (typeof url !== 'string' || !url.endsWith('/social-events/me/memory-invitations')) throw new Error('Unexpected synthetic endpoint');
    if (init?.method === 'PUT') writes.push({ authorization: new Headers(init.headers).get('Authorization'), body: JSON.parse(String(init.body)) as unknown });
    return response({ inApp: init?.method === 'PUT', supportsExtendedMoments: true });
  });
});
afterEach(() => { cleanup(); globalThis.fetch = originalFetch; });

it('does not use replacement real-provider credentials to grant consent from the old checkbox', async () => {
  render(<Probe />, { wrapper: Wrapper });
  await act(async () => { auth.login(account(42, 'synthetic-A')); });
  const checkbox = await screen.findByRole('checkbox', { name: /Recibir invitaciones/ });
  expect(checkbox).toHaveProperty('checked', false);
  // One synchronous batch deliberately keeps the old DOM while the real
  // imperative credentials already point to B; no API/context mock hides it.
  await act(async () => {
    auth.login(account(84, 'synthetic-B'));
    expect(getStoredSessionToken()).toBe('synthetic-B');
    expect(checkbox.isConnected).toBe(true);
    fireEvent.click(checkbox);
    await Promise.resolve();
    await Promise.resolve();
  });
  await waitFor(() => expect(screen.getByRole('checkbox', { name: /Recibir invitaciones/ })).toHaveProperty('disabled', false));
  expect(writes).toEqual([]);
  expect(screen.getByRole('checkbox', { name: /Recibir invitaciones/ })).toHaveProperty('checked', false);
  fireEvent.click(screen.getByRole('checkbox', { name: /Recibir invitaciones/ }));
  await waitFor(() => expect(writes).toHaveLength(1));
  expect(writes[0]?.authorization).toBe('Bearer synthetic-B');
});
