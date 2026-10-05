import React, { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { type QueryClient, useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { jest } from '@jest/globals';

jest.unstable_mockModule('../api/session', () => ({
  loadSessionSnapshot: () => new Promise(() => undefined),
  logoutSessionRequest: async () => undefined,
  reconcileOnboardingProgress: async () => ({ newlyCompleted: false, progress: { eligible: false } }),
}));
jest.unstable_mockModule('../analytics/posthog', () => ({
  getAnalyticsClient: () => ({ ready: false, capture() {}, identify() {}, reset() {} }),
}));
const { SessionProvider, useSession } = await import('./SessionContext');
const { SessionQueryProvider } = await import('./SessionQueryProvider');
let controls: ReturnType<typeof useSession>;
let resolveRead: ((value: string) => void) | undefined;
let reads: number;
let delayed: boolean;
let startMutation: (() => void) | undefined;
let resolveMutation: ((value: string) => void) | undefined;
let mutationCallbacks: number[];
const clients = new Map<number, QueryClient>();
const privateKey = ['internal-feedback', 'list', '', '', ''];

// Mirrors the actual actor-independent InternalFeedbackPage list key. It is
// deliberately a minimal projection, not a full page/browser/HTTP assertion.
function PrivateProjection({ actor }: { actor: number }) {
  const client = useQueryClient();
  clients.set(actor, client);
  const mutation = useMutation({
    mutationFn: () => new Promise<string>(resolve => { resolveMutation = resolve; }),
    onSuccess: value => {
      mutationCallbacks.push(actor);
      client.setQueryData(privateKey, value);
    },
  });
  startMutation = () => mutation.mutate();
  const result = useQuery({
    queryKey: privateKey,
    queryFn: () => {
      reads += 1;
      if (delayed && actor === 101) return new Promise<string>(resolve => { resolveRead = resolve; });
      return Promise.resolve(`SYNTHETIC_PRIVATE_REPORT_PARTY_${actor}`);
    },
  });
  return <div data-testid="projection">{actor}:{result.data ?? 'loading'}</div>;
}
function Harness() {
  controls = useSession();
  const id = controls.session?.partyId;
  return id == null ? <div>signed out</div> : <PrivateProjection key={id} actor={id} />;
}
let root: Root;
let container: HTMLDivElement;
const flush = async () => { await act(async () => { await new Promise(resolve => setTimeout(resolve, 20)); }); };
const login = async (partyId: number) => {
  await act(async () => controls.login({ partyId, username: `synthetic-${partyId}`, displayName: 'Synthetic', roles: ['Intern'] }));
};
beforeEach(async () => {
  (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  window.localStorage.clear(); window.sessionStorage.clear();
  reads = 0; delayed = false; resolveRead = undefined;
  startMutation = undefined; resolveMutation = undefined; mutationCallbacks = []; clients.clear();
  container = document.createElement('div'); document.body.appendChild(container);
  root = createRoot(container);
  await act(async () => root.render(<SessionProvider><SessionQueryProvider><Harness /></SessionQueryProvider></SessionProvider>));
});
afterEach(async () => { await act(async () => root.unmount()); new Set(clients.values()).forEach(client => client.clear()); container.remove(); });

test('separates fresh private cache at the actual logout/login boundary', async () => {
  await login(101); await flush();
  expect(container.textContent).toContain('101:SYNTHETIC_PRIVATE_REPORT_PARTY_101');
  await act(async () => controls.logout());
  expect(container.textContent).toBe('signed out');
  await login(202); await flush();
  expect(container.textContent).toBe('202:SYNTHETIC_PRIVATE_REPORT_PARTY_202');
  expect(clients.get(101)).not.toBe(clients.get(202));
  expect(reads).toBe(2); // B obtains its own authorized response
});

test('ignores an old actor query completion after logout/login', async () => {
  delayed = true;
  await login(101); await flush();
  expect(reads).toBe(1); expect(resolveRead).toBeDefined();
  await act(async () => controls.logout());
  await login(202);
  await act(async () => resolveRead?.('SYNTHETIC_PRIVATE_REPORT_PARTY_101'));
  await flush();
  expect(container.textContent).toBe('202:SYNTHETIC_PRIVATE_REPORT_PARTY_202');
  expect(clients.get(101)).not.toBe(clients.get(202));
  expect(reads).toBe(2);
});


test('isolates a late mutation callback that writes into its captured QueryClient', async () => {
  await login(101); await flush();
  const oldClient = clients.get(101)!;
  await act(async () => startMutation?.());
  await flush();
  expect(resolveMutation).toBeDefined();
  await act(async () => controls.logout());
  await login(202); await flush();
  const newClient = clients.get(202)!;
  expect(newClient).not.toBe(oldClient);
  await act(async () => resolveMutation?.('SYNTHETIC_LATE_PRIVATE_MUTATION_PARTY_101'));
  await flush();
  // Require the stale callback to have really run: unmounting alone must not
  // accidentally make this test pass without exercising the cache write.
  expect(mutationCallbacks).toEqual([101]);
  expect(oldClient.getQueryData(privateKey)).toBe('SYNTHETIC_LATE_PRIVATE_MUTATION_PARTY_101');
  expect(newClient.getQueryData(privateKey)).toBe('SYNTHETIC_PRIVATE_REPORT_PARTY_202');
  expect(container.textContent).toBe('202:SYNTHETIC_PRIVATE_REPORT_PARTY_202');
});


test('allocates a fresh cache when logout and same-actor login are batched', async () => {
  await login(101); await flush();
  const oldClient = clients.get(101);
  await act(async () => {
    controls.logout();
    controls.login({ partyId: 101, username: 'synthetic-101', displayName: 'Synthetic', roles: ['Intern'] });
  });
  await flush();
  expect(clients.get(101)).not.toBe(oldClient);
  expect(reads).toBe(2);
  expect(container.textContent).toBe('101:SYNTHETIC_PRIVATE_REPORT_PARTY_101');
});

test('does not restore a prior cache when an actor returns after another session', async () => {
  await login(101); await flush();
  const firstOccurrence = clients.get(101);
  await login(202); await flush();
  await login(101); await flush();
  expect(clients.get(101)).not.toBe(firstOccurrence);
  expect(reads).toBe(3);
});
