import { jest } from '@jest/globals';
import { Suspense, type ReactNode } from 'react';
import { cleanup, render, screen } from '@testing-library/react';
import { MemoryRouter, Outlet, useLocation, useParams } from 'react-router-dom';

let state: { loading: boolean; session: null | { partyId: number } } = { loading: false, session: null };
const shellRendered = jest.fn();
const publicRendered = jest.fn();
function LocationProbe() {
  const location = useLocation();
  return <output data-testid="location">{`${location.pathname}${location.search}${location.hash}`}</output>;
}
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => state }));
jest.unstable_mockModule('../components/PublicBranding', () => ({ default: ({ children }: { children: ReactNode }) => <>{children}</> }));
jest.unstable_mockModule('./AppShell', () => ({
  Shell: () => { shellRendered(); return <section aria-label="Authenticated shell"><Outlet /></section>; },
  ConfigurationIndexRedirect: () => null,
}));
jest.unstable_mockModule('../pages/DirectoryPublicDetailPage', () => ({ default: function PublicEventFixture({ authenticationFallback }: { authenticationFallback?: string }) {
  publicRendered();
  const { eventId } = useParams();
  return <><p>Public event {eventId}</p><output data-testid="fallback">{authenticationFallback}</output><LocationProbe /></>;
} }));
jest.unstable_mockModule('../pages/SocialEventWorkspacePage', () => ({ default: function WorkspaceFixture() {
  const { eventId } = useParams();
  return <><p>Protected workspace {eventId}</p><LocationProbe /></>;
} }));
jest.unstable_mockModule('../pages/CollaborativeEventCreatorPage', () => ({ default: () => <p>Protected creator</p> }));
jest.unstable_mockModule('../pages/LoginPage', () => ({ default: () => <><p>Login required</p><LocationProbe /></> }));
const { default: AppRoutes } = await import('./AppRoutes');

function open(path: string) {
  return render(<MemoryRouter initialEntries={[path]}><Suspense fallback={<p>Importing route</p>}><AppRoutes /></Suspense></MemoryRouter>);
}
beforeEach(() => { state = { loading: false, session: null }; shellRendered.mockClear(); publicRendered.mockClear(); });
afterEach(cleanup);

it('shows public event metadata at the original invitation URL before requesting authentication', async () => {
  const path = '/social/eventos/141?utm_source=invitation&memoryInvite=synthetic#momentos';
  open(path);
  expect(await screen.findByText('Public event 141')).toBeTruthy();
  expect(screen.getByTestId('location').textContent).toBe(path);
  const fallback = new URL(screen.getByTestId('fallback').textContent, 'https://tdf.test');
  expect(fallback.pathname).toBe('/login');
  expect(fallback.searchParams.get('redirect')).toBe(path);
  expect(shellRendered).not.toHaveBeenCalled();
  expect(screen.queryByText(/Protected workspace/)).toBeNull();
});

it('uses the original authenticated shell and workspace after session verification', async () => {
  state = { loading: false, session: { partyId: 7 } };
  open('/social/eventos/141?compose=1#momentos');
  expect(await screen.findByText('Protected workspace 141')).toBeTruthy();
  expect(screen.getByRole('region', { name: 'Authenticated shell' })).toBeTruthy();
  expect(publicRendered).not.toHaveBeenCalled();
});

it.each([null, { partyId: 7 }])('waits for session bootstrap rather than trusting a cached account: %p', async session => {
  state = { loading: true, session };
  open('/social/eventos/141');
  expect(await screen.findByRole('status')).toBeTruthy();
  expect(shellRendered).not.toHaveBeenCalled();
  expect(publicRendered).not.toHaveBeenCalled();
  expect(screen.queryByText('Login required')).toBeNull();
});

it.each(['compose=1&utm_campaign=memories', 'moment=77', 'tarea=8&tarea=9'])('preserves explicit protected intent through login: %s', async query => {
  const path = `/social/eventos/141?${query}#momentos`;
  open(path);
  expect(await screen.findByText('Login required')).toBeTruthy();
  const destination = new URL(screen.getByTestId('location').textContent, 'https://tdf.test');
  expect(destination.searchParams.get('redirect')).toBe(path);
  expect(publicRendered).not.toHaveBeenCalled();
  expect(shellRendered).not.toHaveBeenCalled();
});

it('does not turn compose=0 into an authentication requirement', async () => {
  open('/social/eventos/141?compose=0');
  expect(await screen.findByText('Public event 141')).toBeTruthy();
});

it('preserves the legacy English event URL context through the public entry', async () => {
  open('/social/events/141?utm_source=shared#momentos');
  expect(await screen.findByText('Public event 141')).toBeTruthy();
  expect(screen.getByTestId('location').textContent).toBe('/social/eventos/141?utm_source=shared#momentos');
});

it('keeps the specific event-creation route ahead of the public ID parameter', async () => {
  state = { loading: false, session: { partyId: 7 } };
  open('/social/eventos/nuevo');
  expect(await screen.findByText('Protected creator')).toBeTruthy();
  expect(publicRendered).not.toHaveBeenCalled();
});
