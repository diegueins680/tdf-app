import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes, useLocation, useNavigate } from 'react-router-dom';

const getProfile = jest.fn(async () => ({
  sppPartyId: 248, sppDisplayName: 'Test member', sppAvatarUrl: 'https://example.test/portrait.jpg',
}));
jest.unstable_mockModule('../api/social', () => ({
  SocialAPI: {
    getProfile,
    listFriends: async () => [],
    listFollowers: async () => [],
    listFollowing: async () => [],
  },
}));
jest.unstable_mockModule('../api/radio', () => ({ RadioAPI: { getPresence: async () => null } }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: null }) }));
jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({ useLocalePreferences: () => ({ locale: 'es' }) }));
jest.unstable_mockModule('../components/events/EventRsvpFeed', () => ({ default: () => null }));
const { default: PublicProfilePage } = await import('./PublicProfilePage');
const clients: QueryClient[] = [];

function HistoryControls() {
  const navigate = useNavigate();
  const location = useLocation();
  return <>
    <button onClick={() => { void navigate(-1); }}>Back</button>
    <button onClick={() => { void navigate(1); }}>Forward</button>
    <output>{location.pathname}</output>
  </>;
}

function renderProfile() {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  clients.push(client);
  render(
    <QueryClientProvider client={client}>
      <MemoryRouter initialEntries={['/perfil/247', '/perfil/248']} initialIndex={1}>
        <HistoryControls />
        <Routes><Route path="/perfil/:partyId" element={<PublicProfilePage />} /></Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  );
}

afterEach(() => {
  cleanup();
  clients.splice(0).forEach((client) => client.clear());
});

it('opens the full profile photo and closes with the button or Escape', async () => {
  renderProfile();
  const trigger = await screen.findByRole('button', { name: 'Ampliar foto de Test member' });
  fireEvent.click(trigger);
  const dialog = await screen.findByRole('dialog', { name: 'Foto de Test member' });
  expect(within(dialog).getByRole('img', { name: 'Test member' }).getAttribute('src'))
    .toBe('https://example.test/portrait.jpg');
  fireEvent.click(within(dialog).getByRole('button', { name: 'Cerrar' }));
  await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
  fireEvent.click(trigger);
  fireEvent.keyDown(await screen.findByRole('dialog'), { key: 'Escape', code: 'Escape' });
  await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
});

it('keeps the initials non-interactive when there is no profile photo', async () => {
  getProfile.mockResolvedValueOnce({ sppPartyId: 248, sppDisplayName: 'Test member', sppAvatarUrl: '  ' });
  renderProfile();
  await screen.findByText('Test member');
  expect(screen.queryByRole('button', { name: 'Ampliar foto de Test member' })).toBeNull();
  expect(screen.queryByRole('dialog')).toBeNull();
});

it('does not reopen the photo after navigating back and forward between profiles', async () => {
  renderProfile();
  const back = screen.getByRole('button', { name: 'Back' });
  fireEvent.click(await screen.findByRole('button', { name: 'Ampliar foto de Test member' }));
  await screen.findByRole('dialog');
  fireEvent.click(back);
  await screen.findByText('/perfil/247');
  await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
  fireEvent.click(screen.getByRole('button', { name: 'Forward' }));
  await screen.findByText('/perfil/248');
  await screen.findByRole('button', { name: 'Ampliar foto de Test member' });
  expect(screen.queryByRole('dialog')).toBeNull();
});
