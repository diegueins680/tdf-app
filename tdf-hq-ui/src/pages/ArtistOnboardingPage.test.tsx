import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter, Route, Routes, useLocation } from 'react-router-dom';
import type { SessionUser } from '../session/SessionContext';

let session: SessionUser | null;
const login = jest.fn();
const logout = jest.fn();
const activate = jest.fn<() => Promise<unknown>>();
const get = jest.fn<() => Promise<unknown>>();

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session, login, logout }),
  getActiveSession: () => session,
  SESSION_STORAGE_KEY: 'tdf-hq-ui/session',
}));
jest.unstable_mockModule('../api/fans', () => ({ Fans: { activateMyArtistProfile: activate } }));
jest.unstable_mockModule('../api/client', () => ({ get }));
const { default: ArtistOnboardingPage } = await import('./ArtistOnboardingPage');

const LoginDestination = () => <div data-testid="login-destination">{useLocation().search}</div>;
const show = (path = '/artista/crear') => render(
  <MemoryRouter initialEntries={[path]}>
    <Routes>
      <Route path="/artista/crear" element={<ArtistOnboardingPage />} />
      <Route path="/mi-artista" element={<div>Editor de artista</div>} />
      <Route path="/login" element={<LoginDestination />} />
    </Routes>
  </MemoryRouter>,
);

beforeEach(() => {
  jest.clearAllMocks();
  window.sessionStorage.clear();
  session = { username: 'artist', displayName: 'Artist', partyId: 42, roles: ['Customer'] };
  activate.mockResolvedValue({ apArtistId: 42 });
  get.mockResolvedValue({ ...session, roles: ['Customer', 'Artist'] });
});

it('activates a customer profile immediately, refreshes server roles and opens the editor', async () => {
  window.sessionStorage.setItem('tdf-hq-ui/session', '{}');
  show();
  expect(screen.queryByText('Solicitar acceso')).not.toBeInTheDocument();
  fireEvent.click(screen.getByRole('button', { name: 'Crear mi perfil de artista' }));
  await screen.findByText('Editor de artista');
  expect(activate).toHaveBeenCalledTimes(1);
  expect(get).toHaveBeenCalledWith('/session');
  expect(login).toHaveBeenCalledWith(expect.objectContaining({ partyId: 42, roles: ['Customer', 'Artist'] }), { remember: false });
});

it('keeps the user on the activation screen when activation fails', async () => {
  activate.mockRejectedValue(new Error('La activación no está disponible'));
  show();
  fireEvent.click(screen.getByRole('button', { name: 'Crear mi perfil de artista' }));
  await screen.findByText('La activación no está disponible');
  expect(login).not.toHaveBeenCalled();
  expect(get).not.toHaveBeenCalled();
});

it('does not overwrite a different account after an in-flight activation', async () => {
  get.mockImplementation(async () => {
    session = { username: 'other', displayName: 'Other', partyId: 99, roles: ['Customer'] };
    return { partyId: 42, roles: ['Artist'] };
  });
  show();
  fireEvent.click(screen.getByRole('button', { name: 'Crear mi perfil de artista' }));
  await waitFor(() => expect(get).toHaveBeenCalled());
  expect(login).not.toHaveBeenCalled();
  expect(screen.queryByText('Editor de artista')).not.toBeInTheDocument();
});

it('offers signup to guests without activating any profile', () => {
  session = null;
  show();
  expect(screen.getByRole('link', { name: 'Crear mi perfil de artista' })).toHaveAttribute('href', '/login?signup=1&intent=artist_profile');
  expect(activate).not.toHaveBeenCalled();
});

it.each(['Customer', 'Artist'])('preserves the selected claim for an authenticated %s without creating a separate profile', (role) => {
  session = { ...session!, roles: [role] };
  show('/artista/crear?claimArtistId=77');
  expect(screen.queryByRole('button', { name: /Crear mi perfil/ })).not.toBeInTheDocument();
  fireEvent.click(screen.getByRole('button', { name: 'Cerrar sesión y reclamar perfil' }));
  expect(logout).toHaveBeenCalledTimes(1);
  expect(screen.getByTestId('login-destination')).toHaveTextContent('?signup=1&intent=artist_profile&claimArtistId=77');
  expect(activate).not.toHaveBeenCalled();
  expect(get).not.toHaveBeenCalled();
});

it('preserves a guest claim through both signup and login', () => {
  session = null;
  show('/artista/crear?claim=77');
  expect(screen.getByRole('link', { name: 'Crear mi perfil de artista' })).toHaveAttribute('href', '/login?signup=1&intent=artist_profile&claimArtistId=77');
  expect(screen.getByRole('link', { name: 'Ya tengo cuenta' })).toHaveAttribute('href', '/login?redirect=%2Fartista%2Fcrear%3FclaimArtistId%3D77');
  expect(logout).not.toHaveBeenCalled();
  expect(activate).not.toHaveBeenCalled();
});
