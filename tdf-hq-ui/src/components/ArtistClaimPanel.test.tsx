import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';

let partyId = 42;
const profileByParty = jest.fn<(id: number) => Promise<{ id: string; name: string }>>();
const claim = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('../api/directory', () => ({ Directory: { profileByParty, claim } }));
jest.unstable_mockModule('../session/SessionContext', () => ({ getActiveSession: () => ({ partyId }) }));
const { default: ArtistClaimPanel } = await import('./ArtistClaimPanel');
const profile = { id: '00000000-0000-4000-8000-000000000077', name: 'Artista importado' };
const description = 'Represento al artista; https://example.com/oficial confirma mi relación.';
const show = () => render(<MemoryRouter><ArtistClaimPanel artistId={77} accountPartyId={42} /></MemoryRouter>);
const fill = async () => {
  await screen.findByText(profile.name);
  fireEvent.change(screen.getByLabelText('Pruebas de titularidad o representación'), { target: { value: description } });
};
beforeEach(() => {
  jest.clearAllMocks();
  partyId = 42;
  profileByParty.mockResolvedValue(profile);
  claim.mockResolvedValue({ status: 'submitted' });
});

it('submits evidence for the resolved profile without claiming immediate access', async () => {
  show();
  await fill();
  fireEvent.click(screen.getByRole('button', { name: 'Solicitar administración del perfil' }));
  await screen.findByText(/Todavía no se ha concedido acceso/);
  expect(profileByParty).toHaveBeenCalledWith(77);
  expect(claim).toHaveBeenCalledWith({ profileId: profile.id, claimType: 'administration', evidence: [{ description }] }, expect.any(String));
  expect(screen.getByRole('link', { name: 'Mis perfiles y clasificados' })).toHaveAttribute('href', '/mis-clasificados');
});

it('requires evidence and keeps the same idempotency key after an uncertain response', async () => {
  claim.mockRejectedValueOnce(new Error('connection lost'));
  show();
  await screen.findByText(profile.name);
  expect(screen.getByRole('button', { name: 'Solicitar administración del perfil' })).toBeDisabled();
  await fill();
  fireEvent.click(screen.getByRole('button', { name: 'Solicitar administración del perfil' }));
  await screen.findByText(/Puedes reintentar sin cerrar tu sesión/);
  fireEvent.click(screen.getByRole('button', { name: 'Solicitar administración del perfil' }));
  await screen.findByText(/Todavía no se ha concedido acceso/);
  expect(claim.mock.calls[1]).toEqual(claim.mock.calls[0]);
});

it('does not invent a profile or submit a claim when public resolution fails', async () => {
  profileByParty.mockRejectedValue(new Error('404'));
  show();
  await screen.findByText(/No encontramos un perfil público/);
  expect(screen.queryByRole('button', { name: 'Solicitar administración del perfil' })).not.toBeInTheDocument();
  expect(claim).not.toHaveBeenCalled();
});

it('cannot submit on behalf of an account that replaced the current session', async () => {
  show();
  await fill();
  partyId = 99;
  fireEvent.click(screen.getByRole('button', { name: 'Solicitar administración del perfil' }));
  expect(claim).not.toHaveBeenCalled();
});

it('does not expose a previous account request result after a session change', async () => {
  let finish!: (value: unknown) => void;
  claim.mockImplementationOnce(() => new Promise((resolve) => { finish = resolve; }));
  show();
  await fill();
  fireEvent.click(screen.getByRole('button', { name: 'Solicitar administración del perfil' }));
  await waitFor(() => expect(claim).toHaveBeenCalledTimes(1));
  partyId = 99;
  finish({ status: 'submitted' });
  await waitFor(() => expect(screen.queryByText('Enviando…')).not.toBeInTheDocument());
  expect(screen.queryByText(/Solicitud enviada para revisión/)).not.toBeInTheDocument();
});
