import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';

let session = { partyId: 42, apiToken: 'synthetic-original' };
const prepareArtistClaim = jest.fn<(id: number) => Promise<{ id: string; name: string }>>();
const claim = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('../api/directory', () => ({ Directory: { prepareArtistClaim, claim } }));
jest.unstable_mockModule('../session/SessionContext', () => ({ getActiveSession: () => session }));
const { default: ArtistClaimPanel } = await import('./ArtistClaimPanel');
const profile = { id: '00000000-0000-4000-8000-000000000077', name: 'Artista importado' };
const receipt = { id: '00000000-0000-4000-8000-000000000088', profileId: profile.id, claimType: 'administration', status: 'submitted', submittedAt: '2026-09-17T23:00:00Z' };
const description = 'Represento al artista; https://example.com/oficial confirma mi relación.';
const show = () => render(<MemoryRouter><ArtistClaimPanel artistId={77} accountPartyId={42} /></MemoryRouter>);
const fill = async () => {
  await screen.findByText(profile.name);
  fireEvent.change(screen.getByLabelText('Pruebas de titularidad o representación'), { target: { value: description } });
};
beforeEach(() => {
  jest.clearAllMocks();
  session = { partyId: 42, apiToken: 'synthetic-original' };
  prepareArtistClaim.mockResolvedValue(profile);
  claim.mockResolvedValue(receipt);
});

it('submits evidence for the resolved profile without claiming immediate access', async () => {
  show();
  await fill();
  fireEvent.click(screen.getByRole('button', { name: 'Solicitar administración del perfil' }));
  await screen.findByText(/El acceso requiere una aprobación verificada/);
  expect(prepareArtistClaim).toHaveBeenCalledWith(77);
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
  await screen.findByText(/El acceso requiere una aprobación verificada/);
  expect(claim.mock.calls[1]).toEqual(claim.mock.calls[0]);
});

it('does not invent a profile or submit a claim when claim preparation fails', async () => {
  prepareArtistClaim.mockRejectedValue(new Error('404'));
  show();
  await screen.findByText(/No pudimos preparar la solicitud/);
  expect(screen.queryByRole('button', { name: 'Solicitar administración del perfil' })).not.toBeInTheDocument();
  expect(claim).not.toHaveBeenCalled();
});

it('cannot submit on behalf of an account that replaced the current session', async () => {
  show();
  await fill();
  session = { partyId: 99, apiToken: 'synthetic-other' };
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
  session = { partyId: 99, apiToken: 'synthetic-other' };
  finish(receipt);
  await waitFor(() => expect(screen.queryByText('Enviando…')).not.toBeInTheDocument());
  expect(screen.queryByText(/Solicitud registrada para revisión/)).not.toBeInTheDocument();
});

it('ignores a stale claim acknowledgement after same-account reauthentication', async () => {
  let finish!: (value: unknown) => void;
  claim.mockImplementationOnce(() => new Promise(resolve => { finish = resolve; }));
  show();
  await fill();
  fireEvent.click(screen.getByRole('button', { name: 'Solicitar administración del perfil' }));
  await waitFor(() => expect(claim).toHaveBeenCalledTimes(1));
  session = { partyId: 42, apiToken: 'synthetic-replaced' };
  finish(receipt);
  await waitFor(() => expect(screen.queryByText('Enviando…')).not.toBeInTheDocument());
  expect(screen.queryByText(/Solicitud registrada para revisión/)).not.toBeInTheDocument();
});

it('keeps evidence and offers retry when an HTTP success lacks a persisted claim receipt', async () => {
  claim.mockResolvedValueOnce({});
  show();
  await fill();
  fireEvent.click(screen.getByRole('button', { name: 'Solicitar administración del perfil' }));
  await screen.findByText(/Puedes reintentar sin cerrar tu sesión/);
  expect(screen.getByDisplayValue(description)).toBeInTheDocument();
  expect(screen.queryByText(/Solicitud registrada para revisión/)).not.toBeInTheDocument();
});


it('retries preparation without discarding the session or creating a claim', async () => {
  prepareArtistClaim.mockRejectedValueOnce(new Error('temporarily unavailable'));
  show();
  fireEvent.click(await screen.findByRole('button', { name: 'Reintentar' }));
  await screen.findByText(profile.name);
  expect(prepareArtistClaim).toHaveBeenCalledTimes(2);
  expect(claim).not.toHaveBeenCalled();
});
