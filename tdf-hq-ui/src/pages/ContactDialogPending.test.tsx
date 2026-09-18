import { jest } from '@jest/globals';
import { useState } from 'react';
import { act, fireEvent, render, screen, waitFor, cleanup } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { PartyCreate, PartyDTO } from '../api/types';

const create = jest.fn<(body: PartyCreate, key: string) => Promise<PartyDTO>>();
jest.unstable_mockModule('../api/parties', () => ({ Parties: { list: async () => [], create, update: jest.fn() } }));
jest.unstable_mockModule('../components/PartyRelatedPopover', () => ({ default: () => null }));
const { default: CompaniesPage } = await import('./CompaniesPage');
const { default: LeadsPage } = await import('./LeadsPage');
const { CreatePartyDialog } = await import('./PartiesPage');
function ContactDialogHarness() {
  const [open, setOpen] = useState(false);
  return <><button onClick={() => setOpen(true)}>Nuevo contacto</button><CreatePartyDialog open={open} onClose={() => setOpen(false)} /></>;
}

afterEach(() => { cleanup(); create.mockReset(); });

it.each([
  ['company', CompaniesPage, 'Nueva empresa', 'Nombre comercial'],
  ['lead', LeadsPage, 'Nuevo lead', 'Nombre'],
  ['contact', ContactDialogHarness, 'Nuevo contacto', 'Nombre / Display'],
] as const)('keeps a pending %s dialog and its retry identity across failed responses', async (_, Page, openLabel, field) => {
  let rejectRequest!: (error: Error) => void;
  create.mockImplementationOnce(() => new Promise((_, reject) => { rejectRequest = reject; }));
  const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  render(<QueryClientProvider client={client}><Page /></QueryClientProvider>);
  fireEvent.click(await screen.findByRole('button', { name: openLabel }));
  fireEvent.change(screen.getByLabelText(new RegExp(`^${field}`)), { target: { value: 'Synthetic Contact' } });
  fireEvent.click(screen.getByRole('button', { name: 'Crear' }));
  await waitFor(() => expect(create).toHaveBeenCalledTimes(1));
  const cancel = screen.getByRole<HTMLButtonElement>('button', { name: 'Cancelar' });
  expect(cancel.disabled).toBe(true);
  fireEvent.click(cancel);
  fireEvent.keyDown(screen.getByRole('dialog'), { key: 'Escape', code: 'Escape' });
  const backdrop = document.querySelector('.MuiBackdrop-root');
  if (backdrop) fireEvent.click(backdrop);
  expect(screen.getByRole('dialog')).toBeTruthy();
  await act(async () => { rejectRequest(new Error('Response lost')); });
  await screen.findByText('Response lost');
  create.mockRejectedValueOnce(new Error('Still unavailable'));
  fireEvent.click(screen.getByRole('button', { name: 'Crear' }));
  await waitFor(() => expect(create).toHaveBeenCalledTimes(2));
  expect(create.mock.calls[0]?.[1]).toBe(create.mock.calls[1]?.[1]);
  client.clear();
});
