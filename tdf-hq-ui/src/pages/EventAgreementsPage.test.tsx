import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { EventWorkspace, WorkspaceCommand } from '../api/eventWorkspace';
import { workspaceFixture } from '../api/eventWorkspace.fixture';
import { advanceAuthSessionEpoch } from '../session/authEvents';

let session: { partyId: number } | null = { partyId: 2 };
const read = jest.fn<(id: string) => Promise<EventWorkspace>>();
const command = jest.fn<(id: string, payload: WorkspaceCommand) => Promise<EventWorkspace>>();
const document = jest.fn<(id: string, path: string) => Promise<Blob>>();
jest.unstable_mockModule('../api/eventWorkspace', () => ({ EventWorkspaceAPI: { read, command, document } }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session }), getStoredSessionToken: () => null }));
jest.unstable_mockModule('../components/party-selector/PartySelector', () => ({ UserSelector: () => null }));
const { default: Page } = await import('./EventAgreementsPage');

function setup() {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  const app = () => <QueryClientProvider client={client}><MemoryRouter initialEntries={['/social/eventos/216/acuerdos']}><Routes><Route path="/social/eventos/:eventId/acuerdos" element={<Page />} /></Routes></MemoryRouter></QueryClientProvider>;
  const view = render(app()); return { ...view, redraw: () => view.rerender(app()) };
}
beforeEach(() => {
  session = { partyId: 2 };
  read.mockReset().mockResolvedValue(workspaceFixture());
  command.mockReset().mockResolvedValue(workspaceFixture()); document.mockReset();
  let count = 0;
  Object.defineProperty(crypto, 'randomUUID', { configurable: true, value: () => `00000000-0000-4000-8000-${String(++count).padStart(12, '0')}` });
});
afterEach(cleanup);

it('gives the artist plan reading and comments without editing or producer approval', async () => {
  setup(); await screen.findByText('Synthetic event plan');
  expect(screen.queryByRole('button', { name: 'Editar plan y cotización' })).toBeNull();
  expect(screen.getByRole('textbox', { name: 'Comentario sobre el plan' })).toBeTruthy();
  fireEvent.click(screen.getByRole('tab', { name: 'Acuerdos (1)' }));
  expect(screen.queryByRole('button', { name: 'Aprobar propuesta y presupuesto' })).toBeNull();
  expect(command).not.toHaveBeenCalled();
});
it('requires explicit consent and the legal name, then accepts the displayed version and hash', async () => {
  setup(); await screen.findByText('Synthetic event plan');
  fireEvent.click(screen.getByRole('tab', { name: 'Acuerdos (1)' }));
  fireEvent.click(screen.getByRole('button', { name: 'Revisar aceptación' }));
  const accept = screen.getByRole<HTMLButtonElement>('button', { name: 'Aceptar acuerdo', exact: true });
  expect(accept.disabled).toBe(true);
  fireEvent.change(screen.getByRole('textbox', { name: 'Tu nombre legal' }), { target: { value: 'Synthetic Recipient' } });
  expect(accept.disabled).toBe(true);
  fireEvent.click(screen.getByRole('checkbox', { name: 'He leído y acepto el acuerdo y sus anexos de esta versión.' }));
  fireEvent.click(accept);
  await waitFor(() => expect(command).toHaveBeenCalledTimes(1));
  expect(command).toHaveBeenCalledWith('216', expect.objectContaining({ action: 'agreement.accept', version: 1, hash: 'a'.repeat(64), agreementId: workspaceFixture().agreements[0]!.id, acceptedName: 'Synthetic Recipient', consent: true }));
  await screen.findByText('Acuerdo aceptado. Ya puedes descargar tu constancia.');
});
it('retains requested changes after a failed command and reuses its retry key', async () => {
  command.mockRejectedValueOnce(new Error('Conexión interrumpida'));
  setup(); await screen.findByText('Synthetic event plan');
  fireEvent.click(screen.getByRole('tab', { name: 'Acuerdos (1)' }));
  const observations = screen.getByRole('textbox', { name: 'Observaciones para el organizador' });
  fireEvent.change(observations, { target: { value: 'Adjust the delivery date' } });
  fireEvent.click(screen.getByRole('button', { name: 'Solicitar cambios' }));
  await screen.findByText('Conexión interrumpida');
  expect((observations as HTMLInputElement).value).toBe('Adjust the delivery date');
  const first = command.mock.calls[0]![1];
  fireEvent.click(screen.getByRole('button', { name: 'Solicitar cambios' }));
  await waitFor(() => expect(command).toHaveBeenCalledTimes(2));
  expect(command.mock.calls[1]![1].requestId).toBe(first.requestId);
});
it('blocks producer approval and sending until budget and scope are complete', async () => {
  session = { partyId: 1 }; const value = workspaceFixture(true); const agreement = value.agreements[0]!;
  agreement.state = 'draft'; agreement.versions[0]!.approved_at = null; agreement.versions[0]!.approved_by = null;
  agreement.versions[0]!.sent_at = null; agreement.versions[0]!.content.pendingFields = ['Delivery scope'];
  read.mockResolvedValue(value);
  setup(); await screen.findByText('Synthetic event plan');
  fireEvent.click(screen.getByRole('tab', { name: 'Acuerdos (1)' }));
  expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Aprobar propuesta y presupuesto' }).disabled).toBe(true);
  expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Enviar al artista' }).disabled).toBe(true);
  expect(screen.queryByRole('button', { name: 'Revisar aceptación' })).toBeNull();
});
it('preserves decimal input and sends exact cents when saving the plan', async () => {
  session = { partyId: 1 }; read.mockResolvedValue(workspaceFixture(true));
  setup(); await screen.findByText('Synthetic event plan');
  fireEvent.click(screen.getByRole('button', { name: 'Editar plan y cotización', exact: true }));
  const amount = screen.getByRole('textbox', { name: 'Importe USD (vacío si falta cotizar)' });
  fireEvent.change(amount, { target: { value: '200.' } });
  expect((amount as HTMLInputElement).value).toBe('200.');
  fireEvent.change(amount, { target: { value: '200.40' } });
  fireEvent.change(screen.getByRole('textbox', { name: 'Tope total recuperable propuesto USD' }), { target: { value: '200.40' } });
  fireEvent.click(screen.getByRole('button', { name: 'Guardar nueva versión del plan' }));
  await waitFor(() => expect(command).toHaveBeenCalledTimes(1));
  expect(command.mock.calls[0]![1].content).toEqual(expect.objectContaining({ budgetCapCents: 20040, budget: [expect.objectContaining({ amountCents: 20040 })] }));
});
it('does not download private bytes after the session changes', async () => {
  let resolve!: (value: Blob) => void;
  document.mockImplementationOnce(() => new Promise((done) => { resolve = done; }));
  const createUrl = jest.fn(() => 'blob:synthetic');
  Object.defineProperty(URL, 'createObjectURL', { configurable: true, value: createUrl });
  const view = setup(); await screen.findByText('Synthetic event plan');
  fireEvent.click(screen.getByRole('button', { name: 'Descargar plan PDF' }));
  await waitFor(() => expect(document).toHaveBeenCalledTimes(1));
  session = { partyId: 3 }; advanceAuthSessionEpoch(); read.mockRejectedValue(new Error('Acceso restringido'));
  view.redraw();
  await act(async () => { resolve(new Blob(['%PDF-synthetic'])); });
  expect(createUrl).not.toHaveBeenCalled();
});
