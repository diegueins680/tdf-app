import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { EventWorkspace, WorkspaceCommand } from '../api/eventWorkspace';
import { workspaceFixture } from '../api/eventWorkspace.fixture';
import { advanceAuthSessionEpoch, getAuthSessionEpoch } from '../session/authEvents';

let session: { partyId: number } | null = { partyId: 2 };
const read = jest.fn<(id: string) => Promise<EventWorkspace>>();
const command = jest.fn<(id: string, payload: WorkspaceCommand) => Promise<EventWorkspace>>();
let agreementRollout: unknown = 'true';
jest.unstable_mockModule('../config/eventAgreementsRollout', () => ({ isEventAgreementsEnabled: () => agreementRollout === 'true' }));
const document = jest.fn<(id: string, path: string) => Promise<Blob>>();
jest.unstable_mockModule('../api/eventWorkspace', () => ({ EventWorkspaceAPI: { read, command, document } }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session }), getStoredSessionToken: () => null }));
jest.unstable_mockModule('../components/party-selector/PartySelector', () => ({ UserSelector: () => null }));
const { ApiError } = await import('../api/client');
const { default: Page } = await import('./EventAgreementsPage');

function setup(cached = false) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  if (cached) client.setQueryData(['event-workspace', '216', session?.partyId ?? null, getAuthSessionEpoch()], workspaceFixture());
  const app = () => <QueryClientProvider client={client}><MemoryRouter initialEntries={['/social/eventos/216/acuerdos']}><Routes><Route path="/social/eventos/:eventId/acuerdos" element={<Page />} /></Routes></MemoryRouter></QueryClientProvider>;
  const view = render(app()); return { ...view, client, redraw: () => view.rerender(app()) };
}
beforeEach(() => {
  agreementRollout = 'true';
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
it('keeps an unsaved proposal revision when a newer version is saved elsewhere and blocks saving until reconciled', async () => {
  session = { partyId: 1 }; const value = workspaceFixture(true); value.agreements[0]!.state = 'changes_requested';
  read.mockResolvedValue(value);
  const { client } = setup(); await screen.findByText('Synthetic event plan');
  fireEvent.click(screen.getByRole('tab', { name: 'Acuerdos (1)' }));
  fireEvent.click(screen.getByRole('button', { name: 'Preparar nueva versión' }));
  const body = () => screen.getByRole<HTMLTextAreaElement>('textbox', { name: 'Texto íntegro del acuerdo' });
  fireEvent.change(body(), { target: { value: 'Unsaved contractual text' } });
  const save = () => screen.getByRole<HTMLButtonElement>('button', { name: 'Guardar borrador de propuesta' });
  expect(save().disabled).toBe(false);

  const next = workspaceFixture(true); const moved = next.agreements[0]!; moved.state = 'changes_requested';
  const first = moved.versions[0]!;
  moved.versions.push({ ...first, version: 2, document_hash: 'c'.repeat(64), content: { ...first.content, body: 'Server text saved in another tab' } });
  moved.current_version = 2;
  read.mockResolvedValue(next);
  await act(async () => { client.setQueryData(['event-workspace', '216', 1, getAuthSessionEpoch()], next); });

  expect(body().value).toBe('Unsaved contractual text');
  await screen.findByText(/Se guardó la versión 2 mientras editabas la versión 1/);
  expect(save().disabled).toBe(true);
  fireEvent.click(screen.getByRole('button', { name: 'Conservar mis cambios' }));
  expect(body().value).toBe('Unsaved contractual text');
  expect(save().disabled).toBe(false);
  fireEvent.click(save());
  await waitFor(() => expect(command).toHaveBeenCalledTimes(1));
  expect(command).toHaveBeenCalledWith('216', expect.objectContaining({ action: 'agreement.revise', version: 2, hash: 'c'.repeat(64), content: expect.objectContaining({ body: 'Unsaved contractual text' }) }));
});
it('lets the organizer discard an unsaved proposal revision in favour of the newer saved version', async () => {
  session = { partyId: 1 }; const value = workspaceFixture(true); value.agreements[0]!.state = 'changes_requested';
  read.mockResolvedValue(value);
  const { client } = setup(); await screen.findByText('Synthetic event plan');
  fireEvent.click(screen.getByRole('tab', { name: 'Acuerdos (1)' }));
  fireEvent.click(screen.getByRole('button', { name: 'Preparar nueva versión' }));
  const body = () => screen.getByRole<HTMLTextAreaElement>('textbox', { name: 'Texto íntegro del acuerdo' });
  fireEvent.change(body(), { target: { value: 'Unsaved contractual text' } });
  const next = workspaceFixture(true); const moved = next.agreements[0]!; moved.state = 'changes_requested';
  const first = moved.versions[0]!;
  moved.versions.push({ ...first, version: 2, document_hash: 'c'.repeat(64), content: { ...first.content, body: 'Server text saved in another tab' } });
  moved.current_version = 2;
  read.mockResolvedValue(next);
  await act(async () => { client.setQueryData(['event-workspace', '216', 1, getAuthSessionEpoch()], next); });
  fireEvent.click(await screen.findByRole('button', { name: 'Descartar mis cambios' }));
  await waitFor(() => expect(body().value).toBe('Server text saved in another tab'));
  expect(screen.queryByText(/mientras editabas/)).toBeNull();
  expect(command).not.toHaveBeenCalled();
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

it.each([undefined, 'false'])('rollout: disabled direct workspace makes no private read or command (%j)', setting => {
  agreementRollout = setting;
  setup(true);
  expect(screen.queryByText('Synthetic event plan')).toBeNull();
  expect(read).not.toHaveBeenCalled();
  expect(command).not.toHaveBeenCalled();
  expect(document).not.toHaveBeenCalled();
  expect(screen.queryByRole('button', { name: 'Aceptar acuerdo', exact: true })).toBeNull();
});


it.each([2, 1])('requires fresh consent after the reviewed proposal changes to version %s or a new hash', async (replacementVersion) => {
  const { client } = setup();
  await screen.findByText('Synthetic event plan');
  fireEvent.click(screen.getByRole('tab', { name: 'Acuerdos (1)' }));
  fireEvent.click(screen.getByRole('button', { name: 'Revisar aceptación' }));
  fireEvent.change(screen.getByRole('textbox', { name: 'Tu nombre legal' }), { target: { value: 'Synthetic Recipient' } });
  fireEvent.click(screen.getByRole('checkbox', { name: 'He leído y acepto el acuerdo y sus anexos de esta versión.' }));
  expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Aceptar acuerdo', exact: true }).disabled).toBe(false);
  const replacement = workspaceFixture();
  const proposal = replacement.agreements[0]!;
  const previous = proposal.versions[0]!;
  proposal.current_version = replacementVersion;
  proposal.versions = [{ ...previous, version: replacementVersion, document_hash: 'b'.repeat(64), content: { ...previous.content, body: 'Replacement terms with the same recipient legal name.' } }];
  await act(async () => {
    client.setQueryData(['event-workspace', '216', session!.partyId, getAuthSessionEpoch()], replacement);
  });
  await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
  expect(command).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Revisar aceptación' }));
  expect(screen.getByRole<HTMLInputElement>('textbox', { name: 'Tu nombre legal' }).value).toBe('');
  expect(screen.getByRole<HTMLInputElement>('checkbox', { name: 'He leído y acepto el acuerdo y sus anexos de esta versión.' }).checked).toBe(false);
  expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Aceptar acuerdo', exact: true }).disabled).toBe(true);
  fireEvent.change(screen.getByRole('textbox', { name: 'Tu nombre legal' }), { target: { value: 'Synthetic Recipient' } });
  fireEvent.click(screen.getByRole('checkbox', { name: 'He leído y acepto el acuerdo y sus anexos de esta versión.' }));
  fireEvent.click(screen.getByRole('button', { name: 'Aceptar acuerdo', exact: true }));
  await waitFor(() => expect(command).toHaveBeenCalledTimes(1));
  expect(command).toHaveBeenCalledWith('216', expect.objectContaining({ action: 'agreement.accept', agreementId: proposal.id, version: replacementVersion, hash: 'b'.repeat(64), acceptedName: 'Synthetic Recipient', consent: true }));
});


it('retains consent for an identical proposal refresh but clears it after withdrawal and resend', async () => {
  const { client } = setup();
  await screen.findByText('Synthetic event plan');
  fireEvent.click(screen.getByRole('tab', { name: 'Acuerdos (1)' }));
  fireEvent.click(screen.getByRole('button', { name: 'Revisar aceptación' }));
  fireEvent.change(screen.getByRole('textbox', { name: 'Tu nombre legal' }), { target: { value: 'Synthetic Recipient' } });
  fireEvent.click(screen.getByRole('checkbox', { name: 'He leído y acepto el acuerdo y sus anexos de esta versión.' }));
  const key = ['event-workspace', '216', session!.partyId, getAuthSessionEpoch()];
  await act(async () => { client.setQueryData(key, workspaceFixture()); });
  expect(screen.getByRole<HTMLInputElement>('checkbox', { name: 'He leído y acepto el acuerdo y sus anexos de esta versión.' }).checked).toBe(true);
  expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Aceptar acuerdo', exact: true }).disabled).toBe(false);
  const withdrawn = workspaceFixture();
  withdrawn.agreements[0]!.state = 'withdrawn';
  await act(async () => { client.setQueryData(key, withdrawn); });
  await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
  await act(async () => { client.setQueryData(key, workspaceFixture()); });
  const reviewAgain = await screen.findByRole('button', { name: 'Revisar aceptación' });
  expect(screen.queryByRole('dialog')).toBeNull();
  expect(command).not.toHaveBeenCalled();
  fireEvent.click(reviewAgain);
  expect(screen.getByRole<HTMLInputElement>('textbox', { name: 'Tu nombre legal' }).value).toBe('');
  expect(screen.getByRole<HTMLInputElement>('checkbox', { name: 'He leído y acepto el acuerdo y sus anexos de esta versión.' }).checked).toBe(false);
});


it('keeps every plan, proposal and comment draft node through a temporary503 and successful retry', async () => {
  session = { partyId: 1 }; read.mockResolvedValue(workspaceFixture(true));
  const { client } = setup();
  await screen.findByText('Synthetic event plan');
  fireEvent.click(screen.getByRole('button', { name: 'Editar plan y cotización', exact: true }));
  let planFields = within(screen.getByRole('tabpanel', { name: 'Plan del evento' }));
  const planTitle = planFields.getByRole<HTMLInputElement>('textbox', { name: 'Título del plan' });
  const planComment = planFields.getByRole<HTMLTextAreaElement>('textbox', { name: 'Comentario sobre el plan' });
  fireEvent.change(planTitle, { target: { value: 'Unsaved plan title' } });
  fireEvent.change(planComment, { target: { value: 'Unsaved plan comment' } });
  fireEvent.click(screen.getByRole('tab', { name: 'Acuerdos (1)' }));
  fireEvent.click(screen.getByRole('button', { name: 'Preparar nuevo acuerdo', exact: true }));
  let agreementFields = within(screen.getByRole('tabpanel', { name: 'Acuerdos del evento' }));
  const proposalTitle = agreementFields.getByRole<HTMLInputElement>('textbox', { name: 'Título del acuerdo' });
  const proposalBody = agreementFields.getByRole<HTMLTextAreaElement>('textbox', { name: 'Texto íntegro del acuerdo' });
  const agreementComment = agreementFields.getByRole<HTMLTextAreaElement>('textbox', { name: 'Comentario sobre el acuerdo' });
  fireEvent.change(proposalTitle, { target: { value: 'Unsaved proposal title' } });
  fireEvent.change(proposalBody, { target: { value: 'Unsaved proposal terms' } });
  fireEvent.change(agreementFields.getByRole('textbox', { name: 'Nombre legal del destinatario' }), { target: { value: 'Synthetic Recipient' } });
  fireEvent.change(agreementComment, { target: { value: 'Unsaved agreement comment' } });
  const error = new ApiError('Temporary workspace read failure', 503);
  read.mockRejectedValueOnce(error);
  await act(async () => { await client.refetchQueries({ queryKey: ['event-workspace', '216', 1, getAuthSessionEpoch()] }); });
  await screen.findByText('Temporary workspace read failure');
  agreementFields = within(screen.getByRole('tabpanel', { name: 'Acuerdos del evento' }));
  expect(client.getQueryData(['event-workspace', '216', 1, getAuthSessionEpoch()])).toBeTruthy();
  expect(agreementFields.getByRole('textbox', { name: 'Título del acuerdo' })).toBe(proposalTitle);
  expect(proposalTitle.value).toBe('Unsaved proposal title');
  expect(agreementFields.getByRole('textbox', { name: 'Texto íntegro del acuerdo' })).toBe(proposalBody);
  expect(proposalBody.value).toBe('Unsaved proposal terms');
  expect(agreementFields.getByRole('textbox', { name: 'Comentario sobre el acuerdo' })).toBe(agreementComment);
  expect(agreementComment.value).toBe('Unsaved agreement comment');
  expect(agreementFields.getByRole<HTMLButtonElement>('button', { name: 'Guardar borrador de propuesta' }).disabled).toBe(true);
  fireEvent.click(screen.getByRole('tab', { name: 'Plan del evento', exact: true }));
  planFields = within(screen.getByRole('tabpanel', { name: 'Plan del evento' }));
  expect(planFields.getByRole('textbox', { name: 'Título del plan' })).toBe(planTitle);
  expect(planTitle.value).toBe('Unsaved plan title');
  expect(planFields.getByRole('textbox', { name: 'Comentario sobre el plan' })).toBe(planComment);
  expect(planComment.value).toBe('Unsaved plan comment');
  expect(planFields.getByRole<HTMLButtonElement>('button', { name: 'Guardar nueva versión del plan' }).disabled).toBe(true);
  expect(command).not.toHaveBeenCalled(); expect(document).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar', exact: true }));
  await waitFor(() => expect(screen.queryByText('Temporary workspace read failure')).toBeNull());
  planFields = within(screen.getByRole('tabpanel', { name: 'Plan del evento' }));
  expect(planFields.getByRole('textbox', { name: 'Título del plan' })).toBe(planTitle);
  expect(planTitle.value).toBe('Unsaved plan title');
  expect(planComment.value).toBe('Unsaved plan comment');
  fireEvent.click(screen.getByRole('tab', { name: 'Acuerdos (1)' }));
  agreementFields = within(screen.getByRole('tabpanel', { name: 'Acuerdos del evento' }));
  expect(agreementFields.getByRole('textbox', { name: 'Título del acuerdo' })).toBe(proposalTitle);
  expect(proposalTitle.value).toBe('Unsaved proposal title');
  expect(proposalBody.value).toBe('Unsaved proposal terms');
  expect(agreementComment.value).toBe('Unsaved agreement comment');
  expect(command).not.toHaveBeenCalled(); expect(document).not.toHaveBeenCalled();
});

it.each([408, 429, 'network'])('preserves the same comment node and blocks a queued write during transient read failure (%s)', async (failure) => {
  const { client } = setup(); await screen.findByText('Synthetic event plan');
  const comment = screen.getByRole<HTMLTextAreaElement>('textbox', { name: 'Comentario sobre el plan' });
  const send = screen.getByRole<HTMLButtonElement>('button', { name: 'Enviar comentario', exact: true });
  fireEvent.change(comment, { target: { value: 'Unsaved transient transport comment' } });
  const error = typeof failure === 'number' ? new ApiError('Temporary comment read failure', failure)
    : Object.assign(new Error('Temporary comment read failure'), { cause: new TypeError('Synthetic offline transport') });
  read.mockRejectedValueOnce(error);
  await act(async () => {
    await client.refetchQueries({ queryKey: ['event-workspace', '216', 2, getAuthSessionEpoch()] });
    fireEvent.click(send);
  });
  expect(command).not.toHaveBeenCalled();
  await screen.findByText('Temporary comment read failure');
  expect(client.getQueryData(['event-workspace', '216', 2, getAuthSessionEpoch()])).toBeTruthy();
  expect(screen.getByRole('textbox', { name: 'Comentario sobre el plan' })).toBe(comment);
  expect(comment.value).toBe('Unsaved transient transport comment');
  expect(send.disabled).toBe(true);
  expect(document).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar', exact: true }));
  await waitFor(() => expect(screen.queryByText('Temporary comment read failure')).toBeNull());
  expect(screen.getByRole('textbox', { name: 'Comentario sobre el plan' })).toBe(comment);
  expect(comment.value).toBe('Unsaved transient transport comment');
  expect(send.disabled).toBe(false);
  expect(command).not.toHaveBeenCalled(); expect(document).not.toHaveBeenCalled();
});

it.each([401, 403, 404, 'malformed'])('hides cached private documents and drafts after denial or an unconfirmed response (%s)', async (failure) => {
  session = { partyId: 1 }; read.mockResolvedValue(workspaceFixture(true));
  const { client } = setup(); await screen.findByText('Synthetic event plan');
  fireEvent.click(screen.getByRole('button', { name: 'Editar plan y cotización', exact: true }));
  fireEvent.change(screen.getByRole('textbox', { name: 'Título del plan' }), { target: { value: 'Private unsaved draft' } });
  read.mockRejectedValueOnce(typeof failure === 'number' ? new ApiError('Workspace access not confirmed', failure) : new Error('Workspace access not confirmed'));
  await act(async () => { await client.refetchQueries({ queryKey: ['event-workspace', '216', 1, getAuthSessionEpoch()] }); });
  await screen.findByText('Workspace access not confirmed');
  expect(client.getQueryData(['event-workspace', '216', 1, getAuthSessionEpoch()])).toBeTruthy();
  expect(screen.queryByText('Synthetic event plan')).toBeNull();
  expect(screen.queryByRole('textbox', { name: 'Título del plan' })).toBeNull();
  expect(screen.queryByDisplayValue('Private unsaved draft')).toBeNull();
  expect(screen.queryByRole('button', { name: 'Descargar plan PDF' })).toBeNull();
  expect(command).not.toHaveBeenCalled(); expect(document).not.toHaveBeenCalled();
});

it('refuses acceptance queued against a failed read and still invalidates consent for the recovered replacement', async () => {
  const { client } = setup(); await screen.findByText('Synthetic event plan');
  fireEvent.click(screen.getByRole('tab', { name: 'Acuerdos (1)' }));
  fireEvent.click(screen.getByRole('button', { name: 'Revisar aceptación' }));
  fireEvent.change(screen.getByRole('textbox', { name: 'Tu nombre legal' }), { target: { value: 'Synthetic Recipient' } });
  fireEvent.click(screen.getByRole('checkbox', { name: 'He leído y acepto el acuerdo y sus anexos de esta versión.' }));
  const accept = screen.getByRole<HTMLButtonElement>('button', { name: 'Aceptar acuerdo', exact: true });
  read.mockRejectedValueOnce(new ApiError('Temporary acceptance read failure', 503));
  await act(async () => {
    await client.refetchQueries({ queryKey: ['event-workspace', '216', 2, getAuthSessionEpoch()] });
    // Deliver the already-queued click before the observer paints its disabled state.
    fireEvent.click(accept);
  });
  expect(command).not.toHaveBeenCalled();
  await screen.findByText('Temporary acceptance read failure');
  // A failed read closes acceptance so its modal cannot trap the workspace retry.
  await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
  expect(command).not.toHaveBeenCalled();
  const replacement = workspaceFixture();
  replacement.agreements[0]!.current_version = 2;
  replacement.agreements[0]!.versions = [{ ...replacement.agreements[0]!.versions[0]!, version: 2, document_hash: 'b'.repeat(64) }];
  read.mockResolvedValueOnce(replacement);
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar', exact: true }));
  await waitFor(() => expect(screen.queryByText('Temporary acceptance read failure')).toBeNull());
  await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
  expect(command).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Revisar aceptación' }));
  expect(screen.getByRole<HTMLInputElement>('textbox', { name: 'Tu nombre legal' }).value).toBe('');
  expect(screen.getByRole<HTMLInputElement>('checkbox', { name: 'He leído y acepto el acuerdo y sus anexos de esta versión.' }).checked).toBe(false);
});

it('does not publish a private PDF that resolves after a workspace read failure', async () => {
  const previous = Object.getOwnPropertyDescriptor(URL, 'createObjectURL');
  const createUrl = jest.fn(() => { throw new Error('Synthetic private bytes must not be exposed'); });
  Object.defineProperty(URL, 'createObjectURL', { configurable: true, value: createUrl });
  let resolveDocument!: (value: Blob) => void;
  document.mockImplementationOnce(() => new Promise(resolve => { resolveDocument = resolve; }));
  try {
    const { client } = setup(); await screen.findByText('Synthetic event plan');
    fireEvent.click(screen.getByRole('button', { name: 'Descargar plan PDF' }));
    await waitFor(() => expect(document).toHaveBeenCalledTimes(1));
    read.mockRejectedValueOnce(new ApiError('Temporary document read failure', 503));
    await act(async () => { await client.refetchQueries({ queryKey: ['event-workspace', '216', 2, getAuthSessionEpoch()] }); });
    await screen.findByText('Temporary document read failure');
    await act(async () => { resolveDocument(new Blob(['%PDF-synthetic'])); });
    expect(createUrl).not.toHaveBeenCalled();
  } finally {
    if (previous) Object.defineProperty(URL, 'createObjectURL', previous);
    else Reflect.deleteProperty(URL, 'createObjectURL');
  }
});
