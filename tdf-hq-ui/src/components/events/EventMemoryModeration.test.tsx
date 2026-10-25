import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const commandMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
let session: { partyId: number } | null = { partyId: 9 };
jest.unstable_mockModule('../../api/client', () => ({ get: getMock, ApiError: class extends Error { status = 503; } }));
jest.unstable_mockModule('../../api/interactions', () => ({ Interactions: { command: commandMock } }));
jest.unstable_mockModule('../../session/SessionContext', () => ({ useSession: () => ({ session }) }));
const { EventMemorySourceActions, EventMemoryModerationPanel } = await import('./EventMemoryModeration');
const controls = { supported: true, eventId: '141', state: 'visible' as const, sourceRevision: '1790000000.123456', canReport: true, canHide: false, canRestore: false, canRemove: false };
const target = '71000000-0000-4000-8000-000000000001';
const report = () => ({ id: target, entityKey: '81', eventId: '141', version: 2,
  source: { ...controls, state: 'hidden' as const, canHide: false, canRestore: true, canReport: false },
  caption: 'Synthetic hidden source', mediaType: 'text', mediaUrl: '', author: null,
  openReports: 0, reportsReferToEarlierContent: false, reasons: [], audit: [] });
const page = () => ({ supported: true, canReview: true, items: [report()], nextCursor: null });
const mount = (child: React.ReactNode) => {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  const wrapper = ({ children }: { children: React.ReactNode }) => <QueryClientProvider client={client}>{children}</QueryClientProvider>;
  return { ...render(child, { wrapper }), client };
};
beforeEach(() => { jest.clearAllMocks(); session = { partyId: 9 }; getMock.mockResolvedValue(page()); commandMock.mockResolvedValue({ reported: true }); });
afterEach(cleanup);
it('does not show enforcement buttons from report permission or permit anonymous reporting', () => {
  const view = mount(<EventMemorySourceActions targetId={target} version={1} controls={controls} />);
  expect(screen.queryByRole('button', { name: 'Ocultar recuerdo' })).toBeNull();
  expect(screen.queryByRole('button', { name: 'Retirar recuerdo' })).toBeNull();
  session = null; view.rerender(<EventMemorySourceActions targetId={target} version={1} controls={controls} />);
  expect(screen.queryByRole('button', { name: 'Denunciar recuerdo' })).toBeNull();
  expect(commandMock).not.toHaveBeenCalled(); view.client.clear();
});
it('retains a failed report and retries the identical key/payload without false success', async () => {
  commandMock.mockResolvedValueOnce('<html>not an acknowledgement</html>').mockResolvedValueOnce({ reported: true });
  const view = mount(<EventMemorySourceActions targetId={target} version={1} controls={controls} />);
  const invalidation = jest.spyOn(view.client, 'invalidateQueries');
  fireEvent.click(screen.getByRole('button', { name: 'Denunciar recuerdo' }));
  fireEvent.change(screen.getByRole('textbox', { name: 'Motivo' }), { target: { value: 'Synthetic concern' } });
  fireEvent.click(screen.getByRole('button', { name: 'Confirmar' }));
  await screen.findByText(/No se pudo confirmar/);
  expect(screen.getByRole('dialog')).toBeTruthy(); expect(invalidation).not.toHaveBeenCalled();
  expect(screen.getByRole('textbox', { name: 'Motivo' })).toHaveProperty('value', 'Synthetic concern');
  fireEvent.click(screen.getByRole('button', { name: 'Confirmar' }));
  await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
  expect(commandMock.mock.calls[1]).toEqual(commandMock.mock.calls[0]);
  expect(commandMock.mock.calls[0]?.[1]).toEqual({ operation: 'source.report', reason: 'Synthetic concern', expectedSourceRevision: controls.sourceRevision });
  view.client.clear();
});
it('drops a pending decision immediately when the active account changes', () => {
  const view = mount(<EventMemorySourceActions targetId={target} version={1} controls={controls} />);
  fireEvent.click(screen.getByRole('button', { name: 'Denunciar recuerdo' }));
  fireEvent.change(screen.getByRole('textbox', { name: 'Motivo' }), { target: { value: 'Prior-account draft' } });
  session = { partyId: 10 }; view.rerender(<EventMemorySourceActions targetId={target} version={1} controls={controls} />);
  expect(screen.queryByRole('dialog')).toBeNull(); expect(commandMock).not.toHaveBeenCalled(); view.client.clear();
});
it('keeps source review absent for ordinary readers without rendering private rows', async () => {
  getMock.mockResolvedValue({ supported: true, canReview: false, items: [], nextCursor: null });
  const view = mount(<EventMemoryModerationPanel eventId="141" organizer={false} />);
  await waitFor(() => expect(getMock).toHaveBeenCalled());
  expect(screen.queryByRole('button', { name: 'Revisar recuerdos' })).toBeNull();
  expect(screen.queryByText('Synthetic hidden source')).toBeNull(); view.client.clear();
});
it('lets a scoped reviewer restore a hidden source with a reason and both current revisions', async () => {
  commandMock.mockResolvedValue({ state: 'visible', version: 3 });
  const view = mount(<EventMemoryModerationPanel eventId="141" organizer />);
  fireEvent.click(await screen.findByRole('button', { name: 'Revisar recuerdos' }));
  expect(screen.getByText('Synthetic hidden source')).toBeTruthy();
  expect(screen.queryByRole('button', { name: 'Retirar recuerdo' })).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: 'Restaurar recuerdo' }));
  fireEvent.change(screen.getByRole('textbox', { name: 'Motivo' }), { target: { value: 'Reviewed synthetic source' } });
  getMock.mockResolvedValue({ supported: true, canReview: true, items: [], nextCursor: null });
  fireEvent.click(screen.getByRole('button', { name: 'Confirmar' }));
  await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
  expect(commandMock).toHaveBeenCalledWith(target, { operation: 'source.restore', reason: 'Reviewed synthetic source', expectedVersion: 2, expectedSourceRevision: controls.sourceRevision }, expect.any(String));
  await waitFor(() => expect(screen.queryByText('Synthetic hidden source')).toBeNull()); view.client.clear();
});
it('can close a report without hiding and warns when the reported content has changed', async () => {
  const item = { ...report(), source: { ...controls, canHide: true, canReport: false }, openReports: 1, reasons: ['Synthetic concern'], reportsReferToEarlierContent: true };
  getMock.mockResolvedValue({ supported: true, canReview: true, items: [item], nextCursor: null });
  commandMock.mockResolvedValue({ state: 'visible', version: 3 });
  const view = mount(<EventMemoryModerationPanel eventId="141" organizer />);
  fireEvent.click(await screen.findByRole('button', { name: 'Revisar recuerdos' }));
  expect(screen.getByText(/Algunas denuncias corresponden/)).toBeTruthy();
  fireEvent.click(screen.getByRole('button', { name: 'Cerrar denuncia sin ocultar' }));
  fireEvent.change(screen.getByRole('textbox', { name: 'Motivo' }), { target: { value: 'Allowed synthetic source' } });
  fireEvent.click(screen.getByRole('button', { name: 'Confirmar' }));
  await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
  expect(commandMock.mock.calls[0]?.[1]).toEqual({ operation: 'source.report.resolve', reason: 'Allowed synthetic source', decision: 'dismissed', expectedVersion: 2, expectedSourceRevision: controls.sourceRevision });
  view.client.clear();
});
