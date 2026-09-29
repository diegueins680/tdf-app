import { jest } from '@jest/globals';
import { act } from 'react';
import { ThemeProvider, createTheme } from '@mui/material';
import { fireEvent, render, screen, waitFor, cleanup } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { InteractionSummary, InteractionComment, InteractionPage, InteractionCommentContext, InteractionCommand } from '../../api/interactions';
import axe from 'axe-core';

const target = '10000000-0000-4000-8000-000000000001';
const rootId = '10000000-0000-4000-8000-000000000002';
const replyId = '10000000-0000-4000-8000-000000000003';
const summary = jest.fn<() => Promise<InteractionSummary>>();
const comments = jest.fn<(...args: unknown[]) => Promise<InteractionPage>>();
const moderation = jest.fn<() => Promise<InteractionPage>>();
const context = jest.fn<() => Promise<InteractionCommentContext>>();
const command = jest.fn<(id: string, input: InteractionCommand, key: string) => Promise<unknown>>();
jest.unstable_mockModule('../../api/interactions', () => ({ Interactions: { summary, comments, context, command, reactors: jest.fn(), moderation } }));
jest.unstable_mockModule('../../session/SessionContext', () => ({ useSession: () => ({ session: { partyId: 7 } }), getActiveSession: () => ({ partyId: 7 }), getStoredSessionToken: () => null }));
jest.unstable_mockModule('../../analytics/posthog', () => ({ getAnalyticsClient: () => ({ capture: jest.fn() }) }));
jest.unstable_mockModule('../../components/party-selector/PartySelector', () => ({ UserSelector: () => null, PartyMultiSelector: () => null }));
const { InteractionPanel } = await import('./InteractionPanel');
let client: QueryClient; let ordinal = 0;
const data: InteractionSummary = { id: target, kind: 'recording', key: 'record', ownerId: 8, title: 'Session', route: '/records', public: true,
  canManage: false, reactable: true, commentable: true, shareable: true, version: 1, commentPolicy: 'everyone', canReact: true, canComment: true,
  canModerate: false, commentCount: 2, rootCount: 1, reactions: [{ id: 'like', code: 'like', emoji: '👍', label: 'Me gusta', count: 0, selectable: true }],
  myReactionTypeId: null, subscription: 'participating', defaultSort: 'newest' };
let root: InteractionComment;
let reply: InteractionComment;
function view(props = {}) {
  client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  return render(<MemoryRouter><QueryClientProvider client={client}><ThemeProvider theme={createTheme({ components: { MuiButtonBase: { defaultProps: { disableRipple: true } } } })}><InteractionPanel kind="recording" entityKey={`record-${++ordinal}`} {...props} /></ThemeProvider></QueryClientProvider></MemoryRouter>);
}
beforeEach(() => {
  jest.clearAllMocks();
  Object.defineProperty(crypto, 'randomUUID', { configurable: true, value: () => `20000000-0000-4000-8000-${String(++ordinal).padStart(12, '0')}` });
  root = { id: rootId, targetId: target, parentId: null, rootId, depth: 0, version: 1, createdAt: '2026-09-28T10:00:00Z', editedAt: null,
    state: 'visible', body: 'Root comment', author: { id: 7, displayName: 'Ana', avatarUrl: null }, canEdit: true, canDelete: true, mentions: [], replyCount: 1 };
  reply = { ...root, id: replyId, parentId: rootId, depth: 1, body: 'A reply', author: { id: 8, displayName: 'Luis', avatarUrl: null }, canEdit: false, canDelete: false, replyCount: 0 };
  summary.mockResolvedValue(data);
  comments.mockImplementation(async (...args) => ({ items: [args[3] ? reply : root], nextCursor: null, sort: 'newest' }));
  context.mockImplementation(async () => ({ target: data, root, comment: reply, parent: root, surrounding: [reply] }));
  command.mockResolvedValue({});
});
afterEach(() => { cleanup(); client?.clear(); });

test('loads only expanded comments and replies, with accessible disclosure controls', async () => {
  view(); const disclosure = await screen.findByRole('button', { name: 'Ver los 2 comentarios' });
  expect(disclosure.getAttribute('aria-expanded')).toBe('false'); expect(comments).not.toHaveBeenCalled();
  act(() => disclosure.focus()); expect(document.activeElement).toBe(disclosure);
  fireEvent.click(disclosure); await screen.findByText('Root comment');
  expect(screen.queryByText('A reply')).toBeNull();
  const replies = screen.getByRole('button', { name: 'Ver 1 respuestas' }); fireEvent.click(replies);
  await screen.findByText('A reply'); expect(replies.getAttribute('aria-expanded')).toBe('true');
  fireEvent.click(screen.getByRole('button', { name: 'Ocultar respuestas' })); expect(screen.queryByText('A reply')).toBeNull();
  expect(document.activeElement).toBe(screen.getByRole('button', { name: 'Ver 1 respuestas' }));
  const result = await axe.run(document.body, { rules: { 'color-contrast': { enabled: false }, region: { enabled: false } } });
  expect(result.violations.map((violation) => violation.id)).toEqual([]);
});

test('rolls back a failed optimistic reaction and announces the error', async () => {
  let fail!: (error: Error) => void;
  command.mockImplementation(() => new Promise((_resolve, reject) => { fail = reject; }));
  view(); fireEvent.click(await screen.findByRole('button', { name: 'Me gusta: 0' }));
  await screen.findByRole('button', { name: 'Me gusta: 1' });
  expect(screen.getByRole('button', { name: 'Me gusta: 1' }).getAttribute('aria-pressed')).toBe('true');
  act(() => fail(new Error('offline')));
  await screen.findByRole('button', { name: 'Me gusta: 0' }); await screen.findByRole('alert');
});

test('keeps failed drafts and the same idempotency key until the user edits', async () => {
  command.mockRejectedValue(new Error('offline')); view({ initiallyExpanded: true });
  fireEvent.change(await screen.findByRole('textbox', { name: 'Escribe un comentario' }), { target: { value: 'Keep my draft' } });
  fireEvent.click(screen.getByRole('button', { name: 'Publicar' })); await screen.findByText(/Tu texto sigue aquí/);
  expect(screen.getByRole<HTMLTextAreaElement>('textbox', { name: 'Escribe un comentario' }).value).toBe('Keep my draft');
  fireEvent.click(screen.getByRole('button', { name: 'Publicar' }));
  await waitFor(() => expect(command).toHaveBeenCalledTimes(2));
  expect(command.mock.calls[0]?.[2]).toBe(command.mock.calls[1]?.[2]);
  await screen.findByText(/Tu texto sigue aquí/);
  fireEvent.change(screen.getByRole('textbox', { name: 'Escribe un comentario' }), { target: { value: 'Edited draft' } });
  fireEvent.click(screen.getByRole('button', { name: 'Publicar' }));
  await waitFor(() => expect(command).toHaveBeenCalledTimes(3));
  expect(command.mock.calls[2]?.[2]).not.toBe(command.mock.calls[0]?.[2]);
});

test('resolves and focuses a deep-linked reply outside the first page', async () => {
  comments.mockResolvedValue({ items: [], nextCursor: null, sort: 'newest' }); view({ focusCommentId: replyId });
  await screen.findByText('A reply');
  await waitFor(() => expect(document.activeElement?.id).toBe(`comment-${replyId}`));
  expect(context).toHaveBeenCalled();
});

test('author deletion leaves a parent placeholder and replies intact', async () => {
  command.mockImplementation(async (_target, input) => {
    if (input.operation === 'comment.delete') root = { ...root, state: 'deleted', body: '', author: null, canEdit: false, canDelete: false, version: 2 };
    return {};
  });
  view({ initiallyExpanded: true }); await screen.findByText('Root comment');
  fireEvent.click(screen.getByRole('button', { name: 'Ver 1 respuestas' })); await screen.findByText('A reply');
  fireEvent.click(screen.getAllByRole('button', { name: 'Opciones del comentario' })[0]!);
  fireEvent.click(await screen.findByRole('menuitem', { name: 'Eliminar mi comentario' }));
  fireEvent.click(await screen.findByRole('button', { name: 'Confirmar' }));
  await screen.findByText('Comentario eliminado'); expect(screen.queryByText('Root comment')).toBeNull(); expect(screen.getByText('A reply')).toBeTruthy();
});

test('bounds retained discussion pages and refetches while allowing backward navigation', async () => {
  comments.mockImplementation(async (...args) => {
    const index = Number(args[4] ?? 0);
    return { items: [{ ...root, id: `10000000-0000-4000-8000-${String(index + 100).padStart(12, '0')}`, body: `Window comment ${index}`, replyCount: 0 }], nextCursor: index < 7 ? String(index + 1) : null, sort: 'newest' };
  });
  view(); fireEvent.click(await screen.findByRole('button', { name: 'Ver los 2 comentarios' }));
  await screen.findByText('Window comment 0');
  for (let index = 1; index < 8; index++) {
    fireEvent.click(await screen.findByRole('button', { name: /Ver más comentarios/ }));
    await screen.findByText(`Window comment ${index}`);
  }
  expect(screen.queryByText('Window comment 0')).toBeNull();
  expect(screen.getByText('Window comment 3')).toBeTruthy();
  comments.mockClear();
  await act(async () => { await client.invalidateQueries({ queryKey: ['interactions'] }); });
  expect(comments).toHaveBeenCalledTimes(5);
  fireEvent.click(screen.getByRole('button', { name: 'Ver comentarios anteriores' }));
  await screen.findByText('Window comment 2');
  expect(screen.queryByText('Window comment 7')).toBeNull();
});


test('lets moderators read report reasons before making a decision', async () => {
  summary.mockResolvedValue({ ...data, canModerate: true });
  moderation.mockResolvedValue({ items: [{ ...root, moderationBody: 'Reported comment', openReports: 1, reportReasons: ['Unwanted personal information'] }], nextCursor: null });
  view({ initiallyExpanded: true }); fireEvent.click(await screen.findByRole('button', { name: 'Moderación' }));
  expect(await screen.findByText('Unwanted personal information')).toBeTruthy();
  expect(screen.getByRole('region', { name: 'Motivos de los reportes' })).toBeTruthy();
  fireEvent.change(screen.getByRole('textbox', { name: 'Motivo de la decisión' }), { target: { value: 'Reviewed the report' } });
  fireEvent.click(screen.getByRole('button', { name: 'Desestimar reportes' }));
  await waitFor(() => expect(command).toHaveBeenCalledWith(target, expect.objectContaining({ operation: 'comment.report.resolve', commentId: rootId }), expect.any(String)));
});


test('permits only withdrawing the selected reaction after write eligibility is revoked', async () => {
  summary.mockResolvedValue({ ...data, canReact: true, myReactionTypeId: 'like', reactions: [
    { ...data.reactions[0]!, count: 1, selectable: false },
    { id: 'love', code: 'love', emoji: '❤️', label: 'Me encanta', count: 2, selectable: false },
  ] });
  view(); const selected = await screen.findByRole<HTMLButtonElement>('button', { name: 'Me gusta: 1' });
  expect(selected.disabled).toBe(false);
  expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Me encanta: 2' }).disabled).toBe(true);
  fireEvent.click(selected);
  await waitFor(() => expect(command).toHaveBeenCalledWith(target, { operation: 'reaction.set', reactionTypeId: null }, expect.any(String)));
});


test('lets owners hide blocked-author content from their scoped moderation queue', async () => {
  summary.mockResolvedValue({ ...data, canManage: true });
  moderation.mockResolvedValue({ items: [{ ...root, author: null, body: '', moderationBody: 'Blocked author content', canEdit: false, canDelete: false }], nextCursor: null });
  view({ initiallyExpanded: true });
  fireEvent.click(await screen.findByRole('button', { name: 'Moderación' }));
  await screen.findByText('Blocked author content');
  expect(screen.queryByRole('button', { name: 'Retirar como administrador' })).toBeNull();
  fireEvent.change(screen.getByRole('textbox', { name: 'Motivo de la decisión' }), { target: { value: 'Publication policy' } });
  fireEvent.click(screen.getByRole('button', { name: 'Ocultar en mi contenido' }));
  await waitFor(() => expect(command).toHaveBeenCalledWith(target, expect.objectContaining({ operation: 'comment.hide', commentId: rootId }), expect.any(String)));
});
