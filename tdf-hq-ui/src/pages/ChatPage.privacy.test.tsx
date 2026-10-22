import { jest } from '@jest/globals';
import { act, cleanup, configure, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { onlineManager, QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, useLocation, useNavigate } from 'react-router-dom';
import type { ChatThreadDTO, ChatMessageDTO } from '../api/types';
configure({ asyncUtilTimeout: 15000 });
jest.setTimeout(30000);
let partyId = 1;
const listThreads = jest.fn<() => Promise<ChatThreadDTO[]>>();
const listMessages = jest.fn<(threadId: number, options: { limit: number }) => Promise<ChatMessageDTO[]>>();
const sendMessage = jest.fn<(thread: number, body: string) => Promise<ChatMessageDTO>>();
const getOrCreateDmThread = jest.fn<() => Promise<ChatThreadDTO>>();
jest.unstable_mockModule('../api/chat', () => ({ ChatAPI: { listThreads, listMessages, sendMessage, getOrCreateDmThread } }));
jest.unstable_mockModule('../api/meta', () => ({ Meta: { health: async () => ({ status: 'ok' }) } }));
jest.unstable_mockModule('../api/social', () => ({ SocialAPI: {
  listFriends: async () => [], listProfiles: async () => [], getProfile: async () => ({ sppDisplayName: 'Private peer' }),
} }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: { partyId } }) }));
const { default: ChatPage } = await import('./ChatPage');
const { useChatUnreadCount } = await import('../hooks/useChatUnreadCount');
const { loadChatReadMap } = await import('../utils/chatReadState');
const thread: ChatThreadDTO = { ctThreadId: 10, ctOtherPartyId: 2, ctOtherDisplayName: 'Private peer',
  ctLastMessage: 'Private preview', ctLastMessageAt: '2026-09-15T12:00:00Z', ctUpdatedAt: '2026-09-15T12:00:00Z' };
const message: ChatMessageDTO = { cmId: 1, cmThreadId: 10, cmSenderPartyId: 2, cmBody: 'Private body', cmCreatedAt: '2026-09-15T12:00:00Z' };
const clients: QueryClient[] = [];
function CurrentLocation() { return <span data-testid="location">{useLocation().search}</span>; }
function NotificationLink() {
  const navigate = useNavigate();
  return <button onClick={() => navigate('/chat?threadId=10')}>Open chat notification</button>;
}
function Unread() { const { unreadCount } = useChatUnreadCount(); return <span data-testid="badge">{unreadCount}</span>; }
function setup(path = '/chat', seed?: (client: QueryClient) => void, staleTime = 30_000) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, staleTime }, mutations: { retry: false } } });
  clients.push(client);
  seed?.(client);
  const tree = () => <QueryClientProvider client={client}><MemoryRouter initialEntries={[path]}><ChatPage /><Unread /><CurrentLocation /><NotificationLink /></MemoryRouter></QueryClientProvider>;
  const view = render(tree());
  return { client, ...view, refresh: () => view.rerender(tree()) };
}
beforeEach(() => {
  partyId = 1; jest.clearAllMocks(); window.localStorage.clear();
  Element.prototype.scrollIntoView = jest.fn();
  listThreads.mockResolvedValue([thread]); listMessages.mockResolvedValue([message]); sendMessage.mockResolvedValue(message);
});
afterEach(() => { cleanup(); clients.splice(0).forEach((client) => client.clear()); onlineManager.setOnline(true); });
it('hides cached previews, headers, bodies and unread counts after a denied thread refetch', async () => {
  const { client } = setup();
  await screen.findByText('Private body');
  listThreads.mockRejectedValue(new Error('Conversación no disponible.'));
  await client.refetchQueries({ queryKey: ['chat-threads', 1] });
  await waitFor(() => {
    expect(screen.queryByText('Private body')).toBeNull();
    expect(screen.queryByText('Private preview')).toBeNull();
    expect(screen.getByTestId('badge').textContent).toBe('0');
    expect(screen.queryByRole('link', { name: 'Abrir perfil de Private peer' })).toBeNull();
    expect(screen.getByTestId('badge').textContent).toBe('0');
    expect(screen.getByRole('button', { name: 'Enviar mensaje' }).hasAttribute('disabled')).toBe(true);
  });
});
it('hides the denied conversation preview and composer after a message refetch fails', async () => {
  const { client } = setup();
  await screen.findByText('Private body');
  listMessages.mockRejectedValue(new Error('Conversación no disponible.'));
  await client.refetchQueries({ queryKey: ['chat-messages', 1, 10] });
  await waitFor(() => {
    expect(screen.queryByText('Private body')).toBeNull();
    expect(screen.queryByText('Private preview')).toBeNull();
    expect(screen.getByTestId('badge').textContent).toBe('0');
    expect(screen.queryByRole('link', { name: 'Abrir perfil de Private peer' })).toBeNull();
    expect(screen.getByRole('button', { name: 'Enviar mensaje' }).hasAttribute('disabled')).toBe(true);
  });
});
it('isolates cached conversations and drafts when switching accounts', async () => {
  const view = setup();
  await screen.findByText('Private body');
  fireEvent.change(screen.getByRole('textbox', { name: 'Escribe un mensaje' }), { target: { value: 'Private draft' } });
  partyId = 3; listThreads.mockResolvedValue([]); listMessages.mockResolvedValue([]);
  view.refresh();
  expect(screen.queryByText('Private body')).toBeNull();
  expect(screen.queryByText('Private preview')).toBeNull();
  expect(screen.queryByDisplayValue('Private draft')).toBeNull();
  await waitFor(() => expect(view.client.getQueryData(['chat-threads', 3])).toEqual([]));
  expect(sendMessage).not.toHaveBeenCalled();
});
it('keeps the API send contract and trims through its existing adapter', async () => {
  setup(); await screen.findByText('Private body');
  fireEvent.change(screen.getByRole('textbox', { name: 'Escribe un mensaje' }), { target: { value: 'Booking follow-up' } });
  fireEvent.click(screen.getByRole('button', { name: 'Enviar mensaje' }));
  await waitFor(() => expect(sendMessage).toHaveBeenCalledWith(10, 'Booking follow-up'));
});

it('opens the notification thread instead of the newest conversation without creating one', async () => {
  listThreads.mockResolvedValue([{ ...thread, ctThreadId: 20 }, thread]);
  window.localStorage.setItem('tdf-chat-selected-thread-v1:party:1', '20');
  setup('/chat?threadId=10');
  await waitFor(() => expect(listMessages).toHaveBeenLastCalledWith(10, { limit: 80 }));
  expect(listMessages.mock.calls.every(([threadId]) => threadId === 10)).toBe(true);
  expect(getOrCreateDmThread).not.toHaveBeenCalled();
});
it.each([true, false])('rechecks notification authorization during same-route navigation (allowed=%s)', async (allowed) => {
  const other = { ...thread, ctThreadId: 20, ctLastMessage: 'Other preview' };
  listThreads.mockResolvedValue([other, thread]);
  listMessages.mockImplementation(async (id) => id === 20
    ? [{ ...message, cmThreadId: 20, cmBody: 'Other body' }] : [message]);
  setup('/chat', (client) => client.setQueryData(['chat-messages', 1, 10], [message]));
  await screen.findByText('Other body');
  const requestsBeforeLink = listThreads.mock.calls.length;
  let resolveThreads!: (rows: ChatThreadDTO[]) => void;
  listThreads.mockReturnValue(new Promise((resolve) => { resolveThreads = resolve; }));
  fireEvent.click(screen.getByRole('button', { name: 'Open chat notification' }));
  expect(screen.queryByText('Private body')).toBeNull();
  expect(screen.queryByText('Private preview')).toBeNull();
  expect(loadChatReadMap(1)['10']).toBeUndefined();
  expect(screen.getByTestId('location').textContent).toBe('?threadId=10');
  await waitFor(() => expect(listThreads.mock.calls.length).toBeGreaterThan(requestsBeforeLink));
  expect(listMessages.mock.calls.every(([id]) => id !== 10)).toBe(true);
  await act(async () => { resolveThreads(allowed ? [thread] : []); });
  if (allowed) {
    await screen.findByText('Private body');
    await waitFor(() => expect(loadChatReadMap(1)['10']).toBe(message.cmCreatedAt));
  } else {
    await screen.findByText('La conversación de esta notificación no está disponible.');
    expect(screen.queryByText('Private body')).toBeNull();
    expect(screen.queryByText('Private preview')).toBeNull();
    expect(loadChatReadMap(1)['10']).toBeUndefined();
    expect(listMessages.mock.calls.every(([id]) => id !== 10)).toBe(true);
  }
  expect(getOrCreateDmThread).not.toHaveBeenCalled();
});
it.each(['ordinary poll', 'previous notification'])('ignores late authorization from a %s', async (source) => {
  const other = { ...thread, ctThreadId: 20, ctLastMessage: 'Other preview' };
  listThreads.mockResolvedValue([other, thread]);
  listMessages.mockImplementation(async (id) => id === 20
    ? [{ ...message, cmThreadId: 20, cmBody: 'Other body' }] : [message]);
  const { client } = setup('/chat', (cache) => cache.setQueryData(['chat-messages', 1, 10], [message]));
  await screen.findByText('Other body');
  const baseline = listThreads.mock.calls.length;
  let resolveOld!: (rows: ChatThreadDTO[]) => void;
  let resolveCurrent!: (rows: ChatThreadDTO[]) => void;
  listThreads.mockReturnValueOnce(new Promise((resolve) => { resolveOld = resolve; }))
    .mockReturnValueOnce(new Promise((resolve) => { resolveCurrent = resolve; }));
  if (source === 'ordinary poll') {
    act(() => { void client.refetchQueries({ queryKey: ['chat-threads', 1] }); });
  } else {
    fireEvent.click(screen.getByRole('button', { name: 'Open chat notification' }));
  }
  await waitFor(() => expect(listThreads.mock.calls.length).toBe(baseline + 1));
  fireEvent.click(screen.getByRole('button', { name: 'Open chat notification' }));
  // A ten-second background poll must not stand in for navigation admission.
  await waitFor(() => expect(listThreads.mock.calls.length).toBe(baseline + 2), { timeout: 2000 });
  await act(async () => { resolveOld([thread]); });
  expect(screen.queryByText('Private body')).toBeNull();
  expect(screen.queryByText('Private preview')).toBeNull();
  expect(loadChatReadMap(1)['10']).toBeUndefined();
  expect(screen.getByTestId('location').textContent).toBe('?threadId=10');
  await act(async () => { resolveCurrent([]); });
  await screen.findByText('La conversación de esta notificación no está disponible.');
  expect(screen.queryByText('Private body')).toBeNull();
  expect(screen.queryByText('Private preview')).toBeNull();
  expect(listMessages.mock.calls.every(([id]) => id !== 10)).toBe(true);
  expect(loadChatReadMap(1)['10']).toBeUndefined();
});
it.each([
  { allowed: true, staleTime: 0 },
  { allowed: false, staleTime: 0 },
  { allowed: true, staleTime: 30_000 },
  { allowed: false, staleTime: 30_000 },
])('waits for fresh notification authorization (allowed=$allowed, staleTime=$staleTime)', async ({ allowed, staleTime }) => {
  let resolveThreads!: (threads: ChatThreadDTO[]) => void;
  listThreads.mockReturnValue(new Promise((resolve) => { resolveThreads = resolve; }));
  setup('/chat?threadId=10', (client) => {
    client.setQueryData(['chat-threads', 1], [thread]);
    client.setQueryData(['chat-messages', 1, 10], [message]);
  }, staleTime);
  expect(screen.queryByText('Private body')).toBeNull();
  expect(screen.queryByText('Private preview')).toBeNull();
  await waitFor(() => expect(listThreads).toHaveBeenCalled());
  expect(listMessages).not.toHaveBeenCalled();
  expect(loadChatReadMap(1)).toEqual({});
  expect(screen.getByRole('button', { name: 'Enviar mensaje' }).hasAttribute('disabled')).toBe(true);
  expect(screen.getByTestId('location').textContent).toBe('?threadId=10');
  await act(async () => { resolveThreads(allowed ? [thread] : []); });
  if (allowed) {
    await screen.findByText('Private body');
    await waitFor(() => expect(loadChatReadMap(1)['10']).toBe(message.cmCreatedAt));
    if (staleTime === 0) expect(listMessages).toHaveBeenCalledWith(10, { limit: 80 });
  } else {
    await screen.findByText('La conversación de esta notificación no está disponible.');
    expect(screen.queryByText('Private body')).toBeNull();
    expect(listMessages).not.toHaveBeenCalled();
    expect(loadChatReadMap(1)).toEqual({});
  }
  expect(getOrCreateDmThread).not.toHaveBeenCalled();
});
it('withholds cached notification content while authorization is offline-paused', async () => {
  onlineManager.setOnline(false);
  let resolveThreads!: (threads: ChatThreadDTO[]) => void;
  listThreads.mockReturnValue(new Promise((resolve) => { resolveThreads = resolve; }));
  const { client } = setup('/chat?threadId=10', (cache) => {
    cache.setQueryData(['chat-threads', 1], [thread]);
    cache.setQueryData(['chat-messages', 1, 10], [message]);
  });
  expect(client.getQueryCache().findAll({ queryKey: ['chat-notification-authorization', 1] })
    .some((query) => query.state.fetchStatus === 'paused')).toBe(true);
  expect(screen.queryByText('Private body')).toBeNull();
  expect(screen.queryByText('Private preview')).toBeNull();
  expect(loadChatReadMap(1)).toEqual({});
  expect(listThreads).not.toHaveBeenCalled();
  expect(listMessages).not.toHaveBeenCalled();
  expect(screen.getByTestId('location').textContent).toBe('?threadId=10');
  await act(async () => { onlineManager.setOnline(true); });
  await waitFor(() => expect(listThreads).toHaveBeenCalled());
  expect(screen.queryByText('Private body')).toBeNull();
  await act(async () => { resolveThreads([]); });
  await screen.findByText('La conversación de esta notificación no está disponible.');
  expect(listMessages).not.toHaveBeenCalled();
  expect(loadChatReadMap(1)).toEqual({});
});
it.each(['999', '0', '-1', '9007199254740992', 'https://evil.example'])('rejects an unavailable notification thread %s', async (id) => {
  const { client } = setup(`/chat?threadId=${encodeURIComponent(id)}`);
  await screen.findByText('La conversación de esta notificación no está disponible.');
  await waitFor(() => expect(screen.getByTestId('location').textContent).toBe(''));
  await act(async () => { await client.refetchQueries({ queryKey: ['chat-threads', 1] }); });
  expect(getOrCreateDmThread).not.toHaveBeenCalled();
  expect(listMessages).not.toHaveBeenCalled();
  expect(screen.getByTestId('badge').textContent).toBe('1');
  expect(screen.queryByText('Private body')).toBeNull();
  // Only an explicit choice may leave the unavailable-link state.
  fireEvent.click(screen.getByText('Private preview'));
  await screen.findByText('Private body');
  expect(listMessages).toHaveBeenCalledWith(10, { limit: 80 });
  expect(screen.queryByText('La conversación de esta notificación no está disponible.')).toBeNull();
});

it('retains a notification link through a failed list request and opens it through the retry button', async () => {
  listThreads.mockRejectedValue(new Error('Temporary network outage'));
  setup('/chat?threadId=10');
  await screen.findByText('Temporary network outage');
  expect(screen.getByTestId('location').textContent).toBe('?threadId=10');
  expect(listMessages).not.toHaveBeenCalled();
  expect(screen.queryByText('La conversación de esta notificación no está disponible.')).toBeNull();
  listThreads.mockResolvedValue([{ ...thread, ctThreadId: 20 }, thread]);
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar' }));
  await screen.findByText('Private body', {}, { timeout: 2000 });
  await waitFor(() => expect(screen.getByTestId('location').textContent).toBe(''));
  expect(listMessages.mock.calls.every(([threadId]) => threadId === 10)).toBe(true);
  expect(getOrCreateDmThread).not.toHaveBeenCalled();
});
