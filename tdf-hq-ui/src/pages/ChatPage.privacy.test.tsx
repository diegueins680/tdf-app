import { jest } from '@jest/globals';
import { cleanup, configure, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { ChatThreadDTO, ChatMessageDTO } from '../api/types';
configure({ asyncUtilTimeout: 15000 });
jest.setTimeout(30000);
let partyId = 1;
const listThreads = jest.fn<() => Promise<ChatThreadDTO[]>>();
const listMessages = jest.fn<() => Promise<ChatMessageDTO[]>>();
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
const thread: ChatThreadDTO = { ctThreadId: 10, ctOtherPartyId: 2, ctOtherDisplayName: 'Private peer',
  ctLastMessage: 'Private preview', ctLastMessageAt: '2026-09-15T12:00:00Z', ctUpdatedAt: '2026-09-15T12:00:00Z' };
const message: ChatMessageDTO = { cmId: 1, cmThreadId: 10, cmSenderPartyId: 2, cmBody: 'Private body', cmCreatedAt: '2026-09-15T12:00:00Z' };
const clients: QueryClient[] = [];
function Unread() { const { unreadCount } = useChatUnreadCount(); return <span data-testid="badge">{unreadCount}</span>; }
function setup() {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  clients.push(client);
  const tree = () => <QueryClientProvider client={client}><MemoryRouter><ChatPage /><Unread /></MemoryRouter></QueryClientProvider>;
  const view = render(tree());
  return { client, ...view, refresh: () => view.rerender(tree()) };
}
beforeEach(() => {
  partyId = 1; jest.clearAllMocks(); window.localStorage.clear();
  Element.prototype.scrollIntoView = jest.fn();
  listThreads.mockResolvedValue([thread]); listMessages.mockResolvedValue([message]); sendMessage.mockResolvedValue(message);
});
afterEach(() => { cleanup(); clients.splice(0).forEach((client) => client.clear()); });
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
