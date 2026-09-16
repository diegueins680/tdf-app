import { jest } from '@jest/globals';
import { render, screen, fireEvent, waitFor, cleanup, configure } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { SocialMe } from '../../api/socialV2';
// MUI cold rendering on the shared development host can exceed the 1s default.
configure({ asyncUtilTimeout: 5000 });
jest.setTimeout(15000);
let partyId = 1;
const me = jest.fn<() => Promise<SocialMe>>();
const following = jest.fn<() => Promise<{ items: never[]; nextCursor: null }>>();
const discover = jest.fn<() => Promise<{ personalized: boolean; items: never[] }>>();
const preferences = jest.fn<() => Promise<unknown>>();
const command = jest.fn<() => Promise<unknown>>();
const relationship = jest.fn<() => Promise<unknown>>();
jest.unstable_mockModule('../../api/socialV2', () => ({ SocialV2: {
  me, following, discover, preferences, command, relationship,
} }));
jest.unstable_mockModule('../../session/SessionContext', () => ({ useSession: () => ({ session: { partyId } }) }));
const { default: SocialWorkspace } = await import('./SocialWorkspace');
function show(client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } })) {
  return render(<QueryClientProvider client={client}><MemoryRouter><SocialWorkspace /></MemoryRouter></QueryClientProvider>);
}
beforeEach(() => {
  partyId = 1; jest.clearAllMocks();
  me.mockResolvedValue({ discoverable: false, personalized: true, revision: 0, relationships: [] });
  following.mockResolvedValue({ items: [], nextCursor: null });
  discover.mockResolvedValue({ personalized: true, items: [] });
  preferences.mockResolvedValue({ discoverable: false, personalized: false, revision: 1 });
});
afterEach(cleanup);
it('defaults to Following and explains its empty state without requesting Discover', async () => {
  show();
  await screen.findByText('Sigue artistas o descubre personas para empezar.');
  expect(screen.getByRole('tab', { name: 'Siguiendo' }).getAttribute('aria-selected')).toBe('true');
  expect(discover).not.toHaveBeenCalled();
});
it('provides a separate discover tab with an accessible panel and cold-start recovery', async () => {
  show();
  fireEvent.click(screen.getByRole('tab', { name: 'Descubrir' }));
  await screen.findByText('No hay sugerencias nuevas. Puedes actualizar tus intereses o volver más tarde.');
  expect(screen.getByRole('tabpanel').getAttribute('aria-labelledby')).toBe('social-discover-tab');
});
it('preserves discoverability when the user opts out of personalization', async () => {
  show();
  fireEvent.click(await screen.findByRole('checkbox', { name: 'Personalizar con mis intereses musicales' }));
  await waitFor(() => expect(preferences).toHaveBeenCalledWith({ discoverable: false, personalized: false }, 0));
});
it('offers retry for a denied or failed feed instead of rendering it as empty success', async () => {
  following.mockRejectedValue(new Error('404'));
  show();
  await screen.findByText('No pudimos cargar las publicaciones.');
  expect(screen.getByRole('button', { name: 'Reintentar' })).toBeTruthy();
  expect(screen.queryByText('Sigue artistas o descubre personas para empezar.')).toBeNull();
});
it('never reuses a previous account query result', async () => {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  client.setQueryData(['social-v2', 1, 'me'], { discoverable: true, personalized: true, revision: 9,
    relationships: [{ displayName: 'Private previous person' }] });
  partyId = 2;
  show(client);
  await screen.findByRole('checkbox', { name: 'Mostrar mi perfil en Descubrir' });
  fireEvent.click(screen.getByRole('tab', { name: 'Conexiones' }));
  await screen.findByText('Aún no hay conexiones. Cada persona debe aceptar para conectarse.');
  expect(screen.queryByText('Private previous person')).toBeNull();
});

it('hides previously cached content when an authoritative refetch denies access', async () => {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  client.setQueryData(['social-v2', 1, 'following'], { pages: [{items: [{postId: 1,
    title: 'Revoked club content', authorName: 'Previous author', content: 'Previously delivered',
    artistId: 5, publishedAt: '2026-01-01T00:00:00Z'}], nextCursor: null}], pageParams: [undefined] });
  following.mockRejectedValue(new Error('404'));
  show(client);
  await screen.findByText('No pudimos cargar las publicaciones.');
  expect(screen.queryByText('Revoked club content')).toBeNull();
});
it('does not fetch social resources for an anonymous session', () => {
  partyId = 0;
  show();
  expect(screen.getByText('Inicia sesión para ver tu comunidad.')).toBeTruthy();
  expect(me).not.toHaveBeenCalled();
  expect(following).not.toHaveBeenCalled();
  expect(discover).not.toHaveBeenCalled();
});
