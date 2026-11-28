import { jest } from '@jest/globals';
import { render, screen, fireEvent, waitFor, cleanup, configure } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { SuggestedFriendDTO } from '../../api/types';
import type { SocialMe, SocialV2 } from '../../api/socialV2';
// MUI cold rendering on the shared development host can exceed the 1s default.
configure({ asyncUtilTimeout: 5000 });
jest.setTimeout(15000);
let partyId = 1;
const me = jest.fn<() => Promise<SocialMe>>();
const following = jest.fn<() => Promise<{ items: never[]; nextCursor: null }>>();
const discover = jest.fn<typeof SocialV2.discover>();
const network = jest.fn<() => Promise<SuggestedFriendDTO[]>>();
jest.unstable_mockModule('../../api/social', () => ({ SocialAPI: { listNetworkSuggestions: network } }));
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
  network.mockResolvedValue([]);
  preferences.mockResolvedValue({ discoverable: false, personalized: false, revision: 1 });
});
afterEach(cleanup);
it('defaults to Following and explains its empty state without requesting Discover', async () => {
  show();
  await screen.findByText('Sigue artistas o descubre personas para empezar.');
  expect(screen.getByRole('tab', { name: 'Siguiendo' }).getAttribute('aria-selected')).toBe('true');
  expect(discover).not.toHaveBeenCalled();
  expect(network).not.toHaveBeenCalled();
});
it('provides a separate discover tab with an accessible panel and cold-start recovery', async () => {
  show();
  fireEvent.click(screen.getByRole('tab', { name: 'Descubrir' }));
  await screen.findByText('No hay sugerencias nuevas. Puedes actualizar tus intereses o volver más tarde.');
  expect(screen.getByRole('tabpanel').getAttribute('aria-labelledby')).toBe('social-discover-tab');
});
it('preserves discoverability when the user opts out of personalization', async () => {
  show();
  fireEvent.click(await screen.findByRole('checkbox', { name: 'Personalizar con mis intereses y mi actividad' }));
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
  expect(network).not.toHaveBeenCalled();
});

function profileItem(partyId: number, displayName: string) {
  return { partyId, displayName, reason: 'public_profile' as const, relationship: {
    partyId, revision: 17, following: false, requested: false, incoming: false, connected: false,
    blocked: false, muted: false, dismissed: false,
  } };
}

it.each([['Conectar', 'request'], ['Seguir', 'follow']])('ranks eligible profiles and sends %s at the current revision', async (label, op) => {
  discover.mockResolvedValue({ personalized: true, items: [profileItem(8, 'Primera'), profileItem(9, 'Segunda')] });
  network.mockResolvedValue([
    { sfPartyId: 7, sfReason: 'network', sfMutualCount: 0 },
    { sfPartyId: 9, sfReason: 'network', sfMutualCount: 0 },
  ]);
  show();
  fireEvent.click(screen.getByRole('tab', { name: 'Descubrir' }));
  await screen.findByText('Cerca de tu red');
  expect(screen.getAllByRole('button', { name: /^Conectar:/ }).map(b => b.getAttribute('aria-label')))
    .toEqual(['Conectar: Segunda', 'Conectar: Primera']);
  fireEvent.click(screen.getByRole('button', { name: label + ': Segunda' }));
  await waitFor(() => expect(command).toHaveBeenCalledWith(9, op, 17, expect.any(String)));
});

it('preserves discovery order and reasons when the viewer opts out', async () => {
  discover.mockResolvedValue({ personalized: false, items: [profileItem(8, 'Primera'), profileItem(9, 'Segunda')] });
  show();
  fireEvent.click(screen.getByRole('tab', { name: 'Descubrir' }));
  await screen.findByRole('button', { name: 'Conectar: Segunda' });
  expect(network).not.toHaveBeenCalled();
  expect(screen.queryByText('Cerca de tu red')).toBeNull();
});

it('keeps Discover usable when the optional ranking fails', async () => {
  discover.mockResolvedValue({ personalized: true, items: [profileItem(8, 'Primera')] });
  network.mockRejectedValueOnce(new Error('unavailable'));
  show();
  fireEvent.click(screen.getByRole('tab', { name: 'Descubrir' }));
  await waitFor(() => expect(network).toHaveBeenCalledTimes(1));
  expect(screen.getByText('Perfil público de la comunidad')).toBeTruthy();
  expect(screen.getByRole('button', { name: 'Seguir: Primera' })).toBeTruthy();
  expect(screen.queryByText('Cerca de tu red')).toBeNull();
});
