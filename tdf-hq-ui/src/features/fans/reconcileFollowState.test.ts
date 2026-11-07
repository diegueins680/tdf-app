import { jest } from '@jest/globals';
import { QueryClient } from '@tanstack/react-query';
import type { FanFollowDTO } from '../../api/types';

const listFollows = jest.fn<() => Promise<FanFollowDTO[]>>();
jest.unstable_mockModule('../../api/fans', () => ({ Fans: { listFollows } }));
const { reconcileFollowState } = await import('./reconcileFollowState');
const follow = { ffArtistId: 17, ffArtistName: 'Artista ficticio', ffStartedAt: '2026-10-26T06:00:00Z' };
let client: QueryClient;
let current: boolean;
const scope = () => ({ viewerId: 42, artistId: 17, ownsSession: () => current });
beforeEach(() => { client = new QueryClient(); current = true; listFollows.mockReset(); });
afterEach(() => client.clear());

it.each([true, false])('replaces stale cached membership with one authoritative read (following=%s)', async following => {
  client.setQueryData(['fan-follows', 42], following ? [] : [follow]);
  listFollows.mockResolvedValue(following ? [follow] : []);
  expect(await reconcileFollowState(client, scope())).toBe(following);
  expect(listFollows).toHaveBeenCalledTimes(1);
  expect(client.getQueryData(['fan-follows', 42])).toEqual(following ? [follow] : []);
});

it('does not confuse an unavailable read with an absent relationship or retry the write', async () => {
  client.setQueryData(['fan-follows', 42], [follow]);
  listFollows.mockRejectedValue(new TypeError('Synthetic offline read'));
  expect(await reconcileFollowState(client, scope())).toBeNull();
  expect(client.getQueryData(['fan-follows', 42])).toEqual([follow]);
  expect(listFollows).toHaveBeenCalledTimes(1);
});

it('does not read when the captured account is no longer current', async () => {
  current = false;
  expect(await reconcileFollowState(client, scope())).toBeNull();
  expect(listFollows).not.toHaveBeenCalled();
});

it('does not cache or confirm a response after ownership changes during the read', async () => {
  client.setQueryData(['fan-follows', 42], []);
  let complete!: (rows: FanFollowDTO[]) => void;
  let started!: () => void;
  const began = new Promise<void>(resolve => { started = resolve; });
  listFollows.mockImplementation(() => new Promise(resolve => { complete = resolve; started(); }));
  const pending = reconcileFollowState(client, scope());
  await began;
  current = false; complete([follow]);
  expect(await pending).toBeNull();
  expect(client.getQueryData(['fan-follows', 42])).toEqual([]);
});

it('does not reuse a query result whose timestamp is ahead of the local clock', async () => {
  client.setQueryData(['fan-follows', 42], [], { updatedAt: Date.now() + 60_000 });
  listFollows.mockResolvedValue([follow]);
  expect(await reconcileFollowState(client, scope())).toBe(true);
  expect(listFollows).toHaveBeenCalledTimes(1);
});

it.each([null, {}, [{ ...follow, ffArtistId: 0 }], [{ ...follow, ffArtistId: '17' }]])('rejects malformed membership evidence %j', async payload => {
  client.setQueryData(['fan-follows', 42], [follow]);
  listFollows.mockResolvedValue(payload as unknown as FanFollowDTO[]);
  expect(await reconcileFollowState(client, scope())).toBeNull();
  expect(client.getQueryData(['fan-follows', 42])).toEqual([follow]);
});

it('refuses unsafe or missing account and artist identifiers before touching the cache', async () => {
  expect(await reconcileFollowState(client, { ...scope(), viewerId: 0 })).toBeNull();
  expect(await reconcileFollowState(client, { ...scope(), artistId: Number.MAX_SAFE_INTEGER + 1 })).toBeNull();
  expect(listFollows).not.toHaveBeenCalled();
});
