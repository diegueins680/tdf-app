import { jest } from '@jest/globals';
import { beginLogoutIntent, clearLogoutIntent, finishLogoutIntent, getLogoutRevision, getLogoutStatus, hasLogoutIntent, LOGOUT_INTENT_KEY, ownsLogoutRevision, subscribeLogoutIntent } from './logoutIntent';

beforeEach(() => { localStorage.clear(); });
afterEach(() => { jest.restoreAllMocks(); });
it('retains an identity-free logout intent and changes revision on fresh login', () => {
  const initial = getLogoutRevision();
  const revision = beginLogoutIntent();
  finishLogoutIntent(revision, false);
  expect(getLogoutStatus()).toBe('unconfirmed');
  expect(ownsLogoutRevision(initial)).toBe(false);
  expect(Object.keys(JSON.parse(localStorage.getItem(LOGOUT_INTENT_KEY)!) as Record<string, unknown>).sort()).toEqual(['revision', 'status']);
  clearLogoutIntent();
  expect(hasLogoutIntent()).toBe(false);
  finishLogoutIntent(revision, true);
  expect(hasLogoutIntent()).toBe(false);
  expect(ownsLogoutRevision(revision)).toBe(false);
});
it.each(['broken', '{}', '{"revision":"x","status":"wrong"}'])('fails closed on malformed persisted state: %s', (raw) => {
  localStorage.setItem(LOGOUT_INTENT_KEY, raw);
  expect(hasLogoutIntent()).toBe(true);
});
it('disables automatic restoration when storage is unavailable, but permits fresh explicit login in memory', () => {
  jest.spyOn(Storage.prototype, 'getItem').mockImplementation(() => { throw new Error('restricted'); });
  jest.spyOn(Storage.prototype, 'setItem').mockImplementation(() => { throw new Error('restricted'); });
  expect(hasLogoutIntent()).toBe(true);
  clearLogoutIntent();
  expect(hasLogoutIntent()).toBe(false);
  beginLogoutIntent();
  expect(hasLogoutIntent()).toBe(true);
  jest.restoreAllMocks();
  clearLogoutIntent();
});
it('notifies same-tab subscribers and remote storage clearing', () => {
  const listener = jest.fn(); const stop = subscribeLogoutIntent(listener);
  beginLogoutIntent(); expect(listener).toHaveBeenCalledTimes(1);
  window.dispatchEvent(new StorageEvent('storage', { key: null }));
  expect(listener).toHaveBeenCalledTimes(2);
  stop();
});
