import { discardRecoverableDraft, readRecoverableDraft, saveRecoverableDraft } from './recoverableDraft';
import { reconcileSessionPersonalData } from '../utils/sessionPersonalData';

const parse = (value: unknown) => typeof value === 'object' && value !== null && 'bio' in value && typeof value.bio === 'string'
  ? { bio: value.bio } : undefined;
beforeEach(() => { sessionStorage.clear(); localStorage.clear(); });

it('restores exact form metadata in the same tab, with owner and scope isolation', () => {
  expect(saveRecoverableDraft(42, 'artist:7', { bio: 'Mi biografía sin guardar' }, 100)).toBe(true);
  expect(readRecoverableDraft(42, 'artist:7', parse, 101)).toEqual({ bio: 'Mi biografía sin guardar' });
  expect(readRecoverableDraft(84, 'artist:7', parse, 101)).toBeUndefined();
  expect(readRecoverableDraft(42, 'artist:8', parse, 101)).toBeUndefined();
  expect(localStorage.length).toBe(0);
});
it('expires, discards and rejects invalid metadata without adopting another account data', () => {
  saveRecoverableDraft(42, 'artist:7', { bio: 'private draft' }, 100);
  expect(readRecoverableDraft(42, 'artist:7', parse, 100 + 8 * 60 * 60 * 1000)).toBeUndefined();
  saveRecoverableDraft(42, 'artist:7', { bio: 'private draft' });
  discardRecoverableDraft(42, 'artist:7');
  expect(readRecoverableDraft(42, 'artist:7', parse)).toBeUndefined();
  expect(saveRecoverableDraft(42, 'artist:7', { accessToken: 'synthetic' })).toBe(false);
  expect(saveRecoverableDraft(42, 'artist:7', { file: new File(['private bytes'], 'private.wav') })).toBe(false);
});
it('uses the existing logout/account-change purge registry', () => {
  saveRecoverableDraft(42, 'artist:7', { bio: 'private draft' });
  reconcileSessionPersonalData(42, 84);
  expect(readRecoverableDraft(42, 'artist:7', parse)).toBeUndefined();
});
