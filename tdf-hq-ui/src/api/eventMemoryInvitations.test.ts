import { jest } from '@jest/globals';
const get = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const put = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const post = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ get, put, post }));
const { EventMemoryInvitations: api } = await import('./eventMemoryInvitations');
const summary = { enabled: false, eventFinished: false, candidates: 0, consented: 0, invitations: 0,
  reminders: 0, opened: 0, published: 0, moments: 0, uniqueAuthors: 0 };
beforeEach(() => { jest.clearAllMocks(); });
it.each(['<html>not confirmation</html>', null, {}, { inApp: 'true', supportsExtendedMoments: true }])('does not cache or confirm a malformed preference: %p', async (value) => {
  get.mockResolvedValue(value); put.mockResolvedValue(value);
  await expect(api.preferences()).rejects.toThrow('No se pudo confirmar');
  await expect(api.setPreference(false)).rejects.toThrow('No se pudo confirmar');
});
it('does not confirm an opt-out when the reply still reports opt-in', async () => {
  put.mockResolvedValue({ inApp: true, supportsExtendedMoments: true });
  await expect(api.setPreference(false)).rejects.toThrow('No se pudo confirmar el cambio');
  put.mockResolvedValue({ inApp: false, supportsExtendedMoments: true });
  await expect(api.setPreference(false)).resolves.toEqual({ inApp: false, supportsExtendedMoments: true });
  expect(put).toHaveBeenLastCalledWith('/social-events/me/memory-invitations', false);
});
it('rejects malformed aggregates and a campaign acknowledgement that did not change its setting', async () => {
  get.mockResolvedValue({ ...summary, candidates: -1 });
  await expect(api.summary('141')).rejects.toThrow('No se pudieron confirmar');
  put.mockResolvedValue(summary);
  await expect(api.setCampaign('141', true)).rejects.toThrow('No se pudo confirmar el cambio');
  put.mockResolvedValue({ ...summary, enabled: true });
  await expect(api.setCampaign('141', true)).resolves.toEqual({ ...summary, enabled: true });
});
it('records attribution only from a structured acknowledgement without assuming a new opening', async () => {
  post.mockResolvedValue('<html>not attribution</html>');
  await expect(api.open('141', 'synthetic')).rejects.toThrow('No se pudo confirmar');
  post.mockResolvedValue({ recorded: false });
  await expect(api.open('141', 'synthetic')).resolves.toEqual({ recorded: false });
});
