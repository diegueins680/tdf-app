import { jest } from '@jest/globals';
import type { SessionResponseDTO } from './session';

const snapshot = jest.fn<() => Promise<SessionResponseDTO | null>>();
const submit = jest.fn<(...args: unknown[]) => Promise<void>>();
jest.unstable_mockModule('./session', () => ({ loadSessionSnapshot: snapshot }));
jest.unstable_mockModule('./feedback', () => ({ submitFeedback: submit }));
const { requestAccountDeletion } = await import('./accountDeletion');
const input = { partyId: 42, categoryId: 'category', severityId: 'severity', locale: 'es-EC' };
beforeEach(() => { snapshot.mockReset(); submit.mockReset(); submit.mockResolvedValue(); });
it.each([null, { partyId: 43, username: 'other@example.com' }])('refuses an expired or changed account before sending: %j', async current => {
  snapshot.mockResolvedValue(current as SessionResponseDTO | null);
  await expect(requestAccountDeletion(input)).rejects.toThrow('authenticate again');
  expect(submit).not.toHaveBeenCalled();
});
it('fails closed when live identity verification fails', async () => {
  snapshot.mockRejectedValue(new Error('offline'));
  await expect(requestAccountDeletion(input)).rejects.toThrow();
  expect(submit).not.toHaveBeenCalled();
});
it('requests whole-account manual deletion for the verified owner, with cookie authentication only', async () => {
  snapshot.mockResolvedValue({ partyId: 42, username: 'owner@example.com', apiToken: 'never-send-this' } as unknown as SessionResponseDTO);
  await requestAccountDeletion(input);
  expect(submit).toHaveBeenCalledTimes(1);
  expect(submit).toHaveBeenCalledWith(expect.objectContaining({
    consent: true, categoryId: 'category', severityId: 'severity', contactEmail: 'owner@example.com',
    description: expect.stringContaining('requested_account_party_id: 42'),
  }), { sessionCookieOnly: true, accountDeletionPartyId: 42 });
  const payload = submit.mock.calls[0]?.[0] as { description: string };
  expect(payload.description).toContain('entire account');
  expect(payload.description).toContain('feedbackCreatedBy');
  expect(JSON.stringify(submit.mock.calls)).not.toContain('never-send-this');
});
it('does not invent an email for username-only accounts and propagates submission failure', async () => {
  snapshot.mockResolvedValue({ partyId: 42, username: 'account-name' } as SessionResponseDTO);
  submit.mockRejectedValue(new Error('unavailable'));
  await expect(requestAccountDeletion(input)).rejects.toThrow('unavailable');
  expect(submit).toHaveBeenCalledWith(expect.objectContaining({ contactEmail: undefined }), { sessionCookieOnly: true, accountDeletionPartyId: 42 });
});
