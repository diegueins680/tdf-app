import { jest } from '@jest/globals';
const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const commandMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ get: getMock }));
jest.unstable_mockModule('./interactions', () => ({ Interactions: { command: commandMock } }));
const { sourceReports, sourceCommand, validateSourceReportPage, validSourceControls, safeMemoryFileUrl } = await import('./eventMemoryModeration');
const target = '71000000-0000-4000-8000-000000000001';
const controls = { supported: true, eventId: '141', state: 'visible' as const, sourceRevision: '1790000000.123456', canReport: true, canHide: false, canRestore: false, canRemove: false };
const row = { id: target, entityKey: '81', eventId: '141', version: 1, source: controls, caption: 'Synthetic memory', mediaType: 'text', mediaUrl: '', author: null,
  openReports: 1, reportsReferToEarlierContent: false, reasons: ['Synthetic concern'], audit: [] };
const page = () => ({ supported: true, canReview: true, items: [row], nextCursor: null });
beforeEach(() => { jest.clearAllMocks(); });
it('scopes an opt-in report read to one event and carries cancellation', async () => {
  getMock.mockResolvedValue(page()); const controller = new AbortController();
  expect((await sourceReports('141', undefined, controller.signal)).items).toHaveLength(1);
  expect(getMock).toHaveBeenCalledWith('/interactions/reports?sourceEvent=141&limit=20', { signal: controller.signal });
});
it('discards the old comment inbox before caching when an old server ignores the opt-in', () => {
  expect(validateSourceReportPage({ items: [{ privateComment: 'not source data' }], nextCursor: null }, '141'))
    .toEqual({ supported: false, canReview: false, items: [], nextCursor: null });
});
it.each([403, 404])('treats missing or revoked review capability as unavailable without caching rows %s', async (status) => {
  getMock.mockRejectedValue(Object.assign(new Error('Unavailable'), { status }));
  await expect(sourceReports('141')).resolves.toEqual({ supported: false, canReview: false, items: [], nextCursor: null });
});
it.each([
  '<!doctype html><html>stale deployment</html>', {},
  { ...page(), canReview: false }, { ...page(), nextCursor: 'invalid' },
  { ...page(), items: [{ ...row, eventId: '142' }] },
  { ...page(), items: [{ ...row, source: { ...controls, eventId: '142' } }] },
  { ...page(), items: [{ ...row, source: { ...controls, canHide: 'true' } }] },
  { ...page(), items: [{ ...row, version: 0 }] },
])('fails closed on malformed, cross-event or unauthorized report rows %#', (value) => {
  expect(() => validateSourceReportPage(value, '141')).toThrow('Invalid event memory review response');
});
it('rejects invalid destinations before network access', async () => {
  await expect(sourceReports('../142')).rejects.toThrow();
  await expect(sourceReports('0141')).rejects.toThrow();
  await expect(sourceReports('141', 'foreign cursor')).rejects.toThrow();
  expect(getMock).not.toHaveBeenCalled();
});
it('does not coerce a malformed state object while rendering source capabilities', () => {
  expect(validSourceControls({ ...controls, state: { toString: 'visible', valueOf: null } })).toBe(false);
});
it('uses the canonical command transport and verifies a report acknowledgement', async () => {
  commandMock.mockResolvedValue({ reported: true });
  const command = { operation: 'source.report' as const, reason: 'Synthetic concern', expectedSourceRevision: controls.sourceRevision };
  await expect(sourceCommand(target, command, target)).resolves.toEqual({ reported: true });
  expect(commandMock).toHaveBeenCalledWith(target, command, target);
});
it.each(['<html>SPA</html>', {}, { state: 'visible', version: 2 }, { state: 'hidden', version: 0 }])('never claims hiding from a malformed/wrong acknowledgement %#', async (reply) => {
  commandMock.mockResolvedValue(reply);
  await expect(sourceCommand(target, { operation: 'source.hide', reason: 'Review', expectedVersion: 1, expectedSourceRevision: controls.sourceRevision }, target)).rejects.toThrow('Invalid source moderation acknowledgement');
});
it.each(['javascript:alert(1)', 'data:text/html,unsafe', 'http://example.test/a', 'https://user:password@example.test/a', '/relative'])('does not expose an unsafe file link %s', (value) => {
  expect(safeMemoryFileUrl(value)).toBeNull();
});
it('permits a credential-free HTTPS public file', () => {
  expect(safeMemoryFileUrl('https://assets.persona.test/memory.mp4')).toBe('https://assets.persona.test/memory.mp4');
});
