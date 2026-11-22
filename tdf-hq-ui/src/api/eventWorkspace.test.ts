import { jest } from '@jest/globals';
import { workspaceFixture } from './eventWorkspace.fixture';
const get = jest.fn<(path: string) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ get, post: jest.fn(), API_BASE_URL: 'https://api.example.test' }));
jest.unstable_mockModule('./authHeader', () => ({ buildAuthorizationHeader: () => 'Bearer synthetic' }));
const { confirmedWorkspace, EventWorkspaceAPI } = await import('./eventWorkspace');

it('accepts the versioned plan and sent proposal fixture', () => {
  expect(confirmedWorkspace(workspaceFixture()).agreements[0]?.current_version).toBe(1);
});
it.each(['money', 'plan', 'version', 'hash', 'approval', 'receipt'])('rejects malformed %s evidence before rendering', (kind) => {
  const value = workspaceFixture();
  const agreement = value.agreements[0]!; const version = agreement.versions[0]!;
  if (kind === 'money') value.plan!.content.budget[0]!.amountCents = Number.MAX_SAFE_INTEGER + 1;
  if (kind === 'plan') (value.plan!.content as unknown as Record<string, unknown>)['activities'] = null;
  if (kind === 'version') version.version = 2;
  if (kind === 'hash') version.document_hash = 'unconfirmed';
  if (kind === 'approval') version.approved_at = null;
  if (kind === 'receipt') agreement.state = 'accepted';
  expect(() => confirmedWorkspace(value)).toThrow();
});
it('rejects a response for another event', async () => {
  get.mockResolvedValueOnce(workspaceFixture());
  await expect(EventWorkspaceAPI.read('999')).rejects.toThrow('otro evento');
});
it('uses authenticated private downloads and rejects non-PDF bytes', async () => {
  const fetch = jest.fn<typeof globalThis.fetch>().mockResolvedValue({ ok: true, headers: new Headers({ 'content-type': 'application/pdf' }), blob: async () => ({ slice: () => ({ text: async () => '<html' }) }) } as unknown as Response);
  const previous = globalThis.fetch; globalThis.fetch = fetch;
  try {
    await expect(EventWorkspaceAPI.document('216', 'plan.pdf')).rejects.toThrow('confirmar el PDF');
    expect(fetch).toHaveBeenCalledWith('https://api.example.test/social-events/events/216/workspace/plan.pdf', expect.objectContaining({ credentials: 'include', cache: 'no-store', headers: { Authorization: 'Bearer synthetic' } }));
  } finally { globalThis.fetch = previous; }
});

it.each(['agreements/550e8400-e29b-41d4-a716-446655440000/1/document.pdf', 'agreements/550e8400-e29b-41d4-a716-446655440000/acceptance.pdf'])('downloads private agreement evidence without calling email send: %s', async (path) => {
  const blob = { slice: () => ({ text: async () => '%PDF-synthetic' }) } as unknown as Blob;
  const fetch = jest.fn<typeof globalThis.fetch>().mockResolvedValue({ ok: true, headers: new Headers({ 'content-type': 'application/pdf' }), blob: async () => blob } as unknown as Response);
  const previous = globalThis.fetch; globalThis.fetch = fetch;
  try {
    await expect(EventWorkspaceAPI.document('216', path)).resolves.toBe(blob);
    expect(fetch).toHaveBeenCalledTimes(1);
    expect(fetch).toHaveBeenCalledWith(`https://api.example.test/social-events/events/216/workspace/${path}`, expect.objectContaining({ credentials: 'include', cache: 'no-store', headers: { Authorization: 'Bearer synthetic' } }));
  } finally { globalThis.fetch = previous; }
});
