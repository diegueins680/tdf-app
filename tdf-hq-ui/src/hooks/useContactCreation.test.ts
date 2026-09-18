import { jest } from '@jest/globals';
const create = jest.fn<(body: unknown, key: string) => Promise<unknown>>();
jest.unstable_mockModule('../api/parties', () => ({ Parties: { create } }));
const { createContactCreation } = await import('./useContactCreation');
const body = { cDisplayName: 'Shared contact details', cIsOrg: false };

beforeEach(() => { create.mockReset(); });

it('reuses the request identity after a lost response or later form failure', async () => {
  const form = createContactCreation();
  create.mockRejectedValueOnce(new Error('response lost')).mockResolvedValue({ partyId: 1 });
  await expect(form.create(body)).rejects.toThrow('response lost');
  await form.create(body);
  await form.create(body);
  expect(new Set(create.mock.calls.map(call => call[1])).size).toBe(1);
});

it('keeps separate form entries and completed submissions separate even with identical fields', async () => {
  const form = createContactCreation();
  create.mockResolvedValue({ partyId: 1 });
  await form.create(body, 'musician-a');
  await form.create(body, 'musician-b');
  form.reset();
  await form.create(body, 'musician-a');
  expect(new Set(create.mock.calls.map(call => call[1])).size).toBe(3);
});

it('shares a key across simultaneous retries but never across different form instances', async () => {
  const first = createContactCreation(), second = createContactCreation();
  create.mockResolvedValue({ partyId: 1 });
  await Promise.all([first.create(body), first.create(body), second.create(body)]);
  expect(create.mock.calls[0]?.[1]).toBe(create.mock.calls[1]?.[1]);
  expect(create.mock.calls[0]?.[1]).not.toBe(create.mock.calls[2]?.[1]);
});
