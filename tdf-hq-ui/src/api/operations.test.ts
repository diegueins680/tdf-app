import { jest } from '@jest/globals';

const get = jest.fn<(path: string) => Promise<unknown>>();
const patch = jest.fn<(path: string, body: unknown) => Promise<unknown>>();
const post = jest.fn<(path: string, body: unknown) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({ get, patch, post }));

const { Operations } = await import('./operations');

describe('Operations API client', () => {
  beforeEach(() => {
    get.mockReset().mockResolvedValue({ items: [], hasMore: false });
    patch.mockReset().mockResolvedValue({});
    post.mockReset().mockResolvedValue({});
  });

  it('serializes every server-side filter without mock fallback data', async () => {
    await Operations.list({
      q: 'transferencia', status: 'waiting', priority: 'urgent', slaState: 'breached',
      seen: false, assigneePartyId: 42, customerPartyId: 9, sourceChannel: 'whatsapp',
      minAmountMinor: 1000, maxAmountMinor: 50000,
    });

    const path = get.mock.calls[0]?.[0] ?? '';
    expect(path).toContain('/operations/work-items?');
    expect(path).toContain('q=transferencia');
    expect(path).toContain('status=waiting');
    expect(path).toContain('seen=false');
    expect(path).toContain('sourceChannel=whatsapp');
  });

  it('sends lifecycle transitions as separate optimistic commands', async () => {
    await Operations.transition({ id: 'item-1', version: 7 } as never, 'resolved', 'Investigated');

    expect(patch).toHaveBeenCalledWith(
      '/operations/work-items/item-1/transition',
      expect.objectContaining({
        expectedVersion: 7,
        targetStatus: 'resolved',
        reason: 'Investigated',
        sourceClient: 'tdf-hq-ui',
      }),
    );
    expect(patch.mock.calls[0]?.[0]).not.toMatch(/invoice|payment|booking/);
  });
});
