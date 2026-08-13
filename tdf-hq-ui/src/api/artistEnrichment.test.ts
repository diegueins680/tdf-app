import { jest } from '@jest/globals';

const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const postMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const patchMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
  patch: patchMock,
}));

const { ArtistEnrichment } = await import('./artistEnrichment');

describe('ArtistEnrichment API', () => {
  beforeEach(() => {
    getMock.mockReset();
    postMock.mockReset();
    patchMock.mockReset();
  });

  it('builds encoded overview filters deterministically', async () => {
    getMock.mockResolvedValueOnce({});
    await ArtistEnrichment.overview('pending review', 42);
    expect(getMock).toHaveBeenCalledWith(
      '/admin/artists/enrichment/overview?status=pending+review&artistId=42',
    );
  });

  it('keeps full-platform and artist-specific dry runs separate', async () => {
    postMock.mockResolvedValue({});
    const request = { aerrMode: 'dry_run' as const, aerrBatchSize: 25 };
    await ArtistEnrichment.run(request);
    await ArtistEnrichment.rerunArtist(42, { ...request, aerrArtistId: 42 });
    expect(postMock).toHaveBeenNthCalledWith(1, '/admin/artists/enrichment/runs', request);
    expect(postMock).toHaveBeenNthCalledWith(
      2,
      '/admin/artists/enrichment/artists/42/rerun',
      { ...request, aerrArtistId: 42 },
    );
  });

  it('serializes idempotent field and identity decisions', async () => {
    patchMock.mockResolvedValue({});
    await ArtistEnrichment.decideSuggestion(7, {
      aedDecision: 'approve',
      aedEditedValue: 'Nombre oficial',
      aedNote: 'Dos fuentes oficiales',
    });
    await ArtistEnrichment.decideIdentity(9, { aedDecision: 'reject' });
    expect(patchMock).toHaveBeenNthCalledWith(
      1,
      '/admin/artists/enrichment/suggestions/7',
      {
        aedDecision: 'approve',
        aedEditedValue: 'Nombre oficial',
        aedNote: 'Dos fuentes oficiales',
      },
    );
    expect(patchMock).toHaveBeenNthCalledWith(
      2,
      '/admin/artists/enrichment/identity-candidates/9',
      { aedDecision: 'reject' },
    );
  });
});
