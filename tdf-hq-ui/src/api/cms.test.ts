import { jest } from '@jest/globals';

const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const postMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const delMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
  del: delMock,
}));

const { Cms } = await import('./cms');

describe('Cms API', () => {
  beforeEach(() => {
    getMock.mockReset();
    postMock.mockReset();
    delMock.mockReset();
  });

  it('normalizes public CMS content returned with backend wire field names', async () => {
    getMock.mockResolvedValueOnce({
      id: 12,
      slug: 'records-sessions',
      locale: 'es',
      version: 3,
      status: 'published',
      title: 'TDF Live Sessions',
      payload: { videos: [{ title: 'Holger Quiñonez - TDF Live Sessions E05' }] },
      createdAt: '2026-04-26T13:03:37Z',
      publishedAt: '2026-04-26T13:04:01Z',
    });

    const content = await Cms.getPublic('records-sessions', 'es');

    expect(getMock).toHaveBeenCalledWith('/cms/content?slug=records-sessions&locale=es');
    expect(content).toEqual({
      ccdId: 12,
      ccdSlug: 'records-sessions',
      ccdLocale: 'es',
      ccdVersion: 3,
      ccdStatus: 'published',
      ccdTitle: 'TDF Live Sessions',
      ccdPayload: { videos: [{ title: 'Holger Quiñonez - TDF Live Sessions E05' }] },
      ccdCreatedAt: '2026-04-26T13:03:37Z',
      ccdPublishedAt: '2026-04-26T13:04:01Z',
    });
  });

  it('normalizes public CMS content lists', async () => {
    getMock.mockResolvedValueOnce([
      {
        id: 1,
        slug: 'records-release-1',
        locale: 'es',
        version: 1,
        status: 'published',
        title: 'Luna Baja',
        payload: { kind: 'release' },
        createdAt: '2026-01-01T00:00:00Z',
        publishedAt: null,
      },
    ]);

    const contents = await Cms.getPublicList({ locale: 'es', slugPrefix: 'records-release-' });

    expect(getMock).toHaveBeenCalledWith('/cms/contents?locale=es&slugPrefix=records-release-');
    expect(contents).toEqual([
      {
        ccdId: 1,
        ccdSlug: 'records-release-1',
        ccdLocale: 'es',
        ccdVersion: 1,
        ccdStatus: 'published',
        ccdTitle: 'Luna Baja',
        ccdPayload: { kind: 'release' },
        ccdCreatedAt: '2026-01-01T00:00:00Z',
        ccdPublishedAt: null,
      },
    ]);
  });

  it('sends admin create payloads with backend wire field names', async () => {
    postMock.mockResolvedValueOnce({
      id: 2,
      slug: 'records-sessions',
      locale: 'es',
      version: 1,
      status: 'draft',
      title: 'TDF Live Sessions',
      payload: { videos: [] },
      createdAt: '2026-04-26T13:03:37Z',
      publishedAt: null,
    });

    const content = await Cms.create({
      cciSlug: 'records-sessions',
      cciLocale: 'es',
      cciTitle: 'TDF Live Sessions',
      cciStatus: 'draft',
      cciPayload: { videos: [] },
    });

    expect(postMock).toHaveBeenCalledWith('/cms/admin/content', {
      slug: 'records-sessions',
      locale: 'es',
      title: 'TDF Live Sessions',
      status: 'draft',
      payload: { videos: [] },
    });
    expect(content.ccdPayload).toEqual({ videos: [] });
  });
});
