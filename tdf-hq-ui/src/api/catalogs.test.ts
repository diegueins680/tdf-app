import { jest } from '@jest/globals';

const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const postMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const postEmptyMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const postTextMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const patchMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
  postEmpty: postEmptyMock,
  postText: postTextMock,
  patch: patchMock,
}));

const { Catalogs } = await import('./catalogs');

describe('Catalog administration API', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('loads persisted content type schemas in the requested locale', async () => {
    getMock.mockResolvedValueOnce([]);

    await Catalogs.listContentTypes('es EC');

    expect(getMock).toHaveBeenCalledWith('/catalog/content-types?locale=es+EC');
  });

  it('batch-loads regional catalogs without duplicate requests', async () => {
    getMock.mockResolvedValueOnce({});

    await Catalogs.listPublicBatch(
      ['locales', 'currencies', 'countries', 'countries'],
      { locale: 'es', page: 1, pageSize: 500 },
    );

    expect(getMock).toHaveBeenCalledWith(
      '/catalogs/batch?code=locales&code=currencies&code=countries&locale=es&page=1&pageSize=500',
    );
  });

  it('loads canonical authored-content IDs and persisted URL metadata', async () => {
    getMock.mockResolvedValueOnce([]);

    await Catalogs.listAuthoredContents('en');

    expect(getMock).toHaveBeenCalledWith('/catalog/authored-contents?locale=en');
  });

  it('loads localized persisted workflow states without a frontend status registry', async () => {
    getMock.mockResolvedValueOnce([]);

    await Catalogs.listWorkflowStates('catalog-publication', 'es');

    expect(getMock).toHaveBeenCalledWith(
      '/catalog/workflow-states?workflowCode=catalog-publication&locale=es',
    );
  });

  it('loads a public workflow snapshot without using administrative catalog access', async () => {
    getMock.mockResolvedValueOnce({});

    await Catalogs.getPublicWorkflowStates('social event/lifecycle', 'en');

    expect(getMock).toHaveBeenCalledWith(
      '/catalogs/workflows/social%20event%2Flifecycle/states?locale=en',
    );
  });

  it('uses canonical IDs and encoded catalog codes for administrative item lookups', async () => {
    getMock.mockResolvedValueOnce({});

    await Catalogs.getItem('service categories', '6186dcca/unsafe', 'en');

    expect(getMock).toHaveBeenCalledWith(
      '/catalog/service%20categories/items/6186dcca%2Funsafe?locale=en',
    );
  });

  it('sends no legacy slug or string body when submitting a revision', async () => {
    postEmptyMock.mockResolvedValueOnce({});

    await Catalogs.submitRevision('revision/id');

    expect(postEmptyMock).toHaveBeenCalledWith('/catalog/revisions/revision%2Fid/submit');
    expect(postMock).not.toHaveBeenCalled();
  });

  it('marks CSV validation as a dry run by default', async () => {
    postTextMock.mockResolvedValueOnce({});

    await Catalogs.importCsv('genres', 'code,nameEs\nrock,Rock');

    expect(postTextMock).toHaveBeenCalledWith(
      '/catalog/genres/import.csv?dryRun=true',
      'code,nameEs\nrock,Rock',
    );
  });
});
