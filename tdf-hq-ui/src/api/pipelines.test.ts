import { jest } from '@jest/globals';

const delMock = jest.fn<(path: string) => Promise<unknown>>();
const getMock = jest.fn<(path: string) => Promise<unknown>>();
const patchMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();
const postMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  del: delMock,
  get: getMock,
  patch: patchMock,
  post: postMock,
}));

const { Pipelines } = await import('./pipelines');

const workflowId = '00000000-0000-4000-8000-000000000106';
const cardId = '30000000-0000-4000-8000-000000000001';
const serviceOfferingId = '10000000-0000-4000-8000-000000000001';
const workflowStateId = '00000000-0000-4000-8000-000000000251';

describe('Pipelines canonical API adapter', () => {
  beforeEach(() => {
    delMock.mockReset().mockResolvedValue(undefined);
    getMock.mockReset().mockResolvedValue({});
    patchMock.mockReset().mockResolvedValue({});
    postMock.mockReset().mockResolvedValue({});
  });

  it('uses the bounded snapshot and UUID-scoped definition/card routes', async () => {
    await Pipelines.snapshot();
    await Pipelines.definitions();
    await Pipelines.stages(` ${workflowId} `);
    await Pipelines.get(workflowId, ` ${cardId} `);

    expect(getMock).toHaveBeenNthCalledWith(1, '/pipelines/snapshot');
    expect(getMock).toHaveBeenNthCalledWith(2, '/pipelines/definitions');
    expect(getMock).toHaveBeenNthCalledWith(3, `/pipelines/${workflowId}/stages`);
    expect(getMock).toHaveBeenNthCalledWith(4, `/pipelines/${workflowId}/${cardId}`);
  });

  it('writes only canonical service and workflow-state UUID fields', async () => {
    await Pipelines.create(workflowId, {
      title: 'Single A',
      serviceOfferingId,
      workflowStateId,
    });
    await Pipelines.update(workflowId, cardId, { workflowStateId, sortOrder: 3 });

    expect(postMock).toHaveBeenCalledWith(`/pipelines/${workflowId}`, {
      title: 'Single A',
      serviceOfferingId,
      workflowStateId,
    });
    expect(patchMock).toHaveBeenCalledWith(`/pipelines/${workflowId}/${cardId}`, {
      workflowStateId,
      sortOrder: 3,
    });
    expect(JSON.stringify(postMock.mock.calls)).not.toMatch(/"(?:type|stage)"/);
    expect(JSON.stringify(patchMock.mock.calls)).not.toMatch(/"(?:type|stage)"/);
  });

  it('encodes identifiers and rejects blank relationships before issuing requests', async () => {
    await Pipelines.remove('workflow/one', 'card two');
    expect(delMock).toHaveBeenCalledWith('/pipelines/workflow%2Fone/card%20two');

    expect(() => Pipelines.list('   ')).toThrow('workflowId is required.');
    expect(() => Pipelines.update(workflowId, ' ', { workflowStateId })).toThrow('cardId is required.');
    expect(getMock).not.toHaveBeenCalled();
    expect(patchMock).not.toHaveBeenCalled();
  });
});
