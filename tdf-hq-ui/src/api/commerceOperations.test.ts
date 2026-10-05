import { jest } from '@jest/globals';

const getMock = jest.fn();
const postMock = jest.fn();
jest.unstable_mockModule('./client', () => ({ get: getMock, post: postMock }));
const { CommerceOperations } = await import('./commerceOperations');

beforeEach(() => { getMock.mockReset(); postMock.mockReset(); });

it('reads reconciliation evidence in sandbox without a mutation', async () => {
  await CommerceOperations.listReconciliationExceptions();
  expect(getMock).toHaveBeenCalledWith('/admin/commerce/reconciliation-exceptions?environment=sandbox');
  expect(postMock).not.toHaveBeenCalled();
});

it('encodes reconciliation filters and preserves invalid values for backend validation', async () => {
  await CommerceOperations.listReconciliationExceptions({ environment: 'production', status: 'open', checkoutId: 'not a uuid&offset=10001', limit: 0, offset: 0 });
  expect(getMock).toHaveBeenCalledWith('/admin/commerce/reconciliation-exceptions?environment=production&status=open&checkoutId=not+a+uuid%26offset%3D10001&limit=0&offset=0');
  expect(postMock).not.toHaveBeenCalled();
});

it('reads the sandbox query report by default without issuing a mutation', async () => {
  await CommerceOperations.listProviderQueries();
  expect(getMock).toHaveBeenCalledWith('/admin/commerce/provider-queries?environment=sandbox');
  expect(postMock).not.toHaveBeenCalled();
});

it('preserves explicit pagination values for server validation', async () => {
  await CommerceOperations.listProviderQueries({ environment: 'production', status: 'retry', limit: 0, offset: 0 });
  expect(getMock).toHaveBeenCalledWith('/admin/commerce/provider-queries?environment=production&status=retry&limit=0&offset=0');
  expect(postMock).not.toHaveBeenCalled();
});
