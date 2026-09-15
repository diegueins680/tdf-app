import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string, init?: RequestInit) => Promise<unknown>>();
const postMock = jest.fn<(path: string, body: unknown, init?: RequestInit) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ get: getMock, post: postMock }));
const { EventOperations } = await import('./eventOperations');

const task = {
  eventId: 80, activityId: 8000, status: 'planned', version: 1,
  policy: { requiresAccountability: true, dependenciesGateCompletion: true, version: 1 },
  raci: [{ partyId: 1, role: 'accountable' }, { partyId: 3, role: 'responsible' }],
  accountabilityNeedsAttention: false,
};

describe('EventOperations task read contract', () => {
  beforeEach(() => { getMock.mockReset(); postMock.mockReset(); });

  it('uses only event/task captures and never caches the authenticated read', async () => {
    getMock.mockResolvedValue(task);
    await expect(EventOperations.task(80, 8000)).resolves.toEqual(task);
    expect(getMock).toHaveBeenCalledWith('/event-operations/events/80/tasks/8000', { cache: 'no-store' });
  });

  it('accepts optional policy and explicit attention without inventing readiness', async () => {
    const advisory = { ...task, policy: undefined, raci: [] };
    getMock.mockResolvedValue(advisory);
    await expect(EventOperations.task(80, 8000)).resolves.toEqual(advisory);
    const attention = { ...task, raci: [], accountabilityNeedsAttention: true };
    getMock.mockResolvedValue(attention);
    await expect(EventOperations.task(80, 8000)).resolves.toEqual(attention);
  });

  it('preserves the largest supported capture without rounding', async () => {
    const limit = Number.MAX_SAFE_INTEGER;
    const boundary = { ...task, eventId: limit, activityId: limit, version: limit };
    getMock.mockResolvedValue(boundary);
    await expect(EventOperations.task(limit, limit)).resolves.toEqual(boundary);
    expect(getMock).toHaveBeenCalledWith(
      '/event-operations/events/9007199254740991/tasks/9007199254740991', { cache: 'no-store' },
    );
  });

  it.each([0, -1, 1.5, NaN, Infinity, Number.MAX_SAFE_INTEGER + 1])(
    'rejects invalid capture %s before making any request', value => {
      expect(() => EventOperations.task(value, 8000)).toThrow('identificadores');
      expect(() => EventOperations.task(80, value)).toThrow('identificadores');
      expect(getMock).not.toHaveBeenCalled();
    },
  );

  const invalid = [
    undefined, null, '', 'private diagnostic', {},
    { ...task, eventId: 81 }, { ...task, activityId: 8100 },
    { ...task, secret: 'private' }, { ...task, status: 'invented' },
    { ...task, policy: null }, { ...task, policy: { ...task.policy, secret: 'private' } },
    { ...task, version: 0 }, { ...task, version: Number.MAX_SAFE_INTEGER + 1 },
    { ...task, raci: [...task.raci, task.raci[0]] },
    { ...task, raci: [{ partyId: 1, role: 'owner' }] },
    { ...task, raci: [{ partyId: Number.MAX_SAFE_INTEGER + 1, role: 'responsible' }] },
    { ...task, raci: [{ partyId: 1, role: 'accountable', email: 'private' }] },
    { ...task, accountabilityNeedsAttention: true },
  ];
  it.each(invalid.map((value, index) => [index, value] as const))(
    'rejects malformed successful response %s without exposing its contents', async (_index, value) => {
      getMock.mockResolvedValue(value);
      await expect(EventOperations.task(80, 8000)).rejects.toThrow(
        'La respuesta de la tarea no es válida. Vuelve a intentarlo.',
      );
    },
  );

  it('propagates transport failure rather than creating an empty task', async () => {
    const failure = new Error('not_found');
    getMock.mockRejectedValue(failure);
    await expect(EventOperations.task(80, 8000)).rejects.toBe(failure);
  });

  it('preserves the existing snapshot and idempotent transition contracts', async () => {
    getMock.mockResolvedValue({ eventId: 80 });
    postMock.mockResolvedValue({ version: 2 });
    await EventOperations.snapshot(80);
    const command = { expectedVersion: 1, targetState: 'planning' as const, correlationId: 'test' };
    await EventOperations.transition(80, 'command-key', command);
    expect(getMock).toHaveBeenCalledWith('/event-operations/events/80');
    expect(postMock).toHaveBeenCalledWith('/event-operations/events/80/transitions', command, {
      headers: { 'Idempotency-Key': 'command-key' },
    });
  });
});
