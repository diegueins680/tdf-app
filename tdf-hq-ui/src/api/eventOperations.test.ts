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

describe('EventOperations RACI editor context contract', () => {
  const context = {
    eventId: 80, activityId: 8000, aggregateRevision: '4', canManage: true, operationReady: true,
    replaceableAssignments: task.raci, eligiblePartyIds: [1, 2, 3],
  };
  const page = { ...context, eligiblePartyIds: Array.from({ length: 100 }, (_, index) => index + 1),
    nextAfterPartyId: 100 };
  beforeEach(() => { getMock.mockReset(); postMock.mockReset(); });

  it('captures the bearer and signal on a single uncached read, without mutation', async () => {
    const signal = new AbortController().signal;
    getMock.mockResolvedValue(context);
    await expect(EventOperations.raciEditorContext(80, 8000, undefined,
      { apiToken: 'captured-context-test', signal })).resolves.toEqual(context);
    expect(getMock).toHaveBeenCalledWith('/event-operations/events/80/tasks/8000/raci/context?afterPartyId=0', {
      cache: 'no-store', headers: { Authorization: 'Bearer captured-context-test' }, signal,
    });
    expect(getMock).toHaveBeenCalledTimes(1);
    expect(postMock).not.toHaveBeenCalled();
  });

  it.each([false, true])('accepts empty non-ready context with canManage=%s', async canManage => {
    const result = { ...context, canManage, operationReady: false,
      replaceableAssignments: [], eligiblePartyIds: [] };
    getMock.mockResolvedValue(result);
    await expect(EventOperations.raciEditorContext(80, 8000)).resolves.toEqual(result);
  });

  it('preserves maximum revision text and requests additional pages only explicitly', async () => {
    const first = { ...page, aggregateRevision: '9223372036854775807' };
    getMock.mockResolvedValue(first);
    await expect(EventOperations.raciEditorContext(80, 8000)).resolves.toEqual(first);
    expect(getMock).toHaveBeenCalledTimes(1);
    const last = { ...context, aggregateRevision: '9223372036854775807', eligiblePartyIds: [101] };
    getMock.mockResolvedValue(last);
    await expect(EventOperations.raciEditorContext(80, 8000, 100)).resolves.toEqual(last);
    expect(getMock).toHaveBeenLastCalledWith(
      '/event-operations/events/80/tasks/8000/raci/context?afterPartyId=100', { cache: 'no-store' });
    expect(getMock).toHaveBeenCalledTimes(2);
    expect(postMock).not.toHaveBeenCalled();
  });

  it.each([-1, 0.5, NaN, Infinity, Number.MAX_SAFE_INTEGER + 1])(
    'rejects invalid cursor %s before dispatch', value => {
      expect(() => EventOperations.raciEditorContext(80, 8000, value)).toThrow('cursor');
      expect(getMock).not.toHaveBeenCalled();
    });
  it.each([0, -1, 0.5, NaN, Infinity, Number.MAX_SAFE_INTEGER + 1])(
    'rejects invalid target %s before dispatch', value => {
      expect(() => EventOperations.raciEditorContext(value, 8000)).toThrow('identificadores');
      expect(() => EventOperations.raciEditorContext(80, value)).toThrow('identificadores');
      expect(getMock).not.toHaveBeenCalled();
    });

  it.each([null, {}, { ...context, eventId: 81 }, { ...context, activityId: 8001 },
    { ...context, aggregateRevision: 4 }, { ...context, aggregateRevision: '04' },
    { ...context, aggregateRevision: '9223372036854775808' },
    { ...context, email: 'private-test-only' }, { ...context, canManage: false },
    { ...context, operationReady: false }, { ...context, canManage: 'true' },
    { ...context, replaceableAssignments: [...task.raci, task.raci[0]] },
    { ...context, replaceableAssignments: [{ partyId: 1, role: 'owner' }] },
    { ...context, replaceableAssignments: [{ partyId: 1, role: 'responsible', grant: 'private' }] },
    { ...context, replaceableAssignments: [{ partyId: Number.MAX_SAFE_INTEGER + 1, role: 'responsible' }] },
    { ...context, eligiblePartyIds: [1, 1] }, { ...context, eligiblePartyIds: [2, 1] },
    { ...context, eligiblePartyIds: [0] }, { ...context, eligiblePartyIds: [Number.MAX_SAFE_INTEGER + 1] },
    { ...context, eligiblePartyIds: Array.from({ length: 101 }, (_, index) => index + 1) },
    { ...context, nextAfterPartyId: null }, { ...context, nextAfterPartyId: 3 },
    { ...page, nextAfterPartyId: 99 }])(
    'rejects malformed or contradictory context with sanitized errors (%#)', async raw => {
      getMock.mockResolvedValue(raw);
      await expect(EventOperations.raciEditorContext(80, 8000)).rejects.toThrow(
        'Las opciones de reasignación no son válidas. Actualiza la tarea.');
      expect(getMock).toHaveBeenCalledTimes(1);
      expect(postMock).not.toHaveBeenCalled();
    });

  it('rejects candidates at or before the exclusive cursor', async () => {
    getMock.mockResolvedValue(context);
    await expect(EventOperations.raciEditorContext(80, 8000, 1)).rejects.toThrow('opciones');
    await expect(EventOperations.raciEditorContext(80, 8000, 2)).rejects.toThrow('opciones');
  });

  it('propagates transport failure without fallback or retry', async () => {
    const failure = new Error('not_found');
    getMock.mockRejectedValue(failure);
    await expect(EventOperations.raciEditorContext(80, 8000)).rejects.toBe(failure);
    expect(getMock).toHaveBeenCalledTimes(1);
    expect(postMock).not.toHaveBeenCalled();
  });
});

describe('EventOperations RACI command contract', () => {
  const key = '60000000-0000-4000-8000-000000000300';
  const command = { expectedRevision: '4', role: 'responsible' as const, fromPartyId: 3, toPartyId: 2,
    reason: 'Motivo sintético', correlationId: 'test-raci' };
  const outcome = { eventId: 82, activityId: 8200, commandId: key, role: 'responsible',
    fromPartyId: 3, toPartyId: 2, aggregateRevision: '6', replayed: false };
  beforeEach(() => { getMock.mockReset(); postMock.mockReset(); });

  it('posts the captured strict request with explicit key, bearer, cancellation and no cache', async () => {
    const controller = new AbortController();
    postMock.mockResolvedValue(outcome);
    await expect(EventOperations.reassignRaci(82, 8200, key, command,
      { apiToken: 'captured-token', signal: controller.signal })).resolves.toEqual(outcome);
    expect(postMock).toHaveBeenCalledWith('/event-operations/events/82/tasks/8200/raci/reassign', command,
      { cache: 'no-store', headers: { 'Idempotency-Key': key, Authorization: 'Bearer captured-token' },
        signal: controller.signal });
    expect(getMock).not.toHaveBeenCalled();
  });

  it('preserves exact BIGINT result and historical replay without a new read', async () => {
    const result = { ...outcome, aggregateRevision: '9223372036854775807', replayed: true };
    postMock.mockResolvedValue(result);
    await expect(EventOperations.reassignRaci(82, 8200, key,
      { ...command, expectedRevision: '9223372036854775805' })).resolves.toEqual(result);
    expect(postMock).toHaveBeenCalledTimes(1); expect(getMock).not.toHaveBeenCalled();
  });

  it('retains response binding when the caller mutates the original request in flight', async () => {
    let resolve!: (value: unknown) => void;
    postMock.mockReturnValue(new Promise(done => { resolve = done; }));
    const mutable = { ...command };
    const result = EventOperations.reassignRaci(82, 8200, key, mutable);
    mutable.toPartyId = 9; mutable.expectedRevision = '5';
    resolve(outcome);
    await expect(result).resolves.toEqual(outcome);
    expect(postMock.mock.calls[0]?.[1]).toEqual(command);
  });

  it.each([{}, { ...command, actorPartyId: 1 }, { ...command, expectedRevision: 4 },
    { ...command, expectedRevision: '04' }, { ...command, expectedRevision: '9223372036854775808' },
    { ...command, role: 'Responsible' }, { ...command, toPartyId: 3 }, { ...command, fromPartyId: 0 },
    { ...command, reason: '\t' }, { ...command, reason: 'x'.repeat(2001) },
    { ...command, correlationId: null }, { ...command, correlationId: 'x'.repeat(201) }])(
    'rejects invalid commands before dispatch (%#)', raw => {
      expect(() => EventOperations.reassignRaci(82, 8200, key, raw as typeof command)).toThrow('solicitud');
      expect(postMock).not.toHaveBeenCalled();
    });
  it.each(['', 'invalid', `${key}\n`])('rejects invalid UUID %s before dispatch', invalidKey => {
    expect(() => EventOperations.reassignRaci(82, 8200, invalidKey, command)).toThrow('solicitud');
    expect(postMock).not.toHaveBeenCalled();
  });
  it.each([null, {}, { ...outcome, eventId: 83 }, { ...outcome, activityId: 8201 },
    { ...outcome, commandId: '60000000-0000-4000-8000-000000000301' },
    { ...outcome, role: 'accountable' }, { ...outcome, fromPartyId: 2 }, { ...outcome, toPartyId: 3 },
    { ...outcome, aggregateRevision: '7' }, { ...outcome, aggregateRevision: 6 },
    { ...outcome, replayed: 'true' }, { ...outcome, secret: 'private' }, { error: 'forbidden' }])(
    'rejects malformed or unbound receipts without retrying (%#)', async raw => {
      postMock.mockResolvedValue(raw);
      await expect(EventOperations.reassignRaci(82, 8200, key, command)).rejects.toThrow('respuesta');
      expect(postMock).toHaveBeenCalledTimes(1);
    });
  it('propagates network failure without retry or generated replacement key', async () => {
    const error = new Error('network failure'); postMock.mockRejectedValue(error);
    await expect(EventOperations.reassignRaci(82, 8200, key, command)).rejects.toBe(error);
    expect(postMock).toHaveBeenCalledTimes(1);
  });
});

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

  it.each(['1', '9007199254740991', '9007199254740992', '9223372036854775807'])(
    'keeps revision %s as exact text with a captured bearer and cancellation signal', async aggregateRevision => {
      const envelope = { task, aggregateRevision };
      const signal = new AbortController().signal;
      getMock.mockResolvedValue(envelope);
      await expect(EventOperations.taskWithRevision(80, 8000, { apiToken: 'captured-test-only', signal }))
        .resolves.toEqual(envelope);
      expect(getMock).toHaveBeenCalledWith('/event-operations/events/80/tasks/8000/revisioned', {
        cache: 'no-store', headers: { Authorization: 'Bearer captured-test-only' }, signal,
      });
      expect(postMock).not.toHaveBeenCalled();
    },
  );

  it.each([undefined, null, 1, 0, true, '', '0', '01', '-1', '+1', ' 1', '1 ', '1\n', '1\r', '1e2',
    '1.0', '١', '１', '9223372036854775808', '10000000000000000000'])(
    'rejects invalid revision token %s without coercion', async aggregateRevision => {
      getMock.mockResolvedValue({ task, aggregateRevision });
      await expect(EventOperations.taskWithRevision(80, 8000)).rejects.toThrow('La respuesta de la tarea no es válida.');
    },
  );

  it.each(invalid.map((value, index) => [index, value] as const))(
    'retains all nested task restrictions in revision envelope %s', async (_index, value) => {
      getMock.mockResolvedValue({ task: value, aggregateRevision: '1' });
      await expect(EventOperations.taskWithRevision(80, 8000)).rejects.toThrow('La respuesta de la tarea no es válida.');
    },
  );

  it('requires the opt-in envelope, never falls back and never widens old task JSON', async () => {
    const envelope = { task, aggregateRevision: '1' };
    for (const raw of [task, null, {}, { ...envelope, private: 'secret' }]) {
      getMock.mockResolvedValue(raw);
      await expect(EventOperations.taskWithRevision(80, 8000)).rejects.toThrow('La respuesta de la tarea no es válida.');
    }
    getMock.mockResolvedValue({ ...task, aggregateRevision: '1' });
    await expect(EventOperations.task(80, 8000)).rejects.toThrow('La respuesta de la tarea no es válida.');
    const failure = new Error('not_found');
    getMock.mockRejectedValue(failure);
    await expect(EventOperations.taskWithRevision(80, 8000)).rejects.toBe(failure);
    expect(getMock).toHaveBeenCalledTimes(6);
  });

  it.each([0, -1, 1.5, NaN, Infinity, Number.MAX_SAFE_INTEGER + 1])(
    'rejects unsafe revisioned-read capture %s before network access', value => {
      expect(() => EventOperations.taskWithRevision(value, 8000)).toThrow('identificadores');
      expect(() => EventOperations.taskWithRevision(80, value)).toThrow('identificadores');
      expect(getMock).not.toHaveBeenCalled();
    },
  );

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
