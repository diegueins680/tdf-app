import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string, init?: RequestInit) => Promise<unknown>>();
const postMock = jest.fn<(path: string, body: unknown, init?: RequestInit) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ get: getMock, post: postMock }));
const { EventOperations } = await import('./eventOperations');
type Command = Parameters<typeof EventOperations.completeTask>[3];

const key = '60000000-0000-4000-8abc-000000000400';
const command = { expectedRevision: '4', reason: ' Preparación lista ', correlationId: ' test-completion ' };
const outcome = { eventId: 83, activityId: 8300, commandId: key, status: 'completed',
  activityVersion: 2, aggregateRevision: '5', replayed: false };
const responseError = 'La respuesta de finalización no es válida. Conserva la solicitud original para verificarla.';
beforeEach(() => { getMock.mockReset(); postMock.mockReset(); });

describe('completion client executable contract', () => {
  it('posts a fresh exact request and captures key, bearer and cancellation without caching', async () => {
    const signal = new AbortController().signal;
    postMock.mockResolvedValue(outcome);
    await expect(EventOperations.completeTask(83, 8300, key, command,
      { apiToken: 'captured-test-token', signal })).resolves.toEqual(outcome);
    expect(postMock).toHaveBeenCalledWith('/event-operations/events/83/tasks/8300/complete', command, {
      cache: 'no-store', headers: { 'Idempotency-Key': key, Authorization: 'Bearer captured-test-token' }, signal,
    });
    expect(postMock.mock.calls[0]?.[1]).not.toBe(command);
    expect(postMock).toHaveBeenCalledTimes(1);
    expect(getMock).not.toHaveBeenCalled();
  });

  it.each([false, true])('accepts original or historical receipt with replayed=%s', async replayed => {
    const result = { ...outcome, replayed, commandId: key.toUpperCase() };
    postMock.mockResolvedValue(result);
    await expect(EventOperations.completeTask(83, 8300, key, command)).resolves.toEqual(result);
    expect(postMock).toHaveBeenCalledTimes(1); expect(getMock).not.toHaveBeenCalled();
  });

  it('preserves maximal exact revision and Unicode code-point text limits', async () => {
    const body = { expectedRevision: '9223372036854775806', reason: '🎛'.repeat(2000), correlationId: '🎵'.repeat(200) };
    const result = { ...outcome, aggregateRevision: '9223372036854775807', activityVersion: 2147483647, replayed: true };
    postMock.mockResolvedValue(result);
    await expect(EventOperations.completeTask(83, 8300, key, body)).resolves.toEqual(result);
    expect(postMock.mock.calls[0]?.[1]).toEqual(body);
  });

  it.each([0, -1, 0.5, NaN, Infinity, Number.MAX_SAFE_INTEGER + 1])('rejects unsafe target %s before network', value => {
    expect(() => EventOperations.completeTask(value, 8300, key, command)).toThrow('identificadores');
    expect(() => EventOperations.completeTask(83, value, key, command)).toThrow('identificadores');
    expect(postMock).not.toHaveBeenCalled();
  });

  it.each(['', 'not-uuid', `${key}\n`, `${key}\r`, `${key} `, ` ${key}`])('rejects invalid key %j', value => {
    expect(() => EventOperations.completeTask(83, 8300, value, command)).toThrow('solicitud');
    expect(postMock).not.toHaveBeenCalled();
  });

  it.each([null, {}, { ...command, actorPartyId: 1 }, { ...command, override: true },
    { ...command, reason: null }, { ...command, reason: ' \t\n' }, { ...command, reason: '🎛'.repeat(2001) },
    { ...command, correlationId: null }, { ...command, correlationId: ' ' }, { ...command, correlationId: '🎵'.repeat(201) },
    ...[null, 4, '0', '-1', '04', '4\n', '4\r', '4 ', ' 4', '1.0', '1e1', '١', '9223372036854775808']
      .map(expectedRevision => ({ ...command, expectedRevision }))])('rejects strict input %# before dispatch', raw => {
    expect(() => EventOperations.completeTask(83, 8300, key, raw as Command)).toThrow('solicitud');
    expect(postMock).not.toHaveBeenCalled(); expect(getMock).not.toHaveBeenCalled();
  });

  it.each([undefined, null, {}, 'private response', { ...outcome, eventId: 84 }, { ...outcome, activityId: 8301 },
    { ...outcome, eventId: Number.MAX_SAFE_INTEGER + 1 }, { ...outcome, commandId: key.replace('400', '401') },
    { ...outcome, commandId: `${key}\n` }, { ...outcome, status: 'planned' }, { ...outcome, private: 'secret' },
    { ...outcome, replayed: null }, { ...outcome, replayed: 'false' },
    ...[0, 1, 2.5, 2147483648, NaN, Infinity, '2'].map(activityVersion => ({ ...outcome, activityVersion })),
    ...[null, 5, '4', '6', '05', '5\n', '9223372036854775808'].map(aggregateRevision => ({ ...outcome, aggregateRevision }))])(
    'rejects malformed or unbound receipt %# without exposing content or retrying', async raw => {
      postMock.mockResolvedValue(raw);
      await expect(EventOperations.completeTask(83, 8300, key, command)).rejects.toThrow(responseError);
      expect(postMock).toHaveBeenCalledTimes(1); expect(getMock).not.toHaveBeenCalled();
    });

  it('never accepts wraparound at maximal expected revision', async () => {
    postMock.mockResolvedValue({ ...outcome, aggregateRevision: '1' });
    await expect(EventOperations.completeTask(83, 8300, key,
      { ...command, expectedRevision: '9223372036854775807' })).rejects.toThrow(responseError);
  });

  it.each([false, true])('keeps receipt binding to the captured request after caller edit=%s', async edit => {
    let resolve!: (raw: unknown) => void;
    postMock.mockReturnValue(new Promise(done => { resolve = done; }));
    const mutable = { ...command };
    const context = { apiToken: 'original-token', signal: new AbortController().signal };
    const originalSignal = context.signal;
    const result = EventOperations.completeTask(83, 8300, key, mutable, context);
    if (edit) {
      mutable.expectedRevision = '5'; mutable.reason = 'Changed'; mutable.correlationId = 'Changed';
      context.apiToken = 'new-token'; context.signal = new AbortController().signal;
    }
    resolve(outcome);
    await expect(result).resolves.toEqual(outcome);
    expect(postMock.mock.calls[0]?.[1]).toEqual(command);
    expect(postMock.mock.calls[0]?.[2]).toEqual({ cache: 'no-store',
      headers: { 'Idempotency-Key': key, Authorization: 'Bearer original-token' }, signal: originalSignal });
  });

  it.each(['forbidden', 'version_conflict', 'dependencies_not_ready', 'event_operations_unavailable', 'AbortError'])(
    'propagates %s without reads, rebase, retry or fake success', async message => {
      const failure = new Error(message);
      postMock.mockRejectedValue(failure);
      await expect(EventOperations.completeTask(83, 8300, key, command)).rejects.toBe(failure);
      expect(postMock).toHaveBeenCalledTimes(1); expect(getMock).not.toHaveBeenCalled();
    });

  it('allows only caller-initiated exact replay after an ambiguous failure', async () => {
    postMock.mockRejectedValueOnce(new Error('network')).mockResolvedValueOnce({ ...outcome, replayed: true });
    await expect(EventOperations.completeTask(83, 8300, key, command)).rejects.toThrow('network');
    expect(postMock).toHaveBeenCalledTimes(1);
    await expect(EventOperations.completeTask(83, 8300, key, command)).resolves.toMatchObject({ replayed: true });
    expect(postMock.mock.calls[1]).toEqual(postMock.mock.calls[0]);
  });

  // Executable refinement of the model's Edit/Dispatch/Receive combinations.
  it.each([false, true].flatMap(edit => [false, true].flatMap(shape =>
    [false, true].map(bound => ({ edit, shape, bound })))))(
    'refines captured request/receipt model case %#', async ({ edit, shape, bound }) => {
      let resolve!: (raw: unknown) => void;
      postMock.mockReturnValue(new Promise(done => { resolve = done; }));
      const mutable = { ...command };
      const pending = EventOperations.completeTask(83, 8300, key, mutable);
      if (edit) mutable.expectedRevision = '5';
      resolve(shape ? { ...outcome, aggregateRevision: bound ? '5' : '6' } : null);
      if (shape && bound) await expect(pending).resolves.toEqual(outcome);
      else await expect(pending).rejects.toThrow(responseError);
      expect(postMock).toHaveBeenCalledTimes(1);
      expect(postMock.mock.calls[0]?.[1]).toEqual(command);
      expect(getMock).not.toHaveBeenCalled();
    });
});
