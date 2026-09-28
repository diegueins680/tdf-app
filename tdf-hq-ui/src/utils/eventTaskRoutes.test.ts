import { eventTaskPath, parseEventTaskId } from './eventTaskRoutes';

describe('canonical task links', () => {
  it.each(['0', '01', '+1', '-1', '1e2', '1.0', ' 1', '1 ', '9007199254740992', '', null, undefined])(
    'rejects ambiguous or unsafe input %s', value => expect(parseEventTaskId(value)).toBeNull(),
  );
  it('preserves safe IDs exactly in the registered event route', () => {
    expect(eventTaskPath('80', '8000')).toBe('/social/eventos/80?tarea=8000');
    expect(eventTaskPath('9007199254740991', '9007199254740991'))
      .toBe('/social/eventos/9007199254740991?tarea=9007199254740991');
  });
  it('does not produce a dead link for legacy nonnumeric IDs', () => {
    expect(eventTaskPath('event-uuid', '8000')).toBeNull();
    expect(eventTaskPath('80', 'activity-uuid')).toBeNull();
  });
});
