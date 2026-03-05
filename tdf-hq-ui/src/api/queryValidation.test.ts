import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string) => Promise<unknown>>();
const postMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();
const patchMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();
const putMock = jest.fn<(path: string, body: unknown) => Promise<unknown>>();
const delMock = jest.fn<(path: string) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
  patch: patchMock,
  put: putMock,
  del: delMock,
}));

const { Payments } = await import('./payments');
const { Internships } = await import('./internships');
const { RadioAPI } = await import('./radio');
const { Trials } = await import('./trials');

describe('API query/id validation', () => {
  beforeEach(() => {
    getMock.mockReset();
    postMock.mockReset();
    patchMock.mockReset();
    putMock.mockReset();
    delMock.mockReset();
    getMock.mockResolvedValue([]);
    postMock.mockResolvedValue({});
    patchMock.mockResolvedValue({});
    putMock.mockResolvedValue({});
    delMock.mockResolvedValue(undefined);
  });

  it('keeps optional payment and internship filters explicit and validated', async () => {
    await Payments.list();
    expect(getMock).toHaveBeenCalledWith('/payments');

    await Payments.list(7);
    expect(getMock).toHaveBeenCalledWith('/payments?partyId=7');

    await Internships.listTimeEntries(null);
    expect(getMock).toHaveBeenCalledWith('/internships/time-entries');

    await Internships.listTimeEntries(9);
    expect(getMock).toHaveBeenCalledWith('/internships/time-entries?partyId=9');

    expect(() => Payments.list(0)).toThrow('partyId debe ser un entero positivo.');
    expect(() => Internships.listTimeEntries(-2)).toThrow('partyId debe ser un entero positivo.');
  });

  it('routes radio presence correctly and rejects invalid ids', async () => {
    await RadioAPI.getPresence();
    expect(getMock).toHaveBeenCalledWith('/radio/presence');

    await RadioAPI.getPresence(5);
    expect(getMock).toHaveBeenCalledWith('/radio/presence/5');

    expect(() => RadioAPI.getPresence(0)).toThrow('partyId debe ser un entero positivo.');
  });

  it('normalizes trial query params and rejects invalid numeric ids', async () => {
    await Trials.listSlots();
    expect(getMock).toHaveBeenCalledWith('/trials/v1/trial-slots');

    await Trials.listSlots(3);
    expect(getMock).toHaveBeenCalledWith('/trials/v1/trial-slots?subjectId=3');

    await Trials.listTeacherClasses(8, {
      subjectId: 2,
      from: ' 2026-03-01T00:00:00Z ',
      to: '   ',
    });
    expect(getMock).toHaveBeenCalledWith('/trials/v1/teachers/8/classes?subjectId=2&from=2026-03-01T00%3A00%3A00Z');

    await Trials.listClassSessions({
      subjectId: 6,
      teacherId: 7,
      studentId: 8,
      from: ' 2026-03-01T00:00:00Z ',
      to: '2026-03-10T23:59:00Z ',
      status: ' programada ',
    });
    expect(getMock).toHaveBeenCalledWith(
      '/trials/v1/class-sessions?subjectId=6&teacherId=7&studentId=8&from=2026-03-01T00%3A00%3A00Z&to=2026-03-10T23%3A59%3A00Z&status=programada',
    );

    expect(() => Trials.listSlots(0)).toThrow('subjectId debe ser un entero positivo.');
    expect(() => Trials.listTeacherClasses(0)).toThrow('teacherId debe ser un entero positivo.');
    expect(() => Trials.listClassSessions({ studentId: 0 })).toThrow('studentId debe ser un entero positivo.');
  });

  it('validates required trial payload ids before mutation requests', async () => {
    await Trials.createClassSession({
      studentId: 2,
      teacherId: 3,
      subjectId: 4,
      roomId: 5,
      startAt: '2026-03-01T10:00:00Z',
      endAt: '2026-03-01T11:00:00Z',
    });
    expect(postMock).toHaveBeenCalledWith('/trials/v1/class-sessions', {
      studentId: 2,
      teacherId: 3,
      subjectId: 4,
      roomId: 5,
      startAt: '2026-03-01T10:00:00Z',
      endAt: '2026-03-01T11:00:00Z',
    });

    await Trials.addTeacherStudent(7, { studentId: 9 });
    expect(postMock).toHaveBeenCalledWith('/trials/v1/teachers/7/students', { studentId: 9 });

    expect(() =>
      Trials.createClassSession({
        studentId: 0,
        teacherId: 3,
        subjectId: 4,
        roomId: 5,
        startAt: '2026-03-01T10:00:00Z',
        endAt: '2026-03-01T11:00:00Z',
      }),
    ).toThrow('studentId debe ser un entero positivo.');

    expect(() => Trials.addTeacherStudent(7, { studentId: 0 })).toThrow('studentId debe ser un entero positivo.');
  });
});
