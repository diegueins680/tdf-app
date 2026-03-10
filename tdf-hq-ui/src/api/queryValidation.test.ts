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
const { Parties } = await import('./parties');
const { Internships } = await import('./internships');
const { RadioAPI } = await import('./radio');
const { Trials } = await import('./trials');
const { Bookings } = await import('./bookings');
const { ChatAPI } = await import('./chat');
const { Label } = await import('./label');
const { Courses } = await import('./courses');
const { SocialAPI } = await import('./social');

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

  it('rejects invalid ids before payment and party resource requests', async () => {
    await Payments.getOne(10);
    expect(getMock).toHaveBeenCalledWith('/payments/10');

    await Parties.getOne(7);
    expect(getMock).toHaveBeenCalledWith('/parties/7');

    await Parties.related(7);
    expect(getMock).toHaveBeenCalledWith('/parties/7/related');

    await Parties.update(8, { uDisplayName: 'Nombre actualizado' });
    expect(putMock).toHaveBeenCalledWith('/parties/8', { uDisplayName: 'Nombre actualizado' });

    await Parties.addRole(9, 'teacher');
    expect(postMock).toHaveBeenCalledWith('/parties/9/roles', 'teacher');

    expect(() => Payments.getOne(0)).toThrow('id debe ser un entero positivo.');
    expect(() => Parties.getOne(0)).toThrow('id debe ser un entero positivo.');
    expect(() => Parties.related(-3)).toThrow('id debe ser un entero positivo.');
    expect(() => Parties.update(0, {})).toThrow('id debe ser un entero positivo.');
    expect(() => Parties.addRole(0, 'teacher')).toThrow('id debe ser un entero positivo.');
  });

  it('validates social profile ids before requesting profile lookups', async () => {
    await SocialAPI.getProfile(7);
    expect(getMock).toHaveBeenCalledWith('/social/profiles/7');

    await SocialAPI.listProfiles([7, 9, 7]);
    expect(getMock).toHaveBeenCalledWith('/social/profiles?partyId=7&partyId=9');

    await expect(SocialAPI.listProfiles([])).resolves.toEqual([]);

    expect(() => SocialAPI.getProfile(0)).toThrow('Party ID inválido para obtener perfil social.');
    expect(() => SocialAPI.listProfiles([3, 0])).toThrow('Party ID inválido para listar perfiles sociales.');
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

  it('normalizes booking filters and validates booking ids', async () => {
    await Bookings.list();
    expect(getMock).toHaveBeenCalledWith('/bookings');

    await Bookings.list({ bookingId: 11, partyId: 22, engineerPartyId: 33 });
    expect(getMock).toHaveBeenCalledWith('/bookings?bookingId=11&partyId=22&engineerPartyId=33');

    await Bookings.update(4, { ubTitle: 'Sesion actualizada' });
    expect(putMock).toHaveBeenCalledWith('/bookings/4', { ubTitle: 'Sesion actualizada' });

    expect(() => Bookings.list({ bookingId: 0 })).toThrow('bookingId debe ser un entero positivo.');
    expect(() => Bookings.list({ partyId: -1 })).toThrow('partyId debe ser un entero positivo.');
    expect(() => Bookings.list({ engineerPartyId: 1.5 })).toThrow('engineerPartyId debe ser un entero positivo.');
    expect(() => Bookings.update(0, {})).toThrow('bookingId debe ser un entero positivo.');
  });

  it('validates optional booking payload party references', async () => {
    await Bookings.create({
      cbTitle: 'Grabacion de demo',
      cbStartsAt: '2026-03-01T10:00:00Z',
      cbEndsAt: '2026-03-01T12:00:00Z',
      cbStatus: 'Confirmed',
      cbPartyId: 7,
      cbEngineerPartyId: 9,
    });
    expect(postMock).toHaveBeenCalledWith('/bookings', {
      cbTitle: 'Grabacion de demo',
      cbStartsAt: '2026-03-01T10:00:00Z',
      cbEndsAt: '2026-03-01T12:00:00Z',
      cbStatus: 'Confirmed',
      cbPartyId: 7,
      cbEngineerPartyId: 9,
    });

    await Bookings.createPublic({
      pbFullName: 'Ana Perez',
      pbEmail: 'ana@example.com',
      pbServiceType: 'Mixing',
      pbStartsAt: '2026-03-01T10:00:00Z',
      pbEngineerPartyId: 10,
    });
    expect(postMock).toHaveBeenCalledWith('/bookings/public', {
      pbFullName: 'Ana Perez',
      pbEmail: 'ana@example.com',
      pbServiceType: 'Mixing',
      pbStartsAt: '2026-03-01T10:00:00Z',
      pbEngineerPartyId: 10,
    });

    expect(() =>
      Bookings.create({
        cbTitle: 'Grabacion de demo',
        cbStartsAt: '2026-03-01T10:00:00Z',
        cbEndsAt: '2026-03-01T12:00:00Z',
        cbStatus: 'Confirmed',
        cbPartyId: 0,
      }),
    ).toThrow('cbPartyId debe ser un entero positivo.');

    expect(() =>
      Bookings.createPublic({
        pbFullName: 'Ana Perez',
        pbEmail: 'ana@example.com',
        pbServiceType: 'Mixing',
        pbStartsAt: '2026-03-01T10:00:00Z',
        pbEngineerPartyId: -3,
      }),
    ).toThrow('pbEngineerPartyId debe ser un entero positivo.');
  });

  it('validates chat ids and sanitizes optional list query params', async () => {
    await ChatAPI.getOrCreateDmThread(12);
    expect(postMock).toHaveBeenCalledWith('/chat/threads/dm/12', {});

    await ChatAPI.listMessages(7, {
      limit: 20,
      beforeId: 30,
      afterId: 10,
    });
    expect(getMock).toHaveBeenCalledWith('/chat/threads/7/messages?limit=20&beforeId=30&afterId=10');

    await ChatAPI.listMessages(7, {
      limit: 0,
      beforeId: -1,
      afterId: Number.NaN,
    });
    expect(getMock).toHaveBeenCalledWith('/chat/threads/7/messages');

    await ChatAPI.sendMessage(7, '  hola equipo  ');
    expect(postMock).toHaveBeenCalledWith('/chat/threads/7/messages', { csmBody: 'hola equipo' });

    expect(() => ChatAPI.getOrCreateDmThread(0)).toThrow('otherPartyId debe ser un entero positivo.');
    expect(() => ChatAPI.listMessages(Number.NaN)).toThrow('threadId debe ser un entero positivo.');
    expect(() => ChatAPI.sendMessage(7, '   ')).toThrow('El mensaje no puede estar vacío.');
  });

  it('validates label owner ids and normalizes path track ids', async () => {
    await Label.listTracks();
    expect(getMock).toHaveBeenCalledWith('/label/tracks');

    await Label.listTracks(8);
    expect(getMock).toHaveBeenCalledWith('/label/tracks?ownerId=8');

    await Label.createTrack({ ltcTitle: 'Pendiente' }, 8);
    expect(postMock).toHaveBeenCalledWith('/label/tracks', {
      ltcTitle: 'Pendiente',
      ltcOwnerId: 8,
    });

    await Label.updateTrack(' track/alpha ', { ltuStatus: 'done' });
    expect(patchMock).toHaveBeenCalledWith('/label/tracks/track%2Falpha', { ltuStatus: 'done' });

    await Label.deleteTrack(' track alpha ');
    expect(delMock).toHaveBeenCalledWith('/label/tracks/track%20alpha');

    expect(() => Label.listTracks(0)).toThrow('ownerId debe ser un entero positivo.');
    expect(() => Label.createTrack({ ltcTitle: 'Pendiente' }, -2)).toThrow('ownerId debe ser un entero positivo.');
    expect(() => Label.updateTrack('   ', { ltuStatus: 'open' })).toThrow('id no puede estar vacío.');
    expect(() => Label.deleteTrack('')).toThrow('id no puede estar vacío.');
  });

  it('normalizes course registration params and validates registration ids', async () => {
    await Courses.listRegistrations({
      slug: ' cohort-1 ',
      status: ' active ',
      limit: 20,
    });
    expect(getMock).toHaveBeenCalledWith('/admin/courses/registrations?slug=cohort-1&status=active&limit=20');

    await Courses.listRegistrations({ slug: '   ', status: '   ' });
    expect(getMock).toHaveBeenCalledWith('/admin/courses/registrations');

    await Courses.listRegistrationEmails(6, 25);
    expect(getMock).toHaveBeenCalledWith('/admin/courses/registrations/6/emails?limit=25');

    await Courses.getRegistration('cohort-1', 8);
    expect(getMock).toHaveBeenCalledWith('/admin/courses/cohort-1/registrations/8');

    await Courses.updateStatus('cohort-1', 9, { status: 'confirmed' } as never);
    expect(patchMock).toHaveBeenCalledWith('/admin/courses/cohort-1/registrations/9/status', { status: 'confirmed' });

    expect(() => Courses.listRegistrations({ limit: 0 })).toThrow('limit debe ser un entero positivo.');
    expect(() => Courses.listRegistrationEmails(0)).toThrow('registrationId debe ser un entero positivo.');
    expect(() => Courses.listRegistrationEmails(6, 0)).toThrow('limit debe ser un entero positivo.');
    expect(() => Courses.getRegistration('cohort-1', 0)).toThrow('registrationId debe ser un entero positivo.');
    expect(() => Courses.updateStatus('cohort-1', 0, { status: 'confirmed' } as never)).toThrow(
      'registrationId debe ser un entero positivo.',
    );
  });

  it('normalizes and validates course slugs used in path segments', async () => {
    await Courses.getMetadata(' cohort 2026 ');
    expect(getMock).toHaveBeenCalledWith('/public/courses/cohort%202026');

    await Courses.register(
      ' cohort/2026 ',
      {
        fullName: 'Ana Perez',
        email: 'ana@example.com',
      } as never,
    );
    expect(postMock).toHaveBeenCalledWith(
      '/public/courses/cohort%2F2026/registrations',
      {
        fullName: 'Ana Perez',
        email: 'ana@example.com',
      },
    );

    expect(() => Courses.getMetadata('   ')).toThrow('slug no puede estar vacío.');
    expect(() => Courses.getRegistration('', 8)).toThrow('slug no puede estar vacío.');
  });
});
