import { buildFollowUpIcs, buildGoogleCalendarUrl, toCalendarUtcStamp, type FollowUpCalendarEntry } from './followUpCalendar';

const entry: FollowUpCalendarEntry = {
  title: 'Seguimiento: Diego Saa · Llamar, confirmar; pago',
  details: 'test@test.com\n\nDice que le llamemos en un mes.',
  startAt: new Date('2026-12-20T15:30:00Z'),
  url: 'https://tdfrecords.net/configuracion/inscripciones-curso?registration=12',
  uid: 'course-follow-up-7@tdfrecords.net',
};

describe('follow-up calendar entries', () => {
  it('formats UTC calendar stamps', () => {
    expect(toCalendarUtcStamp(new Date('2026-01-02T03:04:05Z'))).toBe('20260102T030405Z');
  });

  it('builds a Google Calendar create-event link with a 15 minute slot and the dossier link', () => {
    const url = new URL(buildGoogleCalendarUrl(entry));
    expect(url.origin + url.pathname).toBe('https://calendar.google.com/calendar/render');
    expect(url.searchParams.get('action')).toBe('TEMPLATE');
    expect(url.searchParams.get('text')).toBe(entry.title);
    expect(url.searchParams.get('dates')).toBe('20261220T153000Z/20261220T154500Z');
    expect(url.searchParams.get('details')).toContain('registration=12');
  });

  it('builds an escaped .ics event with a display alarm before the follow-up', () => {
    const ics = buildFollowUpIcs(entry, new Date('2026-11-20T00:00:00Z'));
    const unfolded = ics.replace(/\r\n /g, '');
    expect(ics.endsWith('\r\n')).toBe(true);
    expect(unfolded).toContain('DTSTART:20261220T153000Z');
    expect(unfolded).toContain('DTEND:20261220T154500Z');
    expect(unfolded).toContain('SUMMARY:Seguimiento: Diego Saa · Llamar\\, confirmar\\; pago');
    expect(unfolded).toContain('DESCRIPTION:test@test.com\\n\\nDice que le llamemos en un mes.');
    expect(unfolded).toContain('BEGIN:VALARM');
    expect(unfolded).toContain('TRIGGER:-PT10M');
    expect(ics.split('\r\n').every((line) => Array.from(line).length <= 74)).toBe(true);
  });
});
