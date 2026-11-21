// Calendar entries for a scheduled course follow-up. Both outputs are built in
// the browser: a Google Calendar "create event" link and an .ics file with an
// alarm that Apple Calendar, Outlook and Google Calendar import.

export interface FollowUpCalendarEntry {
  title: string;
  details: string;
  startAt: Date;
  durationMinutes?: number;
  alarmMinutesBefore?: number;
  url?: string;
  uid: string;
}

const DEFAULT_DURATION_MINUTES = 15;
const DEFAULT_ALARM_MINUTES_BEFORE = 10;

const pad = (value: number) => String(value).padStart(2, '0');

export const toCalendarUtcStamp = (date: Date) =>
  `${date.getUTCFullYear()}${pad(date.getUTCMonth() + 1)}${pad(date.getUTCDate())}T${pad(date.getUTCHours())}${pad(date.getUTCMinutes())}${pad(date.getUTCSeconds())}Z`;

const endOf = (entry: FollowUpCalendarEntry) =>
  new Date(entry.startAt.getTime() + (entry.durationMinutes ?? DEFAULT_DURATION_MINUTES) * 60_000);

const detailsWithUrl = (entry: FollowUpCalendarEntry) =>
  [entry.details.trim(), entry.url?.trim() ?? ''].filter((part) => part !== '').join('\n\n');

export const buildGoogleCalendarUrl = (entry: FollowUpCalendarEntry) => {
  const params = new URLSearchParams({
    action: 'TEMPLATE',
    text: entry.title,
    dates: `${toCalendarUtcStamp(entry.startAt)}/${toCalendarUtcStamp(endOf(entry))}`,
    details: detailsWithUrl(entry),
  });
  return `https://calendar.google.com/calendar/render?${params.toString()}`;
};

// RFC 5545 text escaping.
const escapeIcsText = (value: string) =>
  value
    .replace(/\\/g, '\\\\')
    .replace(/\r\n|\r|\n/g, '\\n')
    .replace(/;/g, '\\;')
    .replace(/,/g, '\\,');

// RFC 5545 lines are folded at 75 octets; folding at 73 characters keeps
// most multi-byte text under the limit.
const foldIcsLine = (line: string) => {
  const chars = Array.from(line);
  if (chars.length <= 73) return line;
  const parts: string[] = [];
  for (let index = 0; index < chars.length; index += 73) {
    parts.push(chars.slice(index, index + 73).join(''));
  }
  return parts.join('\r\n ');
};

export const buildFollowUpIcs = (entry: FollowUpCalendarEntry, now: Date = new Date()) => {
  const alarmMinutes = Math.max(0, Math.round(entry.alarmMinutesBefore ?? DEFAULT_ALARM_MINUTES_BEFORE));
  const lines = [
    'BEGIN:VCALENDAR',
    'VERSION:2.0',
    'PRODID:-//TDF Records//Seguimiento de inscripciones//ES',
    'CALSCALE:GREGORIAN',
    'METHOD:PUBLISH',
    'BEGIN:VEVENT',
    `UID:${entry.uid}`,
    `DTSTAMP:${toCalendarUtcStamp(now)}`,
    `DTSTART:${toCalendarUtcStamp(entry.startAt)}`,
    `DTEND:${toCalendarUtcStamp(endOf(entry))}`,
    `SUMMARY:${escapeIcsText(entry.title)}`,
    `DESCRIPTION:${escapeIcsText(detailsWithUrl(entry))}`,
    ...(entry.url ? [`URL:${entry.url}`] : []),
    'BEGIN:VALARM',
    'ACTION:DISPLAY',
    `DESCRIPTION:${escapeIcsText(entry.title)}`,
    `TRIGGER:-PT${alarmMinutes}M`,
    'END:VALARM',
    'END:VEVENT',
    'END:VCALENDAR',
  ];
  return `${lines.map(foldIcsLine).join('\r\n')}\r\n`;
};

export const downloadFollowUpIcs = (entry: FollowUpCalendarEntry, fileName: string) => {
  const blob = new Blob([buildFollowUpIcs(entry)], { type: 'text/calendar;charset=utf-8' });
  const href = URL.createObjectURL(blob);
  const link = document.createElement('a');
  link.href = href;
  link.download = fileName.endsWith('.ics') ? fileName : `${fileName}.ics`;
  document.body.appendChild(link);
  link.click();
  link.remove();
  globalThis.setTimeout(() => URL.revokeObjectURL(href), 0);
};
