export function parseEventTaskId(value: string | null | undefined): number | null {
  if (!value || !/^[1-9][0-9]*$/.test(value)) return null;
  const id = Number(value);
  return Number.isSafeInteger(id) ? id : null;
}

export function eventTaskPath(eventId: string, activityId: string): string | null {
  if (parseEventTaskId(eventId) === null || parseEventTaskId(activityId) === null) return null;
  return `/social/eventos/${eventId}?tarea=${activityId}`;
}
