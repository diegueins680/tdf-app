import { get } from './client';
import type { operations } from './generated/types';

export type AttendeeStatus = operations['listEventAttendees']['parameters']['query']['status'];
type AttendeePage = operations['listEventAttendees']['responses'][200]['content']['application/json'];

export function listAttendees(eventId: string, status: AttendeeStatus, after?: string) {
  const params = new URLSearchParams({ status });
  if (after) params.set('after', after);
  return get<AttendeePage>(`/social-events/events/${encodeURIComponent(eventId)}/attendees?${params.toString()}`);
}
