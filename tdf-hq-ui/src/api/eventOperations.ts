import { get, post } from './client';
import type { components } from './generated/types';

export type EventLifecycleState = components['schemas']['EventLifecycleState'];
export type EventOperationSnapshot = components['schemas']['EventOperationSnapshot'];
export type EventTransitionCommand = components['schemas']['EventTransitionCommand'];
export type EventTransitionOutcome = components['schemas']['EventTransitionOutcome'];

const eventPath = (eventId: number) =>
  `/event-operations/events/${encodeURIComponent(String(eventId))}`;

export const EventOperations = {
  snapshot: (eventId: number) =>
    get<EventOperationSnapshot>(eventPath(eventId)),
  transition: (eventId: number, commandId: string, command: EventTransitionCommand) =>
    post<EventTransitionOutcome>(`${eventPath(eventId)}/transitions`, command, {
      headers: { 'Idempotency-Key': commandId },
    }),
};
