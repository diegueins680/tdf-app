import { get, post } from './client';
import { z } from 'zod';
import type { components } from './generated/types';

export type EventLifecycleState = components['schemas']['EventLifecycleState'];
export type EventOperationSnapshot = components['schemas']['EventOperationSnapshot'];
export type EventTransitionCommand = components['schemas']['EventTransitionCommand'];
export type EventTransitionOutcome = components['schemas']['EventTransitionOutcome'];
export type EventOperationTask = components['schemas']['EventOperationTask'];

const safeInteger = z.number().int().positive().max(Number.MAX_SAFE_INTEGER);
const taskSchema: z.ZodType<EventOperationTask> = z.object({
  eventId: safeInteger,
  activityId: safeInteger,
  status: z.enum(['planned', 'confirmed', 'in_progress', 'completed', 'cancelled']),
  version: safeInteger,
  policy: z.object({
    requiresAccountability: z.boolean(),
    dependenciesGateCompletion: z.boolean(),
    version: safeInteger,
  }).strict().optional(),
  raci: z.array(z.object({
    partyId: safeInteger,
    role: z.enum(['responsible', 'accountable', 'consulted', 'informed']),
  }).strict()),
  accountabilityNeedsAttention: z.boolean(),
}).strict().superRefine((task, context) => {
  const assignments = new Set(task.raci.map(({ partyId, role }) => `${partyId}:${role}`));
  const needsAttention = (task.policy?.requiresAccountability ?? false)
    && (task.raci.filter(({ role }) => role === 'accountable').length !== 1
      || !task.raci.some(({ role }) => role === 'responsible'));
  if (assignments.size !== task.raci.length || needsAttention !== task.accountabilityNeedsAttention) {
    context.addIssue({ code: z.ZodIssueCode.custom, message: 'Proyección RACI inconsistente.' });
  }
});

const taskPath = (eventId: number, activityId: number) => {
  if (![eventId, activityId].every(value => Number.isSafeInteger(value) && value > 0)) {
    throw new Error('Los identificadores de evento y tarea deben ser enteros positivos seguros.');
  }
  return `${eventPath(eventId)}/tasks/${activityId}`;
};

const eventPath = (eventId: number) =>
  `/event-operations/events/${encodeURIComponent(String(eventId))}`;

export const EventOperations = {
  task: (eventId: number, activityId: number, context?: { apiToken?: string; signal?: AbortSignal }): Promise<EventOperationTask> =>
    get<unknown>(taskPath(eventId, activityId), {
      cache: 'no-store',
      ...(context?.apiToken ? { headers: { Authorization: `Bearer ${context.apiToken}` } } : {}),
      ...(context?.signal ? { signal: context.signal } : {}),
    }).then(raw => {
      const result = taskSchema.safeParse(raw);
      if (!result.success || result.data.eventId !== eventId || result.data.activityId !== activityId) {
        // Do not expose malformed server values or decoder diagnostics to logs/UI.
        throw new Error('La respuesta de la tarea no es válida. Vuelve a intentarlo.');
      }
      return result.data;
    }),
  snapshot: (eventId: number) =>
    get<EventOperationSnapshot>(eventPath(eventId)),
  transition: (eventId: number, commandId: string, command: EventTransitionCommand) =>
    post<EventTransitionOutcome>(`${eventPath(eventId)}/transitions`, command, {
      headers: { 'Idempotency-Key': commandId },
    }),
};
