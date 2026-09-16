import { get, post } from './client';
import { z } from 'zod';
import type { components } from './generated/types';

export type EventLifecycleState = components['schemas']['EventLifecycleState'];
export type EventOperationSnapshot = components['schemas']['EventOperationSnapshot'];
export type EventTransitionCommand = components['schemas']['EventTransitionCommand'];
export type EventTransitionOutcome = components['schemas']['EventTransitionOutcome'];
export type EventOperationTask = components['schemas']['EventOperationTask'];
export type EventOperationTaskWithRevision = components['schemas']['EventOperationTaskWithRevision'];
export type EventRaciReassignmentCommand = components['schemas']['EventRaciReassignmentCommand'];
export type EventRaciReassignmentOutcome = components['schemas']['EventRaciReassignmentOutcome'];
export type EventRaciEditorContext = components['schemas']['EventRaciEditorContext'];
export type EventTaskCompletionCommand = components['schemas']['EventTaskCompletionCommand'];
export type EventTaskCompletionOutcome = components['schemas']['EventTaskCompletionOutcome'];

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

const revisionSchema = z.string().regex(/^[1-9][0-9]{0,18}(?![\s\S])/)
  .refine(value => value.length < 19 || value <= '9223372036854775807');
const revisionedTaskSchema: z.ZodType<EventOperationTaskWithRevision> = z.object({
  task: taskSchema,
  aggregateRevision: revisionSchema,
}).strict();

const raciRoleSchema = z.enum(['responsible', 'accountable', 'consulted', 'informed']);
const commandKeySchema = z.string().regex(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}(?![\s\S])/i);
const commandText = (limit: number) => z.string()
  .refine(value => value.trim().length > 0 && Array.from(value).length <= limit);
const raciCommandSchema: z.ZodType<EventRaciReassignmentCommand> = z.object({
  expectedRevision: revisionSchema, role: raciRoleSchema,
  fromPartyId: safeInteger, toPartyId: safeInteger,
  reason: commandText(2000), correlationId: commandText(200),
}).strict().refine(value => value.fromPartyId !== value.toPartyId);
const completionCommandSchema: z.ZodType<EventTaskCompletionCommand> = z.object({
  expectedRevision: revisionSchema, reason: commandText(2000), correlationId: commandText(200),
}).strict();
const completionOutcomeSchema: z.ZodType<EventTaskCompletionOutcome> = z.object({
  eventId: safeInteger, activityId: safeInteger, commandId: commandKeySchema,
  status: z.literal('completed'), activityVersion: z.number().int().min(2).max(2147483647),
  aggregateRevision: revisionSchema, replayed: z.boolean(),
}).strict();
const raciOutcomeSchema: z.ZodType<EventRaciReassignmentOutcome> = z.object({
  eventId: safeInteger, activityId: safeInteger, commandId: commandKeySchema,
  role: raciRoleSchema, fromPartyId: safeInteger, toPartyId: safeInteger,
  aggregateRevision: revisionSchema, replayed: z.boolean(),
}).strict();

const raciEditorContextSchema: z.ZodType<EventRaciEditorContext> = z.object({
  eventId: safeInteger, activityId: safeInteger, aggregateRevision: revisionSchema,
  canManage: z.boolean(), operationReady: z.boolean(),
  replaceableAssignments: z.array(z.object({ partyId: safeInteger, role: raciRoleSchema }).strict()),
  eligiblePartyIds: z.array(safeInteger).max(100), nextAfterPartyId: safeInteger.optional(),
}).strict().superRefine((value, context) => {
  const ids = value.eligiblePartyIds;
  const uniqueSources = new Set(value.replaceableAssignments.map(row => `${row.partyId}:${row.role}`));
  if ((value.operationReady && !value.canManage)
    || (!value.operationReady && (ids.length > 0 || value.replaceableAssignments.length > 0
      || value.nextAfterPartyId !== undefined))
    || !ids.every((id, index) => index === 0 || id > (ids[index - 1] ?? 0))
    || uniqueSources.size !== value.replaceableAssignments.length
    || (value.nextAfterPartyId !== undefined && (ids.length !== 100 || ids[99] !== value.nextAfterPartyId))) {
    context.addIssue({ code: z.ZodIssueCode.custom, message: 'Contexto RACI inconsistente.' });
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
  completeTask: (eventId: number, activityId: number, commandId: string, command: EventTaskCompletionCommand,
    context?: { apiToken?: string; signal?: AbortSignal }): Promise<EventTaskCompletionOutcome> => {
    const path = `${taskPath(eventId, activityId)}/complete`;
    const request = completionCommandSchema.safeParse(command);
    if (!commandKeySchema.safeParse(commandId).success || !request.success) {
      throw new Error('La solicitud de finalización no es válida.');
    }
    // Zod's fresh scalar object keeps in-flight binding independent of caller edits.
    const captured = request.data;
    return post<unknown>(path, captured, {
      cache: 'no-store', headers: { 'Idempotency-Key': commandId,
        ...(context?.apiToken ? { Authorization: `Bearer ${context.apiToken}` } : {}) },
      ...(context?.signal ? { signal: context.signal } : {}),
    }).then(raw => {
      const result = completionOutcomeSchema.safeParse(raw);
      if (!result.success || result.data.eventId !== eventId || result.data.activityId !== activityId
        || result.data.commandId.toLowerCase() !== commandId.toLowerCase()
        || BigInt(result.data.aggregateRevision) !== BigInt(captured.expectedRevision) + 1n) {
        throw new Error('La respuesta de finalización no es válida. Conserva la solicitud original para verificarla.');
      }
      return result.data;
    });
  },
  raciEditorContext: (eventId: number, activityId: number, afterPartyId = 0,
    context?: { apiToken?: string; signal?: AbortSignal }): Promise<EventRaciEditorContext> => {
    const path = `${taskPath(eventId, activityId)}/raci/context`;
    if (!Number.isSafeInteger(afterPartyId) || afterPartyId < 0) {
      throw new Error('El cursor de destinatarios no es válido.');
    }
    return get<unknown>(`${path}?afterPartyId=${afterPartyId}`, {
      cache: 'no-store',
      ...(context?.apiToken ? { headers: { Authorization: `Bearer ${context.apiToken}` } } : {}),
      ...(context?.signal ? { signal: context.signal } : {}),
    }).then(raw => {
      const result = raciEditorContextSchema.safeParse(raw);
      if (!result.success || result.data.eventId !== eventId || result.data.activityId !== activityId
        || result.data.eligiblePartyIds.some(id => id <= afterPartyId)) {
        throw new Error('Las opciones de reasignación no son válidas. Actualiza la tarea.');
      }
      return result.data;
    });
  },
  reassignRaci: (eventId: number, activityId: number, commandId: string, command: EventRaciReassignmentCommand,
    context?: { apiToken?: string; signal?: AbortSignal }): Promise<EventRaciReassignmentOutcome> => {
    const path = `${taskPath(eventId, activityId)}/raci/reassign`;
    const request = raciCommandSchema.safeParse(command);
    if (!commandKeySchema.safeParse(commandId).success || !request.success) {
      throw new Error('La solicitud de reasignación RACI no es válida.');
    }
    // Capture validated scalar fields: caller mutation must not change response binding.
    const captured = request.data;
    return post<unknown>(path, captured, {
      cache: 'no-store', headers: { 'Idempotency-Key': commandId,
        ...(context?.apiToken ? { Authorization: `Bearer ${context.apiToken}` } : {}) },
      ...(context?.signal ? { signal: context.signal } : {}),
    }).then(raw => {
      const result = raciOutcomeSchema.safeParse(raw);
      if (!result.success || result.data.eventId !== eventId || result.data.activityId !== activityId
        || result.data.commandId.toLowerCase() !== commandId.toLowerCase()
        || result.data.role !== captured.role || result.data.fromPartyId !== captured.fromPartyId
        || result.data.toPartyId !== captured.toPartyId
        || BigInt(result.data.aggregateRevision) !== BigInt(captured.expectedRevision) + 2n) {
        throw new Error('La respuesta de reasignación no es válida. Conserva la solicitud original para verificarla.');
      }
      return result.data;
    });
  },
  taskWithRevision: (eventId: number, activityId: number, context?: { apiToken?: string; signal?: AbortSignal }): Promise<EventOperationTaskWithRevision> =>
    get<unknown>(`${taskPath(eventId, activityId)}/revisioned`, {
      cache: 'no-store',
      ...(context?.apiToken ? { headers: { Authorization: `Bearer ${context.apiToken}` } } : {}),
      ...(context?.signal ? { signal: context.signal } : {}),
    }).then(raw => {
      const result = revisionedTaskSchema.safeParse(raw);
      if (!result.success || result.data.task.eventId !== eventId || result.data.task.activityId !== activityId) {
        throw new Error('La respuesta de la tarea no es válida. Vuelve a intentarlo.');
      }
      return result.data;
    }),
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
