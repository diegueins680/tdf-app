import { get } from './client';
import { Interactions } from './interactions';
import type { components } from './generated/types';

export type MemorySourceControls = components['schemas']['EventMemorySourceControls'];
export type MemorySourceReport = components['schemas']['EventMemorySourceReport'];
export type MemorySourceReportPage = components['schemas']['EventMemorySourceReportPage'];
export type MemorySourceCommand = Extract<components['schemas']['InteractionCommand'], {
  operation: 'source.report' | 'source.hide' | 'source.remove' | 'source.restore' | 'source.report.resolve';
}>;
const record = (value: unknown): value is Record<string, unknown> => Boolean(value) && typeof value === 'object' && !Array.isArray(value);
const uuid = (value: unknown) => typeof value === 'string' && /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(value);
export const validSourceControls = (value: unknown): value is MemorySourceControls => record(value)
  && value['supported'] === true && typeof value['eventId'] === 'string'
  && typeof value['sourceRevision'] === 'string' && value['sourceRevision'].length > 0 && value['sourceRevision'].length <= 64
  && typeof value['state'] === 'string' && ['visible', 'hidden', 'removed'].includes(value['state'])
  && typeof value['canReport'] === 'boolean' && typeof value['canHide'] === 'boolean'
  && typeof value['canRestore'] === 'boolean' && typeof value['canRemove'] === 'boolean';

/** Validate before caching: an old API may return the historical comment inbox. */
export function validateSourceReportPage(value: unknown, eventId: string): MemorySourceReportPage {
  if (record(value) && value['supported'] === undefined && Array.isArray(value['items'])) {
    return { supported: false, canReview: false, items: [], nextCursor: null };
  }
  if (!record(value) || value['supported'] !== true || typeof value['canReview'] !== 'boolean'
    || !Array.isArray(value['items']) || value['items'].length > 50 || (!value['canReview'] && value['items'].length > 0)
    || (value['nextCursor'] !== null && !uuid(value['nextCursor']))
    || value['items'].some((row: unknown) => !record(row) || !uuid(row['id']) || row['eventId'] !== eventId
      || typeof row['entityKey'] !== 'string' || !Number.isSafeInteger(row['version']) || Number(row['version']) < 1
      || !validSourceControls(row['source']) || row['source'].eventId !== eventId
      || (row['caption'] !== null && typeof row['caption'] !== 'string') || typeof row['mediaUrl'] !== 'string' || typeof row['mediaType'] !== 'string'
      || !Number.isSafeInteger(row['openReports']) || Number(row['openReports']) < 0 || typeof row['reportsReferToEarlierContent'] !== 'boolean'
      || !Array.isArray(row['reasons']) || row['reasons'].length > 20 || row['reasons'].some((reason: unknown) => typeof reason !== 'string' || reason.length > 1000)
      || !Array.isArray(row['audit']) || row['audit'].length > 5 || row['audit'].some((item: unknown) => !record(item)
        || typeof item['operation'] !== 'string' || typeof item['reason'] !== 'string' || typeof item['createdAt'] !== 'string'
        || (item['previousState'] !== null && typeof item['previousState'] !== 'string') || typeof item['newState'] !== 'string'))) {
    throw new Error('Invalid event memory review response');
  }
  return value as unknown as MemorySourceReportPage;
}

export async function sourceReports(eventId: string, cursor?: string, signal?: AbortSignal) {
  if (!/^[1-9][0-9]{0,17}$/.test(eventId) || (cursor !== undefined && !uuid(cursor))) throw new Error('Invalid memory review destination');
  const query = new URLSearchParams({ sourceEvent: eventId, limit: '20' });
  if (cursor) query.set('cursor', cursor);
  try {
    const value = await get<unknown>(`/interactions/reports?${query}`, { signal });
    return validateSourceReportPage(value, eventId);
  } catch (failure) {
    // Older deployments may reject the historical comment inbox. A revoked or
    // absent review scope also grants no capability, without retaining any rows.
    if (record(failure) && typeof failure['status'] === 'number' && [403, 404].includes(failure['status'])) {
      return { supported: false, canReview: false, items: [], nextCursor: null };
    }
    throw failure;
  }
}

/** A stale API's HTTP 200/HTML is never confirmation of a safety action. */
export async function sourceCommand(targetId: string, command: MemorySourceCommand, requestKey: string) {
  const result: unknown = await Interactions.command(targetId, command, requestKey);
  const expectedState = command.operation === 'source.hide' ? 'hidden' : command.operation === 'source.remove' ? 'removed'
    : command.operation === 'source.restore' ? 'visible' : null;
  if (!record(result) || (command.operation === 'source.report' ? result['reported'] !== true
    : !Number.isSafeInteger(result['version']) || Number(result['version']) < 1
      || (expectedState ? result['state'] !== expectedState : typeof result['state'] !== 'string' || !['visible', 'hidden', 'removed'].includes(result['state'])))) {
    throw new Error('Invalid source moderation acknowledgement');
  }
  return result;
}

export function safeMemoryFileUrl(value: string) {
  try {
    const url = new URL(value);
    return url.protocol === 'https:' && !url.username && !url.password ? url.href : null;
  } catch { return null; }
}
