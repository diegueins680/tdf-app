import { get, post, API_BASE_URL } from './client';
import { buildAuthorizationHeader } from './authHeader';

export interface PlanActivity { title: string; start: string; end: string; responsible: string; notes: string }
export interface PlanBudgetLine { label: string; amountCents: number | null; basis: string }
export interface EventPlanContent {
  title: string; overview: string; activities: PlanActivity[]; budget: PlanBudgetLine[];
  budgetCapCents: number | null; currency: string; deliverables: string[]; pending: string[];
}
export interface EventPlanVersion {
  event_id: number; revision: number; recipient_party_id: number; content: EventPlanContent;
  document_hash: string; created_by: number; created_at: string;
}
export interface AgreementContent { title: string; body: string; recipientLegalName: string; pendingFields: string[] }
export interface AgreementVersion {
  version: number; content: AgreementContent & { plan: EventPlanVersion }; document_hash: string;
  approved_by: number | null; approved_at: string | null; sent_at: string | null; created_at: string;
}
export type AgreementState = 'draft' | 'sent' | 'changes_requested' | 'accepted' | 'rejected' | 'withdrawn';
export interface WorkspaceAction { id: number; action: string; version: number; actor_party_id: number; comment: string | null; created_at: string }
export interface EventAgreement {
  id: string; recipient_party_id: number; current_version: number; state: AgreementState; parent_id: string | null;
  versions: AgreementVersion[]; actions: WorkspaceAction[];
  acceptance: { version: number; accepted_by: number; accepted_name: string; accepted_at: string; document_hash: string } | null;
}
export interface EventWorkspace {
  eventId: string; owner: boolean; plan: EventPlanVersion | null; agreements: EventAgreement[]; planComments: WorkspaceAction[];
}
export interface WorkspaceCommand {
  requestId: string; action: string; version: number; agreementId?: string; hash?: string;
  recipientPartyId?: string; parentId?: string; content?: EventPlanContent | AgreementContent;
  comment?: string; acceptedName?: string; consent?: boolean;
}
const base = (eventId: string) => `/social-events/events/${encodeURIComponent(eventId)}/workspace`;
const record = (value: unknown): value is Record<string, unknown> => Boolean(value && typeof value === 'object' && !Array.isArray(value));
const integer = (value: unknown) => typeof value === 'number' && Number.isSafeInteger(value) && value > 0;
const amount = (value: unknown) => value === null || (typeof value === 'number' && Number.isSafeInteger(value) && value >= 0);
const strings = (value: unknown): value is string[] => Array.isArray(value) && value.every((item) => typeof item === 'string');
const hash = (value: unknown) => typeof value === 'string' && /^[a-f0-9]{64}$/.test(value);
const timestamp = (value: unknown) => typeof value === 'string' && Number.isFinite(Date.parse(value));
const optionalTime = (value: unknown) => value === null || timestamp(value);
const uuid = (value: unknown) => typeof value === 'string' && /^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$/.test(value);
const actions = (value: unknown) => Array.isArray(value) && value.every((item) => record(item) && integer(item['id']) && integer(item['version']) && integer(item['actor_party_id']) && typeof item['action'] === 'string' && (item['comment'] === null || typeof item['comment'] === 'string') && timestamp(item['created_at']));
function planVersion(value: unknown): boolean {
  if (!record(value) || !integer(value['revision']) || !integer(value['event_id']) || !integer(value['recipient_party_id']) || !integer(value['created_by']) || !hash(value['document_hash']) || !timestamp(value['created_at']) || !record(value['content'])) return false;
  const plan = value['content'];
  return typeof plan['title'] === 'string' && typeof plan['overview'] === 'string' && typeof plan['currency'] === 'string' && /^[A-Z]{3}$/.test(plan['currency'])
    && amount(plan['budgetCapCents']) && strings(plan['deliverables']) && strings(plan['pending'])
    && Array.isArray(plan['activities']) && plan['activities'].every((item) => record(item) && ['title', 'start', 'end', 'responsible', 'notes'].every((key) => typeof item[key] === 'string'))
    && Array.isArray(plan['budget']) && plan['budget'].every((item) => record(item) && typeof item['label'] === 'string' && typeof item['basis'] === 'string' && amount(item['amountCents']));
}
export function confirmedWorkspace(value: unknown): EventWorkspace {
  if (!record(value) || typeof value['owner'] !== 'boolean' || typeof value['eventId'] !== 'string'
    || !Array.isArray(value['agreements']) || !actions(value['planComments'])
    || !(value['plan'] === null || planVersion(value['plan']))) {
    throw new Error('No se pudo confirmar el estado del evento. Actualiza antes de continuar.');
  }
  for (const agreement of value['agreements']) {
    if (!record(agreement) || !uuid(agreement['id']) || !integer(agreement['current_version']) || !integer(agreement['recipient_party_id'])
      || !['draft', 'sent', 'changes_requested', 'accepted', 'rejected', 'withdrawn'].includes(String(agreement['state']))
      || !Array.isArray(agreement['versions']) || !actions(agreement['actions'])) throw new Error('La propuesta recibida no es válida. Actualiza el evento.');
    let previous = 0;
    for (const version of agreement['versions']) {
      if (!record(version) || !integer(version['version']) || Number(version['version']) <= previous || Number(version['version']) > Number(agreement['current_version']) || !hash(version['document_hash'])
        || !timestamp(version['created_at']) || !optionalTime(version['approved_at']) || !optionalTime(version['sent_at'])
        || !(version['approved_by'] === null || integer(version['approved_by'])) || (version['approved_at'] === null) !== (version['approved_by'] === null)
        || (version['sent_at'] !== null && version['approved_at'] === null) || !record(version['content']) || typeof version['content']['body'] !== 'string'
        || typeof version['content']['title'] !== 'string' || typeof version['content']['recipientLegalName'] !== 'string' || !strings(version['content']['pendingFields']) || !planVersion(version['content']['plan'])) throw new Error('Falta una versión verificable del acuerdo.');
      previous = Number(version['version']);
    }
    const acceptance = agreement['acceptance'];
    if ((agreement['state'] === 'accepted') !== (acceptance !== null)) throw new Error('El estado de aceptación requiere una constancia verificable.');
    if (acceptance !== null && (!record(acceptance) || !integer(acceptance['version']) || !integer(acceptance['accepted_by']) || typeof acceptance['accepted_name'] !== 'string'
      || !timestamp(acceptance['accepted_at']) || !hash(acceptance['document_hash']) || acceptance['accepted_by'] !== agreement['recipient_party_id']
      || !agreement['versions'].some((version: unknown) => record(version) && version['version'] === acceptance['version'] && version['document_hash'] === acceptance['document_hash']))) throw new Error('No se pudo verificar la constancia de aceptación.');
  }
  return value as unknown as EventWorkspace;
}
export const EventWorkspaceAPI = {
  read: async (eventId: string) => forEvent(eventId, await get<unknown>(base(eventId))),
  command: async (eventId: string, command: WorkspaceCommand) => forEvent(eventId, await post<unknown>(base(eventId), command)),
  document: async (eventId: string, path: string): Promise<Blob> => {
    const authorization = buildAuthorizationHeader();
    const response = await fetch(`${API_BASE_URL}${base(eventId)}/${path}`, { credentials: 'include', cache: 'no-store', headers: authorization ? { Authorization: authorization } : {} });
    if (!response.ok) throw new Error('No se pudo descargar el documento. Comprueba tu sesión y reintenta.');
    if (!(response.headers.get('content-type') ?? '').toLowerCase().includes('application/octet-stream')
      && !(response.headers.get('content-type') ?? '').toLowerCase().includes('application/pdf')) throw new Error('La descarga no contiene un PDF válido.');
    const blob = await response.blob();
    if (!(await blob.slice(0, 5).text()).startsWith('%PDF-')) throw new Error('No se pudo confirmar el PDF. Reintenta la descarga.');
    return blob;
  },
};
function forEvent(eventId: string, value: unknown): EventWorkspace {
  const result = confirmedWorkspace(value);
  if (result.eventId !== eventId) throw new Error('La respuesta corresponde a otro evento. Actualiza antes de continuar.');
  return result;
}
