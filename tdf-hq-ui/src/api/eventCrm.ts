import { z } from 'zod';
import { get, post, postEmpty } from './client';

export const crmStages = ['interested', 'contacted', 'reserved', 'paid', 'closed_lost'] as const;
export type CrmStage = typeof crmStages[number];
export const stageLabels: Record<CrmStage, string> = {
  interested: 'Interesado', contacted: 'Contactado', reserved: 'Reservado', paid: 'Pagado', closed_lost: 'Cerrado sin venta',
};
const id = z.number().int().positive().max(Number.MAX_SAFE_INTEGER);
const leadSchema = z.object({
  id: z.string().uuid(), contact_party_id: id, name: z.string(), email: z.string().nullable(), phone: z.string().nullable(),
  responsible_party_id: id.nullable(), source: z.string(), stage: z.enum(crmStages), notes: z.string(),
  next_action: z.string(), follow_up_at: z.string().nullable(), contacted_at: z.string().nullable(),
  requested_quantity: z.number().int(), order_id: id.nullable(), reservation_expires_at: z.string().nullable(),
  outbound_time: z.string().nullable(), return_time: z.string().nullable(), version: id,
});
const snapshotSchema = z.object({
  event: z.object({ id, title: z.string(), startTime: z.string(), organizerId: z.string().nullable(), venue: z.string().nullable(), timezone: z.string().nullable() }),
  role: z.enum(['owner', 'coordinator', 'sales', 'payment_verifier']),
  leads: z.array(leadSchema),
  members: z.array(z.object({ id: z.string().uuid(), partyId: id, name: z.string(), role: z.string(), revokedAt: z.string().nullable() })),
  invitations: z.array(z.object({ id: z.string().uuid(), role: z.string(), recipient_party_id: id.nullable(), expires_at: z.string(), accepted_at: z.string().nullable(), revoked_at: z.string().nullable() })),
  orders: z.array(z.object({ id, contactId: z.string().nullable(), quantity: z.number().int(), amountMinor: z.number().int(), currency: z.string(), status: z.string(), paymentMethod: z.string().nullable(), holdExpiresAt: z.string().nullable() })),
  manualPayments: z.array(z.object({ orderId: id, provider: z.string(), amountMinor: z.number().int().safe(), quantity: z.number().int(), reference: z.string().nullable(), occurredAt: z.string().nullable(), submittedAt: z.string().nullable(), submittedBy: id.nullable(), status: z.string(), verifiedBy: id.nullable(), verifiedAt: z.string().nullable() })),
  sales: z.array(z.object({ currency: z.string(), gross_minor: z.string().regex(/^[0-9]+$/), refunded_minor: z.string().regex(/^[0-9]+$/), net_minor: z.string().regex(/^[0-9]+$/), paid_orders: z.number().int().nonnegative() })),
  activities: z.array(z.object({ id, title: z.string(), status: z.string(), startTime: z.string(), endTime: z.string().nullable(), notes: z.string().nullable(), version: id })),
  transport: z.array(z.object({ direction: z.string(), departure_time: z.string(), departure_place: z.string(), capacity: z.number().int().nullable(), confirmed_at: z.string().nullable() })),
  transportRequired: z.boolean(),
  inventory: z.array(z.object({ tierId: id, name: z.string(), total: z.number().int(), heldOrSold: z.number().int(), remaining: z.number().int() })),
  waitlist: z.array(z.object({ leadId: z.string().uuid(), waitlistId: id, outboundTime: z.string(), returnTime: z.string(), status: z.string() })),
  combinationSalesEnabled: z.literal(false), audit: z.array(z.object({ operation: z.string(), occurred_at: z.string(), actor_id: id.nullable(), from_stage: z.string().nullable(), to_stage: z.string().nullable() })),
});
export type EventCrm = z.infer<typeof snapshotSchema>;
export type EventLead = EventCrm['leads'][number];
export interface ImportRow {
  name: string; email?: string; phone?: string; contactId?: number; responsibleId?: number;
  source?: string; stage?: string; notes?: string; nextAction?: string; contactedAt?: string;
  followUpAt?: string; quantity?: number; orderId?: number; outboundTime?: string; returnTime?: string; discard?: boolean;
}
const previewRow = z.object({
  error: z.string().optional(), contactId: id.nullable().optional(), candidateIds: z.array(id).optional(),
  stage: z.string().optional(), claimedStage: z.string().optional(), paymentReconciliationRequired: z.boolean().optional(),
  reservationRequired: z.boolean().optional(), duplicateOf: z.number().int().optional(), discarded: z.boolean().optional(),
  email: z.string().nullable().optional(), phone: z.string().nullable().optional(),
});
const previewSchema = z.object({ importId: z.string().uuid(), preview: z.array(previewRow), confirmed: z.boolean() });
export type ImportPreview = z.infer<typeof previewSchema>;
const base = (eventId: string) => `/event-crm/events/${encodeURIComponent(eventId)}`;
export async function getEventCrm(eventId: string, filters: { stage?: string; responsibleId?: string; overdue?: boolean } = {}) {
  const params = new URLSearchParams();
  if (filters.stage) params.set('stage', filters.stage);
  if (filters.responsibleId) params.set('responsibleId', filters.responsibleId);
  if (filters.overdue) params.set('overdue', 'true');
  const snapshot = snapshotSchema.parse(await get<unknown>(`${base(eventId)}?${params}`, { cache: 'no-store' }));
  if (String(snapshot.event.id) !== eventId) throw new Error('El CRM recibido pertenece a otro evento.');
  return snapshot;
}
export async function crmCommand(eventId: string, body: Record<string, unknown>, key: string = crypto.randomUUID()) {
  return z.record(z.string(), z.unknown()).parse(await post<unknown>(`${base(eventId)}/commands`, body, { headers: { 'Idempotency-Key': key } }));
}
export async function previewImport(eventId: string, rows: ImportRow[]) {
  return previewSchema.parse(await crmCommand(eventId, { operation: 'import.preview', rows }));
}
export async function lookupCrmRecipient(eventId: string, query: string) {
  const result = await crmCommand(eventId, { operation: 'recipient.lookup', query });
  return z.array(z.object({ partyId: id, name: z.string(), hasAccount: z.boolean() })).parse(result['candidates']);
}
export async function exportEventCrm(eventId: string) {
  const snapshot = snapshotSchema.parse(await get<unknown>(`${base(eventId)}/export`, { cache: 'no-store' }));
  if (String(snapshot.event.id) !== eventId) throw new Error('La exportación pertenece a otro evento.');
  return snapshot;
}
export const invitationPreviewSchema = z.object({ eventId: id, eventTitle: z.string(), startTime: z.string(), venue: z.string().nullable(), role: z.string(), expiresAt: z.string(), permissions: z.array(z.string()) });
export async function previewTeamInvitation(token: string) {
  return invitationPreviewSchema.parse(await post<unknown>('/event-crm/invitations', { token, accept: false }));
}
export async function acceptTeamInvitation(token: string) {
  return z.object({ eventId: id, membershipId: z.string().uuid(), role: z.string() }).parse(await post<unknown>('/event-crm/invitations', { token, accept: true }));
}
export const requestEmailProof = () => postEmpty<unknown>('/event-crm/email/request');
export const confirmEmailProof = (token: string) => post<unknown>('/event-crm/email/confirm', { token });

/** RFC-4180 field quoting, bounded input, no formula evaluation or guessed types. */
export function parseCrmCsv(text: string): string[][] {
  if (text.length > 1_000_000) throw new Error('El archivo supera el límite de 1 MB.');
  const rows: string[][] = []; let row: string[] = []; let cell = ''; let quoted = false; let closed = false;
  const source = text.replace(/^\uFEFF/, '');
  const field = () => { row.push(cell); cell = ''; closed = false; if (row.length > 40) throw new Error('Máximo 40 columnas.'); };
  const line = () => { field(); if (row.some(value => value !== '')) rows.push(row); row = []; if (rows.length > 501) throw new Error('Máximo 500 filas de datos.'); };
  for (let i = 0; i < source.length; i++) {
    const c = source[i];
    if (quoted) {
      if (c === '"' && source[i + 1] === '"') { cell += '"'; i++; }
      else if (c === '"') { quoted = false; closed = true; }
      else cell += c;
    } else if (c === ',' ) field();
    else if (c === '\r' || c === '\n') { line(); if (c === '\r' && source[i + 1] === '\n') i++; }
    else if (c === '"' && cell === '' && !closed) quoted = true;
    else if (closed || c === '"') throw new Error('Comillas CSV inválidas.');
    else cell += c;
  }
  if (quoted) throw new Error('Falta cerrar una celda entre comillas.');
  if (cell || row.length || closed) line();
  if (rows.length < 2) throw new Error('Incluye encabezados y al menos una fila.');
  return rows;
}

export function mapCrmRows(rows: string[][], mapping: Record<string, number>): ImportRow[] {
  const stages: Record<string, string> = { interesado: 'interested', contactado: 'contacted', reservado: 'reserved', pagado: 'paid', 'cerrado sin venta': 'closed_lost' };
  return rows.slice(1).map(cells => {
    const row: Record<string, unknown> = { name: '', source: 'lista manual' };
    for (const [key, column] of Object.entries(mapping)) {
      const value = (cells[column] ?? '').trim();
      if (!value) continue;
      if (['quantity', 'contactId', 'responsibleId', 'orderId'].includes(key)) {
        if (!/^[1-9][0-9]*$/.test(value) || !Number.isSafeInteger(Number(value))) throw new Error('Revisa las cantidades e identificadores de la lista.');
        row[key] = Number(value);
      } else row[key] = key === 'stage' ? stages[value.toLowerCase()] ?? value : value;
    }
    return row as unknown as ImportRow;
  });
}
