import { useCallback, useEffect, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Link, useParams } from 'react-router-dom';
import { Alert, Box, Button, Card, CardContent, Checkbox, FormControlLabel, MenuItem, Stack, TextField, Typography } from '@mui/material';
import { useSession } from '../session/SessionContext';
import { crmCommand, crmStages, exportEventCrm, getEventCrm, lookupCrmRecipient, mapCrmRows, parseCrmCsv, previewImport, stageLabels, type EventLead, type EventCrm, type ImportPreview, type ImportRow } from '../api/eventCrm';

const roleNames: Record<string, string> = { owner: 'Organizador', coordinator: 'Coordinación', sales: 'Ventas', payment_verifier: 'Verificación de pagos' };
const importFields: Record<string, string> = { name: 'Nombre', email: 'Correo', phone: 'Teléfono', contactId: 'ID de contacto', responsibleId: 'ID de responsable', source: 'Origen', stage: 'Etapa', quantity: 'Entradas', notes: 'Notas', nextAction: 'Próxima acción', contactedAt: 'Fecha de contacto (ISO)', followUpAt: 'Seguimiento (ISO)', orderId: 'Orden existente', outboundTime: 'Hora de ida', returnTime: 'Hora de regreso' };
const formatMinor = (value: string) => `${BigInt(value) / 100n}.${String(BigInt(value) % 100n).padStart(2, '0')}`;
const message = (error: unknown) => error instanceof Error ? error.message : 'No se pudo completar la operación.';

function LeadCard({ lead, members, orders, transport, organizerId, canAssign, save }: { lead: EventLead; members: EventCrm['members']; orders: EventCrm['orders']; transport: EventCrm['transport']; organizerId: string | null; canAssign: boolean; save: (body: Record<string, unknown>) => Promise<boolean> }) {
  const [notes, setNotes] = useState(lead.notes); const [next, setNext] = useState(lead.next_action);
  const [stage, setStage] = useState(lead.stage); const [responsible, setResponsible] = useState(String(lead.responsible_party_id ?? ''));
  const [follow, setFollow] = useState(lead.follow_up_at ?? ''); const [order, setOrder] = useState('');
  const [busy, setBusy] = useState(false); const [dirty, setDirty] = useState(false); const [revision, setRevision] = useState(lead.version);
  const [quantity, setQuantity] = useState(lead.requested_quantity); const [contacted, setContacted] = useState(lead.contacted_at ?? '');
  const [outbound, setOutbound] = useState(lead.outbound_time ?? ''); const [returnTime, setReturnTime] = useState(lead.return_time ?? '');
  useEffect(() => { if (!dirty) { setRevision(lead.version); setNotes(lead.notes); setNext(lead.next_action); setStage(lead.stage); setResponsible(String(lead.responsible_party_id ?? '')); setFollow(lead.follow_up_at ?? ''); setQuantity(lead.requested_quantity); setContacted(lead.contacted_at ?? ''); setOutbound(lead.outbound_time ?? ''); setReturnTime(lead.return_time ?? ''); } }, [lead, dirty]);
  return <Card variant="outlined"><CardContent><Stack spacing={2} onChangeCapture={() => setDirty(true)}>
    <Typography variant="h6">{lead.name}</Typography>
    <Typography>{stageLabels[lead.stage]} · {lead.requested_quantity} entradas · {lead.source}</Typography>
    <Typography variant="body2">{lead.email ?? lead.phone ?? 'Contacto sin canal'}</Typography>
    {lead.reservation_expires_at && <Typography>Reserva hasta {new Date(lead.reservation_expires_at).toLocaleString('es-EC')}</Typography>}
    {lead.outbound_time && <Typography>Ida {lead.outbound_time} · Regreso {lead.return_time}</Typography>}
    <TextField select label="Etapa" value={stage} onChange={e => setStage(e.target.value as typeof stage)} disabled={lead.stage === 'paid'}>
      {crmStages.map(value => <MenuItem key={value} value={value} disabled={value === 'paid' || value === 'reserved'}>{stageLabels[value]}</MenuItem>)}
    </TextField>
    {canAssign && <TextField select label="Responsable" value={responsible} onChange={e => setResponsible(e.target.value)}>
      <MenuItem value="">Sin asignar</MenuItem>{organizerId && <MenuItem value={organizerId}>Organizador</MenuItem>}{members.filter(m => !m.revokedAt && ['coordinator', 'sales'].includes(m.role)).map(m => <MenuItem key={m.id} value={String(m.partyId)}>{m.name}</MenuItem>)}
    </TextField>}
    <TextField label="Fecha de contacto (ISO con zona horaria)" value={contacted} onChange={e => setContacted(e.target.value)} />
    {!lead.order_id && <TextField type="number" label="Cantidad de entradas solicitadas" value={quantity} onChange={e => setQuantity(Number(e.target.value))} inputProps={{ min: 1, max: 100 }} />}
    {!lead.order_id && transport.length > 0 && <>
      <TextField select label="Ida solicitada" value={outbound} onChange={e => setOutbound(e.target.value)}><MenuItem value="">Sin transporte</MenuItem>{transport.filter(t => t.direction === 'outbound').map(t => <MenuItem key={t.departure_time} value={t.departure_time}>{t.departure_time} · {t.departure_place}</MenuItem>)}</TextField>
      <TextField select label="Regreso solicitado" value={returnTime} onChange={e => setReturnTime(e.target.value)}><MenuItem value="">Sin transporte</MenuItem>{transport.filter(t => t.direction === 'return').map(t => <MenuItem key={t.departure_time} value={t.departure_time}>{t.departure_time} · {t.departure_place}</MenuItem>)}</TextField>
    </>}
    <TextField label="Notas" multiline minRows={2} value={notes} onChange={e => setNotes(e.target.value)} inputProps={{ maxLength: 4000 }} />
    <TextField label="Próxima acción" value={next} onChange={e => setNext(e.target.value)} inputProps={{ maxLength: 500 }} />
    <TextField label="Fecha de seguimiento (ISO con zona horaria)" placeholder="2026-11-10T15:00:00-05:00" value={follow} onChange={e => setFollow(e.target.value)} />
    {canAssign && <TextField select label="Vincular orden existente" value={order} onChange={e => setOrder(e.target.value)}>
      <MenuItem value="">Conservar vínculo actual</MenuItem>{orders.filter(o => o.contactId === null || o.contactId === String(lead.contact_party_id)).map(o => <MenuItem key={o.id} value={String(o.id)}>Orden {o.id} · {o.status} · {o.quantity} entradas</MenuItem>)}
    </TextField>}
    {dirty && revision !== lead.version && <Alert severity="warning">La ficha cambió. Revisa la versión nueva antes de guardar.</Alert>}
    {dirty && <Button onClick={() => setDirty(false)}>Descartar cambios de esta ficha</Button>}
    <Button disabled={busy || revision !== lead.version} variant="outlined" onClick={() => { setBusy(true); void save({ operation: 'lead.update', leadId: lead.id, expectedVersion: revision, notes, nextAction: next, stage, followUpAt: follow || null, contactedAt: contacted || null, ...(!lead.order_id ? { quantity, outboundTime: outbound || null, returnTime: returnTime || null } : {}), ...(canAssign ? { responsibleId: responsible || null } : {}), ...(order ? { orderId: Number(order) } : {}) }).then(ok => { if (ok) setDirty(false); }).finally(() => setBusy(false)); }}>Guardar seguimiento</Button>
    {lead.outbound_time && lead.return_time && !lead.order_id && !['paid', 'closed_lost'].includes(lead.stage) && <Button onClick={() => void save({ operation: 'waitlist.join', leadId: lead.id })}>Anotar en lista de espera de esta combinación</Button>}
  </Stack></CardContent></Card>;
}

export default function EventCrmPage() {
  const { eventId = '' } = useParams(); const { session } = useSession();
  const [stage, setStage] = useState(''); const [responsible, setResponsible] = useState(''); const [overdue, setOverdue] = useState(false);
  const [error, setError] = useState(''); const [notice, setNotice] = useState(''); const [busy, setBusy] = useState(false);
  const [recipient, setRecipient] = useState(''); const [recipientKind, setRecipientKind] = useState('email'); const [role, setRole] = useState('sales'); const [identityConfirmed, setIdentityConfirmed] = useState(false); const [privateLink, setPrivateLink] = useState('');
  useEffect(() => { setPrivateLink(''); setRecipient(''); setIdentityConfirmed(false); }, [session?.partyId, eventId]);
  const [lookupComplete, setLookupComplete] = useState(false);
  const [candidates, setCandidates] = useState<{ partyId: number; name: string; hasAccount: boolean }[]>([]);
  const [paymentOrder, setPaymentOrder] = useState(''); const [paymentMethod, setPaymentMethod] = useState('cash');
  const [paymentReference, setPaymentReference] = useState(''); const [paymentDate, setPaymentDate] = useState(''); const [reviewNotes, setReviewNotes] = useState('');
  const [csv, setCsv] = useState<string[][]>([]); const [mapping, setMapping] = useState<Record<string, number>>({});
  const [rows, setRows] = useState<ImportRow[]>([]); const [preview, setPreview] = useState<ImportPreview | null>(null);
  const [name, setName] = useState(''); const [email, setEmail] = useState(''); const [phone, setPhone] = useState('');
  const [taskTitle, setTaskTitle] = useState(''); const [taskNotes, setTaskNotes] = useState(''); const [taskStart, setTaskStart] = useState(''); const [taskEnd, setTaskEnd] = useState('');
  const [taskEditId, setTaskEditId] = useState(''); const [taskRevision, setTaskRevision] = useState<number | null>(null);
  const [task, setTask] = useState(''); const [taskPerson, setTaskPerson] = useState('');
  const query = useQuery({ queryKey: ['event-crm', session?.partyId, eventId, stage, responsible, overdue], queryFn: () => getEventCrm(eventId, { stage, responsibleId: responsible, overdue }), retry: false, staleTime: 0, gcTime: 0, refetchOnWindowFocus: 'always' });
  const save = useCallback(async (body: Record<string, unknown>) => {
    setError(''); setNotice('');
    try { await crmCommand(eventId, body); await query.refetch(); setNotice('Cambio guardado.'); return true; }
    catch (e) { setError(message(e)); await query.refetch(); return false; }
  }, [eventId, query]);
  const attempt = async (action: () => Promise<void>) => { setBusy(true); setError(''); setNotice(''); try { await action(); } catch (e) { setError(message(e)); } finally { setBusy(false); } };
  if (!session) return <Alert severity="info">Inicia sesión para consultar el CRM del evento.</Alert>;
  if (query.isError) return <Alert severity="error">No tienes acceso al CRM de este evento o el piloto todavía no está habilitado.</Alert>;
  if (!query.data) return <Typography>Cargando CRM del evento…</Typography>;
  const data = query.data; const manager = ['owner', 'coordinator'].includes(data.role);
  return <Box sx={{ maxWidth: 1100, mx: 'auto', p: { xs: 1, sm: 3 } }} className="ph-no-capture ph-sensitive">
    <Stack spacing={3}>
      <Typography variant="h4" component="h1">{data.event.title} · CRM</Typography>
      <Typography>{data.event.venue} · {new Date(data.event.startTime).toLocaleDateString('es-EC', { timeZone: data.event.timezone ?? 'America/Guayaquil' })} · {roleNames[data.role]}</Typography>
      {data.transportRequired && <Alert severity="warning">Ventas con chiva desactivadas: falta verificar la reserva conjunta de entrada, ida y regreso y confirmar la capacidad de cada viaje.</Alert>}
      {error && <Alert severity="error">{error}</Alert>}{notice && <Alert severity="success">{notice}</Alert>}
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
        <TextField select label="Filtrar etapa" value={stage} onChange={e => setStage(e.target.value)} sx={{ minWidth: 190 }}><MenuItem value="">Todas las etapas</MenuItem>{crmStages.map(s => <MenuItem key={s} value={s}>{stageLabels[s]}</MenuItem>)}</TextField>
        {manager && <TextField select label="Filtrar responsable" value={responsible} onChange={e => setResponsible(e.target.value)} sx={{ minWidth: 190 }}><MenuItem value="">Todos</MenuItem>{data.members.filter(m => !m.revokedAt).map(m => <MenuItem key={m.id} value={String(m.partyId)}>{m.name}</MenuItem>)}</TextField>}
        <FormControlLabel control={<Checkbox checked={overdue} onChange={e => setOverdue(e.target.checked)} />} label="Seguimiento pendiente" />
      </Stack>
      <Button variant="outlined" onClick={() => void attempt(async () => {
        const fresh = await exportEventCrm(eventId); const url = URL.createObjectURL(new Blob([JSON.stringify(fresh, null, 2)], { type: 'application/json' }));
        const anchor = document.createElement('a'); anchor.href = url; anchor.download = 'crm-evento-privado.json'; anchor.click(); setTimeout(() => URL.revokeObjectURL(url), 1000);
      })}>Exportar datos autorizados (archivo privado)</Button>
      <Typography variant="h5" component="h2">Interesados y seguimiento</Typography>
      {data.leads.length === 0 && <Typography>No hay contactos que coincidan con estos filtros.</Typography>}
      <Box sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr', md: '1fr 1fr' }, gap: 2 }}>{data.leads.map(l => <LeadCard key={l.id} lead={l} members={data.members} orders={data.orders} transport={data.transport} organizerId={data.event.organizerId} canAssign={manager} save={save} />)}</Box>
      {manager && <>
        <Typography variant="h5" component="h2">Añadir interesado</Typography>
        <TextField label="Nombre del contacto" value={name} onChange={e => setName(e.target.value)} /><TextField label="Correo del contacto" value={email} onChange={e => setEmail(e.target.value)} /><TextField label="Teléfono del contacto" value={phone} onChange={e => setPhone(e.target.value)} />
        <Button disabled={busy || !name || (!email && !phone)} onClick={() => void save({ operation: 'lead.create', row: { name, ...(email ? { email } : {}), ...(phone ? { phone } : {}), source: 'manual', stage: 'interested' } })}>Buscar contacto canónico y añadir</Button>
        <Typography variant="h5" component="h2">Importar lista manual</Typography>
        <Typography>Usa CSV con encabezados. La vista previa no crea contactos, reservas ni ventas. Corrige las filas y vuelve a previsualizar antes de confirmar.</Typography>
        <Button component="label" variant="outlined">Elegir CSV privado<input type="file" accept=".csv,text/csv" hidden onChange={e => { const file = e.target.files?.[0]; if (file) void attempt(async () => { if (file.size > 1_000_000) throw new Error('Máximo 1 MB.'); const parsed = parseCrmCsv(await file.text()); setCsv(parsed); setMapping({}); setRows([]); setPreview(null); }); e.target.value = ''; }} /></Button>
        {csv.length > 0 && <>
          <Box sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr', sm: '1fr 1fr' }, gap: 2 }}>{Object.entries(importFields).map(([key, label]) => <TextField select key={key} label={`Columna: ${label}`} value={mapping[key] ?? -1} onChange={e => { const next = { ...mapping }; if (Number(e.target.value) < 0) delete next[key]; else next[key] = Number(e.target.value); setMapping(next); setPreview(null); }}><MenuItem value={-1}>Sin columna</MenuItem>{csv[0]?.map((h, i) => <MenuItem key={i} value={i}>{h || `Columna ${i + 1}`}</MenuItem>)}</TextField>)}</Box>
          <Button disabled={mapping['name'] === undefined || busy} onClick={() => void attempt(async () => { const mapped = mapCrmRows(csv, mapping); setRows(mapped); setPreview(await previewImport(eventId, mapped)); })}>Preparar vista previa</Button>
        </>}
        {rows.map((row, i) => <Card variant="outlined" key={i}><CardContent><Stack spacing={1}>
          <Typography>Fila {i + 1} · {row.name || 'Sin nombre'}</Typography>
          {preview?.preview[i]?.error && <Alert severity="error">{preview.preview[i]?.error}{preview.preview[i]?.candidateIds?.length ? ` · Contactos candidatos: ${preview.preview[i]?.candidateIds?.join(', ')}` : ''}</Alert>}
          {preview?.preview[i]?.paymentReconciliationRequired && <Alert severity="warning">Pago por conciliar. Quedará contactado; no cuenta como venta.</Alert>}
          {preview?.preview[i]?.reservationRequired && <Alert severity="warning">Reserva sin orden vigente. No ocupa plazas.</Alert>}
          {preview?.preview[i]?.duplicateOf !== undefined && <Alert severity="info">Duplicada de la fila {(preview.preview[i]?.duplicateOf ?? 0) + 1}.</Alert>}
          {(['name', 'email', 'phone', 'stage', 'notes', 'orderId', 'contactId'] as const).map(field => <TextField key={field} label={`Fila ${i + 1}: ${importFields[field]}`} value={row[field] ?? ''} onChange={e => { const next = [...rows]; next[i] = { ...row, [field]: ['orderId', 'contactId'].includes(field) ? Number(e.target.value) || undefined : e.target.value }; setRows(next); setPreview(null); }} />)}
          <FormControlLabel control={<Checkbox checked={row.discard ?? false} onChange={e => { const next = [...rows]; next[i] = { ...row, discard: e.target.checked }; setRows(next); setPreview(null); }} />} label={`Descartar fila ${i + 1}`} />
        </Stack></CardContent></Card>)}
        {rows.length > 0 && <Button disabled={busy} onClick={() => void attempt(async () => setPreview(await previewImport(eventId, rows)))}>Validar correcciones</Button>}
        {preview && <Button variant="contained" disabled={busy || preview.preview.some(r => r.error)} onClick={() => void attempt(async () => { await crmCommand(eventId, { operation: 'import.confirm', importId: preview.importId }); await query.refetch(); setNotice('Importación confirmada sin ventas ni reservas adicionales.'); setRows([]); setCsv([]); setPreview(null); })}>Confirmar importación revisada</Button>}
        <Typography variant="h5" component="h2">Equipo e invitaciones</Typography>
        <Alert severity="info">Coordinación gestiona equipo de ventas, logística y CRM. Precios, publicación, cobros y devoluciones pertenecen al organizador. Un verificador de pagos requiere designación expresa del organizador.</Alert>
        <TextField select label="Destinataria por" value={recipientKind} onChange={e => { setRecipientKind(e.target.value); setIdentityConfirmed(false); }}><MenuItem value="email">Correo confirmado para registro y verificación</MenuItem><MenuItem value="account">ID de cuenta TDF confirmada</MenuItem></TextField>
        <TextField label="Cuenta o correo de la destinataria" value={recipient} onChange={e => { setRecipient(e.target.value); setIdentityConfirmed(false); setLookupComplete(false); setCandidates([]); }} autoComplete="off" />
        <Button disabled={busy || !recipient} onClick={() => void attempt(async () => { setCandidates(await lookupCrmRecipient(eventId, recipient)); setLookupComplete(true); })}>Resolver cuenta TDF antes de emitir</Button>
        {lookupComplete && candidates.length === 0 && <Alert severity="info">No hay cuenta coincidente. La destinataria deberá registrarse con el correo confirmado y verificarlo antes de aceptar.</Alert>}
        {candidates.map(c => <Button key={c.partyId} disabled={!c.hasAccount} onClick={() => { setRecipientKind('account'); setRecipient(String(c.partyId)); setLookupComplete(true); setIdentityConfirmed(false); }}>Usar cuenta {c.partyId}: {c.name}{c.hasAccount ? '' : ' · contacto sin cuenta'}</Button>)}
        <TextField select label="Rol de la invitación" value={role} onChange={e => setRole(e.target.value)}><MenuItem value="sales">Ventas: contactos asignados</MenuItem>{data.role === 'owner' && <MenuItem value="coordinator">Coordinación: equipo, logística y CRM del evento</MenuItem>}{data.role === 'owner' && <MenuItem value="payment_verifier">Verificador de pagos: designación expresa</MenuItem>}</TextField>
        <FormControlLabel control={<Checkbox checked={identityConfirmed} onChange={e => setIdentityConfirmed(e.target.checked)} />} label="Confirmé con la persona organizadora la identidad, cuenta o correo y el rol de esta destinataria." />
        <Button disabled={busy || !recipient || !identityConfirmed || !lookupComplete} variant="contained" onClick={() => void attempt(async () => {
          const result = await crmCommand(eventId, { operation: 'invitation.create', role, ...(recipientKind === 'email' ? { recipientEmail: recipient } : { recipientPartyId: Number(recipient) }) });
          if (typeof result['token'] !== 'string') throw new Error('El enlace sólo se muestra al emitirlo. Revoca y genera otro si se perdió.');
          setPrivateLink(`${window.location.origin}/event-team/accept#${result['token']}`); setRecipient(''); setIdentityConfirmed(false); await query.refetch();
        })}>Generar enlace privado de siete días</Button>
        {privateLink && <Alert severity="success"><Stack spacing={1}><Typography>Enlace individual. Se muestra una sola vez; compártelo por un canal privado.</Typography><TextField label="Enlace privado" value={privateLink} InputProps={{ readOnly: true }} autoComplete="off" /><Button onClick={() => void navigator.clipboard.writeText(privateLink)}>Copiar enlace privado</Button><Button onClick={() => setPrivateLink('')}>Ocultar enlace</Button></Stack></Alert>}
        {data.invitations.filter(i => !i.revoked_at && !i.accepted_at).map(i => <Card key={i.id}><CardContent><Typography>{roleNames[i.role]} · vence {new Date(i.expires_at).toLocaleString('es-EC')}</Typography><Button disabled={busy} onClick={() => void save({ operation: 'invitation.revoke', invitationId: i.id })}>Revocar invitación pendiente</Button></CardContent></Card>)}
        {data.members.filter(m => !m.revokedAt).map(m => <Card key={m.id}><CardContent><Typography>{m.name} · {roleNames[m.role]}</Typography>{(data.role === 'owner' || m.role === 'sales') && <Button color="warning" onClick={() => void save({ operation: 'membership.revoke', membershipId: m.id })}>Retirar acceso activo</Button>}</CardContent></Card>)}
        <Typography variant="h5" component="h2">Seguimiento de logística</Typography>
        <TextField select label="Crear o editar actividad" value={taskEditId} onChange={e => { setTaskEditId(e.target.value); const chosen = data.activities.find(a => String(a.id) === e.target.value); setTaskRevision(chosen?.version ?? null); setTaskTitle(chosen?.title ?? ''); setTaskNotes(chosen?.notes ?? ''); setTaskStart(chosen?.startTime ?? ''); setTaskEnd(chosen?.endTime ?? ''); }}><MenuItem value="">Nueva tarea</MenuItem>{data.activities.map(a => <MenuItem key={a.id} value={String(a.id)}>{a.title}</MenuItem>)}</TextField>
        <TextField label="Título de actividad" value={taskTitle} onChange={e => setTaskTitle(e.target.value)} />
        <TextField label="Notas de logística" multiline value={taskNotes} onChange={e => setTaskNotes(e.target.value)} />
        <TextField label="Inicio de actividad (ISO con zona horaria)" value={taskStart} onChange={e => setTaskStart(e.target.value)} />
        <TextField label="Fin de actividad (ISO con zona horaria)" value={taskEnd} onChange={e => setTaskEnd(e.target.value)} />
        <Button disabled={!taskTitle || !taskStart} onClick={() => void save({ operation: 'task.save', title: taskTitle, notes: taskNotes, startTime: taskStart, endTime: taskEnd || null, ...(taskEditId ? { activityId: Number(taskEditId), expectedVersion: taskRevision } : {}) })}>Guardar actividad de logística</Button>
        <Typography variant="h5" component="h2">Asignar logística</Typography>
        <TextField select label="Actividad existente" value={task} onChange={e => setTask(e.target.value)}>{data.activities.map(a => <MenuItem key={a.id} value={String(a.id)}>{a.title}</MenuItem>)}</TextField>
        <TextField select label="Responsable de actividad" value={taskPerson} onChange={e => setTaskPerson(e.target.value)}>{data.members.filter(m => !m.revokedAt).map(m => <MenuItem key={m.id} value={String(m.partyId)}>{m.name}</MenuItem>)}</TextField>
        <Button disabled={!task || !taskPerson} onClick={() => void save({ operation: 'task.assign', activityId: Number(task), responsibleId: Number(taskPerson) })}>Asignar responsable de logística</Button>
      </>}
      <Typography variant="h5" component="h2">Órdenes del evento</Typography>
      {data.orders.map(o => <Typography key={o.id}>Orden {o.id} · {o.quantity} entradas · {(o.amountMinor / 100).toFixed(2)} {o.currency} · {o.status}{o.holdExpiresAt ? ` · Reserva hasta ${new Date(o.holdExpiresAt).toLocaleString('es-EC')}` : ''}</Typography>)}
      {manager && data.sales.map(total => <Typography key={total.currency}>Ventas conciliadas: {total.paid_orders} órdenes · Cobrado {formatMinor(total.gross_minor)} {total.currency} · Devuelto {formatMinor(total.refunded_minor)} · Neto {formatMinor(total.net_minor)}</Typography>)}
      {data.orders.length > 0 && data.role !== 'payment_verifier' && <>
        <Typography variant="h5" component="h2">Registrar evidencia de venta manual</Typography>
        <Typography>Vincula una orden con reserva vigente. Registrar efectivo o transferencia deja el pago pendiente de verificación independiente; no cambia el precio ni genera otra reserva.</Typography>
        <TextField select label="Orden para evidencia manual" value={paymentOrder} onChange={e => setPaymentOrder(e.target.value)}><MenuItem value="">Elige una orden</MenuItem>{data.orders.filter(o => o.status === 'pending').map(o => <MenuItem key={o.id} value={String(o.id)}>Orden {o.id} · {(o.amountMinor / 100).toFixed(2)} {o.currency}</MenuItem>)}</TextField>
        <TextField select label="Medio de pago manual" value={paymentMethod} onChange={e => setPaymentMethod(e.target.value)}><MenuItem value="cash">Efectivo</MenuItem><MenuItem value="bank_transfer">Transferencia seleccionada en checkout</MenuItem></TextField>
        <TextField label="Referencia interna del pago" value={paymentReference} onChange={e => setPaymentReference(e.target.value)} inputProps={{ maxLength: 120 }} />
        <TextField label="Fecha del pago (ISO con zona horaria)" value={paymentDate} onChange={e => setPaymentDate(e.target.value)} placeholder="2026-11-10T15:00:00-05:00" />
        <Button disabled={busy || !paymentOrder || !paymentReference || !paymentDate} onClick={() => void save({ operation: 'manual.submit', orderId: Number(paymentOrder), provider: paymentMethod, reference: paymentReference, occurredAt: paymentDate, amountMinor: data.orders.find(o => o.id === Number(paymentOrder))?.amountMinor })}>Registrar evidencia pendiente</Button>
      </>}
      {data.manualPayments.map(p => <Card key={`${p.orderId}:${p.provider}`}><CardContent><Stack spacing={1}>
        <Typography>Orden {p.orderId} · {p.provider === 'cash' ? 'Efectivo' : 'Transferencia'} · {p.quantity} entradas · {p.status}</Typography>
        <Typography>Referencia: {p.reference ?? 'Pendiente'} · Fecha: {p.occurredAt ?? p.submittedAt ?? 'Pendiente'} · Verificador: {p.verifiedBy ?? 'Pendiente'}</Typography>
        {['owner', 'payment_verifier'].includes(data.role) && ['submitted', 'under_review'].includes(p.status) && <>
          <TextField label={`Notas de verificación de orden ${p.orderId}`} value={reviewNotes} onChange={e => setReviewNotes(e.target.value)} />
          <Button disabled={busy || reviewNotes.trim().length < 3 || p.submittedBy === session.partyId} onClick={() => void save({ operation: 'manual.review', orderId: p.orderId, provider: p.provider, action: 'approve', notes: reviewNotes })}>Confirmar pago manual con evidencia</Button>
          <Button disabled={busy || reviewNotes.trim().length < 3 || p.submittedBy === session.partyId} onClick={() => void save({ operation: 'manual.review', orderId: p.orderId, provider: p.provider, action: 'reject', notes: reviewNotes })}>Rechazar evidencia</Button>
        </>}
      </Stack></CardContent></Card>)}
      <Typography variant="h5" component="h2">Inventario y transporte</Typography>
      <Typography>Cada entrada con transporte requiere una plaza de ida y una de regreso. La disponibilidad depende del menor saldo de la combinación.</Typography>
      {data.transport.map(t => <Typography key={`${t.direction}:${t.departure_time}`}>{t.direction === 'outbound' ? 'Ida' : 'Regreso'} {t.departure_time} desde {t.departure_place}: {t.confirmed_at ? `${t.capacity} plazas confirmadas` : 'Capacidad pendiente de confirmar con transporte'}</Typography>)}
      {data.inventory.map(t => <Typography key={t.tierId}>{t.name}: {t.remaining} disponibles de {t.total} · {t.heldOrSold} reservadas o vendidas en el inventario canónico.</Typography>)}
      <Typography>Lista de espera: {data.waitlist.length} contactos. Anotarse no ocupa plazas ni confirma pagos.</Typography>
      <Button component={Link} to={`/social/eventos/${eventId}`}>Volver al evento</Button>
    </Stack>
  </Box>;
}
