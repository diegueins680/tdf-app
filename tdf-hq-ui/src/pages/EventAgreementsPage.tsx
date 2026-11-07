import { useEffect, useMemo, useRef, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Accordion, AccordionDetails, AccordionSummary, Alert, Box, Button, Card, CardContent, Checkbox, Chip, CircularProgress, Dialog, DialogActions, DialogContent, DialogTitle, Divider, FormControlLabel, MenuItem, Stack, Tab, Tabs, TextField, Typography } from '@mui/material';
import { Link as RouterLink, useParams, useSearchParams } from 'react-router-dom';
import PageShell from '../components/PageShell';
import MarkdownText from '../components/MarkdownText';
import { UserSelector } from '../components/party-selector/PartySelector';
import type { PartySelectorOption } from '../api/partySelector';
import { EventWorkspaceAPI, type AgreementContent, type AgreementVersion, type EventAgreement, type EventPlanContent, type EventWorkspace, type WorkspaceCommand } from '../api/eventWorkspace';
import { useSession } from '../session/SessionContext';
import { isEventAgreementsEnabled } from '../config/eventAgreementsRollout';
import { getAuthSessionEpoch } from '../session/authEvents';
import { useSessionOwner } from '../session/useSessionOwner';
import { useOwnedSessionDraft } from '../session/useOwnedSessionDraft';
import { parseOptionalPriceCents } from './collaborativeEventCreation';
import { formatCurrency } from '../utils/formatters';

const emptyPlan: EventPlanContent = { title: '', overview: '', activities: [], budget: [], budgetCapCents: null, currency: 'USD', deliverables: [], pending: [] };
const emptyAgreement: AgreementContent = { title: '', body: '', recipientLegalName: '', pendingFields: [] };
const states = { draft: 'Borrador', sent: 'Pendiente de tu decisión', changes_requested: 'Cambios solicitados', accepted: 'Aceptado', rejected: 'Rechazado', withdrawn: 'Retirado' };
const lines = (text: string) => text.split('\n').map((value) => value.trim()).filter(Boolean);
const dateLabel = (value: string) => new Date(value).toLocaleString('es-EC', { timeZone: 'America/Guayaquil', dateStyle: 'medium', timeStyle: 'short' });
const money = (cents: number | null, currency: string) => cents === null ? 'Por cotizar' : formatCurrency(cents / 100, currency, 'es-EC');

export function agreementReadiness(version: AgreementVersion): string[] {
  const plan = version.content.plan.content;
  return [...version.content.pendingFields, ...plan.pending,
    ...(/\[(COMPLETAR|PENDIENTE|POR DEFINIR)[^\]]*\]/i.test(version.content.body) ? ['Completar los campos del texto contractual.'] : []),
    ...(plan.budget.some((row) => row.amountCents === null) || !plan.budget.length ? ['Completar la cotización.'] : []),
    ...(plan.budgetCapCents === null || plan.budgetCapCents !== plan.budget.reduce((sum, row) => sum + (row.amountCents ?? 0), 0) ? ['Revisar el tope total y su desglose.'] : [])];
}

export default function EventAgreementsPage() {
  const { eventId = '' } = useParams();
  const { session } = useSession();
  if (!isEventAgreementsEnabled()) return <PageShell title="Acuerdos y plan"><Alert severity="info">El espacio de acuerdos todavía no está habilitado.</Alert><Button component={RouterLink} to={`/social/eventos/${encodeURIComponent(eventId)}`}>Volver al evento</Button></PageShell>;
  return <Workspace key={`${eventId}:${session?.partyId}:${getAuthSessionEpoch()}`} eventId={eventId} ownerId={session?.partyId ?? null} />;
}

function Workspace({ eventId, ownerId }: { eventId: string; ownerId: number | null }) {
  const [search] = useSearchParams();
  const client = useQueryClient();
  const guard = useSessionOwner(ownerId, eventId);
  const mounted = useRef(true);
  useEffect(() => { mounted.current = true; return () => { mounted.current = false; }; }, []);
  const isCurrent = () => mounted.current && guard.isCurrentOwner();
  const query = useQuery({ queryKey: ['event-workspace', eventId, ownerId, getAuthSessionEpoch()], queryFn: () => EventWorkspaceAPI.read(eventId), enabled: ownerId !== null, retry: false });
  const [feedback, setFeedback] = useState('');
  const [tab, setTab] = useState<'plan' | 'agreements'>(() => search.has('agreement') ? 'agreements' : 'plan');
  const [documentError, setDocumentError] = useState('');
  const [downloading, setDownloading] = useState(false);
  const retry = useRef<WorkspaceCommand | null>(null);
  const mutation = useMutation({
    mutationFn: async (command: WorkspaceCommand) => {
      if (!isCurrent()) throw new Error('Tu sesión cambió. Abre de nuevo el evento.');
      const value = await EventWorkspaceAPI.command(eventId, command);
      if (!isCurrent()) throw new Error('Tu sesión cambió. Abre de nuevo el evento.');
      return value;
    },
    onSuccess: (_value, command) => {
      if (!isCurrent()) return;
      retry.current = null;
      setFeedback(command.action === 'agreement.accept' ? 'Acuerdo aceptado. Ya puedes descargar tu constancia.' : 'Cambio guardado.');
      // A replay returns the original result; always reread current server truth.
      void client.invalidateQueries({ queryKey: ['event-workspace', eventId, ownerId] });
    },
  });
  const command = async (payload: Omit<WorkspaceCommand, 'requestId'>) => {
    const previous = retry.current;
    const sameIntent = previous && JSON.stringify({ ...previous, requestId: undefined }) === JSON.stringify(payload);
    const request = sameIntent ? previous : { ...payload, requestId: crypto.randomUUID() };
    retry.current = request;
    await mutation.mutateAsync(request);
  };
  const download = async (path: string, name: string) => {
    setDocumentError(''); setDownloading(true);
    try {
      const pdf = await EventWorkspaceAPI.document(eventId, path);
      if (!isCurrent()) return;
      const url = URL.createObjectURL(pdf); const link = document.createElement('a');
      link.href = url; link.download = name; link.click(); URL.revokeObjectURL(url);
    } catch (error) { if (isCurrent()) setDocumentError(error instanceof Error ? error.message : 'Reintenta la descarga.'); }
    finally { if (isCurrent()) setDownloading(false); }
  };
  const state = query.isError ? undefined : query.data;
  const busy = mutation.isPending || downloading;
  return <PageShell title="Acuerdos y plan del evento" subtitle="Documentos privados · fechas en hora de Quito" actions={<Button component={RouterLink} to={`/social/eventos/${eventId}`}>Volver al evento</Button>} loading={query.isLoading}>
    <Stack spacing={2.5}>
      {ownerId === null && <Alert severity="info">Inicia sesión con la cuenta destinataria o la del organizador para consultar este espacio.</Alert>}
      {query.error && <Alert severity="error" action={<Button onClick={() => { void query.refetch(); }}>Reintentar</Button>}>{query.error instanceof Error ? query.error.message : 'No se pudo cargar el espacio del evento.'}</Alert>}
      {mutation.error && <Alert severity="error">{mutation.error instanceof Error ? mutation.error.message : 'No se pudo guardar. Conservamos lo que escribiste.'}</Alert>}
      {documentError && <Alert severity="error">{documentError}</Alert>}
      {feedback && <Alert severity="success" role="status" onClose={() => setFeedback('')}>{feedback}</Alert>}
      {state && <>
        <Alert severity="info">{state.owner ? 'Prepara el plan y el presupuesto. Después revisa y aprueba cada propuesta antes de enviarla.' : 'Puedes consultar el plan y comentar. Las propuestas enviadas permiten aceptar, rechazar o solicitar cambios.'}</Alert>
        <Tabs value={tab} onChange={(_, value: 'plan' | 'agreements') => setTab(value)} aria-label="Documentos del evento"><Tab value="plan" label="Plan del evento" /><Tab value="agreements" label={`Acuerdos (${state.agreements.length})`} /></Tabs>
        <Box hidden={tab !== 'plan'} role="tabpanel" aria-label="Plan del evento"><Stack spacing={2}>
        {state.plan ? <Card variant="outlined"><CardContent><Stack spacing={1.5}>
          <Typography component="h2" variant="h5">{state.plan.content.title}</Typography>
          <Typography variant="caption">Plan v{state.plan.revision} · {dateLabel(state.plan.created_at)}</Typography>
          <PlanView plan={state.plan.content} />
          <Button disabled={busy} onClick={() => { void download('plan.pdf', 'plan-evento.pdf'); }}>Descargar plan PDF</Button>
          <Comments items={state.planComments} />
          <CommentBox busy={busy} label="Comentario sobre el plan" submit={(comment) => command({ action: 'plan.comment', version: state.plan!.revision, comment })} />
        </Stack></CardContent></Card> : <Alert severity="info">Todavía no hay un plan guardado.</Alert>}
        {state.owner && <Accordion defaultExpanded={!state.plan}><AccordionSummary aria-controls="plan-editor">Editar plan y cotización</AccordionSummary><AccordionDetails id="plan-editor"><PlanEditor state={state} ownerId={ownerId} eventId={eventId} busy={busy} save={command} /></AccordionDetails></Accordion>}
        </Stack></Box>
        <Box hidden={tab !== 'agreements'} role="tabpanel" aria-label="Acuerdos del evento"><Stack spacing={2}>
        <Typography component="h2" variant="h5">Acuerdos</Typography>
        {!state.agreements.length && <Typography color="text.secondary">Todavía no hay propuestas disponibles.</Typography>}
        {state.agreements.map((agreement) => <AgreementCard key={agreement.id} agreement={agreement} owner={state.owner} busy={busy} command={command} download={download} ownerId={ownerId} eventId={eventId} />)}
        {state.owner && state.plan && <Accordion defaultExpanded={!state.agreements.length}><AccordionSummary>Preparar nuevo acuerdo</AccordionSummary><AccordionDetails><AgreementEditor key="new-agreement" ownerId={ownerId} scope={`${eventId}:new`} busy={busy} create={(content) => command({ action: 'agreement.create', version: 0, content })} /></AccordionDetails></Accordion>}
        </Stack></Box>
      </>}
    </Stack>
  </PageShell>;
}

export function PlanView({ plan }: { plan: EventPlanContent }) {
  return <Stack spacing={1.5}>
    <MarkdownText>{plan.overview}</MarkdownText>
    <Typography component="h3" variant="h6">Cronograma y responsables</Typography>
    {plan.activities.map((activity, index) => <Box key={index}><Typography fontWeight={700}>{activity.title}</Typography><Typography variant="body2">{activity.start} – {activity.end} · {activity.responsible || 'Por confirmar'}</Typography><MarkdownText>{activity.notes}</MarkdownText></Box>)}
    <Typography component="h3" variant="h6">Presupuesto</Typography>
    {plan.budget.map((row, index) => <Box key={index}><Typography>{row.label}: {money(row.amountCents, plan.currency)}</Typography><Typography variant="body2" color="text.secondary">{row.basis}</Typography></Box>)}
    <Typography fontWeight={700}>Tope total propuesto: {money(plan.budgetCapCents, plan.currency)}</Typography>
    <Typography component="h3" variant="h6">Entregables</Typography>
    {plan.deliverables.map((text, i) => <Typography key={i}>{text}</Typography>)}
    {plan.pending.length > 0 && <Alert severity="warning"><Typography fontWeight={700}>Pendientes antes de aprobar</Typography>{plan.pending.map((text, i) => <Typography key={i} variant="body2">{text}</Typography>)}</Alert>}
  </Stack>;
}

function Comments({ items, history = false }: { items: EventWorkspace['planComments']; history?: boolean }) {
  const labels: Record<string, string> = { 'agreement.create': 'Propuesta creada', 'agreement.revise': 'Nueva versión', 'agreement.approve': 'Propuesta y presupuesto aprobados', 'agreement.send': 'Propuesta enviada', 'agreement.accept': 'Acuerdo aceptado', 'agreement.reject': 'Propuesta rechazada', 'agreement.withdraw': 'Propuesta retirada', 'agreement.request_changes': 'Cambios solicitados', 'agreement.comment': 'Comentario' };
  return <Stack spacing={1}>{history && <Typography component="h4" variant="subtitle1">Historial del acuerdo</Typography>}{items.filter((item) => history || item.comment).map((item) => <Box key={item.id}><Typography variant="caption">{history ? `${labels[item.action] ?? 'Actualización'} · ` : ''}{dateLabel(item.created_at)} · v{item.version} · cuenta {item.actor_party_id}</Typography>{item.comment && <MarkdownText>{item.comment}</MarkdownText>}</Box>)}</Stack>;
}
function CommentBox({ label, busy, submit }: { label: string; busy: boolean; submit: (text: string) => Promise<unknown> }) {
  const [text, setText] = useState('');
  return <Stack spacing={1}><TextField label={label} multiline minRows={2} value={text} onChange={(event) => setText(event.target.value)} inputProps={{ maxLength: 4000 }} />
    <Button disabled={busy || !text.trim()} onClick={() => { void submit(text.trim()).then(() => setText('')).catch(() => undefined); }}>Enviar comentario</Button></Stack>;
}

function PlanEditor({ state, ownerId, eventId, busy, save }: { state: EventWorkspace; ownerId: number | null; eventId: string; busy: boolean; save: (command: Omit<WorkspaceCommand, 'requestId'>) => Promise<unknown> }) {
  const baseline = useMemo(() => ({ content: state.plan?.content ?? emptyPlan, recipient: String(state.plan?.recipient_party_id ?? ''), revision: state.plan?.revision ?? 0,
    amounts: (state.plan?.content.budget ?? []).map((row) => row.amountCents === null ? '' : (row.amountCents / 100).toString()),
    cap: state.plan?.content.budgetCapCents == null ? '' : (state.plan.content.budgetCapCents / 100).toString(),
  }), [state.plan]);
  const draft = useOwnedSessionDraft({ ownerId, scope: `${eventId}:plan`, baseline, empty: baseline });
  const [person, setPerson] = useState<PartySelectorOption | null>(null);
  const [amountError, setAmountError] = useState('');
  const plan = draft.draft.content;
  const edit = <K extends keyof EventPlanContent>(key: K, value: EventPlanContent[K]) => draft.setDraft((old) => ({ ...old, content: { ...old.content, [key]: value } }));
  return <Card variant="outlined"><CardContent><Stack spacing={2}>
    <Typography component="h2" variant="h6">Editar plan y cotización</Typography>
    {draft.draft.revision !== baseline.revision && <Alert severity="warning" action={<Button onClick={() => draft.setDraft(baseline)}>Descartar cambios y recargar</Button>}>Existe una versión más reciente. Conservamos tus cambios para que puedas revisarlos.</Alert>}
    <UserSelector value={person} onChange={(selected) => { setPerson(selected); if (selected) draft.setDraft((old) => ({ ...old, recipient: String(selected.partyId) })); }} field={{ label: 'Cuenta del artista con acceso de lectura', helperText: state.plan ? 'La cuenta ya asignada se conserva si no eliges otra.' : 'Selecciona la cuenta propietaria del perfil del artista.' }} search={{ context: 'event_logistics', scopeId: eventId }} />
    <TextField label="Título del plan" value={plan.title} onChange={(e) => edit('title', e.target.value)} inputProps={{ maxLength: 200 }} />
    <TextField label="Descripción y alcance" multiline minRows={3} value={plan.overview} onChange={(e) => edit('overview', e.target.value)} />
    {plan.activities.map((activity, index) => <Stack key={index} spacing={1} sx={{ p: 1.5, border: '1px solid', borderColor: 'divider', borderRadius: 2 }}>
      {(['title', 'start', 'end', 'responsible', 'notes'] as const).map((key) => <TextField key={key} label={{ title: 'Actividad', start: 'Inicio (hora de Quito)', end: 'Fin (hora de Quito)', responsible: 'Responsable', notes: 'Notas' }[key]} value={activity[key]} onChange={(e) => edit('activities', plan.activities.map((item, i) => i === index ? { ...item, [key]: e.target.value } : item))} />)}
      <Button color="error" onClick={() => edit('activities', plan.activities.filter((_, i) => i !== index))}>Quitar actividad</Button>
    </Stack>)}
    <Button onClick={() => edit('activities', [...plan.activities, { title: '', start: '', end: '', responsible: '', notes: '' }])}>Añadir actividad al plan</Button>
    {plan.budget.map((row, index) => <Stack key={index} spacing={1} sx={{ p: 1.5, border: '1px solid', borderColor: 'divider', borderRadius: 2 }}>
      <TextField label="Rubro" value={row.label} onChange={(e) => edit('budget', plan.budget.map((item, i) => i === index ? { ...item, label: e.target.value } : item))} />
      <TextField label="Importe USD (vacío si falta cotizar)" value={draft.draft.amounts[index] ?? ''} onChange={(e) => { setAmountError(''); draft.setDraft((old) => ({ ...old, amounts: old.amounts.map((item, i) => i === index ? e.target.value : item) })); }} inputProps={{ inputMode: 'decimal' }} />
      <TextField label="Tarifa, unidades y supuestos" value={row.basis} onChange={(e) => edit('budget', plan.budget.map((item, i) => i === index ? { ...item, basis: e.target.value } : item))} />
      <Button color="error" onClick={() => draft.setDraft((old) => ({ ...old, amounts: old.amounts.filter((_, i) => i !== index), content: { ...old.content, budget: old.content.budget.filter((_, i) => i !== index) } }))}>Quitar rubro</Button>
    </Stack>)}
    <Button onClick={() => draft.setDraft((old) => ({ ...old, amounts: [...old.amounts, ''], content: { ...old.content, budget: [...old.content.budget, { label: '', amountCents: null, basis: '' }] } }))}>Añadir rubro</Button>
    <TextField label="Tope total recuperable propuesto USD" value={draft.draft.cap} onChange={(e) => { setAmountError(''); draft.setDraft((old) => ({ ...old, cap: e.target.value })); }} helperText="Completa el desglose antes de proponer el máximo recuperable." />
    {amountError && <Alert severity="warning">{amountError}</Alert>}
    <TextField label="Entregables (uno por línea)" multiline minRows={3} value={plan.deliverables.join('\n')} onChange={(e) => edit('deliverables', e.target.value.split('\n'))} />
    <TextField label="Pendientes (uno por línea)" multiline minRows={3} value={plan.pending.join('\n')} onChange={(e) => edit('pending', e.target.value.split('\n'))} helperText="Los pendientes bloquean la aprobación de una propuesta que incorpore esta versión." />
    <Button variant="contained" disabled={busy || !draft.ready || !draft.draft.recipient || !plan.title.trim() || draft.draft.revision !== baseline.revision || Boolean(amountError)} onClick={() => {
      const submitted = draft.draft;
      try {
        const budget = plan.budget.map((row, index) => ({ ...row, amountCents: parseOptionalPriceCents(submitted.amounts[index] ?? '') }));
        const budgetCapCents = parseOptionalPriceCents(submitted.cap);
        void save({ action: 'plan.save', version: submitted.revision, recipientPartyId: submitted.recipient, content: { ...plan, budget, budgetCapCents, deliverables: plan.deliverables.map((v) => v.trim()).filter(Boolean), pending: plan.pending.map((v) => v.trim()).filter(Boolean) } }).then(() => draft.acknowledgeSave(submitted)).catch(() => undefined);
      } catch { setAmountError('Revisa los importes: usa máximo dos decimales y deja vacíos los rubros pendientes.'); }
    }}>Guardar nueva versión del plan</Button>
  </Stack></CardContent></Card>;
}

function AgreementEditor({ ownerId, scope, busy, initial = emptyAgreement, create }: { ownerId: number | null; scope: string; busy: boolean; initial?: AgreementContent; create: (content: AgreementContent) => Promise<unknown> }) {
  const draft = useOwnedSessionDraft({ ownerId, scope, baseline: initial, empty: emptyAgreement });
  const [saved, setSaved] = useState(false);
  const edit = (key: keyof AgreementContent, value: string | string[]) => { setSaved(false); draft.setDraft((old) => ({ ...old, [key]: value })); };
  return <Card variant="outlined"><CardContent><Stack spacing={1.5}>
    <Typography component="h3" variant="h6">{initial === emptyAgreement ? 'Preparar nuevo acuerdo' : 'Preparar nueva propuesta'}</Typography>
    <TextField label="Título del acuerdo" value={draft.draft.title} onChange={(e) => edit('title', e.target.value)} />
    <TextField label="Nombre legal del destinatario" value={draft.draft.recipientLegalName} onChange={(e) => edit('recipientLegalName', e.target.value)} />
    <TextField label="Texto íntegro del acuerdo" multiline minRows={10} value={draft.draft.body} onChange={(e) => edit('body', e.target.value)} />
    <TextField label="Campos contractuales pendientes (uno por línea)" multiline minRows={3} value={draft.draft.pendingFields.join('\n')} onChange={(e) => edit('pendingFields', e.target.value.split('\n'))} />
    <Typography variant="body2">Se incorporará una copia de la versión actual del plan como anexo. Guardar un borrador conserva sus pendientes; su envío requiere completar y aprobar el documento y el presupuesto.</Typography>
    <Button variant="contained" disabled={busy || saved || !draft.ready || !draft.draft.title.trim() || !draft.draft.body.trim() || !draft.draft.recipientLegalName.trim()} onClick={() => { void create({ title: draft.draft.title, body: draft.draft.body, recipientLegalName: draft.draft.recipientLegalName, pendingFields: lines(draft.draft.pendingFields.join('\n')) }).then(() => setSaved(true)).catch(() => undefined); }}>Guardar borrador de propuesta</Button>
    {saved && <Alert severity="success">Borrador guardado.</Alert>}
  </Stack></CardContent></Card>;
}

function AgreementCard({ agreement, owner, busy, command, download, ownerId, eventId }: { agreement: EventAgreement; owner: boolean; busy: boolean; command: (command: Omit<WorkspaceCommand, 'requestId'>) => Promise<unknown>; download: (path: string, name: string) => Promise<void>; ownerId: number | null; eventId: string }) {
  const [selectedVersion, setSelectedVersion] = useState<number | null>(null);
  const latest = agreement.versions.find((version) => version.version === selectedVersion) ?? agreement.versions.at(-1);
  const [acceptOpen, setAcceptOpen] = useState(false);
  const [name, setName] = useState('');
  const [consent, setConsent] = useState(false);
  const [comment, setComment] = useState('');
  if (!latest) return null;
  const current = latest.version === agreement.current_version;
  const issues = agreementReadiness(latest);
  const act = (action: string, extra: Partial<WorkspaceCommand> = {}) => command({ action: `agreement.${action}`, agreementId: agreement.id, version: latest.version, hash: latest.document_hash, ...extra });
  const canAccept = !owner && current && agreement.state === 'sent' && issues.length === 0;
  return <Card variant="outlined"><CardContent><Stack spacing={1.5}>
    <Stack direction="row" spacing={1} flexWrap="wrap"><Typography component="h3" variant="h6">{latest.content.title}</Typography><Chip label={states[agreement.state]} /></Stack>
    <Typography variant="caption">Propuesta v{latest.version} · {dateLabel(latest.created_at)}{latest.approved_at ? ' · Aprobada por el organizador' : ' · Aprobación del organizador pendiente'}</Typography>
    <TextField select label="Versión del documento" value={latest.version} onChange={(event) => setSelectedVersion(Number(event.target.value))}>{agreement.versions.map((version) => <MenuItem key={version.version} value={version.version}>Versión {version.version}{version.sent_at ? ' · enviada' : ' · borrador'}</MenuItem>)}</TextField>
    {!current && <Alert severity="info">El organizador está preparando una revisión. Se conserva la última versión enviada para consulta.</Alert>}
    <MarkdownText>{latest.content.body}</MarkdownText>
    <Divider /><Typography component="h4" variant="h6">Anexo: plan v{latest.content.plan.revision}</Typography><PlanView plan={latest.content.plan.content} />
    {issues.length > 0 && <Alert severity="warning">Pendientes: {issues.join(' · ')}</Alert>}
    <Button disabled={busy} onClick={() => { void download(`agreements/${encodeURIComponent(agreement.id)}/${latest.version}/document.pdf`, 'acuerdo-evento.pdf'); }}>Descargar esta propuesta PDF</Button>
    {agreement.acceptance && <Alert severity="success">Aceptado por {agreement.acceptance.accepted_name} el {dateLabel(agreement.acceptance.accepted_at)} · v{agreement.acceptance.version}<Button disabled={busy} onClick={() => { void download(`agreements/${encodeURIComponent(agreement.id)}/acceptance.pdf`, 'constancia-aceptacion.pdf'); }}>Descargar constancia</Button></Alert>}
    {owner && current && agreement.state === 'draft' && <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
      <Button disabled={busy || issues.length > 0 || Boolean(latest.approved_at)} onClick={() => { void act('approve').catch(() => undefined); }}>Aprobar propuesta y presupuesto</Button>
      <Button variant="contained" disabled={busy || issues.length > 0 || !latest.approved_at} onClick={() => { void act('send').catch(() => undefined); }}>Enviar al artista</Button>
    </Stack>}
    {owner && current && ['draft', 'sent', 'changes_requested'].includes(agreement.state) && <Button disabled={busy} onClick={() => { void act('withdraw').catch(() => undefined); }}>Retirar propuesta</Button>}
    {!owner && current && agreement.state === 'sent' && <>
      <TextField label="Observaciones para el organizador" multiline minRows={2} value={comment} onChange={(e) => setComment(e.target.value)} inputProps={{ maxLength: 4000 }} />
      <Stack direction="row" spacing={1} flexWrap="wrap">
        <Button variant="contained" disabled={busy || !canAccept} onClick={() => setAcceptOpen(true)}>Revisar aceptación</Button>
        <Button disabled={busy || !comment.trim()} onClick={() => { void act('request_changes', { comment }).then(() => setComment('')).catch(() => undefined); }}>Solicitar cambios</Button>
        <Button color="error" disabled={busy} onClick={() => { void act('reject', comment.trim() ? { comment } : {}).then(() => setComment('')).catch(() => undefined); }}>Rechazar propuesta</Button>
      </Stack>
    </>}
    <Comments items={agreement.actions} history />
    <CommentBox label="Comentario sobre el acuerdo" busy={busy || !current} submit={(text) => act('comment', { comment: text })} />
    {owner && current && ['draft', 'changes_requested', 'rejected', 'withdrawn'].includes(agreement.state) && <Accordion><AccordionSummary>Preparar nueva versión</AccordionSummary><AccordionDetails><AgreementEditor ownerId={ownerId} scope={`${eventId}:${agreement.id}:${latest.version}`} initial={latest.content} busy={busy} create={(content) => act('revise', { content })} /></AccordionDetails></Accordion>}
    {owner && agreement.state === 'accepted' && <AgreementEditor ownerId={ownerId} scope={`${eventId}:${agreement.id}:amendment`} busy={busy} initial={{ ...latest.content, title: `Anexo a ${latest.content.title}` }} create={(content) => command({ action: 'agreement.create', version: 0, parentId: agreement.id, content: { title: content.title, body: content.body, recipientLegalName: content.recipientLegalName, pendingFields: content.pendingFields } })} />}
    <Dialog open={acceptOpen && canAccept} onClose={() => { if (!busy) setAcceptOpen(false); }} fullWidth aria-labelledby={`accept-${agreement.id}`}>
      <DialogTitle id={`accept-${agreement.id}`}>Aceptar acuerdo v{latest.version}</DialogTitle>
      <DialogContent><Stack spacing={2} sx={{ mt: 1 }}><Typography>Se registrará tu aceptación de esta versión y del plan anexo v{latest.content.plan.revision}. Podrás descargar la constancia y el contenido aceptado.</Typography>
        <TextField label="Tu nombre legal" value={name} onChange={(e) => setName(e.target.value)} helperText={`Destinatario del acuerdo: ${latest.content.recipientLegalName}`} />
        <FormControlLabel control={<Checkbox checked={consent} onChange={(_, checked) => setConsent(checked)} />} label="He leído y acepto el acuerdo y sus anexos de esta versión." />
      </Stack></DialogContent>
      <DialogActions><Button disabled={busy} onClick={() => setAcceptOpen(false)}>Volver a revisar</Button><Button variant="contained" disabled={busy || !consent || name.trim().toLowerCase() !== latest.content.recipientLegalName.trim().toLowerCase()} onClick={() => { void act('accept', { acceptedName: name.trim(), consent }).then(() => { setAcceptOpen(false); setConsent(false); }).catch(() => undefined); }}>{busy ? <CircularProgress size={20} /> : 'Aceptar acuerdo'}</Button></DialogActions>
    </Dialog>
  </Stack></CardContent></Card>;
}
