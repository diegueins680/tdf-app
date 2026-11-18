import { useState } from 'react';
import { Button, Card, CardContent, Stack, Typography } from '@mui/material';
import { calendarEntries, deliveryRisks, nextPerson, pendingDependencies, personName, workingDate, type Project, type Task } from '../../api/productions';
import { Field, Form, SelectField, TaskForm } from './ProductionForms';
import { Reviews, type Save } from './Review';

export function TaskCard({ p, t, actor, save }: { p: Project; t: Task; actor: number; save: Save }) {
  const [expanded, setExpanded] = useState(false);
  const editable = p.role === 'producer' || (p.role === 'engineer' && (actor === t.owner || t.collaborators.includes(actor)));
  const dependencies = pendingDependencies(p, t);
  const approved = t.versions.find(v => v.feedback.some(f => f.kind === 'approve'));
  return <Card variant="outlined"><CardContent><Stack spacing={1}>
    <Typography variant="h6">{t.title}</Typography>
    <Typography>{t.stage} · {t.status} · {t.due}</Typography>
    <Typography variant="body2">{nextPerson(t) == null ? 'Sin acción pendiente' : `Próxima acción: ${personName(p, nextPerson(t))}`}{approved && ` · Audio aprobado: v${approved.number}`}</Typography>
    {t.blockage && <Typography color="error">Bloqueo · {t.blockage}</Typography>}
    {dependencies.length > 0 && <Typography>Depende de {dependencies.map(d => d.title).join(', ')}</Typography>}
    {t.booking && <a href={`/estudio/calendario?bookingId=${t.booking.id}`}>Reserva {t.booking.id} · {new Date(t.booking.start).toLocaleString('es-EC')} · {t.booking.status}</a>}
    <details onToggle={e => setExpanded(e.currentTarget.open)}><summary>Tarea, audio e historial ({t.versions.length})</summary>
      {expanded && <>
      <Reviews {...{ p,t,actor,save,editable }} />
      {editable && t.status !== 'Completada' && <>
        <Form submit="Guardar bloqueo" action={d => void save({ op: 'block', task: t.id, note: d.get('note') })}>
          <Field name="note" title="Motivo del bloqueo (vacío para resolver)" value={t.blockage ?? ''} required={false} /></Form>
        {t.status !== 'En revisión' && <Stack direction="row" flexWrap="wrap">
          <Button onClick={() => void save({ op: 'progress', task: t.id, status: 'En curso' })}>Poner en curso</Button>
          {!t.approver && <Button onClick={() => void save({ op: 'progress', task: t.id, status: 'Completada' })}>Completar</Button>}
        </Stack>}
      </>}
      {p.role === 'producer' && <details><summary>Editar tarea y asignaciones</summary>
        <TaskForm project={p} task={t} save={body => void save(body)} />
        <Form submit="Añadir dependencia" action={d => void save({ op: 'dependency', task: t.id, needs: d.get('needs') })}>
          <SelectField name="needs" title="Debe completarse antes">{p.tasks.filter(a => a.id !== t.id && !t.needs.includes(a.id)).map(a =>
            <option value={a.id} key={a.id}>{a.title} · {p.songs.filter(s => a.songs.includes(s.id)).map(s => s.title).join(', ')}</option>)}</SelectField>
        </Form></details>}
      </>}
    </details>
  </Stack></CardContent></Card>;
}

export function Timeline({ p, save }: { p: Project; save: Save }) {
  const risks = deliveryRisks(p, workingDate());
  return <Stack spacing={2}><Typography variant="h6">Entrega final · {p.delivery}</Typography>
    {risks.map(r => <Typography key={r.task.id} color="warning.main">{r.task.title} · Comprometida {r.task.due}, prevista {r.finish} · Causa: {r.cause}</Typography>)}
    {p.role === 'producer' && <details><summary>Planificar hacia atrás desde la entrega</summary>
      <Form submit="Proponer cronograma" action={d => void save({ op: 'backplan', finish: d.get('finish') })}>
        <Field name="finish" title="Fecha final propuesta" type="date" value={p.delivery} /></Form></details>}
    {p.tasks.map(t => <div key={t.id}><strong>{t.due} · {t.title}</strong> · {p.songs.filter(s => t.songs.includes(s.id)).map(s => s.title).join(', ')} · {t.status}
      {t.status !== 'Completada' && t.due < workingDate() && <Typography color="error">Entrega atrasada</Typography>}
      <Typography variant="body2">{t.duration} días · {personName(p, nextPerson(t))}
        {t.needs.length > 0 && ` · Depende de ${p.tasks.filter(n => t.needs.includes(n.id)).map(n => n.title).join(', ')}`}</Typography>
      {p.role !== 'client' && t.status !== 'Completada' && <details><summary>Evaluar retraso</summary><Form submit="Calcular fechas afectadas" action={d => void save({ op: 'propose', task: t.id, finish: d.get('finish') })}>
        <Field name="finish" title="Finalización prevista" type="date" /></Form></details>}
    </div>)}
    {p.proposals.map(a => <Card key={a.id} variant="outlined"><CardContent><Typography variant="h6">{a.confirmed ? 'Ajuste confirmado' : 'Propuesta de ajuste'}</Typography>
      {a.changes.map(c => <p key={c.task}>{c.title} · {c.current} → {c.suggested} · {c.cause ? `${a.backward ? 'Debe terminar antes de' : 'Depende de'} ${c.cause}` : 'Tarea de origen'}
        {!a.confirmed && a.base !== p.revision && <> · Compromiso actual: {p.tasks.find(t => t.id === c.task)?.due}</>}
        {c.booking && <> · <a href={`/estudio/calendario?bookingId=${c.booking}`}>Revisar disponibilidad de la reserva {c.booking}</a></>}</p>)}
      <p>Entrega final sugerida · {a.delivery}</p>
      {!a.confirmed && (a.base !== p.revision ? <p>El plan cambió. Revisa las fechas y calcula de nuevo.</p>
        : p.role === 'producer' && <Button onClick={() => void save({ op: 'confirm', proposal: a.id })}>Confirmar ajuste</Button>)}
    </CardContent></Card>)}
  </Stack>;
}

export function Calendar({ projects }: { projects: Project[] }) {
  const { sessions, conflicts, load } = calendarEntries(projects);
  return <Stack spacing={1}><Typography variant="h6">Sesiones con horario</Typography>
    {!sessions.length && <p>Sin reservas vinculadas.</p>}
    {sessions.map(({ p, t }) => <div key={t.id}>{p.title} · {t.title} · {new Date(t.booking!.start).toLocaleString('es-EC')} – {new Date(t.booking!.end).toLocaleString('es-EC')}</div>)}
    <Typography variant="h6">Conflictos de agenda</Typography>
    {!conflicts.length && <p>Sin superposiciones de horario para un mismo responsable o recurso.</p>}
    {conflicts.map(({ left, right }) => <div key={left.t.id+right.t.id}>{left.p.title} / {left.t.title} ↔ {right.p.title} / {right.t.title}</div>)}
    <Typography variant="h6">Carga por fecha de entrega</Typography>
    {!load.length && <p>Sin entregas coincidentes por responsable.</p>}
    {load.map(group => <div key={group[0]!.t.id}><strong>{group[0]!.t.due} · {personName(group[0]!.p,nextPerson(group[0]!.t))}</strong>
      <ul>{group.map(({ p,t }) => <li key={t.id}>{p.title} · {t.title}</li>)}</ul></div>)}
  </Stack>;
}
