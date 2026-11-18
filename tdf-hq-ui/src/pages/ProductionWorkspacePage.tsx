import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Alert, Button, Card, CardContent, CircularProgress, Stack, Typography } from '@mui/material';
import PageShell from '../components/PageShell';
import { useSession } from '../session/SessionContext';
import { useDocumentTitle } from '../hooks/useDocumentTitle';
import { createProduction, deliveryRisks, workingDate, listProductions, nextPerson, productionAction, type Action, type Project } from '../api/productions';
import { CreateProject, Field, Form, SelectField, TaskForm } from '../features/production/ProductionForms';
import { Calendar, TaskCard, Timeline } from '../features/production/WorkspaceViews';

const views = ['Tablero por canción', 'Mis tareas', 'Cronograma del proyecto', 'Calendario entre proyectos'] as const;
type View = typeof views[number];

function Workspace({ actor }: { actor: number }) {
  const projects = useQuery({ queryKey: ['productions',actor], queryFn: listProductions, refetchInterval: 15000 });
  const [selected, setSelected] = useState('');
  const [view, setView] = useState<View>('Tablero por canción');
  const [message, setMessage] = useState('');
  const [saving, setSaving] = useState(false);
  const all = projects.data ?? [];
  const p = all.find(entry => entry.id === selected) ?? all[0];
  async function run(action: () => Promise<unknown>) {
    if (saving) return false;
    setSaving(true); setMessage('');
    try { await action(); await projects.refetch(); return true; }
    catch (error) { setMessage(error instanceof Error ? error.message : 'No se pudo guardar.'); await projects.refetch(); return false; }
    finally { setSaving(false); }
  }
  const risks = p && p.role !== 'client' ? deliveryRisks(p, workingDate()) : [];
  const threatened = risks.filter(r => p && r.finish > p.delivery);
  const save = (project: Project, body: Action) => run(() => productionAction(project,body));
  const mine = all.flatMap(project => project.tasks.filter(t => nextPerson(t) === actor).map(t => ({ project,t })))
    .sort((a,b) => a.t.due.localeCompare(b.t.due));
  return <PageShell title="Producciones musicales" subtitle="Canciones, entregas y revisiones del equipo">
    <Stack spacing={2}>
      <details><summary>Nueva producción</summary><fieldset disabled={saving}><CreateProject save={body => void run(async () => {
        const created = await createProduction(body); setSelected(created.id); setView('Tablero por canción');
      })} /></fieldset></details>
      {projects.isPending && <CircularProgress aria-label="Cargando producciones" />}
      {projects.isError && <Alert severity="error" action={<Button onClick={() => void projects.refetch()}>Reintentar</Button>}>No se pudieron cargar tus producciones.</Alert>}
      {message && <Alert severity="error" onClose={() => setMessage('')}>{message}</Alert>}
      {!projects.isPending && !projects.isError && !p && <p>Aún no tienes proyectos. Crea una producción para empezar.</p>}
      {p && <>
        <label>Producción <select value={p.id} onChange={e => setSelected(e.target.value)}>{all.map(a => <option key={a.id} value={a.id}>{a.title}</option>)}</select></label>
        <Typography variant="h5">{p.title}</Typography><Typography>{p.format} · Entrega final {p.delivery}</Typography>
        {threatened.length > 0 && <Alert severity="warning">Entrega final en riesgo · {threatened[0]!.task.title} queda para {threatened[0]!.finish}. Causa: {threatened[0]!.cause}.</Alert>}
        <Stack component="nav" aria-label="Vistas de producción" direction="row" flexWrap="wrap" gap={1}>
          {views.map(item => <Button key={item} aria-pressed={view === item} variant={view === item ? 'contained' : 'outlined'} onClick={() => setView(item)}>{item}</Button>)}
        </Stack>
        {p.role === 'producer' && view === 'Tablero por canción' && <Stack spacing={1}>
          <details><summary>Invitar al equipo</summary><Form submit="Dar acceso al proyecto" action={d => void save(p,{ op: 'invite', ...Object.fromEntries(d) })}>
            <Field name="username" title="Usuario de la cuenta TDF" /><SelectField name="role" title="Permiso del proyecto">
              <option value="engineer">Ingeniería y producción</option><option value="client">Artista o cliente invitado</option><option value="producer">Productor con control del plan</option>
            </SelectField></Form></details>
          <details><summary>Nueva tarea de proyecto o de canciones</summary><TaskForm project={p} save={body => void save(p,body)} /></details>
        </Stack>}
        {view === 'Tablero por canción' && [{ id: '', title: 'Tareas de proyecto' }, ...p.songs].map(s => {
          const tasks = p.tasks.filter(t => s.id ? t.songs.includes(s.id) : !t.songs.length);
          if (!tasks.length) return null;
          return <Stack component="section" key={s.id} spacing={1}><Typography variant="h5">{s.title}</Typography>
            {tasks.map(t => <TaskCard key={t.id} {...{ p,t,actor }} save={body => save(p,body)} />)}</Stack>;
        })}
        {view === 'Mis tareas' && <Stack spacing={1}>{!mine.length && <p>No tienes acciones pendientes.</p>}{mine.map(({ project,t }) =>
          <Card key={t.id}><CardContent><Typography>{project.title} · {project.songs.filter(s => t.songs.includes(s.id)).map(s => s.title).join(', ')}</Typography>
            <TaskCard p={project} {...{ t,actor }} save={body => save(project,body)} /></CardContent></Card>)}</Stack>}
        {view === 'Cronograma del proyecto' && <Timeline p={p} save={body => save(p,body)} />}
        {view === 'Calendario entre proyectos' && <Calendar projects={all} />}
      </>}
    </Stack>
  </PageShell>;
}

export default function ProductionWorkspacePage() {
  useDocumentTitle('Producciones musicales');
  const { session } = useSession();
  return session?.partyId ? <Workspace key={session.partyId} actor={session.partyId} /> : <p>Inicia sesión para abrir tus producciones.</p>;
}
