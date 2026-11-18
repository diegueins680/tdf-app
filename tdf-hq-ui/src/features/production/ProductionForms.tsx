import { useState, type FormEvent, type ReactNode } from 'react';
import { Button, Stack, TextField } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { Label } from '../../api/label';
import { useSession } from '../../session/SessionContext';
import type { Action, Project, Task } from '../../api/productions';

export function Field({ name, title, value, type = 'text', required = true }: { name: string; title: string; value?: string | number; type?: string; required?: boolean }) {
  return <TextField size="small" name={name} label={title} defaultValue={value} type={type} required={required} InputLabelProps={{ shrink: true }} />;
}
export function SelectField({ name, title, value, multiple, children }: { name: string; title: string; value?: string | number | string[]; multiple?: boolean; children: ReactNode }) {
  return <label>{title}<select name={name} defaultValue={value} multiple={multiple} style={{ display: 'block', padding: 8, maxWidth: '100%' }}>{children}</select></label>;
}
export function Form({ action, submit, children }: { action: (data: FormData) => void; submit: string; children: ReactNode }) {
  function onSubmit(event: FormEvent<HTMLFormElement>) {
    event.preventDefault();
    action(new FormData(event.currentTarget));
  }
  return <Stack component="form" onSubmit={onSubmit} spacing={1.5} sx={{ my: 2, maxWidth: 640 }}>
    {children}<Button type="submit" variant="outlined">{submit}</Button></Stack>;
}
const textValue = (data: FormData, name: string) => {
  const value = data.get(name);
  return typeof value === 'string' ? value.trim() : '';
};
const optionalId = (data: FormData, name: string) => {
  const value = textValue(data, name);
  return value === '' ? null : Number(value);
};
const defaultStages = ['Preproducción', 'Grabación', 'Edición', 'Mezcla', 'Masterización', 'Entrega'];
export function CreateProject({ save }: { save: (body: Action) => void }) {
  const { session } = useSession();
  const canLink = session?.roles.some(role => ['artist', 'artista', 'admin'].includes(role.toLowerCase())) ?? false;
  const [showTracks, setShowTracks] = useState(false);
  const [linkedTracks, setLinkedTracks] = useState<string[]>([]);
  const tracks = useQuery({ queryKey: ['production-source-tracks',session?.partyId], queryFn: () => Label.listTracks(session?.partyId),
    enabled: canLink && showTracks && !!session?.partyId });
  const [stages, setStages] = useState(defaultStages.map((name, index) => ({ name, days: index === 5 ? 0 : 1 })));
  return <Form submit="Crear producción privada" action={data => save({ title: data.get('title'), format: data.get('format'), delivery: data.get('delivery'),
    songs: [...linkedTracks.map(track => ({ track })), ...textValue(data, 'songs').split('\n').map(title => title.trim()).filter(Boolean).map(title => ({ title }))], template: stages })}>
    <Field name="title" title="Nombre del proyecto" />
    <SelectField name="format" title="Formato">{['Sencillo', 'EP', 'Álbum'].map(format => <option key={format}>{format}</option>)}</SelectField>
    <Field name="delivery" title="Entrega final" type="date" />
    <TextField required={!linkedTracks.length} multiline minRows={3} label="Canciones, una por línea" name="songs" />
    {canLink && <details onToggle={event => setShowTracks(event.currentTarget.open)}><summary>Vincular pistas propias del label</summary>
      {tracks.isLoading && <p>Cargando pistas…</p>}
      {tracks.isError && <p>No se pudieron cargar tus pistas.</p>}
      {tracks.data?.filter(track => track.ltOwnerId === session?.partyId).map(track => <label key={track.ltId} style={{ display: 'block' }}>
        <input type="checkbox" checked={linkedTracks.includes(track.ltId)} onChange={e => setLinkedTracks(e.target.checked
          ? [...linkedTracks, track.ltId] : linkedTracks.filter(id => id !== track.ltId))} />{track.ltTitle}</label>)}
    </details>}
    <details><summary>Editar plantilla y duración de cada etapa</summary>
      {stages.map((stage, index) => <Stack key={index} direction="row" spacing={1} sx={{ mt: 1 }}>
        <TextField label="Etapa" required value={stage.name} onChange={e => setStages(stages.map((s, i) => i === index ? { ...s, name: e.target.value } : s))} />
        <TextField label="Días" type="number" value={stage.days} inputProps={{ min: 0, max: 365 }} onChange={e => setStages(stages.map((s, i) => i === index ? { ...s, days: Number(e.target.value) } : s))} />
        <Button type="button" disabled={stages.length === 1} onClick={() => setStages(stages.filter((_, i) => i !== index))}>Quitar</Button>
      </Stack>)}
      <Button type="button" onClick={() => setStages([...stages, { name: '', days: 1 }])}>Añadir etapa</Button></details>
  </Form>;
}

export function TaskForm({ project: p, task: t, save }: { project: Project; task?: Task; save: (body: Action) => void }) {
  const team = p.people.filter(person => person.role !== 'client');
  const options = team.map(person => <option key={person.id} value={person.id}>{person.name}</option>);
  return <Form submit={t ? 'Guardar tarea' : 'Añadir tarea'} action={d => save({
    ...Object.fromEntries(d.entries()), op: t ? 'edit' : 'task', task: t?.id,
    approver: optionalId(d, 'approver'), booking: optionalId(d, 'booking'),
    songs: d.getAll('songs'), collaborators: d.getAll('collaborators'),
  })}>
    <Field name="title" title="Tarea" value={t?.title} /><Field name="stage" title="Etapa o hito" value={t?.stage} />
    <Field type="date" name="due" title="Fecha comprometida" value={t?.due ?? p.delivery} />
    <Field type="number" name="duration" title="Duración estimada (días)" value={t?.duration ?? 1} />
    <SelectField name="songs" title="Canciones (vacío para todo el proyecto)" multiple value={t?.songs ?? []}>
      {p.songs.map(s => <option key={s.id} value={s.id}>{s.title}</option>)}</SelectField>
    <SelectField name="owner" title="Responsable" value={t?.owner}>{options}</SelectField>
    <SelectField name="collaborators" title="Colaboradores" multiple value={t?.collaborators.map(String) ?? []}>{options}</SelectField>
    <SelectField name="approver" title="Aprobador" value={t?.approver ?? ''}><option value="">Sin aprobación</option>
      {p.people.map(person => <option key={person.id} value={person.id}>{person.name}</option>)}</SelectField>
    <a href="/estudio/calendario">Consultar disponibilidad y reservas del estudio</a>
    <Field name="booking" title="Número de reserva" type="number" required={false} value={t?.booking?.id} />
  </Form>;
}
