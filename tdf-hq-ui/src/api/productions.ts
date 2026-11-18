import { z } from 'zod';
import { get, post } from './client';

const feedback = z.object({ kind: z.enum(['comment', 'changes', 'approve']), note: z.string(), actor: z.string(), at: z.string() });
const version = z.object({ id: z.string(), number: z.number(), media_type: z.string(), current: z.boolean(), feedback: z.array(feedback) });
const booking = z.object({ id: z.number(), start: z.string(), end: z.string(), status: z.string(), engineer: z.number().nullable(), resources: z.array(z.number()) });
const task = z.object({
  id: z.string(), title: z.string(), stage: z.string(), owner: z.number(), approver: z.number().nullable(),
  due: z.string(), duration: z.number(), status: z.string(), blockage: z.string().nullable(), booking: booking.nullable(),
  songs: z.array(z.string()), collaborators: z.array(z.number()), needs: z.array(z.string()), versions: z.array(version),
});
const project = z.object({
  id: z.string(), title: z.string(), format: z.string(), delivery: z.string(), revision: z.number(), role: z.enum(['producer', 'engineer', 'client']),
  people: z.array(z.object({ id: z.number(), name: z.string(), role: z.string() })),
  songs: z.array(z.object({ id: z.string(), title: z.string() })), tasks: z.array(task),
  proposals: z.array(z.object({ id: z.string(), base: z.number(), delivery: z.string(), confirmed: z.boolean(), backward: z.boolean(),
    changes: z.array(z.object({ task: z.string(), title: z.string(), current: z.string(), suggested: z.string(), cause: z.string().nullable(), booking: z.number().nullable() })) })),
});

export type Project = z.infer<typeof project>;
export type Task = z.infer<typeof task>;
export type Version = z.infer<typeof version>;
export type Action = Record<string, unknown>;
export const listProductions = async () => z.array(project).parse(await get<unknown>('/productions'));
const receipt = z.object({ id: z.string() });
export const createProduction = async (body: Action) => receipt.parse(await post<unknown>('/productions', body));
export const productionAction = async (p: Project, body: Action) =>
  receipt.parse(await post<unknown>(`/productions/${p.id}/actions`, { revision: p.revision, ...body }));
export const nextPerson = (task: Task) => ['Completada', 'Historial'].includes(task.status) ? null
  : task.status === 'En revisión' ? task.approver : task.owner;
export const personName = (p: Project, id: number | null) => p.people.find(person => person.id === id)?.name ?? '—';
export const pendingDependencies = (p: Project, task: Task) => p.tasks.filter(other => task.needs.includes(other.id) && other.status !== 'Completada');
export const workingDate = () => new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil', year: 'numeric', month: '2-digit', day: '2-digit' }).format(new Date());

export function calendarEntries(projects: Project[]) {
  const entries = projects.flatMap(p => p.tasks.map(t => ({ p, t })));
  const sessions = entries.filter(({ t }) => t.booking && !['Cancelled', 'NoShow'].includes(t.booking.status));
  const conflicts = sessions.flatMap((left, i) => sessions.slice(i + 1).filter(right => {
    const a = left.t.booking, b = right.t.booking;
    if (!a || !b || Date.parse(a.start) >= Date.parse(b.end) || Date.parse(b.start) >= Date.parse(a.end)) return false;
    return left.t.owner === right.t.owner || (a.engineer != null && a.engineer === b.engineer)
      || a.resources.some(resource => b.resources.includes(resource));
  }).map(right => ({ left, right })));
  const load = new Map<string, typeof entries>();
  for (const entry of entries) {
    const person = nextPerson(entry.t);
    if (person == null) continue;
    const key = [person, entry.t.due].join('/');
    load.set(key, [...(load.get(key) ?? []), entry]);
  }
  return { conflicts, sessions, load: [...load.values()].filter(group => group.length > 1) };
}

// Longest unfinished path, respecting each task's slack. No committed date is mutated.
export function deliveryRisks(project: Project, today: string) {
  const remaining = new Map(project.tasks.map(task => [task.id, task.needs.length]));
  const following = new Map<string, Task[]>();
  for (const task of project.tasks) for (const id of task.needs) following.set(id, [...(following.get(id) ?? []), task]);
  const queue = project.tasks.filter(task => !task.needs.length);
  const forecasts = new Map<string, { finish: string; cause: string }>();
  const day = 86_400_000;
  for (const task of queue) {
    if (!forecasts.has(task.id) && task.status !== 'Completada' && task.due < today) {
      forecasts.set(task.id, { finish: today, cause: task.title });
    }
    const upstream = forecasts.get(task.id);
    for (const next of following.get(task.id) ?? []) {
      const count = remaining.get(next.id)! - 1;
      remaining.set(next.id, count);
      if (!count) queue.push(next);
      if (!upstream || next.status === 'Completada') continue;
      const end = new Date(Date.parse(upstream.finish) + next.duration * day).toISOString().slice(0, 10);
      if (end > next.due && end > (forecasts.get(next.id)?.finish ?? '')) forecasts.set(next.id, { finish: end, cause: task.title });
    }
  }
  return project.tasks.flatMap(t => {
    const risk = forecasts.get(t.id);
    return risk ? [{ task: t, ...risk }] : [];
  });
}
