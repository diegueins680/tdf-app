import { calendarEntries, deliveryRisks, nextPerson, type Project, type Task } from './productions';

const task = (id: string, data: Partial<Task> = {}): Task => ({
  id, title: id, stage: 'Mezcla', owner: 2, approver: 3, status: 'En curso',
  due: '2027-05-24', duration: 1, blockage: null, booking: null, songs: [], collaborators: [], needs: [], versions: [], ...data,
});
const project = (id: string, tasks: Task[]): Project => ({
  id, title: id, role: 'producer', format: 'EP', delivery: '2027-05-25', revision: 0, tasks, songs: [], people: [], proposals: [],
});
const booking = (id: number, start: string, end: string) => ({ id, start: '2027-05-23T'+start+'Z', end: '2027-05-23T'+end+'Z', engineer: 2, status: 'Confirmed', resources: [8] });

test('touching sessions share a due date without a time conflict, overlapping sessions conflict once', () => {
  const first = project('EP',[task('drums',{ songs:['a','b','c'],booking:booking(1,'09:00','11:00') })]);
  const other = project('single',[task('voice',{ booking:booking(2,'11:00','12:00') })]);
  expect(calendarEntries([first,other]).conflicts).toHaveLength(0);
  expect(calendarEntries([first,other]).load[0]).toHaveLength(2);
  other.tasks[0]!.booking = booking(2,'10:59','12:00');
  expect(calendarEntries([first,other]).conflicts).toHaveLength(1);
  other.tasks[0]!.owner = 8;
  other.tasks[0]!.booking = { ...other.tasks[0]!.booking, engineer: 8, resources: [9] };
  expect(calendarEntries([first,other]).overlaps).toHaveLength(1);
  expect(calendarEntries([first,other]).conflicts).toHaveLength(0);
  expect(calendarEntries([first]).load).toHaveLength(0);
});
test('overdue mix propagates past mastering slack and changes revision ownership', () => {
  const mix = task('mix'), master = task('master',{ needs:['mix'],due:'2027-05-25',duration:2 });
  expect(deliveryRisks(project('EP',[master,mix]),'2027-05-25')).toContainEqual({ task:master, finish:'2027-05-27', cause:'mix' });
  expect(nextPerson({ ...mix, status:'En revisión' })).toBe(3);
  expect(nextPerson({ ...mix, status:'Cambios solicitados' })).toBe(2);
  expect(nextPerson({ ...mix, status:'Completada' })).toBeNull();
});
