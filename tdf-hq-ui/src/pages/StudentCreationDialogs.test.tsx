import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { StudentCreate, StudentDTO } from '../api/trials';
const create = jest.fn<(body: StudentCreate, key: string) => Promise<StudentDTO>>();
jest.unstable_mockModule('../api/trials', () => ({ Trials: {
  createStudent: create,
  listSubjects: async () => [], listTeachers: async () => [], listStudents: async () => [],
  listTeacherStudents: async () => [], listTeacherClasses: async () => [],
  listAvailabilitySlots: async () => [], listClassSessions: async () => [],
} }));
jest.unstable_mockModule('../api/rooms', () => ({ Rooms: { list: async () => [] } }));
jest.unstable_mockModule('../api/bookings', () => ({ Bookings: { update: jest.fn() } }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: { partyId: 17, roles: ['Teacher'], modules: ['scheduling'] } }) }));
const { default: ClassesPage } = await import('./ClassesPage');
const { default: TrialLessonsPage } = await import('./TrialLessonsPage');
const { default: TeacherPortalPage } = await import('./TeacherPortalPage');
afterEach(() => { cleanup(); create.mockReset(); jest.restoreAllMocks(); });
it.each([
  ['classes', ClassesPage, 'Email', 'Guardar alumno'],
  ['trials', TrialLessonsPage, 'Correo', 'Crear alumno'],
  ['teacher', TeacherPortalPage, 'Email', 'Crear'],
] as const)('guards pending %s creation and requires explicit abandonment before rotating its key', async (kind, Page, emailLabel, submitLabel) => {
  let rejectPending!: (error: Error) => void;
  create.mockImplementationOnce(() => new Promise((_resolve, reject) => { rejectPending = reject; }));
  const confirm = jest.spyOn(window, 'confirm').mockReturnValue(false);
  const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  render(<MemoryRouter><QueryClientProvider client={client}><Page /></QueryClientProvider></MemoryRouter>);
  if (kind === 'teacher') fireEvent.click(screen.getByRole('tab', { name: 'Alumnos' }));
  fireEvent.click(await screen.findByRole('button', { name: 'Nuevo alumno' }));
  fireEvent.change(screen.getByLabelText(/^Nombre completo/), { target: { value: 'Synthetic student' } });
  fireEvent.change(screen.getByLabelText(new RegExp(`^${emailLabel}`)), { target: { value: 'student@example.test' } });
  fireEvent.click(screen.getByRole('button', { name: submitLabel }));
  await waitFor(() => expect(create).toHaveBeenCalledTimes(1));
  const key = create.mock.calls[0]?.[1];
  expect(screen.getByRole('button', { name: 'Cancelar' })).toBeDisabled();
  fireEvent.keyDown(screen.getByRole('dialog'), { key: 'Escape', code: 'Escape' });
  const backdrop = document.querySelector('.MuiBackdrop-root');
  if (backdrop) fireEvent.click(backdrop);
  expect(screen.getByRole('dialog')).toBeInTheDocument();
  expect(confirm).not.toHaveBeenCalled();
  await act(async () => { rejectPending(new Error('Response lost')); });
  await waitFor(() => expect(screen.getByRole('button', { name: 'Cancelar' })).toBeEnabled());
  create.mockRejectedValueOnce(new Error('Still unavailable'));
  fireEvent.change(screen.getByLabelText(/^Nombre completo/), { target: { value: 'Edited student' } });
  fireEvent.click(screen.getByRole('button', { name: submitLabel }));
  await waitFor(() => expect(create).toHaveBeenCalledTimes(2));
  expect(create.mock.calls[1]?.[1]).toBe(key);
  await waitFor(() => expect(screen.getByRole('button', { name: 'Cancelar' })).toBeEnabled());
  fireEvent.click(screen.getByRole('button', { name: 'Cancelar' }));
  expect(confirm).toHaveBeenCalledTimes(1);
  expect(screen.getByRole('dialog')).toBeInTheDocument();
  confirm.mockReturnValue(true);
  fireEvent.click(screen.getByRole('button', { name: 'Cancelar' }));
  await waitFor(() => expect(screen.queryByRole('dialog')).not.toBeInTheDocument());
  fireEvent.click(screen.getByRole('button', { name: 'Nuevo alumno' }));
  fireEvent.change(screen.getByLabelText(/^Nombre completo/), { target: { value: 'Another student' } });
  fireEvent.change(screen.getByLabelText(new RegExp(`^${emailLabel}`)), { target: { value: 'another@example.test' } });
  create.mockResolvedValueOnce({ studentId: 23, displayName: 'Another student' });
  fireEvent.click(screen.getByRole('button', { name: submitLabel }));
  await waitFor(() => expect(create).toHaveBeenCalledTimes(3));
  expect(create.mock.calls[2]?.[1]).not.toBe(key);
  await waitFor(() => expect(screen.queryByRole('dialog')).not.toBeInTheDocument());
  client.clear();
});
