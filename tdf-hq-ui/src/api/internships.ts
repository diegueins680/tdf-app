import { del, get, patch, post } from './client';
import type {
  ClockInRequest,
  ClockOutRequest,
  InternPermissionCreate,
  InternPermissionDTO,
  InternPermissionUpdate,
  InternProfileDTO,
  InternProfileUpdate,
  InternProjectCreate,
  InternProjectDTO,
  InternProjectUpdate,
  InternSummaryDTO,
  InternTaskCreate,
  InternTaskDTO,
  InternTaskUpdate,
  InternTimeEntryDTO,
  InternTodoCreate,
  InternTodoDTO,
  InternTodoUpdate,
} from './types';

export const Internships = {
  listInterns: () => get<InternSummaryDTO[]>('/internships/interns'),
  getProfile: () => get<InternProfileDTO>('/internships/profile'),
  updateProfile: (payload: InternProfileUpdate) =>
    patch<InternProfileDTO>('/internships/profile', payload),
  listProjects: () => get<InternProjectDTO[]>('/internships/projects'),
  createProject: (payload: InternProjectCreate) =>
    post<InternProjectDTO>('/internships/projects', payload),
  updateProject: (projectId: string, payload: InternProjectUpdate) =>
    patch<InternProjectDTO>(`/internships/projects/${projectId}`, payload),
  listTasks: () => get<InternTaskDTO[]>('/internships/tasks'),
  createTask: (payload: InternTaskCreate) =>
    post<InternTaskDTO>('/internships/tasks', payload),
  updateTask: (taskId: string, payload: InternTaskUpdate) =>
    patch<InternTaskDTO>(`/internships/tasks/${taskId}`, payload),
  listTodos: () => get<InternTodoDTO[]>('/internships/todos'),
  createTodo: (payload: InternTodoCreate) =>
    post<InternTodoDTO>('/internships/todos', payload),
  updateTodo: (todoId: string, payload: InternTodoUpdate) =>
    patch<InternTodoDTO>(`/internships/todos/${todoId}`, payload),
  deleteTodo: (todoId: string) =>
    del<void>(`/internships/todos/${todoId}`),
  listTimeEntries: (partyId?: number | null) => {
    const qs = partyId ? `?partyId=${partyId}` : '';
    return get<InternTimeEntryDTO[]>(`/internships/time-entries${qs}`);
  },
  clockIn: (payload: ClockInRequest) =>
    post<InternTimeEntryDTO>('/internships/time-entries/clock-in', payload),
  clockOut: (payload: ClockOutRequest) =>
    post<InternTimeEntryDTO>('/internships/time-entries/clock-out', payload),
  listPermissions: () => get<InternPermissionDTO[]>('/internships/permissions'),
  createPermission: (payload: InternPermissionCreate) =>
    post<InternPermissionDTO>('/internships/permissions', payload),
  updatePermission: (permissionId: string, payload: InternPermissionUpdate) =>
    patch<InternPermissionDTO>(`/internships/permissions/${permissionId}`, payload),
};
