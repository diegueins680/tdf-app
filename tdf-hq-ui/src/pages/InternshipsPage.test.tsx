import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

const listInternsMock = jest.fn<() => Promise<unknown[]>>();
const getProfileMock = jest.fn<() => Promise<unknown | null>>();
const listProjectsMock = jest.fn<() => Promise<unknown[]>>();
const listTasksMock = jest.fn<() => Promise<unknown[]>>();
const listTodosMock = jest.fn<() => Promise<unknown[]>>();
const listTimeEntriesMock = jest.fn<(partyId: number | null) => Promise<unknown[]>>();
const listPermissionsMock = jest.fn<() => Promise<unknown[]>>();
const createProjectMock = jest.fn<(payload: unknown) => Promise<unknown>>();
const updateProfileMock = jest.fn<(payload: unknown) => Promise<unknown>>();
const updateProjectMock = jest.fn<(projectId: string, payload: unknown) => Promise<unknown>>();
const createTaskMock = jest.fn<(payload: unknown) => Promise<unknown>>();
const updateTaskMock = jest.fn<(taskId: string, payload: unknown) => Promise<unknown>>();
const createTodoMock = jest.fn<(payload: unknown) => Promise<unknown>>();
const updateTodoMock = jest.fn<(todoId: string, payload: unknown) => Promise<unknown>>();
const deleteTodoMock = jest.fn<(todoId: string) => Promise<unknown>>();
const clockInMock = jest.fn<() => Promise<unknown>>();
const clockOutMock = jest.fn<() => Promise<unknown>>();
const createPermissionMock = jest.fn<(payload: unknown) => Promise<unknown>>();
const updatePermissionMock = jest.fn<(permissionId: string, payload: unknown) => Promise<unknown>>();
const useSessionMock = jest.fn<() => { session: { roles: string[]; modules: string[] } }>();

jest.unstable_mockModule('../api/internships', () => ({
  Internships: {
    listInterns: () => listInternsMock(),
    getProfile: () => getProfileMock(),
    listProjects: () => listProjectsMock(),
    listTasks: () => listTasksMock(),
    listTodos: () => listTodosMock(),
    listTimeEntries: (partyId: number | null) => listTimeEntriesMock(partyId),
    listPermissions: () => listPermissionsMock(),
    createProject: (payload: unknown) => createProjectMock(payload),
    updateProfile: (payload: unknown) => updateProfileMock(payload),
    updateProject: (projectId: string, payload: unknown) => updateProjectMock(projectId, payload),
    createTask: (payload: unknown) => createTaskMock(payload),
    updateTask: (taskId: string, payload: unknown) => updateTaskMock(taskId, payload),
    createTodo: (payload: unknown) => createTodoMock(payload),
    updateTodo: (todoId: string, payload: unknown) => updateTodoMock(todoId, payload),
    deleteTodo: (todoId: string) => deleteTodoMock(todoId),
    clockIn: () => clockInMock(),
    clockOut: () => clockOutMock(),
    createPermission: (payload: unknown) => createPermissionMock(payload),
    updatePermission: (permissionId: string, payload: unknown) => updatePermissionMock(permissionId, payload),
  },
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => useSessionMock(),
}));

jest.unstable_mockModule('../utils/accessControl', () => ({
  hasInternshipsAccess: () => true,
  hasInternshipsAdminAccess: () => true,
  normalizeAccessRoles: (roles: string[] | null | undefined) => roles ?? [],
}));

const { default: InternshipsPage } = await import('./InternshipsPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const waitForExpectation = async (assertion: () => void, attempts = 12) => {
  let lastError: unknown;
  for (let index = 0; index < attempts; index += 1) {
    try {
      assertion();
      return;
    } catch (error) {
      lastError = error;
      await act(async () => {
        await flushPromises();
      });
    }
  }
  throw lastError;
};

const renderPage = async (container: HTMLElement) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <QueryClientProvider client={qc}>
        <InternshipsPage />
      </QueryClientProvider>,
    );
    await flushPromises();
    await flushPromises();
  });

  return {
    cleanup: async () => {
      if (!root) return;
      await act(async () => {
        root?.unmount();
        await flushPromises();
      });
      root = null;
      qc.clear();
      document.body.removeChild(container);
    },
  };
};

const hasLabel = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('label')).some((element) => {
    const text = (element.textContent ?? '').replace('*', '').trim();
    return text === labelText;
  });

const buildProject = (overrides: Record<string, unknown> = {}) => ({
  ipId: 'project-1',
  ipTitle: 'Campana de lanzamiento',
  ipDescription: 'Entregables semanales',
  ipStatus: 'active',
  ipStartAt: null,
  ipDueAt: null,
  ...overrides,
});

const buildIntern = (overrides: Record<string, unknown> = {}) => ({
  isPartyId: 101,
  isName: 'Ada Lovelace',
  ...overrides,
});

const getButtonsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('button')).filter((element) => (element.textContent ?? '').trim() === labelText) as HTMLButtonElement[];

const clickButton = async (button: HTMLButtonElement) => {
  await act(async () => {
    button.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
  });
};

describe('InternshipsPage', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
    if (!window.matchMedia) {
      Object.defineProperty(window, 'matchMedia', {
        writable: true,
        value: () => ({
          matches: false,
          media: '',
          onchange: null,
          addListener: () => undefined,
          removeListener: () => undefined,
          addEventListener: () => undefined,
          removeEventListener: () => undefined,
          dispatchEvent: () => false,
        }),
      });
    }
  });

  beforeEach(() => {
    listInternsMock.mockReset();
    getProfileMock.mockReset();
    listProjectsMock.mockReset();
    listTasksMock.mockReset();
    listTodosMock.mockReset();
    listTimeEntriesMock.mockReset();
    listPermissionsMock.mockReset();
    createProjectMock.mockReset();
    updateProfileMock.mockReset();
    updateProjectMock.mockReset();
    createTaskMock.mockReset();
    updateTaskMock.mockReset();
    createTodoMock.mockReset();
    updateTodoMock.mockReset();
    deleteTodoMock.mockReset();
    clockInMock.mockReset();
    clockOutMock.mockReset();
    createPermissionMock.mockReset();
    updatePermissionMock.mockReset();
    useSessionMock.mockReset();

    useSessionMock.mockReturnValue({ session: { roles: ['admin'], modules: ['internships'] } });
    listInternsMock.mockResolvedValue([]);
    getProfileMock.mockResolvedValue(null);
    listProjectsMock.mockResolvedValue([]);
    listTasksMock.mockResolvedValue([]);
    listTodosMock.mockResolvedValue([]);
    listTimeEntriesMock.mockResolvedValue([]);
    listPermissionsMock.mockResolvedValue([]);
    createProjectMock.mockResolvedValue({});
    updateProfileMock.mockResolvedValue({});
    updateProjectMock.mockResolvedValue({});
    createTaskMock.mockResolvedValue({});
    updateTaskMock.mockResolvedValue({});
    createTodoMock.mockResolvedValue({});
    updateTodoMock.mockResolvedValue({});
    deleteTodoMock.mockResolvedValue({});
    clockInMock.mockResolvedValue({});
    clockOutMock.mockResolvedValue({});
    createPermissionMock.mockResolvedValue({});
    updatePermissionMock.mockResolvedValue({});
  });

  it('keeps the project form collapsed behind one CTA until an admin opens it', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Proyectos');
        expect(container.textContent).toContain('Todavía no hay proyectos. Crea el primero desde Nuevo proyecto.');
        expect(container.textContent).toContain(
          'Usa proyectos para agrupar tareas y entregables. El formulario se abre solo cuando realmente vayas a crear uno.',
        );
        expect(getButtonsByText(container, 'Nuevo proyecto')).toHaveLength(1);
        expect(getButtonsByText(container, 'Guardar proyecto')).toHaveLength(0);
        expect(getButtonsByText(container, 'Cancelar proyecto')).toHaveLength(0);
        expect(hasLabel(container, 'Nombre')).toBe(false);
      });

      await clickButton(getButtonsByText(container, 'Nuevo proyecto')[0]!);

      await waitForExpectation(() => {
        expect(getButtonsByText(container, 'Nuevo proyecto')).toHaveLength(0);
        expect(getButtonsByText(container, 'Guardar proyecto')).toHaveLength(1);
        expect(getButtonsByText(container, 'Cancelar proyecto')).toHaveLength(1);
        expect(hasLabel(container, 'Nombre')).toBe(true);
        expect(container.textContent).not.toContain(
          'Usa proyectos para agrupar tareas y entregables. El formulario se abre solo cuando realmente vayas a crear uno.',
        );
      });

      await clickButton(getButtonsByText(container, 'Cancelar proyecto')[0]!);

      await waitForExpectation(() => {
        expect(container.textContent).toContain('Todavía no hay proyectos. Crea el primero desde Nuevo proyecto.');
        expect(getButtonsByText(container, 'Nuevo proyecto')).toHaveLength(1);
        expect(getButtonsByText(container, 'Guardar proyecto')).toHaveLength(0);
        expect(getButtonsByText(container, 'Cancelar proyecto')).toHaveLength(0);
        expect(hasLabel(container, 'Nombre')).toBe(false);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the task form collapsed behind one CTA and replaces the no-project dead end with guidance', async () => {
    listProjectsMock.mockResolvedValue([buildProject()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Tareas');
        expect(container.textContent).toContain('Todavía no hay tareas. Crea la primera desde Nueva tarea.');
        expect(container.textContent).toContain(
          'Usa tareas para repartir trabajo dentro de un proyecto. El formulario se abre solo cuando realmente vayas a asignar una.',
        );
        expect(getButtonsByText(container, 'Nueva tarea')).toHaveLength(1);
        expect(getButtonsByText(container, 'Crear tarea')).toHaveLength(0);
        expect(getButtonsByText(container, 'Cancelar tarea')).toHaveLength(0);
        expect(hasLabel(container, 'Proyecto')).toBe(false);
        expect(hasLabel(container, 'Título')).toBe(false);
      });

      await clickButton(getButtonsByText(container, 'Nueva tarea')[0]!);

      await waitForExpectation(() => {
        expect(getButtonsByText(container, 'Nueva tarea')).toHaveLength(0);
        expect(getButtonsByText(container, 'Crear tarea')).toHaveLength(1);
        expect(getButtonsByText(container, 'Cancelar tarea')).toHaveLength(1);
        expect(hasLabel(container, 'Proyecto')).toBe(true);
        expect(hasLabel(container, 'Título')).toBe(true);
        expect(container.textContent).not.toContain(
          'Usa tareas para repartir trabajo dentro de un proyecto. El formulario se abre solo cuando realmente vayas a asignar una.',
        );
      });

      await clickButton(getButtonsByText(container, 'Cancelar tarea')[0]!);

      await waitForExpectation(() => {
        expect(container.textContent).toContain('Todavía no hay tareas. Crea la primera desde Nueva tarea.');
        expect(getButtonsByText(container, 'Nueva tarea')).toHaveLength(1);
        expect(getButtonsByText(container, 'Crear tarea')).toHaveLength(0);
        expect(getButtonsByText(container, 'Cancelar tarea')).toHaveLength(0);
        expect(hasLabel(container, 'Proyecto')).toBe(false);
        expect(hasLabel(container, 'Título')).toBe(false);
      });
    } finally {
      await cleanup();
    }
  });

  it('replaces the empty task form with project-first guidance until an admin has something to assign into', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Todavía no hay tareas. Crea primero un proyecto para empezar a asignarlas.');
        expect(container.textContent).toContain(
          'Las tareas se asignan dentro de un proyecto. Crea el primero en Proyectos y luego aparecera Nueva tarea aqui.',
        );
        expect(getButtonsByText(container, 'Nueva tarea')).toHaveLength(0);
        expect(getButtonsByText(container, 'Crear tarea')).toHaveLength(0);
        expect(getButtonsByText(container, 'Cancelar tarea')).toHaveLength(0);
        expect(hasLabel(container, 'Proyecto')).toBe(false);
        expect(hasLabel(container, 'Título')).toBe(false);
      });
    } finally {
      await cleanup();
    }
  });

  it('hides the hour-log intern filter until admins actually have more than one intern to choose from', async () => {
    listInternsMock.mockResolvedValue([buildIntern()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Jornada y registro de horas');
        expect(hasLabel(container, 'Filtrar por pasante')).toBe(false);
        expect(container.textContent).toContain('Pasante disponible');
        expect(container.textContent).toContain('Ada Lovelace');
        expect(container.textContent).toContain('No hace falta filtrarlo: solo hay un pasante cargado ahora mismo.');
      });
    } finally {
      await cleanup();
    }

    listInternsMock.mockResolvedValue([
      buildIntern(),
      buildIntern({ isPartyId: 102, isName: 'Grace Hopper' }),
    ]);

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    try {
      await waitForExpectation(() => {
        expect(hasLabel(secondContainer, 'Filtrar por pasante')).toBe(true);
        expect(secondContainer.textContent).not.toContain('Pasante disponible');
        expect(secondContainer.textContent).not.toContain('No hace falta filtrarlo: solo hay un pasante cargado ahora mismo.');
      });
    } finally {
      await secondRender.cleanup();
    }
  });
});
