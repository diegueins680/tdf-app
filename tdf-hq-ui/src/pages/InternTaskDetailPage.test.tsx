import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, Route, Routes } from 'react-router-dom';

const listTasksMock = jest.fn<() => Promise<unknown[]>>();
const listInternsMock = jest.fn<() => Promise<unknown[]>>();
const listProjectsMock = jest.fn<() => Promise<unknown[]>>();
const listAuditPlansMock = jest.fn<() => Promise<unknown[]>>();
const updateTaskMock = jest.fn<(taskId: string, payload: unknown) => Promise<unknown>>();
const deleteTaskMock = jest.fn<(taskId: string) => Promise<unknown>>();
const useSessionMock = jest.fn<() => { session: { roles: string[]; modules: string[]; partyId?: number } }>();

jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({
    t: (key: string) => (key === 'internships.taskDetail.instructions' ? 'Instrucciones' : key),
  }),
}));

jest.unstable_mockModule('../api/internships', () => ({
  Internships: {
    listTasks: () => listTasksMock(),
    listInterns: () => listInternsMock(),
    listProjects: () => listProjectsMock(),
    updateTask: (taskId: string, payload: unknown) => updateTaskMock(taskId, payload),
    deleteTask: (taskId: string) => deleteTaskMock(taskId),
  },
}));

jest.unstable_mockModule('../api/internAudit', () => ({
  InternAudit: {
    listPlans: () => listAuditPlansMock(),
  },
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  getStoredSessionToken: () => 'test-session-token',
  useSession: () => useSessionMock(),
}));

const { default: InternTaskDetailPage } = await import('./InternTaskDetailPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const waitForExpectation = async (assertion: () => void, attempts = 16) => {
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

const renderPage = async (container: HTMLElement, taskId: string) => {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false, gcTime: 0 },
      mutations: { retry: false },
    },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <QueryClientProvider client={queryClient}>
        <MemoryRouter initialEntries={[`/practicas/tareas/${taskId}`]}>
          <Routes>
            <Route path="/practicas/tareas/:taskId" element={<InternTaskDetailPage />} />
            <Route path="/practicas" element={<div>Página de prácticas</div>} />
          </Routes>
        </MemoryRouter>
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
      queryClient.clear();
      document.body.removeChild(container);
    },
  };
};

const getButtonByText = (root: ParentNode, labelText: string) => {
  const button = Array.from(root.querySelectorAll('button')).find(
    (element) => (element.textContent ?? '').trim() === labelText,
  );
  if (!(button instanceof HTMLButtonElement)) throw new Error(`Could not find button: ${labelText}`);
  return button;
};

const clickButton = async (button: HTMLButtonElement) => {
  await act(async () => {
    button.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
  });
};

const getInputByLabel = (root: ParentNode, labelText: string) => {
  const label = Array.from(root.querySelectorAll('label')).find((element) =>
    (element.textContent ?? '').replace('*', '').trim() === labelText,
  );
  if (!label) throw new Error(`Could not find label: ${labelText}`);
  const inputId = label.getAttribute('for');
  const input = inputId ? document.getElementById(inputId) : null;
  if (input instanceof HTMLInputElement || input instanceof HTMLTextAreaElement) return input;
  const nearbyInput = label.parentElement?.querySelector('input, textarea');
  if (nearbyInput instanceof HTMLInputElement || nearbyInput instanceof HTMLTextAreaElement) return nearbyInput;
  throw new Error(`Could not find input for label: ${labelText}`);
};

const setInputValue = async (input: HTMLInputElement | HTMLTextAreaElement, value: string) => {
  const prototype = input instanceof HTMLTextAreaElement ? HTMLTextAreaElement.prototype : HTMLInputElement.prototype;
  const valueSetter = Object.getOwnPropertyDescriptor(prototype, 'value')?.set;
  if (!valueSetter) throw new Error('Input value setter not found');
  await act(async () => {
    valueSetter.call(input, value);
    input.dispatchEvent(new Event('input', { bubbles: true }));
    input.dispatchEvent(new Event('change', { bubbles: true }));
    await flushPromises();
  });
};

const task = {
  itId: '7e1f7364-8e02-453e-bdf9-b3f17a165fa2',
  itProjectId: 'efd8feba-12de-40f4-ab85-06cee6973935',
  itProjectName: 'Operación de agenda de ensayos — agosto 2026',
  itTitle: 'Agendar ensayos de Milo Mae y Alex Crack',
  itDescription: 'Guía rápida:\n1. Entra a Estudio → Calendario.\n2. Usa la duración sugerida de 90 minutos.',
  itStatus: 'todo',
  itProgress: 0,
  itAssignedTo: 129,
  itAssignedName: 'Stewart Moreira',
  itDueAt: '2026-08-14',
  itCreatedAt: '2026-08-11T19:46:17.795646Z',
  itUpdatedAt: '2026-08-11T19:46:17.795646Z',
};

describe('InternTaskDetailPage', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    listTasksMock.mockReset();
    listInternsMock.mockReset();
    listProjectsMock.mockReset();
    listAuditPlansMock.mockReset();
    updateTaskMock.mockReset();
    deleteTaskMock.mockReset();
    useSessionMock.mockReset();
    useSessionMock.mockReturnValue({ session: { roles: ['admin'], modules: ['internships'], partyId: 1 } });
    listInternsMock.mockResolvedValue([{ isPartyId: 129, isName: 'Stewart Moreira', isRoles: ['Intern'] }]);
    listProjectsMock.mockResolvedValue([{
      ipId: task.itProjectId,
      ipTitle: task.itProjectName,
      ipStatus: 'active',
      ipCreatedAt: task.itCreatedAt,
      ipUpdatedAt: task.itUpdatedAt,
    }]);
    listAuditPlansMock.mockResolvedValue([]);
    updateTaskMock.mockResolvedValue(task);
    deleteTaskMock.mockResolvedValue(undefined);
  });

  it('renders the authorized task and the complete admin toolset', async () => {
    listTasksMock.mockResolvedValue([task]);
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, task.itId);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(task.itTitle);
        expect(container.textContent).toContain(task.itProjectName);
        expect(container.textContent).toContain('Stewart Moreira');
        expect(container.textContent).toContain('Pendiente');
        expect(container.textContent).toContain('Entra a Estudio → Calendario');
        expect(container.textContent).toContain(task.itId);
        expect(container.textContent).toContain('Editar tarea');
        expect(container.textContent).toContain('Refrescar datos');
        expect(container.textContent).toContain('Copiar enlace');
        expect(container.textContent).toContain('Eliminar tarea');
        expect(container.querySelector('[aria-label="Avance de la tarea: 0%"]')).not.toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('lets an admin edit every maintainable task field', async () => {
    listTasksMock.mockResolvedValue([task]);
    const updatedTask = {
      ...task,
      itTitle: 'Agendar y verificar los ensayos',
      itDescription: 'Instrucciones actualizadas',
      itProgress: 25,
      itUpdatedAt: '2026-08-11T21:00:00Z',
    };
    updateTaskMock.mockResolvedValue(updatedTask);
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, task.itId);

    try {
      await waitForExpectation(() => expect(container.textContent).toContain('Editar tarea'));
      await clickButton(getButtonByText(container, 'Editar tarea'));

      await waitForExpectation(() => {
        expect(container.textContent).toContain('Administrar tarea');
        expect(container.textContent).toContain('Proyecto');
        expect(container.textContent).toContain('Responsable');
      });
      await setInputValue(getInputByLabel(container, 'Título'), updatedTask.itTitle);
      await setInputValue(getInputByLabel(container, 'Instrucciones'), updatedTask.itDescription);
      await setInputValue(getInputByLabel(container, 'Avance %'), '25');

      const form = container.querySelector('form');
      if (!(form instanceof HTMLFormElement)) throw new Error('Task edit form not found');
      await act(async () => {
        form.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(updateTaskMock).toHaveBeenCalledWith(task.itId, {
          ituProjectId: task.itProjectId,
          ituTitle: updatedTask.itTitle,
          ituDescription: updatedTask.itDescription,
          ituStatus: 'todo',
          ituProgress: 25,
          ituAssignedTo: 129,
          ituDueAt: '2026-08-14',
        });
        expect(container.textContent).toContain('La tarea se actualizó correctamente.');
        expect(container.textContent).toContain(updatedTask.itTitle);
      });
    } finally {
      await cleanup();
    }
  });

  it('limits an intern to status and progress updates', async () => {
    useSessionMock.mockReturnValue({ session: { roles: ['intern'], modules: ['internships'], partyId: 129 } });
    listTasksMock.mockResolvedValue([task]);
    updateTaskMock.mockResolvedValue({ ...task, itProgress: 40 });
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, task.itId);

    try {
      await waitForExpectation(() => expect(container.textContent).toContain('Actualizar avance'));
      expect(container.textContent).not.toContain('Eliminar tarea');
      await clickButton(getButtonByText(container, 'Actualizar avance'));
      expect(container.textContent).not.toContain('Edita la información, organización');
      expect(Array.from(container.querySelectorAll('label')).some((label) => label.textContent?.includes('Título'))).toBe(false);
      await setInputValue(getInputByLabel(container, 'Avance %'), '40');
      const form = container.querySelector('form');
      if (!(form instanceof HTMLFormElement)) throw new Error('Task progress form not found');
      await act(async () => {
        form.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(updateTaskMock).toHaveBeenCalledWith(task.itId, {
          ituStatus: 'todo',
          ituProgress: 40,
        });
      });
    } finally {
      await cleanup();
    }
  });

  it('routes audit-task lifecycle updates through the audit plan', async () => {
    useSessionMock.mockReturnValue({ session: { roles: ['intern'], modules: ['internships'], partyId: 129 } });
    listTasksMock.mockResolvedValue([task]);
    listAuditPlansMock.mockResolvedValue([{
      iapId: 'audit-plan-id',
      iapTaskId: task.itId,
      iapStatus: 'active',
    }]);
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, task.itId);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Abrir plan de pruebas');
        expect(container.textContent).not.toContain('Actualizar avance');
        expect(container.textContent).not.toContain('Editar tarea');
      });
      expect(updateTaskMock).not.toHaveBeenCalled();
    } finally {
      await cleanup();
    }
  });

  it('keeps audit status and calculated progress out of admin task updates', async () => {
    listTasksMock.mockResolvedValue([task]);
    listAuditPlansMock.mockResolvedValue([{
      iapId: 'audit-plan-id',
      iapTaskId: task.itId,
      iapStatus: 'active',
    }]);
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, task.itId);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Editar tarea');
        expect(container.textContent).toContain('Abrir plan de pruebas');
        expect(container.textContent).not.toContain('Eliminar tarea');
      });
      await clickButton(getButtonByText(container, 'Editar tarea'));
      await waitForExpectation(() => {
        expect(container.textContent).toContain('El estado y el avance se administran desde el plan de pruebas');
      });
      const statusControl = container.querySelector('[aria-labelledby="task-status-label"]');
      expect(statusControl?.getAttribute('aria-disabled')).toBe('true');
      expect(getInputByLabel(container, 'Avance %').disabled).toBe(true);
      const projectControl = container.querySelector('[aria-labelledby="task-project-label"]');
      const assigneeControl = container.querySelector('[aria-labelledby="task-assignee-label"]');
      expect(projectControl?.getAttribute('aria-disabled')).toBe('true');
      expect(assigneeControl?.getAttribute('aria-disabled')).toBe('true');

      const form = container.querySelector('form');
      if (!(form instanceof HTMLFormElement)) throw new Error('Task edit form not found');
      await act(async () => {
        form.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => expect(updateTaskMock).toHaveBeenCalledTimes(1));
      const payload = updateTaskMock.mock.calls[0]?.[1] as Record<string, unknown>;
      expect(payload).not.toHaveProperty('ituStatus');
      expect(payload).not.toHaveProperty('ituProgress');
      expect(payload).not.toHaveProperty('ituProjectId');
      expect(payload).not.toHaveProperty('ituAssignedTo');
    } finally {
      await cleanup();
    }
  });

  it('requires confirmation before an admin can permanently delete a task', async () => {
    listTasksMock.mockResolvedValue([task]);
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, task.itId);

    try {
      await waitForExpectation(() => expect(container.textContent).toContain('Eliminar tarea'));
      await clickButton(getButtonByText(container, 'Eliminar tarea'));
      await waitForExpectation(() => expect(document.body.textContent).toContain(task.itTitle));
      await clickButton(getButtonByText(document.body, 'Confirmar'));
      await waitForExpectation(() => {
        expect(deleteTaskMock).toHaveBeenCalledWith(task.itId);
        expect(container.textContent).toContain('Página de prácticas');
      });
    } finally {
      await cleanup();
    }
  });

  it('does not disclose details when the authorized task list does not contain the ID', async () => {
    listTasksMock.mockResolvedValue([]);
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, 'task-without-access');

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Tarea no encontrada');
        expect(container.textContent).toContain('La tarea no existe o no tienes permiso para verla.');
        expect(container.textContent).not.toContain(task.itTitle);
      });
    } finally {
      await cleanup();
    }
  });
});
