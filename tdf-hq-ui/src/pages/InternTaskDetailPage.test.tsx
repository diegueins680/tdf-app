import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, Route, Routes } from 'react-router-dom';

const listTasksMock = jest.fn<() => Promise<unknown[]>>();

jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({
    t: (key: string) => (key === 'internships.taskDetail.instructions' ? 'Instrucciones' : key),
  }),
}));

jest.unstable_mockModule('../api/internships', () => ({
  Internships: {
    listTasks: () => listTasksMock(),
  },
}));

const { default: InternTaskDetailPage } = await import('./InternTaskDetailPage');

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

const renderPage = async (container: HTMLElement, taskId: string) => {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <QueryClientProvider client={queryClient}>
        <MemoryRouter initialEntries={[`/practicas/tareas/${taskId}`]}>
          <Routes>
            <Route path="/practicas/tareas/:taskId" element={<InternTaskDetailPage />} />
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
  });

  it('renders the authorized task selected by the route ID', async () => {
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
        expect(container.textContent).toContain('90 minutos');
        expect(container.textContent).toContain(task.itId);
        expect(container.querySelector('[aria-label="Avance de la tarea: 0%"]')).not.toBeNull();
        expect(container.querySelector<HTMLAnchorElement>('a[href="/practicas"]')?.textContent).toContain(
          'Volver a Prácticas',
        );
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
