import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { BrainEntryDTO, BrainEntryCreate, BrainEntryUpdate, RagIndexStatus, RagRefreshResponse } from '../api/brain';

const listEntriesMock = jest.fn<(includeInactive?: boolean) => Promise<BrainEntryDTO[]>>();
const createEntryMock = jest.fn<(payload: BrainEntryCreate) => Promise<BrainEntryDTO>>();
const updateEntryMock = jest.fn<(entryId: number, payload: BrainEntryUpdate) => Promise<BrainEntryDTO>>();
const ragStatusMock = jest.fn<() => Promise<RagIndexStatus>>();
const ragRefreshMock = jest.fn<() => Promise<RagRefreshResponse>>();

jest.unstable_mockModule('../api/brain', () => ({
  Brain: {
    listEntries: (includeInactive?: boolean) => listEntriesMock(includeInactive),
    createEntry: (payload: BrainEntryCreate) => createEntryMock(payload),
    updateEntry: (entryId: number, payload: BrainEntryUpdate) => updateEntryMock(entryId, payload),
  },
  RagAdmin: {
    status: () => ragStatusMock(),
    refresh: () => ragRefreshMock(),
  },
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  getStoredSessionToken: () => null,
  useSession: () => ({
    session: {
      username: 'admin',
      displayName: 'Admin',
      roles: ['Admin'],
      modules: ['admin'],
    },
  }),
}));

jest.unstable_mockModule('../utils/logger', () => ({
  logger: {
    log: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

const { default: BrainAdminPage } = await import('./BrainAdminPage');

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
      <MemoryRouter
        initialEntries={['/configuracion/brain']}
        future={{ v7_startTransition: true, v7_relativeSplatPath: true }}
      >
        <QueryClientProvider client={qc}>
          <BrainAdminPage />
        </QueryClientProvider>
      </MemoryRouter>,
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

const buildEntry = (overrides: Partial<BrainEntryDTO> = {}): BrainEntryDTO => ({
  bedId: 101,
  bedTitle: 'Precios de estudio',
  bedBody: 'Informacion base sobre precios, salas y horarios.',
  bedCategory: 'pricing',
  bedTags: ['precios', 'estudio'],
  bedActive: true,
  bedUpdatedAt: '2030-01-03T12:00:00.000Z',
  ...overrides,
});

const createDeferred = <T,>() => {
  let resolve!: (value: T) => void;
  let reject!: (reason?: unknown) => void;
  const promise = new Promise<T>((promiseResolve, promiseReject) => {
    resolve = promiseResolve;
    reject = promiseReject;
  });
  return { promise, resolve, reject };
};

const getActionByText = (root: ParentNode, labelText: string) => {
  const action = Array.from(root.querySelectorAll<HTMLElement>('button, a')).find(
    (element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim() === labelText,
  );
  if (!(action instanceof HTMLElement)) throw new Error(`Action not found: ${labelText}`);
  return action;
};

const getCheckboxByLabelText = (root: ParentNode, labelText: string) => {
  const label = Array.from(root.querySelectorAll<HTMLLabelElement>('label')).find(
    (element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim() === labelText,
  );
  if (!label) throw new Error(`Checkbox label not found: ${labelText}`);
  const input = label.querySelector('input[type="checkbox"]');
  if (!(input instanceof HTMLInputElement)) throw new Error(`Checkbox not found: ${labelText}`);
  return input;
};

const clickAction = async (action: HTMLElement) => {
  await act(async () => {
    action.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
  });
};

const clickCheckbox = async (input: HTMLInputElement) => {
  await act(async () => {
    input.click();
    await flushPromises();
    await flushPromises();
  });
};

describe('BrainAdminPage', () => {
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
    listEntriesMock.mockReset();
    createEntryMock.mockReset();
    updateEntryMock.mockReset();
    ragStatusMock.mockReset();
    ragRefreshMock.mockReset();

    listEntriesMock.mockResolvedValue([]);
    createEntryMock.mockResolvedValue(buildEntry());
    updateEntryMock.mockResolvedValue(buildEntry());
    ragStatusMock.mockResolvedValue({
      risCount: 0,
      risUpdatedAt: null,
      risStale: false,
    });
    ragRefreshMock.mockResolvedValue({
      rrrStatus: 'ok',
      rrrChunks: 0,
    });
  });

  it('shows visible first-load notices while Brain data is loading', async () => {
    const entriesDeferred = createDeferred<BrainEntryDTO[]>();
    const ragStatusDeferred = createDeferred<RagIndexStatus>();
    listEntriesMock.mockReturnValue(entriesDeferred.promise);
    ragStatusMock.mockReturnValue(ragStatusDeferred.promise);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      expect(container.textContent).toContain('Cargando estado RAG');
      expect(container.textContent).toContain('Consultando el indice antes de mostrar acciones.');
      expect(container.textContent).toContain('Cargando entradas');
      expect(container.textContent).toContain('Estamos preparando las entradas del Brain.');
      expect(container.querySelectorAll('[role="status"][aria-busy="true"]')).toHaveLength(2);
      expect(container.querySelectorAll('[role="progressbar"]')).toHaveLength(2);
      expect(container.textContent).not.toContain(
        'No hay entradas activas. Crea la primera entrada del Brain o revisa inactivas si esperabas contenido archivado.',
      );
    } finally {
      await act(async () => {
        entriesDeferred.resolve([]);
        ragStatusDeferred.resolve({
          risCount: 0,
          risUpdatedAt: null,
          risStale: false,
        });
        await flushPromises();
      });
      await cleanup();
    }
  });

  it('keeps the empty Brain entry view focused until an admin asks to review inactive entries', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listEntriesMock).toHaveBeenCalledWith(false);
        expect(container.textContent).toContain(
          'No hay entradas activas. Crea la primera entrada del Brain o revisa inactivas si esperabas contenido archivado.',
        );
        expect(container.textContent).toContain(
          'Crea la primera entrada del Brain para activar este resumen. El refresco del indice aparecera cuando exista contenido para reindexar.',
        );
        expect(container.textContent).not.toContain('Chunks: 0');
        expect(container.textContent).not.toContain('Actualizado:');
        expect(container.textContent).not.toContain('OK');
        expect(container.textContent).not.toContain('Stale');
        expect(() => getActionByText(container, 'Refrescar indice')).toThrow();
        expect(container.textContent).not.toContain('Incluir inactivas');
        expect(getActionByText(container, 'Revisar inactivas')).toBeTruthy();
      });

      await clickAction(getActionByText(container, 'Revisar inactivas'));

      await waitForExpectation(() => {
        expect(listEntriesMock).toHaveBeenCalledWith(true);
        expect(container.textContent).toContain('Incluir inactivas');
        expect(container.textContent).toContain('No hay entradas cargadas, incluyendo inactivas.');
        expect(container.textContent).not.toContain(
          'No hay entradas activas. Crea la primera entrada del Brain o revisa inactivas si esperabas contenido archivado.',
        );
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the first-entry empty state hidden when entries fail to load', async () => {
    listEntriesMock.mockRejectedValue(new Error('brain entries unavailable'));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listEntriesMock).toHaveBeenCalledWith(false);
        expect(container.textContent).toContain('Error cargando entradas');
        expect(container.textContent).toContain('brain entries unavailable');
        expect(container.textContent).not.toContain(
          'No hay entradas activas. Crea la primera entrada del Brain o revisa inactivas si esperabas contenido archivado.',
        );
        expect(container.textContent).not.toContain('No hay entradas cargadas, incluyendo inactivas.');
        expect(() => getActionByText(container, 'Revisar inactivas')).toThrow();
      });
    } finally {
      await cleanup();
    }
  });

  it('shows the inactive filter when the Brain list has entries to filter', async () => {
    listEntriesMock.mockResolvedValue([buildEntry()]);
    ragStatusMock.mockResolvedValue({
      risCount: 12,
      risUpdatedAt: '2030-01-04T12:00:00.000Z',
      risStale: false,
    });

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listEntriesMock).toHaveBeenCalledWith(false);
        expect(container.textContent).toContain('Precios de estudio');
        expect(container.textContent).toContain('Ultima actualizacion y total de chunks indexados.');
        expect(container.textContent).toContain('Chunks: 12');
        expect(container.textContent).toContain('Actualizado:');
        expect(container.textContent).not.toContain('Actualizado: -');
        expect(container.textContent).toContain('OK');
        expect(container.querySelector('[data-testid="brain-admin-rag-index-pending"]')).toBeNull();
        expect(getActionByText(container, 'Refrescar indice')).toBeTruthy();
        expect(container.textContent).toContain('Incluir inactivas');
        expect(container.textContent).not.toContain('Revisar inactivas');
      });
    } finally {
      await cleanup();
    }
  });

  it('replaces unbuilt RAG index placeholders with one next-step hint once entries exist', async () => {
    listEntriesMock.mockResolvedValue([buildEntry()]);
    ragStatusMock.mockResolvedValue({
      risCount: 0,
      risUpdatedAt: null,
      risStale: true,
    });

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('[data-testid="brain-admin-rag-index-pending"]')).not.toBeNull();
        expect(container.textContent).toContain(
          'El indice todavia no tiene chunks. Usa Refrescar indice cuando termines de revisar entradas.',
        );
        expect(getActionByText(container, 'Refrescar indice')).toBeTruthy();
        expect(container.textContent).not.toContain('Chunks: 0');
        expect(container.textContent).not.toContain('Actualizado: -');
        expect(container.textContent).not.toContain('OK');
        expect(container.textContent).not.toContain('Stale');
      });
    } finally {
      await cleanup();
    }
  });

  it('omits repeated active chips until inactive entries are included', async () => {
    listEntriesMock.mockImplementation((includeInactive = false) => Promise.resolve(
      includeInactive
        ? [
            buildEntry({ bedId: 101, bedTitle: 'Precios de estudio', bedActive: true }),
            buildEntry({ bedId: 102, bedTitle: 'Politicas archivadas', bedActive: false }),
          ]
        : [
            buildEntry({ bedId: 101, bedTitle: 'Precios de estudio', bedActive: true }),
            buildEntry({
              bedId: 102,
              bedTitle: 'Horarios de sala',
              bedCategory: 'scheduling',
              bedTags: ['horarios'],
              bedActive: true,
            }),
          ],
    ));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listEntriesMock).toHaveBeenCalledWith(false);
        expect(container.textContent).toContain('Precios de estudio');
        expect(container.textContent).toContain('Horarios de sala');
        expect(container.textContent).toContain('Incluir inactivas');
        expect(container.textContent).not.toContain('Activa');
        expect(container.textContent).not.toContain('Inactiva');
      });

      await clickCheckbox(getCheckboxByLabelText(container, 'Incluir inactivas'));

      await waitForExpectation(() => {
        expect(listEntriesMock).toHaveBeenCalledWith(true);
        expect(container.textContent).toContain('Precios de estudio');
        expect(container.textContent).toContain('Politicas archivadas');
        expect(container.textContent).toContain('Activa');
        expect(container.textContent).toContain('Inactiva');
      });
    } finally {
      await cleanup();
    }
  });

  it('replaces the one-entry list card with a first-entry summary so the next step stays obvious', async () => {
    listEntriesMock.mockResolvedValue([buildEntry()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Primera entrada del Brain');
        expect(container.textContent).toContain(
          'Revisa esta entrada desde un resumen simple. Cuando exista la segunda, volvera la lista completa para comparar titulo, categoria y actualizacion.',
        );
        expect(container.textContent).toContain('Precios de estudio');
        expect(container.textContent).toContain('Categoria: pricing');
        expect(container.textContent).toContain('Tags: precios, estudio');
        expect(container.textContent).toContain('Actualizado:');
        expect(getActionByText(container, 'Editar entrada')).toBeTruthy();
        expect(() => getActionByText(container, 'Editar')).toThrow();
        expect(container.textContent).not.toContain('Activa');
      });
    } finally {
      await cleanup();
    }
  });
});
