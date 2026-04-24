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

const getActionByText = (root: ParentNode, labelText: string) => {
  const action = Array.from(root.querySelectorAll<HTMLElement>('button, a')).find(
    (element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim() === labelText,
  );
  if (!(action instanceof HTMLElement)) throw new Error(`Action not found: ${labelText}`);
  return action;
};

const clickAction = async (action: HTMLElement) => {
  await act(async () => {
    action.dispatchEvent(new MouseEvent('click', { bubbles: true }));
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

  it('shows the inactive filter when the Brain list has entries to filter', async () => {
    listEntriesMock.mockResolvedValue([buildEntry()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listEntriesMock).toHaveBeenCalledWith(false);
        expect(container.textContent).toContain('Precios de estudio');
        expect(container.textContent).toContain('Ultima actualizacion y total de chunks indexados.');
        expect(container.textContent).toContain('Chunks: 0');
        expect(container.textContent).toContain('Actualizado: -');
        expect(container.textContent).toContain('OK');
        expect(getActionByText(container, 'Refrescar indice')).toBeTruthy();
        expect(container.textContent).toContain('Incluir inactivas');
        expect(container.textContent).not.toContain('Revisar inactivas');
      });
    } finally {
      await cleanup();
    }
  });
});
