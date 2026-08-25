import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import type { PartyCreate, PartyDTO, PartyUpdate } from '../api/types';

const listPartiesMock = jest.fn<() => Promise<PartyDTO[]>>();
const createPartyMock = jest.fn<(body: PartyCreate) => Promise<PartyDTO>>();
const updatePartyMock = jest.fn<(id: number, body: PartyUpdate) => Promise<PartyDTO | null>>();

jest.unstable_mockModule('../api/parties', () => ({
  Parties: {
    list: () => listPartiesMock(),
    create: (body: PartyCreate) => createPartyMock(body),
    update: (id: number, body: PartyUpdate) => updatePartyMock(id, body),
  },
}));

jest.unstable_mockModule('../components/PartyRelatedPopover', () => ({
  default: () => null,
}));

const { default: LeadsPage } = await import('./LeadsPage');

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
        <LeadsPage />
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

const buildLead = (overrides: Partial<PartyDTO> = {}): PartyDTO => ({
  partyId: 1,
  displayName: 'Ada Lovelace',
  isOrg: false,
  primaryEmail: 'ada@example.com',
  primaryPhone: '+593999000111',
  notes: null,
  hasUserAccount: false,
  ...overrides,
});

const getSearchInput = (root: ParentNode) => {
  const input = root.querySelector('input[aria-label="Buscar leads"]');
  if (!(input instanceof HTMLInputElement)) {
    throw new Error('Search input not found');
  }
  return input;
};

const setInputValue = async (input: HTMLInputElement, value: string) => {
  const descriptor = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value');
  if (descriptor?.set) {
    descriptor.set.call(input, value);
  } else {
    input.value = value;
  }

  await act(async () => {
    input.dispatchEvent(new Event('input', { bubbles: true }));
    input.dispatchEvent(new Event('change', { bubbles: true }));
    await flushPromises();
    await flushPromises();
  });
};

const clickButtonByAriaLabel = async (root: ParentNode, labelText: string) => {
  const button = root.querySelector(`button[aria-label="${labelText}"]`);
  if (!(button instanceof HTMLButtonElement)) {
    throw new Error(`Button not found: ${labelText}`);
  }

  await act(async () => {
    button.click();
    await flushPromises();
    await flushPromises();
  });
};

const countButtonsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('button')).filter(
    (element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim() === labelText,
  ).length;

const countOccurrences = (root: ParentNode, text: string) =>
  (root.textContent ?? '').split(text).length - 1;

const getColumnHeaders = (root: ParentNode) =>
  Array.from(root.querySelectorAll('th')).map((element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim());

const getRowCellTexts = (row: HTMLTableRowElement) =>
  Array.from(row.querySelectorAll('td')).map((element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim());

describe('LeadsPage', () => {
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
    listPartiesMock.mockReset();
    createPartyMock.mockReset();
    updatePartyMock.mockReset();
    createPartyMock.mockResolvedValue(buildLead());
    updatePartyMock.mockResolvedValue(null);
    listPartiesMock.mockResolvedValue([]);
  });

  it('keeps the first lead load focused on one loading notice instead of early search and table chrome', async () => {
    let resolveLeads: ((value: PartyDTO[]) => void) | null = null;
    const pendingLeads = new Promise<PartyDTO[]>((resolve) => {
      resolveLeads = resolve;
    });
    listPartiesMock.mockReturnValue(pendingLeads);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Cargando leads… El buscador y la tabla aparecerán cuando termine esta primera carga.',
        );
        expect(container.querySelector('input[aria-label="Buscar leads"]')).toBeNull();
        expect(container.querySelector('table')).toBeNull();
      });

      await act(async () => {
        resolveLeads?.([
          buildLead(),
          buildLead({
            partyId: 2,
            displayName: 'Grace Hopper',
            primaryEmail: 'grace@example.com',
            primaryPhone: '+593999000222',
          }),
        ]);
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(container.textContent).not.toContain(
          'Cargando leads… El buscador y la tabla aparecerán cuando termine esta primera carga.',
        );
        expect(getSearchInput(container).getAttribute('placeholder')).toBe('Buscar por nombre, correo, teléfono o nota');
        expect(container.querySelector('table')).not.toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('replaces the empty leads table with a first-lead empty state', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('input[aria-label="Buscar leads"]')).toBeNull();
        expect(container.querySelector('table')).toBeNull();
        expect(container.textContent).toContain(
          'Todavía no hay leads. Crea el primero desde Nuevo lead. El primer lead aparecerá aquí como resumen y la tabla volverá cuando exista un segundo para comparar.',
        );
      });
    } finally {
      await cleanup();
    }
  });

  it('uses one combined contact column in the multi-lead table', async () => {
    listPartiesMock.mockResolvedValue([
      buildLead({
        partyId: 1,
        displayName: 'Ada Lovelace',
        primaryEmail: 'ada@example.com',
        primaryPhone: '+593999000111',
      }),
      buildLead({
        partyId: 2,
        displayName: 'Grace Hopper',
        primaryEmail: 'grace@example.com',
        primaryPhone: null,
        notes: 'Contactado · Referido',
      }),
      buildLead({
        partyId: 3,
        displayName: 'Linus Missing',
        primaryEmail: null,
        primaryPhone: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const bodyRows = Array.from(container.querySelectorAll<HTMLTableRowElement>('tbody tr'));

        expect(getColumnHeaders(container)).toEqual(['Lead', 'Contacto', 'Notas / Estado', 'Acciones']);
        expect(container.textContent).toContain('Contacto reúne correo y teléfono en una sola columna.');
        expect(bodyRows).toHaveLength(3);
        expect(getRowCellTexts(bodyRows[0] ?? document.createElement('tr'))).toEqual([
          'Ada Lovelace',
          'ada@example.com · +593999000111',
          '—',
          'Editar',
        ]);
        expect(getRowCellTexts(bodyRows[1] ?? document.createElement('tr'))).toEqual([
          'Grace Hopper',
          'grace@example.com',
          'Contactado · Referido',
          'Editar',
        ]);
        expect(getRowCellTexts(bodyRows[2] ?? document.createElement('tr'))).toEqual([
          'Linus Missing',
          'Falta correo y teléfono',
          '—',
          'Editar',
        ]);
      });
    } finally {
      await cleanup();
    }
  });

  it('replaces an all-empty notes column with one setup hint', async () => {
    listPartiesMock.mockResolvedValue([
      buildLead({
        partyId: 1,
        displayName: 'Ada Lovelace',
        primaryEmail: 'ada@example.com',
        primaryPhone: '+593999000111',
        notes: null,
      }),
      buildLead({
        partyId: 2,
        displayName: 'Grace Hopper',
        primaryEmail: 'grace@example.com',
        primaryPhone: '+593999000222',
        notes: '',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const bodyRows = Array.from(container.querySelectorAll<HTMLTableRowElement>('tbody tr'));

        expect(getColumnHeaders(container)).toEqual(['Lead', 'Contacto', 'Acciones']);
        expect(container.textContent).toContain(
          'Haz clic en el nombre para revisar relaciones. Contacto reúne correo y teléfono en una sola columna. Usa Editar solo cuando necesites actualizarlo.',
        );
        expect(container.textContent).toContain(
          'Todavía no hay notas ni estado en esta vista. La columna volverá cuando algún lead tenga estado, fuente o siguiente paso.',
        );
        expect(container.textContent).not.toContain('Notas / Estado concentra estado');
        expect(bodyRows).toHaveLength(2);
        expect(getRowCellTexts(bodyRows[0] ?? document.createElement('tr'))).toEqual([
          'Ada Lovelace',
          'ada@example.com · +593999000111',
          'Editar',
        ]);
        expect(getRowCellTexts(bodyRows[1] ?? document.createElement('tr'))).toEqual([
          'Grace Hopper',
          'grace@example.com · +593999000222',
          'Editar',
        ]);
        expect(countOccurrences(container, 'Todavía no hay notas ni estado en esta vista.')).toBe(1);
      });
    } finally {
      await cleanup();
    }
  });

  it('collapses a narrowed lead search into one summary card and restores the full list in one clear step', async () => {
    listPartiesMock.mockResolvedValue([
      buildLead({
        partyId: 1,
        displayName: 'Ada Lovelace',
        primaryEmail: 'ada@example.com',
        primaryPhone: '+593999000111',
      }),
      buildLead({
        partyId: 2,
        displayName: 'Grace Hopper',
        primaryEmail: 'grace@example.com',
        primaryPhone: '+593999000222',
        notes: 'Contactado · Referido',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getSearchInput(container).value).toBe('');
        expect(container.querySelector('table')).not.toBeNull();
        expect(container.textContent).toContain('Ada Lovelace');
        expect(container.textContent).toContain('Grace Hopper');
      });

      await setInputValue(getSearchInput(container), 'ada@example.com');

      await waitForExpectation(() => {
        expect(container.querySelector('table')).toBeNull();
        expect(container.textContent).toContain('1 coincidencia para "ada@example.com"');
        expect(container.textContent).toContain(
          'La búsqueda dejó un solo lead visible. Revísalo aquí; limpia o ajusta el buscador para volver a comparar leads en la tabla.',
        );
        expect(container.textContent).toContain('Contacto: ada@example.com · +593999000111');
        expect(container.textContent).toContain(
          'Todavía no hay contexto registrado. Usa Editar lead para agregar estado, fuente o siguiente paso.',
        );
        expect(countButtonsByText(container, 'Editar')).toBe(0);
        expect(countButtonsByText(container, 'Editar lead')).toBe(1);
        expect(container.querySelectorAll('button[aria-label="Limpiar búsqueda"]')).toHaveLength(1);
        expect(container.textContent).not.toContain('Grace Hopper');
      });

      await clickButtonByAriaLabel(container, 'Limpiar búsqueda');

      await waitForExpectation(() => {
        expect(getSearchInput(container).value).toBe('');
        expect(container.querySelector('table')).not.toBeNull();
        expect(container.textContent).not.toContain('1 coincidencia para "ada@example.com"');
        expect(container.textContent).toContain('Ada Lovelace');
        expect(container.textContent).toContain('Grace Hopper');
      });
    } finally {
      await cleanup();
    }
  });
});
