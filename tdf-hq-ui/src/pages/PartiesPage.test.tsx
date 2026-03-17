import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
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

jest.unstable_mockModule('../api/admin', () => ({
  Admin: {
    createUser: jest.fn(() => Promise.resolve(null)),
  },
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: null }),
}));

jest.unstable_mockModule('../utils/accessControl', () => ({
  canAccessPath: () => false,
}));

jest.unstable_mockModule('../components/PartyRelatedPopover', () => ({
  default: () => null,
}));

const { default: PartiesPage } = await import('./PartiesPage');

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

const buttonText = (element: Element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim();

const getButtonsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('button')).filter((element) => buttonText(element) === labelText) as HTMLButtonElement[];

const clickButton = (button: HTMLButtonElement) => {
  button.dispatchEvent(new MouseEvent('click', { bubbles: true }));
};

const setInputValue = (input: HTMLInputElement, value: string) => {
  const descriptor = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value');
  if (descriptor?.set) {
    descriptor.set.call(input, value);
  } else {
    input.value = value;
  }
  input.dispatchEvent(new Event('input', { bubbles: true }));
  input.dispatchEvent(new Event('change', { bubbles: true }));
};

const getColumnHeaders = (root: ParentNode) =>
  Array.from(root.querySelectorAll('th')).map((element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim());

const getRowCellTexts = (row: HTMLTableRowElement) =>
  Array.from(row.querySelectorAll('td')).map((element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim());

const renderPage = async (container: HTMLElement) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter future={{ v7_startTransition: true, v7_relativeSplatPath: true }}>
        <QueryClientProvider client={qc}>
          <PartiesPage />
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

describe('PartiesPage', () => {
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
    listPartiesMock.mockResolvedValue([]);
    createPartyMock.mockResolvedValue({
      partyId: 1,
      displayName: 'Nuevo contacto',
      isOrg: false,
    } satisfies PartyDTO);
    updatePartyMock.mockResolvedValue(null);
  });

  it('replaces empty CRM chrome with a first-contact empty state', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getButtonsByText(document.body, 'Nuevo contacto')).toHaveLength(1);
        expect(container.querySelector('input[aria-label="Buscar contactos"]')).toBeNull();
        expect(container.querySelector('table')).toBeNull();
        expect(getColumnHeaders(container)).toEqual([]);
        expect(container.textContent).toContain(
          'Todavía no hay contactos. Crea el primero desde Nuevo contacto. El buscador y la tabla aparecerán cuando exista al menos un contacto.',
        );
      });
    } finally {
      await cleanup();
    }
  });

  it('uses a single create CTA and keeps the person-vs-company choice inside the dialog', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getButtonsByText(document.body, 'Nuevo contacto')).toHaveLength(1);
        expect(getButtonsByText(document.body, 'Nueva banda')).toHaveLength(0);
        expect(getButtonsByText(document.body, 'Nueva persona')).toHaveLength(0);
        expect(document.body.textContent).toContain('El tipo se elige dentro del formulario de alta.');
      });

      await act(async () => {
        clickButton(getButtonsByText(document.body, 'Nuevo contacto')[0]!);
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain(
          'Usa Persona para individuos y Empresa para bandas, sellos o negocios.',
        );
        expect(getButtonsByText(document.body, 'Cambiar a empresa')).toHaveLength(1);
      });

      await act(async () => {
        clickButton(getButtonsByText(document.body, 'Cambiar a empresa')[0]!);
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(getButtonsByText(document.body, 'Cambiar a persona')).toHaveLength(1);
      });
    } finally {
      await cleanup();
    }
  });

  it('uses one plain-language company indicator instead of a duplicated org column', async () => {
    listPartiesMock.mockResolvedValue([
      {
        partyId: 1,
        displayName: 'Los Navegantes',
        isOrg: true,
        primaryEmail: 'hola@navegantes.test',
        instagram: '@navegantes',
      } satisfies PartyDTO,
      {
        partyId: 2,
        displayName: 'Ada Lovelace',
        isOrg: false,
        primaryEmail: 'ada@example.com',
        instagram: '@ada',
      } satisfies PartyDTO,
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const bodyRows = Array.from(container.querySelectorAll('tbody tr')) as HTMLTableRowElement[];
        expect(getColumnHeaders(container)).toEqual(['Nombre', 'Email', 'Instagram', 'Acciones']);
        expect(bodyRows).toHaveLength(2);
        expect(container.textContent).toContain('Los Navegantes');
        expect(container.textContent).toContain('Empresa');
        expect(container.textContent).not.toContain('ORG');
        expect(getRowCellTexts(bodyRows[0] ?? document.createElement('tr'))).toEqual([
          'Los NavegantesEmpresa',
          'hola@navegantes.test',
          '@navegantes',
          '',
        ]);
        expect(getRowCellTexts(bodyRows[1] ?? document.createElement('tr'))).toEqual([
          'Ada Lovelace',
          'ada@example.com',
          '@ada',
          '',
        ]);
      });
    } finally {
      await cleanup();
    }
  });

  it('replaces a no-match search table with one reset state and restores the full list in one step', async () => {
    listPartiesMock.mockResolvedValue([
      {
        partyId: 1,
        displayName: 'Los Navegantes',
        isOrg: true,
        primaryEmail: 'hola@navegantes.test',
        instagram: '@navegantes',
      } satisfies PartyDTO,
      {
        partyId: 2,
        displayName: 'Ada Lovelace',
        isOrg: false,
        primaryEmail: 'ada@example.com',
        instagram: '@ada',
      } satisfies PartyDTO,
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const searchInput = container.querySelector('input[aria-label="Buscar contactos"]') as HTMLInputElement | null;
        expect(searchInput).not.toBeNull();
        expect(searchInput?.getAttribute('placeholder')).toBe('Buscar por nombre, email o Instagram');
        expect(container.querySelector('table')).not.toBeNull();
        expect(container.textContent).toContain('Los Navegantes');
        expect(container.textContent).toContain('Ada Lovelace');
      });

      await act(async () => {
        setInputValue(container.querySelector('input[aria-label="Buscar contactos"]') as HTMLInputElement, 'ada@example.com');
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(container.querySelector('table')).not.toBeNull();
        expect(container.textContent).toContain('Ada Lovelace');
        expect(container.textContent).not.toContain('Los Navegantes');
        expect(container.textContent).not.toContain('No hay contactos que coincidan con');
      });

      await act(async () => {
        setInputValue(container.querySelector('input[aria-label="Buscar contactos"]') as HTMLInputElement, 'sin-coincidencias');
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(container.querySelector('table')).toBeNull();
        expect(container.textContent).toContain(
          'No hay contactos que coincidan con "sin-coincidencias". Limpia la búsqueda para volver a ver toda la lista.',
        );
        expect(getButtonsByText(container, 'Limpiar búsqueda')).toHaveLength(1);
      });

      await act(async () => {
        clickButton(getButtonsByText(container, 'Limpiar búsqueda')[0]!);
        await flushPromises();
      });

      await waitForExpectation(() => {
        const searchInput = container.querySelector('input[aria-label="Buscar contactos"]') as HTMLInputElement | null;
        expect(searchInput?.value).toBe('');
        expect(container.querySelector('table')).not.toBeNull();
        expect(container.textContent).toContain('Los Navegantes');
        expect(container.textContent).toContain('Ada Lovelace');
        expect(container.textContent).not.toContain('No hay contactos que coincidan con');
      });
    } finally {
      await cleanup();
    }
  });
});
