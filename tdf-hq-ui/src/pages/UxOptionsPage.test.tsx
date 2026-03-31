import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { DropdownOptionDTO } from '../api/types';

const listDropdownsMock = jest.fn<(category: string, includeInactive?: boolean) => Promise<DropdownOptionDTO[]>>();
const createDropdownMock = jest.fn<(category: string, payload: unknown) => Promise<DropdownOptionDTO>>();
const updateDropdownMock = jest.fn<(category: string, optionId: string, payload: unknown) => Promise<DropdownOptionDTO>>();

jest.unstable_mockModule('../api/admin', () => ({
  Admin: {
    listDropdowns: (category: string, includeInactive?: boolean) => listDropdownsMock(category, includeInactive),
    createDropdown: (category: string, payload: unknown) => createDropdownMock(category, payload),
    updateDropdown: (category: string, optionId: string, payload: unknown) =>
      updateDropdownMock(category, optionId, payload),
  },
}));

const { default: UxOptionsPage } = await import('./UxOptionsPage');

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

const renderPage = async (container: HTMLElement, initialEntry = '/configuracion/ux-opciones') => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter
        initialEntries={[initialEntry]}
        future={{ v7_startTransition: true, v7_relativeSplatPath: true }}
      >
        <QueryClientProvider client={qc}>
          <UxOptionsPage />
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

const buttonText = (element: Element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim();

const getButtonsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll<HTMLButtonElement>('button')).filter((element) => buttonText(element) === labelText);

const clickButton = async (button: HTMLButtonElement) => {
  await act(async () => {
    button.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
  });
};

const buildOption = (overrides: Partial<DropdownOptionDTO> = {}): DropdownOptionDTO => ({
  optionId: 'band-genre-1',
  category: 'band-genre',
  value: 'rock',
  label: 'Rock',
  active: true,
  sortOrder: 1,
  ...overrides,
});

describe('UxOptionsPage', () => {
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
    listDropdownsMock.mockReset();
    createDropdownMock.mockReset();
    updateDropdownMock.mockReset();
    createDropdownMock.mockResolvedValue(buildOption());
    updateDropdownMock.mockResolvedValue(buildOption());
  });

  it('keeps the create form collapsed when a category already has options, then reveals it on demand', async () => {
    listDropdownsMock.mockResolvedValue([buildOption()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listDropdownsMock).toHaveBeenCalledWith('asset-category', false);
        expect(container.textContent).toContain('Nueva opción');
        expect(container.textContent).toContain('Abre este formulario solo cuando necesites agregar un valor nuevo a esta categoría.');
        expect(getButtonsByText(container, 'Agregar opción')).toHaveLength(1);
        expect(getButtonsByText(container, 'Agregar')).toHaveLength(0);
      });

      await clickButton(getButtonsByText(container, 'Agregar opción')[0]!);

      await waitForExpectation(() => {
        expect(getButtonsByText(container, 'Ocultar formulario')).toHaveLength(1);
        expect(getButtonsByText(container, 'Agregar')).toHaveLength(1);
      });
    } finally {
      await cleanup();
    }
  });

  it('opens the create form automatically when the category is empty so the first next step is obvious', async () => {
    listDropdownsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Primera opción');
        expect(container.textContent).toContain(
          'Esta categoría todavía no tiene opciones. Crea la primera para habilitarla en formularios.',
        );
        expect(container.textContent).toContain('No hay opciones aún para esta categoría.');
        expect(container.textContent).not.toContain('Filtrar opciones');
        expect(container.textContent).not.toContain('0 totales · 0 activas');
        expect(getButtonsByText(container, 'Agregar opción')).toHaveLength(0);
        expect(getButtonsByText(container, 'Agregar')).toHaveLength(1);
      });
    } finally {
      await cleanup();
    }
  });

  it('hides list-only filter chrome until a category has more than one saved option', async () => {
    listDropdownsMock.mockResolvedValue([buildOption()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Solo hay una opción por ahora. Edítala directo aquí; el filtro aparecerá cuando exista una segunda.',
        );
        expect(container.textContent).not.toContain('Filtrar opciones');
        expect(container.textContent).not.toContain('1 totales · 1 activas');
        expect(container.textContent).toContain('Valor');
        expect(container.textContent).toContain('Etiqueta');
      });
    } finally {
      await cleanup();
    }
  });
});
