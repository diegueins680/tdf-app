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

const getButtonByAriaLabel = (root: ParentNode, labelText: string) =>
  root.querySelector<HTMLButtonElement>(`button[aria-label="${labelText}"]`);

const getInputByLabel = (container: HTMLElement, labelText: string) => {
  const label = Array.from(container.querySelectorAll('label')).find(
    (element) => buttonText(element) === labelText,
  );
  if (!label) throw new Error(`Label not found: ${labelText}`);

  const inputId = label.getAttribute('for');
  if (inputId) {
    const input = document.getElementById(inputId);
    if (input instanceof HTMLInputElement || input instanceof HTMLTextAreaElement) return input;
  }

  const fallback = label.parentElement?.querySelector<HTMLInputElement | HTMLTextAreaElement>('input,textarea');
  if (!fallback) throw new Error(`Input not found for label: ${labelText}`);
  return fallback;
};

const setInputValue = (input: HTMLInputElement | HTMLTextAreaElement, value: string) => {
  const prototype =
    input instanceof HTMLTextAreaElement ? HTMLTextAreaElement.prototype : HTMLInputElement.prototype;
  const descriptor = Object.getOwnPropertyDescriptor(prototype, 'value');
  if (descriptor?.set) {
    descriptor.set.call(input, value);
  } else {
    input.value = value;
  }
  input.dispatchEvent(new Event('input', { bubbles: true }));
  input.dispatchEvent(new Event('change', { bubbles: true }));
};

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

  it('shows row save actions only after a specific option is edited', async () => {
    listDropdownsMock.mockResolvedValue([buildOption()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Guardar y Revertir aparecen solo en la fila que editas.');
        expect(getButtonsByText(container, 'Guardar')).toHaveLength(0);
        expect(getButtonsByText(container, 'Revertir')).toHaveLength(0);
      });

      const labelInput = getInputByLabel(container, 'Etiqueta');

      await act(async () => {
        setInputValue(labelInput, 'Rock alternativo');
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(getButtonsByText(container, 'Guardar')).toHaveLength(1);
        expect(getButtonsByText(container, 'Revertir')).toHaveLength(1);
      });

      await clickButton(getButtonsByText(container, 'Revertir')[0]!);

      await waitForExpectation(() => {
        expect((labelInput as HTMLInputElement | HTMLTextAreaElement).value).toBe('Rock');
        expect(getButtonsByText(container, 'Guardar')).toHaveLength(0);
        expect(getButtonsByText(container, 'Revertir')).toHaveLength(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the filter clear action inside the field so the list toolbar stays compact', async () => {
    listDropdownsMock.mockResolvedValue([
      buildOption(),
      buildOption({
        optionId: 'band-genre-2',
        value: 'jazz',
        label: 'Jazz',
        sortOrder: 2,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Filtrar opciones');
        expect(getButtonsByText(container, 'Limpiar filtro')).toHaveLength(0);
        expect(getButtonByAriaLabel(container, 'Limpiar filtro')).toBeNull();
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
      });

      const filterInput = getInputByLabel(container, 'Filtrar opciones');

      await act(async () => {
        setInputValue(filterInput, 'jazz');
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(getButtonsByText(container, 'Limpiar filtro')).toHaveLength(0);
        expect(getButtonByAriaLabel(container, 'Limpiar filtro')).toBeTruthy();
        expect(container.querySelectorAll('tbody tr')).toHaveLength(1);
      });

      await clickButton(getButtonByAriaLabel(container, 'Limpiar filtro')!);

      await waitForExpectation(() => {
        expect((filterInput as HTMLInputElement | HTMLTextAreaElement).value).toBe('');
        expect(getButtonByAriaLabel(container, 'Limpiar filtro')).toBeNull();
        expect(container.querySelectorAll('tbody tr')).toHaveLength(2);
      });
    } finally {
      await cleanup();
    }
  });
});
