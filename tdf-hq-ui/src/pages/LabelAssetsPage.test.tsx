import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { AssetCheckoutDTO, AssetDTO, DropdownOptionDTO, RoomDTO } from '../api/types';

const listAssetsMock = jest.fn<() => Promise<AssetDTO[]>>();
const listRoomsMock = jest.fn<() => Promise<RoomDTO[]>>();
const listDropdownsMock = jest.fn<() => Promise<DropdownOptionDTO[]>>();
const historyMock = jest.fn<(assetId: string) => Promise<AssetCheckoutDTO[]>>();

jest.unstable_mockModule('../api/inventory', () => ({
  Inventory: {
    list: () => listAssetsMock(),
    history: (assetId: string) => historyMock(assetId),
    generateQr: jest.fn(() => Promise.resolve({ qrUrl: 'https://example.com/qr' })),
    checkout: jest.fn(() => Promise.resolve(null)),
    checkin: jest.fn(() => Promise.resolve(null)),
    create: jest.fn(() => Promise.resolve(null)),
    update: jest.fn(() => Promise.resolve(null)),
    remove: jest.fn(() => Promise.resolve(null)),
  },
}));

jest.unstable_mockModule('../api/rooms', () => ({
  Rooms: {
    list: () => listRoomsMock(),
  },
}));

jest.unstable_mockModule('../api/admin', () => ({
  Admin: {
    listDropdowns: () => listDropdownsMock(),
  },
}));

jest.unstable_mockModule('../components/AssetDialogs', () => ({
  CheckoutDialog: () => null,
  CheckinDialog: () => null,
}));

jest.unstable_mockModule('../components/GoogleDriveUploadWidget', () => ({
  default: () => null,
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: null }),
  getStoredSessionToken: () => null,
  getActiveSession: () => null,
  setTransientApiToken: () => undefined,
  SESSION_STORAGE_KEY: 'tdf-hq-ui/session',
}));

jest.unstable_mockModule('../utils/accessControl', () => ({
  buildAccessibleModuleSet: () => new Set<string>(),
}));

const { default: LabelAssetsPage } = await import('./LabelAssetsPage');

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

const buildAsset = (overrides: Partial<AssetDTO> = {}): AssetDTO => ({
  assetId: 'asset-1',
  name: 'Sintetizador Uno',
  category: 'Synth',
  status: 'Active',
  condition: 'ok',
  brand: 'Roland',
  model: 'Juno',
  location: null,
  qrToken: 'qr-1',
  photoUrl: null,
  ...overrides,
});

const buildRoom = (overrides: Partial<RoomDTO> = {}): RoomDTO => ({
  roomId: 'room-a',
  rName: 'Sala A',
  rBookable: true,
  ...overrides,
});

const renderPage = async (container: HTMLElement) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter future={{ v7_startTransition: true, v7_relativeSplatPath: true }}>
        <QueryClientProvider client={qc}>
          <LabelAssetsPage />
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

const countLabelsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('label')).filter((label) => (label.textContent ?? '').trim() === labelText).length;

const hasTableHeader = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('th')).some((cell) => (cell.textContent ?? '').trim() === labelText);

const getInputByLabel = (root: ParentNode, labelText: string) => {
  const labels = Array.from(root.querySelectorAll('label'));
  const label = labels.find((el) => {
    const text = (el.textContent ?? '').replace('*', '').trim();
    return text === labelText;
  });
  if (!label) throw new Error(`Label not found: ${labelText}`);
  const forId = label.getAttribute('for');
  if (forId) {
    const input = document.getElementById(forId);
    if (input instanceof HTMLInputElement) return input;
  }
  const fallback = label.parentElement?.querySelector<HTMLInputElement>('input,textarea');
  if (!fallback) throw new Error(`Input not found for label: ${labelText}`);
  return fallback;
};

const setInputValue = async (input: HTMLInputElement, value: string) => {
  await act(async () => {
    const descriptor = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value');
    if (descriptor?.set) {
      descriptor.set.call(input, value);
    } else {
      input.value = value;
    }
    input.dispatchEvent(new Event('input', { bubbles: true }));
    input.dispatchEvent(new Event('change', { bubbles: true }));
    await flushPromises();
  });
};

const getElementByAriaLabel = (root: ParentNode, labelText: string) => {
  const element = root.querySelector(`[aria-label="${labelText}"]`);
  if (!(element instanceof HTMLElement)) {
    throw new Error(`Element not found: ${labelText}`);
  }
  return element;
};

const queryButtonByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('button')).find((candidate) => (candidate.textContent ?? '').trim() === labelText) ?? null;

const countButtonsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('button')).filter((candidate) => (candidate.textContent ?? '').trim() === labelText).length;

const countOccurrences = (haystack: string, needle: string) => haystack.split(needle).length - 1;

const getButtonByText = (root: ParentNode, labelText: string) => {
  const button = queryButtonByText(root, labelText);
  if (!(button instanceof HTMLButtonElement)) {
    throw new Error(`Button not found: ${labelText}`);
  }
  return button;
};

const getMenuItemByText = (root: ParentNode, labelText: string) => {
  const item = Array.from(root.querySelectorAll('[role="menuitem"]')).find(
    (candidate) => (candidate.textContent ?? '').trim() === labelText,
  );
  if (!(item instanceof HTMLElement)) {
    throw new Error(`Menu item not found: ${labelText}`);
  }
  return item;
};

const clickElement = async (element: Element) => {
  await act(async () => {
    element.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
  });
};

describe('LabelAssetsPage', () => {
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
    listAssetsMock.mockReset();
    listRoomsMock.mockReset();
    listDropdownsMock.mockReset();
    historyMock.mockReset();
    listRoomsMock.mockResolvedValue([]);
    listDropdownsMock.mockResolvedValue([]);
    historyMock.mockResolvedValue([]);
  });

  it('replaces empty inventory filter chrome with one first-asset empty state', async () => {
    listAssetsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Todavía no hay assets. Agrégalos desde Agregar asset; el buscador y los filtros aparecerán cuando exista al menos uno.',
        );
        expect(countLabelsByText(container, 'Buscar assets')).toBe(0);
        expect(countLabelsByText(container, 'Categoría')).toBe(0);
        expect(queryButtonByText(container, 'Actualizar')).toBeNull();
        expect(queryButtonByText(container, 'Limpiar filtros')).toBeNull();
        expect(container.querySelector('table')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('replaces single-asset filter chrome with one first-asset helper line', async () => {
    listAssetsMock.mockResolvedValue([buildAsset()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain(
          'Solo hay un asset por ahora. Usa prestamo/devolucion o Acciones desde esta fila. Cuando el catalogo crezca, aqui apareceran buscador y filtros.',
        );
        expect(countLabelsByText(container, 'Buscar assets')).toBe(0);
        expect(countLabelsByText(container, 'Categoría')).toBe(0);
        expect(container.querySelectorAll('[aria-label^="Filtrar assets por estado "]')).toHaveLength(0);
        expect(container.textContent).not.toContain('Mostrando 1 de 1 assets');
        expect(container.textContent).not.toContain('Vista actual');
        expect(queryButtonByText(container, 'Limpiar filtros')).toBeNull();
        expect(container.querySelector('table')).not.toBeNull();
        expect(container.textContent).toContain('Sintetizador Uno');
      });
    } finally {
      await cleanup();
    }
  });

  it('renders one movement action per asset and picks the relevant action from status', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset(),
      buildAsset({
        assetId: 'asset-2',
        name: 'Bateria Roja',
        status: 'Booked',
      }),
      buildAsset({
        assetId: 'asset-3',
        name: 'Microfono Beta',
        status: 'OutForMaintenance',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const movementButtons = Array.from(container.querySelectorAll('button')).filter((button) => {
        const label = button.getAttribute('aria-label') ?? '';
        return label.startsWith('Abrir prestamo de ') || label.startsWith('Abrir devolucion de ');
      });

      expect(container.textContent).toContain('Sintetizador Uno');
      expect(container.textContent).toContain('Bateria Roja');
      expect(container.textContent).toContain('Microfono Beta');
      expect(movementButtons).toHaveLength(3);
      expect(container.querySelector('button[aria-label="Abrir prestamo de Sintetizador Uno"]')).not.toBeNull();
      expect(container.querySelector('button[aria-label="Abrir devolucion de Sintetizador Uno"]')).toBeNull();
      expect(container.querySelector('button[aria-label="Abrir devolucion de Bateria Roja"]')).not.toBeNull();
      expect(container.querySelector('button[aria-label="Abrir prestamo de Bateria Roja"]')).toBeNull();
      expect(container.querySelector('button[aria-label="Abrir prestamo de Microfono Beta"]')).not.toBeNull();
      expect(container.querySelector('button[aria-label="Abrir devolucion de Microfono Beta"]')).toBeNull();
      expect(container.querySelector('button[aria-label^="Abrir check-out de "]')).toBeNull();
      expect(container.querySelector('button[aria-label^="Abrir check-in de "]')).toBeNull();
    });

    await cleanup();
  });

  it('keeps one overflow actions entry point per asset row for secondary tasks', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset(),
      buildAsset({
        assetId: 'asset-2',
        name: 'Bateria Roja',
        status: 'Booked',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(
        'Usa el boton de prestamo o devolucion para registrar movimientos rapidos. Abre Acciones para editar, ver el QR, revisar el historial o eliminar el asset.',
      );
      expect(container.querySelectorAll('button[aria-label^="Abrir acciones para "]')).toHaveLength(2);
      expect(container.querySelector('button[aria-label="Editar activo Sintetizador Uno"]')).toBeNull();
      expect(container.querySelector('button[aria-label="Abrir QR de Sintetizador Uno"]')).toBeNull();
      expect(container.querySelector('button[aria-label="Eliminar activo Sintetizador Uno"]')).toBeNull();
      expect(queryButtonByText(container, 'Historial')).toBeNull();
    });

    await clickElement(getElementByAriaLabel(container, 'Abrir acciones para Sintetizador Uno'));

    await waitForExpectation(() => {
      expect(getMenuItemByText(document.body, 'Editar')).toBeTruthy();
      expect(getMenuItemByText(document.body, 'Ver QR')).toBeTruthy();
      expect(getMenuItemByText(document.body, 'Historial')).toBeTruthy();
      expect(getMenuItemByText(document.body, 'Eliminar')).toBeTruthy();
    });

    await cleanup();
  });

  it('shows one dismissible history panel with an empty-state message when an asset has no movements', async () => {
    listAssetsMock.mockResolvedValue([buildAsset()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Sintetizador Uno');
      expect(container.textContent).not.toContain('Historial · Sintetizador Uno');
    });

    await clickElement(getElementByAriaLabel(container, 'Abrir acciones para Sintetizador Uno'));

    await waitForExpectation(() => {
      expect(getMenuItemByText(document.body, 'Historial')).toBeTruthy();
    });

    await clickElement(getMenuItemByText(document.body, 'Historial'));

    await waitForExpectation(() => {
      expect(historyMock).toHaveBeenCalledWith('asset-1');
      expect(container.textContent).toContain('Historial · Sintetizador Uno');
      expect(container.textContent).toContain('Todavia no hay prestamos o devoluciones registrados para este asset.');
      expect(countButtonsByText(container, 'Ocultar historial')).toBe(1);
    });

    await clickElement(getButtonByText(container, 'Ocultar historial'));

    await waitForExpectation(() => {
      expect(container.textContent).not.toContain('Historial · Sintetizador Uno');
      expect(container.textContent).not.toContain('Todavia no hay prestamos o devoluciones registrados para este asset.');
      expect(countButtonsByText(container, 'Ocultar historial')).toBe(0);
    });

    await cleanup();
  });

  it('replaces a single category selector with context copy and restores it when multiple asset categories exist', async () => {
    listDropdownsMock.mockResolvedValue([
      {
        optionId: 'cat-cables',
        category: 'asset-category',
        value: 'Cables',
        label: 'Cables',
        active: true,
        sortOrder: 20,
      },
    ]);
    listAssetsMock.mockResolvedValue([
      buildAsset(),
      buildAsset({
        assetId: 'asset-2',
        name: 'Bateria Roja',
        status: 'Booked',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countLabelsByText(container, 'Categoría')).toBe(0);
      expect(container.textContent).toContain('Categoria disponible');
      expect(container.textContent).toContain('Synth');
      expect(container.textContent).toContain('No hace falta filtrarla: es la unica categoria cargada ahora mismo.');
      expect(hasTableHeader(container, 'Categoría')).toBe(false);
    });

    await cleanup();

    listDropdownsMock.mockResolvedValue([]);
    listAssetsMock.mockResolvedValue([
      buildAsset(),
      buildAsset({
        assetId: 'asset-2',
        name: 'Guitarra Clasica',
        category: 'Guitarras',
      }),
    ]);

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    await waitForExpectation(() => {
      expect(countLabelsByText(secondContainer, 'Categoría')).toBe(1);
      expect(secondContainer.textContent).not.toContain('No hace falta filtrarla: es la unica categoria cargada ahora mismo.');
      expect(hasTableHeader(secondContainer, 'Categoría')).toBe(true);
    });

    await secondRender.cleanup();
  });

  it('combines single category and single status context into one summary block', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset(),
      buildAsset({
        assetId: 'asset-2',
        name: 'Bateria Roja',
        brand: 'Roland',
        model: 'JD-Xi',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countLabelsByText(container, 'Categoría')).toBe(0);
      expect(container.querySelectorAll('[aria-label^="Filtrar assets por estado "]')).toHaveLength(0);
      expect(container.textContent).toContain('Vista actual');
      expect(container.textContent).toContain('Synth · Activos');
      expect(container.textContent).toContain(
        'No hace falta filtrar categoría ni estado: esta vista solo tiene una categoría y un estado por ahora.',
      );
      expect(container.textContent).not.toContain('Categoria disponible');
      expect(container.textContent).not.toContain('Estado disponible');
      expect(hasTableHeader(container, 'Categoría')).toBe(false);
      expect(hasTableHeader(container, 'Estado')).toBe(false);
    });

    await cleanup();
  });

  it('summarizes one shared location once and restores the location column when rows differ again', async () => {
    listRoomsMock.mockResolvedValue([buildRoom()]);
    listAssetsMock.mockResolvedValue([
      buildAsset({ location: 'room-a' }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Bateria Roja',
        location: 'room-a',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const text = container.textContent ?? '';
      expect(text).toContain('Mostrando una sola ubicación: Sala A.');
      expect(countOccurrences(text, 'Sala A')).toBe(1);
      expect(hasTableHeader(container, 'Ubicación')).toBe(false);
      expect(text).toContain('Sintetizador Uno');
      expect(text).toContain('Bateria Roja');
    });

    await cleanup();

    listRoomsMock.mockResolvedValue([
      buildRoom(),
      buildRoom({ roomId: 'room-b', rName: 'Sala B' }),
    ]);
    listAssetsMock.mockResolvedValue([
      buildAsset({ location: 'room-a' }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Bateria Roja',
        location: 'room-b',
      }),
    ]);

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    await waitForExpectation(() => {
      expect(secondContainer.textContent).not.toContain('Mostrando una sola ubicación:');
      expect(hasTableHeader(secondContainer, 'Ubicación')).toBe(true);
      expect(secondContainer.textContent).toContain('Sala A');
      expect(secondContainer.textContent).toContain('Sala B');
    });

    await secondRender.cleanup();
  });

  it('summarizes one visible category once after search narrows a mixed catalog and restores the category column when cleared', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset(),
      buildAsset({
        assetId: 'asset-2',
        name: 'Sintetizador Dos',
        brand: 'Roland',
        model: 'JD-Xi',
        status: 'Booked',
      }),
      buildAsset({
        assetId: 'asset-3',
        name: 'Guitarra Clasica',
        category: 'Guitarras',
        brand: 'Yamaha',
        model: 'C40',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasTableHeader(container, 'Categoría')).toBe(true);
      expect(container.textContent).not.toContain('Mostrando una sola categoría:');
      expect(container.textContent).toContain('Guitarra Clasica');
    });

    await setInputValue(getInputByLabel(container, 'Buscar assets'), 'Roland');

    await waitForExpectation(() => {
      const text = container.textContent ?? '';
      expect(text).toContain('Mostrando una sola categoría: Synth.');
      expect(countOccurrences(text, 'Mostrando una sola categoría: Synth.')).toBe(1);
      expect(hasTableHeader(container, 'Categoría')).toBe(false);
      expect(text).toContain('Sintetizador Uno');
      expect(text).toContain('Sintetizador Dos');
      expect(text).not.toContain('Guitarra Clasica');
    });

    await setInputValue(getInputByLabel(container, 'Buscar assets'), '');

    await waitForExpectation(() => {
      expect(container.textContent).not.toContain('Mostrando una sola categoría:');
      expect(hasTableHeader(container, 'Categoría')).toBe(true);
      expect(container.textContent).toContain('Guitarra Clasica');
    });

    await cleanup();
  });

  it('uses one status chip group to filter and reset the asset list', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset(),
      buildAsset({
        assetId: 'asset-2',
        name: 'Bateria Roja',
        status: 'Booked',
      }),
      buildAsset({
        assetId: 'asset-3',
        name: 'Microfono Beta',
        status: 'OutForMaintenance',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelectorAll('[aria-label^="Filtrar assets por estado "]')).toHaveLength(4);
      expect(countLabelsByText(container, 'Estado')).toBe(0);
      expect(hasTableHeader(container, 'Estado')).toBe(true);
      expect(container.textContent).toContain('Todos (3)');
      expect(container.textContent).not.toContain('Retirados');
      expect(container.textContent).toContain('Sintetizador Uno');
      expect(container.textContent).toContain('Bateria Roja');
      expect(container.textContent).toContain('Microfono Beta');
    });

    await clickElement(getElementByAriaLabel(container, 'Filtrar assets por estado Prestados'));

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Bateria Roja');
      expect(container.textContent).not.toContain('Sintetizador Uno');
      expect(container.textContent).not.toContain('Microfono Beta');
    });

    await clickElement(getElementByAriaLabel(container, 'Filtrar assets por estado Todos'));

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Sintetizador Uno');
      expect(container.textContent).toContain('Bateria Roja');
      expect(container.textContent).toContain('Microfono Beta');
    });

    await cleanup();
  });

  it('replaces a single real status filter with context copy when the current view does not need status filtering', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset(),
      buildAsset({
        assetId: 'asset-2',
        name: 'Bateria Roja',
        category: 'Guitarras',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelectorAll('[aria-label^="Filtrar assets por estado "]')).toHaveLength(0);
      expect(countLabelsByText(container, 'Categoría')).toBe(1);
      expect(container.textContent).toContain('Estado disponible');
      expect(container.textContent).toContain('Activos');
      expect(container.textContent).toContain('No hace falta filtrarlo: es el unico estado presente en esta vista.');
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Filtrar por estado');
      expect(hasTableHeader(container, 'Estado')).toBe(false);
    });

    await cleanup();
  });

  it('summarizes active filters and clears them in one action', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset(),
      buildAsset({
        assetId: 'asset-2',
        name: 'Bateria Roja',
        status: 'Booked',
      }),
      buildAsset({
        assetId: 'asset-3',
        name: 'Microfono Beta',
        status: 'OutForMaintenance',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Mostrando 3 de 3 assets');
      expect(queryButtonByText(container, 'Limpiar filtros')).toBeNull();
    });

    await clickElement(getElementByAriaLabel(container, 'Filtrar assets por estado Mantenimiento'));
    await setInputValue(getInputByLabel(container, 'Buscar assets'), 'Beta');

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Mostrando 1 de 3 assets');
      expect(container.textContent).toContain('Busca: Beta');
      expect(container.textContent).toContain('Estado: Mantenimiento');
      expect(countOccurrences(container.textContent ?? '', 'Busca: Beta')).toBe(1);
      expect(countOccurrences(container.textContent ?? '', 'Estado: Mantenimiento')).toBe(1);
      expect(container.textContent).toContain('Microfono Beta');
      expect(container.textContent).not.toContain('Sintetizador Uno');
      expect(container.textContent).not.toContain('Bateria Roja');
      expect(container.textContent).not.toContain('2 filtros activos');
      expect(queryButtonByText(container, 'Limpiar filtros')).not.toBeNull();
    });

    await clickElement(getButtonByText(container, 'Limpiar filtros'));

    await waitForExpectation(() => {
      expect(getInputByLabel(container, 'Buscar assets').value).toBe('');
      expect(container.textContent).toContain('Mostrando 3 de 3 assets');
      expect(container.textContent).toContain('Sintetizador Uno');
      expect(container.textContent).toContain('Bateria Roja');
      expect(container.textContent).toContain('Microfono Beta');
      expect(container.textContent).not.toContain('2 filtros activos');
      expect(container.textContent).not.toContain('Busca: Beta');
      expect(container.textContent).not.toContain('Estado: Mantenimiento');
      expect(queryButtonByText(container, 'Limpiar filtros')).toBeNull();
    });

    await cleanup();
  });

  it('replaces an empty filtered table with one contextual reset state', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset(),
      buildAsset({
        assetId: 'asset-2',
        name: 'Bateria Roja',
        status: 'Booked',
      }),
      buildAsset({
        assetId: 'asset-3',
        name: 'Microfono Beta',
        status: 'OutForMaintenance',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelector('table')).not.toBeNull();
      expect(container.textContent).toContain('Mostrando 3 de 3 assets');
    });

    await clickElement(getElementByAriaLabel(container, 'Filtrar assets por estado Mantenimiento'));
    await setInputValue(getInputByLabel(container, 'Buscar assets'), 'Nada');

    await waitForExpectation(() => {
      const text = container.textContent ?? '';
      expect(container.textContent).not.toContain('Mostrando 0 de 3 assets');
      expect(text).toContain(
        'No hay assets con los filtros actuales: Busca: Nada · Estado: Mantenimiento. Limpia filtros o ajusta la búsqueda si esperabas resultados.',
      );
      expect(countOccurrences(text, 'Busca: Nada')).toBe(1);
      expect(countOccurrences(text, 'Estado: Mantenimiento')).toBe(1);
      expect(text).not.toContain('2 filtros activos');
      expect(container.querySelector('table')).toBeNull();
      expect(countButtonsByText(container, 'Limpiar filtros')).toBe(1);
    });

    await clickElement(getButtonByText(container, 'Limpiar filtros'));

    await waitForExpectation(() => {
      expect(getInputByLabel(container, 'Buscar assets').value).toBe('');
      expect(container.textContent).toContain('Mostrando 3 de 3 assets');
      expect(container.textContent).toContain('Sintetizador Uno');
      expect(container.textContent).toContain('Bateria Roja');
      expect(container.textContent).toContain('Microfono Beta');
      expect(container.textContent).not.toContain('No hay assets con los filtros actuales:');
      expect(container.querySelector('table')).not.toBeNull();
      expect(countButtonsByText(container, 'Limpiar filtros')).toBe(0);
    });

    await cleanup();
  });
});
