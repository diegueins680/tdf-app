import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import type { AssetCheckoutDTO, AssetDTO, PartyDTO, RoomDTO } from '../api/types';

const listAssetsMock = jest.fn<() => Promise<AssetDTO[] | { items: AssetDTO[] }>>();
const listRoomsMock = jest.fn<() => Promise<RoomDTO[]>>();
const listPartiesMock = jest.fn<() => Promise<PartyDTO[]>>();
const historyMock = jest.fn<(assetId: string) => Promise<AssetCheckoutDTO[]>>();

jest.unstable_mockModule('../api/inventory', () => ({
  Inventory: {
    list: () => listAssetsMock(),
    history: (assetId: string) => historyMock(assetId),
    generateQr: jest.fn(() => Promise.resolve({ qrUrl: 'https://example.com/qr' })),
    checkout: jest.fn(() => Promise.resolve(null)),
    checkin: jest.fn(() => Promise.resolve(null)),
  },
}));

jest.unstable_mockModule('../api/rooms', () => ({
  Rooms: {
    list: () => listRoomsMock(),
  },
}));

jest.unstable_mockModule('../api/parties', () => ({
  Parties: {
    list: () => listPartiesMock(),
  },
}));

jest.unstable_mockModule('../components/AssetDialogs', () => ({
  CheckoutDialog: () => null,
  CheckinDialog: () => null,
}));

const { default: InventoryPage } = await import('./InventoryPage');

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
        <InventoryPage />
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

const buildAsset = (overrides: Partial<AssetDTO> = {}): AssetDTO => ({
  assetId: 'asset-1',
  name: 'Neumann U87',
  category: 'Micrófono',
  status: 'Active',
  condition: 'Excelente',
  location: 'Sala A',
  qrToken: 'qr-1',
  ...overrides,
});

const buildCheckoutHistoryEntry = (overrides: Partial<AssetCheckoutDTO> = {}): AssetCheckoutDTO => ({
  checkoutId: 'checkout-1',
  assetId: 'asset-1',
  targetKind: 'party',
  targetPartyRef: 'Ada Lovelace',
  targetRoomId: null,
  targetSessionId: null,
  checkedOutBy: 'admin',
  dueAt: '2030-01-04T03:04:05.000Z',
  checkedOutAt: '2030-01-02T03:04:05.000Z',
  returnedAt: null,
  conditionOut: 'Excelente',
  conditionIn: null,
  notes: 'Uso en grabación.',
  ...overrides,
});

const hasTableHeader = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('th')).some((cell) => (cell.textContent ?? '').trim() === labelText);

describe('InventoryPage', () => {
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
    listPartiesMock.mockReset();
    historyMock.mockReset();

    listAssetsMock.mockResolvedValue([buildAsset()]);
    listRoomsMock.mockResolvedValue([]);
    listPartiesMock.mockResolvedValue([]);
    historyMock.mockResolvedValue([]);
  });

  it('replaces the blank inventory table with first-run guidance when there are no assets', async () => {
    listAssetsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Primeros pasos');
        expect(container.textContent).toContain(
          'Todavía no hay equipos registrados. Cuando exista el primero, aquí verás estado, ubicación, QR e historial para operar check-out y check-in desde una sola fila.',
        );
        expect(container.textContent).toContain(
          'Si estás esperando la carga inicial del inventario, usa Actualizar para volver a consultar sin revisar una tabla vacía.',
        );
        expect(container.querySelector('table')).toBeNull();
        expect(hasTableHeader(container, 'Equipo')).toBe(false);
        expect(hasTableHeader(container, 'Acciones')).toBe(false);
      });
    } finally {
      await cleanup();
    }
  });

  it('replaces the one-row inventory table with a first-asset summary card and plain-language actions', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Primer equipo registrado');
        expect(container.textContent).toContain(
          'Revisa estado, ubicación y el siguiente movimiento desde este resumen. Cuando exista el segundo equipo, volverá la tabla operativa.',
        );
        expect(container.textContent).toContain('Neumann U87');
        expect(container.textContent).toContain('Categoría: Micrófono');
        expect(container.textContent).toContain('Estado: Disponible');
        expect(container.textContent).toContain('Ubicación: Sala A');
        expect(container.textContent).toContain('Condición: Excelente');
        expect(container.querySelector('table')).toBeNull();
        expect(hasTableHeader(container, 'Equipo')).toBe(false);
        expect(hasTableHeader(container, 'Acciones')).toBe(false);
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'Ver QR',
          ),
        ).toBe(true);
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'Registrar check-out',
          ),
        ).toBe(true);
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'Historial',
          ),
        ).toBe(true);
      });
    } finally {
      await cleanup();
    }
  });

  it('hides empty optional metadata in the single-asset summary so first-run stays focused on real context', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        location: '   ',
        condition: '',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Primer equipo registrado');
        expect(container.textContent).toContain('Categoría: Micrófono');
        expect(container.textContent).toContain('Estado: Disponible');
        expect(container.textContent).not.toContain('Ubicación:');
        expect(container.textContent).not.toContain('Condición:');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps repeated table actions compact by moving QR and history into one row actions menu', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset(),
      buildAsset({
        assetId: 'asset-2',
        name: 'Apollo Twin',
        status: 'Booked',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('table')).not.toBeNull();
        expect(container.textContent).toContain('Disponible');
        expect(container.textContent).toContain('Prestado');
        expect(container.textContent).not.toContain('Active');
        expect(container.textContent).not.toContain('Booked');
        expect(container.textContent).toContain(
          'Usa el botón de check-out o check-in cuando esté disponible para registrar el siguiente movimiento. Abre Acciones para ver QR o historial.',
        );
        expect(container.querySelectorAll('button[aria-label^="Abrir acciones para "]')).toHaveLength(2);
        expect(container.querySelector('button[aria-label="Abrir QR de Neumann U87"]')).toBeNull();
        expect(container.querySelector('button[aria-label="Abrir historial de Neumann U87"]')).toBeNull();
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'Historial',
          ),
        ).toBe(false);
      });

      await act(async () => {
        const actionsButton = container.querySelector<HTMLButtonElement>('[aria-label="Abrir acciones para Neumann U87"]');
        actionsButton?.click();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Ver QR');
        expect(document.body.textContent).toContain('Historial');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the operations table once there is more than one asset to compare', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset(),
      buildAsset({
        assetId: 'asset-2',
        name: 'Apollo Twin',
        status: 'Booked',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).not.toContain('Primer equipo registrado');
        expect(container.textContent).toContain('Neumann U87');
        expect(container.textContent).toContain('Apollo Twin');
        expect(container.querySelector('table')).not.toBeNull();
        expect(hasTableHeader(container, 'Equipo')).toBe(true);
        expect(hasTableHeader(container, 'Acciones')).toBe(true);
      });
    } finally {
      await cleanup();
    }
  });

  it('hides the empty location column until at least one asset has a registered location', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Activo Uno',
        location: '   ',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Prestado Uno',
        status: 'Booked',
        location: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('table')).not.toBeNull();
        expect(hasTableHeader(container, 'Equipo')).toBe(true);
        expect(hasTableHeader(container, 'Estado')).toBe(true);
        expect(hasTableHeader(container, 'Ubicación')).toBe(false);
        expect(hasTableHeader(container, 'Acciones')).toBe(true);
        expect(container.textContent).toContain(
          'La ubicación aparecerá en la tabla cuando al menos un equipo tenga una ubicación registrada.',
        );

        const rows = Array.from(container.querySelectorAll('tbody tr'));
        expect(rows).toHaveLength(2);
        expect(rows[0]?.querySelectorAll('td')).toHaveLength(3);
        expect(rows[1]?.querySelectorAll('td')).toHaveLength(3);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps category and condition inside the equipment cell instead of restoring extra detail columns', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset(),
      buildAsset({
        assetId: 'asset-2',
        name: 'Apollo Twin',
        status: 'Booked',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(hasTableHeader(container, 'Equipo')).toBe(true);
        expect(hasTableHeader(container, 'Categoría')).toBe(false);
        expect(hasTableHeader(container, 'Condición')).toBe(false);
        expect(container.textContent).toContain('Neumann U87');
        expect(container.textContent).toContain('Micrófono');
        expect(container.textContent).toContain('Condición: Excelente');
      });
    } finally {
      await cleanup();
    }
  });

  it('shows only the movement action that matches each asset status instead of keeping both check-in and check-out controls per row', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({ assetId: 'asset-1', name: 'Activo Uno', status: 'Active' }),
      buildAsset({ assetId: 'asset-2', name: 'Prestado Uno', status: 'Booked' }),
      buildAsset({ assetId: 'asset-3', name: 'Retirado Uno', status: 'Retired' }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('[aria-label="Abrir check-out de Activo Uno"]')).not.toBeNull();
        expect(container.querySelector('[aria-label="Abrir check-in de Activo Uno"]')).toBeNull();
        expect(container.querySelector('[aria-label="Abrir check-in de Prestado Uno"]')).not.toBeNull();
        expect(container.querySelector('[aria-label="Abrir check-out de Prestado Uno"]')).toBeNull();
        expect(container.querySelector('[aria-label="Abrir check-in de Retirado Uno"]')).toBeNull();
        expect(container.querySelector('[aria-label="Abrir check-out de Retirado Uno"]')).toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('turns an empty history request into one dismissible panel instead of a silent no-op', async () => {
    historyMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('button[aria-label="Abrir check-out de Neumann U87"]')).not.toBeNull();
      });

      await act(async () => {
        const historyButton = Array.from(container.querySelectorAll('button')).find(
          (button) => (button.textContent ?? '').trim() === 'Historial',
        );
        historyButton?.click();
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(historyMock).toHaveBeenCalledWith('asset-1');
        expect(container.textContent).toContain('Historial · Neumann U87');
        expect(container.textContent).toContain(
          'Todavía no hay movimientos registrados para este equipo. Cuando ocurra el primero, aquí verás salida, devolución, destino y notas.',
        );
        expect(hasTableHeader(container, 'Salida')).toBe(false);
      });

      await act(async () => {
        const hideButton = Array.from(container.querySelectorAll('button')).find(
          (button) => (button.textContent ?? '').trim() === 'Ocultar historial',
        );
        hideButton?.click();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(container.textContent).not.toContain('Historial · Neumann U87');
        expect(container.textContent).not.toContain(
          'Todavía no hay movimientos registrados para este equipo. Cuando ocurra el primero, aquí verás salida, devolución, destino y notas.',
        );
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps checkout history inside the checkout flow until the admin explicitly opens the standalone history panel', async () => {
    historyMock.mockResolvedValue([
      buildCheckoutHistoryEntry(),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('[aria-label="Abrir check-out de Neumann U87"]')).not.toBeNull();
      });

      await act(async () => {
        const checkoutButton = container.querySelector<HTMLButtonElement>('[aria-label="Abrir check-out de Neumann U87"]');
        checkoutButton?.click();
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(historyMock).toHaveBeenCalledWith('asset-1');
        expect(container.textContent).not.toContain('Historial · Neumann U87');
        expect(container.textContent).not.toContain('Uso en grabación.');
      });

      await act(async () => {
        const historyButton = Array.from(container.querySelectorAll('button')).find(
          (button) => (button.textContent ?? '').trim() === 'Historial',
        );
        historyButton?.click();
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(historyMock).toHaveBeenCalledTimes(2);
        expect(container.textContent).toContain('Historial · Neumann U87');
        expect(container.textContent).toContain('Uso en grabación.');
      });
    } finally {
      await cleanup();
    }
  });
});
