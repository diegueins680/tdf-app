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
  checkoutId: 1,
  assetId: 'asset-1',
  targetKind: 'party',
  targetPartyId: 9,
  targetRoomId: null,
  targetSessionId: null,
  dueAt: '2030-01-04T03:04:05.000Z',
  checkedOutAt: '2030-01-02T03:04:05.000Z',
  returnedAt: null,
  conditionOut: 'Excelente',
  conditionIn: null,
  notes: 'Uso en grabación.',
  partyName: 'Ada Lovelace',
  roomName: null,
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

  it('keeps the operations table once at least one asset is available', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Neumann U87');
        expect(container.textContent).not.toContain('Primeros pasos');
        expect(container.querySelector('table')).not.toBeNull();
        expect(hasTableHeader(container, 'Equipo')).toBe(true);
        expect(hasTableHeader(container, 'Acciones')).toBe(true);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps category and condition inside the equipment cell instead of restoring extra detail columns', async () => {
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
