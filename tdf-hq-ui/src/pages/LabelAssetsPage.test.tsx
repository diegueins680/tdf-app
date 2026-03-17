import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { AssetDTO, DropdownOptionDTO, RoomDTO } from '../api/types';

const listAssetsMock = jest.fn<() => Promise<AssetDTO[]>>();
const listRoomsMock = jest.fn<() => Promise<RoomDTO[]>>();
const listDropdownsMock = jest.fn<() => Promise<DropdownOptionDTO[]>>();

jest.unstable_mockModule('../api/inventory', () => ({
  Inventory: {
    list: () => listAssetsMock(),
    history: jest.fn(() => Promise.resolve([])),
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

const getElementByAriaLabel = (root: ParentNode, labelText: string) => {
  const element = root.querySelector(`[aria-label="${labelText}"]`);
  if (!(element instanceof HTMLElement)) {
    throw new Error(`Element not found: ${labelText}`);
  }
  return element;
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
    listRoomsMock.mockResolvedValue([]);
    listDropdownsMock.mockResolvedValue([]);
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
      expect(container.querySelectorAll('[aria-label^="Filtrar assets por estado "]')).toHaveLength(5);
      expect(countLabelsByText(container, 'Estado')).toBe(0);
      expect(container.textContent).toContain('Todos (3)');
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
});
