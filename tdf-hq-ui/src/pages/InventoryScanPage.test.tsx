import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { AssetDTO } from '../api/types';

const byQrTokenMock = jest.fn<(token: string) => Promise<AssetDTO>>();
const checkoutMock = jest.fn<(assetId: string, payload: unknown) => Promise<unknown>>();
const checkinMock = jest.fn<(assetId: string, payload: unknown) => Promise<unknown>>();

jest.unstable_mockModule('../api/inventory', () => ({
  Inventory: {
    byQrToken: (token: string) => byQrTokenMock(token),
    checkout: (assetId: string, payload: unknown) => checkoutMock(assetId, payload),
    checkin: (assetId: string, payload: unknown) => checkinMock(assetId, payload),
  },
}));

const { default: InventoryScanPage } = await import('./InventoryScanPage');

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

const renderPage = async (container: HTMLElement, initialEntry = '/inventario/scan/token-demo') => {
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
          <Routes>
            <Route path="/inventario/scan/:token" element={<InventoryScanPage />} />
            <Route path="*" element={<InventoryScanPage />} />
          </Routes>
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

const buildAsset = (overrides: Partial<AssetDTO> = {}): AssetDTO => ({
  assetId: 'asset-1',
  name: 'Neumann U87',
  category: 'Micrófono',
  status: 'Active',
  condition: 'Lista',
  location: 'Sala A',
  qrToken: 'token-demo',
  ...overrides,
});

const countButtonsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('button')).filter((element) => (element.textContent ?? '').trim() === labelText).length;

const countOccurrences = (root: ParentNode, text: string) =>
  (root.textContent ?? '').split(text).length - 1;

describe('InventoryScanPage', () => {
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
    byQrTokenMock.mockReset();
    checkoutMock.mockReset();
    checkinMock.mockReset();
    checkoutMock.mockResolvedValue({});
    checkinMock.mockResolvedValue({});
  });

  it.each([
    {
      label: 'available',
      asset: buildAsset({ status: 'Active' }),
      expectedText: 'Registrar check-out',
    },
    {
      label: 'checked-out',
      asset: buildAsset({ status: 'Booked' }),
      expectedText: 'Registrar check-in',
    },
    {
      label: 'unavailable',
      asset: buildAsset({ status: 'OutForMaintenance' }),
      expectedText: 'No se permite check-in/out mientras esté fuera de servicio.',
    },
  ])('keeps one shared refresh action for the $label scan state', async ({ asset, expectedText }) => {
    byQrTokenMock.mockResolvedValue(asset);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(byQrTokenMock).toHaveBeenCalledWith('token-demo');
        expect(container.textContent).toContain(expectedText);
        expect(countButtonsByText(container, 'Refrescar estado')).toBe(1);
        expect(
          countOccurrences(
            container,
            'Si otro operador ya movio este equipo desde otra pantalla, usa Refrescar estado para confirmar que accion sigue aqui.',
          ),
        ).toBe(1);
      });
    } finally {
      await cleanup();
    }
  });
});
