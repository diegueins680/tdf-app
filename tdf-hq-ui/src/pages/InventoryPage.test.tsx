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
  disposition: 'loan',
  holderEmail: 'ada@example.com',
  holderPhone: '0999999999',
  checkedOutBy: 'admin',
  dueAt: '2030-01-04T03:04:05.000Z',
  checkedOutAt: '2030-01-02T03:04:05.000Z',
  returnedAt: null,
  conditionOut: 'Excelente',
  photoOutUrl: null,
  conditionIn: null,
  photoInUrl: null,
  notes: 'Uso en grabación.',
  ...overrides,
});

const hasTableHeader = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('th')).some((cell) => (cell.textContent ?? '').trim() === labelText);

const countOccurrencesIgnoringCase = (value: string, fragment: string) =>
  value.toLocaleLowerCase().split(fragment.toLocaleLowerCase()).length - 1;

const getRowTextByAssetName = (root: ParentNode, assetName: string) => {
  const row = Array.from(root.querySelectorAll<HTMLTableRowElement>('tbody tr')).find(
    (tableRow) => (tableRow.textContent ?? '').includes(assetName),
  );

  if (!row) throw new Error(`Row not found for asset ${assetName}`);

  return row.textContent ?? '';
};

const openSingleAssetSecondaryAction = async (container: HTMLElement, actionLabel: string) => {
  await act(async () => {
    const actionsButton = container.querySelector<HTMLButtonElement>(
      'button[aria-label="Abrir QR, enlace e historial de Neumann U87"]',
    );
    actionsButton?.click();
    await flushPromises();
  });

  await waitForExpectation(() => {
    expect(document.body.textContent).toContain(actionLabel);
  });

  await act(async () => {
    const menuItem = Array.from(document.body.querySelectorAll<HTMLElement>('[role="menuitem"]')).find(
      (item) => (item.textContent ?? '').trim() === actionLabel,
    );
    menuItem?.click();
    await flushPromises();
    await flushPromises();
  });
};

const setInputValue = async (input: HTMLInputElement, value: string) => {
  await act(async () => {
    const nativeValueSetter = Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype, 'value')?.set;
    nativeValueSetter?.call(input, value);
    input.dispatchEvent(new Event('input', { bubbles: true }));
    input.dispatchEvent(new Event('change', { bubbles: true }));
    await flushPromises();
    await flushPromises();
  });
};

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

  it('replaces the blank inventory table and detached refresh chrome with one first-run reload path when there are no assets', async () => {
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
          'Si estás esperando la carga inicial del inventario, vuelve a consultar desde aquí sin revisar una tabla vacía.',
        );
        expect(container.querySelector('table')).toBeNull();
        expect(hasTableHeader(container, 'Equipo')).toBe(false);
        expect(hasTableHeader(container, 'Acciones')).toBe(false);
        expect(
          Array.from(container.querySelectorAll('button')).filter(
            (button) => (button.textContent ?? '').trim() === 'Volver a consultar inventario',
          ),
        ).toHaveLength(1);
        expect(
          Array.from(container.querySelectorAll('button')).filter(
            (button) => (button.textContent ?? '').trim() === 'Actualizar',
          ),
        ).toHaveLength(0);
      });

      const initialCallCount = listAssetsMock.mock.calls.length;
      expect(initialCallCount).toBeGreaterThan(0);

      await act(async () => {
        const reloadButton = Array.from(container.querySelectorAll<HTMLButtonElement>('button')).find(
          (button) => (button.textContent ?? '').trim() === 'Volver a consultar inventario',
        );
        reloadButton?.click();
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(listAssetsMock.mock.calls.length).toBeGreaterThan(initialCallCount);
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
            (button) => (button.textContent ?? '').trim() === 'QR e historial',
          ),
        ).toBe(true);
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'Registrar check-out',
          ),
        ).toBe(true);
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'QR y enlace público',
          ),
        ).toBe(false);
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'Copiar enlace',
          ),
        ).toBe(false);
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'Historial',
          ),
        ).toBe(false);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps QR sharing behind one clearer entry point so the secondary menu does not duplicate copy-link actions', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('button[aria-label="Abrir check-out de Neumann U87"]')).not.toBeNull();
        expect(container.querySelector('button[aria-label="Abrir QR, enlace e historial de Neumann U87"]')).not.toBeNull();
        expect(container.querySelector('button[aria-label="Abrir QR de Neumann U87"]')).toBeNull();
        expect(container.textContent).not.toContain('Copiar enlace');
        expect(container.textContent).not.toContain('Historial');
      });

      await act(async () => {
        const actionsButton = container.querySelector<HTMLButtonElement>(
          'button[aria-label="Abrir QR, enlace e historial de Neumann U87"]',
        );
        actionsButton?.click();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('QR y enlace público');
        expect(document.body.textContent).toContain('Historial');
        expect(
          Array.from(document.body.querySelectorAll<HTMLElement>('[role="menuitem"]')).some(
            (item) => (item.textContent ?? '').trim() === 'Copiar enlace',
          ),
        ).toBe(false);
      });

      await act(async () => {
        const qrMenuItem = Array.from(document.body.querySelectorAll<HTMLElement>('[role="menuitem"]')).find(
          (item) => (item.textContent ?? '').trim() === 'QR y enlace público',
        );
        qrMenuItem?.click();
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('QR y enlace público');
        expect(document.body.textContent).toContain('Enlace público');
        expect(document.body.textContent).toContain('Copiar enlace');
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

  it('collapses single-asset checkout detail into one custody summary plus contact', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        status: 'Booked',
        currentCheckoutTarget: 'Grace Hopper',
        currentCheckoutDisposition: 'rental',
        currentCheckoutHolderEmail: ' grace@example.com ',
        currentCheckoutHolderPhone: ' 0999999999 ',
        currentCheckoutAt: '2030-01-03T03:04:05.000Z',
        currentCheckoutDueAt: '2030-01-05T03:04:05.000Z',
        currentCheckoutPaymentType: 'card',
        currentCheckoutPaymentInstallments: 2,
        currentCheckoutPaymentAmountCents: 250000,
        currentCheckoutPaymentCurrency: 'USD',
        currentCheckoutPaymentOutstandingCents: 50000,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const text = container.textContent ?? '';
        expect(text).toContain('Primer equipo registrado');
        expect(text).toContain('Tenencia actual: Grace Hopper');
        expect(text).toContain('Contexto: Alquiler');
        expect(text).toContain('Salida:');
        expect(text).toContain('Retorno pactado:');
        expect(text).toContain('Pago: Tarjeta');
        expect(text).toContain('Contacto: grace@example.com · 0999999999');
        expect(text).not.toContain('Tiene:');
        expect(text).not.toContain('Movimiento:');
      });
    } finally {
      await cleanup();
    }
  });

  it('explains the single-asset no-movement state instead of leaving the lone secondary menu unexplained', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        status: 'Retired',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Primer equipo registrado');
        expect(container.textContent).toContain('Estado: Retirado');
        expect(container.textContent).toContain(
          'En este estado no hay check-out ni check-in disponibles. Usa QR e historial si necesitas revisar el registro.',
        );
        expect(container.querySelector('button[aria-label="Abrir check-out de Neumann U87"]')).toBeNull();
        expect(container.querySelector('button[aria-label="Abrir check-in de Neumann U87"]')).toBeNull();
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'QR e historial',
          ),
        ).toBe(true);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps repeated table actions compact by naming the secondary row menu after QR and history', async () => {
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
          'Usa check-out o check-in cuando esté disponible para registrar el siguiente movimiento.',
        );
        expect(container.textContent).not.toContain('Abre Acciones para ver QR o historial.');
        expect(container.querySelectorAll('button[aria-label^="Abrir QR, enlace e historial de "]')).toHaveLength(2);
        expect(container.querySelector('button[aria-label="Abrir QR de Neumann U87"]')).toBeNull();
        expect(container.querySelector('button[aria-label="Abrir historial de Neumann U87"]')).toBeNull();
        expect(
          Array.from(container.querySelectorAll('button')).filter(
            (button) => (button.textContent ?? '').trim() === 'QR e historial',
          ),
        ).toHaveLength(2);
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'Acciones',
          ),
        ).toBe(false);
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'Historial',
          ),
        ).toBe(false);
      });

      await act(async () => {
        const actionsButton = container.querySelector<HTMLButtonElement>(
          '[aria-label="Abrir QR, enlace e historial de Neumann U87"]',
        );
        actionsButton?.click();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('QR y enlace público');
        expect(document.body.textContent).toContain('Historial');
        expect(document.body.textContent).not.toContain('Copiar enlace');
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

  it('collapses a single filtered inventory match into the focused summary card before falling back to the empty-search reset path', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Neumann U87',
        category: 'Micrófono',
        location: 'Sala A',
        status: 'Active',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Apollo Twin',
        category: 'Interfaz',
        location: 'Sala B',
        status: 'Booked',
        currentCheckoutTarget: 'Grace Hopper',
        currentCheckoutAt: '2030-01-03T03:04:05.000Z',
      }),
      buildAsset({
        assetId: 'asset-3',
        name: 'Genelec 8040',
        category: 'Monitor',
        location: 'Bodega',
        status: 'Retired',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('table')).not.toBeNull();
        expect(container.querySelector('input[aria-label="Buscar en inventario"]')).not.toBeNull();
        expect(container.textContent).toContain('Neumann U87');
        expect(container.textContent).toContain('Apollo Twin');
        expect(container.textContent).toContain('Genelec 8040');
      });

      const searchInput = container.querySelector<HTMLInputElement>('input[aria-label="Buscar en inventario"]');
      expect(searchInput).not.toBeNull();

      await setInputValue(searchInput!, 'grace');

      await waitForExpectation(() => {
        expect(container.querySelector('table')).toBeNull();
        expect(container.textContent).not.toContain('Mostrando 1 de 3 equipos.');
        expect(container.textContent).toContain('Resultado único');
        expect(container.textContent).toContain(
          'Tu búsqueda ya dejó un solo equipo visible. Revisa estado, ubicación y el siguiente movimiento desde este resumen.',
        );
        expect(container.textContent).toContain('Apollo Twin');
        expect(container.textContent).not.toContain('Neumann U87');
        expect(container.textContent).not.toContain('Genelec 8040');
        expect(container.querySelector('[aria-label="Abrir check-in de Apollo Twin"]')).not.toBeNull();
        expect(container.querySelector('[aria-label="Abrir QR, enlace e historial de Apollo Twin"]')).not.toBeNull();
        expect(
          Array.from(container.querySelectorAll('button')).filter(
            (button) => (button.textContent ?? '').trim() === 'Volver a la tabla completa',
          ),
        ).toHaveLength(1);
      });

      await setInputValue(searchInput!, 'zzzz');

      await waitForExpectation(() => {
        expect(container.querySelector('table')).toBeNull();
        expect(container.textContent).not.toContain('Mostrando 0 de 3 equipos.');
        expect(container.textContent).toContain('Sin coincidencias');
        expect(container.textContent).toContain(
          'No encontramos equipos que coincidan con tu búsqueda. Ajusta o limpia el término desde el campo de arriba para volver a la vista completa.',
        );
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'Volver a la tabla completa',
          ),
        ).toBe(true);
      });

      await act(async () => {
        const clearButton = Array.from(container.querySelectorAll<HTMLButtonElement>('button')).find(
          (button) => (button.textContent ?? '').trim() === 'Volver a la tabla completa',
        );
        clearButton?.click();
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(container.querySelector('table')).not.toBeNull();
        expect(container.textContent).not.toContain('Sin coincidencias');
        expect(container.textContent).not.toContain('Mostrando 0 de 3 equipos.');
        expect(container.textContent).toContain('Neumann U87');
        expect(container.textContent).toContain('Apollo Twin');
        expect(container.textContent).toContain('Genelec 8040');
        expect(container.querySelectorAll('tbody tr')).toHaveLength(3);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the lighter clear-search chrome only while a filtered inventory table still has multiple matches', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Neumann U87',
        category: 'Micrófono',
        location: 'Sala A',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Apollo Twin',
        category: 'Interfaz',
        location: 'Sala B',
        status: 'Booked',
      }),
      buildAsset({
        assetId: 'asset-3',
        name: 'Genelec 8040',
        category: 'Monitor',
        location: 'Bodega',
        status: 'Retired',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('input[aria-label="Buscar en inventario"]')).not.toBeNull();
      });

      const searchInput = container.querySelector<HTMLInputElement>('input[aria-label="Buscar en inventario"]');
      expect(searchInput).not.toBeNull();

      await setInputValue(searchInput!, 'sala');

      await waitForExpectation(() => {
        expect(container.querySelector('table')).not.toBeNull();
        expect(container.textContent).toContain('Mostrando 2 de 3 equipos.');
        expect(container.textContent).not.toContain('Resultado único');
        expect(container.textContent).not.toContain('Sin coincidencias');
        expect(
          Array.from(container.querySelectorAll('button')).filter(
            (button) => (button.textContent ?? '').trim() === 'Limpiar búsqueda',
          ),
        ).toHaveLength(1);
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'Volver a la tabla completa',
          ),
        ).toBe(false);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the empty-search recovery attached to the field instead of duplicating clear or refresh actions', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Neumann U87',
        category: 'Micrófono',
        location: 'Sala A',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Apollo Twin',
        category: 'Interfaz',
        location: 'Sala B',
        status: 'Booked',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('input[aria-label="Buscar en inventario"]')).not.toBeNull();
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'Actualizar',
          ),
        ).toBe(true);
      });

      const searchInput = container.querySelector<HTMLInputElement>('input[aria-label="Buscar en inventario"]');
      expect(searchInput).not.toBeNull();

      await setInputValue(searchInput!, 'sin-match');

      await waitForExpectation(() => {
        expect(container.textContent).toContain('Sin coincidencias');
        expect(
          Array.from(container.querySelectorAll('button')).filter(
            (button) => (button.textContent ?? '').trim() === 'Volver a la tabla completa',
          ),
        ).toHaveLength(1);
        expect(
          Array.from(container.querySelectorAll('button')).some(
            (button) => (button.textContent ?? '').trim() === 'Actualizar',
          ),
        ).toBe(false);
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
        expect(hasTableHeader(container, 'Tenencia actual')).toBe(true);
        expect(hasTableHeader(container, 'Salida')).toBe(false);
        expect(hasTableHeader(container, 'Ubicación')).toBe(false);
        expect(hasTableHeader(container, 'Acciones')).toBe(true);
        expect(container.textContent).toContain(
          'La ubicación aparecerá en la tabla cuando al menos un equipo tenga una ubicación registrada. Usa check-out o check-in cuando esté disponible para registrar el siguiente movimiento.',
        );

        const rows = Array.from(container.querySelectorAll('tbody tr'));
        expect(rows).toHaveLength(2);
        expect(rows[0]?.querySelectorAll('td')).toHaveLength(4);
        expect(rows[1]?.querySelectorAll('td')).toHaveLength(4);
      });
    } finally {
      await cleanup();
    }
  });

  it('combines repeated shared-column guidance into one summary when the visible inventory already matches on several fields', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Activo Uno',
        category: 'Micrófono',
        location: 'Sala A',
        condition: 'Excelente',
        status: 'Active',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Activo Dos',
        category: ' micrófono ',
        location: 'sala a',
        condition: ' excelente ',
        status: ' active ',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const text = container.textContent ?? '';
        expect(container.querySelector('[data-testid="inventory-shared-columns-summary"]')?.textContent?.trim()).toBe(
          'Se ocultaron columnas porque toda esta vista coincide en estado Disponible, categoría Micrófono, ubicación Sala A y condición Excelente. Volverán cuando esta vista mezcle valores distintos.',
        );
        expect(text).not.toContain('Mostrando un solo estado:');
        expect(text).not.toContain('Mostrando una sola categoría:');
        expect(text).not.toContain('Mostrando una sola ubicación:');
        expect(text).not.toContain('Mostrando una sola condición:');
        expect(countOccurrencesIgnoringCase(text, 'Se ocultaron columnas porque toda esta vista coincide en')).toBe(1);
        expect(countOccurrencesIgnoringCase(text, 'Disponible')).toBe(1);
        expect(countOccurrencesIgnoringCase(text, 'Micrófono')).toBe(1);
        expect(countOccurrencesIgnoringCase(text, 'Sala A')).toBe(1);
        expect(countOccurrencesIgnoringCase(text, 'Excelente')).toBe(1);
        expect(hasTableHeader(container, 'Estado')).toBe(false);
        expect(hasTableHeader(container, 'Ubicación')).toBe(false);

        const rows = Array.from(container.querySelectorAll('tbody tr'));
        expect(rows).toHaveLength(2);
        expect(rows[0]?.querySelectorAll('td')).toHaveLength(2);
        expect(rows[1]?.querySelectorAll('td')).toHaveLength(2);
      });
    } finally {
      await cleanup();
    }
  });

  it('summarizes one shared location once and restores the location column when assets diverge again', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Activo Uno',
        location: 'Sala A',
        condition: 'Excelente',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Retirado Uno',
        category: 'Interfaz',
        location: 'sala a',
        condition: 'Bueno',
        status: 'Retired',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const text = container.textContent ?? '';
        expect(text).toContain(
          'Mostrando una sola ubicación: Sala A. La columna volverá cuando esta vista mezcle ubicaciones distintas.',
        );
        expect(countOccurrencesIgnoringCase(text, 'Mostrando una sola ubicación: Sala A.')).toBe(1);
        expect(countOccurrencesIgnoringCase(text, 'Sala A')).toBe(1);
        expect(text).not.toContain('Se ocultaron columnas porque toda esta vista coincide en');
        expect(hasTableHeader(container, 'Estado')).toBe(true);
        expect(hasTableHeader(container, 'Ubicación')).toBe(false);

        const rows = Array.from(container.querySelectorAll('tbody tr'));
        expect(rows).toHaveLength(2);
        expect(rows[0]?.querySelectorAll('td')).toHaveLength(3);
        expect(rows[1]?.querySelectorAll('td')).toHaveLength(3);
      });
    } finally {
      await cleanup();
    }

    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Activo Uno',
        location: 'Sala A',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Retirado Uno',
        category: 'Interfaz',
        location: 'Sala B',
        status: 'Retired',
      }),
    ]);

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    try {
      await waitForExpectation(() => {
        const text = secondContainer.textContent ?? '';
        expect(text).not.toContain('Mostrando una sola ubicación:');
        expect(text).not.toContain('Se ocultaron columnas porque toda esta vista coincide en');
        expect(hasTableHeader(secondContainer, 'Estado')).toBe(true);
        expect(hasTableHeader(secondContainer, 'Ubicación')).toBe(true);
        expect(text).toContain('Sala A');
        expect(text).toContain('Sala B');
      });
    } finally {
      await secondRender.cleanup();
    }
  });

  it('summarizes one shared category once and restores row categories when the visible inventory mix changes', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Activo Uno',
        category: 'Micrófono',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Retirado Uno',
        category: ' micrófono ',
        location: 'Sala B',
        status: 'Retired',
        condition: 'Bueno',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const text = container.textContent ?? '';
        expect(text).toContain(
          'Mostrando una sola categoría: Micrófono. La categoría volverá cuando esta vista mezcle categorías distintas.',
        );
        expect(countOccurrencesIgnoringCase(text, 'Mostrando una sola categoría: Micrófono.')).toBe(1);
        expect(countOccurrencesIgnoringCase(text, 'Micrófono')).toBe(1);
        expect(text).not.toContain('Se ocultaron columnas porque toda esta vista coincide en');
        expect(hasTableHeader(container, 'Estado')).toBe(true);
        expect(hasTableHeader(container, 'Ubicación')).toBe(true);

        const rows = Array.from(container.querySelectorAll('tbody tr'));
        expect(rows).toHaveLength(2);
        expect(rows[0]?.textContent).not.toContain('Micrófono');
        expect(rows[1]?.textContent).not.toContain('Micrófono');
        expect(rows[0]?.textContent).toContain('Condición: Excelente');
        expect(rows[1]?.textContent).toContain('Condición: Bueno');
      });
    } finally {
      await cleanup();
    }

    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Activo Uno',
        category: 'Micrófono',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Retirado Uno',
        category: 'Interfaz',
        location: 'Sala B',
        status: 'Retired',
      }),
    ]);

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    try {
      await waitForExpectation(() => {
        const text = secondContainer.textContent ?? '';
        expect(text).not.toContain('Mostrando una sola categoría:');
        expect(text).not.toContain('Se ocultaron columnas porque toda esta vista coincide en');
        expect(text).toContain('Micrófono');
        expect(text).toContain('Interfaz');
      });
    } finally {
      await secondRender.cleanup();
    }
  });

  it('summarizes one shared status once and restores the status column when the visible inventory mixes states again', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Activo Uno',
        category: 'Micrófono',
        location: 'Sala A',
        condition: 'Excelente',
        status: 'Active',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Activo Dos',
        category: 'Interfaz',
        location: 'Sala B',
        condition: 'Bueno',
        status: ' active ',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const text = container.textContent ?? '';
        expect(text).toContain(
          'Mostrando un solo estado: Disponible. La columna volverá cuando esta vista mezcle estados distintos.',
        );
        expect(countOccurrencesIgnoringCase(text, 'Mostrando un solo estado: Disponible.')).toBe(1);
        expect(hasTableHeader(container, 'Estado')).toBe(false);
        expect(hasTableHeader(container, 'Ubicación')).toBe(true);

        const rows = Array.from(container.querySelectorAll('tbody tr'));
        expect(rows).toHaveLength(2);
        expect(rows[0]?.querySelectorAll('td')).toHaveLength(3);
        expect(rows[1]?.querySelectorAll('td')).toHaveLength(3);
      });
    } finally {
      await cleanup();
    }

    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Activo Uno',
        category: 'Micrófono',
        location: 'Sala A',
        status: 'Active',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Prestado Uno',
        category: 'Interfaz',
        location: 'Sala B',
        status: 'Booked',
        currentCheckoutTarget: 'Grace Hopper',
        currentCheckoutAt: '2030-01-03T03:04:05.000Z',
      }),
    ]);

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    try {
      await waitForExpectation(() => {
        const text = secondContainer.textContent ?? '';
        expect(text).not.toContain('Mostrando un solo estado:');
        expect(hasTableHeader(secondContainer, 'Estado')).toBe(true);
      });
    } finally {
      await secondRender.cleanup();
    }
  });

  it('hides empty current-custody columns until a checked-out asset adds real context', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Activo Uno',
        location: 'Sala A',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Activo Dos',
        location: 'Sala B',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('table')).not.toBeNull();
        expect(hasTableHeader(container, 'Equipo')).toBe(true);
        expect(hasTableHeader(container, 'Estado')).toBe(false);
        expect(hasTableHeader(container, 'Tenencia actual')).toBe(false);
        expect(hasTableHeader(container, 'Salida')).toBe(false);
        expect(hasTableHeader(container, 'Ubicación')).toBe(true);
        expect(hasTableHeader(container, 'Acciones')).toBe(true);
        expect(container.textContent).toContain(
          'Quién lo tiene y desde cuándo aparecerán en la tabla cuando al menos un equipo tenga un check-out activo. Usa check-out para registrar la siguiente salida.',
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

  it('compresses current custody details into one scan-friendly summary line plus contact', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Activo Uno',
        location: 'Sala A',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Prestado Uno',
        status: 'Booked',
        location: 'Sala B',
        currentCheckoutTarget: 'Grace Hopper',
        currentCheckoutDisposition: 'rental',
        currentCheckoutHolderEmail: 'grace@example.com',
        currentCheckoutAt: '2030-01-03T03:04:05.000Z',
        currentCheckoutDueAt: '2030-01-05T03:04:05.000Z',
        currentCheckoutPaymentType: 'card',
        currentCheckoutPaymentInstallments: 2,
        currentCheckoutPaymentAmountCents: 250000,
        currentCheckoutPaymentCurrency: 'USD',
        currentCheckoutPaymentOutstandingCents: 50000,
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
        expect(hasTableHeader(container, 'Tenencia actual')).toBe(true);
        expect(hasTableHeader(container, 'Salida')).toBe(false);
        expect(hasTableHeader(container, 'Ubicación')).toBe(true);

        const rows = Array.from(container.querySelectorAll('tbody tr'));
        expect(rows).toHaveLength(2);
        expect(rows[0]?.querySelectorAll('td')).toHaveLength(5);
        expect(rows[1]?.querySelectorAll('td')).toHaveLength(5);

        const custodyCell = rows[1]?.querySelectorAll('td')[2];
        const captionTexts = Array.from(custodyCell?.querySelectorAll('span') ?? [])
          .map((node) => (node.textContent ?? '').trim())
          .filter(Boolean);
        expect(custodyCell?.textContent).toContain('Grace Hopper');
        expect(captionTexts).toContain('grace@example.com');
        expect(captionTexts.some(
          (text) => text.includes('Alquiler')
            && text.includes('Salida:')
            && text.includes('Retorno pactado:')
            && text.includes('Pago: Tarjeta'),
        )).toBe(true);
        expect(captionTexts).toHaveLength(2);
      });
    } finally {
      await cleanup();
    }
  });

  it('matches the table guidance to check-in only rows so first-time admins do not look for missing check-out controls', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Prestado Uno',
        status: 'Booked',
        currentCheckoutTarget: 'Ada Lovelace',
        currentCheckoutAt: '2030-01-02T03:04:05.000Z',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Prestado Dos',
        status: 'Booked',
        currentCheckoutTarget: 'Grace Hopper',
        currentCheckoutAt: '2030-01-03T03:04:05.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('table')).not.toBeNull();
        expect(container.textContent).toContain('Usa check-in para registrar el siguiente retorno.');
        expect(container.textContent).not.toContain('Usa check-out o check-in cuando esté disponible para registrar el siguiente movimiento.');
        expect(container.textContent).not.toContain('Usa check-out para registrar la siguiente salida.');
        expect(container.querySelector('[aria-label="Abrir check-in de Prestado Uno"]')).not.toBeNull();
        expect(container.querySelector('[aria-label="Abrir check-out de Prestado Uno"]')).toBeNull();
        expect(container.querySelector('[aria-label="Abrir check-in de Prestado Dos"]')).not.toBeNull();
        expect(container.querySelector('[aria-label="Abrir check-out de Prestado Dos"]')).toBeNull();
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
        category: 'Interfaz',
        status: 'Booked',
        condition: 'Bueno',
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
        expect(container.textContent).toContain('Condición: Bueno');
      });
    } finally {
      await cleanup();
    }
  });

  it('summarizes one shared condition once and restores row detail when the visible inventory conditions diverge again', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Activo Uno',
        category: 'Micrófono',
        location: 'Sala A',
        condition: 'Excelente',
        status: 'Active',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Retirado Uno',
        category: 'Interfaz',
        location: 'Sala B',
        condition: ' excelente ',
        status: 'Retired',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const text = container.textContent ?? '';
        expect(text).toContain(
          'Mostrando una sola condición: Excelente. El detalle volverá cuando esta vista mezcle condiciones distintas.',
        );
        expect(countOccurrencesIgnoringCase(text, 'Mostrando una sola condición: Excelente.')).toBe(1);
        expect(getRowTextByAssetName(container, 'Activo Uno')).not.toContain('Condición:');
        expect(getRowTextByAssetName(container, 'Retirado Uno')).not.toContain('Condición:');
        expect(countOccurrencesIgnoringCase(text, 'Condición:')).toBe(1);
      });
    } finally {
      await cleanup();
    }

    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Activo Uno',
        category: 'Micrófono',
        location: 'Sala A',
        condition: 'Excelente',
        status: 'Active',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Retirado Uno',
        category: 'Interfaz',
        location: 'Sala B',
        condition: 'Bueno',
        status: 'Retired',
      }),
    ]);

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    try {
      await waitForExpectation(() => {
        const text = secondContainer.textContent ?? '';
        expect(text).not.toContain('Mostrando una sola condición:');
        expect(getRowTextByAssetName(secondContainer, 'Activo Uno')).toContain('Condición: Excelente');
        expect(getRowTextByAssetName(secondContainer, 'Retirado Uno')).toContain('Condición: Bueno');
      });
    } finally {
      await secondRender.cleanup();
    }
  });

  it('hides empty condition labels in table rows so busy inventory views only show real metadata', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({
        assetId: 'asset-1',
        name: 'Activo Uno',
        condition: 'Excelente',
      }),
      buildAsset({
        assetId: 'asset-2',
        name: 'Activo Dos',
        condition: '   ',
      }),
      buildAsset({
        assetId: 'asset-3',
        name: 'Activo Tres',
        condition: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(hasTableHeader(container, 'Equipo')).toBe(true);
        expect(container.textContent).toContain('Activo Uno');
        expect(container.textContent).toContain('Activo Dos');
        expect(container.textContent).toContain('Activo Tres');
        expect(container.textContent).toContain('Condición: Excelente');
        expect(getRowTextByAssetName(container, 'Activo Uno')).toContain('Condición: Excelente');
        expect(getRowTextByAssetName(container, 'Activo Dos')).not.toContain('Condición:');
        expect(getRowTextByAssetName(container, 'Activo Tres')).not.toContain('Condición:');
        expect(countOccurrencesIgnoringCase(container.textContent ?? '', 'Condición:')).toBe(1);
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

  it('keeps the table guidance honest when the current inventory has no movement actions', async () => {
    listAssetsMock.mockResolvedValue([
      buildAsset({ assetId: 'asset-1', name: 'Retirado Uno', status: 'Retired' }),
      buildAsset({ assetId: 'asset-2', name: 'Retirado Dos', status: 'Retired' }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('table')).not.toBeNull();
        expect(container.textContent).toContain('En esta vista no hay movimientos disponibles por ahora.');
        expect(container.textContent).not.toContain(
          'Usa check-out o check-in cuando esté disponible para registrar el siguiente movimiento.',
        );
        expect(container.querySelector('button[aria-label="Abrir check-out de Retirado Uno"]')).toBeNull();
        expect(container.querySelector('button[aria-label="Abrir check-in de Retirado Uno"]')).toBeNull();
        expect(container.querySelector('button[aria-label="Abrir check-out de Retirado Dos"]')).toBeNull();
        expect(container.querySelector('button[aria-label="Abrir check-in de Retirado Dos"]')).toBeNull();
        expect(container.querySelectorAll('button[aria-label^="Abrir QR, enlace e historial de "]')).toHaveLength(2);
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

      await openSingleAssetSecondaryAction(container, 'Historial');

      await waitForExpectation(() => {
        expect(historyMock).toHaveBeenCalledWith('asset-1');
        expect(container.textContent).toContain('Historial · Neumann U87');
        expect(container.textContent).toContain(
          'Todavía no hay movimientos registrados para este equipo. Cuando ocurra el primero, aquí verás salida, devolución, destino y notas.',
        );
        expect(hasTableHeader(container, 'Salida')).toBe(false);
      });

      await act(async () => {
        const actionsButton = container.querySelector<HTMLButtonElement>(
          'button[aria-label="Abrir QR, enlace e historial de Neumann U87"]',
        );
        actionsButton?.click();
        await flushPromises();
      });

      await waitForExpectation(() => {
        const historyMenuItem = Array.from(document.body.querySelectorAll<HTMLElement>('[role="menuitem"]')).find(
          (item) => (item.textContent ?? '').trim() === 'Historial abierto aquí abajo',
        );
        expect(historyMenuItem).toBeDefined();
        expect(historyMenuItem?.getAttribute('aria-disabled')).toBe('true');
      });

      await act(async () => {
        const historyMenuItem = Array.from(document.body.querySelectorAll<HTMLElement>('[role="menuitem"]')).find(
          (item) => (item.textContent ?? '').trim() === 'Historial abierto aquí abajo',
        );
        historyMenuItem?.click();
        await flushPromises();
        await flushPromises();
      });

      expect(historyMock).toHaveBeenCalledTimes(1);

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
      buildCheckoutHistoryEntry({
        paymentType: 'card',
        paymentInstallments: 3,
        paymentAmountCents: 250000,
        paymentCurrency: 'USD',
        paymentOutstandingCents: 50000,
      }),
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

      await openSingleAssetSecondaryAction(container, 'Historial');

      await waitForExpectation(() => {
        expect(historyMock).toHaveBeenCalledTimes(2);
        expect(container.textContent).toContain('Historial · Neumann U87');
        expect(container.textContent).toContain('Uso en grabación.');
        expect(container.textContent).toContain('Tarjeta');
        expect(container.textContent).toContain('3 cuotas');
        expect(container.textContent).toContain('saldo');
      });
    } finally {
      await cleanup();
    }
  });
});
