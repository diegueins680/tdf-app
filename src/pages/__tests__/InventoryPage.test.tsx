import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { vi } from 'vitest';
import InventoryPage from '../InventoryPage';
import type { AssetDTO } from '../../api/types';

const mockList = vi.fn();
const mockCreate = vi.fn();
const mockUpdate = vi.fn();
const mockRemove = vi.fn();

vi.mock('../../api/inventory', () => ({
  Inventory: {
    list: (params?: { q?: string; page?: number; pageSize?: number }) => mockList(params),
    create: (payload: unknown) => mockCreate(payload),
    update: (assetId: string, payload: unknown) => mockUpdate(assetId, payload),
    remove: (assetId: string) => mockRemove(assetId),
  },
}));

function createQueryClient() {
  return new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });
}

function renderPage() {
  const queryClient = createQueryClient();

  return render(
    <QueryClientProvider client={queryClient}>
      <InventoryPage />
    </QueryClientProvider>,
  );
}

function buildAsset(overrides: Partial<AssetDTO> = {}): AssetDTO {
  return {
    assetId: 'asset-1',
    name: 'Neumann U87',
    category: 'Micrófono',
    status: 'Active',
    location: 'Sala A',
    ...overrides,
  };
}

describe('InventoryPage', () => {
  beforeAll(() => {
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
    vi.clearAllMocks();
    mockList.mockResolvedValue({
      items: [buildAsset()],
      page: 1,
      pageSize: 20,
      total: 1,
    });
    mockCreate.mockResolvedValue(undefined);
    mockUpdate.mockResolvedValue(undefined);
    mockRemove.mockResolvedValue(undefined);
  });

  it('keeps the toolbar focused on live actions and explains that bulk import is not available yet', async () => {
    renderPage();

    expect(await screen.findByText('Inventario')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.queryByRole('button', { name: /Importar CSV/i })).not.toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Exportar CSV/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Agregar activo/i })).toBeInTheDocument();
      expect(
        screen.getByText(
          /La importación masiva todavía no está disponible\. Por ahora usa Agregar activo para nuevos equipos\./i,
        ),
      ).toBeInTheDocument();
    });
  });

  it('keeps one edit control per row and sends the QR action directly to print', async () => {
    const user = userEvent.setup();
    const printMock = vi.fn();
    const writeMock = vi.fn();
    const closeMock = vi.fn();

    vi.spyOn(window, 'open').mockImplementation(
      () =>
        ({
          document: {
            write: writeMock,
            close: closeMock,
          },
          print: printMock,
        }) as unknown as Window,
    );

    renderPage();

    expect(await screen.findByText('Neumann U87')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getAllByLabelText('Editar activo Neumann U87')).toHaveLength(1);
      expect(screen.getByLabelText('Imprimir QR de Neumann U87')).toBeInTheDocument();
    });

    await user.click(screen.getByLabelText('Imprimir QR de Neumann U87'));

    expect(window.open).toHaveBeenCalledTimes(1);
    expect(writeMock).toHaveBeenCalledTimes(1);
    expect(closeMock).toHaveBeenCalledTimes(1);
    expect(printMock).toHaveBeenCalledTimes(1);
    expect(screen.queryByRole('menu')).not.toBeInTheDocument();
    expect(screen.queryByRole('menuitem', { name: /editar/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Editar…')).not.toBeInTheDocument();
  });
});
