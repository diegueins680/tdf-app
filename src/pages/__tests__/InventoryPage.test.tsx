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
      expect(screen.getByRole('button', { name: /Exportar CSV/i })).toBeEnabled();
      expect(screen.getByRole('button', { name: /Agregar activo/i })).toBeInTheDocument();
      expect(
        screen.getByText(
          /La importación masiva todavía no está disponible\. Por ahora usa Agregar activo para nuevos equipos\./i,
        ),
      ).toBeInTheDocument();
    });
  });

  it('replaces the first-run empty table with a focused setup state until the first asset exists', async () => {
    mockList.mockResolvedValue({
      items: [],
      page: 1,
      pageSize: 20,
      total: 0,
    });

    renderPage();

    expect(await screen.findByText('Inventario')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.queryByRole('button', { name: /Exportar CSV/i })).not.toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Agregar activo/i })).toBeInTheDocument();
      expect(
        screen.getByText(
          /Empieza con Agregar activo\. La exportación CSV aparecerá cuando exista el primer equipo\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText('Todavía no hay activos cargados.')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Usa Agregar activo para registrar el primero\. Cuando exista al menos uno, aquí podrás buscarlo, editarlo, imprimir su QR y exportar el inventario actual\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('textbox', { name: /Buscar/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Nombre$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Categoría$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Ubicación$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acciones$/i })).not.toBeInTheDocument();
    expect(screen.queryByText('No se encontraron activos.')).not.toBeInTheDocument();
  });

  it('replaces the single-row table with a compact first-asset summary and keeps one action entry point', async () => {
    const user = userEvent.setup();
    renderPage();

    expect(await screen.findByText('Inventario')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primer activo registrado.')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Revísalo aquí; cuando exista el segundo, volverán la búsqueda y la tabla para comparar equipos\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText('Neumann U87')).toBeInTheDocument();
      expect(screen.getByText('Categoría: Micrófono')).toBeInTheDocument();
      expect(screen.getByText('Estado: Activo')).toBeInTheDocument();
      expect(screen.getByText('Ubicación: Sala A')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Acciones de Neumann U87' })).toBeInTheDocument();
    });

    expect(screen.queryByRole('textbox', { name: /Buscar/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Nombre$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Categoría$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Ubicación$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acciones$/i })).not.toBeInTheDocument();
    expect(screen.queryByLabelText('Editar activo Neumann U87')).not.toBeInTheDocument();
    expect(screen.queryByLabelText('Imprimir QR de Neumann U87')).not.toBeInTheDocument();
    expect(screen.queryByRole('menuitem', { name: /Editar activo/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('menuitem', { name: /Imprimir QR/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('menuitem', { name: /Eliminar activo/i })).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: 'Acciones de Neumann U87' }));

    expect(await screen.findByRole('menuitem', { name: /Editar activo/i })).toBeInTheDocument();
    expect(screen.getByRole('menuitem', { name: /Imprimir QR/i })).toBeInTheDocument();
    expect(screen.getByRole('menuitem', { name: /Eliminar activo/i })).toBeInTheDocument();
  });

  it('uses one row actions trigger and reveals explicit labels only when the menu opens', async () => {
    const user = userEvent.setup();
    const printMock = vi.fn();
    const writeMock = vi.fn();
    const closeMock = vi.fn();
    mockList.mockResolvedValue({
      items: [
        buildAsset(),
        buildAsset({
          assetId: 'asset-2',
          name: 'Yamaha HS8',
          category: 'Monitor',
          status: 'Booked',
          location: 'Sala B',
        }),
      ],
      page: 1,
      pageSize: 20,
      total: 2,
    });

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
      expect(screen.getAllByRole('button', { name: /^Acciones de /i })).toHaveLength(2);
    });

    expect(screen.queryByLabelText('Editar activo Neumann U87')).not.toBeInTheDocument();
    expect(screen.queryByLabelText('Imprimir QR de Neumann U87')).not.toBeInTheDocument();
    expect(screen.queryByLabelText('Eliminar activo Neumann U87')).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: 'Acciones de Neumann U87' }));

    expect(await screen.findByRole('menuitem', { name: /Editar activo/i })).toBeInTheDocument();
    expect(screen.getByRole('menuitem', { name: /Imprimir QR/i })).toBeInTheDocument();
    expect(screen.getByRole('menuitem', { name: /Eliminar activo/i })).toBeInTheDocument();

    await user.click(screen.getByRole('menuitem', { name: /Imprimir QR/i }));

    expect(window.open).toHaveBeenCalledTimes(1);
    expect(writeMock).toHaveBeenCalledTimes(1);
    expect(closeMock).toHaveBeenCalledTimes(1);
    expect(printMock).toHaveBeenCalledTimes(1);
    expect(screen.queryByRole('menu')).not.toBeInTheDocument();
    expect(screen.queryByRole('menuitem', { name: /Editar activo/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('menuitem', { name: /Imprimir QR/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('menuitem', { name: /Eliminar activo/i })).not.toBeInTheDocument();
  });
});
