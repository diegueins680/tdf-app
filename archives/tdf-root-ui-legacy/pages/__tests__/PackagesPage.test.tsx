import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { MemoryRouter } from 'react-router-dom';
import { vi } from 'vitest';
import PackagesPage from '../PackagesPage';
import type { PackageProductDTO } from '../../api/types';

const mockListProducts = vi.fn();
const mockCreateProduct = vi.fn();
const mockUpdateProduct = vi.fn();
const mockCreatePurchase = vi.fn();

vi.mock('../../api/packages', () => ({
  PackagesApi: {
    listProducts: () => mockListProducts(),
    createProduct: (body: unknown) => mockCreateProduct(body),
    updateProduct: (id: number, body: unknown) => mockUpdateProduct(id, body),
    createPurchase: (body: unknown) => mockCreatePurchase(body),
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
    <MemoryRouter>
      <QueryClientProvider client={queryClient}>
        <PackagesPage />
      </QueryClientProvider>
    </MemoryRouter>,
  );
}

function buildProduct(overrides: Partial<PackageProductDTO> = {}): PackageProductDTO {
  return {
    ppId: 301,
    ppName: '10 horas de estudio',
    ppService: 'Recording',
    ppUnitsKind: 'Hours',
    ppUnitsQty: 10,
    ppPriceCents: 25000,
    ...overrides,
  };
}

describe('PackagesPage', () => {
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
    mockListProducts.mockResolvedValue([]);
    mockCreateProduct.mockResolvedValue(buildProduct());
    mockUpdateProduct.mockResolvedValue(buildProduct());
    mockCreatePurchase.mockResolvedValue(undefined);
  });

  it('keeps first-run package setup focused on the first product instead of dead-end purchase actions', async () => {
    renderPage();

    expect(await screen.findByText('Paquetes')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Todavía no hay paquetes configurados.')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Empieza con Nuevo producto\. La compra y la vista resumida aparecerán cuando exista el primer paquete para vender\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Nuevo producto/i })).toBeInTheDocument();
    });

    expect(screen.getByRole('tab', { name: /Catálogo/i })).toBeInTheDocument();
    expect(screen.queryByRole('tab', { name: /Compras/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('link', { name: /Vista resumida/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Registrar compra/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('table')).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Producto$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Servicio$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Unidades$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Precio$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acciones$/i })).not.toBeInTheDocument();
    expect(screen.queryByText('Aún no hay productos creados.')).not.toBeInTheDocument();
  });

  it('keeps purchases and summary available once a package product exists', async () => {
    const user = userEvent.setup();
    mockListProducts.mockResolvedValue([buildProduct()]);

    renderPage();

    expect(await screen.findByText('Paquetes')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByRole('table')).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Producto$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Servicio$/i })).toBeInTheDocument();
      expect(screen.getByText('10 horas de estudio')).toBeInTheDocument();
      expect(screen.getByRole('link', { name: /Vista resumida/i })).toBeInTheDocument();
      expect(screen.getByRole('tab', { name: /Compras/i })).toBeInTheDocument();
    });

    expect(screen.queryByText('Todavía no hay paquetes configurados.')).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Registrar compra/i })).not.toBeInTheDocument();

    await user.click(screen.getByRole('tab', { name: /Compras/i }));

    expect(screen.getByRole('button', { name: /Registrar compra/i })).toBeInTheDocument();
    expect(
      screen.getByText(
        /La compra generará movimientos en los saldos del paquete y, si está habilitado, emitirá una factura borrador ligada al booking o cliente\./i,
      ),
    ).toBeInTheDocument();
  });
});
