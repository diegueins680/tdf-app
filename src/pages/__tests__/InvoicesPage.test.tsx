import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import { vi } from 'vitest';
import InvoicesPage from '../InvoicesPage';
import type { InvoiceDTO } from '../../api/types';

const mockInvoicesList = vi.fn();
const mockInvoicesCreate = vi.fn();

vi.mock('../../api/invoices', () => ({
  Invoices: {
    list: () => mockInvoicesList(),
    create: (body: unknown) => mockInvoicesCreate(body),
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
        <InvoicesPage />
      </QueryClientProvider>
    </MemoryRouter>,
  );
}

function buildInvoice(overrides: Partial<InvoiceDTO> = {}): InvoiceDTO {
  return {
    invId: 101,
    number: 'FAC-001',
    statusI: 'Issued',
    subtotalC: 10000,
    taxC: 1200,
    totalC: 11200,
    currency: 'USD',
    customerId: 7,
    notes: null,
    receiptId: 501,
    lineItems: [],
    ...overrides,
  };
}

describe('InvoicesPage', () => {
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
    mockInvoicesList.mockResolvedValue([]);
    mockInvoicesCreate.mockResolvedValue(buildInvoice());
  });

  it('keeps the initial invoice loading state compact instead of showing empty table controls', async () => {
    mockInvoicesList.mockImplementation(() => new Promise(() => undefined));

    renderPage();

    expect(await screen.findByText('Facturación')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText(/Cargando facturas y recibos asociados…/i)).toBeInTheDocument();
      expect(screen.getByText('Cargando facturas…')).toBeInTheDocument();
    });

    expect(screen.getByRole('button', { name: /Nueva factura/i })).toBeInTheDocument();
    expect(screen.queryByRole('table')).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^#$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Total$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Recibo$/i })).not.toBeInTheDocument();
  });

  it('replaces the empty invoices table with one first-invoice action and clear guidance', async () => {
    renderPage();

    expect(await screen.findByText('Facturación')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByRole('button', { name: /Nueva factura/i })).toBeInTheDocument();
      expect(screen.getByText('Todavía no hay facturas registradas.')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Empieza con Nueva factura\. La tabla y enlaces a recibos aparecerán cuando exista la primera factura\./i,
        ),
      ).toBeInTheDocument();
      expect(
        screen.getByText(
          /Usa Nueva factura para emitir la primera\. Cuando exista al menos una, aquí podrás revisar totales, estado y recibos sin navegar una tabla vacía\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('table')).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^#$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Total$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Subtotal$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^IVA$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Recibo$/i })).not.toBeInTheDocument();
    expect(screen.queryByText(/^No hay facturas registradas\.$/i)).not.toBeInTheDocument();
  });

  it('keeps the invoice table available once invoice rows exist', async () => {
    mockInvoicesList.mockResolvedValue([buildInvoice()]);

    renderPage();

    expect(await screen.findByText('Facturación')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByRole('table')).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^#$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Total$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Estado$/i })).toBeInTheDocument();
      expect(screen.getByText('FAC-001')).toBeInTheDocument();
      expect(screen.getByText('Issued')).toBeInTheDocument();
      expect(screen.getByRole('link', { name: /Ver recibo/i })).toBeInTheDocument();
      expect(
        screen.getByText(
          /Revisa facturas emitidas, totales, estado y recibos asociados desde esta vista\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByText('Todavía no hay facturas registradas.')).not.toBeInTheDocument();
  });
});
