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
    mockInvoicesList.mockResolvedValue([
      buildInvoice(),
      buildInvoice({
        invId: 102,
        number: 'FAC-002',
        statusI: 'Draft',
        subtotalC: 5000,
        taxC: 600,
        totalC: 5600,
        receiptId: null,
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Facturación')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByRole('table')).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^#$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Total$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Estado$/i })).toBeInTheDocument();
      expect(screen.getByText('FAC-001')).toBeInTheDocument();
      expect(screen.getByText('FAC-002')).toBeInTheDocument();
      expect(screen.getByText('Emitida')).toHaveAttribute('title', 'Issued');
      expect(screen.getByText('Borrador')).toHaveAttribute('title', 'Draft');
      expect(screen.getByRole('link', { name: /Ver recibo/i })).toBeInTheDocument();
      expect(
        screen.getByText(
          /Revisa facturas emitidas, totales, estado y recibos asociados desde esta vista\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByText('Todavía no hay facturas registradas.')).not.toBeInTheDocument();
    expect(screen.queryByText('Primera factura registrada.')).not.toBeInTheDocument();
  });

  it('replaces the one-invoice table with a compact first-invoice summary', async () => {
    mockInvoicesList.mockResolvedValue([buildInvoice()]);

    renderPage();

    expect(await screen.findByText('Facturación')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primera factura registrada.')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Revisa la primera factura aquí\. La tabla aparecerá cuando exista una segunda para comparar\./i,
        ),
      ).toBeInTheDocument();
      expect(
        screen.getByText(
          /Revísala aquí; cuando exista una segunda, volverá la tabla para comparar totales, estado y recibos\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText(/Factura:\s*#FAC-001/i)).toBeInTheDocument();
      expect(screen.getByText(/Total:/i)).toHaveTextContent(/112/);
      expect(screen.getByText(/Estado:\s*Emitida/i)).toHaveAttribute('title', 'Issued');
      expect(screen.getByText(/Subtotal \/ IVA:/i)).toHaveTextContent(/100.*12/);
      expect(screen.getByRole('link', { name: /Ver recibo/i })).toBeInTheDocument();
    });

    expect(screen.queryByRole('table')).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^#$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Total$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Estado$/i })).not.toBeInTheDocument();
    expect(screen.queryByText(/^Issued$/i)).not.toBeInTheDocument();
  });
});
