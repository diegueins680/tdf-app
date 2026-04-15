import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { render, screen, waitFor } from '@testing-library/react';
import { vi } from 'vitest';
import PartiesPage from '../PartiesPage';

const mockPartiesList = vi.fn();
const mockPartiesCreate = vi.fn();
const mockPartiesGetOne = vi.fn();
const mockPartiesUpdate = vi.fn();
const mockPartiesAddRole = vi.fn();
const mockBandsList = vi.fn();
const mockBandsCreate = vi.fn();
const mockBandsDetail = vi.fn();
const mockConvertPartyToStudent = vi.fn();

vi.mock('../../api/parties', () => ({
  Parties: {
    list: () => mockPartiesList(),
    create: (body: unknown) => mockPartiesCreate(body),
    getOne: (id: number) => mockPartiesGetOne(id),
    update: (id: number, body: unknown) => mockPartiesUpdate(id, body),
    addRole: (id: number, role: string) => mockPartiesAddRole(id, role),
  },
}));

vi.mock('../../api/bands', () => ({
  Bands: {
    list: (params?: { page?: number; pageSize?: number }) => mockBandsList(params),
    create: (body: unknown) => mockBandsCreate(body),
    detail: (id: string) => mockBandsDetail(id),
  },
}));

vi.mock('../../utils/partyRoleHelpers', () => ({
  convertPartyToStudent: (party: unknown) => mockConvertPartyToStudent(party),
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
      <PartiesPage />
    </QueryClientProvider>,
  );
}

describe('PartiesPage', () => {
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
    mockPartiesList.mockResolvedValue([]);
    mockPartiesCreate.mockResolvedValue(undefined);
    mockPartiesGetOne.mockResolvedValue(undefined);
    mockPartiesUpdate.mockResolvedValue(undefined);
    mockPartiesAddRole.mockResolvedValue(undefined);
    mockBandsList.mockResolvedValue({ items: [], page: 1, pageSize: 20, total: 0 });
    mockBandsCreate.mockResolvedValue(undefined);
    mockBandsDetail.mockResolvedValue(undefined);
    mockConvertPartyToStudent.mockResolvedValue({ studentId: 1 });
  });

  it('replaces the empty CRM table with one first-contact action and hides the dead-end band flow', async () => {
    renderPage();

    expect(await screen.findByText('Personas / CRM')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Todavía no hay contactos cargados.')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Usa Nueva Persona para registrar el primer cliente, proveedor o colaborador\./i,
        ),
      ).toBeInTheDocument();
      expect(
        screen.getByText(
          /Empieza con Nueva Persona\. La búsqueda, el detalle y la creación de bandas aparecerán cuando exista el primer contacto\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.getByRole('button', { name: /Nueva Persona/i })).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Nueva Banda/i })).not.toBeInTheDocument();
    expect(screen.queryByPlaceholderText(/Buscar…/i)).not.toBeInTheDocument();
    expect(screen.queryByRole('table')).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Nombre$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Org$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Email$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Instagram$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Acciones$/i })).not.toBeInTheDocument();
  });
});
