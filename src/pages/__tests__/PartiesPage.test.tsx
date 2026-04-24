import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { render, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { vi } from 'vitest';
import PartiesPage from '../PartiesPage';
import type { PartyDTO } from '../../api/types';

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

function buildParty(overrides: Partial<PartyDTO> = {}): PartyDTO {
  return {
    partyId: 101,
    displayName: 'Acme Studios',
    isOrg: true,
    legalName: 'Acme Studios S.A.',
    primaryEmail: 'booking@acme.test',
    primaryPhone: '+593999000111',
    whatsapp: null,
    instagram: '@acmestudios',
    taxId: null,
    emergencyContact: null,
    notes: null,
    ...overrides,
  };
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

  it('collapses the first contact into a compact summary with one actions entry point', async () => {
    const user = userEvent.setup();
    mockPartiesList.mockResolvedValue([buildParty()]);

    renderPage();

    expect(await screen.findByText('Personas / CRM')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primer contacto registrado.')).toBeInTheDocument();
      expect(
        screen.getByText(
          /Revísalo aquí; cuando exista el segundo, volverán la búsqueda y la tabla para gestionar varios contactos sin acciones duplicadas\./i,
        ),
      ).toBeInTheDocument();
      expect(screen.getByText('Acme Studios')).toBeInTheDocument();
      expect(screen.getByText('Tipo: Organización')).toBeInTheDocument();
      expect(screen.getByText('Correo: booking@acme.test')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Acciones de Acme Studios/i })).toBeInTheDocument();
      expect(
        screen.getByText(/Nueva Banda se habilita cuando exista al menos una persona para asignarla como integrante\./i),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('button', { name: /Nueva Banda/i })).not.toBeInTheDocument();
    expect(screen.queryByPlaceholderText(/Buscar…/i)).not.toBeInTheDocument();
    expect(screen.queryByRole('table')).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Nombre$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('menuitem', { name: /Abrir ficha/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('menuitem', { name: /Editar contacto/i })).not.toBeInTheDocument();

    await user.click(screen.getByRole('button', { name: /Acciones de Acme Studios/i }));

    expect(await screen.findByRole('menuitem', { name: /Abrir ficha/i })).toBeInTheDocument();
    expect(screen.getByRole('menuitem', { name: /Editar contacto/i })).toBeInTheDocument();
    expect(screen.queryByRole('menuitem', { name: /Convertir a estudiante/i })).not.toBeInTheDocument();
  });

  it('hides empty optional contact rows in the one-contact summary so onboarding stays focused', async () => {
    mockPartiesList.mockResolvedValue([
      buildParty({
        isOrg: false,
        primaryEmail: '   ',
        primaryPhone: null,
        whatsapp: '   ',
        instagram: null,
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Personas / CRM')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByText('Primer contacto registrado.')).toBeInTheDocument();
      expect(screen.getByText('Tipo: Persona')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /Acciones de Acme Studios/i })).toBeInTheDocument();
    });

    expect(screen.queryByText(/Correo:/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Teléfono \/ WhatsApp:/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/Instagram:/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/^—$/i)).not.toBeInTheDocument();
  });

  it('uses one explicit actions trigger per CRM row and reveals role-specific actions inside the menu', async () => {
    const user = userEvent.setup();
    mockPartiesList.mockResolvedValue([
      buildParty(),
      buildParty({
        partyId: 102,
        displayName: 'Ada Lovelace',
        isOrg: false,
        legalName: null,
        primaryEmail: 'ada@example.com',
        primaryPhone: null,
        instagram: null,
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Personas / CRM')).toBeInTheDocument();

    const orgRow = await screen.findByText('Acme Studios').then((cell) => cell.closest('tr'));
    const personRow = await screen.findByText('Ada Lovelace').then((cell) => cell.closest('tr'));

    if (!(orgRow instanceof HTMLElement) || !(personRow instanceof HTMLElement)) {
      throw new Error('Expected CRM rows to render for both contacts');
    }

    await waitFor(() => {
      expect(within(orgRow).getAllByRole('button')).toHaveLength(1);
      expect(within(personRow).getAllByRole('button')).toHaveLength(1);
      expect(
        within(orgRow).getByRole('button', { name: /Acciones de Acme Studios/i }),
      ).toBeInTheDocument();
      expect(
        within(personRow).getByRole('button', { name: /Acciones de Ada Lovelace/i }),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('menuitem', { name: /Abrir ficha/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('menuitem', { name: /Editar contacto/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('menuitem', { name: /Convertir a estudiante/i })).not.toBeInTheDocument();

    await user.click(within(orgRow).getByRole('button', { name: /Acciones de Acme Studios/i }));

    expect(await screen.findByRole('menuitem', { name: /Abrir ficha/i })).toBeInTheDocument();
    expect(screen.getByRole('menuitem', { name: /Editar contacto/i })).toBeInTheDocument();
    expect(screen.queryByRole('menuitem', { name: /Convertir a estudiante/i })).not.toBeInTheDocument();

    await user.click(within(personRow).getByRole('button', { name: /Acciones de Ada Lovelace/i }));

    expect(await screen.findByRole('menuitem', { name: /Convertir a estudiante/i })).toBeInTheDocument();
  });

  it('hides empty contact columns once the CRM table is in compact mode', async () => {
    mockPartiesList.mockResolvedValue([
      buildParty({
        primaryEmail: '   ',
        instagram: null,
      }),
      buildParty({
        partyId: 102,
        displayName: 'Ada Lovelace',
        isOrg: false,
        legalName: null,
        primaryEmail: null,
        primaryPhone: null,
        instagram: '   ',
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Personas / CRM')).toBeInTheDocument();

    await waitFor(() => {
      expect(screen.getByRole('table')).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Nombre$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Org$/i })).toBeInTheDocument();
      expect(screen.getByRole('columnheader', { name: /^Acciones$/i })).toBeInTheDocument();
      expect(
        screen.getByText(
          /Vista compacta: Email e Instagram aparecerán cuando exista información real en esos campos\./i,
        ),
      ).toBeInTheDocument();
    });

    expect(screen.queryByRole('columnheader', { name: /^Email$/i })).not.toBeInTheDocument();
    expect(screen.queryByRole('columnheader', { name: /^Instagram$/i })).not.toBeInTheDocument();
    expect(screen.queryByText(/^—$/i)).not.toBeInTheDocument();
  });
});
