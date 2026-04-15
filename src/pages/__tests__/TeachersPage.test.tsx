import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { render, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { vi } from 'vitest';
import TeachersPage from '../TeachersPage';
import type { PartyDTO } from '../../api/types';

const mockList = vi.fn();
const mockAddRole = vi.fn();
const mockCreate = vi.fn();

vi.mock('../../api/parties', () => ({
  Parties: {
    list: () => mockList(),
    addRole: (partyId: number, role: string) => mockAddRole(partyId, role),
    create: (payload: unknown) => mockCreate(payload),
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
      <TeachersPage />
    </QueryClientProvider>,
  );
}

function buildParty(overrides: Partial<PartyDTO> = {}): PartyDTO {
  return {
    partyId: 1,
    displayName: 'Ada Lovelace',
    isOrg: false,
    primaryEmail: 'ada@example.com',
    primaryPhone: null,
    notes: null,
    ...overrides,
  };
}

describe('TeachersPage', () => {
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
    window.localStorage.clear();
    mockList.mockResolvedValue([
      buildParty(),
      buildParty({
        partyId: 2,
        displayName: 'Grace Hopper',
        primaryEmail: 'grace@example.com',
      }),
    ]);
    mockAddRole.mockResolvedValue(undefined);
    mockCreate.mockResolvedValue(buildParty({ partyId: 99 }));
  });

  it('replaces the empty roster table with one first-step action until the first teacher exists', async () => {
    mockList.mockResolvedValue([]);

    renderPage();

    expect(await screen.findByText('Profesores')).toBeTruthy();

    await waitFor(() => {
      expect(screen.getByText('Todavía no hay profesores registrados.')).toBeTruthy();
      expect(
        screen.getByText(
          /Usa Nuevo profesor para crear el primero\. Cuando exista, aquí podrás confirmar si ya está en el roster y sumar más docentes sin revisar filtros vacíos\./i,
        ),
      ).toBeTruthy();
      expect(
        screen.getByText(
          /Empieza con Nuevo profesor\. La búsqueda, el filtro de roster y la tabla aparecerán cuando exista el primer contacto\./i,
        ),
      ).toBeTruthy();
    });

    expect(screen.getByRole('button', { name: /Nuevo profesor/i })).toBeTruthy();
    expect(screen.queryByRole('textbox', { name: /Buscar/i })).toBeNull();
    expect(screen.queryByRole('checkbox', { name: /Solo roster/i })).toBeNull();
    expect(screen.queryByRole('table')).toBeNull();
    expect(screen.queryByRole('columnheader', { name: /^Nombre$/i })).toBeNull();
    expect(screen.queryByRole('columnheader', { name: /^Contacto$/i })).toBeNull();
    expect(screen.queryByRole('columnheader', { name: /^Notas$/i })).toBeNull();
    expect(screen.queryByRole('columnheader', { name: /^Acciones$/i })).toBeNull();
  });

  it('replaces the one-row roster table with a compact first-contact summary and one clear action', async () => {
    const user = userEvent.setup();
    mockList.mockResolvedValue([
      buildParty({
        primaryPhone: '+593999000111',
        notes: 'Disponible los sábados',
      }),
    ]);

    renderPage();

    expect(await screen.findByText('Profesores')).toBeTruthy();

    await waitFor(() => {
      expect(screen.getByText('Primer contacto docente disponible.')).toBeTruthy();
      expect(
        screen.getByText(
          /Confirma aquí su información principal y usa una sola acción para sumarlo al roster si hace falta\./i,
        ),
      ).toBeTruthy();
      expect(
        screen.getByText(
          /Revísalo aquí; cuando exista el segundo, volverán la búsqueda, el filtro de roster y la tabla para comparar docentes\./i,
        ),
      ).toBeTruthy();
      expect(screen.getByText('Ada Lovelace')).toBeTruthy();
      expect(screen.getByText(/Contacto:\s*ada@example\.com · \+593999000111/i)).toBeTruthy();
      expect(screen.getByText(/Notas:\s*Disponible los sábados/i)).toBeTruthy();
      expect(screen.getByText(/Roster:\s*Pendiente de agregar/i)).toBeTruthy();
      expect(screen.getByRole('button', { name: /Agregar al roster/i })).toBeTruthy();
    });

    expect(screen.queryByRole('textbox', { name: /Buscar/i })).toBeNull();
    expect(screen.queryByRole('checkbox', { name: /Solo roster/i })).toBeNull();
    expect(screen.queryByRole('table')).toBeNull();
    expect(screen.queryByRole('columnheader', { name: /^Nombre$/i })).toBeNull();
    expect(screen.queryByText('Ya en roster')).toBeNull();

    await user.click(screen.getByRole('button', { name: /Agregar al roster/i }));

    expect(mockAddRole).toHaveBeenCalledTimes(1);
    expect(mockAddRole).toHaveBeenCalledWith(1, 'Teacher');
  });

  it('removes the redundant add action for contacts already in the teacher roster', async () => {
    const user = userEvent.setup();
    window.localStorage.setItem('tdf-teacher-roster', JSON.stringify([1]));

    renderPage();

    expect(await screen.findByText('Profesores')).toBeTruthy();

    const adaRow = await screen.findByText('Ada Lovelace').then((cell) => cell.closest('tr'));
    const graceRow = await screen.findByText('Grace Hopper').then((cell) => cell.closest('tr'));

    if (!(adaRow instanceof HTMLElement) || !(graceRow instanceof HTMLElement)) {
      throw new Error('Expected both teacher rows to render');
    }

    await waitFor(() => {
      expect(within(adaRow).getByText('Ya en roster')).toBeTruthy();
      expect(
        within(adaRow).queryByRole('button', { name: /Agregar al roster/i }),
      ).toBeNull();
      expect(
        within(graceRow).getByRole('button', { name: /Agregar al roster/i }),
      ).toBeTruthy();
    });

    await user.click(within(graceRow).getByRole('button', { name: /Agregar al roster/i }));

    expect(mockAddRole).toHaveBeenCalledTimes(1);
    expect(mockAddRole).toHaveBeenCalledWith(2, 'Teacher');
  });
});
