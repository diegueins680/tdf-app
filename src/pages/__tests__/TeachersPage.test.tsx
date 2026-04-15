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
