import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { useState, type ReactElement } from 'react';
import type { PartySelectorOption, PartySelectorPage } from '../../api/partySelector';

const searchPartiesForSelector = jest.fn<
  (params: { query: string; cursor?: number; signal?: AbortSignal }) => Promise<PartySelectorPage>
>();

jest.unstable_mockModule('../../api/partySelector', () => ({
  searchPartiesForSelector,
}));

const { PartyMultiSelector, PartySelector } = await import('./PartySelector');

const ana: PartySelectorOption = {
  partyId: 17,
  partyType: 'person',
  displayName: 'Ana María Ruiz',
  username: 'anaruiz',
  avatarUrl: null,
  secondaryLabel: 'Artista',
  accountStatus: 'active',
};

const beatriz: PartySelectorOption = {
  ...ana,
  partyId: 29,
  displayName: 'Beatriz Núñez',
  username: 'bea',
};

function Harness() {
  const [value, setValue] = useState<PartySelectorOption[]>([]);
  return (
    <PartyMultiSelector
      value={value}
      onChange={setValue}
      field={{ label: 'Contactos de campaña', required: true }}
      search={{ kind: 'any', accountOnly: false }}
    />
  );
}

const renderWithQueryClient = (element: ReactElement) => {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });
  return render(
    <QueryClientProvider client={queryClient}>
      {element}
    </QueryClientProvider>,
  );
};

const renderHarness = () => renderWithQueryClient(<Harness />);

describe('PartyMultiSelector', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    searchPartiesForSelector.mockResolvedValue({ items: [ana], nextCursor: null });
  });

  it('does not treat typed text as a selection and renders an identity chip after choosing', async () => {
    renderHarness();
    const input = screen.getByRole('combobox', { name: 'Contactos de campaña' });

    fireEvent.change(input, { target: { value: 'Ana' } });
    expect(screen.queryByText('Ana María Ruiz · @anaruiz')).not.toBeInTheDocument();

    const option = await screen.findByRole('option', { name: /Ana María Ruiz/i }, { timeout: 1500 });
    fireEvent.click(option);

    expect(screen.getByText('Ana María Ruiz · @anaruiz')).toBeInTheDocument();
    await waitFor(() => expect(searchPartiesForSelector).toHaveBeenCalledTimes(1));
    expect(searchPartiesForSelector).toHaveBeenCalledWith(expect.objectContaining({
      query: 'Ana',
      kind: 'any',
      accountOnly: false,
    }));
  });

  it('shows an initial single selection without searching for its display label', () => {
    renderWithQueryClient(
      <PartySelector
        value={ana}
        onChange={jest.fn()}
        field={{ label: 'Artista' }}
      />,
    );

    expect(screen.getByRole('combobox', { name: 'Artista' })).toHaveValue('Ana María Ruiz');
    expect(searchPartiesForSelector).not.toHaveBeenCalled();
  });

  it('keeps existing selections when a later search fails', async () => {
    renderHarness();
    const input = screen.getByRole('combobox', { name: 'Contactos de campaña' });
    fireEvent.change(input, { target: { value: 'Ana' } });
    fireEvent.click(await screen.findByRole('option', { name: /Ana María Ruiz/i }, { timeout: 1500 }));

    searchPartiesForSelector.mockRejectedValueOnce(new Error('sin conexión'));
    fireEvent.change(input, { target: { value: 'Beatriz' } });

    expect(await screen.findByRole('alert', {}, { timeout: 1500 })).toHaveTextContent(
      'No se modificaron las selecciones',
    );
    expect(screen.getByText('Ana María Ruiz · @anaruiz')).toBeInTheDocument();

    searchPartiesForSelector.mockResolvedValueOnce({ items: [beatriz], nextCursor: null });
    fireEvent.click(screen.getByRole('button', { name: 'Reintentar' }));
    expect(await screen.findByRole('option', { name: /Beatriz Núñez/i })).toBeInTheDocument();
    expect(screen.getByText('Ana María Ruiz · @anaruiz')).toBeInTheDocument();
  });

  it('loads the next cursor page without replacing earlier results', async () => {
    searchPartiesForSelector
      .mockResolvedValueOnce({ items: [ana], nextCursor: 17 })
      .mockResolvedValueOnce({ items: [beatriz], nextCursor: null });
    renderHarness();

    fireEvent.change(screen.getByRole('combobox', { name: 'Contactos de campaña' }), {
      target: { value: 'artistas' },
    });
    expect(await screen.findByRole('option', { name: /Ana María Ruiz/i }, { timeout: 1500 })).toBeInTheDocument();

    fireEvent.mouseDown(screen.getByRole('button', { name: 'Ver más resultados' }));
    fireEvent.click(screen.getByRole('button', { name: 'Ver más resultados' }));

    expect(await screen.findByRole('option', { name: /Beatriz Núñez/i })).toBeInTheDocument();
    expect(screen.getByRole('option', { name: /Ana María Ruiz/i })).toBeInTheDocument();
    expect(searchPartiesForSelector).toHaveBeenLastCalledWith(expect.objectContaining({
      query: 'artistas',
      cursor: 17,
    }));
  });

  it('cancels an obsolete request and never renders its late response', async () => {
    let resolveAna: ((page: PartySelectorPage) => void) | undefined;
    let resolveBeatriz: ((page: PartySelectorPage) => void) | undefined;
    searchPartiesForSelector.mockImplementation(({ query }) => new Promise((resolve) => {
      if (query === 'Ana') resolveAna = resolve;
      if (query === 'Beatriz') resolveBeatriz = resolve;
    }));
    renderHarness();
    const input = screen.getByRole('combobox', { name: 'Contactos de campaña' });

    fireEvent.change(input, { target: { value: 'Ana' } });
    await waitFor(() => expect(searchPartiesForSelector).toHaveBeenCalledTimes(1), { timeout: 1500 });
    const firstSignal = searchPartiesForSelector.mock.calls[0]?.[0].signal;
    fireEvent.change(input, { target: { value: 'Beatriz' } });
    await waitFor(() => expect(searchPartiesForSelector).toHaveBeenCalledTimes(2), { timeout: 1500 });

    expect(firstSignal?.aborted).toBe(true);
    resolveAna?.({ items: [ana], nextCursor: null });
    resolveBeatriz?.({ items: [beatriz], nextCursor: null });
    expect(await screen.findByRole('option', { name: /Beatriz Núñez/i })).toBeInTheDocument();
    expect(screen.queryByRole('option', { name: /Ana María Ruiz/i })).not.toBeInTheDocument();
  });

  it('deduplicates identical searches within the same authorized cache scope', async () => {
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    render(
      <QueryClientProvider client={queryClient}>
        <Harness />
        <Harness />
      </QueryClientProvider>,
    );
    const inputs = screen.getAllByRole('combobox', { name: 'Contactos de campaña' });
    fireEvent.change(inputs[0]!, { target: { value: 'Ana' } });
    fireEvent.change(inputs[1]!, { target: { value: 'Ana' } });

    await waitFor(() => expect(screen.getAllByRole('option', { name: /Ana María Ruiz/i })).toHaveLength(2), {
      timeout: 1500,
    });
    expect(searchPartiesForSelector).toHaveBeenCalledTimes(1);
  });
});
