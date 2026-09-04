import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { useState } from 'react';
import type { PartySelectorOption, PartySelectorPage } from '../../api/partySelector';

const searchPartiesForSelector = jest.fn<
  (params: { query: string; signal?: AbortSignal }) => Promise<PartySelectorPage>
>();

jest.unstable_mockModule('../../api/partySelector', () => ({
  searchPartiesForSelector,
}));

const { PartyMultiSelector } = await import('./PartySelector');

const ana: PartySelectorOption = {
  partyId: 17,
  partyType: 'person',
  displayName: 'Ana María Ruiz',
  username: 'anaruiz',
  avatarUrl: null,
  secondaryLabel: 'Artista',
  accountStatus: 'active',
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

describe('PartyMultiSelector', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    searchPartiesForSelector.mockResolvedValue({ items: [ana], nextCursor: null });
  });

  it('does not treat typed text as a selection and renders an identity chip after choosing', async () => {
    render(<Harness />);
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

  it('keeps existing selections when a later search fails', async () => {
    render(<Harness />);
    const input = screen.getByRole('combobox', { name: 'Contactos de campaña' });
    fireEvent.change(input, { target: { value: 'Ana' } });
    fireEvent.click(await screen.findByRole('option', { name: /Ana María Ruiz/i }, { timeout: 1500 }));

    searchPartiesForSelector.mockRejectedValueOnce(new Error('sin conexión'));
    fireEvent.change(input, { target: { value: 'Beatriz' } });

    expect(await screen.findByRole('alert', {}, { timeout: 1500 })).toHaveTextContent(
      'No se modificaron las selecciones',
    );
    expect(screen.getByText('Ana María Ruiz · @anaruiz')).toBeInTheDocument();
  });
});
