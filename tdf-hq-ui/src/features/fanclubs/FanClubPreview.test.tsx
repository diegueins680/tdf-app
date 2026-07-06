import { render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import type { FanClubDTO } from '../../api/types';
import { FanClubPreview } from './FanClubPreview';

const club = (overrides: Partial<FanClubDTO> = {}): FanClubDTO => ({
  fcId: 10,
  fcArtistId: 42,
  fcName: 'Club Luna',
  fcDescription: 'Sesiones, memorias y anuncios del proyecto.',
  fcOfficers: [],
  fcFollowerCount: 128,
  fcArtistImageUrl: 'https://cdn.example.com/luna.jpg',
  ...overrides,
});

describe('FanClubPreview', () => {
  it('renders fan club cards linked to their club page', () => {
    render(
      <MemoryRouter future={{ v7_relativeSplatPath: true, v7_startTransition: true }}>
        <FanClubPreview clubs={[club()]} loading={false} />
      </MemoryRouter>,
    );

    expect(screen.getByText('Tus clubes')).not.toBeNull();
    expect(screen.getByText('Club Luna')).not.toBeNull();
    expect(screen.getByText('128 seguidores')).not.toBeNull();
    expect(screen.getByRole('link', { name: /Club Luna/i }).getAttribute('href')).toBe('/fans/clubs/42');
  });
});
