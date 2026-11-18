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
  it('links the club details to the club and the photo to the artist profile', () => {
    render(
      <MemoryRouter>
        <FanClubPreview clubs={[club()]} loading={false} />
      </MemoryRouter>,
    );

    expect(screen.getByText('Tus clubes')).not.toBeNull();
    expect(screen.getByText('Club Luna')).not.toBeNull();
    expect(screen.getByText('128 seguidores')).not.toBeNull();
    expect(screen.getByRole('link', { name: /Club Luna 128 seguidores/i }).getAttribute('href')).toBe('/fans/clubs/42');
    expect(screen.getByRole('link', { name: 'Club Luna' }).getAttribute('href')).toBe('/a/42');
  });
});
