import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import type { ArtistProfileDTO } from '../api/types';

const listPublicArtistsMock = jest.fn<() => Promise<ArtistProfileDTO[]>>();

jest.unstable_mockModule('../api/fans', () => ({
  Fans: {
    listPublicArtists: listPublicArtistsMock,
  },
}));

const { default: TdfPlatformPage } = await import('./TdfPlatformPage');

const artists: ArtistProfileDTO[] = [
  {
    apArtistId: 2,
    apDisplayName: 'Artista Menor',
    apSlug: 'artista-menor',
    apBio: 'Proyecto emergente.',
    apCity: 'Quito',
    apGenres: 'Indie',
    apHeroImageUrl: 'https://cdn.example.test/minor.jpg',
    apSpotifyArtistId: null,
    apSpotifyUrl: null,
    apYoutubeChannelId: null,
    apYoutubeUrl: null,
    apWebsiteUrl: null,
    apFeaturedVideoUrl: null,
    apHighlights: null,
    apFollowerCount: 7,
    apHasUserAccount: true,
  },
  {
    apArtistId: 1,
    apDisplayName: 'Artista Mayor',
    apSlug: 'artista-mayor',
    apBio: 'Comunidad activa.',
    apCity: 'Quito',
    apGenres: 'Pop',
    apHeroImageUrl: 'https://cdn.example.test/major.jpg',
    apSpotifyArtistId: null,
    apSpotifyUrl: null,
    apYoutubeChannelId: null,
    apYoutubeUrl: null,
    apWebsiteUrl: null,
    apFeaturedVideoUrl: null,
    apHighlights: null,
    apFollowerCount: 120,
    apHasUserAccount: true,
  },
];

function renderPage() {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
    },
  });

  render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter future={{ v7_relativeSplatPath: true, v7_startTransition: true }}>
        <TdfPlatformPage />
      </MemoryRouter>
    </QueryClientProvider>,
  );
}

describe('TdfPlatformPage', () => {
  beforeEach(() => {
    listPublicArtistsMock.mockReset();
    listPublicArtistsMock.mockResolvedValue(artists);
  });

  it('explains TDF and sends users to fan or artist signup routes', async () => {
    renderPage();

    expect(screen.getByRole('heading', { name: 'TDF Records' })).not.toBeNull();
    expect(screen.getAllByRole('link', { name: 'Crear cuenta' })[0]?.getAttribute('href')).toBe(
      '/login?signup=1&redirect=/fans',
    );
    expect(screen.getByRole('link', { name: 'Perfil fan' }).getAttribute('href')).toBe(
      '/login?signup=1&roles=Fan&redirect=/fans',
    );
    expect(screen.getByRole('link', { name: 'Perfil artista' }).getAttribute('href')).toBe(
      '/login?signup=1&intent=artist&redirect=/mi-artista',
    );

    await screen.findByText('Artista Mayor');
    const carouselText = screen.getByLabelText('Carrusel de artistas destacados').textContent ?? '';
    expect(carouselText.indexOf('Artista Mayor')).toBeLessThan(carouselText.indexOf('Artista Menor'));
    expect(screen.getByText('120 seguidores')).not.toBeNull();
    expect(screen.getByRole('img', { name: 'Imagen de Artista Mayor' }).getAttribute('src')).toBe(
      'https://cdn.example.test/major.jpg',
    );
  });
});
