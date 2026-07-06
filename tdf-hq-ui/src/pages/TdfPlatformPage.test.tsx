import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import type { ArtistProfileDTO } from '../api/types';

const listPublicArtistsMock = jest.fn<() => Promise<ArtistProfileDTO[]>>();

const TDF_PLATFORM_TEST_TRANSLATIONS: Readonly<Record<string, string>> = {
  'tdfPlatform.cta.createAccount': 'Crear cuenta',
  'tdfPlatform.cta.fanProfile': 'Perfil fan',
  'tdfPlatform.cta.artistProfile': 'Perfil artista',
  'tdfPlatform.cta.createFanProfile': 'Crear perfil fan',
  'tdfPlatform.cta.createArtistProfile': 'Crear perfil de artista',
  'tdfPlatform.cta.viewArtistProfile': 'Ver perfil',
  'tdfPlatform.cta.explore': 'Explorar',
  'tdfPlatform.cta.reserveExperience': 'Reservar experiencia',
  'tdfPlatform.cta.viewLocation': 'Ver ubicación',
  'tdfPlatform.cta.viewReleases': 'Ver lanzamientos',
  'tdfPlatform.empty.artists':
    'El carrusel se llenará automáticamente cuando existan artistas publicados en la plataforma.',
  'tdfPlatform.empty.services': 'Pronto verás nuevas rutas TDF en este espacio.',
  'tdfPlatform.empty.fanBenefits': 'Pronto agregaremos beneficios para fans.',
  'tdfPlatform.empty.artistBenefits': 'Pronto agregaremos beneficios para artistas.',
  'tdfPlatform.sections.startEyebrow': 'Empieza por tu cuenta',
} as const satisfies Record<string, string>;

type ArtistRankingFixtureContract = Readonly<{
  minorFollowerCount: number;
  majorFollowerCount: number;
  majorFollowerLabel: string;
}>;

const DECIMAL_BASE = 10;
const MINOR_ARTIST_FOLLOWER_COUNT = 7;
const MAJOR_ARTIST_FOLLOWER_COUNT = DECIMAL_BASE * (DECIMAL_BASE + 2);
const ARTIST_RANKING_FIXTURE = {
  minorFollowerCount: MINOR_ARTIST_FOLLOWER_COUNT,
  majorFollowerCount: MAJOR_ARTIST_FOLLOWER_COUNT,
  majorFollowerLabel: `${MAJOR_ARTIST_FOLLOWER_COUNT.toLocaleString('es-EC')} seguidores`,
} as const satisfies ArtistRankingFixtureContract;

jest.unstable_mockModule('../api/fans', () => ({
  Fans: {
    listPublicArtists: listPublicArtistsMock,
  },
}));

jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({
    t: (key: string): string => TDF_PLATFORM_TEST_TRANSLATIONS[key] ?? key,
  }),
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
    apFollowerCount: ARTIST_RANKING_FIXTURE.minorFollowerCount,
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
    apFollowerCount: ARTIST_RANKING_FIXTURE.majorFollowerCount,
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
    expect(ARTIST_RANKING_FIXTURE.majorFollowerCount).toBeGreaterThan(
      ARTIST_RANKING_FIXTURE.minorFollowerCount,
    );
    const carouselText = screen.getByLabelText('Carrusel de artistas destacados').textContent ?? '';
    expect(carouselText.indexOf('Artista Mayor')).toBeLessThan(carouselText.indexOf('Artista Menor'));
    expect(screen.getByText(ARTIST_RANKING_FIXTURE.majorFollowerLabel)).not.toBeNull();
    expect(screen.getByRole('img', { name: 'Imagen de Artista Mayor' }).getAttribute('src')).toBe(
      'https://cdn.example.test/major.jpg',
    );
  });
});
