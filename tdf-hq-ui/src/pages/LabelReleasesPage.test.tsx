import { jest } from '@jest/globals';
import { render, screen, cleanup, fireEvent, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { ReactNode } from 'react';
import type { ArtistProfileDTO, ArtistReleaseDTO } from '../api/types';
import type { MusicPublicReleaseSummary } from '../api/musicReleases';
const listPublic = jest.fn<(...args: unknown[]) => Promise<MusicPublicReleaseSummary[]>>(async () => [{ id: 'canonical', artistPartyId: 17, title: 'Published title', slug: 'published',
  displayArtist: 'Synthetic', kind: 'single', publishedAt: '2024-01-01', coverAssetId: null } as MusicPublicReleaseSummary]);
const listArtists = jest.fn(async () => [{ apArtistId: 17, apDisplayName: 'Synthetic' }] as ArtistProfileDTO[]);
const getReleases = jest.fn<(artistId: number) => Promise<ArtistReleaseDTO[]>>(async () => [{ arArtistId: 17, arReleaseId: 1, arTitle: 'Legacy title', arReleaseDate: '2025-01-01' }] as ArtistReleaseDTO[]);
jest.unstable_mockModule('../api/admin', () => ({ Admin: {
  listArtistProfiles: listArtists,
} }));
jest.unstable_mockModule('../api/fans', () => ({ Fans: {
  getReleases,
} }));
jest.unstable_mockModule('../api/musicReleases', () => ({ musicReleases: { listPublic } }));
jest.unstable_mockModule('../api/records', () => ({ Records: { getFeed: jest.fn(async () => ({ releases: [] })) } }));
jest.unstable_mockModule('../components/SessionGate', () => ({ SessionGate: ({ children }: { children: ReactNode }) => children }));
jest.unstable_mockModule('../components/StreamingPlayer', () => ({ default: () => null }));
const { default: LabelReleasesPage } = await import('./LabelReleasesPage');
afterEach(() => { cleanup(); jest.restoreAllMocks(); });
it('shows published and legacy releases through the same label catalog without converting legacy identity', async () => {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  render(<QueryClientProvider client={client}><MemoryRouter><LabelReleasesPage /></MemoryRouter></QueryClientProvider>);
  expect((await screen.findByRole('link', { name: /Published title/ })).getAttribute('href')).toBe('/musica/published');
  const legacyTitle = await screen.findByText('Legacy title');
  expect(screen.getByText('Artistas: 1')).toBeTruthy();
  const publishedLink = screen.getByRole('link', { name: /Published title/ });
  expect(legacyTitle.compareDocumentPosition(publishedLink) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
  fireEvent.mouseDown(screen.getByRole('combobox', { name: 'Ordenar' }));
  fireEvent.click(await screen.findByRole('option', { name: 'Más antiguos' }));
  await waitFor(() => expect(screen.getByRole('link', { name: /Published title/ })
    .compareDocumentPosition(screen.getByText('Legacy title')) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy());
  client.clear();
});


it('includes a canonical-only recent publication in all summary metrics', async () => {
  getReleases.mockResolvedValueOnce([]);
  listPublic.mockResolvedValueOnce([{ id: 'only', artistPartyId: 17, title: 'Only publication', slug: 'only', kind: 'single',
    displayArtist: 'Synthetic', publishedAt: new Date().toISOString(), releaseAtUtc: null, coverAssetId: null } as MusicPublicReleaseSummary]);
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  render(<QueryClientProvider client={client}><MemoryRouter><LabelReleasesPage /></MemoryRouter></QueryClientProvider>);
  await screen.findByRole('link', { name: /Only publication/ });
  expect(screen.getByText('Releases: 1')).toBeTruthy();
  expect(screen.getByText('Artistas: 1')).toBeTruthy();
  expect(screen.getByText('Últimos 30 días: 1')).toBeTruthy();
  client.clear();
});

it('counts distinct Party IDs even when their display names match across sources', async () => {
  listArtists.mockResolvedValueOnce([{ apArtistId: 17, apDisplayName: 'Same name' }, { apArtistId: 18, apDisplayName: 'Same name' }] as ArtistProfileDTO[]);
  getReleases.mockImplementationOnce(async () => [])
    .mockImplementationOnce(async () => [{ arArtistId: 18, arReleaseId: 2, arTitle: 'Other identity', arReleaseDate: null }] as ArtistReleaseDTO[]);
  listPublic.mockResolvedValueOnce([{ id: 'canonical', artistPartyId: 17, title: 'Recent release', slug: 'recent', kind: 'single',
    displayArtist: 'Same name', publishedAt: '2024-01-01', releaseAtUtc: new Date().toISOString(), coverAssetId: null } as MusicPublicReleaseSummary])
    .mockResolvedValueOnce([]);
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  render(<QueryClientProvider client={client}><MemoryRouter><LabelReleasesPage /></MemoryRouter></QueryClientProvider>);
  await screen.findByText('Other identity');
  expect(screen.getByText('Releases: 2')).toBeTruthy();
  expect(screen.getByText('Artistas: 2')).toBeTruthy();
  expect(screen.getByText('Últimos 30 días: 1')).toBeTruthy();
  client.clear();
});

it.each(['canonical', 'legacy', 'mixed'] as const)(
  'preserves %s catalog totals across date boundaries, pagination and filters', async (source) => {
    const now = Date.parse('2026-10-30T12:00:00Z');
    jest.spyOn(Date, 'now').mockReturnValue(now);
    const threshold = now - 30 * 24 * 60 * 60 * 1000;
    const dates = [now - 1, threshold, threshold - 1, now + 86400000, 'invalid', null,
      ...Array<number>(7).fill(threshold - 86400000)];
    const canonical: MusicPublicReleaseSummary[] = [];
    const legacy: ArtistReleaseDTO[] = [];
    dates.forEach((date, index) => {
      const artistId = index === 12 ? 18 : 17;
      const value = typeof date === 'number' ? new Date(date).toISOString() : date;
      if (source === 'canonical' || (source === 'mixed' && index % 2 === 0)) {
        canonical.push({ id: `canonical-${index}`, artistPartyId: artistId, title: `Publication ${index}`,
          slug: `publication-${index}`, kind: 'single', displayArtist: 'Same name', coverAssetId: null,
          // Exercise the publishedAt fallback as well as releaseAtUtc taking precedence.
          publishedAt: index === 0 ? value : '2020-01-01T00:00:00Z',
          releaseAtUtc: index === 0 ? null : value ?? '' } as MusicPublicReleaseSummary);
      } else {
        legacy.push({ arArtistId: artistId, arReleaseId: index + 1, arTitle: `Publication ${index}`,
          arReleaseDate: value } as ArtistReleaseDTO);
      }
    });
    listArtists.mockResolvedValueOnce([17, 18].map((apArtistId) => ({ apArtistId, apDisplayName: 'Same name' })) as ArtistProfileDTO[]);
    listPublic.mockImplementation(async (_query, artistId) => canonical.filter((release) => release.artistPartyId === artistId));
    getReleases.mockImplementation(async (artistId) => legacy.filter((release) => release.arArtistId === artistId));
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      render(<QueryClientProvider client={client}><MemoryRouter><LabelReleasesPage /></MemoryRouter></QueryClientProvider>);
      await screen.findByText('Releases: 13');
      const expectTotals = () => {
        expect(screen.getByText('Releases: 13')).toBeTruthy();
        expect(screen.getByText('Artistas: 2')).toBeTruthy();
        // The existing rule includes future releases, the exact threshold, and excludes invalid dates.
        expect(screen.getByText('Últimos 30 días: 3')).toBeTruthy();
      };
      expectTotals();
      const nextPage = await screen.findByRole('button', { name: 'Ir a la página siguiente' });
      expect(screen.queryByText('Publication 4')).toBeNull();
      fireEvent.click(nextPage);
      await screen.findByText('Publication 4');
      expectTotals();
      fireEvent.change(screen.getAllByRole('textbox', { name: 'Buscar' })[0]!, { target: { value: 'Publication 0' } });
      await waitFor(() => expect(screen.queryByText('Publication 4')).toBeNull());
      await screen.findByText('Publication 0');
      expectTotals();
      expect(screen.queryByRole('button', { name: 'Ir a la página siguiente' })).toBeNull();
    } finally {
      client.clear();
      // Keep the original fixture defaults independent of this table-driven matrix.
      listPublic.mockReset().mockResolvedValue([]);
      getReleases.mockReset().mockResolvedValue([]);
    }
  },
);
