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
afterEach(cleanup);
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
