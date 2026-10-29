import { jest } from '@jest/globals';
import { render, screen, cleanup, fireEvent, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { ReactNode } from 'react';
const listPublic = jest.fn(async () => [{ id: 'canonical', artistPartyId: 17, title: 'Published title', slug: 'published',
  displayArtist: 'Synthetic', kind: 'single', publishedAt: '2024-01-01', coverAssetId: null }]);
jest.unstable_mockModule('../api/admin', () => ({ Admin: {
  listArtistProfiles: jest.fn(async () => [{ apArtistId: 17, apDisplayName: 'Synthetic' }]),
} }));
jest.unstable_mockModule('../api/fans', () => ({ Fans: {
  getReleases: jest.fn(async () => [{ arArtistId: 17, arReleaseId: 1, arTitle: 'Legacy title', arReleaseDate: '2025-01-01' }]),
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
  const publishedLink = screen.getByRole('link', { name: /Published title/ });
  expect(legacyTitle.compareDocumentPosition(publishedLink) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
  fireEvent.mouseDown(screen.getByRole('combobox', { name: 'Ordenar' }));
  fireEvent.click(await screen.findByRole('option', { name: 'Más antiguos' }));
  await waitFor(() => expect(screen.getByRole('link', { name: /Published title/ })
    .compareDocumentPosition(screen.getByText('Legacy title')) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy());
  client.clear();
});
