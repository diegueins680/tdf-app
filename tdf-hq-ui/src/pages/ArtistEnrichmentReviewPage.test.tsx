import '@testing-library/jest-dom';
import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { ArtistEnrichmentOverview } from '../api/artistEnrichment';

const overview = jest.fn<() => Promise<ArtistEnrichmentOverview>>();
const run = jest.fn();
jest.unstable_mockModule('../api/artistEnrichment', () => ({ ArtistEnrichment: { overview, run } }));
const { default: ArtistEnrichmentReviewPage } = await import('./ArtistEnrichmentReviewPage');

afterEach(() => { cleanup(); jest.clearAllMocks(); });

function showPage(networkStatus: 'link_validation_valid' | 'link_validation_broken', scopedIdentities = false) {
  overview.mockResolvedValue({
    aeoProfiles: [{ apeArtistId: 115, apeArtistName: 'Verde 70', apeReviewStatus: 'verified', apeMissingFields: ['achievements'], apeBrokenFields: ['heroImageUrl'] }],
    aeoSources: [
      { arsId: 1, arsArtistId: 115, arsSourceUrl: 'https://images.example.test/photo.jpg', arsSourceType: networkStatus, arsSupportedFields: 'heroImageUrl', arsRetrievedAt: '2026-10-26T10:00:00Z' },
      { arsId: 2, arsArtistId: 115, arsSourceUrl: 'https://images.example.test/photo.jpg', arsSourceType: 'link_validation_broken', arsSupportedFields: 'heroImageUrl', arsRetrievedAt: '2026-10-26T10:00:01Z', arsAttribution: '{"reason":"third_party_hotlink_requires_authorized_ingestion"}' },
    ],
    aeoInventory: scopedIdentities ? [{ airId: 1, airArtistId: 115, airSourceType: 'social_artist_profile', airSourceRecordId: '50', airOriginalName: 'Verde 70', airNormalizedName: 'verde 70', airDisposition: 'matched', airFirstSeenAt: '2026-10-26T10:00:00Z', airLastSeenAt: '2026-10-26T10:00:00Z' }] : [],
    aeoIdentityCandidates: scopedIdentities ? [
      { aicId: 1, aicArtistId: 116, aicInventoryReferenceId: 2, aicProvider: 'external_research', aicConfidence: 0.5, aicStatus: 'pending', aicEvidence: '{}', aicCreatedAt: '2026-10-26T10:00:00Z', aicUpdatedAt: '2026-10-26T10:00:00Z' },
      { aicId: 2, aicArtistId: null, aicInventoryReferenceId: 1, aicProvider: 'external_research', aicConfidence: 0.5, aicStatus: 'pending', aicEvidence: '{}', aicCreatedAt: '2026-10-26T10:00:00Z', aicUpdatedAt: '2026-10-26T10:00:00Z' },
      { aicId: 3, aicArtistId: 115, aicInventoryReferenceId: 3, aicProvider: 'external_research', aicConfidence: 0.5, aicStatus: 'pending', aicEvidence: '{}', aicCreatedAt: '2026-10-26T10:00:00Z', aicUpdatedAt: '2026-10-26T10:00:00Z' },
      { aicId: 4, aicArtistId: 116, aicInventoryReferenceId: 1, aicProvider: 'external_research', aicConfidence: 0.5, aicStatus: 'pending', aicEvidence: '{}', aicCreatedAt: '2026-10-26T10:00:00Z', aicUpdatedAt: '2026-10-26T10:00:00Z' },
    ] : [],
    aeoSuggestions: [], aeoChanges: [], aeoRuns: [], aeoMedia: [],
  });
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  render(<QueryClientProvider client={client}><MemoryRouter><ArtistEnrichmentReviewPage /></MemoryRouter></QueryClientProvider>);
  return client;
}

it('explains verification and separates an external-image review from broken-link filtering', async () => {
  const client = showPage('link_validation_valid');
  try {
    await screen.findByRole('heading', { name: 'Verde 70' });
    expect(screen.getByText('Cómo verificar un artista')).toBeVisible();
    expect(screen.getByText('Verificado')).toBeVisible();
    expect(screen.getByText('Imagen externa: revisar autorización de uso')).toBeVisible();
    expect(screen.queryByText('Link roto: heroImageUrl')).not.toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Filtrar links rotos' }));
    expect(screen.queryByRole('heading', { name: 'Verde 70' })).not.toBeInTheDocument();
    expect(run).not.toHaveBeenCalled();
  } finally { client.clear(); }
});

it('continues to show and filter genuine failures alongside an image-rights review', async () => {
  const client = showPage('link_validation_broken');
  try {
    await screen.findByRole('heading', { name: 'Verde 70' });
    expect(screen.getByText('Link roto: heroImageUrl')).toBeVisible();
    expect(screen.getByText('Imagen externa: revisar autorización de uso')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Filtrar links rotos' }));
    expect(screen.getByRole('heading', { name: 'Verde 70' })).toBeVisible();
  } finally { client.clear(); }
});

it('scopes identity counts to the selected artist without hiding competing matches for its references', async () => {
  const client = showPage('link_validation_valid', true);
  try {
    await screen.findByRole('tab', { name: 'Identidades (4)' });
    fireEvent.mouseDown(screen.getByLabelText('Artista'));
    fireEvent.click(screen.getByRole('option', { name: 'Verde 70' }));
    await waitFor(() => expect(screen.getByRole('tab', { name: 'Identidades (3)' })).toBeVisible());
    expect(run).not.toHaveBeenCalled();
  } finally { client.clear(); }
});
