import { afterEach, beforeEach, describe, expect, it, jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import type { ArtistProfileDTO, ArtistRecommendationDTO, FanFollowDTO } from '../../api/types';

const listRecommendations = jest.fn<(limit?: number) => Promise<ArtistRecommendationDTO[]>>();
jest.unstable_mockModule('../../api/fans', () => ({ Fans: { listRecommendations } }));
const { RecommendedArtists, RECOMMENDED_ARTISTS_LIMIT } = await import('./RecommendedArtists');

const artist = (apArtistId: number, apDisplayName: string): ArtistProfileDTO => (
  { apArtistId, apDisplayName, apGenreIds: [], apFollowerCount: 0, apHasUserAccount: true }
);
const follow = (ffArtistId: number, ffArtistName: string): FanFollowDTO => (
  { ffArtistId, ffArtistName, ffStartedAt: '2026-11-01' }
);
const catalog = [artist(1, 'Seguida'), artist(2, 'Parecida'), artist(3, 'Popular')];

let client: QueryClient;
beforeEach(() => {
  listRecommendations.mockReset();
  client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
});
afterEach(() => { cleanup(); client.clear(); });

type Props = Parameters<typeof RecommendedArtists>[0];
const element = (props: Partial<Props>) => (
  <MemoryRouter>
    <QueryClientProvider client={client}>
      <RecommendedArtists viewerId={7} personalized artists={catalog} follows={[follow(1, 'Seguida')]} starterArtists={[]} {...props} />
    </QueryClientProvider>
  </MemoryRouter>
);

describe('RecommendedArtists', () => {
  it('shows the artists the server matched to the fan, best first', async () => {
    listRecommendations.mockResolvedValue([
      { arecArtistId: 2, arecReason: 'similar_fans', arecScore: 0.5, arecSeedArtistIds: [1] },
      { arecArtistId: 3, arecReason: 'popular', arecScore: 0, arecSeedArtistIds: [] },
    ]);
    render(element({}));

    await screen.findByText('Parecida');
    expect(listRecommendations).toHaveBeenCalledWith(RECOMMENDED_ARTISTS_LIMIT);
    const card = screen.getByTestId('recommended-artists');
    expect(Array.from(card.querySelectorAll('a')).map((link) => link.textContent)).toEqual(['Parecida', 'Popular']);
    expect(card.textContent).toContain('Artistas que siguen otros fans con gustos parecidos a los tuyos.');
    // The reason describes the link; the artist name stays its accessible name.
    expect(screen.getByRole('link', { name: 'Parecida' })).toBeTruthy();
    fireEvent.mouseOver(screen.getByText('Parecida'));
    expect((await screen.findByRole('tooltip')).textContent).toBe('Porque sigues a Seguida');
  });

  it('asks again when the follows change', async () => {
    listRecommendations.mockResolvedValue([]);
    const view = render(element({}));
    await waitFor(() => expect(listRecommendations).toHaveBeenCalledTimes(1));

    view.rerender(element({ follows: [follow(1, 'Seguida'), follow(3, 'Popular')] }));
    await waitFor(() => expect(listRecommendations).toHaveBeenCalledTimes(2));
  });

  it('shows the starter picks without asking the server when there is nothing to personalize', () => {
    render(element({ personalized: false, follows: [], starterArtists: [catalog[2]!] }));

    expect(screen.getByText('Popular')).toBeTruthy();
    expect(screen.getByTestId('recommended-artists').textContent).not.toContain('gustos parecidos');
    expect(listRecommendations).not.toHaveBeenCalled();
  });

  it('stays hidden when the server has nothing or cannot answer', async () => {
    listRecommendations.mockRejectedValue(new Error('Synthetic outage'));
    render(element({}));

    await waitFor(() => expect(listRecommendations).toHaveBeenCalledTimes(1));
    expect(screen.queryByTestId('recommended-artists')).toBeNull();
  });
});
