import { jest } from '@jest/globals';
import { fireEvent, render, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { ExperienceReviewPage } from '../../api/reviews';

const firstPage: ExperienceReviewPage = {
  summary: {
    targetKind: 'event',
    targetId: '42',
    average: 4.5,
    count: 2,
  },
  items: [{
    id: '11111111-1111-4111-8111-111111111111',
    targetKind: 'event',
    targetId: '42',
    rating: 5,
    body: 'Primera reseña verificada',
    status: 'published',
    createdAt: '2030-01-01T00:00:00Z',
    verified: true,
    sourceKind: 'event_ticket_order',
    author: { name: 'Primera autora', avatarUrl: null },
  }],
  nextCursor: '22222222-2222-4222-8222-222222222222',
};

const secondPage: ExperienceReviewPage = {
  summary: firstPage.summary,
  items: [{
    ...firstPage.items[0],
    id: '33333333-3333-4333-8333-333333333333',
    body: 'Segunda reseña verificada',
    author: { name: 'Segunda autora', avatarUrl: null },
  }],
  nextCursor: null,
};

const listMock = jest.fn<(
  targetKind: 'event',
  targetId: string,
  cursor?: string,
) => Promise<ExperienceReviewPage>>();

jest.unstable_mockModule('../../api/reviews', () => ({
  Reviews: {
    list: listMock,
    eligibility: jest.fn(),
    create: jest.fn(),
  },
}));

jest.unstable_mockModule('../../api/directory', () => ({
  Directory: { report: jest.fn() },
}));

jest.unstable_mockModule('../../session/SessionContext', () => ({
  useSession: () => ({ session: null }),
}));

const { default: ExperienceReviews } = await import('./ExperienceReviews');

describe('ExperienceReviews pagination', () => {
  beforeEach(() => {
    listMock.mockReset()
      .mockResolvedValueOnce(firstPage)
      .mockResolvedValueOnce(secondPage);
  });

  it('loads the next cursor and keeps reviews from earlier pages visible', async () => {
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    const view = render(
      <QueryClientProvider client={queryClient}>
        <ExperienceReviews targetKind="event" targetId="42" />
      </QueryClientProvider>,
    );

    try {
      expect(await screen.findByText('Primera reseña verificada')).toBeTruthy();
      fireEvent.click(screen.getByRole('button', { name: 'Ver más reseñas' }));

      expect(await screen.findByText('Segunda reseña verificada')).toBeTruthy();
      expect(screen.getByText('Primera reseña verificada')).toBeTruthy();
      expect(listMock).toHaveBeenNthCalledWith(1, 'event', '42', undefined);
      expect(listMock).toHaveBeenNthCalledWith(
        2,
        'event',
        '42',
        '22222222-2222-4222-8222-222222222222',
      );
      expect(screen.queryByRole('button', { name: 'Ver más reseñas' })).toBeNull();
    } finally {
      view.unmount();
      queryClient.clear();
    }
  });
});
