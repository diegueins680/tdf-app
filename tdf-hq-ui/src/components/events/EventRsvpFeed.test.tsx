import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { SocialRsvpFeedPageDTO } from '../../api/socialEvents';

const listFeed = jest.fn<() => Promise<SocialRsvpFeedPageDTO>>();
const listDirectoryFeed = jest.fn<() => Promise<SocialRsvpFeedPageDTO>>();
jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: { listRsvpFeed: listFeed, listDirectoryProfileRsvpFeed: listDirectoryFeed },
}));
jest.unstable_mockModule('../../utils/env', () => ({ env: { read: () => undefined } }));
jest.unstable_mockModule('../../analytics/useAnalytics', () => ({ useAnalytics: () => null }));

const { ApiError } = await import('../../api/client');
const { eventRsvpQueryKeys } = await import('./EventRsvpControls');
const { default: EventRsvpFeed } = await import('./EventRsvpFeed');
const clients: QueryClient[] = [];
const emptyFeed: SocialRsvpFeedPageDTO = { feedItems: [], feedNextCursor: null };

function renderFeed(directorySlug?: string, disableRetries = false) {
  const client = new QueryClient({ defaultOptions: { queries: {
    retryDelay: 0,
    // Match the application's policy; the feed must inherit it.
    retry: disableRetries ? false : (failureCount, error) =>
      !(error instanceof ApiError && error.status >= 400 && error.status < 500) && failureCount < 3,
  } } });
  clients.push(client);
  render(
    <QueryClientProvider client={client}>
      <MemoryRouter>
        <EventRsvpFeed partyId="248" directorySlug={directorySlug} isSelf={false} locale={directorySlug ? 'en' : 'es'} />
      </MemoryRouter>
    </QueryClientProvider>,
  );
  return client;
}

beforeEach(() => {
  listFeed.mockReset().mockResolvedValue(emptyFeed);
  listDirectoryFeed.mockReset().mockResolvedValue(emptyFeed);
});

afterEach(() => {
  cleanup();
  clients.splice(0).forEach((client) => client.clear());
});

it('treats a profile visibility 404 as unavailable without retrying', async () => {
  listFeed.mockRejectedValue(new ApiError('Profile not found', 404));
  renderFeed();
  await screen.findByText('La actividad de RSVP no está disponible para este perfil.');
  expect(screen.queryByRole('button', { name: 'Reintentar' })).toBeNull();
  expect(listFeed).toHaveBeenCalledTimes(1);
  expect(listFeed).toHaveBeenCalledWith('248', undefined, 20);
});

it('handles unavailable directory activity in English through the slug endpoint', async () => {
  listDirectoryFeed.mockRejectedValue(new ApiError('Profile not found', 404));
  renderFeed('public-person');
  await screen.findByText('RSVP activity is not available for this profile.');
  expect(listDirectoryFeed).toHaveBeenCalledTimes(1);
  expect(listDirectoryFeed).toHaveBeenCalledWith('public-person', undefined, 20);
  expect(listFeed).not.toHaveBeenCalled();
});

it.each([
  new ApiError('Internal server error', 500),
  new Error('Network unavailable'),
])('preserves failure feedback and recovery for $message', async (error) => {
  listFeed.mockRejectedValue(error);
  renderFeed();
  await screen.findByText('No pudimos cargar la actividad de RSVP.');
  expect(listFeed).toHaveBeenCalledTimes(4);
  listFeed.mockResolvedValue(emptyFeed);
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar' }));
  await screen.findByText('Todavía no hay actividad de RSVP visible.');
  expect(listFeed).toHaveBeenCalledTimes(5);
});

it.each([400, 401, 403, 429])('does not automatically retry HTTP %i', async (status) => {
  listFeed.mockRejectedValue(new ApiError('Request rejected', status));
  renderFeed();
  await screen.findByText('No pudimos cargar la actividad de RSVP.');
  expect(listFeed).toHaveBeenCalledTimes(1);
  listFeed.mockResolvedValue(emptyFeed);
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar' }));
  await screen.findByText('Todavía no hay actividad de RSVP visible.');
  expect(listFeed).toHaveBeenCalledTimes(2);
});

it('honors a query client configured without automatic retries', async () => {
  listFeed.mockRejectedValue(new ApiError('Internal server error', 500));
  renderFeed(undefined, true);
  await screen.findByText('No pudimos cargar la actividad de RSVP.');
  expect(listFeed).toHaveBeenCalledTimes(1);
});

it('keeps a successful empty feed distinct from an unavailable feed', async () => {
  renderFeed();
  await screen.findByText('Todavía no hay actividad de RSVP visible.');
  expect(screen.queryByText('La actividad de RSVP no está disponible para este perfil.')).toBeNull();
});

it('hides cached activity when a refetch loses profile visibility', async () => {
  listFeed.mockResolvedValue({
    feedItems: [{
      feedItemType: 'event_rsvp', feedEventId: '42', feedStatus: 'accepted',
      feedShowOnProfile: true, feedEventTitle: 'Public concert',
      feedEventStart: '2030-01-01T20:00:00Z', feedEventTimezone: 'UTC',
      feedWorkflowStateCode: 'announced', feedActionAt: '2026-10-01T12:00:00Z',
      feedCanonicalUrl: '/eventos/42', feedCanEdit: false, feedCanShare: true,
    }], feedNextCursor: null,
  });
  const client = renderFeed();
  await screen.findByText('Public concert');
  listFeed.mockRejectedValue(new ApiError('Profile not found', 404));
  void client.invalidateQueries({ queryKey: eventRsvpQueryKeys.feed('248') });
  await screen.findByText('La actividad de RSVP no está disponible para este perfil.');
  await waitFor(() => expect(screen.queryByText('Public concert')).toBeNull());
  expect(listFeed).toHaveBeenCalledTimes(2);
});
