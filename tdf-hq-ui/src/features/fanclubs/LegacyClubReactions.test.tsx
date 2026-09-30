import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { FanClubFeedItemDTO, ReactionSummaryDTO } from '../../api/types';

const reactToPost = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const reactToMemory = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('../../api/fans', () => ({ Fans: { reactToPost, reactToMemory } }));
jest.unstable_mockModule('../../components/ReactionBar', () => ({ default: ({ reactions, onReact, loading }: {
  reactions: ReactionSummaryDTO; onReact: (id: string) => void; loading: boolean;
}) => <button disabled={loading} onClick={() => onReact('love')}>Existing reactions: {reactions.rsItems[0]?.rsiCount}</button> }));
const { LegacyClubReactions } = await import('./LegacyClubReactions');
let client: QueryClient;
beforeEach(() => { jest.clearAllMocks(); reactToPost.mockResolvedValue({}); reactToMemory.mockResolvedValue({}); });
afterEach(() => { cleanup(); client?.clear(); });
function view(kind: string) {
  client = new QueryClient({ defaultOptions: { mutations: { retry: false } } });
  const invalidate = jest.spyOn(client, 'invalidateQueries');
  const item = { fcfId: 17, fcfKind: kind, fcfReactions: { rsMyReactionTypeId: null, rsItems: [{ rsiCount: 5 }] } } as FanClubFeedItemDTO;
  render(<QueryClientProvider client={client}><LegacyClubReactions artistId={9} item={item} /></QueryClientProvider>);
  return invalidate;
}
test.each(['post', 'memory'])('keeps existing %s counts and routes writes through the authorized legacy adapter', async (kind) => {
  const invalidate = view(kind);
  fireEvent.click(screen.getByRole('button', { name: 'Existing reactions: 5' }));
  const selected = kind === 'post' ? reactToPost : reactToMemory;
  await waitFor(() => expect(selected).toHaveBeenCalledWith(9, 17, { crrReactionTypeId: 'love' }));
  expect(kind === 'post' ? reactToMemory : reactToPost).not.toHaveBeenCalled();
  await waitFor(() => expect(invalidate).toHaveBeenCalledWith({ queryKey: ['interactions'] }));
  expect(invalidate).toHaveBeenCalledWith({ queryKey: ['fan-club-feed', 9] });
});
test('announces a failed legacy write, keeps counts and permits retry', async () => {
  reactToPost.mockRejectedValue(new Error('offline')); view('post');
  fireEvent.click(screen.getByRole('button', { name: 'Existing reactions: 5' }));
  await screen.findByRole('alert');
  await waitFor(() => expect(screen.getByRole<HTMLButtonElement>('button').disabled).toBe(false));
  fireEvent.click(screen.getByRole('button'));
  await waitFor(() => expect(reactToPost).toHaveBeenCalledTimes(2));
});
