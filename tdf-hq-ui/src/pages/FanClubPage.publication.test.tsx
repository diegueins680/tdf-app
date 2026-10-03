import { jest } from '@jest/globals';
import type { ReactNode } from 'react';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes } from 'react-router-dom';

const club = { fcName: 'Test club', fcOfficers: [], fcFollowerCount: 3 };
const posts = Array.from({ length: 25 }, (_, index) => ({ fcpId: index + 1, fcpTitle: `Post ${index + 1}`, fcpContent: 'Source body', fcpAuthorName: 'Author', fcpMediaUrls: [], fcpCreatedAt: '2026-09-29T00:00:00Z', fcpIsHidden: false }));
const memories = Array.from({ length: 25 }, (_, index) => ({ fcmId: index + 1, fcmTitle: `Memory ${index + 1}`, fcmMemberName: 'Author', fcmMediaUrls: [], fcmCreatedAt: '2026-09-29T00:00:00Z', fcmIsHidden: false, fcmIsDeleted: false }));
jest.unstable_mockModule('../api/fans', () => ({ Fans: {
  getMyClub: jest.fn(async () => club), listClubFeed: jest.fn(async () => []),
  listClubPosts: jest.fn(async () => posts), listClubMemories: jest.fn(async () => memories),
} }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: { partyId: 8, roles: [], modules: [] } }) }));
jest.unstable_mockModule('../components/GoogleDriveUploadWidget', () => ({ default: () => null }));
jest.unstable_mockModule('../features/interactions/InteractionPanel', () => ({ InteractionPanel: () => null }));
jest.unstable_mockModule('../components/PageShell', () => ({ default: ({ children }: { children: ReactNode }) => <>{children}</>, SkeletonCards: () => null, EmptyState: () => <div>Empty</div> }));
jest.unstable_mockModule('@mui/icons-material', () => Object.fromEntries(['PushPinOutlined', 'VisibilityOff', 'HowToVote', 'CalendarMonth', 'Forum', 'Groups', 'Add', 'PhotoLibrary', 'Report', 'Person', 'LockOutlined', 'MailOutline', 'Whatshot', 'TrendingUp', 'FiberNew'].map((name) => [name, () => null])));
const { default: FanClubPage } = await import('./FanClubPage');
afterEach(cleanup);
beforeEach(() => Object.defineProperty(HTMLElement.prototype, 'scrollIntoView', { configurable: true, value: jest.fn() }));
it.each([['post', 'Post 17', 'Foro'], ['memory', 'Memory 17', 'Recuerdos']])('opens the %s tab and focuses its requested source beyond the first page', async (parameter, title, tab) => {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  const view = render(<QueryClientProvider client={client}><MemoryRouter initialEntries={[`/fans/clubs/9?${parameter}=17`]}><Routes><Route path="/fans/clubs/:artistId" element={<FanClubPage />} /></Routes></MemoryRouter></QueryClientProvider>);
  const heading = await screen.findByText(title, { exact: true });
  const card = heading.closest('[tabindex="-1"]');
  await waitFor(() => expect(document.activeElement).toBe(card));
  expect(screen.getByRole('tab', { name: tab }).getAttribute('aria-selected')).toBe('true');
  expect(screen.queryByText(parameter === 'post' ? 'Post 1' : 'Memory 1', { exact: true })).toBeNull();
  fireEvent.click(screen.getByRole('tab', { name: 'Feed' }));
  expect(screen.getByRole('tab', { name: 'Feed' }).getAttribute('aria-selected')).toBe('true');
  view.unmount(); client.clear();
});
