import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { RecordsFeedDTO } from '../api/records';
import { PLAYER_LOAD_TRACK_EVENT, type LoadPlayerTrackDetail } from '../player/types';
import type { InteractionIdentity, InteractionSummary } from '../api/interactions';

const getFeed = jest.fn<() => Promise<RecordsFeedDTO>>();
const summary = jest.fn<(...args: unknown[]) => Promise<InteractionSummary>>();
jest.unstable_mockModule('../api/records', () => ({ Records: { getFeed } }));
jest.unstable_mockModule('../api/interactions', () => ({ Interactions: { summary } }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: null }), getActiveSession: () => null, getStoredSessionToken: () => null }));
jest.unstable_mockModule('../features/interactions/InteractionPanel', () => ({
  InteractionPanel: ({ kind, entityKey, initiallyExpanded }: InteractionIdentity & { initiallyExpanded?: boolean }) =>
    <div data-testid="publication-panel" data-kind={kind} data-key={entityKey} data-expanded={String(initiallyExpanded)} />,
}));
jest.unstable_mockModule('../features/records/RecordThumbnail', () => ({ default: () => <span>Preview</span> }));
const { default: RecordsPublicPage } = await import('./RecordsPublicPage');

const id = '00100000-0000-4000-8000-000000000001';
const collections = [
  ['recordings', 'recording', 'recording'],
  ['sessions', 'recording_session', 'session'],
  ['releases', 'record_release', 'release'],
] as const;
const nativeMetadata = { schemaVersion: 1, kind: 'session', artistPartyIds: [535], artist: 'Synthetic artist',
  state: 'processing', durationMs: 30000, sources: [], chapters: [], credits: [] };
function item(title: string, metadata: object = nativeMetadata) {
  return { id, code: title, title, contributors: [], sortOrder: 0, resources: [
    { id: `${title}-youtube`, providerCode: 'youtube', externalCode: 'Synthetic001', kind: 'video',
      url: 'https://www.youtube.com/watch?v=Synthetic001', primary: true, sortOrder: 0 },
    { id: `${title}-native`, providerCode: 'tdf', externalCode: title, kind: 'video',
      url: 'https://media.invalid/source.mp4', primary: false, sortOrder: 1, providerMetadata: { tdfNative: metadata } },
  ] };
}
function feed(entries: Partial<Record<'recordings' | 'sessions' | 'releases', unknown[]>> = {}) {
  return { locale: 'es', revision: 1, collections: [], recordings: [], sessions: [], releases: [], ...entries } as RecordsFeedDTO;
}
let client: QueryClient;
function view(data: RecordsFeedDTO, query = '') {
  getFeed.mockResolvedValue(data);
  client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  return render(<QueryClientProvider client={client}><MemoryRouter initialEntries={[`/records${query}`]}><RecordsPublicPage /></MemoryRouter></QueryClientProvider>);
}
beforeEach(() => {
  jest.clearAllMocks();
  Object.defineProperty(HTMLElement.prototype, 'scrollIntoView', { configurable: true, value: jest.fn() });
  summary.mockResolvedValue({ id: 'target', title: 'Authorized publication without preview', kind: 'recording', key: id } as InteractionSummary);
});
afterEach(() => { cleanup(); client?.clear(); });

test.each(collections.filter(([collection]) => collection !== 'releases'))('selects one native %s discussion without duplicating the fallback lookup', async (collection, kind, parameter) => {
  view(feed({ [collection]: [item('Native selected')] }), `?${parameter}=${id}`);
  const article = await screen.findByRole('article', { name: 'Native selected' });
  await waitFor(() => expect(document.activeElement).toBe(article));
  const panels = screen.getAllByTestId('publication-panel');
  expect(panels).toHaveLength(1);
  expect(article.contains(panels[0]!)).toBe(true);
  expect(panels[0]!.getAttribute('data-kind')).toBe(kind);
  expect(panels[0]!.getAttribute('data-key')).toBe(id);
  expect(panels[0]!.getAttribute('data-expanded')).toBe('true');
  expect(summary).not.toHaveBeenCalled();
});

test('preserves native release provider links and delegates its one discussion and selected focus to ReleasesGrid', async () => {
  const release = item('Native release');
  release.resources.push({ id: 'release-spotify', providerCode: 'spotify', externalCode: 'release', kind: 'audio-track',
    url: 'https://open.spotify.com/album/synthetic-release', primary: true, sortOrder: 2 });
  view(feed({ releases: [release] }), `?release=${id}`);
  const nativeCard = await screen.findByRole('article', { name: 'Native release' });
  const providers = [['YouTube', 'https://www.youtube.com/watch?v=Synthetic001'],
    ['tdf', 'https://media.invalid/source.mp4'], ['Spotify', 'https://open.spotify.com/album/synthetic-release']];
  for (const [label, url] of providers) {
    const link = await screen.findByRole('link', { name: label, exact: true });
    expect(link.getAttribute('href')).toBe(url);
    expect(link.getAttribute('target')).toBe('_blank');
    expect(link.getAttribute('rel')).toBe('noopener noreferrer');
  }
  const panels = screen.getAllByTestId('publication-panel');
  expect(panels).toHaveLength(1);
  const panel = panels[0]!;
  expect(panel.getAttribute('data-kind')).toBe('record_release');
  expect(panel.getAttribute('data-key')).toBe(id);
  expect(nativeCard.contains(panel)).toBe(false);
  expect(nativeCard.hasAttribute('tabindex')).toBe(false);
  await waitFor(() => expect(document.activeElement).toBe(panel.closest('[tabindex="-1"]')));
  expect(summary).not.toHaveBeenCalled();
});

test.each(collections)('keeps the existing %s card and canonical interactions when native metadata is malformed', async (collection, kind) => {
  view(feed({ [collection]: [item('Legacy retained', { ...nativeMetadata, schemaVersion: 999 })] }));
  const panel = await screen.findByTestId('publication-panel');
  expect(screen.queryByRole('article', { name: 'Legacy retained' })).toBeNull();
  expect(panel.getAttribute('data-kind')).toBe(kind);
  expect(panel.getAttribute('data-key')).toBe(id);
  expect(screen.getAllByTestId('publication-panel')).toHaveLength(1);
});

test('keeps equal string IDs in different publication kinds distinct, including a malformed legacy peer', async () => {
  view(feed({ sessions: [item('Native session')], recordings: [item('Legacy recording', { schemaVersion: 999 })] }));
  await screen.findByRole('article', { name: 'Native session' });
  await waitFor(() => expect(screen.getAllByTestId('publication-panel')).toHaveLength(2));
  expect(screen.getAllByTestId('publication-panel').map(node => [node.getAttribute('data-kind'), node.getAttribute('data-key')]))
    .toEqual([['recording_session', id], ['recording', id]]);
});

test('keeps an authorized selected-publication fallback for feed entries without a renderable card', async () => {
  view(feed({ recordings: [{ ...item('No preview'), resources: [] }] }), `?recording=${id}`);
  await screen.findByRole('heading', { name: 'Authorized publication without preview' });
  expect(summary).toHaveBeenCalledTimes(1);
  expect(summary.mock.calls[0]?.slice(0, 2)).toEqual([{ kind: 'recording', entityKey: id }, false]);
  expect(screen.getAllByTestId('publication-panel')).toHaveLength(1);
});


function readyNativeMetadata() {
  return { ...nativeMetadata, state: 'ready', sources: [
    { url: 'https://media.invalid/native.mp4', quality: 'high', mediaType: 'video/mp4' },
    { url: 'https://media.invalid/native.m4a', quality: 'high', mediaType: 'audio/mp4' },
  ] };
}

test('keeps the legacy publication without native controls when its TDF resource is explicitly unavailable', async () => {
  const publication = item('Unavailable native', readyNativeMetadata());
  publication.resources = publication.resources.map(resource => resource.providerCode === 'tdf'
    ? { ...resource, availability: 'unavailable' } : resource);
  view(feed({ recordings: [publication] }));
  const panel = await screen.findByTestId('publication-panel');
  expect(screen.queryByRole('article', { name: 'Unavailable native' })).toBeNull();
  expect(screen.queryByRole('button', { name: 'Ver sesión', exact: true })).toBeNull();
  expect(screen.queryByRole('button', { name: 'Escuchar', exact: true })).toBeNull();
  expect(panel.getAttribute('data-kind')).toBe('recording');
  expect(panel.getAttribute('data-key')).toBe(id);
  expect(screen.getAllByTestId('publication-panel')).toHaveLength(1);
  expect((await screen.findByRole('link', { name: 'Ver video', exact: true })).getAttribute('href')).toBe('https://www.youtube.com/watch?v=Synthetic001');
});

test('keeps independent native playback when the YouTube resource is unavailable and its embed flag is denied', async () => {
  const publication = item('Independent native', readyNativeMetadata());
  publication.resources = publication.resources.map(resource => resource.providerCode === 'youtube'
    ? { ...resource, availability: 'unavailable' }
    : { ...resource, availability: 'available', providerMetadata: { ...resource.providerMetadata, embeddable: false } });
  const playback = jest.fn<(event: Event) => void>();
  window.addEventListener(PLAYER_LOAD_TRACK_EVENT, playback);
  try {
    view(feed({ recordings: [publication] }));
    const article = await screen.findByRole('article', { name: 'Independent native' });
    expect(article.contains(screen.getByTestId('publication-panel'))).toBe(true);
    const listen = screen.getByRole('button', { name: 'Escuchar', exact: true });
    expect((listen as HTMLButtonElement).disabled).toBe(false);
    fireEvent.click(listen);
    expect(playback).toHaveBeenCalledTimes(1);
    const detail = (playback.mock.calls[0]![0] as CustomEvent<LoadPlayerTrackDetail>).detail;
    expect(detail.mediaMode).toBe('audio');
    expect(detail.track.id).toBe(`records:recording:${id}:full`);
    expect(detail.track.sources).toHaveLength(2);
    expect(detail.track.sources.every(source => source.url.startsWith('https://media.invalid/native.'))).toBe(true);
  } finally { window.removeEventListener(PLAYER_LOAD_TRACK_EVENT, playback); }
});
