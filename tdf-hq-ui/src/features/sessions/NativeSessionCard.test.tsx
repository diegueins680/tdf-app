import { jest } from '@jest/globals';
import { cleanup, render, screen } from '@testing-library/react';
import type { InteractionIdentity } from '../../api/interactions';
import type { NativePublication } from './nativeSessions';

jest.unstable_mockModule('../interactions/InteractionPanel', () => ({
  InteractionPanel: ({ kind, entityKey, initiallyExpanded }: InteractionIdentity & { initiallyExpanded?: boolean }) =>
    <div data-testid="native-publication-interactions" data-kind={kind} data-entity-key={entityKey} data-expanded={String(initiallyExpanded)} />,
}));
const { NativeSessionCard } = await import('./NativeSessionCard');
afterEach(cleanup);

it.each<NativePublication>([
  { kind: 'recording', entityKey: '00100000-0000-4000-8000-000000000001' },
  { kind: 'recording_session', entityKey: '00200000-0000-4000-8000-000000000002' },
  { kind: 'record_release', entityKey: '00300000-0000-4000-8000-000000000003' },
])('renders the canonical interaction panel for the original $kind publication', (publication) => {
  const session = { id: publication.entityKey, title: 'Synthetic session', artist: 'Synthetic artist', artistPartyIds: [535],
    durationMs: 30000, state: 'ready' as const, publication,
    sources: [{ url: 'https://media.invalid/session.mp4', quality: 'high' as const, mediaType: 'video/mp4' },
      { url: 'https://media.invalid/session.m4a', quality: 'high' as const, mediaType: 'audio/mp4' }], chapters: [], credits: [] };
  render(<NativeSessionCard session={session} />);
  const panel = screen.getByTestId('native-publication-interactions');
  expect(panel.getAttribute('data-kind')).toBe(publication.kind);
  expect(panel.getAttribute('data-entity-key')).toBe(publication.entityKey);
});

it('focuses the selected native publication and expands its canonical discussion', () => {
  Object.defineProperty(HTMLElement.prototype, 'scrollIntoView', { configurable: true, value: jest.fn() });
  const session = { id: '00100000-0000-4000-8000-000000000001', title: 'Selected native session',
    artist: 'Synthetic artist', artistPartyIds: [535], publication: { kind: 'recording_session' as const, entityKey: '00100000-0000-4000-8000-000000000001' },
    durationMs: 30000, state: 'processing' as const, sources: [], chapters: [], credits: [] };
  render(<NativeSessionCard session={session} selected />);
  expect(screen.getByRole('article', { name: session.title })).toBe(document.activeElement);
  expect(screen.getByTestId('native-publication-interactions').getAttribute('data-expanded')).toBe('true');
});

it('keeps native playback while delegating discussion and selected focus to another presentation', () => {
  const scroll = jest.fn();
  Object.defineProperty(HTMLElement.prototype, 'scrollIntoView', { configurable: true, value: scroll });
  const session = { id: 'release-id', title: 'Release playback', artist: 'Synthetic artist', artistPartyIds: [535],
    publication: { kind: 'record_release' as const, entityKey: 'release-id' }, durationMs: 30000,
    state: 'processing' as const, sources: [], chapters: [], credits: [] };
  render(<NativeSessionCard session={session} selected showInteractions={false} />);
  const article = screen.getByRole('article', { name: session.title });
  expect(article.hasAttribute('tabindex')).toBe(false);
  expect(scroll).not.toHaveBeenCalled();
  expect(screen.queryByTestId('native-publication-interactions')).toBeNull();
  expect(screen.getByRole('button', { name: 'Ver sesión' })).toBeTruthy();
  expect(screen.getByRole('button', { name: 'Escuchar' })).toBeTruthy();
});
