import '@testing-library/jest-dom';
import { fireEvent, render, screen } from '@testing-library/react';
import { afterEach, beforeEach, jest } from '@jest/globals';
const capture = jest.fn();
const playerPause = jest.fn();
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => ({ capture }) }));
jest.unstable_mockModule('../player/PlayerProvider', () => ({ useOptionalPlayer: () => ({ pause: playerPause }) }));
const { default: SessionVideoPreview } = await import('./SessionVideoPreview');
const { PLAYER_TAKEOVER_EVENT } = await import('../player/types');

describe('inline video playback boundary', () => {
  beforeEach(() => { capture.mockClear(); playerPause.mockClear(); jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => {}); });
  afterEach(() => { jest.restoreAllMocks(); });
  it('loads no external frame before intent and stops/removes the old source on navigation', () => {
    const { container, rerender } = render(<SessionVideoPreview url="https://youtu.be/uRNyIMLiqU8" title="Session" />);
    expect(container.querySelector('iframe')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
    expect(container.querySelector('iframe')?.src).toContain('youtube-nocookie.com/embed/uRNyIMLiqU8');
    expect(container.querySelector('iframe')?.getAttribute('referrerpolicy')).toBe('strict-origin-when-cross-origin');
    rerender(<SessionVideoPreview url="https://media.example.org/another.mp4" title="Another session" />);
    expect(container.querySelector('iframe')).toBeNull();
    expect(container.querySelector('video')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
    expect(container.querySelector('video')?.controls).toBe(true);
    expect(container.querySelector('video')?.autoplay).toBe(false);
    fireEvent.error(container.querySelector('video')!);
    expect(screen.getByText(/No pudimos reproducir/)).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Cerrar reproductor' }));
    expect(container.querySelector('video')).toBeNull();
  });
  it('never inserts arbitrary source HTML', () => {
    const { container } = render(<SessionVideoPreview url="javascript:alert(1)" title="Unsafe" />);
    expect(container.querySelector('iframe')).toBeNull();
    expect(container.querySelector('a')).toBeNull();
  });
  it('explicitly pauses direct media when leaving the player', () => {
    const view = render(<SessionVideoPreview url="https://media.example.org/real.mp4" title="Session" />);
    fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
    view.unmount();
    expect(HTMLMediaElement.prototype.pause).toHaveBeenCalledTimes(1);
  });
  it('keeps the same source playing when its library metadata finishes loading', () => {
    const { container, rerender } = render(<SessionVideoPreview url="https://youtu.be/uRNyIMLiqU8" title="Session" analyticsContext={{ artistId: 520, videoId: 'featured' }} />);
    fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
    const frame = container.querySelector('iframe');
    rerender(<SessionVideoPreview url="https://www.youtube.com/watch?v=uRNyIMLiqU8" title="Session with credits" analyticsContext={{ artistId: 520, videoId: 'library-metadata-id' }} />);
    expect(container.querySelector('iframe')).toBe(frame);
  });
});


describe('source playback contracts with protected-main explicit intent', () => {
  beforeEach(() => { capture.mockClear(); playerPause.mockClear(); jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => {}); });
  afterEach(() => { jest.restoreAllMocks(); });
it('records observable public file playback without exposing media URLs', () => {
  render(<SessionVideoPreview url="https://media.example.com/session.mp4" title="Session" trackPublicPlayback />);
  fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
  const video = screen.getByLabelText('Session');
  fireEvent.play(video); fireEvent.ended(video);
  expect(capture.mock.calls.map((row) => row[0])).toEqual(['video_campaign_play_started', 'video_campaign_play_ended']);
  expect(JSON.stringify(capture.mock.calls)).not.toContain('media.example.com');
  fireEvent.error(video);
  expect(screen.getByRole('alert')).toHaveTextContent('No pudimos reproducir');
  expect(screen.getByRole('link')).toHaveAttribute('href', 'https://media.example.com/session.mp4');
});

it('does not count private editor previews or provider frame loads as playback', () => {
  const { rerender } = render(<SessionVideoPreview url="https://media.example.com/session.mp4" title="Preview" />);
  fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
  fireEvent.play(screen.getByLabelText('Preview'));
  rerender(<SessionVideoPreview url="https://youtu.be/msDxB2NU3c4" title="Provider" trackPublicPlayback />);
  fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
  fireEvent.load(screen.getByTitle('Provider'));
  expect(capture).not.toHaveBeenCalled();
});

it.each([
  ['?resourcekey=0-example_key&usp=sharing', '?resourcekey=0-example_key'],
  ['?resourcekey=key%2Bvalue%26other%3Dvalue&usp=sharing', '?resourcekey=key%2Bvalue%26other%3Dvalue'],
  ['?usp=sharing', ''],
  ['?resourcekey=', ''],
])('preserves only a supplied Drive resource key in the preview (%s)', (sourceQuery, previewQuery) => {
  const source = `https://drive.google.com/file/d/synthetic-file/view${sourceQuery}`;
  render(<SessionVideoPreview url={source} title="Drive session" trackPublicPlayback />);
  fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
  expect(screen.getByTitle('Drive session')).toHaveAttribute('src',
    `https://drive.google.com/file/d/synthetic-file/preview${previewQuery}`);
  expect(screen.getByRole('link')).toHaveAttribute('href', source);
  fireEvent.load(screen.getByTitle('Drive session'));
  expect(capture).not.toHaveBeenCalled();
});

it('hands native video playback to and from the global transport without overlapping audio', () => {
  const pause = jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => undefined);
  render(<SessionVideoPreview url="https://media.example.com/session.mp4" title="Video handoff" />);
  fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
  fireEvent.play(screen.getByLabelText('Video handoff'));
  expect(playerPause).toHaveBeenCalledTimes(1);
  window.dispatchEvent(new CustomEvent(PLAYER_TAKEOVER_EVENT));
  expect(pause).toHaveBeenCalledTimes(1);
});

});
