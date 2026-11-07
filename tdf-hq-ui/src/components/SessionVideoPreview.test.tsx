import { fireEvent, render, screen } from '@testing-library/react';
import { afterEach, beforeEach, jest } from '@jest/globals';
const playerPause = jest.fn();
jest.unstable_mockModule('../player/PlayerProvider', () => ({ useOptionalPlayer: () => ({ pause: playerPause }) }));
const { default: SessionVideoPreview } = await import('./SessionVideoPreview');
const { PLAYER_TAKEOVER_EVENT } = await import('../player/types');

describe('inline video playback boundary', () => {
  beforeEach(() => { playerPause.mockClear(); jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => {}); });
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


describe('global audio coordination preserves current video behavior', () => {
  beforeEach(() => { playerPause.mockClear(); jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => {}); });
  afterEach(() => { jest.restoreAllMocks(); });

it('keeps the current video intent boundary and pauses global audio on actual file play', () => {
  const view = render(<SessionVideoPreview url="https://media.example.org/real.mp4" title="Session" />);
  expect(view.container.querySelector('video')).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
  fireEvent.play(view.container.querySelector('video')!);
  expect(playerPause).toHaveBeenCalledTimes(1);
});
it('pauses the existing inline file on player takeover without replacing its source', () => {
  const view = render(<SessionVideoPreview url="https://media.example.org/real.mp4" title="Session" />);
  fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
  const video = view.container.querySelector('video');
  fireEvent(window, new Event(PLAYER_TAKEOVER_EVENT));
  expect(HTMLMediaElement.prototype.pause).toHaveBeenCalledTimes(1);
  expect(view.container.querySelector('video')).toBe(video);
});

});
