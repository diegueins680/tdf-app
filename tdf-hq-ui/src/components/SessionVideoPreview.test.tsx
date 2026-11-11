import { fireEvent, render, screen } from '@testing-library/react';
import { afterEach, beforeEach, jest } from '@jest/globals';
import SessionVideoPreview from './SessionVideoPreview';

describe('inline video playback boundary', () => {
  beforeEach(() => { jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => {}); });
  afterEach(() => { jest.restoreAllMocks(); });
  it('shows the actual YouTube still when the saved poster is a Spotify page', () => {
    const { container } = render(<SessionVideoPreview url="https://youtu.be/5i64z0dAso4?si=tracking" title="Artist video"
      poster="https://open.spotify.com/artist/6348Nu8zc4gZC8vtHzPP8R" />);
    expect(container.querySelector('img')?.getAttribute('src')).toBe('https://i.ytimg.com/vi/5i64z0dAso4/hqdefault.jpg');
    expect(container.querySelector('iframe')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
    expect(container.querySelector('iframe')?.src).toContain('/embed/5i64z0dAso4');
  });
  it('recovers from failed video thumbnails without leaving a broken image', () => {
    const { container, rerender } = render(<SessionVideoPreview url="https://youtu.be/5i64z0dAso4" title="Artist video" />);
    fireEvent.error(container.querySelector('img')!);
    expect(container.querySelector('img')?.src).toContain('/mqdefault.jpg');
    fireEvent.error(container.querySelector('img')!);
    expect(container.querySelector('img')).toBeNull();
    expect(screen.getByRole('button', { name: 'Ver video aquí' })).toBeTruthy();
    rerender(<SessionVideoPreview url="https://youtu.be/uRNyIMLiqU8" title="Another video" />);
    expect(container.querySelector('img')?.src).toContain('/uRNyIMLiqU8/hqdefault.jpg');
  });
  it('loads no external frame before intent and stops/removes the old source on navigation', () => {
    const { container, rerender } = render(<SessionVideoPreview url="https://youtu.be/uRNyIMLiqU8" title="Session" />);
    expect(container.querySelector('iframe')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
    expect(container.querySelector('iframe')?.src).toContain('youtube-nocookie.com/embed/uRNyIMLiqU8');
    expect(container.querySelector('iframe')?.getAttribute('referrerpolicy')).toBe('strict-origin-when-cross-origin');
    rerender(<SessionVideoPreview url="https://media.example.org/another.mp4" title="Another session" />);
    expect(container.querySelector('iframe')).toBeNull();
    expect(container.querySelector('video')?.preload).toBe('metadata');
    expect(container.querySelector('video')?.controls).toBe(false);
    fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
    expect(container.querySelector('video')?.controls).toBe(true);
    expect(container.querySelector('video')?.autoplay).toBe(false);
    fireEvent.error(container.querySelector('video')!);
    expect(screen.getByText(/No pudimos reproducir/)).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Cerrar reproductor' }));
    expect(container.querySelector('video')?.controls).toBe(false);
    expect(container.querySelector('video')?.autoplay).toBe(false);
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
