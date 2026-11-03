import { fireEvent, render, screen } from '@testing-library/react';
import { afterEach, beforeEach, jest } from '@jest/globals';
import SessionVideoPreview from './SessionVideoPreview';

describe('inline video playback boundary', () => {
  beforeEach(() => { jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => {}); });
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
});
