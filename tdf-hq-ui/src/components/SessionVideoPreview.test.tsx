import { fireEvent, render, screen } from '@testing-library/react';
import { afterEach, beforeEach, jest } from '@jest/globals';
import SessionVideoPreview from './SessionVideoPreview';
import { PlayerProvider, usePlayer } from '../player/PlayerProvider';
import { act } from '@testing-library/react';

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
  it('keeps the same source playing when its library metadata finishes loading', () => {
    const { container, rerender } = render(<SessionVideoPreview url="https://youtu.be/uRNyIMLiqU8" title="Session" analyticsContext={{ artistId: 520, videoId: 'featured' }} />);
    fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
    const frame = container.querySelector('iframe');
    rerender(<SessionVideoPreview url="https://www.youtube.com/watch?v=uRNyIMLiqU8" title="Session with credits" analyticsContext={{ artistId: 520, videoId: 'library-metadata-id' }} />);
    expect(container.querySelector('iframe')).toBe(frame);
  });
});

it('hands native and provider previews back to the current global engine', async () => {
  jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => undefined);
  jest.spyOn(HTMLMediaElement.prototype, 'load').mockImplementation(() => undefined);
  jest.spyOn(HTMLMediaElement.prototype, 'currentSrc', 'get').mockImplementation(function(this:HTMLMediaElement){return this.src;});
  jest.spyOn(HTMLMediaElement.prototype, 'paused', 'get').mockReturnValue(false);
  jest.spyOn(HTMLMediaElement.prototype, 'play').mockImplementation(function(this:HTMLMediaElement){this.dispatchEvent(new Event('playing'));return Promise.resolve();});
  let player: ReturnType<typeof usePlayer>;
  function Probe(){player=usePlayer();return null;}
  const item=(id:string)=>({id,title:id,artist:'Fixture',sources:[{url:`https://media.example/${id}.mp3`,quality:'high' as const,mediaType:'audio/mpeg'}]});
  const view=render(<PlayerProvider><Probe/><SessionVideoPreview url="https://media.example.com/session.mp4" title="Video handoff"/></PlayerProvider>);
  await act(async()=>player.playTrack(item('first')));
  expect(player!.status).toBe('playing');
  fireEvent.click(screen.getByRole('button',{name:'Ver video aquí'}));
  expect(player!.status).toBe('paused');
  fireEvent.play(screen.getByLabelText('Video handoff'));
  await act(async()=>player.playTrack(item('second')));
  expect(screen.queryByLabelText('Video handoff')).toBeNull();
  expect(player!.status).toBe('playing');
  view.rerender(<PlayerProvider><Probe/><SessionVideoPreview url="https://youtu.be/msDxB2NU3c4" title="Provider handoff"/></PlayerProvider>);
  fireEvent.click(screen.getByRole('button',{name:'Ver video aquí'}));
  expect(player!.status).toBe('paused');
  expect(screen.getByTitle('Provider handoff')).toBeTruthy();
  await act(async()=>player.playTrack(item('third')));
  expect(screen.queryByTitle('Provider handoff')).toBeNull();
  expect(player!.status).toBe('playing');
});
