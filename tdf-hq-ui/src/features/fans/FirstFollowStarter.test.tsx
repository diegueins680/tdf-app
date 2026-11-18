import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot } from 'react-dom/client';
import FirstFollowStarter, { pickStarterArtists } from './FirstFollowStarter';

(globalThis as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

const artist = (apArtistId: number, apDisplayName: string, apFollowerCount = 0) => ({ apArtistId, apDisplayName, apFollowerCount });

function mount(props: Partial<Parameters<typeof FirstFollowStarter>[0]> = {}) {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);
  const onToggle = jest.fn();
  const onSeeAll = jest.fn();
  const render = (followedIds: ReadonlySet<number>) => act(() => {
    root.render(
      <FirstFollowStarter
        artists={[artist(9, 'Diego Saa', 21), artist(20, 'Otro man', 3)]}
        followedIds={followedIds}
        disabled={false}
        onToggle={onToggle}
        onSeeAll={onSeeAll}
        {...props}
      />,
    );
  });
  render(new Set());
  return { container, onToggle, onSeeAll, render, cleanup: () => { act(() => root.unmount()); container.remove(); } };
}

describe('FirstFollowStarter', () => {
  it('offers the most followed artists first and keeps the order stable for ties', () => {
    const picked = pickStarterArtists(
      [artist(1, 'Zeta'), artist(2, 'Alfa'), artist(3, 'Popular', 40), artist(4, 'Medio', 7), artist(5, 'Beta'), artist(6, 'Gama')],
    );
    expect(picked.map((a) => a.apDisplayName)).toEqual(['Popular', 'Medio', 'Alfa', 'Beta', 'Gama']);
  });

  it('follows with one tap and keeps the followed artist in place', () => {
    const view = mount();
    const follow = view.container.querySelector<HTMLButtonElement>('button[aria-label="Seguir a Diego Saa"]');
    expect(follow?.getAttribute('aria-pressed')).toBe('false');
    act(() => follow?.click());
    expect(view.onToggle).toHaveBeenCalledWith(9, false);

    view.render(new Set([9]));
    expect(view.container.textContent).toContain('Listo: ya sigues a tu primer artista');
    const following = view.container.querySelector<HTMLButtonElement>('button[aria-label="Siguiendo a Diego Saa"]');
    expect(following?.getAttribute('aria-pressed')).toBe('true');
    expect(view.container.querySelector('button[aria-label="Seguir a Otro man"]')).not.toBeNull();
    act(() => following?.click());
    expect(view.onToggle).toHaveBeenLastCalledWith(9, true);
    view.cleanup();
  });

  it('does not accept taps while a follow is being saved and links to the full list', () => {
    const view = mount({ disabled: true });
    expect(view.container.querySelector<HTMLButtonElement>('button[aria-label="Seguir a Diego Saa"]')?.disabled).toBe(true);
    const seeAll = Array.from(view.container.querySelectorAll('button')).find((b) => b.textContent === 'Ver todos los artistas');
    act(() => seeAll?.click());
    expect(view.onSeeAll).toHaveBeenCalledTimes(1);
    view.cleanup();
  });

  it('renders nothing without artists to suggest', () => {
    const view = mount({ artists: [] });
    expect(view.container.textContent).toBe('');
    view.cleanup();
  });
});
