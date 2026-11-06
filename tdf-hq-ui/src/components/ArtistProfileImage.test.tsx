import { cleanup, fireEvent, render } from '@testing-library/react';
import ArtistProfileImage from './ArtistProfileImage';
import { artistPlaceholderImage } from '../utils/artistFallbacks';

afterEach(cleanup);

it('renders provisional initials immediately and accepts an artist replacement', () => {
  const { container, rerender } = render(<ArtistProfileImage artistName="Roj(o)" />);
  expect(container.querySelector('img')?.getAttribute('src')).toBe(artistPlaceholderImage('Roj(o)'));
  expect(container.querySelector('img')?.getAttribute('alt')).toBe('Avatar provisional de Roj(o)');
  rerender(<ArtistProfileImage artistName="Roj(o)" imageUrl="https://cdn.example/rojo.webp" />);
  expect(container.querySelector('img')?.getAttribute('src')).toBe('https://cdn.example/rojo.webp');
});

it('removes failing responsive sources and falls back once, without refetch loops', () => {
  const { container } = render(<ArtistProfileImage artistName="Las Sintéticas" imageUrl="https://cdn.example/a.webp" avifSourceSet="https://cdn.example/a.avif 800w" />);
  fireEvent.error(container.querySelector('img')!);
  expect(container.querySelector('source')).toBeNull();
  const fallback = container.querySelector('img')!;
  expect(fallback.dataset.artistImage).toBe('initials');
  expect(fallback.getAttribute('src')).toBe(artistPlaceholderImage('Las Sintéticas'));
  fireEvent.error(fallback);
  expect(container.querySelector('img')).toBe(fallback);
});

it('isolates a retired source error from a new artist and new image', () => {
  const { container, rerender } = render(<ArtistProfileImage artistName="First" imageUrl="https://cdn.example/first.jpg" />);
  const oldImage = container.querySelector('img')!;
  rerender(<ArtistProfileImage artistName="Second" imageUrl="https://cdn.example/second.jpg" />);
  fireEvent.error(oldImage);
  expect(container.querySelector('img')?.getAttribute('src')).toBe('https://cdn.example/second.jpg');
});
