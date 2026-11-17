import { fireEvent, render, screen } from '@testing-library/react';
import ArtistCoverPreview from './ArtistCoverPreview';

it('shows an actionable error for page links and recovers when replaced by an image', () => {
  const { container, rerender } = render(<ArtistCoverPreview url="https://open.spotify.com/artist/abc" />);
  expect(container.querySelector('img')).toBeNull();
  expect(screen.getByRole('alert').textContent).toContain('Sube una imagen');
  rerender(<ArtistCoverPreview url="https://cdn.example/cover.jpg" />);
  expect(screen.queryByRole('alert')).toBeNull();
  fireEvent.error(screen.getByAltText('Vista previa de portada'));
  expect(container.querySelector('img')).toBeNull();
  expect(screen.getByRole('alert').textContent).toContain('enlace sea público');
  rerender(<ArtistCoverPreview url="https://cdn.example/replacement.jpg" />);
  expect(screen.getByAltText('Vista previa de portada').getAttribute('src')).toBe('https://cdn.example/replacement.jpg');
});
