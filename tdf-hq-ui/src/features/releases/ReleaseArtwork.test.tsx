import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import ReleaseArtwork from './ReleaseArtwork';

afterEach(cleanup);

it('tries catalog artwork and artist photo after failures, then shows an accessible placeholder', () => {
  render(<ReleaseArtwork sources={['cover.jpg', 'catalog.jpg', 'artist.jpg']} title="DMT" artistName="Skankafe" />);
  for (const source of ['cover.jpg', 'catalog.jpg', 'artist.jpg']) {
    const image = screen.getByRole('img', { name: 'Portada de DMT' });
    expect(image.getAttribute('src')).toBe(source);
    fireEvent.error(image);
  }
  expect(screen.getByRole('img', { name: 'Sin portada disponible: DMT' }).textContent).toBe('SK');
});

it('retries new artwork when asynchronous catalog data arrives', () => {
  const { rerender } = render(<ReleaseArtwork sources={['broken.jpg']} title="DMT" artistName="Skankafe" />);
  fireEvent.error(screen.getByRole('img'));
  rerender(<ReleaseArtwork sources={['catalog.jpg']} title="DMT" artistName="Skankafe" />);
  expect(screen.getByRole('img').getAttribute('src')).toBe('catalog.jpg');
});

it('renders a named placeholder when no source exists', () => {
  render(<ReleaseArtwork sources={[]} title="DMT" artistName="Skankafe" />);
  expect(screen.getByRole('img', { name: 'Sin portada disponible: DMT' }).textContent).toBe('SK');
});
