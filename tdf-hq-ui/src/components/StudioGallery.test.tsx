import { jest } from '@jest/globals';
import { createTheme } from '@mui/material/styles';
import '@testing-library/jest-dom';
import { render, screen, fireEvent, within, act } from '@testing-library/react';
import { StudioGalleryPhotos } from './StudioGallery';
import { studioGallerySeed } from '../testFixtures/studioGallery';
const StudioGallery = () => <StudioGalleryPhotos photos={studioGallerySeed.photos.slice(4)} />;

const leavingScreen = createTheme().transitions.duration.leavingScreen;

describe('StudioGallery', () => {
  beforeEach(() => jest.useFakeTimers());
  afterEach(() => { jest.clearAllTimers(); jest.useRealTimers(); });
  it('shows recent additions first and paginates without duplicating or dropping archive photos', () => {
    render(<StudioGallery />);
    const photos = () => screen.getAllByRole('button', { name: /^Ampliar foto:/ });
    const firstPage = photos().map(button => button.getAttribute('aria-label'));
    expect(firstPage).toHaveLength(6);
    expect(firstPage[0]).toBe('Ampliar foto: Roj(o) en vivo en TDF');
    expect(screen.getByRole('status')).toHaveTextContent('Fotos 1–6 de 9');
    expect(screen.getByRole('button', { name: 'Página anterior de fotos' })).toBeDisabled();
    fireEvent.click(screen.getByRole('button', { name: 'Página siguiente de fotos' }));
    const lastPage = photos().map(button => button.getAttribute('aria-label'));
    expect(lastPage).toHaveLength(3);
    expect(new Set([...firstPage, ...lastPage]).size).toBe(9);
    expect(screen.getByRole('status')).toHaveTextContent('Fotos 7–9 de 9');
    expect(screen.getByRole('button', { name: 'Página siguiente de fotos' })).toBeDisabled();
    expect(screen.getByRole('button', { name: 'Página actual 2 de fotos' })).toHaveAttribute('aria-current', 'page');
    fireEvent.click(screen.getByRole('button', { name: 'Ir a la página 1 de fotos' }));
    expect(photos().map(button => button.getAttribute('aria-label'))).toEqual(firstPage);
  });

  it('shows all thirteen migrated photos over three pages and credits the Maps photos', () => {
    render(<StudioGalleryPhotos photos={studioGallerySeed.photos} />);
    const labels = () => screen.getAllByRole('button', { name: /^Ampliar foto:/ }).map(button => button.getAttribute('aria-label'));
    const seen = labels();
    expect(seen).toHaveLength(6);
    expect(seen[0]).toBe('Ampliar foto: Procesadores y preamplificadores');
    fireEvent.click(screen.getByRole('button', { name: seen[0]! }));
    const dialog = screen.getByRole('dialog');
    expect(within(dialog).getByText(/Daniel Escudero/)).toHaveTextContent('jun');
    expect(within(dialog).getByRole('link')).toHaveAttribute('href', studioGallerySeed.photos[0]!.sourceUrl);
  });

  it('paginates all thirteen photos without duplicates and clamps the page after removal', () => {
    const view = render(<StudioGalleryPhotos photos={studioGallerySeed.photos} />);
    const labels = () => screen.getAllByRole('button', { name: /^Ampliar foto:/ }).map(button => button.getAttribute('aria-label'));
    const seen = labels();
    fireEvent.click(screen.getByRole('button', { name: 'Página siguiente de fotos' }));
    seen.push(...labels());
    expect(labels()).toHaveLength(6);
    fireEvent.click(screen.getByRole('button', { name: 'Página siguiente de fotos' }));
    seen.push(...labels());
    expect(labels()).toHaveLength(1);
    expect(new Set(seen).size).toBe(13);
    expect(screen.getByRole('status')).toHaveTextContent('Fotos 13–13 de 13');
    view.rerender(<StudioGalleryPhotos photos={studioGallerySeed.photos.slice(0, 4)} />);
    expect(labels()).toHaveLength(4);
    expect(screen.getByRole('status')).toHaveTextContent('Fotos 1–4 de 4');
    expect(screen.queryByRole('navigation')).not.toBeInTheDocument();
  });

  it('opens the correct image from the last page and restores focus there after closing', async () => {
    render(<StudioGallery />);
    fireEvent.click(screen.getByRole('button', { name: 'Ir a la página 2 de fotos' }));
    const trigger = screen.getByRole('button', { name: 'Ampliar foto: La música nos reúne' });
    act(() => trigger.focus());
    fireEvent.click(trigger);
    const dialog = screen.getByRole('dialog', { name: 'La música nos reúne' });
    expect(within(dialog).getByRole('img')).toHaveAttribute('src', '/assets/tdf-studio/studio-community-1280.webp');
    fireEvent.keyDown(dialog, { key: 'Escape' });
    await act(async () => { await jest.advanceTimersByTimeAsync(leavingScreen); });
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
    expect(trigger).toHaveFocus();
    expect(screen.getByRole('status')).toHaveTextContent('Fotos 7–9 de 9');
  });

  it('labels new session stills with their capture date and credits', () => {
    render(<StudioGallery />);
    fireEvent.click(screen.getByRole('button', { name: 'Ampliar foto: Roj(o) en vivo en TDF' }));
    const dialog = screen.getByRole('dialog', { name: 'Roj(o) en vivo en TDF' });
    expect(within(dialog).getByText(/24 feb.*2025/)).toHaveTextContent('Fotograma de la sesión');
    expect(within(dialog).getByRole('link', { name: 'Ver publicación y créditos' })).toHaveAttribute('href', 'https://www.instagram.com/p/DfLV5lMCnfX/');
    expect(within(dialog).getByRole('img')).toHaveAttribute('src', '/assets/tdf-studio/rojo-live-room-1280.webp');
  });

  it('opens a complete local photo and restores keyboard focus after Escape', async () => {
    render(<StudioGallery />);
    const gallery = screen.getByRole('group', { name: 'Fotos del estudio TDF' });
    expect(within(gallery).getAllByRole('button', { name: /^Ampliar foto:/ })).toHaveLength(6);
    const trigger = within(gallery).getByRole('button', { name: 'Ampliar foto: Sala de control' });
    act(() => trigger.focus());
    fireEvent.click(trigger);
    const dialog = screen.getByRole('dialog', { name: 'Sala de control' });
    const photo = within(dialog).getByRole('img');
    expect(photo).toHaveAttribute('src', '/assets/tdf-studio/control-room-1280.webp');
    expect(within(dialog).getByRole('status')).toHaveTextContent('Cargando foto');
    fireEvent.load(photo);
    expect(within(dialog).queryByRole('status')).not.toBeInTheDocument();
    fireEvent.keyDown(dialog, { key: 'Escape' });
    await act(async () => { await jest.advanceTimersByTimeAsync(leavingScreen); });
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
    expect(trigger).toHaveFocus();
  });

  it('recovers a missing thumbnail through the full photo without removing other photos', () => {
    render(<StudioGallery />);
    const trigger = screen.getByRole('button', { name: 'Ampliar foto: Sala de control' });
    fireEvent.error(within(trigger).getByRole('img'));
    expect(within(trigger).getByText('Foto no disponible. Abre para reintentar.')).toBeInTheDocument();
    expect(screen.getAllByRole('img')).toHaveLength(5);
    fireEvent.click(trigger);
    expect(within(screen.getByRole('dialog')).getByRole('img')).toHaveAttribute('src', '/assets/tdf-studio/control-room-1280.webp');
  });

  it('offers a real new request after a preview failure and still permits closing', async () => {
    render(<StudioGallery />);
    fireEvent.click(screen.getByRole('button', { name: 'Ampliar foto: Sala de grabación' }));
    const dialog = screen.getByRole('dialog', { name: 'Sala de grabación' });
    fireEvent.error(within(dialog).getByRole('img'));
    expect(within(dialog).getByRole('alert')).toHaveTextContent('No se pudo cargar esta foto');
    fireEvent.click(within(dialog).getByRole('button', { name: 'Reintentar foto' }));
    const retry = within(dialog).getByRole('img');
    expect(retry).toHaveAttribute('src', '/assets/tdf-studio/recording-room-1280.webp?retry=1');
    fireEvent.load(retry);
    expect(within(dialog).queryByRole('alert')).not.toBeInTheDocument();
    fireEvent.click(within(dialog).getByRole('button', { name: 'Cerrar foto' }));
    await act(async () => { await jest.advanceTimersByTimeAsync(leavingScreen); });
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
  });
});


it('uses the full-size photo when a published optional thumbnail is empty', () => {
  const photo = { ...studioGallerySeed.photos[0]!, thumbnailUrl: '' };
  render(<StudioGalleryPhotos photos={[photo]} />);
  expect(screen.getByRole('img', { name: photo.alt })).toHaveAttribute('src', photo.imageUrl);
});
