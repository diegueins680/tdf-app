import { jest } from '@jest/globals';
import { createTheme } from '@mui/material/styles';
import '@testing-library/jest-dom';
import { render, screen, fireEvent, within, act } from '@testing-library/react';
import StudioGallery from './StudioGallery';

const leavingScreen = createTheme().transitions.duration.leavingScreen;

describe('StudioGallery', () => {
  beforeEach(() => jest.useFakeTimers());
  afterEach(() => { jest.clearAllTimers(); jest.useRealTimers(); });
  it('opens a complete local photo and restores keyboard focus after Escape', async () => {
    render(<StudioGallery />);
    const gallery = screen.getByRole('group', { name: 'Fotos del estudio TDF' });
    expect(within(gallery).getAllByRole('button')).toHaveLength(6);
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
