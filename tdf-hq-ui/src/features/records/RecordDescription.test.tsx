import '@testing-library/jest-dom';
import { afterEach, expect, it, jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import RecordDescription from './RecordDescription';

afterEach(() => {
  cleanup();
  jest.restoreAllMocks();
});

it('keeps short descriptions visible and opens overflowing descriptions outside the card', async () => {
  const bodyHeight = jest.spyOn(HTMLElement.prototype, 'offsetHeight', 'get').mockReturnValue(20);
  jest.spyOn(Element.prototype, 'clientHeight', 'get').mockReturnValue(60);
  const { rerender } = render(<RecordDescription text="Sesión en Quito" title="Ritual" />);
  expect(screen.getByText('Sesión en Quito')).toBeVisible();
  expect(screen.queryByRole('button')).toBeNull();

  bodyHeight.mockReturnValue(400);
  rerender(<RecordDescription text={'Créditos\n\n[Artista](/artista)'} title="Ritual" />);
  expect(screen.queryByRole('dialog')).toBeNull();
  expect(screen.getByText('Artista').closest('[inert]')).toBeInTheDocument();
  fireEvent.click(screen.getByRole('button', { name: 'Leer más' }));
  const dialog = within(screen.getByRole('dialog', { name: 'Ritual' }));
  expect(dialog.getByRole('link', { name: 'Artista' })).toHaveAttribute('href', '/artista');
  fireEvent.click(dialog.getByRole('button', { name: 'Cerrar' }));
  await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
});
