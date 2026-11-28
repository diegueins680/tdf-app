import { jest } from '@jest/globals';
import { fireEvent, render, screen } from '@testing-library/react';

const post = jest.fn<() => Promise<string>>();
let partyId = 1;
jest.unstable_mockModule('../api/client', () => ({ post }));
jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: { partyId } }),
}));
const { default: ChatKitPage } = await import('./ChatKitPage');

it('sends turns through the backend and clears them when the account changes', async () => {
  post.mockResolvedValueOnce('Respuesta uno').mockResolvedValueOnce('Respuesta dos');
  const view = render(<ChatKitPage />);
  const send = (message: string) => {
    fireEvent.change(screen.getByRole('textbox'), { target: { value: message } });
    fireEvent.click(screen.getByRole('button', { name: 'Enviar' }));
  };
  send('Hola');
  await screen.findByText('Respuesta uno');
  send('Gracias');
  await screen.findByText('Respuesta dos');
  expect(post).toHaveBeenLastCalledWith('/assistant/chat', ['Hola', 'Respuesta uno', 'Gracias']);
  partyId = 2;
  view.rerender(<ChatKitPage />);
  expect(screen.queryByText('Respuesta uno')).toBeNull();
});
