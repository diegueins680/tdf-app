import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';

interface Channels { email: boolean; push: boolean }
const getNotificationChannels = jest.fn<() => Promise<Channels>>();
const updateNotificationChannels = jest.fn<(channels: Channels) => Promise<Channels>>();
jest.unstable_mockModule('../api/chat', () => ({
  ChatAPI: { getNotificationChannels, updateNotificationChannels },
}));
const { default: ChatNotificationChannels } = await import('./ChatNotificationChannels');

const isChecked = (element: HTMLElement) => (element as HTMLInputElement).checked;

function setup() {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  render(<QueryClientProvider client={client}><ChatNotificationChannels /></QueryClientProvider>);
}

beforeEach(() => {
  jest.clearAllMocks();
  getNotificationChannels.mockResolvedValue({ email: true, push: true });
  updateNotificationChannels.mockImplementation(async (channels) => channels);
});
afterEach(() => cleanup());

it('does not load preferences until the dialog is opened', () => {
  setup();
  expect(getNotificationChannels).not.toHaveBeenCalled();
});

it('turns the email reminder off while keeping phone notifications', async () => {
  setup();
  fireEvent.click(screen.getByRole('button', { name: 'Avisos' }));
  const email = await screen.findByRole('checkbox', { name: /Correo si no lo lees/ });
  expect(isChecked(email)).toBe(true);
  fireEvent.click(email);
  await waitFor(() => expect(updateNotificationChannels).toHaveBeenCalledWith({ email: false, push: true }));
  await waitFor(() => expect(isChecked(screen.getByRole('checkbox', { name: /Correo si no lo lees/ }))).toBe(false));
  expect(isChecked(screen.getByRole('checkbox', { name: /Notificación en el celular/ }))).toBe(true);
});

it('explains a failed load and offers a retry', async () => {
  getNotificationChannels.mockRejectedValueOnce(new Error('offline'));
  setup();
  fireEvent.click(screen.getByRole('button', { name: 'Avisos' }));
  expect(await screen.findByText('No se pudieron cargar tus preferencias.')).toBeTruthy();
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar' }));
  expect(isChecked(await screen.findByRole('checkbox', { name: /Correo si no lo lees/ }))).toBe(true);
});
