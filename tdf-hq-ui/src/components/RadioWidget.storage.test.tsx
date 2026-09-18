import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import { fireEvent, waitFor } from '@testing-library/react';

const search = jest.fn(async () => []);
const clearPresence = jest.fn(async () => undefined);
jest.unstable_mockModule('../api/radio', () => ({ RadioAPI: { search, clearPresence, listAutoStopOptions: jest.fn(async () => []) } }));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: { listItems: jest.fn(async () => ({ items: [], total: 0 })) } }));
jest.unstable_mockModule('../utils/tidalAgent', () => ({ generateTidalCode: jest.fn() }));
const { default: RadioWidget } = await import('./RadioWidget');
(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

it.each(['getter', 'getItem', 'setItem'] as const)('keeps authenticated radio preferences usable under %s denial', async (operation) => {
  window.localStorage.clear();
  const descriptor = Object.getOwnPropertyDescriptor(window, 'localStorage')!;
  const deny = () => { throw new DOMException('Denied', 'SecurityError'); };
  const spy = operation === 'getter' ? undefined : jest.spyOn(Storage.prototype, operation).mockImplementation(deny);
  if (operation === 'getter') Object.defineProperty(window, 'localStorage', { configurable: true, get: deny });
  const pause = jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => undefined);
  const load = jest.spyOn(HTMLMediaElement.prototype, 'load').mockImplementation(() => undefined);
  const container = document.createElement('div'); document.body.appendChild(container);
  const root = createRoot(container);
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  try {
    await act(async () => { root.render(<QueryClientProvider client={client}><MemoryRouter initialEntries={['/admin/diagnosticos']}><RadioWidget /></MemoryRouter></QueryClientProvider>); });
    const mute = await waitFor(() => {
      const button = container.querySelector<HTMLButtonElement>('[aria-label="Silenciar radio"]');
      expect(button).not.toBeNull(); return button!;
    });
    await act(async () => { fireEvent.click(mute); });
    await waitFor(() => expect(container.querySelector('[aria-label="Quitar silencio"]')).not.toBeNull());
    expect(container.querySelector('[aria-label="Control de radio"]')).not.toBeNull();
  } finally {
    await act(async () => root.unmount()); container.remove(); client.clear();
    Object.defineProperty(window, 'localStorage', descriptor); spy?.mockRestore(); pause.mockRestore(); load.mockRestore();
  }
});
