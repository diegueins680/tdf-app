import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';

const getConfig = jest.fn(async () => null);
const listEvents = jest.fn(async () => []);
jest.unstable_mockModule('../api/calendar', () => ({ CalendarApi: {
  getConfig, listEvents, getAuthUrl: jest.fn(), exchangeCode: jest.fn(), sync: jest.fn(),
} }));
jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({
  useLocalePreferences: () => ({ locale: 'es', timezone: 'America/Guayaquil' }),
}));
const { default: CalendarSyncPage } = await import('./CalendarSyncPage');
(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

const pause = () => new Promise(resolve => setTimeout(resolve, 0));
describe('CalendarSyncPage optional browser preferences', () => {
  afterEach(() => jest.restoreAllMocks());
  it.each(['getItem', 'setItem', 'removeItem', 'getter'] as const)(
    'keeps the real form usable when storage %s fails', async (failure) => {
      window.localStorage.clear();
      getConfig.mockClear(); listEvents.mockClear();
      const unavailable = () => { throw new DOMException('Storage unavailable', 'SecurityError'); };
      if (failure === 'getter') jest.spyOn(window, 'localStorage', 'get').mockImplementation(unavailable);
      else jest.spyOn(Storage.prototype, failure).mockImplementation(unavailable);
      const container = document.createElement('div'); document.body.append(container);
      const root = createRoot(container);
      const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
      try {
        await act(async () => {
          root.render(<MemoryRouter><QueryClientProvider client={client}><CalendarSyncPage /></QueryClientProvider></MemoryRouter>);
          await pause();
        });
        await act(async () => { await pause(); });
        expect(container.textContent).toContain('Integración Google Calendar');
        expect(getConfig).toHaveBeenCalled();
        expect(listEvents).toHaveBeenCalled();
        const clear = [...container.querySelectorAll('button')].find(button => button.textContent?.includes('Desconectar y limpiar'));
        expect(clear).toBeDefined();
        await act(async () => { clear?.click(); await pause(); });
        expect(container.textContent).toContain('Integración Google Calendar');
        expect(container.querySelector<HTMLInputElement>('input[role="combobox"]')?.value).toBe('primary');
      } finally {
        await act(async () => root.unmount()); client.clear(); container.remove();
      }
    },
  );
});
