import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import { fireEvent, waitFor } from '@testing-library/react';

const search = jest.fn(async () => []);
const clearPresence = jest.fn(async () => undefined);
jest.unstable_mockModule('../api/radio', () => ({ RadioAPI: { search, clearPresence, listAutoStopOptions: jest.fn(async () => []) } }));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: { listItems: jest.fn(async () => ({ items: [], total: 0 })) } }));
jest.unstable_mockModule('../utils/tidalAgent', () => ({ generateTidalCode: jest.fn() }));
const { default: RadioWidget } = await import('./RadioWidget');
(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

const VAR = '--tdf-radio-bar-height';
const originalMatchMedia = window.matchMedia;

/** Emulates a viewport width for MUI's useMediaQuery (max-width queries only). */
const setViewportWidth = (width: number) => {
  window.matchMedia = ((query: string) => {
    const max = /max-width:\s*([\d.]+)px/.exec(query);
    const min = /min-width:\s*([\d.]+)px/.exec(query);
    const matches = (max ? width <= Number(max[1]) : true) && (min ? width >= Number(min[1]) : true);
    return {
      matches,
      media: query,
      onchange: null,
      addListener: () => undefined,
      removeListener: () => undefined,
      addEventListener: () => undefined,
      removeEventListener: () => undefined,
      dispatchEvent: () => false,
    } as MediaQueryList;
  }) as typeof window.matchMedia;
};

let pause: ReturnType<typeof jest.spyOn>;
let load: ReturnType<typeof jest.spyOn>;
let rect: ReturnType<typeof jest.spyOn>;

beforeEach(() => {
  window.localStorage.clear();
  pause = jest.spyOn(HTMLMediaElement.prototype, 'pause').mockImplementation(() => undefined);
  load = jest.spyOn(HTMLMediaElement.prototype, 'load').mockImplementation(() => undefined);
  rect = jest.spyOn(HTMLElement.prototype, 'getBoundingClientRect').mockImplementation(function (this: HTMLElement) {
    const height = this.getAttribute('data-testid') === 'radio-docked-bar' ? 58 : 0;
    return { x: 0, y: 0, top: 0, left: 0, right: 0, bottom: height, width: 0, height, toJSON: () => ({}) } as DOMRect;
  });
});

afterEach(() => {
  window.matchMedia = originalMatchMedia;
  pause.mockRestore();
  load.mockRestore();
  rect.mockRestore();
  document.documentElement.style.removeProperty(VAR);
  document.body.style.paddingBottom = '';
});

const renderWidget = async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root: Root = createRoot(container);
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  await act(async () => {
    root.render(
      <QueryClientProvider client={client}>
        <MemoryRouter initialEntries={['/live-sessions/registro']}>
          <RadioWidget />
        </MemoryRouter>
      </QueryClientProvider>,
    );
  });
  const bar = await waitFor(() => {
    const node = container.querySelector<HTMLElement>('[data-testid="radio-docked-bar"]');
    expect(node).not.toBeNull();
    return node!;
  });
  return {
    container,
    bar,
    unmount: async () => {
      await act(async () => root.unmount());
      container.remove();
      client.clear();
    },
  };
};

it('drops prev/next from the docked bar on phones while keeping play, mute, hide and expand', async () => {
  setViewportWidth(360);
  const { container, unmount } = await renderWidget();
  try {
    await waitFor(() => expect(container.querySelector('[aria-label="Saltar a la estación anterior"]')).toBeNull());
    expect(container.querySelector('[aria-label="Saltar a la siguiente estación"]')).toBeNull();
    for (const label of ['Reproducir radio', 'Silenciar radio', 'Ocultar barra de radio', 'Expandir radio']) {
      expect(container.querySelector(`[aria-label="${label}"]`)).not.toBeNull();
    }
    // The status text takes the remaining width (flex: 1, minWidth: 0) instead of a fixed 220px box.
    const status = container.querySelector<HTMLElement>('[data-testid="radio-docked-status"]')!;
    expect(window.getComputedStyle(status).minWidth).toBe('0');
    expect(window.getComputedStyle(status).flexGrow).toBe('1');
  } finally {
    await unmount();
  }
});

it('keeps prev/next in the docked bar on wider screens', async () => {
  setViewportWidth(1024);
  const { container, unmount } = await renderWidget();
  try {
    expect(container.querySelector('[aria-label="Saltar a la estación anterior"]')).not.toBeNull();
    expect(container.querySelector('[aria-label="Saltar a la siguiente estación"]')).not.toBeNull();
  } finally {
    await unmount();
  }
});

it('publishes the docked bar height as a CSS variable and reserves it at the bottom of the page', async () => {
  setViewportWidth(360);
  document.body.style.paddingBottom = '';
  const { container, unmount } = await renderWidget();
  try {
    await waitFor(() => expect(document.documentElement.style.getPropertyValue(VAR)).toBe('58px'));
    expect(document.body.style.paddingBottom).toBe('calc(var(--tdf-global-player-height, 0px) + var(--tdf-radio-bar-height, 0px))');

    // Hiding the bar releases the reserved space.
    await act(async () => {
      fireEvent.click(container.querySelector('[aria-label="Ocultar barra de radio"]')!);
    });
    await waitFor(() => expect(document.documentElement.style.getPropertyValue(VAR)).toBe(''));
    expect(document.body.style.paddingBottom).toBe('');
  } finally {
    await unmount();
  }
});

it('removes the CSS variable when the widget unmounts', async () => {
  setViewportWidth(360);
  const { unmount } = await renderWidget();
  await waitFor(() => expect(document.documentElement.style.getPropertyValue(VAR)).toBe('58px'));
  await unmount();
  expect(document.documentElement.style.getPropertyValue(VAR)).toBe('');
  expect(document.body.style.paddingBottom).toBe('');
});
