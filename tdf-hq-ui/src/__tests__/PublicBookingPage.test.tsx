import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { createRoot, type Root } from 'react-dom/client';
import { act } from 'react';

const createPublicMock = jest.fn(async (_payload: unknown) => ({ bookingId: 123 }));

jest.unstable_mockModule('../api/bookings', () => ({
  Bookings: {
    createPublic: createPublicMock,
  },
}));

jest.unstable_mockModule('../api/services', () => ({
  Services: {
    listPublic: async () => [],
  },
}));

jest.unstable_mockModule('../api/rooms', () => ({
  Rooms: {
    listPublic: async () => [],
  },
}));

jest.unstable_mockModule('../api/engineers', () => ({
  Engineers: {
    listPublic: async () => [],
  },
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: null, logout: () => {} }),
  getStoredSessionToken: () => null,
}));

const { default: PublicBookingPage } = await import('../pages/PublicBookingPage');

const renderPage = async (container: HTMLElement) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);
  await act(async () => {
    root?.render(
      <QueryClientProvider client={qc}>
        <PublicBookingPage />
      </QueryClientProvider>,
    );
  });
  return {
    qc,
    cleanup: async () => {
      if (!root) return;
      await act(async () => {
        root?.unmount();
      });
      root = null;
    },
  };
};

const getInputByLabel = (container: HTMLElement, labelText: string) => {
  const labels = Array.from(container.querySelectorAll('label'));
  const label = labels.find((el) => {
    const text = (el.textContent ?? '').replace('*', '').trim();
    return text === labelText;
  });
  if (!label) throw new Error(`Label not found: ${labelText}`);
  const forId = label.getAttribute('for');
  if (forId) {
    const input = document.getElementById(forId);
    if (input && input instanceof HTMLInputElement) return input;
  }
  const fallback = label.parentElement?.querySelector<HTMLInputElement>('input,textarea');
  if (!fallback) throw new Error(`Input not found for label: ${labelText}`);
  return fallback;
};

const setInputValue = (input: HTMLInputElement, value: string) => {
  const setter = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value')?.set;
  if (!setter) {
    input.value = value;
  } else {
    setter.call(input, value);
  }
  input.dispatchEvent(new Event('input', { bubbles: true }));
  input.dispatchEvent(new Event('change', { bubbles: true }));
};

const clickCheckboxNearText = (container: HTMLElement, text: string) => {
  const textEl = Array.from(container.querySelectorAll<HTMLElement>('*')).find((el) => {
    if (el.children.length > 0) return false;
    return (el.textContent ?? '').trim() === text;
  });
  if (!textEl) throw new Error(`Text not found: ${text}`);
  let current: HTMLElement | null = textEl;
  while (current && current !== container) {
    const checkbox = current.querySelector<HTMLInputElement>('input[type="checkbox"]');
    if (checkbox) {
      checkbox.click();
      return;
    }
    current = current.parentElement;
  }
  throw new Error(`Checkbox not found near: ${text}`);
};

describe('PublicBookingPage', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    createPublicMock.mockClear();
    globalThis.fetch = jest.fn(async () => ({
      ok: true,
      json: async () => ({ available: true }),
    })) as unknown as typeof fetch;
  });

  it('hides the rooms field from the public form', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    expect(container.textContent).not.toContain('Salas asignadas');

    await cleanup();
    document.body.removeChild(container);
  });

  it('submits auto-assigned rooms based on service rules', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    const { Bookings } = await import('../api/bookings');
    expect(Bookings.createPublic).toBe(createPublicMock);

    await act(async () => {
      setInputValue(getInputByLabel(container, 'Nombre completo'), 'Test User');
      setInputValue(getInputByLabel(container, 'Correo'), 'test@example.com');
      setInputValue(getInputByLabel(container, 'Fecha y hora'), '2030-01-01T18:00');
    });

    await act(async () => {
      clickCheckboxNearText(container, 'Asignar ingeniero después');
    });

    await act(async () => {
      const submitButton = container.querySelector<HTMLButtonElement>('button[type="submit"]');
      if (!submitButton) throw new Error('Submit button not found');
      submitButton.click();
    });

    expect(createPublicMock).toHaveBeenCalledTimes(1);
    const payload = createPublicMock.mock.calls[0]?.[0] as { pbResourceIds?: string[] | null } | undefined;
    expect(payload?.pbResourceIds).toEqual(['Live Room', 'Control Room']);

    await cleanup();
    document.body.removeChild(container);
  });
});
