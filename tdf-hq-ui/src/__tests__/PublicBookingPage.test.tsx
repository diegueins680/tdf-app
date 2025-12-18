import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { createRoot, type Root } from 'react-dom/client';
import { act } from 'react';
import { DateTime } from 'luxon';

const createPublicMock = jest.fn((_: unknown) => Promise.resolve({ bookingId: 123 }));
const logoutMock = jest.fn();

jest.unstable_mockModule('../api/bookings', () => ({
  Bookings: {
    createPublic: createPublicMock,
  },
}));

jest.unstable_mockModule('../api/services', () => ({
  Services: {
    listPublic: () => Promise.resolve([]),
  },
}));

jest.unstable_mockModule('../api/rooms', () => ({
  Rooms: {
    listPublic: () => Promise.resolve([]),
  },
}));

jest.unstable_mockModule('../api/engineers', () => ({
  Engineers: {
    listPublic: () => Promise.resolve([]),
  },
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: null, logout: logoutMock }),
  getStoredSessionToken: () => null,
}));

const { default: PublicBookingPage } = await import('../pages/PublicBookingPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

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
    await flushPromises();
  });
  return {
    qc,
    cleanup: async () => {
      if (!root) return;
      await act(async () => {
        root?.unmount();
        await flushPromises();
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
  const descriptor = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value');
  if (descriptor?.set) {
    descriptor.set.call(input, value);
  } else {
    input.value = value;
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
    const checkboxes = current.querySelectorAll<HTMLInputElement>('input[type="checkbox"]');
    if (checkboxes.length === 1) {
      const checkbox = checkboxes.item(0);
      if (!checkbox) throw new Error(`Checkbox not found near: ${text}`);
      checkbox.click();
      return checkbox;
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
    globalThis.fetch = jest.fn(() =>
      Promise.resolve({
        ok: true,
        json: () => Promise.resolve({ available: true }),
      }),
    ) as unknown as typeof fetch;
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
      const userZone = Intl.DateTimeFormat().resolvedOptions().timeZone ?? 'UTC';
      const desiredStudio = DateTime.fromObject(
        { year: 2030, month: 1, day: 1, hour: 12, minute: 0 },
        { zone: 'America/Guayaquil' },
      );
      const desiredUser = desiredStudio.setZone(userZone);
      const dateInput = getInputByLabel(container, 'Fecha y hora');
      setInputValue(dateInput, desiredUser.toFormat("yyyy-MM-dd'T'HH:mm"));
      await flushPromises();
    });

    await act(async () => {
      const checkbox = clickCheckboxNearText(container, 'Asignar ingeniero después');
      expect(checkbox?.checked).toBe(true);
      await flushPromises();
    });

    await act(async () => {
      const submitButton = container.querySelector<HTMLButtonElement>('button[type="submit"]');
      if (!submitButton) throw new Error('Submit button not found');
      submitButton.click();
      await flushPromises();
    });

    expect(createPublicMock).toHaveBeenCalledTimes(1);
    const payload = createPublicMock.mock.calls[0]?.[0] as { pbResourceIds?: string[] | null } | undefined;
    expect(payload?.pbResourceIds).toEqual(['Live Room', 'Control Room']);

    await cleanup();
    document.body.removeChild(container);
  });
});
