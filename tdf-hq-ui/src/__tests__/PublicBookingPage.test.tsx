import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { createRoot, type Root } from 'react-dom/client';
import { act } from 'react';
import type { ComponentProps } from 'react';
import { DateTime } from 'luxon';
import { MemoryRouter } from 'react-router-dom';
import type { ServiceCatalogDTO } from '../api/types';

interface CreatePublicPayload {
  pbResourceIds?: string[] | null;
}

interface PublicRoomItem {
  roomId: string;
  rName: string;
  rBookable: boolean;
}

type PublicServiceCatalogItem = ServiceCatalogDTO;

const createPublicMock = jest.fn<(payload: CreatePublicPayload) => Promise<{ bookingId: number }>>(
  () => Promise.resolve({ bookingId: 123 }),
);
const logoutMock = jest.fn();
const listPublicServicesMock = jest.fn<() => Promise<PublicServiceCatalogItem[]>>(
  () => Promise.resolve([]),
);
const listPublicEngineersMock = jest.fn<() => Promise<{ peId: number; peName: string }[]>>(() => Promise.resolve([]));
const defaultPublicRooms: PublicRoomItem[] = [
  { roomId: 'room-live', rName: 'Live Room', rBookable: true },
  { roomId: 'room-control', rName: 'Control Room', rBookable: true },
  { roomId: 'room-vocal', rName: 'Vocal Booth', rBookable: true },
];
const listPublicRoomsMock = jest.fn<() => Promise<PublicRoomItem[]>>(
  () => Promise.resolve(defaultPublicRooms),
);

jest.unstable_mockModule('../api/bookings', () => ({
  Bookings: {
    createPublic: createPublicMock,
  },
}));

jest.unstable_mockModule('../api/services', () => ({
  Services: {
    listPublic: () => listPublicServicesMock(),
  },
}));

jest.unstable_mockModule('../api/rooms', () => ({
  Rooms: {
    listPublic: () => listPublicRoomsMock(),
  },
}));

jest.unstable_mockModule('../api/engineers', () => ({
  Engineers: {
    listPublic: () => listPublicEngineersMock(),
  },
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: null, logout: logoutMock }),
  getStoredSessionToken: () => null,
}));

const { default: PublicBookingPage, resolveFirstAvailableShortcut } = await import('../pages/PublicBookingPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const renderPage = async (
  container: HTMLElement,
  options: { route?: string; props?: ComponentProps<typeof PublicBookingPage> } = {},
) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);
  await act(async () => {
    root?.render(
      <MemoryRouter
        initialEntries={[options.route ?? '/reservar']}
        future={{ v7_startTransition: true, v7_relativeSplatPath: true }}
      >
        <QueryClientProvider client={qc}>
          <PublicBookingPage {...options.props} />
        </QueryClientProvider>
      </MemoryRouter>,
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

const clickButtonByText = (container: HTMLElement, label: string) => {
  const button = Array.from(container.querySelectorAll('button')).find(
    (candidate) => candidate.textContent?.trim() === label,
  );
  if (!button) throw new Error(`Button not found: ${label}`);
  button.click();
};

const submitBookingForm = (container: HTMLElement) => {
  const form = container.querySelector('form');
  if (!form) throw new Error('Form not found');
  form.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
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
    listPublicServicesMock.mockReset();
    listPublicServicesMock.mockResolvedValue([]);
    listPublicEngineersMock.mockReset();
    listPublicEngineersMock.mockResolvedValue([]);
    listPublicRoomsMock.mockReset();
    listPublicRoomsMock.mockResolvedValue(defaultPublicRooms);
    window.localStorage.clear();
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

  it('computes quick schedule shortcuts using studio-timezone opening hours', () => {
    const now = DateTime.fromISO('2030-01-01T06:30:00.000Z');

    const todayShortcut = resolveFirstAvailableShortcut({
      dayOffset: 0,
      now,
      studioTimeZone: 'America/Guayaquil',
      userTimeZone: 'Europe/Madrid',
    });
    const tomorrowShortcut = resolveFirstAvailableShortcut({
      dayOffset: 1,
      now,
      studioTimeZone: 'America/Guayaquil',
      userTimeZone: 'Europe/Madrid',
    });

    expect(todayShortcut.toFormat("yyyy-MM-dd'T'HH:mm")).toBe('2030-01-01T14:00');
    expect(tomorrowShortcut.toFormat("yyyy-MM-dd'T'HH:mm")).toBe('2030-01-02T14:00');
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
      clickButtonByText(container, 'Continuar');
      await flushPromises();
    });

    await act(async () => {
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
      clickButtonByText(container, 'Revisar reserva');
      await flushPromises();
    });

    await act(async () => {
      const submitButton = container.querySelector<HTMLButtonElement>('button[type="submit"]');
      if (!submitButton) throw new Error('Submit button not found');
      submitButton.click();
      await flushPromises();
    });

    expect(createPublicMock).toHaveBeenCalledTimes(1);
    const payload = createPublicMock.mock.calls[0]?.[0];
    expect(payload?.pbResourceIds).toEqual(['room-live', 'room-control']);

    await cleanup();
    document.body.removeChild(container);
  });

  it('uses the DJ Booth preset route copy, service label, and room assignment', async () => {
    listPublicRoomsMock.mockResolvedValueOnce([
      ...defaultPublicRooms,
      { roomId: 'room-dj', rName: 'DJ Booth', rBookable: true },
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, {
      route: '/dj-booth',
      props: { preset: 'dj-booth' },
    });

    expect(container.textContent).toContain('Reserva práctica en DJ Booth');
    expect(container.textContent).toContain('Agenda horas de práctica o alquiler del DJ Booth');

    await act(async () => {
      setInputValue(getInputByLabel(container, 'Nombre completo'), 'DJ Test');
      setInputValue(getInputByLabel(container, 'Correo'), 'dj@example.com');
      clickButtonByText(container, 'Continuar');
      await flushPromises();
    });

    expect(container.textContent).toContain('Práctica en DJ Booth');

    await act(async () => {
      const userZone = Intl.DateTimeFormat().resolvedOptions().timeZone ?? 'UTC';
      const desiredStudio = DateTime.fromObject(
        { year: 2030, month: 1, day: 1, hour: 12, minute: 0 },
        { zone: 'America/Guayaquil' },
      );
      const desiredUser = desiredStudio.setZone(userZone);
      setInputValue(getInputByLabel(container, 'Fecha y hora'), desiredUser.toFormat("yyyy-MM-dd'T'HH:mm"));
      await flushPromises();
    });

    await act(async () => {
      clickButtonByText(container, 'Revisar reserva');
      await flushPromises();
    });

    await act(async () => {
      const submitButton = container.querySelector<HTMLButtonElement>('button[type="submit"]');
      if (!submitButton) throw new Error('Submit button not found');
      submitButton.click();
      await flushPromises();
    });

    expect(createPublicMock).toHaveBeenCalledTimes(1);
    expect(createPublicMock).toHaveBeenCalledWith(
      expect.objectContaining({
        pbServiceType: 'Práctica en DJ Booth',
        pbResourceIds: ['room-dj'],
      }),
    );

    await cleanup();
    document.body.removeChild(container);
  });

  it('can preselect DJ Booth from the public service query parameter', async () => {
    listPublicRoomsMock.mockResolvedValueOnce([
      ...defaultPublicRooms,
      { roomId: 'room-dj', rName: 'DJ Booth', rBookable: true },
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, { route: '/reservar?servicio=dj-booth' });

    await act(async () => {
      setInputValue(getInputByLabel(container, 'Nombre completo'), 'DJ Query');
      setInputValue(getInputByLabel(container, 'Correo'), 'query@example.com');
      clickButtonByText(container, 'Continuar');
      await flushPromises();
    });

    expect(container.textContent).toContain('Práctica en DJ Booth');

    await cleanup();
    document.body.removeChild(container);
  });

  it('drops auto-assigned room ids when a public room label is ambiguous', async () => {
    listPublicRoomsMock.mockResolvedValueOnce([
      { roomId: 'room-live-a', rName: 'Live Room', rBookable: true },
      { roomId: 'room-live-b', rName: 'Live Room', rBookable: true },
      { roomId: 'room-control', rName: 'Control Room', rBookable: true },
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await act(async () => {
      setInputValue(getInputByLabel(container, 'Nombre completo'), 'Test User');
      setInputValue(getInputByLabel(container, 'Correo'), 'test@example.com');
      clickButtonByText(container, 'Continuar');
      await flushPromises();
    });

    await act(async () => {
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
      clickButtonByText(container, 'Revisar reserva');
      await flushPromises();
    });

    await act(async () => {
      const submitButton = container.querySelector<HTMLButtonElement>('button[type="submit"]');
      if (!submitButton) throw new Error('Submit button not found');
      submitButton.click();
      await flushPromises();
    });

    expect(createPublicMock).toHaveBeenCalledTimes(1);
    const payload = createPublicMock.mock.calls[0]?.[0];
    expect(payload?.pbResourceIds).toBeNull();

    await cleanup();
    document.body.removeChild(container);
  });

  it('prevents moving to schedule step with invalid email', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await act(async () => {
      setInputValue(getInputByLabel(container, 'Nombre completo'), 'Test User');
      setInputValue(getInputByLabel(container, 'Correo'), 'correo-invalido');
      clickButtonByText(container, 'Continuar');
      await flushPromises();
    });

    expect(container.textContent).toContain('Ingresa un correo válido para enviarte la confirmación.');
    const dateLabel = Array.from(container.querySelectorAll('label')).find(
      (label) => (label.textContent ?? '').replace('*', '').trim() === 'Fecha y hora',
    );
    expect(dateLabel).toBeUndefined();
    expect(createPublicMock).not.toHaveBeenCalled();

    await cleanup();
    document.body.removeChild(container);
  });

  it('does not overwrite in-progress contact edits after services load', async () => {
    let resolveServices: ((value: PublicServiceCatalogItem[]) => void) | null = null;
    listPublicServicesMock.mockReturnValueOnce(
      new Promise<PublicServiceCatalogItem[]>((resolve) => {
        resolveServices = resolve;
      }),
    );
    window.localStorage.setItem(
      'tdf-public-booking-profile',
      JSON.stringify({
        fullName: 'Nombre Guardado',
        email: 'guardado@tdf.com',
        phone: '+593000000000',
        serviceType: 'Grabación de banda',
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    const fullNameInput = getInputByLabel(container, 'Nombre completo');
    expect(fullNameInput.value).toBe('Nombre Guardado');

    await act(async () => {
      setInputValue(fullNameInput, 'Nombre Editado');
      await flushPromises();
    });
    expect(fullNameInput.value).toBe('Nombre Editado');

    await act(async () => {
      if (!resolveServices) throw new Error('Service resolver was not initialized');
      resolveServices([
        {
          scId: 91,
          scName: 'Grabación de banda',
          scKind: 'Recording',
          scPricingModel: 'Hourly',
          scRateCents: 10000,
          scCurrency: 'USD',
          scBillingUnit: 'hora',
          scTaxBps: 1200,
          scActive: true,
        },
      ]);
      await flushPromises();
    });

    expect(getInputByLabel(container, 'Nombre completo').value).toBe('Nombre Editado');

    await cleanup();
    document.body.removeChild(container);
  });

  it('advances by step on form submit before sending booking', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await act(async () => {
      setInputValue(getInputByLabel(container, 'Nombre completo'), 'Test User');
      setInputValue(getInputByLabel(container, 'Correo'), 'test@example.com');
      submitBookingForm(container);
      await flushPromises();
    });
    expect(getInputByLabel(container, 'Fecha y hora')).toBeInstanceOf(HTMLInputElement);
    expect(createPublicMock).not.toHaveBeenCalled();

    await act(async () => {
      const userZone = Intl.DateTimeFormat().resolvedOptions().timeZone ?? 'UTC';
      const desiredStudio = DateTime.fromObject(
        { year: 2030, month: 1, day: 1, hour: 12, minute: 0 },
        { zone: 'America/Guayaquil' },
      );
      const desiredUser = desiredStudio.setZone(userZone);
      const dateInput = getInputByLabel(container, 'Fecha y hora');
      setInputValue(dateInput, desiredUser.toFormat("yyyy-MM-dd'T'HH:mm"));
      clickCheckboxNearText(container, 'Asignar ingeniero después');
      submitBookingForm(container);
      await flushPromises();
    });
    expect(container.textContent).toContain('Resumen rápido');
    expect(createPublicMock).not.toHaveBeenCalled();

    await act(async () => {
      submitBookingForm(container);
      await flushPromises();
    });
    expect(createPublicMock).toHaveBeenCalledTimes(1);

    await cleanup();
    document.body.removeChild(container);
  });

  it('does not auto-bind an ambiguous engineer id when duplicate names exist', async () => {
    const consoleErrorSpy = jest.spyOn(console, 'error').mockImplementation(() => undefined);
    listPublicEngineersMock.mockResolvedValueOnce([
      { peId: 7, peName: 'Ana' },
      { peId: 9, peName: 'Ana' },
    ]);

    try {
      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await act(async () => {
        setInputValue(getInputByLabel(container, 'Nombre completo'), 'Test User');
        setInputValue(getInputByLabel(container, 'Correo'), 'test@example.com');
        clickButtonByText(container, 'Continuar');
        await flushPromises();
      });

      await act(async () => {
        const userZone = Intl.DateTimeFormat().resolvedOptions().timeZone ?? 'UTC';
        const desiredStudio = DateTime.fromObject(
          { year: 2030, month: 1, day: 1, hour: 12, minute: 0 },
          { zone: 'America/Guayaquil' },
        );
        const desiredUser = desiredStudio.setZone(userZone);
        setInputValue(getInputByLabel(container, 'Fecha y hora'), desiredUser.toFormat("yyyy-MM-dd'T'HH:mm"));
        setInputValue(getInputByLabel(container, 'Ingeniero asignado'), 'Ana');
        await flushPromises();
      });

      await act(async () => {
        clickButtonByText(container, 'Revisar reserva');
        await flushPromises();
      });

      await act(async () => {
        const submitButton = container.querySelector<HTMLButtonElement>('button[type="submit"]');
        if (!submitButton) throw new Error('Submit button not found');
        submitButton.click();
        await flushPromises();
      });

      expect(createPublicMock).toHaveBeenCalledTimes(1);
      expect(createPublicMock).toHaveBeenCalledWith(
        expect.objectContaining({
          pbEngineerPartyId: null,
          pbEngineerName: 'Ana',
        }),
      );
      expect(
        consoleErrorSpy.mock.calls.some(([message]) =>
          String(message).includes('Encountered two children with the same key'),
        ),
      ).toBe(false);

      await cleanup();
      document.body.removeChild(container);
    } finally {
      consoleErrorSpy.mockRestore();
    }
  });
});
