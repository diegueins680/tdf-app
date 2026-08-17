import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { createRoot, type Root } from 'react-dom/client';
import { act } from 'react';
import type { ComponentProps } from 'react';
import { DateTime } from 'luxon';
import { MemoryRouter } from 'react-router-dom';
import type { ServiceCatalogDTO } from '../api/types';

jest.setTimeout(15_000);

interface CreatePublicPayload {
  pbServiceOfferingId: string;
  pbResourceIds?: string[] | null;
}

interface CreatePublicCheckoutPayload {
  pbcServiceOfferingId: string;
  pbcDurationMinutes: number;
  pbcTermsAccepted: boolean;
  pbcResourceIds?: string[] | null;
}

interface PublicRoomItem {
  roomId: string;
  rName: string;
  rBookable: boolean;
}

type PublicServiceCatalogItem = ServiceCatalogDTO;

const BAND_RECORDING_ID = '11111111-1111-4111-8111-111111111111';
const DJ_PRACTICE_ID = '22222222-2222-4222-8222-222222222222';

const defaultPublicServices: PublicServiceCatalogItem[] = [
  {
    scId: BAND_RECORDING_ID,
    scCode: 'band-recording',
    scName: 'Grabación de banda',
    scNameEs: 'Grabación de banda',
    scNameEn: 'Band recording',
    scCategoryId: '33333333-3333-4333-8333-333333333333',
    scKind: 'recording',
    scPricingModelId: '44444444-4444-4444-8444-444444444444',
    scPricingModel: 'hourly',
    scRateCents: 10000,
    scCurrency: 'USD',
    scCurrencyId: '55555555-5555-4555-8555-555555555555',
    scBillingUnit: 'hour',
    scTaxRateCode: 'ec-iva-standard',
    scDefaultDurationMinutes: 120,
    scRequiresEngineer: true,
    scDefaultResources: [
      {
        sdrResourceId: '1',
        sdrResourceName: 'Live Room',
        sdrSelectionModeId: '66666666-6666-4666-8666-666666666666',
        sdrSelectionMode: 'all',
        sdrSortOrder: 10,
      },
      {
        sdrResourceId: '2',
        sdrResourceName: 'Control Room',
        sdrSelectionModeId: '66666666-6666-4666-8666-666666666666',
        sdrSelectionMode: 'all',
        sdrSortOrder: 20,
      },
    ],
    scSortOrder: 10,
    scActive: true,
  },
  {
    scId: DJ_PRACTICE_ID,
    scCode: 'dj-booth-practice',
    scName: 'Práctica en DJ Booth',
    scNameEs: 'Práctica en DJ Booth',
    scNameEn: 'DJ Booth practice',
    scCategoryId: '77777777-7777-4777-8777-777777777777',
    scKind: 'dj-practice',
    scPricingModelId: '44444444-4444-4444-8444-444444444444',
    scPricingModel: 'hourly',
    scRateCents: 5000,
    scCurrency: 'USD',
    scCurrencyId: '55555555-5555-4555-8555-555555555555',
    scBillingUnit: 'hour',
    scTaxRateCode: 'ec-iva-standard',
    scDefaultDurationMinutes: 60,
    scRequiresEngineer: false,
    scDefaultResources: [
      {
        sdrResourceId: '3',
        sdrResourceName: 'DJ Booth',
        sdrSelectionModeId: '88888888-8888-4888-8888-888888888888',
        sdrSelectionMode: 'first-available',
        sdrSortOrder: 10,
      },
    ],
    scSortOrder: 20,
    scActive: true,
  },
];

const createPublicMock = jest.fn<(payload: CreatePublicPayload) => Promise<{ bookingId: number }>>(
  () => Promise.resolve({ bookingId: 123 }),
);
const createPublicCheckoutMock = jest.fn<
  (payload: CreatePublicCheckoutPayload, idempotencyKey: string) => Promise<{
    booking: {
      bookingId: number;
      title: string;
      startsAt: string;
      endsAt: string;
      status: string;
      serviceOfferingId: string;
      serviceType: string;
      resources: [];
    };
    checkoutId: string;
    lookupToken: string;
    paymentStatus: string;
    fulfillmentStatus: string;
    holdExpiresAt: string;
    quote: {
      policyVersion: string;
      currency: string;
      durationMinutes: number;
      subtotalMinor: number;
      taxMinor: number;
      totalMinor: number;
      depositMinor: number;
      balanceMinor: number;
      depositBps: number;
      termsVersion: string;
    };
    paymentMethods: Array<'datafast' | 'paypal'>;
  }>
>(() => Promise.resolve({
  booking: {
    bookingId: 456,
    title: 'Grabacion de banda',
    startsAt: '2030-01-01T17:00:00Z',
    endsAt: '2030-01-01T19:00:00Z',
    status: 'Tentative',
    serviceOfferingId: BAND_RECORDING_ID,
    serviceType: 'Grabacion de banda',
    resources: [],
  },
  checkoutId: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
  lookupToken: 'lookup-secret',
  paymentStatus: 'awaiting_payment',
  fulfillmentStatus: 'on_hold',
  holdExpiresAt: '2030-01-01T16:15:00Z',
  quote: {
    policyVersion: 'studio-v1',
    currency: 'USD',
    durationMinutes: 120,
    subtotalMinor: 20000,
    taxMinor: 3000,
    totalMinor: 23000,
    depositMinor: 11500,
    balanceMinor: 11500,
    depositBps: 5000,
    termsVersion: 'studio-terms-v1',
  },
  paymentMethods: [],
}));
const storePublicBookingLookupTokenMock = jest.fn();
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
  loadPublicBookingLookupToken: () => 'lookup-secret',
  storePublicBookingLookupToken: storePublicBookingLookupTokenMock,
  Bookings: {
    createPublic: createPublicMock,
    createPublicCheckout: createPublicCheckoutMock,
    createPublicDatafastCheckout: jest.fn(),
    createPublicPaypalOrder: jest.fn(),
    capturePublicPaypalOrder: jest.fn(),
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
    createPublicCheckoutMock.mockClear();
    storePublicBookingLookupTokenMock.mockClear();
    listPublicServicesMock.mockReset();
    listPublicServicesMock.mockResolvedValue(defaultPublicServices);
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

  it('submits the canonical offering ID and lets the backend assign persisted default resources', async () => {
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
    expect(payload).toMatchObject({
      pbServiceOfferingId: BAND_RECORDING_ID,
      pbResourceIds: null,
    });

    await cleanup();
    document.body.removeChild(container);
  });

  it('creates an idempotent deposit checkout only from an authoritative server quote', async () => {
    globalThis.fetch = jest.fn(() =>
      Promise.resolve({
        ok: true,
        json: () => Promise.resolve({
          available: true,
          quote: {
            policyVersion: 'studio-v1',
            currency: 'USD',
            durationMinutes: 120,
            subtotalMinor: 20000,
            taxMinor: 3000,
            totalMinor: 23000,
            depositMinor: 11500,
            balanceMinor: 11500,
            depositBps: 5000,
            termsVersion: 'studio-terms-v1',
          },
        }),
      }),
    ) as unknown as typeof fetch;

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await act(async () => {
      setInputValue(getInputByLabel(container, 'Nombre completo'), 'Checkout Test');
      setInputValue(getInputByLabel(container, 'Correo'), 'checkout@example.com');
      clickButtonByText(container, 'Continuar');
      await flushPromises();
    });

    await act(async () => {
      const userZone = Intl.DateTimeFormat().resolvedOptions().timeZone ?? 'UTC';
      const desiredStudio = DateTime.fromObject(
        { year: 2030, month: 1, day: 1, hour: 12, minute: 0 },
        { zone: 'America/Guayaquil' },
      );
      setInputValue(
        getInputByLabel(container, 'Fecha y hora'),
        desiredStudio.setZone(userZone).toFormat("yyyy-MM-dd'T'HH:mm"),
      );
      setInputValue(getInputByLabel(container, 'Duración (min)'), '120');
      clickCheckboxNearText(container, 'Asignar ingeniero después');
      await flushPromises();
    });

    await act(async () => {
      clickButtonByText(container, 'Revisar reserva');
      await flushPromises();
    });

    expect(container.textContent).toContain('Precio autorizado: USD 230.00 total · depósito USD 115.00');
    const termsCheckbox = container.querySelector<HTMLInputElement>(
      'input[aria-label="Aceptar precio y política de reserva"]',
    );
    if (!termsCheckbox) throw new Error('Terms checkbox not found');

    await act(async () => {
      termsCheckbox.click();
      await flushPromises();
      submitBookingForm(container);
      await flushPromises();
    });

    expect(createPublicMock).not.toHaveBeenCalled();
    expect(createPublicCheckoutMock).toHaveBeenCalledTimes(1);
    const [payload, idempotencyKey] = createPublicCheckoutMock.mock.calls[0] ?? [];
    expect(payload).toMatchObject({
      pbcServiceOfferingId: BAND_RECORDING_ID,
      pbcDurationMinutes: 120,
      pbcTermsAccepted: true,
      pbcResourceIds: null,
    });
    expect(idempotencyKey).toMatch(/^service-booking-/);
    expect(storePublicBookingLookupTokenMock).toHaveBeenCalledWith(456, 'lookup-secret');
    expect(container.textContent).toContain('Orden creada · depósito pendiente');
    expect(container.textContent).toContain('todavía no está pagado ni confirmado');
    expect(container.textContent).not.toContain('Pago confirmado');

    await cleanup();
    document.body.removeChild(container);
  });

  it('uses the DJ Booth preset route copy and canonical service offering', async () => {
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
        pbServiceOfferingId: DJ_PRACTICE_ID,
        pbResourceIds: null,
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

  it('lets the backend resolve persisted default resources when legacy room UUIDs are ambiguous', async () => {
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
    expect(payload).toMatchObject({
      pbServiceOfferingId: BAND_RECORDING_ID,
      pbResourceIds: null,
    });

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
          scId: BAND_RECORDING_ID,
          scCode: 'band-recording',
          scName: 'Grabación de banda',
          scNameEs: 'Grabación de banda',
          scNameEn: 'Band recording',
          scCategoryId: '33333333-3333-4333-8333-333333333333',
          scKind: 'recording',
          scPricingModelId: '44444444-4444-4444-8444-444444444444',
          scPricingModel: 'hourly',
          scRateCents: 10000,
          scCurrency: 'USD',
          scCurrencyId: '55555555-5555-4555-8555-555555555555',
          scBillingUnit: 'hour',
          scTaxRateCode: 'ec-iva-standard',
          scDefaultDurationMinutes: 120,
          scRequiresEngineer: true,
          scDefaultResources: [],
          scSortOrder: 10,
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
