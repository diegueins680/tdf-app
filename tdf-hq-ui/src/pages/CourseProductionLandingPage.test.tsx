import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, Route, Routes, useLocation } from 'react-router-dom';
import type { SessionUser } from '../session/SessionContext';
import type {
  CourseCheckoutResponse,
  CourseMetadata,
  CourseRegistrationRequest,
} from '../api/courses';

const getMetadataMock = jest.fn<(slug: string) => Promise<CourseMetadata>>();
const registerMock = jest.fn<
  (slug: string, payload: CourseRegistrationRequest, idempotencyKey: string) => Promise<CourseCheckoutResponse>
>();
const getCheckoutMock = jest.fn<
  (slug: string, registrationId: number, lookupToken: string) => Promise<CourseCheckoutResponse>
>();

jest.unstable_mockModule('../api/courses', () => ({
  Courses: {
    getMetadata: (slug: string) => getMetadataMock(slug),
    register: (slug: string, payload: CourseRegistrationRequest, idempotencyKey: string) =>
      registerMock(slug, payload, idempotencyKey),
    getCheckout: (slug: string, registrationId: number, lookupToken: string) =>
      getCheckoutMock(slug, registrationId, lookupToken),
  },
}));

jest.unstable_mockModule('../hooks/useCmsContent', () => ({
  useCmsContent: () => ({ data: undefined }),
}));

jest.unstable_mockModule('../components/PublicBrandBar', () => ({
  default: ({ tagline }: { tagline?: string }) => <div>{tagline}</div>,
}));

const sessionState: { current: SessionUser | null; loading: boolean } = { current: null, loading: false };

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({
    session: sessionState.current,
    loading: sessionState.loading,
    login: () => undefined,
    logout: () => undefined,
    setApiToken: () => undefined,
  }),
  getActiveSession: () => sessionState.current,
  getStoredSessionToken: () => null,
  setTransientApiToken: () => undefined,
  SESSION_STORAGE_KEY: 'tdf-hq-ui/session',
}));

const { default: CourseProductionLandingPage } = await import('./CourseProductionLandingPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const createDeferred = <T,>() => {
  let resolve!: (value: T) => void;
  let reject!: (reason?: unknown) => void;
  const promise = new Promise<T>((resolvePromise, rejectPromise) => {
    resolve = resolvePromise;
    reject = rejectPromise;
  });
  return { promise, resolve, reject };
};

const waitForExpectation = async (assertion: () => void, attempts = 14) => {
  let lastError: unknown;
  for (let index = 0; index < attempts; index += 1) {
    try {
      assertion();
      return;
    } catch (error) {
      lastError = error;
      await act(async () => {
        await flushPromises();
      });
    }
  }
  throw lastError;
};

const buildMetadata = (overrides: Partial<CourseMetadata> = {}): CourseMetadata => ({
  slug: 'bateria-guillermo-diaz-abr-2026',
  title: 'Curso de Bateria con Guillermo Diaz',
  subtitle: 'Programa presencial de 8 niveles.',
  format: 'Presencial',
  duration: '8 sesiones sabatinas (16 horas en total)',
  price: 240,
  currency: 'USD',
  capacity: 12,
  remaining: 12,
  locationLabel: 'TDF Records - Quito',
  locationMapUrl: 'https://maps.app.goo.gl/6pVYZ2CsbvQfGhAz6',
  whatsappCtaUrl: 'https://wa.me/?text=INSCRIBIRME',
  landingUrl: 'https://tdf-app.pages.dev/curso/bateria-guillermo-diaz-abr-2026',
  daws: ['Bateria acustica', 'Groove'],
  includes: ['8 sesiones presenciales con Guillermo Diaz'],
  sessions: [{ label: 'Nivel 1', date: '2026-04-25' }],
  syllabus: [
    { title: 'Nivel 1 - Fundamentos fisicos y sonido base', topics: ['Postura', 'Pulso estable'] },
    { title: 'Nivel 8 - Ensamble, performance y proyecto final', topics: ['Proyecto final'] },
  ],
  sessionStartHour: 10,
  sessionDurationHours: 2,
  instructorName: 'Guillermo Diaz',
  instructorBio: 'Baterista e instructor.',
  instructorAvatarUrl: 'https://tdf-app.pages.dev/assets/tdf-ui/guillermo-diaz-bateria.jpg',
  ...overrides,
} as CourseMetadata);

const buildLeadResponse = (overrides: Partial<CourseCheckoutResponse> = {}): CourseCheckoutResponse => ({
  registrationId: 7,
  courseSlug: 'bateria-guillermo-diaz-abr-2026',
  checkoutId: null,
  lookupToken: null,
  paymentStatus: 'not_started',
  fulfillmentStatus: 'lead_received',
  holdExpiresAt: null,
  quote: null,
  paymentMethods: [],
  checkoutAvailable: false,
  ...overrides,
});

function LocationProbe() {
  const location = useLocation();
  return <output data-testid="location-probe">{`${location.pathname}${location.search}`}</output>;
}

const renderPage = async (container: HTMLElement, initialEntry: string) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);
  const tree = () => (
    <QueryClientProvider client={qc}>
      <MemoryRouter initialEntries={[initialEntry]}>
        <Routes>
          <Route path="/curso/:slug" element={<CourseProductionLandingPage />} />
          <Route path="/curso/:slug/orden/:registrationId" element={<CourseProductionLandingPage />} />
          <Route path="/login" element={<div>Synthetic login</div>} />
        </Routes>
        <LocationProbe />
      </MemoryRouter>
    </QueryClientProvider>
  );

  await act(async () => {
    root?.render(tree());
    await flushPromises();
  });

  return {
    // Re-render after changing the mocked session state (bootstrap settling).
    rerender: async () => {
      await act(async () => {
        root?.render(tree());
        await flushPromises();
      });
    },
    cleanup: async () => {
      if (!root) return;
      await act(async () => {
        root?.unmount();
        await flushPromises();
      });
      root = null;
      qc.clear();
      document.body.removeChild(container);
    },
  };
};

const text = (element: Element | null | undefined) => (element?.textContent ?? '').replace(/\s+/g, ' ').trim();

const setInputValue = async (input: HTMLInputElement | HTMLTextAreaElement, value: string) => {
  const descriptor = Object.getOwnPropertyDescriptor(
    input instanceof HTMLTextAreaElement ? HTMLTextAreaElement.prototype : HTMLInputElement.prototype,
    'value',
  );
  await act(async () => {
    descriptor?.set?.call(input, value);
    input.dispatchEvent(new Event('input', { bubbles: true }));
    await flushPromises();
  });
};

const click = async (element: HTMLElement) => {
  await act(async () => {
    element.click();
    await flushPromises();
  });
};

const getDialog = () => document.querySelector<HTMLElement>('[role="dialog"]');

// MUI keeps the dialog mounted until its exit transition finishes.
const waitForDialogToClose = async () => {
  for (let attempt = 0; attempt < 40 && getDialog(); attempt += 1) {
    await act(async () => {
      await new Promise<void>((resolve) => setTimeout(resolve, 25));
    });
  }
  expect(getDialog()).toBeNull();
};

const fieldById = <T extends HTMLElement = HTMLInputElement>(id: string) => {
  const element = document.getElementById(id) as T | null;
  if (!element) throw new Error(`Expected #${id}`);
  return element;
};

const buttonByText = (scope: ParentNode, label: string) => {
  const button = Array.from(scope.querySelectorAll<HTMLButtonElement>('button'))
    .find((candidate) => text(candidate) === label);
  if (!button) throw new Error(`Expected button "${label}"`);
  return button;
};

const openEnrollmentFromHero = async (container: HTMLElement) => {
  await waitForExpectation(() => {
    expect(text(container)).toContain('Reserva tu cupo');
  });
  await click(buttonByText(container, 'Inscribirme'));
  await waitForExpectation(() => {
    expect(getDialog()).not.toBeNull();
  });
  const dialog = getDialog();
  if (!dialog) throw new Error('Expected enrollment dialog');
  return dialog;
};

const submitEnrollment = async () => {
  const form = getDialog()?.querySelector('form');
  if (!form) throw new Error('Enrollment form not found');
  await act(async () => {
    form.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
    await flushPromises();
  });
};

const fillGuestEnrollment = async ({
  fullName = 'Ana Torres',
  email = 'ana@example.com',
  phone,
  howHeard,
}: { fullName?: string; email?: string; phone?: string; howHeard?: string } = {}) => {
  await setInputValue(fieldById('course-enroll-fullname'), fullName);
  await setInputValue(fieldById('course-enroll-email'), email);
  if (phone !== undefined) await setInputValue(fieldById('course-enroll-phone'), phone);
  if (howHeard !== undefined) {
    await setInputValue(fieldById<HTMLTextAreaElement>('course-enroll-how-heard'), howHeard);
  }
  const terms = getDialog()?.querySelector<HTMLInputElement>('input[type="checkbox"]');
  if (!terms) throw new Error('Expected terms checkbox');
  if (!terms.checked) await click(terms);
};

const apiError = (message: string, status: number) => Object.assign(new Error(message), { status });

describe('CourseProductionLandingPage', () => {
  const scrollIntoViewMock = jest.fn();

  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
    if (!window.matchMedia) {
      Object.defineProperty(window, 'matchMedia', {
        writable: true,
        value: () => ({
          matches: false,
          media: '',
          onchange: null,
          addListener: () => undefined,
          removeListener: () => undefined,
          addEventListener: () => undefined,
          removeEventListener: () => undefined,
          dispatchEvent: () => false,
        }),
      });
    }
    Object.defineProperty(Element.prototype, 'scrollIntoView', {
      configurable: true,
      writable: true,
      value: scrollIntoViewMock,
    });
  });

  beforeEach(() => {
    getMetadataMock.mockReset();
    registerMock.mockReset();
    getCheckoutMock.mockReset();
    scrollIntoViewMock.mockReset();
    sessionState.current = null;
    sessionState.loading = false;
    window.sessionStorage.clear();
    getMetadataMock.mockResolvedValue(buildMetadata());
    registerMock.mockResolvedValue(buildLeadResponse());
    getCheckoutMock.mockResolvedValue(buildLeadResponse());
  });

  it('loads a generic course slug and renders the instructor, photo, and pensum from metadata', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      await waitForExpectation(() => {
        expect(getMetadataMock).toHaveBeenCalledWith('bateria-guillermo-diaz-abr-2026');
        expect(text(container)).toContain('Curso de Bateria con Guillermo Diaz');
        expect(text(container)).toContain('Guillermo Diaz');
        expect(text(container)).toContain('Enfoque: Bateria acustica, Groove');
        expect(text(container)).toContain('Nivel 8 - Ensamble, performance y proyecto final');
        expect(container.querySelector('img[src="https://tdf-app.pages.dev/assets/tdf-ui/guillermo-diaz-bateria.jpg"]')).not.toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  it('opens the enrollment dialog from the hero CTA with the first field focused and no scrolling', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      const dialog = await openEnrollmentFromHero(container);
      const titleId = dialog.getAttribute('aria-labelledby');
      expect(titleId).toBeTruthy();
      expect(text(document.getElementById(titleId ?? ''))).toBe(
        'Inscríbete en Curso de Bateria con Guillermo Diaz',
      );
      expect(dialog.querySelector('#course-enroll-fullname')).not.toBeNull();
      expect(dialog.querySelector('#course-enroll-email')).not.toBeNull();
      expect(dialog.querySelector('#course-enroll-phone')).not.toBeNull();
      expect(text(dialog)).toContain('Acepto los términos y la política de cancelación del curso.');
      expect(text(dialog)).not.toContain('servidor asociará');
      await waitForExpectation(() => {
        expect(document.activeElement).toBe(fieldById('course-enroll-fullname'));
      });
      expect(scrollIntoViewMock).not.toHaveBeenCalled();

      const close = dialog.querySelector<HTMLButtonElement>('button[aria-label="Cerrar"]');
      if (!close) throw new Error('Expected close button');
      await click(close);
      await waitForDialogToClose();
      // Focus returns to the CTA that opened the dialog.
      expect(document.activeElement).toBe(buttonByText(container, 'Inscribirme'));
    } finally {
      await cleanup();
    }
  });

  it('auto-opens from ?inscribirme=1 and removes the param when closed', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(
      container,
      '/curso/bateria-guillermo-diaz-abr-2026?utm_source=ig&inscribirme=1',
    );

    try {
      await waitForExpectation(() => {
        expect(getDialog()).not.toBeNull();
        expect(document.activeElement).toBe(fieldById('course-enroll-fullname'));
      });
      const close = getDialog()?.querySelector<HTMLButtonElement>('button[aria-label="Cerrar"]');
      if (!close) throw new Error('Expected close button');
      await click(close);
      await waitForExpectation(() => {
        expect(text(container.querySelector('[data-testid="location-probe"]'))).toBe(
          '/curso/bateria-guillermo-diaz-abr-2026?utm_source=ig',
        );
      });
      await waitForDialogToClose();
    } finally {
      await cleanup();
    }
  });

  it('submits public registrations to the selected generic course slug', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026?utm_source=ig');

    try {
      await openEnrollmentFromHero(container);
      await fillGuestEnrollment({ phone: '+593999001122', howHeard: 'Instagram' });
      await submitEnrollment();

      await waitForExpectation(() => {
        expect(registerMock).toHaveBeenCalledWith(
          'bateria-guillermo-diaz-abr-2026',
          {
            fullName: 'Ana Torres',
            email: 'ana@example.com',
            phoneE164: '+593999001122',
            source: 'landing',
            howHeard: 'Instagram',
            utm: { source: 'ig', medium: undefined, campaign: undefined, content: undefined },
            termsAccepted: true,
          },
          expect.stringMatching(/^course-checkout-/),
        );
      });
    } finally {
      await cleanup();
    }
  });

  it('normalizes an Ecuador local mobile number to E.164 before sending', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      await openEnrollmentFromHero(container);
      await fillGuestEnrollment({ fullName: 'Juan', email: 'juan@gmail.com', phone: '0988384849', howHeard: 'Un amigo' });
      expect(text(document.getElementById('course-enroll-phone-helper-text'))).toBe(
        'Lo enviaremos como +593988384849.',
      );
      await submitEnrollment();

      await waitForExpectation(() => {
        expect(registerMock).toHaveBeenCalledTimes(1);
      });
      expect(registerMock.mock.calls[0]?.[1]).toMatchObject({
        fullName: 'Juan',
        email: 'juan@gmail.com',
        phoneE164: '+593988384849',
        howHeard: 'Un amigo',
        termsAccepted: true,
      });
    } finally {
      await cleanup();
    }
  });

  it('blocks an unusable phone locally, explains the format and focuses the field', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      await openEnrollmentFromHero(container);
      await fillGuestEnrollment({ phone: '12345' });
      await submitEnrollment();

      const phoneInput = fieldById('course-enroll-phone');
      await waitForExpectation(() => {
        expect(phoneInput.getAttribute('aria-invalid')).toBe('true');
        expect(text(document.getElementById('course-enroll-phone-helper-text'))).toContain(
          'Usa un número como 0991234567 o +593991234567',
        );
        expect(document.activeElement).toBe(phoneInput);
      });
      expect(phoneInput.getAttribute('aria-describedby')).toContain('course-enroll-phone-helper-text');
      expect(scrollIntoViewMock).toHaveBeenCalled();
      expect(registerMock).not.toHaveBeenCalled();
    } finally {
      await cleanup();
    }
  });

  it('shows a server phone rejection next to the phone field and rotates the key only for a changed payload', async () => {
    registerMock
      .mockRejectedValueOnce(apiError('phoneE164 inválido', 400))
      .mockRejectedValueOnce(apiError('phoneE164 inválido', 400))
      .mockResolvedValueOnce(buildLeadResponse());
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      await openEnrollmentFromHero(container);
      await fillGuestEnrollment({ phone: '+447700900123' });
      await submitEnrollment();

      const phoneInput = fieldById('course-enroll-phone');
      await waitForExpectation(() => {
        expect(registerMock).toHaveBeenCalledTimes(1);
        expect(phoneInput.getAttribute('aria-invalid')).toBe('true');
        expect(text(document.getElementById('course-enroll-phone-helper-text'))).toContain(
          'Revisa tu número de WhatsApp',
        );
        expect(document.activeElement).toBe(phoneInput);
      });
      expect(text(getDialog())).not.toContain('phoneE164');
      expect(getDialog()?.querySelector('[role="alert"]')).toBeNull();

      // An identical retry keeps the same Idempotency-Key.
      await submitEnrollment();
      await waitForExpectation(() => expect(registerMock).toHaveBeenCalledTimes(2));
      expect(registerMock.mock.calls[1]?.[2]).toBe(registerMock.mock.calls[0]?.[2]);

      // A corrected payload after a definitive rejection gets a fresh key.
      await setInputValue(phoneInput, '0988384849');
      await submitEnrollment();
      await waitForExpectation(() => expect(registerMock).toHaveBeenCalledTimes(3));
      expect(registerMock.mock.calls[2]?.[1]).toMatchObject({ phoneE164: '+593988384849' });
      expect(registerMock.mock.calls[2]?.[2]).toEqual(expect.stringMatching(/^course-checkout-/));
      expect(registerMock.mock.calls[2]?.[2]).not.toBe(registerMock.mock.calls[0]?.[2]);
      await waitForExpectation(() => {
        expect(text(getDialog())).toContain('Solicitud recibida');
      });
    } finally {
      await cleanup();
    }
  });

  it('closes the dialog and moves to the order page when checkout is available', async () => {
    const heldCheckout = buildLeadResponse({
      registrationId: 41,
      lookupToken: 'synthetic-lookup-token',
      checkoutId: null,
      paymentStatus: 'pending',
      fulfillmentStatus: 'seat_held',
      checkoutAvailable: true,
    });
    registerMock.mockResolvedValueOnce(heldCheckout);
    getCheckoutMock.mockResolvedValue(heldCheckout);
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      await openEnrollmentFromHero(container);
      await fillGuestEnrollment({ phone: '0988384849' });
      await submitEnrollment();

      await waitForExpectation(() => {
        expect(text(container.querySelector('[data-testid="location-probe"]'))).toBe(
          '/curso/bateria-guillermo-diaz-abr-2026/orden/41',
        );
        expect(text(container)).toContain('Estado de tu inscripción');
        expect(text(container)).toContain('Cupo retenido temporalmente. Todavía no está pagado ni inscrito.');
      });
      await waitForDialogToClose();
      expect(text(document.body)).not.toContain('Solicitud recibida');
    } finally {
      window.localStorage.clear();
      await cleanup();
    }
  });

  it('keeps the honest lead-received state visible after a delayed submit resolves', async () => {
    const pendingRegistration = createDeferred<CourseCheckoutResponse>();
    registerMock.mockReturnValueOnce(pendingRegistration.promise);
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      await openEnrollmentFromHero(container);
      await fillGuestEnrollment();
      await submitEnrollment();

      await waitForExpectation(() => {
        expect(registerMock).toHaveBeenCalledTimes(1);
      });
      const submitButton = getDialog()?.querySelector<HTMLButtonElement>('button[type="submit"]');
      expect(submitButton?.disabled).toBe(true);
      expect(submitButton?.getAttribute('aria-busy')).toBe('true');
      expect(getDialog()?.querySelector('form')?.getAttribute('aria-busy')).toBe('true');
      // A second submit while the first is in flight is ignored.
      await submitEnrollment();
      expect(registerMock).toHaveBeenCalledTimes(1);

      await act(async () => {
        pendingRegistration.resolve(buildLeadResponse());
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(text(getDialog())).toContain('Solicitud recibida');
        expect(text(getDialog())).toContain('Te escribiremos a ana@example.com');
        expect(text(container)).toContain('Inscripción recibida');
        expect(text(container)).toContain('no está habilitado y no se reservó ni pagó un cupo');
      });
    } finally {
      await cleanup();
    }
  });

  it('does not show received, held, or paid states after the registration API fails', async () => {
    registerMock.mockRejectedValueOnce(new Error('provider unavailable'));
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      await openEnrollmentFromHero(container);
      await fillGuestEnrollment();
      await submitEnrollment();

      await waitForExpectation(() => {
        expect(text(getDialog())).toContain('No pudimos registrar tu inscripción');
        expect(text(document.body)).not.toContain('Solicitud recibida');
        expect(text(document.body)).not.toContain('Inscripción recibida');
        expect(text(document.body)).not.toContain('Cupo retenido temporalmente');
        expect(text(document.body)).not.toContain('Pago verificado');
      });
      expect(text(getDialog())).not.toContain('provider unavailable');
      // An ambiguous failure (nothing confirms the server rejected it) keeps the
      // key, so a retry cannot create a second registration.
      await setInputValue(fieldById('course-enroll-fullname'), 'Corrected synthetic name');
      registerMock.mockRejectedValueOnce(new Error('still unavailable'));
      await submitEnrollment();
      await waitForExpectation(() => expect(registerMock).toHaveBeenCalledTimes(2));
      expect(registerMock.mock.calls[0]?.[2]).toBe(registerMock.mock.calls[1]?.[2]);
    } finally {
      await cleanup();
    }
  });

  it('shows a friendly connection alert with the WhatsApp fallback on network errors', async () => {
    registerMock.mockRejectedValueOnce(
      new Error('No se pudo conectar con el servicio. Revisa tu conexión e inténtalo de nuevo.'),
    );
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      await openEnrollmentFromHero(container);
      await fillGuestEnrollment();
      await submitEnrollment();

      await waitForExpectation(() => {
        const alert = getDialog()?.querySelector('[role="alert"]');
        expect(text(alert)).toContain('No pudimos conectarnos. Revisa tu conexión a internet e intenta de nuevo.');
        expect(alert?.querySelector('a[href="https://wa.me/?text=INSCRIBIRME"]')).not.toBeNull();
      });
    } finally {
      await cleanup();
    }
  });

  const approvedTerms = {
    termsVersion: 'course-terms-v3',
    termsSummary: 'El cupo se confirma al verificar el pago.',
    cancellationPolicy: 'Reembolso total hasta 7 días antes de la primera sesión.',
  };
  const termsCheckbox = () => {
    const checkbox = getDialog()?.querySelector<HTMLInputElement>('input[type="checkbox"]');
    if (!checkbox) throw new Error('Expected terms checkbox');
    return checkbox;
  };

  it('shows the approved course terms collapsed next to the consent and sends the version that was shown', async () => {
    getMetadataMock.mockResolvedValue(buildMetadata({ checkoutTerms: approvedTerms }));
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      const dialog = await openEnrollmentFromHero(container);
      const termsButton = dialog.querySelector<HTMLButtonElement>('#course-terms-button')!;
      const cancellationButton = dialog.querySelector<HTMLButtonElement>('#course-cancellation-policy-button')!;
      expect(termsButton.getAttribute('aria-expanded')).toBe('false');
      expect(cancellationButton.getAttribute('aria-expanded')).toBe('false');
      expect(text(termsButton)).toContain('Versión course-terms-v3');
      expect(text(dialog.querySelector(`#${cancellationButton.getAttribute('aria-controls')}`)))
        .toBe('Reembolso total hasta 7 días antes de la primera sesión.');
      expect(termsCheckbox().getAttribute('aria-describedby'))
        .toBe('course-terms-button course-cancellation-policy-button');
      expect(text(dialog)).toContain('Acepto los términos del curso (versión course-terms-v3)');

      await fillGuestEnrollment();
      await submitEnrollment();
      await waitForExpectation(() => expect(registerMock).toHaveBeenCalledTimes(1));
      expect(registerMock.mock.calls[0]?.[1]).toMatchObject({
        termsAccepted: true,
        acceptedTermsVersion: 'course-terms-v3',
      });
    } finally {
      await cleanup();
    }
  });

  it('sends no terms version when the course has no approved checkout policy', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      const dialog = await openEnrollmentFromHero(container);
      expect(dialog.querySelector('#course-terms-button')).toBeNull();
      await fillGuestEnrollment();
      await submitEnrollment();
      await waitForExpectation(() => expect(registerMock).toHaveBeenCalledTimes(1));
      expect(registerMock.mock.calls[0]?.[1]).not.toHaveProperty('acceptedTermsVersion');
    } finally {
      await cleanup();
    }
  });

  it('keeps consent cleared and locked until the changed course terms arrive, then asks again', async () => {
    getMetadataMock.mockResolvedValue(buildMetadata({ checkoutTerms: approvedTerms }));
    registerMock.mockRejectedValueOnce(
      apiError('Course terms changed; review the current terms and accept them again', 409),
    );
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      await openEnrollmentFromHero(container);
      await fillGuestEnrollment();
      const refreshedMetadata = createDeferred<CourseMetadata>();
      getMetadataMock.mockReturnValue(refreshedMetadata.promise);
      await submitEnrollment();

      await waitForExpectation(() => {
        expect(text(getDialog())).toContain('Los términos del curso se actualizaron');
      });
      expect(text(getDialog())).not.toContain('Course terms changed');
      // The old version is still on screen while the new one loads: it must not be acceptable.
      expect(termsCheckbox().checked).toBe(false);
      expect(termsCheckbox().disabled).toBe(true);
      await submitEnrollment();
      expect(registerMock).toHaveBeenCalledTimes(1);

      await act(async () => {
        refreshedMetadata.resolve(buildMetadata({
          checkoutTerms: {
            termsVersion: 'course-terms-v4',
            termsSummary: 'Nuevos términos.',
            cancellationPolicy: 'Nueva política.',
          },
        }));
        await flushPromises();
      });
      await waitForExpectation(() => {
        expect(text(getDialog())).toContain('versión course-terms-v4');
        expect(termsCheckbox().disabled).toBe(false);
      });
      expect(termsCheckbox().checked).toBe(false);

      await click(termsCheckbox());
      await submitEnrollment();
      await waitForExpectation(() => expect(registerMock).toHaveBeenCalledTimes(2));
      expect(registerMock.mock.calls[1]?.[1]).toMatchObject({ acceptedTermsVersion: 'course-terms-v4' });
      expect(registerMock.mock.calls[1]?.[2]).not.toBe(registerMock.mock.calls[0]?.[2]);
    } finally {
      await cleanup();
    }
  });

  it('maps a full course conflict to a seats message instead of raw server text', async () => {
    registerMock.mockRejectedValueOnce(apiError('No course seats remain', 409));
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      await openEnrollmentFromHero(container);
      await fillGuestEnrollment();
      await submitEnrollment();

      await waitForExpectation(() => {
        expect(text(getDialog()?.querySelector('[role="alert"]'))).toContain('Ya no quedan cupos para esta fecha');
      });
      expect(text(getDialog())).not.toContain('No course seats remain');
    } finally {
      await cleanup();
    }
  });

  it('prefills the signed-in account and asks only for what is missing', async () => {
    sessionState.current = {
      username: 'ana@example.com',
      displayName: 'Ana Torres',
      roles: ['customer'],
    };
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      const dialog = await openEnrollmentFromHero(container);
      expect(text(dialog)).toContain('Usaremos los datos de tu cuenta');
      expect(text(dialog)).toContain('Ana Torres');
      expect(text(dialog)).toContain('ana@example.com');
      expect(dialog.querySelector('#course-enroll-fullname')).toBeNull();
      expect(dialog.querySelector('#course-enroll-email')).toBeNull();
      expect(text(dialog)).not.toContain('Inicia sesión para autocompletar');
      const terms = dialog.querySelector<HTMLInputElement>('input[type="checkbox"]');
      await waitForExpectation(() => {
        expect(document.activeElement).toBe(terms);
      });

      await click(buttonByText(dialog, 'Editar datos'));
      expect(fieldById('course-enroll-fullname').value).toBe('Ana Torres');
      expect(fieldById('course-enroll-email').value).toBe('ana@example.com');

      if (!terms) throw new Error('Expected terms checkbox');
      await click(terms);
      await submitEnrollment();
      await waitForExpectation(() => {
        expect(registerMock).toHaveBeenCalledTimes(1);
      });
      expect(registerMock.mock.calls[0]?.[1]).toMatchObject({
        fullName: 'Ana Torres',
        email: 'ana@example.com',
        termsAccepted: true,
      });
    } finally {
      await cleanup();
    }
  });

  it('never prefills from a cached session that bootstrap has not verified', async () => {
    // Shared browser: a previous person's expired session is still cached.
    sessionState.current = { username: 'previa@example.com', displayName: 'Persona Previa', roles: ['customer'] };
    sessionState.loading = true;
    const container = document.createElement('div');
    document.body.appendChild(container);
    const view = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      const dialog = await openEnrollmentFromHero(container);
      expect(text(dialog)).not.toContain('Persona Previa');
      expect(text(dialog)).not.toContain('previa@example.com');
      expect(fieldById('course-enroll-fullname').value).toBe('');
      expect(fieldById('course-enroll-email').value).toBe('');

      // The server rejects the cached session.
      sessionState.current = null;
      sessionState.loading = false;
      await view.rerender();
      expect(fieldById('course-enroll-fullname').value).toBe('');
      expect(fieldById('course-enroll-email').value).toBe('');
      expect(text(container.ownerDocument.body)).not.toContain('previa@example.com');
    } finally {
      await view.cleanup();
    }
  });

  it('removes account-prefilled values when that account is no longer the verified session', async () => {
    sessionState.current = { username: 'ana@example.com', displayName: 'Ana Torres', roles: ['customer'] };
    const container = document.createElement('div');
    document.body.appendChild(container);
    const view = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      const dialog = await openEnrollmentFromHero(container);
      expect(text(dialog)).toContain('Ana Torres');
      sessionState.current = null;
      await view.rerender();
      expect(text(container.ownerDocument.body)).not.toContain('ana@example.com');
      expect(fieldById('course-enroll-fullname').value).toBe('');
      expect(fieldById('course-enroll-email').value).toBe('');
    } finally {
      await view.cleanup();
    }
  });

  it('keeps campaign parameters in the login round-trip so attribution survives', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const view = await renderPage(
      container,
      '/curso/bateria-guillermo-diaz-abr-2026?utm_source=instagram&utm_medium=social&utm_campaign=tu_escena',
    );

    try {
      const dialog = await openEnrollmentFromHero(container);
      const login = Array.from(dialog.querySelectorAll<HTMLAnchorElement>('a'))
        .find((anchor) => text(anchor) === 'Inicia sesión para autocompletar');
      if (!login) throw new Error('Expected login link');
      const redirect = new URLSearchParams(login.getAttribute('href')?.split('?')[1] ?? '').get('redirect') ?? '';
      const target = new URL(redirect, 'https://tdf.local');
      expect(target.pathname).toBe('/curso/bateria-guillermo-diaz-abr-2026');
      expect(target.searchParams.get('inscribirme')).toBe('1');
      expect(target.searchParams.get('utm_source')).toBe('instagram');
      expect(target.searchParams.get('utm_medium')).toBe('social');
      expect(target.searchParams.get('utm_campaign')).toBe('tu_escena');
    } finally {
      await view.cleanup();
    }
  });

  it('lets guests sign in to autofill and restores what they typed after the round-trip', async () => {
    const first = document.createElement('div');
    document.body.appendChild(first);
    const firstRender = await renderPage(first, '/curso/bateria-guillermo-diaz-abr-2026');

    try {
      const dialog = await openEnrollmentFromHero(first);
      await setInputValue(fieldById('course-enroll-fullname'), 'Juan');
      await setInputValue(fieldById('course-enroll-phone'), '0988384849');
      const login = Array.from(dialog.querySelectorAll<HTMLAnchorElement>('a'))
        .find((anchor) => text(anchor) === 'Inicia sesión para autocompletar');
      if (!login) throw new Error('Expected login link');
      expect(login.getAttribute('href')).toBe(
        `/login?redirect=${encodeURIComponent('/curso/bateria-guillermo-diaz-abr-2026?inscribirme=1')}`,
      );
      await click(login);
      await waitForExpectation(() => {
        expect(text(first.querySelector('[data-testid="location-probe"]'))).toContain('/login?redirect=');
      });
    } finally {
      await firstRender.cleanup();
    }

    const second = document.createElement('div');
    document.body.appendChild(second);
    const secondRender = await renderPage(second, '/curso/bateria-guillermo-diaz-abr-2026?inscribirme=1');
    try {
      await waitForExpectation(() => {
        expect(getDialog()).not.toBeNull();
        expect(fieldById('course-enroll-fullname').value).toBe('Juan');
        expect(fieldById('course-enroll-phone').value).toBe('0988384849');
        // Name is filled, so focus moves to the first empty required field.
        expect(document.activeElement).toBe(fieldById('course-enroll-email'));
      });
      expect(window.sessionStorage.getItem('tdf:course-enroll-draft:bateria-guillermo-diaz-abr-2026')).toBeNull();
    } finally {
      await secondRender.cleanup();
    }
  });
});
