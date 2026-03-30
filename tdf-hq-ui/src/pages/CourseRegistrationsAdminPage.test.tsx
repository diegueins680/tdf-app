import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type {
  CourseCohortOptionDTO,
  CourseEmailEventDTO,
  CourseRegistrationDTO,
  CourseRegistrationDossierDTO,
  CourseRegistrationFollowUpDTO,
  CourseRegistrationReceiptDTO,
} from '../api/courses';
import { formatTimestampForDisplay } from '../utils/dateTime';

const listCohortsMock = jest.fn<() => Promise<CourseCohortOptionDTO[]>>();
const listRegistrationsMock = jest.fn<
  (params?: { slug?: string; status?: string; limit?: number }) => Promise<CourseRegistrationDTO[]>
>();
const getRegistrationDossierMock = jest.fn<
  (slug: string, registrationId: number) => Promise<CourseRegistrationDossierDTO | null>
>();
const listRegistrationEmailsMock = jest.fn<
  (registrationId: number, limit?: number) => Promise<CourseEmailEventDTO[]>
>();

jest.unstable_mockModule('../api/courses', () => ({
  Courses: {
    listCohorts: () => listCohortsMock(),
    listRegistrations: (params?: { slug?: string; status?: string; limit?: number }) => listRegistrationsMock(params),
    getRegistrationDossier: (slug: string, registrationId: number) =>
      getRegistrationDossierMock(slug, registrationId),
    listRegistrationEmails: (registrationId: number, limit?: number) =>
      listRegistrationEmailsMock(registrationId, limit),
    updateStatus: jest.fn(() => Promise.resolve(null)),
    updateRegistrationNotes: jest.fn(() => Promise.resolve(null)),
    createReceipt: jest.fn(() => Promise.resolve(null)),
    updateReceipt: jest.fn(() => Promise.resolve(null)),
    deleteReceipt: jest.fn(() => Promise.resolve(null)),
    createFollowUp: jest.fn(() => Promise.resolve(null)),
    updateFollowUp: jest.fn(() => Promise.resolve(null)),
    deleteFollowUp: jest.fn(() => Promise.resolve(null)),
  },
}));

jest.unstable_mockModule('../components/GoogleDriveUploadWidget', () => ({
  default: () => null,
}));

const { default: CourseRegistrationsAdminPage } = await import('./CourseRegistrationsAdminPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const waitForExpectation = async (assertion: () => void, attempts = 12) => {
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

const buildRegistration = (overrides: Partial<CourseRegistrationDTO> = {}): CourseRegistrationDTO => ({
  crId: 101,
  crCourseSlug: 'beatmaking-101',
  crPartyId: 9,
  crFullName: 'Ada Lovelace',
  crEmail: 'ada@example.com',
  crPhoneE164: '+593999000111',
  crSource: 'landing',
  crStatus: 'pending_payment',
  crAdminNotes: null,
  crHowHeard: null,
  crUtmSource: null,
  crUtmMedium: null,
  crUtmCampaign: null,
  crUtmContent: null,
  crCreatedAt: '2030-01-02T03:04:05.000Z',
  crUpdatedAt: '2030-01-02T03:04:05.000Z',
  ...overrides,
});

const buildRegistrations = (
  count: number,
  overrides: (index: number) => Partial<CourseRegistrationDTO> = () => ({}),
): CourseRegistrationDTO[] => (
  Array.from({ length: count }, (_, index) => buildRegistration({
    crId: 101 + index,
    crPartyId: 9 + index,
    crFullName: `Estudiante ${index + 1}`,
    crEmail: `student${index + 1}@example.com`,
    ...overrides(index),
  }))
);

const buildDossier = (
  overrides: Partial<CourseRegistrationDossierDTO> = {},
): CourseRegistrationDossierDTO => ({
  crdRegistration: buildRegistration(),
  crdReceipts: [],
  crdFollowUps: [],
  crdCanMarkPaid: false,
  ...overrides,
});

const buildReceipt = (
  overrides: Partial<CourseRegistrationReceiptDTO> = {},
): CourseRegistrationReceiptDTO => ({
  crrId: 301,
  crrRegistrationId: 101,
  crrPartyId: 9,
  crrFileUrl: 'https://example.com/receipt.pdf',
  crrFileName: 'receipt.pdf',
  crrMimeType: 'application/pdf',
  crrNotes: null,
  crrUploadedBy: 4,
  crrCreatedAt: '2030-01-02T03:04:05.000Z',
  crrUpdatedAt: '2030-01-02T03:04:05.000Z',
  ...overrides,
});

const buildFollowUp = (
  overrides: Partial<CourseRegistrationFollowUpDTO> = {},
): CourseRegistrationFollowUpDTO => ({
  crfId: 401,
  crfRegistrationId: 101,
  crfPartyId: 9,
  crfEntryType: 'call',
  crfSubject: 'Confirmó transferencia',
  crfNotes: 'Dijo que enviará el comprobante hoy.',
  crfAttachmentUrl: null,
  crfAttachmentName: null,
  crfNextFollowUpAt: null,
  crfCreatedBy: 4,
  crfCreatedAt: '2030-01-04T03:04:05.000Z',
  crfUpdatedAt: '2030-01-04T03:04:05.000Z',
  ...overrides,
});

const buildEmailEvent = (
  overrides: Partial<CourseEmailEventDTO> = {},
): CourseEmailEventDTO => ({
  ceId: 501,
  ceCourseSlug: 'beatmaking-101',
  ceRegistrationId: 101,
  ceRecipientEmail: 'ada@example.com',
  ceRecipientName: 'Ada Lovelace',
  ceEventType: 'payment_reminder',
  ceStatus: 'sent',
  ceMessage: 'Recordatorio de pago enviado.',
  ceCreatedAt: '2030-01-03T03:04:05.000Z',
  ...overrides,
});

const emptyReceiptAlertMessage =
  'Agrega el primer comprobante para documentar el pago y habilitar Marcar pagado. Cuando lo guardes aparecerá aquí con enlace y acciones para revisarlo después.';
const showSystemEmailsLabel = 'Ver correos del sistema';
const hideSystemEmailsLabel = 'Ocultar correos del sistema';
const systemEmailHistoryHelperText =
  'Historial persistente de correos del sistema para esta inscripción. Usa el refresco del expediente para volver a consultarlo.';
const emptySystemEmailHistoryMessage =
  'Todavía no hay correos del sistema registrados para esta inscripción. Cuando se envíe el primero, aparecerá aquí.';
const emptyFollowUpAlertMessage =
  'Aún no hay seguimiento manual. Documenta llamadas, mensajes o próximos pasos desde aquí. Los cambios de estado y los comprobantes nuevos también quedarán registrados aquí.';
const followUpComposerHelpText =
  'Abre el formulario solo cuando necesites documentar una llamada, mensaje o próximo paso.';
const openPaymentDossierLabel = 'Abrir expediente de pago';

const renderPage = async (container: HTMLElement, initialEntry = '/inscripciones-curso') => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter
        initialEntries={[initialEntry]}
        future={{ v7_startTransition: true, v7_relativeSplatPath: true }}
      >
        <QueryClientProvider client={qc}>
          <CourseRegistrationsAdminPage />
        </QueryClientProvider>
      </MemoryRouter>,
    );
    await flushPromises();
    await flushPromises();
  });

  return {
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
    if (input instanceof HTMLInputElement || input instanceof HTMLTextAreaElement) return input;
  }
  const fallback = label.parentElement?.querySelector<HTMLInputElement | HTMLTextAreaElement>('input,textarea');
  if (!fallback) throw new Error(`Input not found for label: ${labelText}`);
  return fallback;
};

const hasLabel = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('label')).some((el) => {
    const text = (el.textContent ?? '').replace('*', '').trim();
    return text === labelText;
  });

const setInputValue = (input: HTMLInputElement | HTMLTextAreaElement, value: string) => {
  const prototype = input instanceof HTMLTextAreaElement ? HTMLTextAreaElement.prototype : HTMLInputElement.prototype;
  const descriptor = Object.getOwnPropertyDescriptor(prototype, 'value');
  if (descriptor?.set) {
    descriptor.set.call(input, value);
  } else {
    input.value = value;
  }
  input.dispatchEvent(new Event('input', { bubbles: true }));
  input.dispatchEvent(new Event('change', { bubbles: true }));
};

const getButtonByText = (root: ParentNode, labelText: string) => {
  const buttons = Array.from(root.querySelectorAll('button'));
  const button = buttons.find((el) => (el.textContent ?? '').trim() === labelText);
  if (!(button instanceof HTMLButtonElement)) {
    throw new Error(`Button not found: ${labelText}`);
  }
  return button;
};

const getButtonByAriaLabel = (root: ParentNode, labelText: string) => {
  const button = root.querySelector(`button[aria-label="${labelText}"]`);
  if (!(button instanceof HTMLButtonElement)) {
    throw new Error(`Button not found: ${labelText}`);
  }
  return button;
};

const getMenuItemByText = (root: ParentNode, labelText: string) => {
  const items = Array.from(root.querySelectorAll('[role="menuitem"]'));
  const item = items.find((el) => (el.textContent ?? '').trim() === labelText);
  if (!(item instanceof HTMLElement)) {
    throw new Error(`Menu item not found: ${labelText}`);
  }
  return item;
};

const getDialog = () => {
  const dialog = document.body.querySelector('[role="dialog"]');
  if (!(dialog instanceof HTMLElement)) {
    throw new Error('Dialog not found');
  }
  return dialog;
};

const countOccurrences = (root: ParentNode, text: string) =>
  (root.textContent ?? '').split(text).length - 1;

const hasExactText = (root: ParentNode, text: string) =>
  Array.from(root.querySelectorAll('*')).some((el) => (el.textContent ?? '').trim() === text);

const countButtonsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('button')).filter((el) => (el.textContent ?? '').trim() === labelText).length;

const clickElement = (element: Element) => {
  element.dispatchEvent(new MouseEvent('click', { bubbles: true }));
};

const clickButton = (button: HTMLButtonElement) => {
  clickElement(button);
};

describe('CourseRegistrationsAdminPage', () => {
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
  });

  beforeEach(() => {
    listCohortsMock.mockReset();
    listRegistrationsMock.mockReset();
    getRegistrationDossierMock.mockReset();
    listRegistrationEmailsMock.mockReset();
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([buildRegistration()]);
    getRegistrationDossierMock.mockResolvedValue(null);
    listRegistrationEmailsMock.mockResolvedValue([]);
  });

  it('consolidates row details into the dossier without extra list chrome', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).not.toContain(
        'Los filtros se aplican automáticamente al cambiar. Empieza por cohorte y estado; Ajustar límite aparecerá cuando esta vista llene el lote actual o si ya estás usando un límite personalizado.',
      );
      expect(container.textContent).not.toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).not.toContain('Slug: beatmaking-101');
      expect(container.textContent).not.toContain('Aplicar filtros');
      expect(container.textContent).not.toContain(
        'Abre expediente para ver notas, comprobantes y seguimiento. Usa el estado solo para cambios rapidos.',
      );
      expect(container.textContent).toContain(
        'Expediente reúne notas, comprobantes, seguimiento y correos. Usa Estado solo para cambios rápidos.',
      );
      expect(countOccurrences(
        container,
        'Expediente reúne notas, comprobantes, seguimiento y correos. Usa Estado solo para cambios rápidos.',
      )).toBe(1);
      expect(container.textContent).not.toContain('Abrir expediente');
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace').textContent?.trim()).toBe(
        'Expediente',
      );
      expect(container.textContent).not.toContain('Cambiar estado:');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe(
        'Cambiar estado',
      );
      expect(countOccurrences(container, 'Pendiente de pago')).toBe(1);
      expect(container.textContent).not.toContain(showSystemEmailsLabel);
      expect(hasLabel(container, 'Límite')).toBe(false);
      expect(countButtonsByText(container, 'Ajustar límite')).toBe(0);
      expect(
        Array.from(container.querySelectorAll('button')).some((el) => {
          const label = (el.textContent ?? '').trim();
          return label.startsWith('Copiar CSV');
        }),
      ).toBe(false);
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: undefined,
        limit: 200,
      });
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Expediente de inscripción');
      expect(document.body.textContent).toContain(emptySystemEmailHistoryMessage);
      expect(document.body.textContent).not.toContain(showSystemEmailsLabel);
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(emptyReceiptAlertMessage);
      expect(document.body.textContent).not.toContain(
        'Todavía no hay comprobantes. Agrega el primero para documentar el pago y habilitar Marcar pagado.',
      );
      expect(document.body.textContent).not.toContain(
        'Sube un comprobante o pega una URL existente para habilitar Marcar pagado.',
      );
      expect(document.body.textContent).not.toContain('0 guardados');
      expect(getButtonByText(document.body, 'Agregar primer comprobante')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(1);
      expect(document.body.textContent).toContain(emptyFollowUpAlertMessage);
      expect(document.body.textContent).not.toContain('0 entradas');
      expect(document.body.textContent).not.toContain('Registrar seguimiento');
      expect(document.body.textContent).not.toContain(followUpComposerHelpText);
      expect(hasLabel(document.body, 'Nombre visible')).toBe(false);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(false);
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(false);
      expect(hasLabel(document.body, 'URL del adjunto')).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar primer comprobante'));
      clickButton(getButtonByText(document.body, 'Registrar primer seguimiento'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Guardar comprobante')).toBeTruthy();
      expect(getButtonByText(document.body, 'Guardar comprobante').disabled).toBe(true);
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir archivo')).toBeTruthy();
      expect(getButtonByText(document.body, 'Agregar detalles opcionales')).toBeTruthy();
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Usar enlace existente en lugar de subir adjunto',
        ),
      ).toBe(false);
      expect(getButtonByText(document.body, 'Cancelar comprobante')).toBeTruthy();
      expect(getButtonByText(document.body, 'Cancelar seguimiento')).toBeTruthy();
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Cancelar',
        ),
      ).toBe(false);
      expect(document.body.textContent).toContain('Registrar seguimiento');
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(document.body.textContent).toContain(followUpComposerHelpText);
      expect(document.body.textContent).toContain(
        'Primero elige el archivo o pega un enlace; luego podras ajustar el nombre visible y las notas.',
      );
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(0);
      expect(hasLabel(document.body, 'Nombre visible')).toBe(false);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(false);
      expect(hasLabel(document.body, 'Asunto')).toBe(false);
      expect(hasLabel(document.body, 'Próximo seguimiento')).toBe(false);
      expect(document.body.textContent).toContain('Agrega tipo, asunto, recordatorio o evidencia solo si hacen falta.');
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Usar enlace existente en lugar de subir archivo'));
      clickButton(getButtonByText(document.body, 'Agregar detalles opcionales'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir adjunto')).toBeTruthy();
      expect(hasLabel(document.body, 'Asunto')).toBe(true);
      expect(hasLabel(document.body, 'Próximo seguimiento')).toBe(true);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Usar enlace existente en lugar de subir adjunto'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'Nombre visible')).toBe(true);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(true);
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(true);
      expect(hasLabel(document.body, 'URL del adjunto')).toBe(true);
    });

    await act(async () => {
      setInputValue(getInputByLabel(document.body, 'URL del comprobante'), 'https://example.com/new-receipt.pdf');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Guardar comprobante').disabled).toBe(false);
    });

    await cleanup();
  });

  it('explains the dossier once while keeping each condensed row action self-explanatory', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countOccurrences(
        container,
        'Expediente reúne notas, comprobantes, seguimiento y correos. Usa Estado solo para cambios rápidos.',
      )).toBe(1);
      expect(countButtonsByText(container, 'Expediente')).toBe(2);
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(2);
      expect(countButtonsByText(container, 'Cambiar')).toBe(0);
      expect(countButtonsByText(container, 'Estado')).toBe(0);
      expect(countOccurrences(container, 'Pendiente de pago')).toBe(1);
      expect(container.querySelectorAll('button[aria-label^="Abrir expediente de "]')).toHaveLength(2);
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(2);
      expect(container.textContent).not.toContain('Abrir expediente');
      expect(container.textContent).not.toContain('Estado: Pendiente de pago');
    });

    await cleanup();
  });

  it('reveals the limit toggle only when the current batch reaches its cap or a custom limit is active', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Límite')).toBe(false);
      expect(countButtonsByText(container, 'Ajustar límite')).toBe(0);
      expect(container.textContent).not.toContain(
        'Los filtros se aplican automáticamente al cambiar. Empieza por cohorte y estado; Ajustar límite aparecerá cuando esta vista llene el lote actual o si ya estás usando un límite personalizado.',
      );
      expect(container.textContent).not.toContain('Límite activo:');
    });

    await cleanup();

    const cappedRegistrations = buildRegistrations(200);
    listRegistrationsMock.mockImplementation((params) => Promise.resolve(cappedRegistrations.slice(0, params?.limit ?? 200)));

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    await waitForExpectation(() => {
      expect(hasLabel(secondContainer, 'Límite')).toBe(false);
      expect(getButtonByText(secondContainer, 'Ajustar límite')).toBeTruthy();
      expect(secondContainer.textContent).toContain(
        'Esta vista ya está acotada a una cohorte y un estado. Usa Ajustar límite solo cuando necesites revisar un lote distinto.',
      );
    });

    await act(async () => {
      clickButton(getButtonByText(secondContainer, 'Ajustar límite'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(secondContainer, 'Límite')).toBe(true);
      expect(getButtonByText(secondContainer, 'Ocultar límite')).toBeTruthy();
      expect(secondContainer.textContent).toContain(
        'Máximo de filas a cargar en esta vista. Déjalo en 200 salvo que necesites revisar un lote distinto.',
      );
    });

    await secondRender.cleanup();

    listRegistrationsMock.mockClear();

    const thirdContainer = document.createElement('div');
    document.body.appendChild(thirdContainer);
    const thirdRender = await renderPage(thirdContainer, '/inscripciones-curso?limit=50');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: undefined,
        limit: 50,
      });
      expect(hasLabel(thirdContainer, 'Límite')).toBe(false);
      expect(getButtonByText(thirdContainer, 'Ajustar límite (50)')).toBeTruthy();
      expect(thirdContainer.textContent).toContain('Límite actual: hasta 50 inscripciones.');
      expect(thirdContainer.textContent).not.toContain('Vista filtrada: límite 50.');
      expect(thirdContainer.textContent).not.toContain('Límite activo: 50');
      expect(getButtonByText(thirdContainer, 'Restablecer límite')).toBeTruthy();
      expect(
        Array.from(thirdContainer.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Restablecer filtros',
        ),
      ).toBe(false);
    });

    await thirdRender.cleanup();
  });

  it('shows the selected cohort once in the filtered summary instead of repeating it on each row', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?slug=beatmaking-101');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: 'beatmaking-101',
        status: undefined,
        limit: 200,
      });
      expect(container.textContent).toContain('Vista actual');
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101) · Pendiente de pago');
      expect(container.textContent).not.toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).not.toContain('Slug: beatmaking-101');
      expect(container.textContent).toContain('Fuente: landing');
    });

    await cleanup();
  });

  it('condenses each registration contact line into one scan-friendly summary', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crPhoneE164: null,
      }),
      buildRegistration({
        crId: 103,
        crFullName: 'Katherine Johnson',
        crEmail: null,
        crPhoneE164: '+593999000222',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasExactText(container, 'ada@example.com · +593999000111')).toBe(true);
      expect(hasExactText(container, '+593999000111')).toBe(false);
      expect(hasExactText(container, 'grace@example.com')).toBe(true);
      expect(hasExactText(container, '+593999000222')).toBe(true);
      expect(hasExactText(container, 'Sin correo · +593999000222')).toBe(false);
    });

    await cleanup();
  });

  it('uses the best available contact as the visible identity when the registration name is blank', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crFullName: '   ',
        crEmail: 'sin-nombre@example.com',
        crPhoneE164: '+593999000777',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasExactText(container, 'sin-nombre@example.com')).toBe(true);
      expect(hasExactText(container, '+593999000777')).toBe(true);
      expect(countOccurrences(container, 'sin-nombre@example.com')).toBe(1);
      expect(container.textContent).not.toContain('Sin nombre');
      expect(getButtonByAriaLabel(container, 'Abrir expediente de sin-nombre@example.com')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Cambiar estado para sin-nombre@example.com')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Abrir expediente de sin-nombre@example.com'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const dialog = document.body.querySelector<HTMLElement>('[role="dialog"]');
      expect(dialog?.textContent).toContain('sin-nombre@example.com');
      expect(dialog?.textContent).toContain('+593999000777');
      expect(dialog?.textContent).not.toContain('Sin nombre');
    });

    await cleanup();
  });

  it('condenses row cohort, source, and created metadata into one summary line', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crCourseSlug: 'mixing-bootcamp',
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crSource: 'referral',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasExactText(
        container,
        `Cohorte: Beatmaking 101 (beatmaking-101) · Fuente: landing · Creado: ${formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-')}`,
      )).toBe(true);
      expect(hasExactText(
        container,
        `Cohorte: Mixing Bootcamp (mixing-bootcamp) · Fuente: referral · Creado: ${formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-')}`,
      )).toBe(true);
    });

    await cleanup();
  });

  it('folds real admin-note signals into the existing row summary instead of rendering a separate chip', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crAdminNotes: 'Confirmó pago por WhatsApp.' }),
      buildRegistration({
        crId: 102,
        crCourseSlug: 'mixing-bootcamp',
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crAdminNotes: '   ',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasExactText(
        container,
        `Cohorte: Beatmaking 101 (beatmaking-101) · Creado: ${formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-')} · Notas internas`,
      )).toBe(true);
      expect(hasExactText(
        container,
        `Cohorte: Mixing Bootcamp (mixing-bootcamp) · Creado: ${formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-')}`,
      )).toBe(true);
      expect(container.textContent).not.toContain('Con notas');
      expect(countOccurrences(container, 'Notas internas')).toBe(1);
    });

    await cleanup();
  });

  it('replaces a single cohort selector with context copy and restores it when multiple cohorts exist', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'paid',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.textContent).toContain('Cohorte disponible');
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).toContain(
        'No hace falta filtrarla: es la unica cohorte disponible ahora mismo. Usa Estado para cambiar la vista.',
      );
      expect(container.textContent).not.toContain('Los filtros se aplican automáticamente al cambiar.');
      expect(container.textContent).not.toContain('Empieza por cohorte y estado.');
      expect(container.textContent).not.toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Mostrando una sola cohorte: Beatmaking 101 (beatmaking-101).');
    });

    await cleanup();

    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    await waitForExpectation(() => {
      expect(hasLabel(secondContainer, 'Curso / cohorte')).toBe(true);
      expect(secondContainer.textContent).not.toContain('No hace falta filtrarla: es la unica cohorte disponible ahora mismo.');
    });

    await secondRender.cleanup();
  });

  it('keeps the helper copy focused on cohort when status is already implied by the current view', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crStatus: 'paid' }),
      buildRegistration({
        crId: 102,
        crCourseSlug: 'mixing-bootcamp',
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'paid',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Curso / cohorte')).toBe(true);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.textContent).toContain('Estado disponible');
      expect(container.textContent).toContain('Pagado');
      expect(container.textContent).toContain(
        'No hace falta filtrarlo: es el unico estado presente en esta vista. Usa cohorte para cambiar la vista.',
      );
      expect(container.textContent).not.toContain('Los filtros se aplican automáticamente al cambiar.');
      expect(container.textContent).not.toContain('Empieza por cohorte y estado.');
    });

    await cleanup();
  });

  it('absorbs a shared source into the single-cohort summary block instead of adding another summary line', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'paid',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.textContent).toContain('Cohorte disponible');
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).toContain('Fuente visible: landing.');
      expect(container.textContent).not.toContain('Mostrando una sola fuente: landing.');
      expect(container.textContent).not.toContain('Fuente: landing');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('combines single-choice cohort and status context into one summary block', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.textContent).toContain('Vista actual');
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101) · Pendiente de pago');
      expect(container.textContent).toContain(
        'No hace falta filtrar cohorte ni estado: esta vista solo tiene una cohorte y un estado por ahora.',
      );
      expect(container.textContent).not.toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).not.toContain('Cohorte disponible');
      expect(container.textContent).not.toContain('Estado disponible');
    });

    await cleanup();
  });

  it('absorbs a shared source into the same single-choice summary block instead of adding another summary line', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.textContent).toContain('Vista actual');
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101) · Pendiente de pago');
      expect(container.textContent).toContain('Fuente visible: landing.');
      expect(container.textContent).not.toContain('Mostrando una sola fuente: landing.');
      expect(container.textContent).not.toContain('Fuente: landing');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('uses the course label inside the dossier instead of raw slug jargon', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Curso: Beatmaking 101 (beatmaking-101) · Fuente: landing');
      expect(document.body.textContent).not.toContain('Slug: beatmaking-101');
    });

    await cleanup();
  });

  it('omits the empty-source placeholder in the dossier header so admins only see real context', async () => {
    const sourceLessRegistration = buildRegistration({ crSource: null });
    listRegistrationsMock.mockResolvedValue([sourceLessRegistration]);
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: sourceLessRegistration,
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const dialog = getDialog();
      expect(dialog.textContent).toContain(
        `Curso: Beatmaking 101 (beatmaking-101) · Creado: ${formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-')}`,
      );
      expect(dialog.textContent).not.toContain('Fuente: Sin fuente');
    });

    await cleanup();
  });

  it('keeps the dossier header focused on contact context instead of a raw party id when identity is already visible', async () => {
    getRegistrationDossierMock.mockResolvedValue(buildDossier());

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('ada@example.com');
      expect(document.body.textContent).toContain('+593999000111');
      expect(document.body.textContent).not.toContain('Party #9');
    });

    await cleanup();
  });

  it('keeps the party id as a fallback when the dossier lacks name, email, and phone context', async () => {
    const sparseRegistration = buildRegistration({
      crFullName: null,
      crEmail: null,
      crPhoneE164: null,
    });
    listRegistrationsMock.mockResolvedValue([sparseRegistration]);
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: sparseRegistration,
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Party #9');
      expect(document.body.textContent).toContain('Sin correo ni teléfono');
    });

    await cleanup();
  });

  it('keeps the empty notes CTA lightweight in the header while explaining the editor stays collapsed', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(
        'Aún no hay notas internas. Ábrelas solo cuando necesites dejar contexto, acuerdos o próximos pasos.',
      );
      expect(getButtonByText(document.body, 'Abrir notas')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar primera nota')).toBe(0);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Abrir notas'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Guardar notas')).toBeTruthy();
      expect(getButtonByText(document.body, 'Cancelar notas')).toBeTruthy();
      expect(getButtonByText(document.body, 'Guardar notas').disabled).toBe(true);
      expect(countButtonsByText(document.body, 'Ocultar editor')).toBe(0);
      expect(countButtonsByText(document.body, 'Abrir notas')).toBe(0);
      expect(document.body.textContent).not.toContain(
        'Aún no hay notas internas. Ábrelas solo cuando necesites dejar contexto, acuerdos o próximos pasos.',
      );
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Cancelar notas'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(
        'Aún no hay notas internas. Ábrelas solo cuando necesites dejar contexto, acuerdos o próximos pasos.',
      );
      expect(getButtonByText(document.body, 'Abrir notas')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Cancelar notas')).toBe(0);
      expect(countButtonsByText(document.body, 'Guardar notas')).toBe(0);
    });

    await cleanup();
  });

  it('absorbs a shared source into the shared cohort summary when the page still offers multiple cohort choices', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'paid',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countOccurrences(
        container,
        'Mostrando una sola cohorte: Beatmaking 101 (beatmaking-101). Fuente visible: landing.',
      )).toBe(1);
      expect(container.textContent).not.toContain('Mostrando una sola fuente: landing.');
      expect(container.textContent).not.toContain('Fuente: landing');
      expect(container.textContent).not.toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('summarizes a shared visible source once instead of repeating it on each row', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crCourseSlug: 'mixing-bootcamp',
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Estado disponible');
      expect(container.textContent).toContain('Pendiente de pago');
      expect(container.textContent).toContain('Fuente visible: landing.');
      expect(container.textContent).not.toContain('Fuente: landing');
      expect(container.textContent).toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).toContain('Cohorte: Mixing Bootcamp (mixing-bootcamp)');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('keeps row source details visible when the current list mixes known and missing sources', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crCourseSlug: 'mixing-bootcamp',
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crSource: '',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).not.toContain('Mostrando una sola fuente:');
      expect(container.textContent).not.toContain('Todas las inscripciones visibles están sin fuente registrada.');
      expect(container.textContent).toContain('Fuente: landing');
      expect(container.textContent).toContain('Fuente: Sin fuente');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('summarizes a shared missing source once instead of repeating Sin fuente on each row', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crSource: '' }),
      buildRegistration({
        crId: 102,
        crCourseSlug: 'mixing-bootcamp',
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crSource: '',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Estado disponible');
      expect(container.textContent).toContain('Pendiente de pago');
      expect(container.textContent).toContain('Fuente visible: sin fuente registrada.');
      expect(container.textContent).not.toContain('Mostrando una sola fuente:');
      expect(container.textContent).not.toContain('Fuente: Sin fuente');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('uses a status chip group instead of a dropdown and keeps the filter resettable', async () => {
    const pendingRegistration = buildRegistration();
    const paidRegistration = buildRegistration({
      crId: 102,
      crFullName: 'Grace Hopper',
      crEmail: 'grace@example.com',
      crStatus: 'paid',
    });
    const cancelledRegistration = buildRegistration({
      crId: 103,
      crFullName: 'Katherine Johnson',
      crEmail: 'katherine@example.com',
      crStatus: 'cancelled',
    });

    listRegistrationsMock.mockImplementation((params) => Promise.resolve(
      params?.status === 'paid'
        ? [paidRegistration]
        : params?.status === 'cancelled'
          ? [cancelledRegistration]
          : params?.status === 'pending_payment'
            ? [pendingRegistration]
            : [pendingRegistration, paidRegistration, cancelledRegistration],
    ));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Estado')).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(4);
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Todos').textContent?.trim()).toBe('Todos (3)');
      expect(container.textContent).not.toContain('Total: 3');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
      expect(container.textContent).toContain('Katherine Johnson');
      expect(container.textContent).not.toContain('Vista filtrada:');
    });

    const paidFilter = container.querySelector<HTMLElement>('[aria-label="Filtrar inscripciones por estado Pagado"]');
    if (!paidFilter) {
      throw new Error('Status chip not found: Pagado');
    }

    listRegistrationsMock.mockClear();

    await act(async () => {
      clickElement(paidFilter);
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenLastCalledWith({
        slug: undefined,
        status: 'paid',
        limit: 200,
      });
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.textContent).toContain('Grace Hopper');
      expect(container.textContent).not.toContain('Ada Lovelace');
      expect(container.textContent).not.toContain('Katherine Johnson');
      expect(container.textContent).toContain('Vista actual');
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101) · Pagado');
      expect(container.textContent).toContain(
        'No hace falta filtrar cohorte ni estado: esta vista solo tiene una cohorte y un estado por ahora.',
      );
      expect(container.textContent).not.toContain('Vista filtrada: estado pagado.');
      expect(getButtonByText(container, 'Restablecer filtros')).toBeTruthy();
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      clickButton(getButtonByText(container, 'Restablecer filtros'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenLastCalledWith({
        slug: undefined,
        status: undefined,
        limit: 200,
      });
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(4);
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
      expect(container.textContent).toContain('Katherine Johnson');
      expect(container.textContent).not.toContain('Vista filtrada:');
      expect(container.textContent).not.toContain('Estado disponible');
    });

    await cleanup();
  });

  it('labels the status chip group as a filter so it does not compete with row status actions', async () => {
    const pendingRegistration = buildRegistration();
    const paidRegistration = buildRegistration({
      crId: 102,
      crFullName: 'Grace Hopper',
      crEmail: 'grace@example.com',
      crStatus: 'paid',
    });
    const cancelledRegistration = buildRegistration({
      crId: 103,
      crFullName: 'Katherine Johnson',
      crEmail: 'katherine@example.com',
      crStatus: 'cancelled',
    });

    listRegistrationsMock.mockImplementation((params) => Promise.resolve(
      params?.status === 'paid'
        ? [paidRegistration]
        : params?.status === 'cancelled'
          ? [cancelledRegistration]
          : params?.status === 'pending_payment'
            ? [pendingRegistration]
            : [pendingRegistration, paidRegistration, cancelledRegistration],
    ));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasExactText(container, 'Filtrar por estado')).toBe(true);
      expect(hasExactText(container, 'Estado')).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(4);
      expect(countOccurrences(container, 'Estado:')).toBe(3);
    });

    await cleanup();
  });

  it('hides zero-result status filters once the current view already has registrations', async () => {
    const pendingRegistration = buildRegistration();
    const paidRegistration = buildRegistration({
      crId: 102,
      crFullName: 'Grace Hopper',
      crEmail: 'grace@example.com',
      crStatus: 'paid',
    });

    listRegistrationsMock.mockResolvedValue([pendingRegistration, paidRegistration]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(3);
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Todos"]')).not.toBeNull();
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Pendiente de pago"]')).not.toBeNull();
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Pagado"]')).not.toBeNull();
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Cancelado"]')).toBeNull();
      expect(container.textContent).toContain('Solo aparecen estados con inscripciones en esta vista.');
    });

    await cleanup();
  });

  it('replaces a single real status filter with context copy when the current view does not need status filtering', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.textContent).toContain('Estado disponible');
      expect(container.textContent).toContain('Pendiente de pago');
      expect(container.textContent).toContain(
        'No hace falta filtrarlo: es el unico estado presente en esta vista. Usa cohorte para cambiar la vista.',
      );
      expect(container.textContent).not.toContain('Los filtros se aplican automáticamente al cambiar.');
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Solo aparecen estados con inscripciones en esta vista.');
    });

    await cleanup();
  });

  it('shows only meaningful quick status actions for each registration row', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'paid',
      }),
      buildRegistration({
        crId: 103,
        crFullName: 'Katherine Johnson',
        crEmail: 'katherine@example.com',
        crStatus: 'cancelled',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).not.toContain(
        'Abre expediente para ver notas, comprobantes y seguimiento. Usa el estado solo para cambios rapidos.',
      );
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(3);
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Estado: Pendiente de pago');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Estado: Pagado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Katherine Johnson').textContent?.trim()).toBe('Estado: Cancelado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').getAttribute('aria-haspopup')).toBe('menu');
      expect(container.textContent).not.toContain('Estado disponible');
      expect(countOccurrences(container, 'Cambiar estado:')).toBe(0);
      expect(countOccurrences(container, 'Estado:')).toBe(3);
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(0);
      expect(container.querySelector('button[aria-label="Subir comprobante y marcar pagado para Ada Lovelace"]')).toBeNull();
      expect(container.querySelector('button[aria-label="Marcar pendiente para Grace Hopper"]')).toBeNull();
      expect(container.querySelector('button[aria-label="Cancelar inscripción para Katherine Johnson"]')).toBeNull();
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(openPaymentDossierLabel);
      expect(document.body.textContent).toContain('Cancelar inscripción');
      expect(document.body.textContent).not.toContain('Marcar pendiente');
      expect(document.body.textContent).not.toContain('Estado actual:');
      expect(document.body.textContent).not.toContain('Subir comprobante para marcar pagado');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Marcar pendiente');
      expect(document.body.textContent).toContain('Cancelar inscripción');
      expect(document.body.textContent).not.toContain(openPaymentDossierLabel);
      expect(document.body.textContent).not.toContain('Estado actual:');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Katherine Johnson'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(openPaymentDossierLabel);
      expect(document.body.textContent).toContain('Marcar pendiente');
      expect(document.body.textContent).not.toContain('Cancelar inscripción');
      expect(document.body.textContent).not.toContain('Estado actual:');
      expect(document.body.textContent).not.toContain('Subir comprobante para marcar pagado');
    });

    await cleanup();
  });

  it('shows email history inline inside the dossier without opening duplicate close actions', async () => {
    listRegistrationEmailsMock.mockResolvedValue([
      buildEmailEvent(),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, showSystemEmailsLabel)).toBeTruthy();
      expect(countButtonsByText(document.body, 'Cerrar')).toBe(1);
      expect(document.body.textContent).not.toContain(systemEmailHistoryHelperText);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, showSystemEmailsLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationEmailsMock).toHaveBeenCalledWith(101, 200);
      expect(getButtonByText(document.body, hideSystemEmailsLabel)).toBeTruthy();
      expect(getButtonByAriaLabel(document.body, 'Refrescar expediente y correos')).toBeTruthy();
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Actualizar correos',
        ),
      ).toBe(false);
      expect(document.body.textContent).toContain(systemEmailHistoryHelperText);
      expect(document.body.textContent).toContain('Recordatorio de pago enviado.');
      expect(countButtonsByText(document.body, 'Cerrar')).toBe(1);
    });

    getRegistrationDossierMock.mockClear();
    listRegistrationEmailsMock.mockClear();

    await act(async () => {
      clickButton(getButtonByAriaLabel(document.body, 'Refrescar expediente y correos'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getRegistrationDossierMock).toHaveBeenCalledWith('beatmaking-101', 101);
      expect(listRegistrationEmailsMock).toHaveBeenCalledWith(101, 200);
    });

    await cleanup();
  });

  it('replaces the empty system-email action with passive guidance for first-time admins', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(emptySystemEmailHistoryMessage);
      expect(document.body.textContent).not.toContain(showSystemEmailsLabel);
      expect(document.body.textContent).toContain(emptyFollowUpAlertMessage);
      expect(document.body.textContent).not.toContain(
        'Aún no hay seguimiento manual. Documenta llamadas, correos o próximos pasos desde aquí. Los cambios de estado y los comprobantes nuevos también quedarán registrados aquí.',
      );
      expect(document.body.textContent).not.toContain('Correos del sistema');
      expect(document.body.textContent).not.toContain(systemEmailHistoryHelperText);
    });

    await cleanup();
  });

  it('localizes dossier activity labels so admins do not see raw internal event jargon', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdFollowUps: [
          buildFollowUp({
            crfEntryType: 'status_change',
            crfSubject: null,
          }),
        ],
      }),
    );
    listRegistrationEmailsMock.mockResolvedValue([
      buildEmailEvent({
        ceEventType: 'registration_confirmation',
        ceStatus: 'skipped',
        ceMessage: 'No se envío el correo porque ya existía un envío reciente.',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Cambio de estado');
      expect(document.body.textContent).not.toContain('Status Change');
      expect(document.body.querySelector('button[aria-label^="Abrir acciones para seguimiento Cambio de estado del "]')).not.toBeNull();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, showSystemEmailsLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Confirmación de inscripción');
      expect(document.body.textContent).toContain('Omitido');
      expect(document.body.textContent).not.toContain('Registration Confirmation');
      expect(document.body.textContent).not.toContain('skipped');
    });

    await cleanup();
  });

  it('uses a scoped dossier refresh control instead of a generic footer action', async () => {
    getRegistrationDossierMock.mockResolvedValue(buildDossier());

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(document.body, 'Refrescar expediente')).toBeTruthy();
      expect(getButtonByAriaLabel(document.body, 'Refrescar expediente').disabled).toBe(false);
      expect(getRegistrationDossierMock).toHaveBeenCalledWith('beatmaking-101', 101);
      expect(countButtonsByText(document.body, 'Cerrar')).toBe(1);
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Actualizar',
        ),
      ).toBe(false);
    });

    getRegistrationDossierMock.mockClear();

    await act(async () => {
      clickButton(getButtonByAriaLabel(document.body, 'Refrescar expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getRegistrationDossierMock).toHaveBeenCalledWith('beatmaking-101', 101);
    });

    await cleanup();
  });

  it('replaces the empty receipt list area with one guided first-receipt CTA', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [],
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(emptyReceiptAlertMessage);
      expect(document.body.textContent).not.toContain(
        'Todavía no hay comprobantes. Agrega el primero para documentar el pago y habilitar Marcar pagado.',
      );
      expect(document.body.textContent).not.toContain(
        'Cuando guardes el primer comprobante, quedara listado aqui con enlace y acciones para revisarlo despues.',
      );
      expect(countButtonsByText(document.body, 'Agregar primer comprobante')).toBe(1);
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Agregar comprobante',
        ),
      ).toBe(false);
      expect(document.body.textContent).not.toContain('Abrir comprobante');
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(false);
    });

    await cleanup();
  });

  it('switches the first-receipt section guidance from empty-state copy to direct form guidance once the composer is open', async () => {
    const firstReceiptComposerHelpText =
      'Este formulario ya está abierto para registrar el primer comprobante. Guárdalo y aparecerá aquí con enlace y acciones para revisarlo después.';

    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [],
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(emptyReceiptAlertMessage);
      expect(document.body.textContent).not.toContain(firstReceiptComposerHelpText);
      expect(getButtonByText(document.body, 'Agregar primer comprobante')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar primer comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(firstReceiptComposerHelpText);
      expect(document.body.textContent).not.toContain(emptyReceiptAlertMessage);
      expect(getButtonByText(document.body, 'Guardar comprobante')).toBeTruthy();
      expect(getButtonByText(document.body, 'Cancelar comprobante')).toBeTruthy();
    });

    await cleanup();
  });

  it('keeps the first receipt flow focused until there is something saved to review', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [],
      }),
    );

    let container = document.createElement('div');
    document.body.appendChild(container);
    let page = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.querySelector('[data-testid="course-registration-receipt-list-pane"]')).not.toBeNull();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar primer comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.querySelector('[data-testid="course-registration-receipt-composer-pane"]')).not.toBeNull();
      expect(document.body.querySelector('[data-testid="course-registration-receipt-list-pane"]')).toBeNull();
    });

    await page.cleanup();

    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [buildReceipt()],
      }),
    );

    container = document.createElement('div');
    document.body.appendChild(container);
    page = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Agregar comprobante')).toBeTruthy();
      expect(document.body.querySelector('[data-testid="course-registration-receipt-list-pane"]')).not.toBeNull();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.querySelector('[data-testid="course-registration-receipt-composer-pane"]')).not.toBeNull();
      expect(document.body.querySelector('[data-testid="course-registration-receipt-list-pane"]')).not.toBeNull();
      expect(document.body.textContent).toContain('receipt.pdf');
    });

    await page.cleanup();
  });

  it('lets admins hide an empty receipt URL fallback after opening it by mistake', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [],
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Agregar primer comprobante')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar primer comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(false);
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir archivo')).toBeTruthy();
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir archivo').getAttribute('aria-expanded')).toBe('false');
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Usar enlace existente en lugar de subir archivo'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(true);
      expect(hasLabel(document.body, 'Nombre visible')).toBe(true);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(true);
      expect(getButtonByText(document.body, 'Ocultar enlace existente')).toBeTruthy();
      expect(getButtonByText(document.body, 'Ocultar enlace existente').getAttribute('aria-expanded')).toBe('true');
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Ocultar enlace existente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir archivo')).toBeTruthy();
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir archivo').getAttribute('aria-expanded')).toBe('false');
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Ocultar enlace existente',
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('opens the payment dossier from the status menu without duplicating the receipt-upload action', async () => {
    const markPaidReceiptSectionHelpText = 'Este formulario ya está abierto para registrar el primer comprobante. Guárdalo y luego podrás marcar la inscripción como pagada.';
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getMenuItemByText(document.body, openPaymentDossierLabel)).toBeTruthy();
      expect(document.body.textContent).not.toContain('Subir comprobante para marcar pagado');
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, openPaymentDossierLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Registrar pago de inscripción');
      expect(document.body.textContent).not.toContain('Expediente de inscripción');
      expect(document.body.textContent).not.toContain(
        'Sube un comprobante o pega una URL existente para habilitar Marcar pagado.',
      );
      expect(document.body.textContent).toContain(markPaidReceiptSectionHelpText);
      expect(document.body.textContent).not.toContain(emptyReceiptAlertMessage);
      expect(hasLabel(document.body, 'Nombre visible')).toBe(true);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(true);
      expect(getButtonByText(document.body, 'Guardar comprobante')).toBeTruthy();
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Agregar comprobante',
        ),
      ).toBe(false);
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Agregar primer comprobante',
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('keeps the mark-paid flow focused on the pay action when a saved receipt already unlocks it', async () => {
    const registration = buildRegistration();
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: registration,
        crdReceipts: [buildReceipt({ crrRegistrationId: registration.crId })],
        crdCanMarkPaid: true,
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getMenuItemByText(document.body, openPaymentDossierLabel)).toBeTruthy();
      expect(document.body.textContent).not.toContain('Subir comprobante para marcar pagado');
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, openPaymentDossierLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Confirmar pago de inscripción');
      expect(document.body.textContent).not.toContain('Expediente de inscripción');
      expect(getButtonByText(document.body, 'Marcar pagado')).toBeTruthy();
      expect(getButtonByText(document.body, 'Agregar comprobante')).toBeTruthy();
      expect(document.body.textContent).not.toContain(
        'Sube un comprobante o pega una URL existente para habilitar Marcar pagado.',
      );
      expect(document.body.textContent).not.toContain(
        'Este formulario ya está abierto para registrar el primer comprobante. Guárdalo y luego podrás marcar la inscripción como pagada.',
      );
      expect(document.body.textContent).not.toContain(emptyReceiptAlertMessage);
      expect(document.body.textContent).toContain('receipt.pdf');
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Guardar comprobante',
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('keeps empty internal notes collapsed until the admin explicitly opens them', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration({ crAdminNotes: null }),
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(
        'Aún no hay notas internas. Ábrelas solo cuando necesites dejar contexto, acuerdos o próximos pasos.',
      );
      expect(hasLabel(document.body, 'Notas internas')).toBe(false);
      expect(getButtonByText(document.body, 'Abrir notas')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Abrir notas')).toBe(1);
      expect(countButtonsByText(document.body, 'Agregar primera nota')).toBe(0);
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Agregar notas',
        ),
      ).toBe(false);
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Guardar notas',
        ),
      ).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Abrir notas'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'Notas internas')).toBe(true);
      expect(getButtonByText(document.body, 'Guardar notas')).toBeTruthy();
      expect(getButtonByText(document.body, 'Guardar notas').disabled).toBe(true);
      expect(document.body.textContent).toContain('Edita el contenido para habilitar Guardar.');
    });

    await act(async () => {
      setInputValue(getInputByLabel(document.body, 'Notas internas'), 'Confirmó pago por WhatsApp.');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Guardar notas').disabled).toBe(false);
    });

    await cleanup();
  });

  it('keeps optional follow-up details collapsed until admins ask for them', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Registrar primer seguimiento')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(1);
      expect(document.body.textContent).toContain(emptyFollowUpAlertMessage);
      expect(document.body.textContent).not.toContain('0 entradas');
      expect(document.body.textContent).not.toContain('Registrar seguimiento');
      expect(document.body.textContent).not.toContain(followUpComposerHelpText);
      expect(hasLabel(document.body, 'Tipo')).toBe(false);
      expect(hasLabel(document.body, 'Nota de seguimiento')).toBe(false);
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Guardar seguimiento',
        ),
      ).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Registrar primer seguimiento'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Registrar seguimiento');
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(document.body.textContent).toContain(followUpComposerHelpText);
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(0);
      expect(hasLabel(document.body, 'Tipo')).toBe(false);
      expect(hasLabel(document.body, 'Nota de seguimiento')).toBe(true);
      expect(hasLabel(document.body, 'Asunto')).toBe(false);
      expect(hasLabel(document.body, 'Próximo seguimiento')).toBe(false);
      expect(document.body.textContent).toContain('Agrega tipo, asunto, recordatorio o evidencia solo si hacen falta.');
      expect(getButtonByText(document.body, 'Agregar detalles opcionales')).toBeTruthy();
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Usar enlace existente en lugar de subir adjunto',
        ),
      ).toBe(false);
      expect(getButtonByText(document.body, 'Guardar seguimiento')).toBeTruthy();
      expect(getButtonByText(document.body, 'Cancelar seguimiento')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar detalles opcionales'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'Tipo')).toBe(true);
      expect(hasLabel(document.body, 'Asunto')).toBe(true);
      expect(hasLabel(document.body, 'Próximo seguimiento')).toBe(true);
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir adjunto')).toBeTruthy();
    });

    await cleanup();
  });

  it('lets admins close empty optional follow-up details after opening them', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Registrar primer seguimiento')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Registrar primer seguimiento'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Agregar detalles opcionales')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar detalles opcionales'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'Asunto')).toBe(true);
      expect(hasLabel(document.body, 'Próximo seguimiento')).toBe(true);
      expect(getButtonByText(document.body, 'Ocultar detalles opcionales')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Ocultar detalles opcionales'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const reopenButton = getButtonByText(document.body, 'Agregar detalles opcionales');
      expect(reopenButton).toBeTruthy();
      expect(reopenButton.getAttribute('aria-expanded')).toBe('false');
      expect(document.body.textContent).toContain('Agrega tipo, asunto, recordatorio o evidencia solo si hacen falta.');
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Ocultar detalles opcionales',
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('keeps one follow-up actions entry point per saved note and reveals edit only on demand', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdFollowUps: [buildFollowUp()],
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(document.body, 'Abrir acciones para seguimiento Confirmó transferencia')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Acciones')).toBe(1);
      expect(document.body.textContent).not.toContain('1 entrada');
      expect(document.body.querySelector('button[aria-label="Editar seguimiento Confirmó transferencia"]')).toBeNull();
      expect(document.body.querySelector('button[aria-label="Eliminar seguimiento Confirmó transferencia"]')).toBeNull();
      expect(
        Array.from(document.body.querySelectorAll('[role="menuitem"]')).some(
          (el) => (el.textContent ?? '').trim() === 'Editar seguimiento',
        ),
      ).toBe(false);
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Actualizar seguimiento',
        ),
      ).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(document.body, 'Abrir acciones para seguimiento Confirmó transferencia'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getMenuItemByText(document.body, 'Editar seguimiento')).toBeTruthy();
      expect(getMenuItemByText(document.body, 'Eliminar seguimiento')).toBeTruthy();
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, 'Editar seguimiento'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Actualizar seguimiento')).toBeTruthy();
      expect(hasLabel(document.body, 'Tipo')).toBe(true);
      expect(hasLabel(document.body, 'Nota de seguimiento')).toBe(true);
    });

    await cleanup();
  });

  it('shows the mark-paid action only when the dossier can actually use it', async () => {
    const markPaidReceiptHint = 'Sube un comprobante o pega una URL existente para habilitar Marcar pagado.';
    const registration = buildRegistration();
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({ crdRegistration: registration, crdCanMarkPaid: false }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(emptyReceiptAlertMessage);
      expect(document.body.textContent).not.toContain(markPaidReceiptHint);
      expect(document.body.textContent).not.toContain('0 guardados');
      expect(getButtonByText(document.body, 'Agregar primer comprobante')).toBeTruthy();
      expect(Array.from(document.body.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Marcar pagado')).toBe(false);
    });

    await cleanup();

    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: registration,
        crdReceipts: [buildReceipt({ crrRegistrationId: registration.crId })],
        crdCanMarkPaid: true,
      }),
    );

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer);

    await waitForExpectation(() => {
      expect(getButtonByText(secondContainer, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(secondContainer, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Marcar pagado')).toBeTruthy();
      expect(document.body.textContent).not.toContain(markPaidReceiptHint);
    });

    await secondRender.cleanup();
  });

  it('keeps one receipt actions entry point per saved receipt and reveals edit only on demand', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [buildReceipt()],
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(document.body, 'Abrir acciones para comprobante receipt.pdf')).toBeTruthy();
      expect(document.body.textContent).not.toContain('1 guardado');
      expect(document.body.querySelector('button[aria-label="Editar receipt.pdf"]')).toBeNull();
      expect(document.body.querySelector('button[aria-label="Eliminar receipt.pdf"]')).toBeNull();
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Actualizar comprobante',
        ),
      ).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(document.body, 'Abrir acciones para comprobante receipt.pdf'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getMenuItemByText(document.body, 'Editar comprobante')).toBeTruthy();
      expect(getMenuItemByText(document.body, 'Eliminar comprobante')).toBeTruthy();
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, 'Editar comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Actualizar comprobante')).toBeTruthy();
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(true);
    });

    await cleanup();
  });

  it('shows dossier count chips only when a section has multiple saved items', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [
          buildReceipt(),
          buildReceipt({ crrId: 302, crrFileName: 'receipt-2.pdf' }),
        ],
        crdFollowUps: [
          buildFollowUp(),
          buildFollowUp({ crfId: 402, crfSubject: 'Pidió confirmación final' }),
        ],
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('2 guardados');
      expect(document.body.textContent).toContain('2 entradas');
    });

    await cleanup();
  });

  it('shows when the list is filtered and lets admins reset filters in one step', async () => {
    const cappedRegistrations = buildRegistrations(200);
    listRegistrationsMock.mockImplementation((params) => Promise.resolve(cappedRegistrations.slice(0, params?.limit ?? 200)));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Ajustar límite')).toBeTruthy();
      expect(container.textContent).not.toContain('Vista filtrada:');
      expect(Array.from(container.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Restablecer filtros')).toBe(false);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      clickButton(getButtonByText(container, 'Ajustar límite'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Límite')).toBe(true);
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, 'Límite'), '50');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenLastCalledWith({
        slug: undefined,
        status: undefined,
        limit: 50,
      });
      expect(container.textContent).toContain('Límite actual: hasta 50 inscripciones.');
      expect(container.textContent).not.toContain('Vista filtrada: límite 50.');
      expect(container.textContent).not.toContain('Límite activo: 50');
      expect(getButtonByText(container, 'Restablecer límite')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Ocultar límite'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Ajustar límite (50)')).toBeTruthy();
      expect(container.textContent).toContain('Límite actual: hasta 50 inscripciones.');
      expect(container.textContent).not.toContain('Vista filtrada: límite 50.');
      expect(container.textContent).not.toContain('Límite activo: 50');
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      clickButton(getButtonByText(container, 'Restablecer límite'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenLastCalledWith({
        slug: undefined,
        status: undefined,
        limit: 200,
      });
      expect(container.textContent).not.toContain('Vista filtrada:');
      expect(getButtonByText(container, 'Ajustar límite')).toBeTruthy();
      expect(container.textContent).not.toContain('Límite activo:');
      expect(Array.from(container.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Restablecer filtros')).toBe(false);
    });

    await cleanup();
  });

  it('shows a single contextual empty state when filters hide every registration', async () => {
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(
      container,
      '/inscripciones-curso?slug=beatmaking-101&status=paid&limit=50',
    );

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: 'beatmaking-101',
        status: 'paid',
        limit: 50,
      });
      expect(container.textContent).toContain(
        'No hay inscripciones en la vista actual: cohorte Beatmaking 101 (beatmaking-101) · estado pagado · límite 50. Restablece la vista o usa refrescar si esperabas resultados.',
      );
      expect(container.textContent).not.toContain(
        'Los filtros se aplican automáticamente al cambiar. Empieza por cohorte y estado; usa Ajustar límite solo cuando necesites revisar un lote distinto. Ajusta la vista o usa refrescar si esperabas resultados.',
      );
      expect(container.textContent).not.toContain('Vista filtrada:');
      expect(container.textContent).not.toContain('No hay inscripciones para esta vista.');
      expect(countButtonsByText(container, 'Restablecer vista')).toBe(1);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      clickButton(getButtonByText(container, 'Restablecer vista'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenLastCalledWith({
        slug: undefined,
        status: undefined,
        limit: 200,
      });
      expect(container.textContent).toContain(
        'Todavía no hay inscripciones. Cuando exista la primera, aquí aparecerán cohorte, estado y tamaño del lote para filtrar la vista.',
      );
      expect(container.textContent).not.toContain('Todavía no hay inscripciones para mostrar en esta vista.');
      expect(container.textContent).not.toContain('No hay inscripciones con los filtros actuales:');
      expect(Array.from(container.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Ajustar límite')).toBe(false);
      expect(Array.from(container.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Restablecer filtros')).toBe(false);
    });

    await cleanup();
  });

  it('keeps the header comparative by hiding a solitary total chip and redundant single-status totals', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).not.toContain('Total: 1');
      expect(container.textContent).not.toContain('Pendientes: 1');
      expect(container.textContent).not.toContain('Pagadas: 0');
      expect(container.textContent).not.toContain('Canceladas: 0');
      expect(container.textContent).toContain('Vista actual');
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101) · Pendiente de pago');
      expect(container.textContent).not.toContain('Cohorte disponible');
      expect(container.textContent).not.toContain('Estado disponible');
      expect(container.textContent).not.toContain(
        'Los filtros se aplican automáticamente al cambiar. Empieza por cohorte y estado; Ajustar límite aparecerá cuando esta vista llene el lote actual o si ya estás usando un límite personalizado.',
      );
      expect(container.textContent).not.toContain(
        'Los totales de arriba resumen esta vista y usan los mismos colores que cada estado.',
      );
    });

    await cleanup();
  });

  it('keeps multi-row single-status counts in passive list context instead of the header', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).not.toContain('Total: 2');
      expect(container.textContent).toContain('Mostrando 2 inscripciones en esta vista.');
      expect(getButtonByText(container, 'Copiar CSV')).toBeTruthy();
      expect(
        Array.from(container.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Copiar CSV (2 filas)',
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('shows status counts on the filter chips instead of a separate totals legend', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'paid',
      }),
      buildRegistration({
        crId: 103,
        crFullName: 'Katherine Johnson',
        crEmail: 'katherine@example.com',
        crStatus: 'cancelled',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Todos').textContent?.trim()).toBe('Todos (3)');
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pendiente de pago').textContent?.trim()).toBe('Pendiente de pago (1)');
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pagado').textContent?.trim()).toBe('Pagado (1)');
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Cancelado').textContent?.trim()).toBe('Cancelado (1)');
      expect(container.textContent).not.toContain('Total: 3');
      expect(container.textContent).not.toContain('Pendientes: 1');
      expect(container.textContent).not.toContain('Pagadas: 1');
      expect(container.textContent).not.toContain('Canceladas: 1');
      expect(container.textContent).not.toContain(
        'Los totales de arriba resumen esta vista y usan los mismos colores que cada estado.',
      );
    });

    await cleanup();
  });

  it('keeps CSV export focused on real list views and leaves row counts in passive context copy', async () => {
    const registrations = buildRegistrations(200, (index) => {
      if (index % 3 === 1) {
        return { crStatus: 'paid' };
      }
      if (index % 3 === 2) {
        return { crStatus: 'cancelled' };
      }
      return { crStatus: 'pending_payment' };
    });
    listRegistrationsMock.mockImplementation((params) => Promise.resolve(registrations.slice(0, params?.limit ?? 200)));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).not.toContain('Total: 200');
      expect(container.textContent).not.toContain('Pagadas:');
      expect(container.textContent).not.toContain('Pendientes:');
      expect(container.textContent).not.toContain('Canceladas:');
      expect(container.textContent).not.toContain(
        'Los totales de arriba resumen esta vista y usan los mismos colores que cada estado.',
      );
      expect(container.textContent).not.toContain('Leyenda de estados:');
      expect(container.textContent).toContain('Mostrando 200 inscripciones en esta vista.');
      expect(
        Array.from(container.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Copiar CSV (200 filas)',
        ),
      ).toBe(false);
      expect(getButtonByText(container, 'Copiar CSV')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Ajustar límite'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Límite')).toBe(true);
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, 'Límite'), '50');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenLastCalledWith({
        slug: undefined,
        status: undefined,
        limit: 50,
      });
      expect(container.textContent).toContain('Límite actual: hasta 50 inscripciones.');
      expect(container.textContent).toContain('Mostrando 50 inscripciones.');
      expect(container.textContent).not.toContain('Mostrando 50 inscripciones con los filtros actuales.');
      expect(container.textContent).not.toContain('Vista filtrada: límite 50.');
      expect(getButtonByText(container, 'Restablecer límite')).toBeTruthy();
      expect(getButtonByText(container, 'Copiar CSV filtrado')).toBeTruthy();
      expect(
        Array.from(container.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Copiar CSV filtrado (50 filas)',
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('hides result-only summaries when the current view has no registrations', async () => {
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(
        'Todavía no hay inscripciones. Cuando exista la primera, aquí aparecerán cohorte, estado y tamaño del lote para filtrar la vista.',
      );
      expect(container.textContent).not.toContain('Todavía no hay inscripciones para mostrar en esta vista.');
      expect(countOccurrences(container, 'Todavía no hay inscripciones. Cuando exista la primera, aquí aparecerán cohorte, estado y tamaño del lote para filtrar la vista.')).toBe(1);
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.textContent).not.toContain('Cambiar estado:');
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Cohorte disponible');
      expect(container.textContent).not.toContain('Estado disponible');
      expect(container.textContent).not.toContain('Los filtros se aplican automáticamente al cambiar.');
      expect(container.textContent).not.toContain('Leyenda de estados:');
      expect(container.textContent).not.toContain('Total: 0');
      expect(container.textContent).not.toContain('Pagadas: 0');
      expect(container.textContent).not.toContain('Pendientes: 0');
      expect(container.textContent).not.toContain('Canceladas: 0');
      expect(Array.from(container.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Ajustar límite')).toBe(false);
      expect(getButtonByText(container, 'Refrescar lista')).toBeTruthy();
      expect(
        Array.from(container.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim().startsWith('Copiar CSV'),
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('keeps the list refresh action scope-labeled so it does not blend into dossier refresh', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Refrescar lista')).toBeTruthy();
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(1);
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Refrescar lista')).toBeTruthy();
      expect(getButtonByAriaLabel(document.body, 'Refrescar expediente')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Refrescar lista')).toBe(1);
    });

    await cleanup();
  });
});
