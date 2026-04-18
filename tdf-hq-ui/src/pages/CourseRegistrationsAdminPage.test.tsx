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
const emptyReceiptEvidenceAlertMessage =
  'Agrega el primer comprobante solo si necesitas documentar evidencia de pago. Cuando lo guardes aparecerá aquí con enlace y acciones para revisarlo después.';
const receiptComposerHelpText =
  'Este formulario ya está abierto para guardar otro comprobante o pegar un enlace existente.';
const editingReceiptComposerHelpText =
  'Edita el comprobante y guarda los cambios para actualizar el registro.';
const showSystemEmailsLabel = 'Ver correos del sistema';
const hideSystemEmailsLabel = 'Ocultar correos del sistema';
const systemEmailHistoryHelperText =
  'Historial persistente de correos del sistema para esta inscripción. Usa el refresco del expediente para volver a consultarlo.';
const emptySystemEmailHistoryMessage =
  'Todavía no hay correos del sistema registrados para esta inscripción. Cuando se envíe el primero, aparecerá aquí.';
const optionalDossierContextActionsLabel = 'Notas y seguimiento';
const markPaidEmptyNotesHelperText =
  'Agrega una nota solo si necesitas dejar contexto extra sobre este pago.';
const markPaidEmptyFollowUpHelperText =
  'Agrega seguimiento solo si necesitas dejar contexto manual aparte del comprobante o del cambio de estado.';
const emptyFollowUpAlertMessage =
  'Aún no hay seguimiento manual. Documenta llamadas, mensajes o próximos pasos desde aquí. Los cambios de estado y los comprobantes nuevos también quedarán registrados aquí.';
const firstFollowUpComposerHelpText =
  'Este formulario ya está abierto para registrar el primer seguimiento. Guárdalo y aparecerá aquí para revisarlo después.';
const openPaymentWorkflowLabel = 'Registrar pago';
const reopenPendingLabel = 'Reabrir como pendiente';
const copyVisibleCsvLabel = (count: number) => `Copiar visibles (${count} fila${count === 1 ? '' : 's'})`;
const localSearchLabel = 'Buscar inscripciones';
const activeStatusFilterHelperText = 'Esta vista ya está filtrada por ese estado. Tócalo otra vez para volver a ver todos.';
const clearPaidStatusFilterLabel = 'Quitar filtro de estado Pagado';
const customStatusFilterUnavailableMessage =
  'Los estados visibles no coinciden con los filtros estándar. Usa el menú de estado de cada inscripción para normalizarlos.';
const dossierScopeHint =
  'Abre el expediente desde el nombre; usa Cambiar estado para acciones rápidas.';
const dossierOnlyScopeHint =
  'Abre el expediente desde el nombre; el estado abre acciones rápidas.';
const contactDossierScopeHint =
  'Abre el expediente desde el contacto; usa Cambiar estado para acciones rápidas.';
const contactDossierOnlyScopeHint =
  'Abre el expediente desde el contacto; el estado abre acciones rápidas.';
const recordDossierScopeHint =
  'Abre el expediente desde el registro; usa Cambiar estado para acciones rápidas.';
const initialEmptyStateConfigMessage =
  'Todavía no hay inscripciones. Configura el curso inicial; cuando llegue la primera inscripción podrás revisar pago, seguimiento y correos aquí.';
const initialEmptyStateMultiCohortMessage =
  'Todavía no hay inscripciones. Ya hay formularios configurados; abre Cursos para decidir cuál compartir primero.';
const singleCohortInitialEmptyStateMessage =
  'Todavía no hay inscripciones para Beatmaking 101 (beatmaking-101). Cuando llegue la primera podrás revisar pago, seguimiento y correos aquí.';
const initialEmptyStateConfigActionLabel = 'Configurar cursos';
const initialEmptyStateMultiCohortActionLabel = 'Abrir Cursos';
const initialEmptyStateFormActionLabel = 'Abrir formulario público';
const initialCohortResolutionMessage =
  'Revisando cohortes configuradas para mostrar el siguiente paso correcto.';
const initialCohortErrorMessage =
  'No se pudieron cargar las cohortes para elegir qué formulario compartir. Reintenta cohortes antes de filtrar o revisar la lista.';
const cohortFilterUnavailableMessage =
  'No se pudieron cargar cohortes. La lista sigue disponible; reintenta cohortes para recuperar el filtro por curso.';

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
    if (labelText === 'Expediente') {
      const dossierButtons = Array.from(root.querySelectorAll<HTMLButtonElement>('button[aria-label^="Abrir expediente de "]'));
      if (dossierButtons.length === 1) {
        return dossierButtons[0]!;
      }
    }
    throw new Error(`Button not found: ${labelText}`);
  }
  return button;
};

const getButtonByAriaLabel = (root: ParentNode, labelText: string) => {
  const button = Array.from(root.querySelectorAll<HTMLButtonElement>('button')).find(
    (candidate) => candidate.getAttribute('aria-label') === labelText,
  );
  if (!(button instanceof HTMLButtonElement)) {
    throw new Error(`Button not found: ${labelText}`);
  }
  return button;
};

const getDossierTriggers = (root: ParentNode) =>
  Array.from(root.querySelectorAll<HTMLButtonElement>('button[aria-label^="Abrir expediente de "]'));

const getOnlyDossierTrigger = (root: ParentNode) => {
  const buttons = getDossierTriggers(root);
  if (buttons.length !== 1) {
    throw new Error(`Expected exactly one dossier trigger, found ${buttons.length}`);
  }
  return buttons[0]!;
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

  it('keeps the single-result view minimal while still hinting at dossier and status actions', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).not.toContain(
        'Los filtros se aplican automáticamente al cambiar. Empieza por cohorte y estado; Ajustar límite aparecerá cuando esta vista llene el lote actual o si ya estás usando un límite personalizado.',
      );
      expect(container.textContent).not.toContain(
        'Vista única por ahora: una cohorte y un estado.',
      );
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).not.toContain('Slug: beatmaking-101');
      expect(container.textContent).not.toContain('Aplicar filtros');
      expect(container.textContent).not.toContain(
        'Abre expediente para ver notas, comprobantes y seguimiento. Usa el estado solo para cambios rapidos.',
      );
      expect(container.textContent).not.toContain(
        'Expediente reúne notas, pagos, seguimiento y correos de la inscripción.',
      );
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        dossierOnlyScopeHint,
      );
      expect(countOccurrences(container, dossierOnlyScopeHint)).toBe(1);
      expect(container.textContent).not.toContain('Abrir expediente');
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace').textContent?.trim()).toBe('Ada Lovelace');
      expect(container.textContent).not.toContain('Cambiar estado:');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
      expect(countOccurrences(container, 'Pendiente de pago')).toBe(1);
      expect(container.textContent).not.toContain(showSystemEmailsLabel);
      expect(hasLabel(container, 'Límite')).toBe(false);
      expect(countButtonsByText(container, 'Ajustar límite')).toBe(0);
      expect(
        Array.from(container.querySelectorAll('button')).some((el) => {
          const label = (el.textContent ?? '').trim();
          return label.startsWith('Copiar visibles');
        }),
      ).toBe(false);
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: undefined,
        limit: 200,
      });
    });

    await act(async () => {
      clickButton(getOnlyDossierTrigger(container));
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Expediente de inscripción');
      expect(document.body.textContent).not.toContain(emptySystemEmailHistoryMessage);
      expect(document.body.textContent).not.toContain(showSystemEmailsLabel);
      expect(document.body.querySelector('[aria-label="Refrescar expediente"]')).toBeNull();
      expect(document.body.querySelector('[aria-label="Refrescar expediente y correos"]')).toBeNull();
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
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(1);
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(document.body.textContent).not.toContain('0 entradas');
      expect(document.body.textContent).not.toContain('Registrar seguimiento');
      expect(document.body.textContent).not.toContain(firstFollowUpComposerHelpText);
      expect(hasLabel(document.body, 'Nombre visible')).toBe(false);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(false);
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(false);
      expect(hasLabel(document.body, 'URL del adjunto')).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar primer comprobante'));
      clickButton(getButtonByText(document.body, 'Agregar seguimiento'));
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
      expect(hasExactText(document.body, 'Registrar seguimiento')).toBe(false);
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(document.body.textContent).toContain(firstFollowUpComposerHelpText);
      expect(document.body.textContent).toContain(
        'Primero elige el archivo o pega un enlace; luego podrás ajustar el nombre visible y las notas.',
      );
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(0);
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

  it('keeps the dossier loading state focused instead of showing premature empty prompts', async () => {
    getRegistrationDossierMock.mockImplementation(() => new Promise(() => undefined));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getOnlyDossierTrigger(container)).toBeTruthy();
    });

    await act(async () => {
      clickButton(getOnlyDossierTrigger(container));
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Expediente de inscripción');
      expect(document.body.textContent).toContain('Cargando expediente…');
      expect(document.body.querySelector('[aria-label="Refrescar expediente"]')).toBeNull();
      expect(document.body.querySelector('[aria-label="Refrescar expediente y correos"]')).toBeNull();
      expect(document.body.textContent).not.toContain(emptySystemEmailHistoryMessage);
      expect(document.body.textContent).not.toContain(emptyReceiptAlertMessage);
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(document.body.textContent).not.toContain('Notas internas');
      expect(document.body.textContent).not.toContain('Comprobantes de pago');
      expect(document.body.textContent).not.toContain('Seguimiento');
      expect(countButtonsByText(document.body, 'Agregar primer comprobante')).toBe(0);
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(0);
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
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        dossierScopeHint,
      );
      expect(countOccurrences(
        container,
        dossierScopeHint,
      )).toBe(1);
      expect(countButtonsByText(container, 'Expediente')).toBe(0);
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(2);
      expect(countButtonsByText(container, 'Cambiar')).toBe(0);
      expect(countButtonsByText(container, 'Estado')).toBe(0);
      expect(countOccurrences(container, 'Pendiente de pago')).toBe(1);
      expect(container.querySelectorAll('button[aria-label^="Abrir expediente de "]')).toHaveLength(2);
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(2);
      expect(container.textContent).not.toContain('Abrir expediente');
      expect(container.textContent).not.toContain('Estado: Pendiente de pago');
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace').textContent?.trim()).toBe('Ada Lovelace');
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Grace Hopper').textContent?.trim()).toBe('Grace Hopper');
    });

    await cleanup();
  });

  it('drops the first-time dossier hint as soon as an admin opens any dossier once', async () => {
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
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        dossierScopeHint,
      );
      expect(countOccurrences(container, dossierScopeHint)).toBe(1);
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Expediente de inscripción');
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(container.textContent).not.toContain(dossierScopeHint);
    });

    await cleanup();
  });

  it('drops the first-time dossier hint after opening the row status menu once', async () => {
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
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        dossierScopeHint,
      );
      expect(countOccurrences(container, dossierScopeHint)).toBe(1);
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(openPaymentWorkflowLabel);
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(container.textContent).not.toContain(dossierScopeHint);
    });

    await cleanup();
  });

  it('keeps the current-view summary but drops filter onboarding copy after the first row action', async () => {
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
      expect(container.textContent).toContain('Vista actual');
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101) · Pendiente de pago');
      expect(container.textContent).toContain(
        'Vista única por ahora: una cohorte y un estado.',
      );
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(openPaymentWorkflowLabel);
      expect(container.textContent).toContain('Vista actual');
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101) · Pendiente de pago');
      expect(container.textContent).not.toContain(
        'Vista única por ahora: una cohorte y un estado.',
      );
    });

    await cleanup();
  });

  it('drops the first-time dossier hint after the admin uses a filter control once', async () => {
    listRegistrationsMock.mockImplementation((params) => {
      if (params?.status === 'paid') {
        return Promise.resolve([
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
            crStatus: 'paid',
          }),
        ]);
      }

      return Promise.resolve([
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
          crStatus: 'paid',
        }),
      ]);
    });

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        dossierOnlyScopeHint,
      );
      expect(countOccurrences(container, dossierOnlyScopeHint)).toBe(1);
      expect(container.textContent).not.toContain(dossierScopeHint);
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pagado').textContent?.trim()).toBe('Pagado (2)');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pagado'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenLastCalledWith({
        slug: undefined,
        status: 'paid',
        limit: 200,
      });
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(container.textContent).not.toContain(dossierOnlyScopeHint);
      expect(container.textContent).not.toContain('Mostrando 2 inscripciones.');
      expect(getButtonByText(container, copyVisibleCsvLabel(2))).toBeTruthy();
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
      expect(
        secondContainer
          .querySelector('[data-testid="course-registration-current-view-summary"] button')
          ?.textContent
          ?.trim(),
      ).toBe('Ajustar límite');
      expect(secondContainer.textContent).toContain(
        'Vista única por ahora: una cohorte y un estado. Usa Ajustar límite solo cuando necesites revisar un lote distinto.',
      );
      expect(secondContainer.textContent).not.toContain(
        'Esta vista ya está acotada a una cohorte y un estado. Usa Ajustar límite solo cuando necesites revisar un lote distinto.',
      );
      expect(countOccurrences(
        secondContainer,
        'Vista única por ahora: una cohorte y un estado. Usa Ajustar límite solo cuando necesites revisar un lote distinto.',
      )).toBe(1);
      expect(
        Array.from(secondContainer.querySelectorAll('button')).filter(
          (button) => (
            button.textContent?.trim() === 'Ajustar límite'
            && !button.closest('[data-testid="course-registration-current-view-summary"]')
          ),
        ),
      ).toHaveLength(0);
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
  }, 20_000);

  it('treats the selected cohort as passive context when it is the only configured cohort', async () => {
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
      expect(container.textContent).not.toContain('Fuente visible: landing.');
      expect(container.textContent).not.toContain('Fuente: landing');
      expect(container.textContent).not.toContain(`Creado: ${formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-')}`);
      expect(container.textContent).not.toContain('Vista filtrada:');
      expect(countButtonsByText(container, 'Mostrar todas las cohortes')).toBe(0);
      expect(container.querySelector('[data-testid="course-registration-inline-reset"]')).toBeNull();
    });

    await cleanup();
  });

  it('combines filtered scope and visible-count copy without adding shared date chrome', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crStatus: 'paid',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'paid',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?slug=beatmaking-101&status=paid');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: 'beatmaking-101',
        status: 'paid',
        limit: 200,
      });
      const summary = container.querySelector<HTMLElement>('[data-testid="course-registration-filter-summary"]');
      expect(summary?.textContent?.trim()).toBe(
        'Vista filtrada: cohorte Beatmaking 101 (beatmaking-101).',
      );
      expect(container.querySelectorAll('[data-testid="course-registration-filter-summary"]')).toHaveLength(1);
      const createdAtLabel = formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-');
      expect(container.querySelector('[data-testid="course-registration-shared-created-at-summary"]')).toBeNull();
      expect(container.textContent).not.toContain(`Misma fecha de registro: ${createdAtLabel}.`);
      expect(countOccurrences(container, `Creado: ${createdAtLabel}`)).toBe(0);
      expect(getButtonByText(container, 'Restablecer filtros')).toBeTruthy();
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
    });

    await cleanup();
  });

  it('keeps a cohort-only filtered view focused on the cohort select instead of repeating reset chrome', async () => {
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
    const { cleanup } = await renderPage(container, '/inscripciones-curso?slug=beatmaking-101');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: 'beatmaking-101',
        status: undefined,
        limit: 200,
      });
      expect(hasLabel(container, 'Curso / cohorte')).toBe(true);
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).not.toContain('Mostrando 2 inscripciones.');
      expect(getButtonByText(container, copyVisibleCsvLabel(2))).toBeTruthy();
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Vista filtrada: cohorte Beatmaking 101 (beatmaking-101).');
      expect(countButtonsByText(container, 'Mostrar todas las cohortes')).toBe(0);
      expect(container.textContent).not.toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
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
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        contactDossierOnlyScopeHint,
      );
      expect(container.textContent).not.toContain(dossierScopeHint);
      expect(container.textContent).not.toContain(contactDossierScopeHint);
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

  it('uses unique registration ids instead of repeating the generic no-name fallback', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crFullName: null,
        crEmail: null,
        crPhoneE164: null,
      }),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: '   ',
        crEmail: '   ',
        crPhoneE164: '   ',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        recordDossierScopeHint,
      );
      expect(container.textContent).not.toContain(dossierScopeHint);
      expect(hasExactText(container, 'Registro #101')).toBe(true);
      expect(hasExactText(container, 'Registro #102')).toBe(true);
      expect(countOccurrences(container, 'Sin correo ni teléfono')).toBe(0);
      expect(countOccurrences(container, 'Sin nombre')).toBe(0);
      expect(container.querySelectorAll('button[aria-label^="Abrir expediente de registro #"]')).toHaveLength(2);
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para registro #"]')).toHaveLength(2);
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Abrir expediente de registro #101'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const dialog = getDialog();
      expect(dialog.textContent).toContain('Registro #101');
      expect(hasExactText(dialog, 'Sin datos de contacto. Referencia interna: Party #9.')).toBe(true);
      expect(hasExactText(dialog, 'Party #9')).toBe(false);
      expect(dialog.textContent).not.toContain('Sin correo ni teléfono');
      expect(dialog.textContent).not.toContain('Sin nombre');
    });

    await cleanup();
  });

  it('disambiguates duplicate named registrations in row actions without adding generic row controls', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Ana Torres',
        crEmail: 'ana.primary@example.com',
        crPhoneE164: null,
      }),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: 'Ana Torres',
        crEmail: 'ana.alt@example.com',
        crPhoneE164: '+593999000222',
      }),
      buildRegistration({
        crId: 103,
        crPartyId: 11,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crPhoneE164: '+593999000333',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelector('button[aria-label="Abrir expediente de Ana Torres"]')).toBeNull();
      expect(container.querySelector('button[aria-label="Cambiar estado para Ana Torres"]')).toBeNull();
      expect(
        getButtonByAriaLabel(container, 'Abrir expediente de Ana Torres (ana.primary@example.com)'),
      ).toBeTruthy();
      expect(
        getButtonByAriaLabel(container, 'Cambiar estado para Ana Torres (ana.primary@example.com)'),
      ).toBeTruthy();
      expect(
        getButtonByAriaLabel(container, 'Abrir expediente de Ana Torres (ana.alt@example.com · +593999000222)'),
      ).toBeTruthy();
      expect(
        getButtonByAriaLabel(container, 'Cambiar estado para Ana Torres (ana.alt@example.com · +593999000222)'),
      ).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Grace Hopper')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper')).toBeTruthy();
      expect(countOccurrences(container, 'Registro #101')).toBe(1);
      expect(countOccurrences(container, 'Registro #102')).toBe(1);
      expect(countOccurrences(container, 'Registro #103')).toBe(0);
      expect(countButtonsByText(container, 'Expediente')).toBe(0);
    });

    await cleanup();
  });

  it('keeps tiny default row context focused on cohort and source instead of repeated timestamps', async () => {
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
        'Cohorte: Beatmaking 101 (beatmaking-101)',
      )).toBe(true);
      expect(hasExactText(
        container,
        'Cohorte: Mixing Bootcamp (mixing-bootcamp) · Fuente: referral',
      )).toBe(true);
      expect(container.textContent).not.toContain('Fuente: landing');
      expect(container.textContent).not.toContain('Creado:');
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
        'Cohorte: Beatmaking 101 (beatmaking-101) · Notas internas',
      )).toBe(true);
      expect(hasExactText(
        container,
        'Cohorte: Mixing Bootcamp (mixing-bootcamp)',
      )).toBe(true);
      expect(container.textContent).not.toContain('Con notas');
      expect(container.textContent).not.toContain('Creado:');
      expect(countOccurrences(container, 'Notas internas')).toBe(1);
    });

    await cleanup();
  });

  it('summarizes shared internal notes once instead of repeating the same row signal', async () => {
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
        crAdminNotes: 'Pidió factura a nombre de empresa.',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasExactText(container, 'Notas internas en todas las inscripciones visibles.')).toBe(true);
      expect(countOccurrences(container, 'Notas internas')).toBe(1);
      expect(hasExactText(
        container,
        'Cohorte: Beatmaking 101 (beatmaking-101)',
      )).toBe(true);
      expect(hasExactText(
        container,
        'Cohorte: Mixing Bootcamp (mixing-bootcamp)',
      )).toBe(true);
      expect(hasExactText(
        container,
        'Cohorte: Beatmaking 101 (beatmaking-101) · Notas internas',
      )).toBe(false);
      expect(container.textContent).not.toContain('Creado:');
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
        'Cohorte única por ahora. Usa Estado para cambiar la vista.',
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
      expect(secondContainer.textContent).not.toContain('Cohorte única por ahora.');
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
        'Estado único en esta vista. Usa cohorte para cambiar la vista.',
      );
      expect(container.textContent).not.toContain('Los filtros se aplican automáticamente al cambiar.');
      expect(container.textContent).not.toContain('Empieza por cohorte y estado.');
    });

    await cleanup();
  });

  it('absorbs a shared source into the single-cohort summary block instead of adding another summary line', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crSource: 'instagram' }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'paid',
        crSource: 'instagram',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.textContent).toContain('Cohorte disponible');
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).toContain('Fuente visible: instagram.');
      expect(container.textContent).not.toContain('Mostrando una sola fuente: instagram.');
      expect(container.textContent).not.toContain('Fuente: instagram');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('omits passive single-choice context when the default view has only one registration', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Beatmaking 101 (beatmaking-101) · Pendiente de pago');
      expect(container.textContent).not.toContain(
        'Vista única por ahora: una cohorte y un estado.',
      );
      expect(container.textContent).not.toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).not.toContain('Cohorte disponible');
      expect(container.textContent).not.toContain('Estado disponible');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
    });

    await cleanup();
  });

  it('absorbs a shared source into the same single-choice summary block instead of adding another summary line', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crSource: 'instagram' }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crSource: 'instagram',
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
      expect(container.textContent).toContain('Fuente visible: instagram.');
      expect(container.textContent).not.toContain('Mostrando una sola fuente: instagram.');
      expect(container.textContent).not.toContain('Fuente: instagram');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('uses the course label inside the dossier while omitting the default public-form source', async () => {
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
        `Curso: Beatmaking 101 (beatmaking-101) · Creado: ${formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-')}`,
      );
      expect(document.body.textContent).not.toContain('Fuente: landing');
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

  it('keeps meaningful custom source context in the dossier header', async () => {
    const referredRegistration = buildRegistration({ crSource: 'instagram' });
    listRegistrationsMock.mockResolvedValue([referredRegistration]);
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: referredRegistration,
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
      expect(dialog.textContent).toContain('Curso: Beatmaking 101 (beatmaking-101) · Fuente: instagram');
      expect(dialog.textContent).not.toContain('Fuente: landing');
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
      const dialog = getDialog();
      expect(hasExactText(dialog, 'Sin datos de contacto. Referencia interna: Party #9.')).toBe(true);
      expect(hasExactText(dialog, 'Party #9')).toBe(false);
      expect(dialog.textContent).not.toContain('Sin correo ni teléfono');
    });

    await cleanup();
  });

  it('keeps empty notes behind one optional dossier action until the admin asks for them', async () => {
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
      const dialogHeadings = Array.from(getDialog().querySelectorAll('h6')).map((element) => (element.textContent ?? '').trim());
      expect(dialogHeadings).not.toContain('Notas internas (opcional)');
      expect(document.body.textContent).not.toContain(
        'Aún no hay notas internas. Registra la primera solo cuando necesites dejar contexto, acuerdos o próximos pasos.',
      );
      expect(getButtonByText(document.body, 'Agregar nota')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(1);
      expect(countButtonsByText(document.body, 'Agregar nota opcional')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar primera nota')).toBe(0);
      expect(countButtonsByText(document.body, 'Abrir notas')).toBe(0);
      expect(countButtonsByText(document.body, 'Editar notas')).toBe(0);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar nota'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const dialogHeadings = Array.from(getDialog().querySelectorAll('h6')).map((element) => (element.textContent ?? '').trim());
      expect(dialogHeadings).toContain('Notas internas');
      expect(getButtonByText(document.body, 'Guardar notas')).toBeTruthy();
      expect(getButtonByText(document.body, 'Cancelar notas')).toBeTruthy();
      expect(getButtonByText(document.body, 'Guardar notas').disabled).toBe(true);
      expect(countButtonsByText(document.body, 'Ocultar editor')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
      expect(document.body.textContent).not.toContain(
        'Aún no hay notas internas. Registra la primera solo cuando necesites dejar contexto, acuerdos o próximos pasos.',
      );
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Cancelar notas'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).not.toContain(
        'Aún no hay notas internas. Registra la primera solo cuando necesites dejar contexto, acuerdos o próximos pasos.',
      );
      expect(Array.from(getDialog().querySelectorAll('h6')).map((element) => (element.textContent ?? '').trim()))
        .not.toContain('Notas internas (opcional)');
      expect(getButtonByText(document.body, 'Agregar nota')).toBeTruthy();
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
      buildRegistration({ crSource: 'instagram' }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'paid',
        crSource: 'instagram',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countOccurrences(
        container,
        'Mostrando una sola cohorte: Beatmaking 101 (beatmaking-101). Fuente visible: instagram.',
      )).toBe(1);
      expect(container.textContent).not.toContain('Mostrando una sola fuente: instagram.');
      expect(container.textContent).not.toContain('Fuente: instagram');
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
      buildRegistration({ crSource: 'instagram' }),
      buildRegistration({
        crId: 102,
        crCourseSlug: 'mixing-bootcamp',
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crSource: 'instagram',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Estado disponible');
      expect(container.textContent).toContain('Pendiente de pago');
      expect(container.textContent).toContain('Fuente visible: instagram.');
      expect(container.textContent).not.toContain('Fuente: instagram');
      expect(container.textContent).toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).toContain('Cohorte: Mixing Bootcamp (mixing-bootcamp)');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('normalizes shared source casing before deciding whether to repeat row source details', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crSource: 'Instagram' }),
      buildRegistration({
        crId: 102,
        crCourseSlug: 'mixing-bootcamp',
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crSource: ' instagram ',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Fuente visible: Instagram.');
      expect(container.textContent).not.toContain('Fuente visible: instagram.');
      expect(container.textContent).not.toContain('Fuente: Instagram');
      expect(container.textContent).not.toContain('Fuente: instagram');
      expect(container.textContent).toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).toContain('Cohorte: Mixing Bootcamp (mixing-bootcamp)');
    });

    await cleanup();
  });

  it('keeps custom row source details visible while omitting default and empty source placeholders', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crSource: 'referral' }),
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
      expect(container.textContent).toContain('Fuente: referral');
      expect(container.textContent).not.toContain('Fuente: landing');
      expect(container.textContent).not.toContain('Fuente: Sin fuente');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('omits missing-created placeholders from sparse rows and dossier context', async () => {
    const sparseRegistration = buildRegistration({
      crCreatedAt: undefined,
      crUpdatedAt: undefined,
      crSource: null,
    });

    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      sparseRegistration,
      buildRegistration({
        crId: 102,
        crCourseSlug: 'mixing-bootcamp',
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crCreatedAt: undefined,
        crUpdatedAt: undefined,
        crSource: 'referral',
      }),
    ]);
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: sparseRegistration,
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasExactText(container, 'Cohorte: Beatmaking 101 (beatmaking-101)')).toBe(true);
      expect(hasExactText(container, 'Cohorte: Mixing Bootcamp (mixing-bootcamp) · Fuente: referral')).toBe(true);
      expect(container.textContent).not.toContain('Creado: -');
      expect(container.textContent).not.toContain('Fuente: Sin fuente');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const dialog = getDialog();
      expect(hasExactText(dialog, 'Curso: Beatmaking 101 (beatmaking-101)')).toBe(true);
      expect(dialog.textContent).not.toContain('Creado: -');
      expect(dialog.textContent).not.toContain('Fuente: Sin fuente');
    });

    await cleanup();
  });

  it('omits missing-cohort placeholders from sparse rows and dossier context', async () => {
    const sparseRegistration = buildRegistration({
      crCourseSlug: '',
      crCreatedAt: undefined,
      crUpdatedAt: undefined,
      crSource: 'referral',
    });

    listCohortsMock.mockResolvedValue([]);
    listRegistrationsMock.mockResolvedValue([
      sparseRegistration,
      buildRegistration({
        crId: 102,
        crCourseSlug: '',
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crCreatedAt: undefined,
        crUpdatedAt: undefined,
        crSource: 'referral',
      }),
    ]);
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: sparseRegistration,
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Fuente visible: referral.');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
      expect(container.textContent).not.toContain('Cohorte:');
      expect(container.textContent).not.toContain('Sin cohorte');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const dialog = getDialog();
      expect(hasExactText(dialog, 'Fuente: referral')).toBe(true);
      expect(dialog.textContent).not.toContain('Curso:');
      expect(dialog.textContent).not.toContain('Sin cohorte');
    });

    await cleanup();
  });

  it('hides a shared missing source instead of turning it into first-run list copy', async () => {
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
      expect(container.textContent).not.toContain('Fuente visible: sin fuente registrada.');
      expect(container.textContent).not.toContain('Mostrando una sola fuente:');
      expect(container.textContent).not.toContain('Fuente: Sin fuente');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('uses a status chip group instead of a dropdown and drops filter onboarding copy after that first filter action', async () => {
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
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(3);
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Todos"]')).toBeNull();
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
      expect(getButtonByAriaLabel(container, clearPaidStatusFilterLabel)).toBeTruthy();
      expect(container.textContent).toContain('Grace Hopper');
      expect(container.textContent).not.toContain('Ada Lovelace');
      expect(container.textContent).not.toContain('Katherine Johnson');
      expect(container.textContent).toContain('Cohorte disponible');
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101)');
      expect(getButtonByAriaLabel(container, clearPaidStatusFilterLabel).textContent?.trim()).toBe('Pagado (1)');
      expect(getButtonByAriaLabel(container, clearPaidStatusFilterLabel).getAttribute('aria-pressed')).toBe('true');
      expect(container.textContent).toContain(activeStatusFilterHelperText);
      expect(container.textContent).not.toContain('Vista filtrada: estado pagado.');
      expect(countButtonsByText(container, 'Mostrar todos los estados')).toBe(0);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, clearPaidStatusFilterLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenLastCalledWith({
        slug: undefined,
        status: undefined,
        limit: 200,
      });
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(3);
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Todos"]')).toBeNull();
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
      expect(container.textContent).toContain('Katherine Johnson');
      expect(container.textContent).not.toContain(activeStatusFilterHelperText);
      expect(container.textContent).not.toContain('Vista filtrada:');
      expect(container.textContent).not.toContain('Estado disponible');
      expect(container.textContent).not.toContain(
        'Los filtros se aplican automáticamente al cambiar. Empieza por cohorte y estado; Ajustar límite aparecerá cuando esta vista llene el lote actual o si ya estás usando un límite personalizado.',
      );
    });

    await cleanup();
  });

  it('keeps a single filtered result focused on shared context instead of repeating an obvious count', async () => {
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
    const { cleanup } = await renderPage(container, '/inscripciones-curso?status=paid');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: 'paid',
        limit: 200,
      });
      expect(container.textContent).toContain('Cohorte disponible');
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101)');
      expect(getButtonByAriaLabel(container, clearPaidStatusFilterLabel).textContent?.trim()).toBe('Pagado (1)');
      expect(getButtonByAriaLabel(container, clearPaidStatusFilterLabel).getAttribute('aria-pressed')).toBe('true');
      expect(container.textContent).toContain(activeStatusFilterHelperText);
      expect(hasExactText(container, 'Filtrar por estado')).toBe(false);
      expect(container.querySelector('[role="group"][aria-label="Filtro de estado activo: Pagado"]')).not.toBeNull();
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Pagado"]')).toBeNull();
      expect(container.textContent).not.toContain('Mostrando 1 inscripción.');
      expect(countButtonsByText(container, 'Mostrar todos los estados')).toBe(0);
      expect(container.querySelector('[data-testid="course-registration-inline-reset"]')).toBeNull();
      expect(countButtonsByText(container, copyVisibleCsvLabel(1))).toBe(0);
    });

    await cleanup();
  });

  it('keeps status-filtered rows action-first once the filter already states the shared status', async () => {
    const paidRegistration = buildRegistration({
      crId: 102,
      crFullName: 'Grace Hopper',
      crEmail: 'grace@example.com',
      crStatus: 'paid',
    });
    const secondPaidRegistration = buildRegistration({
      crId: 103,
      crFullName: 'Katherine Johnson',
      crEmail: 'katherine@example.com',
      crStatus: 'paid',
    });
    const cancelledRegistration = buildRegistration({
      crId: 104,
      crFullName: 'Annie Easley',
      crEmail: 'annie@example.com',
      crStatus: 'cancelled',
    });

    listRegistrationsMock.mockImplementation((params) => Promise.resolve(
      params?.status === 'paid'
        ? [paidRegistration, secondPaidRegistration]
        : params?.status === 'cancelled'
          ? [cancelledRegistration]
          : [paidRegistration, secondPaidRegistration, cancelledRegistration],
    ));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?status=paid');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: 'paid',
        limit: 200,
      });
      expect(getButtonByAriaLabel(container, clearPaidStatusFilterLabel).textContent?.trim()).toBe('Pagado (2)');
      expect(getButtonByAriaLabel(container, clearPaidStatusFilterLabel).getAttribute('aria-pressed')).toBe('true');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Cambiar estado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Katherine Johnson').textContent?.trim()).toBe('Cambiar estado');
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(2);
      expect(countOccurrences(container, 'Estado: Pagado')).toBe(0);
    });

    await cleanup();
  });

  it('keeps the single-result default source out of header and row chrome', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Beatmaking 101 (beatmaking-101) · Pendiente de pago');
      expect(container.textContent).not.toContain('Fuente visible: landing.');
      expect(container.textContent).not.toContain('Fuente: landing');
      expect(container.textContent).not.toContain(`Creado: ${formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-')}`);
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace').textContent?.trim()).toBe('Ada Lovelace');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
    });

    await cleanup();
  });

  it('keeps the single-result row status visible when there is no shared status summary', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Beatmaking 101 (beatmaking-101) · Pendiente de pago');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(0);
      expect(countOccurrences(container, 'Pendiente de pago')).toBe(1);
      expect(countOccurrences(container, 'Estado: Pendiente de pago')).toBe(0);
    });

    await cleanup();
  });

  it('keeps row status actions scan-friendly while the filter group carries the status label', async () => {
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
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(3);
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Todos"]')).toBeNull();
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Pagado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Katherine Johnson').textContent?.trim()).toBe('Cancelado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').getAttribute('title')).toBe(
        'Cambiar estado; actual: Pendiente de pago',
      );
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').getAttribute('title')).toBe(
        'Cambiar estado; actual: Pagado',
      );
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Katherine Johnson').getAttribute('title')).toBe(
        'Cambiar estado; actual: Cancelado',
      );
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').getAttribute('aria-haspopup')).toBe('menu');
      expect(countOccurrences(container, 'Estado:')).toBe(0);
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
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(2);
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Todos"]')).toBeNull();
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Pendiente de pago"]')).not.toBeNull();
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Pagado"]')).not.toBeNull();
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Cancelado"]')).toBeNull();
      expect(container.textContent).toContain('Solo aparecen estados con inscripciones en esta vista.');
    });

    await cleanup();
  });

  it('replaces empty status filter chrome with guidance for custom backend statuses', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crStatus: 'needs_review',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'waitlist',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const customStatusSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-status-filter-unavailable"]',
      );

      expect(customStatusSummary?.textContent).toContain('Sin filtros de estado');
      expect(customStatusSummary?.textContent).toContain(customStatusFilterUnavailableMessage);
      expect(customStatusSummary?.textContent).not.toContain('Usa Cambiar estado');
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.querySelector('[role="group"][aria-label="Filtros de estado de inscripciones"]')).toBeNull();
      expect(hasExactText(container, 'Filtrar por estado')).toBe(false);
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Needs Review');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Waitlist');
      expect(container.textContent).not.toContain('needs_review');
      expect(container.textContent).not.toContain('waitlist');
    });

    await cleanup();
  });

  it('summarizes one shared custom status instead of repeating it across every row action', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crStatus: 'needs_review',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'needs_review',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const customStatusSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-single-custom-status-summary"]',
      );

      expect(customStatusSummary?.textContent).toContain('Estado no estándar');
      expect(customStatusSummary?.textContent).toContain('Needs Review');
      expect(customStatusSummary?.textContent).toContain(customStatusFilterUnavailableMessage);
      expect(container.querySelector('[data-testid="course-registration-status-filter-unavailable"]')).toBeNull();
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Cambiar estado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Cambiar estado');
      expect(countOccurrences(container, 'Needs Review')).toBe(1);
      expect(container.textContent).not.toContain('needs_review');
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
        'Estado único en esta vista. Usa cohorte para cambiar la vista.',
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
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        dossierOnlyScopeHint,
      );
      expect(container.textContent).not.toContain(dossierScopeHint);
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace').textContent?.trim()).toBe('Ada Lovelace');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').className).toContain('MuiButton-text');
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(3);
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Pagado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Katherine Johnson').textContent?.trim()).toBe('Cancelado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').getAttribute('aria-haspopup')).toBe('menu');
      expect(container.textContent).not.toContain('Estado disponible');
      expect(countOccurrences(container, 'Cambiar estado:')).toBe(0);
      expect(countOccurrences(container, 'Estado:')).toBe(0);
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
      expect(document.body.textContent).toContain(openPaymentWorkflowLabel);
      expect(document.body.textContent).toContain('Cancelar inscripción');
      expect(document.body.textContent).not.toContain('Marcar pendiente');
      expect(
        Array.from(document.body.querySelectorAll('[role="menuitem"]')).map((element) => (element.textContent ?? '').trim()),
      ).not.toContain('Abrir expediente de pago');
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
      expect(document.body.textContent).not.toContain(openPaymentWorkflowLabel);
      expect(document.body.textContent).not.toContain('Estado actual:');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Katherine Johnson'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(reopenPendingLabel);
      expect(getMenuItemByText(document.body, reopenPendingLabel).getAttribute('title')).toBe(
        'Usa esta acción para reabrir la inscripción como pendiente.',
      );
      expect(document.body.textContent).not.toContain(openPaymentWorkflowLabel);
      expect(document.body.textContent).not.toContain('Marcar pendiente');
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
      const actions = document.body.querySelector<HTMLElement>('[data-testid="course-registration-dossier-actions"]');

      expect(actions).toBeTruthy();
      expect(getButtonByText(document.body, showSystemEmailsLabel)).toBeTruthy();
      expect(countButtonsByText(actions!, showSystemEmailsLabel)).toBe(1);
      expect(countButtonsByText(actions!, optionalDossierContextActionsLabel)).toBe(1);
      expect(countButtonsByText(actions!, 'Más contexto')).toBe(0);
      expect(countButtonsByText(actions!, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(actions!, 'Agregar seguimiento')).toBe(0);
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

  it('resets expanded system-email history when moving to another dossier', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
      }),
    ]);
    listRegistrationEmailsMock.mockImplementation((registrationId) => (
      Promise.resolve(registrationId === 101 ? [buildEmailEvent()] : [])
    ));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, showSystemEmailsLabel)).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, showSystemEmailsLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, hideSystemEmailsLabel)).toBeTruthy();
      expect(document.body.textContent).toContain(systemEmailHistoryHelperText);
      expect(document.body.textContent).toContain('Recordatorio de pago enviado.');
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Cerrar'));
      await new Promise((resolve) => setTimeout(resolve, 250));
      await flushPromises();
      await flushPromises();
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Abrir expediente de Grace Hopper'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Grace Hopper');
      expect(document.body.textContent).not.toContain(emptySystemEmailHistoryMessage);
      expect(countButtonsByText(document.body, hideSystemEmailsLabel)).toBe(0);
      expect(countButtonsByText(document.body, showSystemEmailsLabel)).toBe(0);
      expect(document.body.textContent).not.toContain(systemEmailHistoryHelperText);
      expect(document.body.textContent).not.toContain('Recordatorio de pago enviado.');
    });

    await cleanup();
  });

  it('keeps first-time dossiers focused by hiding empty system-email guidance until there is history', async () => {
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
      expect(document.body.textContent).not.toContain(emptySystemEmailHistoryMessage);
      expect(document.body.textContent).not.toContain(showSystemEmailsLabel);
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(getButtonByText(document.body, 'Agregar seguimiento')).toBeTruthy();
      expect(document.body.textContent).not.toContain(
        'Aún no hay seguimiento manual. Documenta llamadas, correos o próximos pasos desde aquí. Los cambios de estado y los comprobantes nuevos también quedarán registrados aquí.',
      );
      expect(document.body.textContent).not.toContain('Correos del sistema');
      expect(document.body.textContent).not.toContain(systemEmailHistoryHelperText);
      expect(document.body.querySelector('[data-testid="course-registration-empty-email-history-hint"]')).toBeNull();
    });

    await cleanup();
  });

  it('omits empty system-email guidance when the registration has no email contact', async () => {
    const emailLessRegistration = buildRegistration({ crEmail: null });
    listRegistrationsMock.mockResolvedValue([emailLessRegistration]);
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({ crdRegistration: emailLessRegistration }),
    );
    listRegistrationEmailsMock.mockRejectedValue(new Error('Email service unavailable'));

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
      expect(document.body.textContent).toContain('+593999000111');
      expect(document.body.textContent).toContain(emptyReceiptAlertMessage);
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(getButtonByText(document.body, 'Agregar seguimiento')).toBeTruthy();
      expect(document.body.textContent).not.toContain(emptySystemEmailHistoryMessage);
      expect(document.body.textContent).not.toContain(showSystemEmailsLabel);
      expect(document.body.querySelector('[data-testid="course-registration-empty-email-history-hint"]')).toBeNull();
      expect(document.body.querySelector('[aria-label="Refrescar expediente y correos"]')).toBeNull();
      expect(listRegistrationEmailsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('hides passive empty email guidance when the pay action is already primary', async () => {
    getRegistrationDossierMock.mockResolvedValue(buildDossier({ crdCanMarkPaid: true }));

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
      const actions = document.body.querySelector<HTMLElement>('[data-testid="course-registration-dossier-actions"]');
      const emptyHint = document.body.querySelector<HTMLElement>(
        '[data-testid="course-registration-empty-email-history-hint"]',
      );

      expect(actions).toBeTruthy();
      expect(countButtonsByText(actions!, 'Marcar pagado')).toBe(1);
      expect(countButtonsByText(actions!, optionalDossierContextActionsLabel)).toBe(1);
      expect(countButtonsByText(actions!, 'Más contexto')).toBe(0);
      expect(countButtonsByText(actions!, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(actions!, 'Agregar seguimiento')).toBe(0);
      expect(actions?.textContent).not.toContain(emptySystemEmailHistoryMessage);
      expect(actions?.textContent).not.toContain(showSystemEmailsLabel);
      expect(emptyHint).toBeNull();
      expect(document.body.textContent).not.toContain(emptySystemEmailHistoryMessage);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, optionalDossierContextActionsLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getMenuItemByText(document.body, 'Agregar nota')).toBeTruthy();
      expect(getMenuItemByText(document.body, 'Agregar seguimiento')).toBeTruthy();
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

  it('uses a scoped dossier refresh control once the dossier has saved activity', async () => {
    getRegistrationDossierMock.mockResolvedValue(buildDossier({
      crdReceipts: [buildReceipt()],
    }));

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

  it('keeps empty receipt guidance from referencing mark-paid after the registration is already paid', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration({ crStatus: 'paid' }),
        crdReceipts: [],
        crdCanMarkPaid: false,
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
      expect(document.body.textContent).toContain('Pagado');
      expect(document.body.textContent).toContain(emptyReceiptEvidenceAlertMessage);
      expect(document.body.textContent).not.toContain(emptyReceiptAlertMessage);
      expect(document.body.textContent).not.toContain('habilitar Marcar pagado');
      expect(countButtonsByText(document.body, 'Agregar primer comprobante')).toBe(1);
      expect(countButtonsByText(document.body, 'Marcar pagado')).toBe(0);
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

  it('keeps saved receipts easy to scan by showing section guidance only when the receipt form is active', async () => {
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
      expect(getButtonByText(document.body, 'Agregar comprobante')).toBeTruthy();
      const receiptLink = document.body.querySelector<HTMLAnchorElement>('a[href="https://example.com/receipt.pdf"]');
      expect(receiptLink?.textContent).toContain('receipt.pdf');
      expect(document.body.textContent).not.toContain('Abrir comprobante');
      expect(document.body.textContent).not.toContain(receiptComposerHelpText);
      expect(document.body.textContent).not.toContain(editingReceiptComposerHelpText);
      expect(document.body.textContent).not.toContain(
        'Abre el formulario solo cuando necesites guardar un comprobante o pegar un enlace existente.',
      );
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.querySelector('[data-testid="course-registration-receipt-composer-pane"]')).not.toBeNull();
      expect(document.body.textContent).toContain(receiptComposerHelpText);
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(document.body, 'Abrir acciones para comprobante receipt.pdf'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getMenuItemByText(document.body, 'Editar comprobante')).toBeTruthy();
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, 'Editar comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(editingReceiptComposerHelpText);
      expect(document.body.textContent).not.toContain(receiptComposerHelpText);
    });

    await cleanup();
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
      expect(getMenuItemByText(document.body, openPaymentWorkflowLabel)).toBeTruthy();
      expect(document.body.textContent).not.toContain('Subir comprobante para marcar pagado');
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, openPaymentWorkflowLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Registrar pago de inscripción');
      expect(document.body.textContent).not.toContain('Expediente de inscripción');
      expect(document.body.textContent).not.toContain(
        'Sube un comprobante o pega una URL existente para habilitar Marcar pagado.',
      );
      expect(document.body.textContent).not.toContain(emptySystemEmailHistoryMessage);
      expect(document.body.textContent).not.toContain(showSystemEmailsLabel);
      expect(document.body.textContent).toContain(markPaidReceiptSectionHelpText);
      expect(document.body.textContent).not.toContain(emptyReceiptAlertMessage);
      expect(document.body.textContent).toContain(
        'Primero elige el archivo o pega un enlace; luego podrás ajustar el nombre visible y las notas.',
      );
      expect(hasLabel(document.body, 'Nombre visible')).toBe(false);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(false);
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir archivo')).toBeTruthy();
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
      const dialogHeadings = Array.from(getDialog().querySelectorAll('h6')).map((element) => (element.textContent ?? '').trim());
      expect(dialogHeadings.indexOf('Comprobantes de pago')).toBeGreaterThan(-1);
      expect(dialogHeadings.indexOf('Notas internas (opcional)')).toBeGreaterThan(-1);
      expect(dialogHeadings.indexOf('Comprobantes de pago')).toBeLessThan(dialogHeadings.indexOf('Notas internas (opcional)'));
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
    });

    await cleanup();
  });

  it('closes the first payment workflow directly instead of reopening the empty receipt prompt', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace')).toBeTruthy();
      });

      await act(async () => {
        clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace'));
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(getMenuItemByText(document.body, openPaymentWorkflowLabel)).toBeTruthy();
      });

      await act(async () => {
        clickElement(getMenuItemByText(document.body, openPaymentWorkflowLabel));
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Registrar pago de inscripción');
        expect(getButtonByText(document.body, 'Cerrar pago')).toBeTruthy();
        expect(countButtonsByText(document.body, 'Cerrar')).toBe(0);
        expect(countButtonsByText(document.body, 'Cancelar comprobante')).toBe(0);
        expect(countButtonsByText(document.body, 'Agregar primer comprobante')).toBe(0);
        expect(document.body.textContent).not.toContain(emptyReceiptAlertMessage);
      });

      await act(async () => {
        clickButton(getButtonByText(document.body, 'Cerrar pago'));
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(document.body.textContent).not.toContain('Registrar pago de inscripción');
        expect(countButtonsByText(document.body, 'Cerrar pago')).toBe(0);
        expect(countButtonsByText(document.body, 'Agregar primer comprobante')).toBe(0);
        expect(document.body.textContent).not.toContain(emptyReceiptAlertMessage);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps one explicit close action when the payment workflow cannot load', async () => {
    getRegistrationDossierMock.mockRejectedValue(new Error('Dossier unavailable'));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace')).toBeTruthy();
      });

      await act(async () => {
        clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace'));
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(getMenuItemByText(document.body, openPaymentWorkflowLabel)).toBeTruthy();
      });

      await act(async () => {
        clickElement(getMenuItemByText(document.body, openPaymentWorkflowLabel));
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Registrar pago de inscripción');
        expect(document.body.textContent).toContain('No se pudo cargar el expediente: Dossier unavailable');
        expect(countButtonsByText(document.body, 'Cerrar')).toBe(1);
        expect(countButtonsByText(document.body, 'Cerrar pago')).toBe(0);
        expect(countButtonsByText(document.body, 'Guardar comprobante')).toBe(0);
        expect(countButtonsByText(document.body, 'Agregar primer comprobante')).toBe(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps empty notes secondary in the mark-paid flow until the admin explicitly needs them', async () => {
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
      expect(getMenuItemByText(document.body, openPaymentWorkflowLabel)).toBeTruthy();
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, openPaymentWorkflowLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(markPaidEmptyNotesHelperText);
      expect(getButtonByText(document.body, 'Agregar nota')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Agregar primera nota')).toBe(0);
      expect(document.body.textContent).not.toContain(
        'Aún no hay notas internas. Registra la primera solo cuando necesites dejar contexto, acuerdos o próximos pasos.',
      );
      expect(hasLabel(document.body, 'Notas internas')).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar nota'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'Notas internas')).toBe(true);
      expect(getButtonByText(document.body, 'Guardar notas')).toBeTruthy();
      expect(getButtonByText(document.body, 'Cancelar notas')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
      expect(document.body.textContent).not.toContain(markPaidEmptyNotesHelperText);
    });

    await cleanup();
  });

  it('keeps empty follow-up secondary in the mark-paid flow until the admin explicitly needs it', async () => {
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
      expect(getMenuItemByText(document.body, openPaymentWorkflowLabel)).toBeTruthy();
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, openPaymentWorkflowLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasExactText(document.body, 'Seguimiento (opcional)')).toBe(true);
      expect(document.body.textContent).toContain(markPaidEmptyFollowUpHelperText);
      expect(getButtonByText(document.body, 'Agregar seguimiento opcional')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(0);
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(hasLabel(document.body, 'Nota de seguimiento')).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar seguimiento opcional'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasExactText(document.body, 'Primer seguimiento')).toBe(true);
      expect(hasLabel(document.body, 'Nota de seguimiento')).toBe(true);
      expect(getButtonByText(document.body, 'Guardar seguimiento')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Agregar seguimiento opcional')).toBe(0);
      expect(document.body.textContent).not.toContain(markPaidEmptyFollowUpHelperText);
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
      expect(getMenuItemByText(document.body, openPaymentWorkflowLabel)).toBeTruthy();
      expect(document.body.textContent).not.toContain('Subir comprobante para marcar pagado');
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, openPaymentWorkflowLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Confirmar pago de inscripción');
      expect(document.body.textContent).not.toContain('Expediente de inscripción');
      expect(getButtonByText(document.body, 'Marcar pagado')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Agregar comprobante')).toBe(0);
      expect(document.body.textContent).not.toContain(
        'Sube un comprobante o pega una URL existente para habilitar Marcar pagado.',
      );
      expect(document.body.textContent).not.toContain(
        'Este formulario ya está abierto para registrar el primer comprobante. Guárdalo y luego podrás marcar la inscripción como pagada.',
      );
      expect(document.body.textContent).not.toContain(emptyReceiptAlertMessage);
      expect(document.body.textContent).not.toContain(markPaidEmptyNotesHelperText);
      expect(document.body.textContent).not.toContain(markPaidEmptyFollowUpHelperText);
      expect(hasExactText(document.body, 'Notas internas (opcional)')).toBe(false);
      expect(hasExactText(document.body, 'Seguimiento (opcional)')).toBe(false);
      expect(document.body.textContent).toContain('receipt.pdf');
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Guardar comprobante',
        ),
      ).toBe(false);
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento opcional')).toBe(0);
      expect(getButtonByAriaLabel(document.body, 'Abrir acciones para comprobante receipt.pdf')).toBeTruthy();
    });

    await cleanup();
  });

  it('removes the mark-paid action after success so admins do not repeat the payment update', async () => {
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

    try {
      await waitForExpectation(() => {
        expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace')).toBeTruthy();
      });

      await act(async () => {
        clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace'));
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(getMenuItemByText(document.body, openPaymentWorkflowLabel)).toBeTruthy();
      });

      await act(async () => {
        clickElement(getMenuItemByText(document.body, openPaymentWorkflowLabel));
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(getButtonByText(document.body, 'Marcar pagado')).toBeTruthy();
        expect(countButtonsByText(document.body, 'Marcar pagado')).toBe(1);
      });

      await act(async () => {
        clickButton(getButtonByText(document.body, 'Marcar pagado'));
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Inscripción marcada como pagada.');
        expect(countButtonsByText(document.body, 'Marcar pagado')).toBe(0);
        expect(document.body.textContent).not.toContain(markPaidEmptyNotesHelperText);
        expect(document.body.textContent).not.toContain(markPaidEmptyFollowUpHelperText);
        expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
        expect(countButtonsByText(document.body, 'Agregar seguimiento opcional')).toBe(0);
        expect(countButtonsByText(document.body, 'Cerrar')).toBe(1);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps existing system-email history out of the mark-paid workflow so the pay action stays primary', async () => {
    const registration = buildRegistration();
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: registration,
        crdReceipts: [buildReceipt({ crrRegistrationId: registration.crId })],
        crdCanMarkPaid: true,
      }),
    );
    listRegistrationEmailsMock.mockResolvedValue([
      buildEmailEvent(),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace')).toBeTruthy();
    });

    listRegistrationEmailsMock.mockClear();

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getMenuItemByText(document.body, openPaymentWorkflowLabel)).toBeTruthy();
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, openPaymentWorkflowLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const actions = document.body.querySelector<HTMLElement>('[data-testid="course-registration-dossier-actions"]');

      expect(document.body.textContent).toContain('Confirmar pago de inscripción');
      expect(getButtonByText(document.body, 'Marcar pagado')).toBeTruthy();
      expect(countButtonsByText(actions!, 'Marcar pagado')).toBe(1);
      expect(actions?.textContent).not.toContain(showSystemEmailsLabel);
      expect(document.body.textContent).not.toContain(showSystemEmailsLabel);
      expect(document.body.textContent).not.toContain(hideSystemEmailsLabel);
      expect(document.body.textContent).not.toContain(systemEmailHistoryHelperText);
      expect(document.body.textContent).not.toContain('Recordatorio de pago enviado.');
      expect(document.body.querySelector('[aria-label="Refrescar expediente y correos"]')).toBeNull();
      expect(document.body.querySelector('[aria-label="Refrescar expediente"]')).toBeNull();
      expect(listRegistrationEmailsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps empty internal notes out of the dossier body until the admin explicitly opens them', async () => {
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
      const dialogHeadings = Array.from(getDialog().querySelectorAll('h6')).map((element) => (element.textContent ?? '').trim());
      expect(dialogHeadings).not.toContain('Notas internas (opcional)');
      expect(document.body.textContent).not.toContain(
        'Aún no hay notas internas. Registra la primera solo cuando necesites dejar contexto, acuerdos o próximos pasos.',
      );
      expect(hasLabel(document.body, 'Notas internas')).toBe(false);
      expect(getButtonByText(document.body, 'Agregar nota')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(1);
      expect(countButtonsByText(document.body, 'Agregar nota opcional')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar primera nota')).toBe(0);
      expect(countButtonsByText(document.body, 'Abrir notas')).toBe(0);
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
      clickButton(getButtonByText(document.body, 'Agregar nota'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const dialogHeadings = Array.from(getDialog().querySelectorAll('h6')).map((element) => (element.textContent ?? '').trim());
      expect(dialogHeadings).toContain('Notas internas');
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
      expect(getButtonByText(document.body, 'Agregar seguimiento')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(1);
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(document.body.textContent).not.toContain('0 entradas');
      expect(document.body.textContent).not.toContain('Registrar seguimiento');
      expect(document.body.textContent).not.toContain(firstFollowUpComposerHelpText);
      expect(hasLabel(document.body, 'Tipo')).toBe(false);
      expect(hasLabel(document.body, 'Nota de seguimiento')).toBe(false);
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Guardar seguimiento',
        ),
      ).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar seguimiento'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasExactText(document.body, 'Registrar seguimiento')).toBe(false);
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(document.body.textContent).toContain(firstFollowUpComposerHelpText);
      expect(document.body.textContent).not.toContain(
        'Abre el formulario solo cuando necesites documentar una llamada, mensaje o próximo paso.',
      );
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(0);
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
      expect(getButtonByText(document.body, 'Guardar seguimiento').disabled).toBe(true);
      expect(getButtonByText(document.body, 'Cancelar seguimiento')).toBeTruthy();
    });

    await act(async () => {
      setInputValue(getInputByLabel(document.body, 'Nota de seguimiento'), 'Confirmó que enviará el comprobante.');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Guardar seguimiento').disabled).toBe(false);
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

  it('keeps the first follow-up flow focused until there is something saved to review', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdFollowUps: [],
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
      expect(getButtonByText(document.body, 'Agregar seguimiento')).toBeTruthy();
      expect(document.body.querySelector('[data-testid="course-registration-follow-up-list-pane"]')).toBeNull();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar seguimiento'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.querySelector('[data-testid="course-registration-follow-up-composer-pane"]')).not.toBeNull();
      expect(document.body.querySelector('[data-testid="course-registration-follow-up-list-pane"]')).toBeNull();
    });

    await page.cleanup();

    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdFollowUps: [buildFollowUp()],
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
      expect(getButtonByText(document.body, 'Agregar seguimiento')).toBeTruthy();
      expect(document.body.querySelector('[data-testid="course-registration-follow-up-list-pane"]')).not.toBeNull();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar seguimiento'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.querySelector('[data-testid="course-registration-follow-up-composer-pane"]')).not.toBeNull();
      expect(document.body.querySelector('[data-testid="course-registration-follow-up-list-pane"]')).not.toBeNull();
      expect(document.body.textContent).toContain('Confirmó transferencia');
    });

    await page.cleanup();
  });

  it('keeps history framing out of the follow-up section until there is at least one saved entry', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdFollowUps: [],
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
      expect(getButtonByText(document.body, 'Agregar seguimiento')).toBeTruthy();
      expect(hasExactText(document.body, 'Seguimiento')).toBe(false);
      expect(hasExactText(document.body, 'Historial de seguimiento')).toBe(false);
      expect(hasExactText(document.body, 'Primer seguimiento')).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar seguimiento'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasExactText(document.body, 'Primer seguimiento')).toBe(true);
      expect(hasExactText(document.body, 'Historial de seguimiento')).toBe(false);
      expect(hasExactText(document.body, 'Seguimiento')).toBe(false);
    });

    await page.cleanup();

    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdFollowUps: [buildFollowUp()],
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
      expect(hasExactText(document.body, 'Historial de seguimiento')).toBe(true);
      expect(hasExactText(document.body, 'Primer seguimiento')).toBe(false);
    });

    await page.cleanup();
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
      expect(getButtonByText(document.body, 'Agregar seguimiento')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar seguimiento'));
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

  it('lets admins hide an empty follow-up URL fallback without collapsing drafted details', async () => {
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
      expect(getButtonByText(document.body, 'Agregar seguimiento')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar seguimiento'));
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
      expect(hasLabel(document.body, 'URL del adjunto')).toBe(false);
    });

    await act(async () => {
      setInputValue(getInputByLabel(document.body, 'Asunto'), 'Confirmar transferencia');
      await flushPromises();
      await flushPromises();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Usar enlace existente en lugar de subir adjunto'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'Asunto')).toBe(true);
      expect(hasLabel(document.body, 'URL del adjunto')).toBe(true);
      expect(getInputByLabel(document.body, 'Asunto').value).toBe('Confirmar transferencia');
      expect(getButtonByText(document.body, 'Ocultar enlace existente')).toBeTruthy();
      expect(getButtonByText(document.body, 'Ocultar enlace existente').getAttribute('aria-expanded')).toBe('true');
      expect(countButtonsByText(document.body, 'Ocultar detalles opcionales')).toBe(0);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Ocultar enlace existente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'Asunto')).toBe(true);
      expect(getInputByLabel(document.body, 'Asunto').value).toBe('Confirmar transferencia');
      expect(hasLabel(document.body, 'URL del adjunto')).toBe(false);
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir adjunto')).toBeTruthy();
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir adjunto').getAttribute('aria-expanded')).toBe('false');
      expect(countButtonsByText(document.body, 'Ocultar enlace existente')).toBe(0);
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
      expect(countButtonsByText(document.body, 'Acciones')).toBe(0);
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
      expect(countButtonsByText(document.body, 'Acciones')).toBe(0);
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

  it('disambiguates repeated receipt filenames so saved receipt actions stay distinct', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [
          buildReceipt(),
          buildReceipt({
            crrId: 302,
            crrFileUrl: 'https://example.com/receipt-copy.pdf',
            crrFileName: 'receipt.pdf',
          }),
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
      expect(document.body.textContent).toContain('receipt.pdf · #301');
      expect(document.body.textContent).toContain('receipt.pdf · #302');
      expect(
        document.body.querySelector('button[aria-label="Abrir acciones para comprobante receipt.pdf"]'),
      ).toBeNull();
      expect(
        document.body.querySelectorAll('button[aria-label="Abrir acciones para comprobante receipt.pdf · #301"]'),
      ).toHaveLength(1);
      expect(
        document.body.querySelectorAll('button[aria-label="Abrir acciones para comprobante receipt.pdf · #302"]'),
      ).toHaveLength(1);
    });

    await cleanup();
  });

  it('disambiguates repeated follow-up subjects so saved follow-up actions stay distinct', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdFollowUps: [
          buildFollowUp(),
          buildFollowUp({
            crfId: 402,
            crfSubject: 'Confirmó transferencia',
            crfNotes: 'Pidió que confirmemos recepción.',
          }),
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
      expect(
        document.body.querySelector('button[aria-label="Abrir acciones para seguimiento Confirmó transferencia"]'),
      ).toBeNull();
      expect(
        document.body.querySelectorAll('button[aria-label="Abrir acciones para seguimiento Confirmó transferencia · #401"]'),
      ).toHaveLength(1);
      expect(
        document.body.querySelectorAll('button[aria-label="Abrir acciones para seguimiento Confirmó transferencia · #402"]'),
      ).toHaveLength(1);
      expect(countOccurrences(document.body, 'Confirmó transferencia')).toBe(2);
      expect(document.body.textContent).toContain('2 entradas');
    });

    await cleanup();
  });

  it('deduplicates repeated dossier artifacts before showing saved-item counts or actions', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [buildReceipt(), buildReceipt()],
        crdFollowUps: [buildFollowUp(), buildFollowUp()],
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
      expect(document.body.textContent).not.toContain('2 guardados');
      expect(document.body.textContent).not.toContain('2 entradas');
      expect(
        document.body.querySelectorAll('button[aria-label="Abrir acciones para comprobante receipt.pdf"]'),
      ).toHaveLength(1);
      expect(
        document.body.querySelectorAll('button[aria-label="Abrir acciones para seguimiento Confirmó transferencia"]'),
      ).toHaveLength(1);
      expect(countOccurrences(document.body, 'receipt.pdf')).toBe(1);
      expect(countOccurrences(document.body, 'Confirmó transferencia')).toBe(1);
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
  }, 20_000);

  it('keeps a filtered-empty view focused on clearing filters instead of offering duplicate recovery actions', async () => {
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
        'No hay inscripciones en la vista actual: cohorte Beatmaking 101 (beatmaking-101) · estado pagado · límite 50. Revisa los filtros o restablece la vista si esperabas resultados.',
      );
      expect(container.textContent).not.toContain('Restablece la vista o usa refrescar si esperabas resultados.');
      expect(container.textContent).not.toContain(
        'Los filtros se aplican automáticamente al cambiar. Empieza por cohorte y estado; usa Ajustar límite solo cuando necesites revisar un lote distinto. Ajusta la vista o usa refrescar si esperabas resultados.',
      );
      expect(container.textContent).not.toContain('Vista filtrada:');
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.textContent).not.toContain('Esta vista ya está filtrada por ese estado.');
      expect(Array.from(container.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Ajustar límite (50)')).toBe(false);
      expect(container.textContent).not.toContain('No hay inscripciones para esta vista.');
      expect(countButtonsByText(container, 'Restablecer vista')).toBe(1);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(container.querySelector('[data-testid="course-registration-header-actions"]')).toBeNull();
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
      expect(container.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(container.textContent).not.toContain('Todavía no hay inscripciones para mostrar en esta vista.');
      expect(container.textContent).not.toContain('No hay inscripciones con los filtros actuales:');
      expect(Array.from(container.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Ajustar límite')).toBe(false);
      expect(Array.from(container.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Restablecer filtros')).toBe(false);
      expect(container.querySelector('[data-testid="course-registration-initial-empty-state"]')).not.toBeNull();
      expect(
        container.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(container.querySelector('a[href="/configuracion/cursos"]')).toBeNull();
    });

    await cleanup();
  });

  it('keeps the limit-only empty view focused on refresh instead of a useless reset action', async () => {
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?limit=50');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: undefined,
        limit: 50,
      });
      expect(container.textContent).toContain(
        'No hay inscripciones con el límite actual: límite 50. Usa refrescar si esperabas resultados.',
      );
      expect(countButtonsByText(container, 'Restablecer límite')).toBe(0);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(1);
    });

    await cleanup();
  });

  it('keeps a URL-only cohort slug as a filter instead of treating it as a configured form', async () => {
    listCohortsMock.mockResolvedValue([]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?slug=archived-course');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: 'archived-course',
        status: undefined,
        limit: 200,
      });
      expect(container.querySelector('[data-testid="course-registration-initial-empty-state"]')).toBeNull();
      expect(container.textContent).toContain(
        'No hay inscripciones con los filtros actuales: cohorte archived-course. Revisa los filtros o restablece la vista si esperabas resultados.',
      );
      expect(container.textContent).not.toContain('Todavía no hay inscripciones para archived-course.');
      expect(container.textContent).not.toContain(initialEmptyStateFormActionLabel);
      expect(container.querySelector('a[href="/inscripcion/archived-course"]')).toBeNull();
      expect(countButtonsByText(container, 'Mostrar todas las cohortes')).toBe(1);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      clickButton(getButtonByText(container, 'Mostrar todas las cohortes'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenLastCalledWith({
        slug: undefined,
        status: undefined,
        limit: 200,
      });
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState?.textContent).toContain(initialEmptyStateConfigMessage);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateConfigActionLabel);
      expect(emptyState?.querySelector('a[href^="/inscripcion/"]')).toBeNull();
    });

    await cleanup();
  });

  it('keeps reset attached to the current-view summary when a custom limit still shows multiple registrations', async () => {
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
    const { cleanup } = await renderPage(container, '/inscripciones-curso?limit=50');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: undefined,
        limit: 50,
      });
      expect(container.textContent).toContain('Vista actual');
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101) · Pendiente de pago');
      expect(container.textContent).toContain('Límite actual: hasta 50 inscripciones.');
      expect(container.textContent).not.toContain('Mostrando 2 inscripciones.');
      expect(container.querySelector('[data-testid="course-registration-inline-reset"]')?.textContent?.trim()).toBe('Restablecer límite');
      expect(countButtonsByText(container, 'Restablecer límite')).toBe(1);
      expect(getButtonByText(container, copyVisibleCsvLabel(2))).toBeTruthy();
      expect(countButtonsByText(container, 'Copiar visibles filtrado')).toBe(0);
      expect(countButtonsByText(container, 'Copiar visibles')).toBe(0);
    });

    await cleanup();
  });

  it('keeps a custom limit passive without reintroducing the default public-form source', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?limit=50');

    await waitForExpectation(() => {
      const contextSummary = container.querySelector<HTMLElement>('[data-testid="course-registration-single-choice-context"]');
      expect(container.textContent).toContain('Vista actual');
      expect(contextSummary?.textContent?.trim()).toBe('Límite actual: hasta 50 inscripciones.');
      expect(container.querySelectorAll('[data-testid="course-registration-single-choice-context"]')).toHaveLength(1);
      expect(countOccurrences(container, 'Fuente visible: landing.')).toBe(0);
      expect(countOccurrences(container, 'Límite actual: hasta 50 inscripciones.')).toBe(1);
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
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Beatmaking 101 (beatmaking-101) · Pendiente de pago');
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

  it('keeps tiny single-status lists focused on passive count and row actions instead of export chrome', async () => {
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
      expect(
        container.querySelector('[data-testid="course-registration-single-choice-context"]')?.textContent?.trim(),
      ).toBe('Mostrando 2 inscripciones en esta vista.');
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(container.textContent).not.toContain(`Creado: ${formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-')}`);
      expect(countButtonsByText(container, copyVisibleCsvLabel(2))).toBe(0);
      expect(
        Array.from(container.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Copiar visibles (2 filas)',
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('deduplicates repeated registrations before showing row actions or export utilities', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
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
      expect(
        container.querySelector('[data-testid="course-registration-single-choice-context"]')?.textContent?.trim(),
      ).toBe('Mostrando 2 inscripciones en esta vista.');
      expect(container.querySelectorAll('button[aria-label="Abrir expediente de Ada Lovelace"]')).toHaveLength(1);
      expect(container.querySelectorAll('button[aria-label="Cambiar estado para Ada Lovelace"]')).toHaveLength(1);
      expect(container.querySelectorAll('button[aria-label="Abrir expediente de Grace Hopper"]')).toHaveLength(1);
      expect(countOccurrences(container, 'Ada Lovelace')).toBe(1);
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(countButtonsByText(container, copyVisibleCsvLabel(2))).toBe(0);
    });

    await cleanup();
  });

  it('merges duplicate registration records so the surviving row keeps the clearest identity', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crPartyId: null,
        crFullName: '   ',
        crEmail: '   ',
        crPhoneE164: null,
        crSource: '   ',
        crAdminNotes: '   ',
      }),
      buildRegistration({
        crId: 101,
        crPartyId: 9,
        crFullName: 'Ada Lovelace',
        crEmail: 'ada@example.com',
        crPhoneE164: '+593999000111',
        crSource: 'instagram',
        crAdminNotes: 'Confirmó pago por WhatsApp.',
      }),
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
      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace')).toBeTruthy();
      expect(container.querySelector('button[aria-label="Abrir expediente de registro #101"]')).toBeNull();
      expect(countOccurrences(container, 'Registro #101')).toBe(0);
      expect(container.textContent).toContain('ada@example.com · +593999000111');
      expect(container.textContent).toContain('Fuente: instagram · Notas internas');
      expect(container.querySelectorAll('button[aria-label="Cambiar estado para Ada Lovelace"]')).toHaveLength(1);
      expect(countButtonsByText(container, copyVisibleCsvLabel(2))).toBe(0);
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
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pendiente de pago').textContent?.trim()).toBe('Pendiente de pago (1)');
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pagado').textContent?.trim()).toBe('Pagado (1)');
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Cancelado').textContent?.trim()).toBe('Cancelado (1)');
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Todos"]')).toBeNull();
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

  it('keeps three-row default lists focused on scanning instead of export chrome', async () => {
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
      const listUtilities = container.querySelector<HTMLElement>('[data-testid="course-registration-list-utilities"]');
      const createdAtLabel = formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-');
      expect(listUtilities).toBeNull();
      expect(container.textContent).not.toContain('Mostrando 3 inscripciones en esta vista.');
      expect(container.querySelector('[data-testid="course-registration-shared-created-at-summary"]')).toBeNull();
      expect(container.textContent).not.toContain(`Misma fecha de registro: ${createdAtLabel}.`);
      expect(countOccurrences(container, `Creado: ${createdAtLabel}`)).toBe(0);
      expect(countButtonsByText(container, copyVisibleCsvLabel(3))).toBe(0);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(container.querySelector('[data-testid="course-registration-header-actions"]')).toBeNull();
    });

    await cleanup();
  });

  it('keeps medium default lists focused until search and export are useful', async () => {
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
      buildRegistration({
        crId: 104,
        crFullName: 'Dorothy Vaughan',
        crEmail: 'dorothy@example.com',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(4);
      expect(hasLabel(container, localSearchLabel)).toBe(false);
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(container.textContent).not.toContain('Mostrando 4 inscripciones en esta vista.');
      expect(countButtonsByText(container, copyVisibleCsvLabel(4))).toBe(0);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
    });

    await cleanup();
  });

  it('adds local search for busy loaded lists without re-querying cohort or status filters', async () => {
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, (index) => (
        index === 8
          ? {
            crFullName: 'Nina Simone',
            crEmail: 'nina@example.com',
            crStatus: 'paid',
          }
          : {}
      )),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(hasLabel(container, 'Buscar registros cargados')).toBe(false);
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre, email, teléfono o estado',
      );
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        dossierOnlyScopeHint,
      );
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'nina');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Nina Simone');
      expect(container.textContent).not.toContain('Estudiante 1');
      expect(container.textContent).toContain('Mostrando 1 de 9 inscripciones cargadas.');
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(container.textContent).not.toContain(dossierOnlyScopeHint);
      expect(container.textContent).not.toContain('Mostrando 9 inscripciones en esta vista.');
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(countButtonsByText(container, copyVisibleCsvLabel(1))).toBe(0);
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Limpiar búsqueda'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect((getInputByLabel(container, localSearchLabel) as HTMLInputElement).value).toBe('');
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps hidden default and empty sources out of busy-list local search', async () => {
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, (index) => ({
        crSource: index % 2 === 0 ? 'landing' : null,
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre, email o teléfono',
      );
      expect(container.textContent).not.toContain('Fuente: landing');
      expect(container.textContent).not.toContain('Fuente: Sin fuente');
      expect(container.textContent).not.toContain('Fuente visible: landing.');
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'landing');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain('No hay coincidencias para "landing" en las 9 inscripciones cargadas.');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('matches busy-list local search against accent-free cohort names', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'live-production', ccTitle: 'Producción en vivo' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Ana Torres',
        crEmail: 'ana@example.com',
        crCourseSlug: 'live-production',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Carlos Vega',
        crEmail: 'carlos@example.com',
        crCourseSlug: 'live-production',
      }),
      ...buildRegistrations(7, (index) => ({
        crId: 201 + index,
        crPartyId: 21 + index,
        crCourseSlug: 'beatmaking-101',
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre, email, teléfono o curso',
      );
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'produccion');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(container.textContent).toContain('Ana Torres');
      expect(container.textContent).toContain('Carlos Vega');
      expect(container.textContent).not.toContain('Estudiante 1');
      expect(container.textContent).toContain('Mostrando 2 de 9 inscripciones cargadas.');
      expect(container.textContent).toContain('Mostrando una sola cohorte: Producción en vivo (live-production).');
      expect(container.textContent).not.toContain('No hay coincidencias para "produccion"');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('uses registration ids as the busy-list search fallback when rows lack contact identity', async () => {
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, (index) => ({
        crId: 501 + index,
        crPartyId: null,
        crFullName: '   ',
        crEmail: '   ',
        crPhoneE164: null,
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const searchInput = getInputByLabel(container, localSearchLabel);
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(searchInput.getAttribute('placeholder')).toBe('Registro');
      expect(searchInput.getAttribute('placeholder')).not.toBe('Nombre, email, teléfono');
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.textContent).toContain('Registro #501');
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'registro #506');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(getButtonByAriaLabel(container, 'Abrir expediente de registro #506')).toBeTruthy();
      expect(container.textContent).toContain('Registro #506');
      expect(container.textContent).not.toContain('Registro #501');
      expect(container.textContent).toContain('Mostrando 1 de 9 inscripciones cargadas.');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('summarizes shared status, cohort, and source once after local search narrows a mixed list', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
      { ccSlug: 'live-production', ccTitle: 'Producción en vivo' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Ada Lovelace',
        crEmail: 'ada@example.com',
        crCourseSlug: 'beatmaking-101',
        crSource: 'landing',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Nina Simone',
        crEmail: 'nina1@example.com',
        crCourseSlug: 'mixing-bootcamp',
        crSource: 'referral',
        crStatus: 'paid',
      }),
      buildRegistration({
        crId: 103,
        crFullName: 'Nina Garcia',
        crEmail: 'nina2@example.com',
        crCourseSlug: 'mixing-bootcamp',
        crSource: ' referral ',
        crStatus: 'paid',
      }),
      ...buildRegistrations(6, (index) => ({
        crId: 200 + index,
        crPartyId: 40 + index,
        crFullName: `Estudiante ${index + 1}`,
        crEmail: `student${index + 1}@example.com`,
        crCourseSlug: index % 2 === 0 ? 'beatmaking-101' : 'live-production',
        crSource: index % 2 === 0 ? 'instagram' : 'landing',
        crStatus: index % 3 === 0 ? 'pending_payment' : 'cancelled',
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const searchInput = getInputByLabel(container, localSearchLabel);
      expect(searchInput.getAttribute('placeholder')).toBe('Nombre, email, teléfono, estado, fuente o curso');
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'nina');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(container.textContent).toContain('Nina Simone');
      expect(container.textContent).toContain('Nina Garcia');
      expect(container.textContent).toContain('Mostrando 2 de 9 inscripciones cargadas.');
      expect(container.textContent).toContain(
        'Estado visible: Pagado. Mostrando una sola cohorte: Mixing Bootcamp (mixing-bootcamp). Fuente visible: referral.',
      );
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Nina Simone').textContent?.trim()).toBe('Cambiar estado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Nina Garcia').textContent?.trim()).toBe('Cambiar estado');
      expect(countButtonsByText(container, 'Pagado')).toBe(0);
      expect(container.textContent).not.toContain('Cohorte: Mixing Bootcamp (mixing-bootcamp)');
      expect(container.textContent).not.toContain('Fuente: referral');
    });

    await cleanup();
  });

  it('summarizes shared internal notes once after local search narrows a mixed list', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
      { ccSlug: 'live-production', ccTitle: 'Producción en vivo' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Nina Simone',
        crEmail: 'nina1@example.com',
        crCourseSlug: 'beatmaking-101',
        crAdminNotes: 'Pidió factura.',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Nina Garcia',
        crEmail: 'nina2@example.com',
        crCourseSlug: 'mixing-bootcamp',
        crAdminNotes: 'Confirmó pago por WhatsApp.',
      }),
      ...buildRegistrations(7, (index) => ({
        crId: 200 + index,
        crPartyId: 40 + index,
        crFullName: `Estudiante ${index + 1}`,
        crEmail: `student${index + 1}@example.com`,
        crCourseSlug: index % 2 === 0 ? 'beatmaking-101' : 'live-production',
        crAdminNotes: index === 0 ? 'Revisar cupo.' : null,
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.textContent).toContain('Notas internas');
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'nina');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(hasExactText(container, 'Notas internas en todas las inscripciones visibles.')).toBe(true);
      expect(countOccurrences(container, 'Notas internas')).toBe(1);
      expect(hasExactText(container, 'Cohorte: Beatmaking 101 (beatmaking-101) · Notas internas')).toBe(false);
      expect(hasExactText(container, 'Cohorte: Mixing Bootcamp (mixing-bootcamp) · Notas internas')).toBe(false);
      expect(container.textContent).toContain('Nina Simone');
      expect(container.textContent).toContain('Nina Garcia');
      expect(listRegistrationsMock).toHaveBeenCalledTimes(1);
    });

    await cleanup();
  });

  it('lets admins find registrations by internal note text without exposing the note in the row', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Ada Lovelace',
        crEmail: 'ada@example.com',
        crAdminNotes: 'Necesita beca parcial antes de confirmar.',
      }),
      ...buildRegistrations(8, (index) => ({
        crId: 200 + index,
        crPartyId: 40 + index,
        crFullName: `Estudiante ${index + 1}`,
        crEmail: `student${index + 1}@example.com`,
        crAdminNotes: null,
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre, email, teléfono o nota',
      );
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'beca parcial');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).not.toContain('Estudiante 1');
      expect(container.textContent).toContain('Notas internas');
      expect(container.textContent).not.toContain('Necesita beca parcial antes de confirmar.');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('clears local search when admins switch server filters so the list is not double-filtered', async () => {
    const pendingRegistrations = buildRegistrations(9);
    const initialRegistrations = [
      ...pendingRegistrations,
      buildRegistration({
        crId: 210,
        crPartyId: 21,
        crFullName: 'Nina Simone',
        crEmail: 'nina@example.com',
        crStatus: 'paid',
      }),
    ];
    listRegistrationsMock.mockImplementation((params) =>
      Promise.resolve(params?.status === 'pending_payment' ? pendingRegistrations : initialRegistrations));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(10);
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'nina');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Nina Simone');
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pendiente de pago'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenLastCalledWith({
        slug: undefined,
        status: 'pending_payment',
        limit: 200,
      });
      expect((getInputByLabel(container, localSearchLabel) as HTMLInputElement).value).toBe('');
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.textContent).toContain('Estudiante 1');
      expect(container.textContent).not.toContain('Nina Simone');
      expect(container.textContent).not.toContain('No hay coincidencias para "nina"');
    });

    await cleanup();
  });

  it('uses one explicit empty-search recovery action instead of an icon-only clear control', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(9));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'sin coincidencias');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain('No hay coincidencias para "sin coincidencias" en las 9 inscripciones cargadas.');
      expect(container.textContent).not.toContain('Búsqueda local en el lote cargado (9 inscripciones).');
      expect(container.textContent).not.toContain('Mostrando 0 de 9 inscripciones cargadas.');
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(1);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).toBeNull();
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Limpiar búsqueda'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect((getInputByLabel(container, localSearchLabel) as HTMLInputElement).value).toBe('');
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps the visible CSV export scoped to local search results', async () => {
    const writeTextMock = jest.fn<(text: string) => Promise<void>>().mockResolvedValue(undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText: writeTextMock },
    });
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Nina Simone',
        crEmail: 'nina1@example.com',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Nina Garcia',
        crEmail: 'nina2@example.com',
      }),
      ...buildRegistrations(7, (index) => ({
        crId: 201 + index,
        crPartyId: 20 + index,
        crFullName: `Estudiante ${index + 1}`,
        crEmail: `student${index + 1}@example.com`,
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'nina');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(2);
      const copyButton = getButtonByText(container, copyVisibleCsvLabel(2));
      expect(copyButton).toBeTruthy();
      expect(copyButton.textContent).not.toContain('CSV');
      expect(copyButton.getAttribute('aria-label')).toBe('Copiar 2 filas visibles como CSV');
      expect(copyButton.getAttribute('title')).toBe('Copia solo las filas visibles en esta vista como CSV.');
      expect(container.textContent).toContain('Mostrando 2 de 9 inscripciones cargadas.');
      expect(container.textContent).not.toContain('Mostrando 9 inscripciones en esta vista.');
    });

    await act(async () => {
      clickButton(getButtonByText(container, copyVisibleCsvLabel(2)));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(writeTextMock).toHaveBeenCalledTimes(1);
      const csv = writeTextMock.mock.calls[0]?.[0] ?? '';
      expect(csv.split('\n')).toHaveLength(3);
      expect(csv).toContain('"Nina Simone"');
      expect(csv).toContain('"Nina Garcia"');
      expect(csv).not.toContain('"Estudiante 1"');
      expect(container.textContent).toContain('Copiado CSV (2 filas)');
      expect(countButtonsByText(container, copyVisibleCsvLabel(2))).toBe(0);
    });

    await cleanup();
  });

  it('clears stale CSV feedback when local search changes the visible export scope', async () => {
    const writeTextMock = jest.fn<(text: string) => Promise<void>>().mockResolvedValue(undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText: writeTextMock },
    });
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Nina Simone',
        crEmail: 'nina1@example.com',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Nina Garcia',
        crEmail: 'nina2@example.com',
      }),
      ...buildRegistrations(7, (index) => ({
        crId: 201 + index,
        crPartyId: 20 + index,
        crFullName: `Estudiante ${index + 1}`,
        crEmail: `student${index + 1}@example.com`,
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'nina');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(getButtonByText(container, copyVisibleCsvLabel(2))).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, copyVisibleCsvLabel(2)));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(writeTextMock).toHaveBeenCalledTimes(1);
      expect(container.textContent).toContain('Copiado CSV (2 filas)');
      expect(countButtonsByText(container, copyVisibleCsvLabel(2))).toBe(0);
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Limpiar búsqueda'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect((getInputByLabel(container, localSearchLabel) as HTMLInputElement).value).toBe('');
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.textContent).not.toContain('Copiado CSV (2 filas)');
      expect(getButtonByText(container, copyVisibleCsvLabel(9))).toBeTruthy();
      expect(listRegistrationsMock).toHaveBeenCalledTimes(1);
    });

    await cleanup();
  });

  it('folds capped default-list guidance into local search instead of adding utility copy', async () => {
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
      const listUtilities = container.querySelector<HTMLElement>('[data-testid="course-registration-list-utilities"]');
      expect(container.textContent).not.toContain('Total: 200');
      expect(container.textContent).not.toContain('Pagadas:');
      expect(container.textContent).not.toContain('Pendientes:');
      expect(container.textContent).not.toContain('Canceladas:');
      expect(container.textContent).not.toContain(
        'Los totales de arriba resumen esta vista y usan los mismos colores que cada estado.',
      );
      expect(container.textContent).not.toContain('Leyenda de estados:');
      expect(container.textContent).not.toContain('Mostrando 200 inscripciones en esta vista.');
      expect(container.textContent).toContain(
        'Busca dentro de las 200 inscripciones cargadas. Usa Ajustar límite si necesitas revisar más registros.',
      );
      expect(listUtilities?.textContent).not.toContain('Se cargó el límite de 200 inscripciones');
      expect(
        Array.from(container.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === copyVisibleCsvLabel(200),
        ),
      ).toBe(true);
      expect(listUtilities).not.toBeNull();
      expect(container.querySelector('[data-testid="course-registration-header-actions"]')).toBeNull();
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(getButtonByText(listUtilities!, copyVisibleCsvLabel(200))).toBeTruthy();
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
      expect(container.textContent).not.toContain('Mostrando 50 inscripciones.');
      expect(container.textContent).not.toContain('Mostrando 50 inscripciones con los filtros actuales.');
      expect(container.textContent).not.toContain('Vista filtrada: límite 50.');
      expect(getButtonByText(container, 'Restablecer límite')).toBeTruthy();
      expect(getButtonByText(container, copyVisibleCsvLabel(50))).toBeTruthy();
      expect(countButtonsByText(container, 'Copiar visibles filtrado')).toBe(0);
      expect(countButtonsByText(container, 'Copiar visibles')).toBe(0);
      expect(
        Array.from(container.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Copiar visibles filtrado (50 filas)',
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('keeps the initial loading state focused on the first-result setup instead of filters or refresh actions', async () => {
    listRegistrationsMock.mockImplementation(() => new Promise(() => undefined));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Cargando inscripciones…');
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Cohorte disponible');
      expect(container.textContent).not.toContain('Estado disponible');
      expect(Array.from(container.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Refrescar lista')).toBe(false);
    });

    await cleanup();
  });

  it('keeps first-load registration errors focused on retry instead of showing unusable filters', async () => {
    listRegistrationsMock.mockRejectedValue(new Error('Backend unavailable'));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('No se pudieron cargar las inscripciones: Backend unavailable');
      expect(countButtonsByText(container, 'Reintentar inscripciones')).toBe(1);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.textContent).not.toContain(singleCohortInitialEmptyStateMessage);
      expect(container.textContent).not.toContain(initialEmptyStateConfigMessage);
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Cohorte disponible');
      expect(container.textContent).not.toContain('Estado disponible');
      expect(container.querySelector('[data-testid="course-registration-header-actions"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
    });

    await cleanup();
  });

  it('scopes the header retry to cohort loading failures when registrations already loaded', async () => {
    listCohortsMock.mockRejectedValueOnce(new Error('Cohort service unavailable'));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledTimes(1);
      const cohortFallback = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-cohort-filter-unavailable"]',
      );

      expect(cohortFallback?.textContent).toContain(cohortFilterUnavailableMessage);
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(getButtonByText(container, 'Reintentar cohortes')).toBeTruthy();
      expect(countButtonsByText(container, 'Reintentar cohortes')).toBe(1);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Reintentar cohortes'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listCohortsMock).toHaveBeenCalledTimes(2);
      expect(listRegistrationsMock).toHaveBeenCalledTimes(1);
      expect(container.querySelector('[data-testid="course-registration-cohort-filter-unavailable"]')).toBeNull();
      expect(container.textContent).not.toContain(cohortFilterUnavailableMessage);
      expect(countButtonsByText(container, 'Reintentar cohortes')).toBe(0);
    });

    await cleanup();
  });

  it('keeps the first-run empty state focused on sharing the configured cohort form instead of list actions', async () => {
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(singleCohortInitialEmptyStateMessage);
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Comparte el formulario público');
      expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
      expect(emptyState?.textContent).not.toContain('Abre el formulario público y comparte el enlace');
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(emptyState?.querySelector('a[href="/configuracion/cursos"]')).toBeNull();
      expect(container.textContent).not.toContain('Todavía no hay inscripciones para mostrar en esta vista.');
      expect(countOccurrences(container, singleCohortInitialEmptyStateMessage)).toBe(1);
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('tamaño del lote');
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
      expect(Array.from(container.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Refrescar lista')).toBe(false);
      expect(
        Array.from(container.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim().startsWith('Copiar visibles'),
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('keeps first-run cohort labels from repeating slug-only titles', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'beatmaking-101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Todavía no hay inscripciones para beatmaking-101. Cuando llegue la primera podrás revisar pago, seguimiento y correos aquí.',
      );
      expect(emptyState?.textContent).not.toContain('beatmaking-101 (beatmaking-101)');
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
    });

    await cleanup();
  });

  it('waits for cohort context before showing first-run empty-state actions', async () => {
    listCohortsMock.mockImplementation(() => new Promise<CourseCohortOptionDTO[]>(() => undefined));
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const loadingState = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-initial-cohort-loading"]',
      );
      expect(loadingState?.textContent).toContain(initialCohortResolutionMessage);
      expect(container.querySelector('[data-testid="course-registration-initial-empty-state"]')).toBeNull();
      expect(container.textContent).not.toContain(initialEmptyStateConfigMessage);
      expect(container.textContent).not.toContain(initialEmptyStateMultiCohortMessage);
      expect(container.textContent).not.toContain(singleCohortInitialEmptyStateMessage);
      expect(container.textContent).not.toContain('Todavía no hay inscripciones para mostrar en esta vista.');
      expect(container.querySelector('a[href="/configuracion/cursos"]')).toBeNull();
      expect(container.querySelector('a[href^="/inscripcion/"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(
        Array.from(container.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim().startsWith('Copiar visibles'),
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('keeps first-run cohort failures focused on retry instead of filters and empty-list chrome', async () => {
    listCohortsMock.mockRejectedValueOnce(new Error('Cohort service unavailable'));
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const errorState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-cohort-error"]');
      expect(errorState?.textContent).toContain(initialCohortErrorMessage);
      expect(countButtonsByText(container, 'Reintentar cohortes')).toBe(1);
      expect(container.querySelector('[data-testid="course-registration-header-actions"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-initial-empty-state"]')).toBeNull();
      expect(container.textContent).not.toContain(initialEmptyStateConfigMessage);
      expect(container.textContent).not.toContain(singleCohortInitialEmptyStateMessage);
      expect(container.textContent).not.toContain('Todavía no hay inscripciones para mostrar en esta vista.');
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Reintentar cohortes'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listCohortsMock).toHaveBeenCalledTimes(2);
      expect(container.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(container.querySelector('[data-testid="course-registration-initial-cohort-error"]')).toBeNull();
    });

    await cleanup();
  });

  it('keeps course setup as the first-run action when no cohort is configured yet', async () => {
    listCohortsMock.mockResolvedValue([]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState?.textContent).toContain(initialEmptyStateConfigMessage);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateConfigActionLabel);
      expect(emptyState?.querySelector('a[href^="/inscripcion/"]')).toBeNull();
    });

    await cleanup();
  });

  it('keeps multi-cohort first-run setup focused on choosing an existing cohort to share', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(initialEmptyStateMultiCohortMessage);
      expect(emptyState?.textContent).not.toContain(initialEmptyStateConfigMessage);
      expect(emptyState?.textContent).not.toContain('Elige qué formulario público compartir');
      expect(emptyState?.textContent).not.toContain('Elige en Configuración de cursos');
      expect(emptyState?.textContent).not.toContain('copiar o abrir');
      expect(emptyState?.textContent).not.toContain('Ver cohortes');
      expect(countOccurrences(emptyState!, 'formulario público')).toBe(0);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateMultiCohortActionLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      expect(emptyState?.querySelector('a[href^="/inscripcion/"]')).toBeNull();
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
    });

    await cleanup();
  });

  it('keeps the minimal single-result view focused on row actions instead of a standalone list refresh', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Expediente')).toBeTruthy();
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
    });

    await cleanup();
  });

  it('folds tiny default-list counts into the current view instead of adding a utility row', async () => {
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
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(
        container.querySelector('[data-testid="course-registration-single-choice-context"]')?.textContent?.trim(),
      ).toBe('Mostrando 2 inscripciones en esta vista.');
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(document.body.querySelector('[aria-label="Refrescar expediente"]')).toBeNull();
      expect(
        container.querySelector('[data-testid="course-registration-single-choice-context"]')?.textContent?.trim(),
      ).toBe('Mostrando 2 inscripciones en esta vista.');
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
    });

    await cleanup();
  });
});
