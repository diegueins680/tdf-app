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
  default: ({
    label,
    onComplete,
  }: {
    label?: unknown;
    onComplete?: (files: { name: string; publicUrl: string }[]) => void;
  }) => {
    const labelText = typeof label === 'string' ? label : '';
    const isReceiptUpload = labelText.includes('comprobante') || labelText.includes('Archivo listo');
    const fileName = isReceiptUpload ? 'mock-receipt.pdf' : 'mock-follow-up.pdf';

    return (
      <button
        type="button"
        aria-label={isReceiptUpload ? 'Completar comprobante de prueba' : 'Completar adjunto de prueba'}
        data-testid={isReceiptUpload ? 'mock-receipt-upload' : 'mock-follow-up-upload'}
        onClick={() => onComplete?.([{ name: fileName, publicUrl: `https://example.com/${fileName}` }])}
      />
    );
  },
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
  'El primer comprobante documenta el pago y habilita Marcar pagado. Cuando lo guardes aparecerá aquí con enlace y acciones para revisarlo después.';
const emptyReceiptEvidenceAlertMessage =
  'Agrega evidencia solo si necesitas documentar este pago. Se guardará aquí con un enlace para revisarla después.';
const receiptComposerHelpText =
  'Este formulario ya está abierto para guardar otro comprobante o pegar un enlace existente.';
const editingReceiptComposerHelpText =
  'Edita el comprobante y guarda los cambios para actualizar el registro.';
const showSystemEmailsLabel = 'Ver correos del sistema';
const hideSystemEmailsLabel = 'Ocultar correos del sistema';
const retrySystemEmailsLabel = 'Reintentar correos';
const systemEmailHistoryHelperText =
  'Historial persistente de correos del sistema para esta inscripción. Usa el refresco del expediente para volver a consultarlo.';
const emptySystemEmailHistoryMessage =
  'Todavía no hay correos del sistema registrados para esta inscripción. Cuando se envíe el primero, aparecerá aquí.';
const optionalDossierContextActionsLabel = 'Agregar nota o seguimiento';
const compactOptionalDossierContextActionsLabel = 'Agregar contexto';
const optionalDossierNotesActionLabel = 'Agregar nota interna';
const optionalDossierFollowUpActionLabel = 'Agregar seguimiento manual';
const markPaidEmptyNotesHelperText =
  'Agrega una nota solo si necesitas dejar contexto extra sobre este pago.';
const markPaidEmptyFollowUpHelperText =
  'Agrega seguimiento solo si necesitas dejar contexto manual aparte del comprobante o del cambio de estado.';
const markPaidOptionalFollowUpActionLabel = 'Agregar seguimiento';
const markPaidOptionalFollowUpAccessibleLabel = 'Agregar seguimiento opcional';
const emptyFollowUpAlertMessage =
  'Aún no hay seguimiento manual. Documenta llamadas, mensajes o próximos pasos desde aquí. Los cambios de estado y los comprobantes nuevos también quedarán registrados aquí.';
const firstFollowUpComposerHelpText =
  'Este formulario ya está abierto para registrar el primer seguimiento. Escribe la nota y aparecerá Guardar seguimiento.';
const openPaymentWorkflowLabel = 'Registrar pago';
const markPaymentPendingLabel = 'Marcar pago pendiente';
const compactPaymentPendingActionLabel = 'Pasar a pendiente';
const normalizePaymentPendingLabel = 'Normalizar a pendiente';
const normalizeCancelledLabel = 'Normalizar a cancelado';
const reopenPendingLabel = 'Reabrir como pendiente';
const copyVisibleCsvLabel = (count: number) => `Copiar CSV (${count} inscripci${count === 1 ? 'ón' : 'ones'})`;
const copyVisibleSearchCsvLabel = 'Copiar CSV';
const staleCopyVisibleSearchCsvLabel = 'Copiar visibles como CSV';
const localSearchLabel = 'Buscar inscripciones';
const loadLimitLabel = 'Límite de carga';
const loadLimitHelperText = 'Máximo de inscripciones cargadas en esta vista.';
const emptySearchLimitRecoveryLabel = 'Aumentar límite';
const activeStatusFilterHelperText = 'Selecciona el estado activo otra vez para volver a ver todos.';
const clearPaidStatusFilterLabel = 'Quitar filtro de estado Pagado';
const clearPendingStatusFilterLabel = 'Quitar filtro de estado Pendiente de pago';
const customStatusFilterUnavailableMessage =
  'Normaliza cada fila desde Estado para recuperar los filtros estándar.';
const customStatusFilterUnavailableTitle = 'Estados no estándar';
const dossierScopeHint =
  'Usa el nombre para abrir expediente; el menú de estado muestra acciones.';
const dossierLinkScopeHint =
  'Usa el nombre para abrir expediente.';
const paymentWorkflowDossierScopeHint =
  'Usa el nombre para abrir expediente; el menú de estado incluye Registrar pago.';
const dossierOnlyScopeHint =
  'Usa el nombre para abrir expediente; el menú de estado abre acciones rápidas.';
const customStatusNormalizationScopeHint =
  'Usa el nombre para abrir expediente; el menú de estado ofrece normalizar a pendiente o cancelado.';
const pendingRecoveryScopeHint =
  'Usa el nombre para abrir expediente; Reabrir vuelve a pendiente.';
const paidRecoveryScopeHint =
  'Usa el nombre para abrir expediente; Marcar pago pendiente devuelve la inscripción a pendiente.';
const emailDossierScopeHint =
  'Usa el correo para abrir expediente; el menú de estado muestra acciones.';
const emailPaymentWorkflowDossierScopeHint =
  'Usa el correo para abrir expediente; el menú de estado incluye Registrar pago.';
const phonePaymentWorkflowDossierScopeHint =
  'Usa el teléfono para abrir expediente; el menú de estado incluye Registrar pago.';
const recordDossierScopeHint =
  'Usa el número de registro para abrir expediente; el menú de estado muestra acciones.';
const recordPaymentWorkflowDossierScopeHint =
  'Usa el número de registro para abrir expediente; el menú de estado incluye Registrar pago.';
const recordDossierLinkScopeHint =
  'Usa el número de registro para abrir expediente.';
const mixedIdentityPaymentWorkflowDossierScopeHint =
  'Usa el nombre, el correo o el número de registro para abrir expediente; el menú de estado incluye Registrar pago.';
const mixedIdentityDossierLinkScopeHint =
  'Usa el nombre, el correo o el número de registro para abrir expediente.';
const dossierErrorRetryLabel = 'Reintentar expediente';
const paymentStatusMenuButtonLabel = 'Pago y estado';
const paymentStatusMenuButtonAriaLabel = (targetLabel: string) =>
  `Abrir opciones de pago y estado para ${targetLabel}`;
const initialEmptyStateConfigMessage =
  'Todavía no hay inscripciones. El primer formulario público enviará aquí las nuevas solicitudes.';
const initialEmptyStateMultiCohortMessage =
  'Hay 2 formularios públicos listos para recibir la primera inscripción: Beatmaking 101 y Mixing Bootcamp.';
const singleCohortInitialEmptyStateMessage =
  'Todavía no hay inscripciones para Beatmaking 101. La página pública ya está lista para recibir la primera.';
const initialEmptyStateConfigActionLabel = 'Configurar primer formulario';
const initialEmptyStateMultiCohortActionLabel = 'Elegir formulario público';
const initialEmptyStateSingleCourseVariantActionLabel = 'Elegir variante';
const initialEmptyStateFormActionLabel = 'Abrir formulario público';
const initialEmptyStateNewTabDescription = 'Se abre en una pestaña nueva.';
const initialEmptyStateNewTabDescriptionId = 'course-registration-initial-empty-state-new-tab-description';
const initialEmptyStateConfigActionAriaLabel = 'Configurar el primer formulario público de curso';
const initialEmptyStateMultiCohortActionAriaLabel = 'Ver formularios públicos para elegir cuál compartir primero';
const initialEmptyStateSingleCourseVariantActionAriaLabel =
  'Ver variantes públicas para elegir cuál compartir primero';
const initialRegistrationLoadingMessage = 'Cargando inscripciones…';
const initialCohortResolutionMessage =
  'Revisando formularios de curso para mostrar el siguiente paso.';
const initialCohortErrorMessage =
  'No se pudieron cargar los formularios de curso. Reintenta para elegir qué formulario compartir.';
const initialCohortRetryLabel = 'Reintentar formularios';
const cohortFilterUnavailableMessage =
  'No se pudieron cargar cohortes. La lista sigue disponible; el filtro por curso volverá cuando se recupere esa información.';
const cohortFilterLoadingMessage =
  'La lista ya está disponible; el filtro por curso aparecerá cuando terminen de cargar los formularios.';
const emptyCohortFilterMessage =
  'Sin filtro por cohorte hasta configurar cursos. La lista sigue disponible.';

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
    queryClient: qc,
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

const mouseDownElement = (element: Element) => {
  element.dispatchEvent(new MouseEvent('mousedown', { bubbles: true }));
};

const clickButton = (button: HTMLButtonElement) => {
  clickElement(button);
};

const getComboboxByLabel = (root: ParentNode, labelText: string) => {
  const labels = Array.from(root.querySelectorAll('label'));
  const label = labels.find((el) => {
    const text = (el.textContent ?? '').replace('*', '').trim();
    return text === labelText;
  });
  if (!label) throw new Error(`Label not found: ${labelText}`);

  const labelId = label.id;
  const combobox = Array.from(root.querySelectorAll<HTMLElement>('[role="combobox"]')).find((candidate) => {
    const labelledBy = candidate.getAttribute('aria-labelledby') ?? '';
    return labelId && labelledBy.split(/\s+/).includes(labelId);
  });
  if (!combobox) throw new Error(`Combobox not found for label: ${labelText}`);

  return combobox;
};

const openDossierContextAction = async (labelText: string) => {
  const groupedMenuLabel = labelText === 'Agregar nota'
    ? optionalDossierNotesActionLabel
    : labelText === 'Agregar seguimiento'
      ? optionalDossierFollowUpActionLabel
      : labelText;
  const directButton = Array.from(document.body.querySelectorAll<HTMLButtonElement>('button')).find(
    (button) => (button.textContent ?? '').trim() === labelText,
  );

  if (directButton) {
    await act(async () => {
      clickButton(directButton);
      await flushPromises();
      await flushPromises();
    });
    return;
  }

  const groupedContextButton = Array.from(document.body.querySelectorAll<HTMLButtonElement>('button')).find(
    (button) => {
      const text = (button.textContent ?? '').trim();
      return text === optionalDossierContextActionsLabel || text === compactOptionalDossierContextActionsLabel;
    },
  );

  if (!groupedContextButton) {
    throw new Error(`Grouped dossier context action not found for: ${labelText}`);
  }

  await act(async () => {
    clickButton(groupedContextButton);
    await flushPromises();
    await flushPromises();
  });

  await act(async () => {
    clickElement(getMenuItemByText(document.body, groupedMenuLabel));
    await flushPromises();
    await flushPromises();
  });
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
        paymentWorkflowDossierScopeHint,
      );
      expect(countOccurrences(container, paymentWorkflowDossierScopeHint)).toBe(1);
      expect(container.textContent).not.toContain(dossierOnlyScopeHint);
      expect(container.textContent).not.toContain('Abrir expediente');
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace').textContent?.trim()).toBe('Ada Lovelace');
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace').getAttribute('title')).toBe(
        'Abrir expediente de Ada Lovelace',
      );
      expect(container.textContent).not.toContain('Cambiar estado:');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
      expect(countOccurrences(container, 'Pendiente de pago')).toBe(1);
      expect(container.textContent).not.toContain(showSystemEmailsLabel);
      expect(hasLabel(container, loadLimitLabel)).toBe(false);
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
        'Agrega el primer comprobante para documentar el pago',
      );
      expect(document.body.textContent).not.toContain(
        'Todavía no hay comprobantes. Agrega el primero para documentar el pago y habilitar Marcar pagado.',
      );
      expect(document.body.textContent).not.toContain(
        'Sube un comprobante o pega una URL existente para habilitar Marcar pagado.',
      );
      expect(document.body.textContent).not.toContain('0 guardados');
      const firstReceiptAction = getButtonByText(document.body, 'Agregar comprobante');
      expect(firstReceiptAction).toBeTruthy();
      expect(firstReceiptAction.getAttribute('aria-label')).toBe('Agregar primer comprobante');
      expect(firstReceiptAction.getAttribute('title')).toBe('Agregar primer comprobante');
      expect(countButtonsByText(document.body, 'Agregar primer comprobante')).toBe(0);
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
      expect(countButtonsByText(document.body, 'Notas y seguimiento')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(0);
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
      clickButton(getButtonByText(document.body, 'Agregar comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countButtonsByText(document.body, 'Guardar comprobante')).toBe(0);
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir archivo')).toBeTruthy();
      expect(countButtonsByText(document.body, optionalDossierContextActionsLabel)).toBe(0);
      expect(countButtonsByText(document.body, compactOptionalDossierContextActionsLabel)).toBe(0);
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Usar enlace existente en lugar de subir adjunto',
        ),
      ).toBe(false);
      expect(getButtonByText(document.body, 'Cancelar comprobante')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Cancelar seguimiento')).toBe(0);
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Cancelar',
        ),
      ).toBe(false);
      expect(hasExactText(document.body, 'Registrar seguimiento')).toBe(false);
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(document.body.textContent).not.toContain(firstFollowUpComposerHelpText);
      expect(document.body.textContent).toContain(
        'Primero elige el archivo o pega un enlace; luego podrás ajustar el nombre visible y las notas.',
      );
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(0);
      expect(hasLabel(document.body, 'Nombre visible')).toBe(false);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(false);
      expect(hasLabel(document.body, 'Asunto')).toBe(false);
      expect(hasLabel(document.body, 'Próximo seguimiento')).toBe(false);
      expect(document.body.textContent).not.toContain('Agrega tipo, asunto, recordatorio o evidencia solo si hacen falta.');
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Usar enlace existente en lugar de subir archivo'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(true);
      expect(hasLabel(document.body, 'Nombre visible')).toBe(false);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(false);
      expect(hasLabel(document.body, 'URL del adjunto')).toBe(false);
    });

    await act(async () => {
      setInputValue(getInputByLabel(document.body, 'URL del comprobante'), 'https://example.com/new-receipt.pdf');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'Nombre visible')).toBe(true);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(true);
      expect(getButtonByText(document.body, 'Guardar comprobante').disabled).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Cancelar comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(emptyReceiptAlertMessage);
      expect(getButtonByText(document.body, 'Agregar comprobante')).toBeTruthy();
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
      expect(countButtonsByText(document.body, 'Guardar comprobante')).toBe(0);
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(false);
      expect(hasLabel(document.body, 'Nombre visible')).toBe(false);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(false);
    });

    await openDossierContextAction('Agregar seguimiento');

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Agregar detalles opcionales')).toBeTruthy();
      expect(getButtonByText(document.body, 'Cancelar seguimiento')).toBeTruthy();
      expect(hasExactText(document.body, 'Registrar seguimiento')).toBe(false);
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(document.body.textContent).toContain(firstFollowUpComposerHelpText);
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(0);
      expect(hasLabel(document.body, 'Asunto')).toBe(false);
      expect(hasLabel(document.body, 'Próximo seguimiento')).toBe(false);
      expect(document.body.textContent).toContain('Agrega tipo, asunto, recordatorio o evidencia solo si hacen falta.');
    });

    await act(async () => {
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
      expect(hasLabel(document.body, 'Nombre visible')).toBe(false);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(false);
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(false);
      expect(hasLabel(document.body, 'URL del adjunto')).toBe(true);
    });

    await cleanup();
  });

  it('keeps a custom single-result source visible without reopening filter chrome', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crSource: 'instagram_story',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Fuente visible: Instagram story.');
      expect(container.textContent).toContain('Fuente: Instagram story');
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace').textContent?.trim()).toBe('Ada Lovelace');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
    });

    await cleanup();
  });

  it('keeps a single missing-contact registration clear without adding row placeholder copy', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crEmail: null,
        crPhoneE164: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        `${paymentWorkflowDossierScopeHint} Contacto pendiente en esta inscripción.`,
      );
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace').textContent?.trim()).toBe('Ada Lovelace');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
      expect(countOccurrences(container, 'Contacto pendiente en esta inscripción.')).toBe(1);
      expect(container.textContent).not.toContain('Sin correo ni teléfono');
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
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

  it('puts the dossier load retry inside the error instead of hiding it behind the title icon', async () => {
    getRegistrationDossierMock
      .mockRejectedValueOnce(new Error('Dossier unavailable'))
      .mockResolvedValueOnce(buildDossier());

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getOnlyDossierTrigger(container)).toBeTruthy();
    });

    await act(async () => {
      clickButton(getOnlyDossierTrigger(container));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('No se pudo cargar el expediente: Dossier unavailable');
      expect(getButtonByText(document.body, dossierErrorRetryLabel)).toBeTruthy();
      expect(countButtonsByText(document.body, dossierErrorRetryLabel)).toBe(1);
      expect(document.body.querySelector('[aria-label="Refrescar expediente"]')).toBeNull();
      expect(document.body.querySelector('[aria-label="Refrescar expediente y correos"]')).toBeNull();
      expect(document.body.textContent).not.toContain(emptyReceiptAlertMessage);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, dossierErrorRetryLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getRegistrationDossierMock).toHaveBeenCalledTimes(2);
      expect(document.body.textContent).not.toContain('No se pudo cargar el expediente: Dossier unavailable');
      expect(countButtonsByText(document.body, dossierErrorRetryLabel)).toBe(0);
      expect(document.body.textContent).toContain(emptyReceiptAlertMessage);
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
      const contextSummary = container.querySelector<HTMLElement>('[data-testid="course-registration-single-choice-context"]');

      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(contextSummary?.textContent).toContain(dossierLinkScopeHint);
      expect(contextSummary?.textContent).not.toContain(paymentWorkflowDossierScopeHint);
      expect(countOccurrences(
        container,
        dossierLinkScopeHint,
      )).toBe(1);
      expect(countButtonsByText(container, 'Expediente')).toBe(0);
      expect(countButtonsByText(container, paymentStatusMenuButtonLabel)).toBe(2);
      expect(countButtonsByText(container, openPaymentWorkflowLabel)).toBe(0);
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(0);
      expect(countButtonsByText(container, 'Cambiar')).toBe(0);
      expect(countButtonsByText(container, 'Estado')).toBe(0);
      expect(countOccurrences(container, 'Pendiente de pago')).toBe(1);
      expect(container.querySelectorAll('button[aria-label^="Abrir expediente de "]')).toHaveLength(2);
      expect(container.querySelectorAll('button[aria-label^="Abrir opciones de pago y estado para "]')).toHaveLength(2);
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(0);
      expect(container.textContent).not.toContain('Abrir expediente');
      expect(container.textContent).not.toContain('Estado: Pendiente de pago');
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace').textContent?.trim()).toBe('Ada Lovelace');
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Grace Hopper').textContent?.trim()).toBe('Grace Hopper');
      expect(getButtonByAriaLabel(container, paymentStatusMenuButtonAriaLabel('Ada Lovelace')).textContent?.trim()).toBe(paymentStatusMenuButtonLabel);
      expect(getButtonByAriaLabel(container, paymentStatusMenuButtonAriaLabel('Grace Hopper')).textContent?.trim()).toBe(paymentStatusMenuButtonLabel);
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
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(countOccurrences(container, dossierLinkScopeHint)).toBe(1);
      expect(container.textContent).not.toContain(paymentWorkflowDossierScopeHint);
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Expediente de inscripción');
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(container.textContent).not.toContain(dossierLinkScopeHint);
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
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(countOccurrences(container, dossierLinkScopeHint)).toBe(1);
      expect(container.textContent).not.toContain(paymentWorkflowDossierScopeHint);
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, paymentStatusMenuButtonAriaLabel('Ada Lovelace')));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(openPaymentWorkflowLabel);
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(container.textContent).not.toContain(dossierLinkScopeHint);
    });

    await cleanup();
  });

  it('drops the first-time dossier hint after the direct cancelled-row recovery action', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crStatus: 'cancelled',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        pendingRecoveryScopeHint,
      );
      expect(container.textContent).not.toContain(dossierOnlyScopeHint);
      expect(getButtonByAriaLabel(container, 'Reabrir como pendiente para Ada Lovelace').textContent?.trim()).toBe(
        reopenPendingLabel,
      );
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Reabrir como pendiente para Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Estado actualizado para Ada Lovelace.');
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(container.textContent).not.toContain(dossierOnlyScopeHint);
    });

    await cleanup();
  });

  it('uses compact reopen actions when a shared cancelled summary already owns the current status', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crStatus: 'cancelled',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'cancelled',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const currentView = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-current-view-summary"]',
      );
      const adaAction = getButtonByAriaLabel(container, 'Reabrir como pendiente para Ada Lovelace');
      const graceAction = getButtonByAriaLabel(container, 'Reabrir como pendiente para Grace Hopper');

      expect(currentView?.textContent).toContain('Beatmaking 101 · Cancelado');
      expect(currentView?.textContent).toContain(pendingRecoveryScopeHint);
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(adaAction.textContent?.trim()).toBe('Reabrir');
      expect(graceAction.textContent?.trim()).toBe('Reabrir');
      expect(adaAction.getAttribute('title')).toBe('Reabrir como pendiente; actual: Cancelado');
      expect(adaAction.getAttribute('aria-haspopup')).toBeNull();
      expect(countButtonsByText(container, 'Reabrir')).toBe(2);
      expect(countButtonsByText(container, reopenPendingLabel)).toBe(0);
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
      const currentViewSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-current-view-summary"]',
      );
      const contextSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-single-choice-context"]',
      );

      expect(currentViewSummary?.textContent).toContain('Vista actual');
      expect(currentViewSummary?.textContent).toContain('Beatmaking 101 · Pendiente de pago');
      expect(contextSummary?.textContent).toContain(dossierLinkScopeHint);
      expect(contextSummary?.textContent).not.toContain(paymentWorkflowDossierScopeHint);
      expect(currentViewSummary?.textContent).not.toContain(
        'Vista única por ahora: una cohorte y un estado.',
      );
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, paymentStatusMenuButtonAriaLabel('Ada Lovelace')));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const currentViewSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-current-view-summary"]',
      );
      const contextSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-single-choice-context"]',
      );

      expect(document.body.textContent).toContain(openPaymentWorkflowLabel);
      expect(currentViewSummary?.textContent).toContain('Vista actual');
      expect(currentViewSummary?.textContent).toContain('Beatmaking 101 · Pendiente de pago');
      expect(contextSummary?.textContent).not.toContain(dossierLinkScopeHint);
      expect(currentViewSummary?.textContent).not.toContain(
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
      expect(countButtonsByText(container, copyVisibleCsvLabel(2))).toBe(0);
      expect(container.querySelector('button[aria-label="Copiar 2 inscripciones visibles como CSV"]')).toBeNull();
    });

    await cleanup();
  });

  it('reveals the limit toggle only when the current batch reaches its cap or a custom limit is active', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, loadLimitLabel)).toBe(false);
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
      const limitToggle = getButtonByText(secondContainer, 'Ajustar límite');

      expect(hasLabel(secondContainer, 'Límite')).toBe(false);
      expect(limitToggle).toBeTruthy();
      expect(limitToggle.getAttribute('aria-label')).toBe('Ajustar límite de carga');
      expect(limitToggle.getAttribute('title')).toBe(
        'Mostrar el campo de límite de carga para revisar un lote distinto.',
      );
      expect(
        secondContainer
          .querySelector('[data-testid="course-registration-current-view-summary"] button')
          ?.textContent
          ?.trim(),
      ).toBe('Ajustar límite');
      expect(secondContainer.textContent).toContain(
        'Busca dentro de las 200 inscripciones cargadas.',
      );
      expect(secondContainer.textContent).not.toContain(
        'Busca dentro de las 200 inscripciones cargadas. Usa Ajustar límite si necesitas revisar más registros.',
      );
      expect(secondContainer.textContent).not.toContain(
        'Vista única por ahora: una cohorte y un estado. Usa Ajustar límite solo cuando necesites revisar un lote distinto.',
      );
      expect(secondContainer.textContent).not.toContain(
        'Esta vista ya está acotada a una cohorte y un estado. Usa Ajustar límite solo cuando necesites revisar un lote distinto.',
      );
      expect(countOccurrences(
        secondContainer,
        'Vista única por ahora: una cohorte y un estado. Usa Ajustar límite solo cuando necesites revisar un lote distinto.',
      )).toBe(0);
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
      const hideLimitToggle = getButtonByText(secondContainer, 'Ocultar límite');

      expect(hasLabel(secondContainer, loadLimitLabel)).toBe(true);
      expect(hasLabel(secondContainer, 'Límite')).toBe(false);
      expect(hideLimitToggle).toBeTruthy();
      expect(hideLimitToggle.getAttribute('aria-label')).toBe('Ocultar límite de carga');
      expect(hideLimitToggle.getAttribute('title')).toBe('Ocultar el campo de límite de carga.');
      expect(secondContainer.textContent).toContain(loadLimitHelperText);
      expect(secondContainer.textContent).not.toContain('Máximo de filas a cargar');
    });

    await secondRender.cleanup();

    listRegistrationsMock.mockClear();

    const thirdContainer = document.createElement('div');
    document.body.appendChild(thirdContainer);
    const thirdRender = await renderPage(thirdContainer, '/inscripciones-curso?limit=50');

    await waitForExpectation(() => {
      const customLimitToggle = getButtonByText(thirdContainer, 'Ajustar límite (50)');

      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: undefined,
        limit: 50,
      });
      expect(hasLabel(thirdContainer, 'Límite')).toBe(false);
      expect(customLimitToggle).toBeTruthy();
      expect(customLimitToggle.getAttribute('aria-label')).toBe('Ajustar límite de carga (50)');
      expect(customLimitToggle.getAttribute('title')).toBe(
        'Mostrar el campo de límite de carga para revisar un lote distinto.',
      );
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

  it('keeps a redundant single-cohort URL filter out of the minimal single-result view', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?slug=beatmaking-101');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: 'beatmaking-101',
        status: undefined,
        limit: 200,
      });
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.textContent).not.toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).not.toContain('Slug: beatmaking-101');
      expect(container.textContent).not.toContain('Fuente visible: landing.');
      expect(container.textContent).not.toContain('Fuente: landing');
      expect(container.textContent).not.toContain(`Creado: ${formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-')}`);
      expect(container.textContent).not.toContain('Vista filtrada:');
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        paymentWorkflowDossierScopeHint,
      );
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace')).toBeTruthy();
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
        'Vista filtrada: cohorte Beatmaking 101.',
      );
      expect(summary?.textContent).not.toContain('(beatmaking-101)');
      expect(container.querySelectorAll('[data-testid="course-registration-filter-summary"]')).toHaveLength(1);
      const createdAtLabel = formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-');
      expect(container.querySelector('[data-testid="course-registration-shared-created-at-summary"]')).toBeNull();
      expect(container.textContent).not.toContain(`Misma fecha de registro: ${createdAtLabel}.`);
      expect(countOccurrences(container, `Creado: ${createdAtLabel}`)).toBe(0);
      const activeStatusSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-active-status-summary"]',
      );
      expect(activeStatusSummary?.textContent).toContain('Estado filtrado');
      expect(activeStatusSummary?.textContent).toContain('Pagado');
      expect(activeStatusSummary?.textContent).toContain(
        'La vista filtrada ya muestra solo este estado.',
      );
      expect(activeStatusSummary?.textContent).not.toContain('Restablecer vista');
      expect(container.querySelector('[role="group"][aria-label="Filtro de estado activo: Pagado"]')).toBeNull();
      expect(container.querySelector(`[aria-label="${clearPaidStatusFilterLabel}"]`)).toBeNull();
      expect(getButtonByText(container, 'Restablecer vista')).toBeTruthy();
      expect(countButtonsByText(container, 'Restablecer vista')).toBe(1);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
    });

    await cleanup();
  });

  it('keeps a tiny cohort-only filtered view focused on the cohort select instead of export chrome', async () => {
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
      expect(container.textContent).toContain('Beatmaking 101');
      expect(container.textContent).not.toContain('Mostrando 2 inscripciones.');
      expect(countButtonsByText(container, copyVisibleCsvLabel(2))).toBe(0);
      expect(container.querySelector('button[aria-label="Copiar 2 inscripciones visibles como CSV"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Vista filtrada: cohorte Beatmaking 101 (beatmaking-101).');
      expect(countButtonsByText(container, 'Mostrar todas las cohortes')).toBe(0);
      expect(container.textContent).not.toContain('Cohorte: Beatmaking 101');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('keeps humanized cohort filter labels from repeating the raw slug', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'beatmaking-101' },
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
      expect(hasLabel(container, 'Curso / cohorte')).toBe(true);
      expect(container.textContent).toContain('Beatmaking 101');
      expect(container.textContent).not.toContain('beatmaking-101');
      expect(container.textContent).not.toContain('Beatmaking 101 (beatmaking-101)');
    });

    await cleanup();
  });

  it('strips public-form wrappers from selected cohort filter labels', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Formulario público - Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Página pública para inscripciones - Mixing Bootcamp' },
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
      expect(hasLabel(container, 'Curso / cohorte')).toBe(true);
      expect(container.textContent).toContain('Beatmaking 101');
      expect(container.textContent).not.toContain('Formulario público - Beatmaking 101');
      expect(container.textContent).not.toContain('Página pública para inscripciones');
      expect(container.textContent).not.toContain('Beatmaking 101 (beatmaking-101)');
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

  it('does not repeat contact values that are already the visible registration identity', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crFullName: 'ops@example.com',
        crEmail: 'ops@example.com',
        crPhoneE164: '+593999000111',
      }),
      buildRegistration({
        crId: 102,
        crFullName: '999000222',
        crEmail: null,
        crPhoneE164: '+593 999 000 222',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countOccurrences(container, 'ops@example.com')).toBe(1);
      expect(hasExactText(container, '+593999000111')).toBe(true);
      expect(hasExactText(container, '999000222')).toBe(true);
      expect(container.textContent).not.toContain('+593 999 000 222');
      expect(getButtonByAriaLabel(container, 'Abrir expediente de ops@example.com')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de 999000222')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Abrir expediente de ops@example.com'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const dialog = getDialog();
      expect(countOccurrences(dialog, 'ops@example.com')).toBe(1);
      expect(hasExactText(dialog, '+593999000111')).toBe(true);
      expect(dialog.textContent).not.toContain('ops@example.com · +593999000111');
    });

    await cleanup();
  });

  it('names the visible email identity in first-time dossier guidance when the registration name is blank', async () => {
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
        emailPaymentWorkflowDossierScopeHint,
      );
      expect(container.textContent).not.toContain(dossierScopeHint);
      expect(container.textContent).not.toContain(emailDossierScopeHint);
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

  it('names the visible phone identity in first-time dossier guidance when it is the only contact', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crFullName: '   ',
        crEmail: '   ',
        crPhoneE164: '+593999000777',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        phonePaymentWorkflowDossierScopeHint,
      );
      expect(container.textContent).not.toContain('Usa el contacto para abrir expediente');
      expect(hasExactText(container, '+593999000777')).toBe(true);
      expect(container.textContent).not.toContain('Sin nombre');
      expect(getButtonByAriaLabel(container, 'Abrir expediente de +593999000777')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Cambiar estado para +593999000777')).toBeTruthy();
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
      const contextSummary = container.querySelector<HTMLElement>('[data-testid="course-registration-single-choice-context"]');

      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(contextSummary?.textContent).toContain(recordDossierLinkScopeHint);
      expect(contextSummary?.textContent).not.toContain(recordPaymentWorkflowDossierScopeHint);
      expect(container.textContent).not.toContain(dossierScopeHint);
      expect(hasExactText(container, 'Registro #101')).toBe(true);
      expect(hasExactText(container, 'Registro #102')).toBe(true);
      expect(countOccurrences(container, 'Sin correo ni teléfono')).toBe(0);
      expect(countOccurrences(container, 'Sin nombre')).toBe(0);
      expect(container.querySelectorAll('button[aria-label^="Abrir expediente de registro #"]')).toHaveLength(2);
      expect(container.querySelectorAll('button[aria-label^="Abrir opciones de pago y estado para registro #"]')).toHaveLength(2);
      expect(countButtonsByText(container, paymentStatusMenuButtonLabel)).toBe(2);
      expect(countButtonsByText(container, openPaymentWorkflowLabel)).toBe(0);
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

  it('summarizes record-only missing contact once without adding row placeholders', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crFullName: 'Ada Lovelace',
        crEmail: 'ada@example.com',
        crPhoneE164: '+593999000111',
      }),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: null,
        crEmail: null,
        crPhoneE164: null,
      }),
      buildRegistration({
        crId: 103,
        crPartyId: 11,
        crFullName: '   ',
        crEmail: '   ',
        crPhoneE164: '   ',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de registro #102')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de registro #103')).toBeTruthy();
      expect(hasExactText(container, '2 inscripciones visibles con contacto pendiente.')).toBe(true);
      expect(countOccurrences(container, '2 inscripciones visibles con contacto pendiente.')).toBe(1);
      expect(countOccurrences(container, 'Sin correo ni teléfono')).toBe(0);
      expect(countOccurrences(container, 'Sin nombre')).toBe(0);
    });

    await cleanup();
  });

  it('uses one precise first-time hint for mixed identity rows instead of row-level guidance', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: '   ',
        crEmail: 'contacto@example.com',
        crPhoneE164: '+593999000222',
      }),
      buildRegistration({
        crId: 103,
        crPartyId: 11,
        crFullName: null,
        crEmail: null,
        crPhoneE164: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const contextSummary = container.querySelector<HTMLElement>('[data-testid="course-registration-single-choice-context"]');

      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(contextSummary?.textContent).toContain(mixedIdentityDossierLinkScopeHint);
      expect(contextSummary?.textContent).not.toContain(mixedIdentityPaymentWorkflowDossierScopeHint);
      expect(countOccurrences(container, mixedIdentityDossierLinkScopeHint)).toBe(1);
      expect(container.textContent).not.toContain(dossierScopeHint);
      expect(container.textContent).not.toContain(emailDossierScopeHint);
      expect(container.textContent).not.toContain(recordDossierScopeHint);
      expect(container.textContent).not.toContain('dato principal de cada fila');
      expect(countOccurrences(container, 'para abrir expediente')).toBe(1);
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de contacto@example.com')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de registro #103')).toBeTruthy();
      expect(container.querySelectorAll('button[aria-label^="Abrir expediente de "]')).toHaveLength(3);
      expect(container.querySelectorAll('button[aria-label^="Abrir opciones de pago y estado para "]')).toHaveLength(3);
      expect(countButtonsByText(container, paymentStatusMenuButtonLabel)).toBe(3);
      expect(countButtonsByText(container, openPaymentWorkflowLabel)).toBe(0);
    });

    await cleanup();
  });

  it('summarizes missing contact once when every named visible registration needs it', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crFullName: 'Ada Lovelace',
        crEmail: null,
        crPhoneE164: null,
      }),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: 'Grace Hopper',
        crEmail: '   ',
        crPhoneE164: '   ',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Grace Hopper')).toBeTruthy();
      expect(hasExactText(container, 'Contacto pendiente en todas las inscripciones visibles.')).toBe(true);
      expect(countOccurrences(container, 'Sin correo ni teléfono')).toBe(0);
      expect(countOccurrences(container, 'Contacto pendiente en todas las inscripciones visibles.')).toBe(1);
      expect(container.querySelectorAll('button[aria-label^="Abrir expediente de "]')).toHaveLength(2);
      expect(container.querySelectorAll('button[aria-label^="Abrir opciones de pago y estado para "]')).toHaveLength(2);
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const dialog = getDialog();
      expect(dialog.textContent).toContain('Ada Lovelace');
      expect(hasExactText(dialog, 'Sin correo ni teléfono')).toBe(true);
    });

    await cleanup();
  });

  it('summarizes partial missing contact once without adding row placeholders', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crFullName: 'Ada Lovelace',
        crEmail: null,
        crPhoneE164: null,
      }),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crPhoneE164: '+593999000222',
      }),
      buildRegistration({
        crId: 103,
        crPartyId: 11,
        crFullName: 'Katherine Johnson',
        crEmail: 'katherine@example.com',
        crPhoneE164: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Grace Hopper')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Katherine Johnson')).toBeTruthy();
      expect(hasExactText(container, '1 inscripción visible con contacto pendiente.')).toBe(true);
      expect(countOccurrences(container, '1 inscripción visible con contacto pendiente.')).toBe(1);
      expect(container.textContent).not.toContain('Contacto pendiente en todas las inscripciones visibles.');
      expect(countOccurrences(container, 'Sin correo ni teléfono')).toBe(0);
      expect(container.querySelectorAll('button[aria-label^="Abrir expediente de "]')).toHaveLength(3);
      expect(container.querySelectorAll('button[aria-label^="Abrir opciones de pago y estado para "]')).toHaveLength(3);
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
        getButtonByAriaLabel(container, paymentStatusMenuButtonAriaLabel('Ana Torres (ana.primary@example.com)')),
      ).toBeTruthy();
      expect(
        getButtonByAriaLabel(container, 'Abrir expediente de Ana Torres (ana.alt@example.com · +593999000222)'),
      ).toBeTruthy();
      expect(
        getButtonByAriaLabel(container, paymentStatusMenuButtonAriaLabel('Ana Torres (ana.alt@example.com · +593999000222)')),
      ).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Grace Hopper')).toBeTruthy();
      expect(getButtonByAriaLabel(container, paymentStatusMenuButtonAriaLabel('Grace Hopper'))).toBeTruthy();
      expect(countOccurrences(container, 'Registro #101')).toBe(0);
      expect(countOccurrences(container, 'Registro #102')).toBe(0);
      expect(countOccurrences(container, 'Registro #103')).toBe(0);
      expect(countButtonsByText(container, 'Expediente')).toBe(0);
    });

    await cleanup();
  });

  it('keeps duplicate row actions distinct when name and contact are both repeated', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Ana Torres',
        crEmail: 'ana.shared@example.com',
        crPhoneE164: '+593999000222',
      }),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: 'Ana Torres',
        crEmail: 'ana.shared@example.com',
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
      expect(
        container.querySelector(
          'button[aria-label="Abrir expediente de Ana Torres (ana.shared@example.com · +593999000222)"]',
        ),
      ).toBeNull();
      expect(
        getButtonByAriaLabel(
          container,
          'Abrir expediente de Ana Torres (ana.shared@example.com · +593999000222 · registro #101)',
        ),
      ).toBeTruthy();
      expect(
        getButtonByAriaLabel(
          container,
          paymentStatusMenuButtonAriaLabel('Ana Torres (ana.shared@example.com · +593999000222 · registro #101)'),
        ),
      ).toBeTruthy();
      expect(
        getButtonByAriaLabel(
          container,
          'Abrir expediente de Ana Torres (ana.shared@example.com · +593999000222 · registro #102)',
        ),
      ).toBeTruthy();
      expect(
        getButtonByAriaLabel(
          container,
          paymentStatusMenuButtonAriaLabel('Ana Torres (ana.shared@example.com · +593999000222 · registro #102)'),
        ),
      ).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Grace Hopper')).toBeTruthy();
      expect(getButtonByAriaLabel(container, paymentStatusMenuButtonAriaLabel('Grace Hopper'))).toBeTruthy();
      expect(countOccurrences(container, 'Registro #101')).toBe(1);
      expect(countOccurrences(container, 'Registro #102')).toBe(1);
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
        'Cohorte: Beatmaking 101',
      )).toBe(true);
      expect(hasExactText(
        container,
        'Cohorte: Mixing Bootcamp · Fuente: referral',
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
        'Cohorte: Beatmaking 101 · Notas internas',
      )).toBe(true);
      expect(hasExactText(
        container,
        'Cohorte: Mixing Bootcamp',
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
        'Cohorte: Beatmaking 101',
      )).toBe(true);
      expect(hasExactText(
        container,
        'Cohorte: Mixing Bootcamp',
      )).toBe(true);
      expect(hasExactText(
        container,
        'Cohorte: Beatmaking 101 · Notas internas',
      )).toBe(false);
      expect(container.textContent).not.toContain('Creado:');
    });

    await cleanup();
  });

  it('uses the search helper as the only notes explanation when hidden notes drive the match', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(8, (index) => ({
      crAdminNotes: index < 2 ? 'Beca aprobada por coordinación.' : null,
    })));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'beca');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(container.textContent).toContain('Mostrando 2 de 8 inscripciones cargadas.');
      expect(container.textContent).toContain('Coinciden con nota interna.');
      expect(container.textContent).not.toContain('Notas internas en todas las inscripciones visibles.');
      expect(countOccurrences(container, 'nota interna')).toBe(1);
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
      expect(container.textContent).toContain('Formulario público');
      expect(container.textContent).toContain('Beatmaking 101');
      expect(container.textContent).not.toContain('Cohorte disponible');
      expect(container.textContent).not.toContain('Cohorte única por ahora.');
      expect(hasExactText(container, 'Filtrar por estado')).toBe(true);
      expect(container.textContent).not.toContain('Usa Estado para cambiar la vista.');
      expect(container.textContent).not.toContain('Los filtros se aplican automáticamente al cambiar.');
      expect(container.textContent).not.toContain('Empieza por cohorte y estado.');
      expect(container.textContent).not.toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Mostrando una sola cohorte: Beatmaking 101.');
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
      expect(secondContainer.textContent).not.toContain('Formulario público');
      expect(secondContainer.textContent).not.toContain('Cohorte única por ahora.');
    });

    await secondRender.cleanup();
  });

  it('keeps cohort context visible without exposing raw slugs when one configured cohort does not cover every loaded registration', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crCourseSlug: 'legacy-workshop',
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
      expect(container.textContent).not.toContain('Cohorte disponible');
      expect(container.textContent).not.toContain('Cohorte única por ahora.');
      expect(hasExactText(container, 'Cohorte: Beatmaking 101')).toBe(true);
      expect(hasExactText(container, 'Cohorte: Legacy Workshop')).toBe(true);
      expect(container.textContent).not.toContain('Cohorte: legacy-workshop');
      expect(container.textContent).not.toContain('Mostrando una sola cohorte: Beatmaking 101.');
    });

    await cleanup();
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
      expect(container.textContent).toContain('Estado único en esta vista.');
      expect(hasLabel(container, 'Curso / cohorte')).toBe(true);
      expect(container.textContent).not.toContain('Usa cohorte para cambiar la vista.');
      expect(container.textContent).not.toContain('Los filtros se aplican automáticamente al cambiar.');
      expect(container.textContent).not.toContain('Empieza por cohorte y estado.');
    });

    await cleanup();
  });

  it('folds passive single-status context into busy-list search instead of rendering another filter block', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue(buildRegistrations(8, (index) => ({
      crCourseSlug: index % 2 === 0 ? 'beatmaking-101' : 'mixing-bootcamp',
    })));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Curso / cohorte')).toBe(true);
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(container.querySelector('[data-testid="course-registration-single-status-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Estado disponible');
      expect(container.textContent).not.toContain('Estado único en esta vista.');
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.textContent).toContain(
        `Pendiente de pago. Busca dentro de las 8 inscripciones cargadas. ${paymentWorkflowDossierScopeHint}`,
      );
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(0);
      expect(container.querySelectorAll('button[aria-label^="Registrar pago o cambiar estado para "]')).toHaveLength(8);
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(0);
      expect(countButtonsByText(container, openPaymentWorkflowLabel)).toBe(0);
      expect(countOccurrences(container, 'Pendiente de pago')).toBe(1);
    });

    await cleanup();
  });

  it('folds shared source context into busy-list search instead of keeping a passive status block', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue(buildRegistrations(8, (index) => ({
      crCourseSlug: index % 2 === 0 ? 'beatmaking-101' : 'mixing-bootcamp',
      crSource: 'instagram',
    })));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Curso / cohorte')).toBe(true);
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(container.querySelector('[data-testid="course-registration-single-status-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Estado disponible');
      expect(container.textContent).toContain(
        `Pendiente de pago · Fuente visible: Instagram. Busca dentro de las 8 inscripciones cargadas. ${paymentWorkflowDossierScopeHint}`,
      );
      expect(countOccurrences(container, 'Fuente visible: Instagram')).toBe(1);
      expect(container.textContent).not.toContain('Fuente: Instagram');
      expect(countOccurrences(container, 'Pendiente de pago')).toBe(1);
      expect(container.querySelectorAll('button[aria-label^="Registrar pago o cambiar estado para "]')).toHaveLength(8);
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(0);
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
      expect(container.textContent).toContain('Formulario público');
      expect(container.textContent).toContain('Beatmaking 101');
      expect(container.textContent).toContain('Beatmaking 101 · Fuente: Instagram');
      expect(container.textContent).not.toContain('Mostrando una sola fuente: Instagram.');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('humanizes camelCase source labels before showing shared source context', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crSource: 'instagramStory',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'paid',
        crSource: 'instagramStory',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const cohortSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-single-cohort-summary"]',
      );

      expect(cohortSummary).not.toBeNull();
      expect(cohortSummary?.textContent).toContain('Beatmaking 101 · Fuente: Instagram story');
      expect(container.textContent).not.toContain('instagramStory');
      expect(countOccurrences(container, 'Fuente: Instagram story')).toBe(1);
      expect(container.textContent).not.toContain('Fuente visible: Instagram story.');
      expect(getDossierTriggers(container)).toHaveLength(2);
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
      expect(container.textContent).not.toContain('Beatmaking 101 · Pendiente de pago');
      expect(container.textContent).not.toContain(
        'Vista única por ahora: una cohorte y un estado.',
      );
      expect(container.textContent).not.toContain('Cohorte: Beatmaking 101');
      expect(container.textContent).not.toContain('Cohorte disponible');
      expect(container.textContent).not.toContain('Estado disponible');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
    });

    await cleanup();
  });

  it('keeps one registration focused on the row even when multiple cohorts are configured', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Cohorte disponible');
      expect(container.textContent).not.toContain('Estado disponible');
      expect(container.textContent).toContain('Cohorte: Beatmaking 101');
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        paymentWorkflowDossierScopeHint,
      );
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace').textContent?.trim()).toBe('Ada Lovelace');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
      expect(countOccurrences(container, 'Pendiente de pago')).toBe(1);
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
      expect(container.textContent).toContain('Beatmaking 101 · Pendiente de pago');
      expect(container.textContent).toContain('Fuente visible: Instagram.');
      expect(container.textContent).not.toContain('Mostrando una sola fuente: Instagram.');
      expect(container.textContent).not.toContain('Fuente: Instagram');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('uses the concise course label inside the dossier while omitting raw slug and default source noise', async () => {
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
        `Curso: Beatmaking 101 · Creado: ${formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-')}`,
      );
      expect(document.body.textContent).not.toContain('Curso: Beatmaking 101 (beatmaking-101)');
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
        `Curso: Beatmaking 101 · Creado: ${formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-')}`,
      );
      expect(dialog.textContent).not.toContain('Curso: Beatmaking 101 (beatmaking-101)');
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
      expect(dialog.textContent).toContain('Curso: Beatmaking 101 · Fuente: Instagram');
      expect(dialog.textContent).not.toContain('Curso: Beatmaking 101 (beatmaking-101)');
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
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar nota opcional')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar primera nota')).toBe(0);
      expect(countButtonsByText(document.body, 'Abrir notas')).toBe(0);
      expect(countButtonsByText(document.body, 'Editar notas')).toBe(0);
    });

    await openDossierContextAction('Agregar nota');

    await waitForExpectation(() => {
      const dialogHeadings = Array.from(getDialog().querySelectorAll('h6')).map((element) => (element.textContent ?? '').trim());
      expect(dialogHeadings).toContain('Notas internas');
      expect(getButtonByText(document.body, 'Cancelar notas')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Guardar notas')).toBe(0);
      expect(document.body.textContent).toContain('Escribe una nota para mostrar Guardar notas.');
      expect(countButtonsByText(document.body, 'Ocultar editor')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(0);
      expect(document.body.textContent).not.toContain(
        'Aún no hay notas internas. Registra la primera solo cuando necesites dejar contexto, acuerdos o próximos pasos.',
      );
    });

    await act(async () => {
      setInputValue(getInputByLabel(document.body, 'Notas internas'), 'Confirmó pago por WhatsApp.');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Guardar notas').disabled).toBe(false);
      expect(document.body.textContent).not.toContain('Escribe una nota para mostrar Guardar notas.');
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
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(document.body, 'Cancelar notas')).toBe(0);
      expect(countButtonsByText(document.body, 'Guardar notas')).toBe(0);
    });

    await cleanup();
  });

  it('keeps the dialog close action out of inline composer flows', async () => {
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
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
      expect(countButtonsByText(document.body, 'Cerrar')).toBe(1);
    });

    await openDossierContextAction('Agregar nota');

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Cancelar notas')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Cerrar')).toBe(0);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Cancelar notas'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
      expect(countButtonsByText(document.body, 'Cerrar')).toBe(1);
    });

    await cleanup();
  });

  it('keeps sibling dossier add actions hidden while one inline composer is active', async () => {
    const registrationWithNotes = buildRegistration({
      crAdminNotes: 'Confirmo pago por transferencia.',
    });
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: registrationWithNotes,
        crdReceipts: [buildReceipt()],
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
      expect(getButtonByText(document.body, 'Editar notas')).toBeTruthy();
      expect(getButtonByText(document.body, 'Agregar comprobante')).toBeTruthy();
      expect(getButtonByText(document.body, 'Agregar seguimiento')).toBeTruthy();
      expect(document.body.textContent).toContain('receipt.pdf');
      expect(document.body.textContent).toContain('Confirmó transferencia');
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Editar notas'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'Notas internas')).toBe(true);
      expect(getButtonByText(document.body, 'Cancelar notas')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Editar notas')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar comprobante')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(0);
      expect(countButtonsByText(document.body, 'Cancelar comprobante')).toBe(0);
      expect(countButtonsByText(document.body, 'Cancelar seguimiento')).toBe(0);
      expect(document.body.textContent).toContain('receipt.pdf');
      expect(document.body.textContent).toContain('Confirmó transferencia');
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Cancelar notas'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Editar notas')).toBeTruthy();
      expect(getButtonByText(document.body, 'Agregar comprobante')).toBeTruthy();
      expect(getButtonByText(document.body, 'Agregar seguimiento')).toBeTruthy();
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
        'Mostrando una sola cohorte: Beatmaking 101. Fuente visible: Instagram.',
      )).toBe(1);
      expect(container.textContent).not.toContain('Mostrando una sola fuente: Instagram.');
      expect(container.textContent).not.toContain('Fuente: Instagram');
      expect(container.textContent).not.toContain('Cohorte: Beatmaking 101');
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
      expect(container.textContent).toContain('Pendiente de pago · Fuente: Instagram');
      expect(container.textContent).toContain('Cohorte: Beatmaking 101');
      expect(container.textContent).toContain('Cohorte: Mixing Bootcamp');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
    });

    await cleanup();
  });

  it('keeps a source-only summary concise instead of repeating source details on mixed rows', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crSource: 'instagram_story' }),
      buildRegistration({
        crId: 102,
        crCourseSlug: 'mixing-bootcamp',
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'paid',
        crSource: 'Instagram story',
      }),
      buildRegistration({
        crId: 103,
        crFullName: 'Katherine Johnson',
        crEmail: 'katherine@example.com',
        crStatus: 'cancelled',
        crSource: 'instagram-story',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countOccurrences(container, 'Fuente visible: Instagram story.')).toBe(1);
      expect(container.textContent).not.toContain('Mostrando una sola fuente: Instagram story.');
      expect(container.textContent).not.toContain('Fuente: Instagram story');
      expect(container.textContent).toContain('Cohorte: Beatmaking 101');
      expect(container.textContent).toContain('Cohorte: Mixing Bootcamp');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
      expect(container.textContent).toContain('Katherine Johnson');
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
      expect(container.textContent).toContain('Pendiente de pago · Fuente: Instagram');
      expect(container.textContent).not.toContain('Fuente visible: instagram.');
      expect(container.textContent).not.toContain('Fuente visible: Instagram.');
      expect(container.textContent).toContain('Cohorte: Beatmaking 101');
      expect(container.textContent).toContain('Cohorte: Mixing Bootcamp');
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
      expect(hasExactText(container, 'Cohorte: Beatmaking 101')).toBe(true);
      expect(hasExactText(container, 'Cohorte: Mixing Bootcamp · Fuente: referral')).toBe(true);
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
      expect(hasExactText(dialog, 'Curso: Beatmaking 101')).toBe(true);
      expect(dialog.textContent).not.toContain('Curso: Beatmaking 101 (beatmaking-101)');
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
      expect(container.textContent).toContain('Pendiente de pago · Fuente: referral');
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

  it('uses a status chip group for discovery, then switches a single-result status filter to one explicit reset path', async () => {
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
      expect(container.textContent).toContain('Grace Hopper');
      expect(container.textContent).not.toContain('Ada Lovelace');
      expect(container.textContent).not.toContain('Katherine Johnson');
      expect(container.textContent).toContain('Formulario público');
      expect(container.textContent).toContain('Beatmaking 101');
      expect(container.querySelector<HTMLElement>('[data-testid="course-registration-active-status-summary"]')?.textContent).toContain('Estado filtrado');
      expect(container.querySelector<HTMLElement>('[data-testid="course-registration-active-status-summary"]')?.textContent).toContain('Pagado');
      expect(container.querySelector('[role="group"][aria-label="Filtro de estado activo: Pagado"]')).toBeNull();
      expect(container.querySelector(`[aria-label="${clearPaidStatusFilterLabel}"]`)).toBeNull();
      const pendingRecoveryAction = getButtonByAriaLabel(container, 'Marcar pago pendiente para Grace Hopper');
      expect(pendingRecoveryAction.textContent?.trim()).toBe(compactPaymentPendingActionLabel);
      expect(pendingRecoveryAction.getAttribute('title')).toBe('Marcar pago pendiente; actual: Pagado');
      expect(pendingRecoveryAction.getAttribute('aria-haspopup')).toBeNull();
      expect(container.querySelector('button[aria-label="Cambiar estado para Grace Hopper"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista filtrada: estado pagado.');
      expect(countButtonsByText(container, 'Mostrar todos los estados')).toBe(1);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      clickButton(getButtonByText(container, 'Mostrar todos los estados'));
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
      expect(container.textContent).not.toContain('Vista filtrada:');
      expect(container.textContent).not.toContain('Estado disponible');
      expect(container.textContent).not.toContain(
        'Los filtros se aplican automáticamente al cambiar. Empieza por cohorte y estado; Ajustar límite aparecerá cuando esta vista llene el lote actual o si ya estás usando un límite personalizado.',
      );
    });

    await cleanup();
  });

  it('normalizes shared URL status variants before showing the registration list', async () => {
    const pendingRegistration = buildRegistration();
    const paidRegistration = buildRegistration({
      crId: 102,
      crFullName: 'Grace Hopper',
      crEmail: 'grace@example.com',
      crStatus: 'paid',
    });

    listRegistrationsMock.mockImplementation((params) => Promise.resolve(
      params?.status === 'pending_payment'
        ? [pendingRegistration]
        : [pendingRegistration, paidRegistration],
    ));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?status=pending-payment');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: 'pending_payment',
        limit: 200,
      });
      expect(container.querySelector<HTMLElement>('[data-testid="course-registration-active-status-summary"]')?.textContent).toContain('Estado filtrado');
      expect(container.querySelector<HTMLElement>('[data-testid="course-registration-active-status-summary"]')?.textContent).toContain('Pendiente de pago');
      expect(container.querySelector(`[aria-label="${clearPendingStatusFilterLabel}"]`)).toBeNull();
      expect(countButtonsByText(container, 'Mostrar todos los estados')).toBe(1);
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace')).toBeTruthy();
      expect(container.textContent).not.toContain('Grace Hopper');
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
    });

    await cleanup();
  });

  it('keeps a single filtered result focused on shared context instead of a lone status chip', async () => {
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
      expect(container.textContent).toContain('Formulario público');
      expect(container.textContent).toContain('Beatmaking 101');
      const activeStatusSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-active-status-summary"]',
      );
      expect(activeStatusSummary?.textContent).toContain('Estado filtrado');
      expect(activeStatusSummary?.textContent).toContain('Pagado');
      expect(countButtonsByText(activeStatusSummary!, 'Mostrar todos los estados')).toBe(1);
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(hasExactText(container, 'Filtrar por estado')).toBe(false);
      expect(container.querySelector('[role="group"][aria-label="Filtro de estado activo: Pagado"]')).toBeNull();
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Pagado"]')).toBeNull();
      expect(container.querySelector(`[aria-label="${clearPaidStatusFilterLabel}"]`)).toBeNull();
      expect(container.textContent).not.toContain('Mostrando 1 inscripción.');
      expect(countButtonsByText(container, 'Mostrar todos los estados')).toBe(1);
      expect(container.querySelector('[data-testid="course-registration-inline-reset"]')).toBeNull();
      expect(countButtonsByText(container, copyVisibleCsvLabel(1))).toBe(0);
      const pendingRecoveryAction = getButtonByAriaLabel(container, 'Marcar pago pendiente para Grace Hopper');
      expect(pendingRecoveryAction.textContent?.trim()).toBe(compactPaymentPendingActionLabel);
      expect(pendingRecoveryAction.getAttribute('title')).toBe('Marcar pago pendiente; actual: Pagado');
      expect(pendingRecoveryAction.getAttribute('aria-haspopup')).toBeNull();
      expect(container.querySelector('button[aria-label="Cambiar estado para Grace Hopper"]')).toBeNull();
    });

    await cleanup();
  });

  it('folds the shared source into the single-cohort summary instead of stacking another helper row', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crSource: 'meta_ads',
      }),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'paid',
        crSource: 'meta_ads',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const cohortSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-single-cohort-summary"]',
      );

      expect(cohortSummary).not.toBeNull();
      expect(cohortSummary?.textContent).toContain('Formulario público');
      expect(cohortSummary?.textContent).toContain('Beatmaking 101 · Fuente: Meta ads');
      expect(cohortSummary?.textContent).not.toContain('Fuente visible: Meta ads.');
      expect(countOccurrences(cohortSummary!, 'Fuente: Meta ads')).toBe(1);
      expect(container.textContent).not.toContain('Mostrando una sola fuente: Meta ads.');
    });

    await cleanup();
  });

  it('uses direct payment-pending actions once the active filter already states paid status', async () => {
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
      const activeStatusSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-active-status-summary"]',
      );
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')?.textContent?.trim()).toBe(
        paidRecoveryScopeHint,
      );
      expect(activeStatusSummary?.textContent).toContain('Estado filtrado');
      expect(activeStatusSummary?.textContent).toContain('Pagado');
      expect(container.textContent).not.toContain(pendingRecoveryScopeHint);
      expect(container.textContent).not.toContain('el menú de estado muestra acciones.');
      expect(container.querySelector(`[aria-label="${clearPaidStatusFilterLabel}"]`)).toBeNull();
      expect(countButtonsByText(container, 'Mostrar todos los estados')).toBe(1);
      const graceAction = getButtonByAriaLabel(container, 'Marcar pago pendiente para Grace Hopper');
      const katherineAction = getButtonByAriaLabel(container, 'Marcar pago pendiente para Katherine Johnson');
      expect(graceAction.textContent?.trim()).toBe(compactPaymentPendingActionLabel);
      expect(katherineAction.textContent?.trim()).toBe(compactPaymentPendingActionLabel);
      expect(graceAction.getAttribute('title')).toBe('Marcar pago pendiente; actual: Pagado');
      expect(graceAction.getAttribute('aria-haspopup')).toBeNull();
      expect(container.querySelector('button[aria-label="Cambiar estado para Grace Hopper"]')).toBeNull();
      expect(container.querySelector('button[aria-label="Cambiar estado para Katherine Johnson"]')).toBeNull();
      expect(countButtonsByText(container, compactPaymentPendingActionLabel)).toBe(2);
      expect(countButtonsByText(container, markPaymentPendingLabel)).toBe(0);
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(0);
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
      expect(container.textContent).not.toContain('Beatmaking 101 · Pendiente de pago');
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
      expect(container.textContent).not.toContain('Beatmaking 101 · Pendiente de pago');
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
      expect(getButtonByAriaLabel(container, 'Reabrir como pendiente para Katherine Johnson').textContent?.trim()).toBe(reopenPendingLabel);
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').getAttribute('title')).toBe(
        'Registrar pago o cambiar estado; actual: Pendiente de pago',
      );
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').getAttribute('title')).toBe(
        'Cambiar estado; actual: Pagado',
      );
      expect(getButtonByAriaLabel(container, 'Reabrir como pendiente para Katherine Johnson').getAttribute('title')).toBe(
        'Reabrir como pendiente; actual: Cancelado',
      );
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').getAttribute('aria-haspopup')).toBe('menu');
      expect(getButtonByAriaLabel(container, 'Reabrir como pendiente para Katherine Johnson').getAttribute('aria-haspopup')).toBeNull();
      expect(countOccurrences(container, 'Estado:')).toBe(0);
    });

    await cleanup();
  });

  it('keeps active status-filter fallback copy accurate when mixed statuses are returned', async () => {
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
    const { cleanup } = await renderPage(container, '/inscripciones-curso?status=paid');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: 'paid',
        limit: 200,
      });
      expect(container.querySelector('[data-testid="course-registration-active-status-summary"]')).toBeNull();
      expect(container.querySelector('[role="group"][aria-label="Filtro de estado activo: Pagado"]')).not.toBeNull();
      expect(getButtonByAriaLabel(container, clearPaidStatusFilterLabel)).toBeTruthy();
      expect(container.textContent).toContain(activeStatusFilterHelperText);
      expect(container.textContent).not.toContain('Esta vista ya está filtrada por ese estado.');
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
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

  it('keeps mixed standard and custom statuses explicit instead of collapsing to one status summary', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'needs_review',
      }),
      buildRegistration({
        crId: 103,
        crFullName: 'Katherine Johnson',
        crEmail: 'katherine@example.com',
        crStatus: 'waitlist',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-single-status-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Beatmaking 101 · Pendiente de pago');
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pendiente de pago').textContent?.trim()).toBe('Pendiente de pago (1)');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Needs Review');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Katherine Johnson').textContent?.trim()).toBe('Waitlist');
      expect(container.querySelector('[data-testid="course-registration-status-filter-unavailable"]')).toBeNull();
      expect(container.textContent).not.toContain('needs_review');
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

      expect(customStatusSummary?.textContent).toContain(customStatusFilterUnavailableTitle);
      expect(customStatusSummary?.textContent).toContain(customStatusFilterUnavailableMessage);
      expect(customStatusSummary?.textContent).not.toContain('Sin filtros de estado');
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

  it('preserves operational acronyms in custom status fallback labels', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crStatus: 'crm_review',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'api_follow_up',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelector('[data-testid="course-registration-status-filter-unavailable"]')).not.toBeNull();
      const crmStatusAction = getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace');
      const apiStatusAction = getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper');

      expect(crmStatusAction.textContent?.trim()).toBe('CRM Review');
      expect(crmStatusAction.getAttribute('title')).toBe('Cambiar estado; actual: CRM Review');
      expect(apiStatusAction.textContent?.trim()).toBe('API Follow Up');
      expect(apiStatusAction.getAttribute('title')).toBe('Cambiar estado; actual: API Follow Up');
      expect(container.textContent).not.toContain('Crm Review');
      expect(container.textContent).not.toContain('Api Follow Up');
      expect(container.textContent).not.toContain('crm_review');
      expect(container.textContent).not.toContain('api_follow_up');
    });

    await cleanup();
  });

  it('keeps custom-status fallback guidance from duplicating first-run filter onboarding', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crStatus: 'needs_review',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crCourseSlug: 'mixing-bootcamp',
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

      expect(hasLabel(container, 'Curso / cohorte')).toBe(true);
      expect(customStatusSummary?.textContent).toContain(customStatusFilterUnavailableMessage);
      expect(countOccurrences(container, customStatusFilterUnavailableMessage)).toBe(1);
      expect(container.textContent).not.toContain('Los filtros se aplican automáticamente al cambiar.');
      expect(container.textContent).not.toContain('Empieza por cohorte y estado');
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
    });

    await cleanup();
  });

  it('uses busy-list search for custom statuses without adding the status fallback panel', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crStatus: 'needs_review',
      }),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'waitlist',
      }),
      ...buildRegistrations(7, (index) => ({
        crId: 201 + index,
        crPartyId: 31 + index,
        crStatus: 'needs_review',
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const customStatusSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-status-filter-unavailable"]',
      );
      const searchInput = getInputByLabel(container, localSearchLabel);

      expect(searchInput.getAttribute('placeholder')).toBe('Nombre, contacto o estado');
      expect(container.textContent).toContain(customStatusNormalizationScopeHint);
      expect(container.textContent).not.toContain(dossierOnlyScopeHint);
      expect(customStatusSummary).toBeNull();
      expect(container.textContent).not.toContain('Sin filtros de estado');
      expect(container.textContent).not.toContain(customStatusFilterUnavailableMessage);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.querySelector('[role="group"][aria-label="Filtros de estado de inscripciones"]')).toBeNull();
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'waitlist');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Grace Hopper');
      expect(container.textContent).not.toContain('Ada Lovelace');
      expect(container.textContent).toContain('Mostrando 1 de 9 inscripciones cargadas.');
      expect(container.textContent).toContain(customStatusNormalizationScopeHint);
      expect(container.textContent).not.toContain(dossierOnlyScopeHint);
      expect(container.querySelector('[data-testid="course-registration-status-filter-unavailable"]')).toBeNull();
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('folds a shared custom status into busy-list search guidance instead of rendering fallback status chrome', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'live-production', ccTitle: 'Producción en vivo' },
    ]);
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, (index) => ({
        crCourseSlug: index % 2 === 0 ? 'beatmaking-101' : 'live-production',
        crStatus: 'manual_review',
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const searchInput = getInputByLabel(container, localSearchLabel);

      expect(hasLabel(container, 'Curso / cohorte')).toBe(true);
      expect(searchInput.getAttribute('placeholder')).toBe('Nombre o contacto');
      expect(searchInput.getAttribute('placeholder')).not.toContain('curso');
      expect(container.querySelector('[data-testid="course-registration-single-custom-status-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-status-filter-unavailable"]')).toBeNull();
      expect(container.textContent).toContain(
        `Manual Review. Busca dentro de las 9 inscripciones cargadas. ${customStatusNormalizationScopeHint}`,
      );
      expect(container.textContent).not.toContain('Estado no estándar');
      expect(container.textContent).not.toContain(customStatusFilterUnavailableMessage);
      expect(countOccurrences(container, 'Manual Review')).toBe(1);
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(0);
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(9);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    await cleanup();
  });

  it('keeps custom-status row menus focused on normalization instead of payment workflow', async () => {
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
      expect(container.querySelector('[data-testid="course-registration-status-filter-unavailable"]')).not.toBeNull();
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Needs Review');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).not.toContain(openPaymentWorkflowLabel);
      expect(document.body.textContent).not.toContain('Marcar pagado');
      expect(document.body.textContent).not.toContain(markPaymentPendingLabel);
      expect(document.body.textContent).not.toContain('Cancelar inscripción');
      expect(getMenuItemByText(document.body, normalizePaymentPendingLabel)).toBeTruthy();
      expect(getMenuItemByText(document.body, normalizeCancelledLabel)).toBeTruthy();
      expect(getMenuItemByText(document.body, normalizePaymentPendingLabel).getAttribute('title')).toBe(
        'Usa esta acción para normalizar la inscripción a pendiente de pago.',
      );
      expect(getMenuItemByText(document.body, normalizeCancelledLabel).getAttribute('title')).toBe(
        'Usa esta acción para normalizar la inscripción como cancelada.',
      );
    });

    await cleanup();
  });

  it('normalizes known backend status variants before showing filters or row actions', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crStatus: 'PENDING-PAYMENT',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'PAID',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pendiente de pago').textContent?.trim()).toBe('Pendiente de pago (1)');
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pagado').textContent?.trim()).toBe('Pagado (1)');
      expect(container.querySelector('[data-testid="course-registration-status-filter-unavailable"]')).toBeNull();
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Pagado');
      expect(container.textContent).not.toContain('Pending Payment');
      expect(container.textContent).not.toContain('Paid');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).not.toContain(openPaymentWorkflowLabel);
      expect(document.body.textContent).not.toContain('Marcar pagado');
      expect(getMenuItemByText(document.body, markPaymentPendingLabel)).toBeTruthy();
      expect(document.body.textContent).not.toContain('Cancelar inscripción');
    });

    await cleanup();
  });

  it('normalizes terse pending registrations into the standard payment workflow', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crStatus: 'pending',
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
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pendiente de pago').textContent?.trim()).toBe('Pendiente de pago (1)');
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pagado').textContent?.trim()).toBe('Pagado (1)');
      expect(container.querySelector('[data-testid="course-registration-status-filter-unavailable"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-single-custom-status-summary"]')).toBeNull();
      expect(container.textContent).not.toContain(customStatusFilterUnavailableMessage);
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Pagado');
      expect(container.textContent).not.toContain('Pending');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getMenuItemByText(document.body, openPaymentWorkflowLabel)).toBeTruthy();
      expect(getMenuItemByText(document.body, 'Cancelar inscripción')).toBeTruthy();
    });

    await cleanup();
  });

  it('normalizes common payment and cancellation aliases into the standard status controls', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crStatus: 'payment_pending',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'canceled',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pendiente de pago').textContent?.trim()).toBe('Pendiente de pago (1)');
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Cancelado').textContent?.trim()).toBe('Cancelado (1)');
      expect(container.querySelector('[data-testid="course-registration-status-filter-unavailable"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-single-custom-status-summary"]')).toBeNull();
      expect(container.textContent).not.toContain(customStatusFilterUnavailableMessage);
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
      expect(getButtonByAriaLabel(container, 'Reabrir como pendiente para Grace Hopper').textContent?.trim()).toBe(reopenPendingLabel);
      expect(container.textContent).not.toContain('Payment Pending');
      expect(container.textContent).not.toContain('Canceled');
    });

    await cleanup();
  });

  it('collapses one shared custom status into the current-view summary instead of repeating passive filter chrome', async () => {
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
      const currentViewSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-current-view-summary"]',
      );

      expect(currentViewSummary?.textContent).toContain('Vista actual');
      expect(currentViewSummary?.textContent).toContain('Beatmaking 101 · Needs Review');
      expect(currentViewSummary?.textContent).toContain(customStatusFilterUnavailableMessage);
      expect(currentViewSummary?.textContent).not.toContain('Vista única por ahora: una cohorte y un estado.');
      expect(container.querySelector('[data-testid="course-registration-single-cohort-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-single-custom-status-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-status-filter-unavailable"]')).toBeNull();
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Cambiar estado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Cambiar estado');
      expect(countOccurrences(container, 'Needs Review')).toBe(1);
      expect(countOccurrences(container, 'Usa Estado')).toBe(0);
      expect(countOccurrences(container, customStatusFilterUnavailableMessage)).toBe(1);
      expect(container.textContent).not.toContain('needs_review');
    });

    await cleanup();
  });

  it('normalizes shared custom status variants before falling back to repeated row labels', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crStatus: 'needs-review',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'Needs Review',
      }),
      buildRegistration({
        crId: 103,
        crFullName: 'Katherine Johnson',
        crEmail: 'katherine@example.com',
        crStatus: 'needs.review',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const currentViewSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-current-view-summary"]',
      );

      expect(currentViewSummary?.textContent).toContain('Beatmaking 101 · Needs Review');
      expect(currentViewSummary?.textContent).toContain(customStatusFilterUnavailableMessage);
      expect(container.querySelector('[data-testid="course-registration-single-custom-status-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-status-filter-unavailable"]')).toBeNull();
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Cambiar estado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Cambiar estado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Katherine Johnson').textContent?.trim()).toBe('Cambiar estado');
      expect(countOccurrences(container, 'Needs Review')).toBe(1);
      expect(container.textContent).not.toContain('needs-review');
      expect(container.textContent).not.toContain('needs.review');
    });

    await cleanup();
  });

  it('summarizes shared blank statuses once instead of repeating unknown-state row labels', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crStatus: '   ',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: '',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const currentViewSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-current-view-summary"]',
      );

      expect(currentViewSummary?.textContent).toContain('Beatmaking 101 · Estado desconocido');
      expect(currentViewSummary?.textContent).toContain(customStatusFilterUnavailableMessage);
      expect(container.querySelector('[data-testid="course-registration-single-custom-status-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-status-filter-unavailable"]')).toBeNull();
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Cambiar estado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Cambiar estado');
      expect(countOccurrences(container, 'Estado desconocido')).toBe(1);
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
      expect(container.textContent).toContain('Estado único en esta vista.');
      expect(container.textContent).not.toContain('Usa cohorte para cambiar la vista.');
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
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(2);
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Pagado');
      expect(getButtonByAriaLabel(container, 'Reabrir como pendiente para Katherine Johnson').textContent?.trim()).toBe(reopenPendingLabel);
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').getAttribute('aria-haspopup')).toBe('menu');
      expect(getButtonByAriaLabel(container, 'Reabrir como pendiente para Katherine Johnson').getAttribute('aria-haspopup')).toBeNull();
      expect(container.textContent).not.toContain('Estado disponible');
      expect(countOccurrences(container, 'Cambiar estado:')).toBe(0);
      expect(countOccurrences(container, 'Estado:')).toBe(0);
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(0);
      expect(container.querySelector('button[aria-label="Subir comprobante y marcar pagado para Ada Lovelace"]')).toBeNull();
      expect(container.querySelector('button[aria-label="Marcar pago pendiente para Grace Hopper"]')).toBeNull();
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
      expect(getMenuItemByText(document.body, openPaymentWorkflowLabel).getAttribute('aria-label')).toBe(
        'Registrar pago para Ada Lovelace',
      );
      expect(getMenuItemByText(document.body, 'Cancelar inscripción').getAttribute('aria-label')).toBe(
        'Cancelar inscripción para Ada Lovelace',
      );
      expect(document.body.textContent).not.toContain(markPaymentPendingLabel);
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
      expect(document.body.textContent).toContain(markPaymentPendingLabel);
      expect(document.body.textContent).not.toContain('Cancelar inscripción');
      expect(getMenuItemByText(document.body, markPaymentPendingLabel).getAttribute('aria-label')).toBe(
        'Marcar pago pendiente para Grace Hopper',
      );
      expect(
        Array.from(document.body.querySelectorAll('[role="menuitem"]')).map((element) => (element.textContent ?? '').trim()),
      ).toEqual([markPaymentPendingLabel]);
      expect(
        document.body.querySelector('[role="menuitem"][aria-label="Cancelar inscripción para Grace Hopper"]'),
      ).toBeNull();
      expect(document.body.textContent).not.toContain(openPaymentWorkflowLabel);
      expect(document.body.textContent).not.toContain('Estado actual:');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Reabrir como pendiente para Katherine Johnson'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Estado actualizado para Katherine Johnson.');
      expect(document.body.querySelectorAll('[role="menuitem"]')).toHaveLength(0);
      expect(document.body.textContent).not.toContain(openPaymentWorkflowLabel);
      expect(document.body.textContent).not.toContain(markPaymentPendingLabel);
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
      expect(countButtonsByText(actions!, compactOptionalDossierContextActionsLabel)).toBe(1);
      expect(countButtonsByText(actions!, optionalDossierContextActionsLabel)).toBe(0);
      expect(countButtonsByText(actions!, 'Más contexto')).toBe(0);
      expect(countButtonsByText(actions!, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(actions!, 'Agregar seguimiento')).toBe(0);
      expect(document.body.querySelector('[aria-label="Refrescar expediente"]')).toBeNull();
      expect(document.body.querySelector('[aria-label="Refrescar expediente y correos"]')).toBeNull();
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

  it('deduplicates repeated system-email events before rendering history cards', async () => {
    listRegistrationEmailsMock.mockResolvedValue([
      buildEmailEvent({
        ceStatus: '   ',
        ceEventType: '   ',
        ceMessage: '   ',
        ceCreatedAt: '   ',
      }),
      buildEmailEvent({
        ceStatus: 'sent',
        ceEventType: 'payment_reminder',
        ceMessage: 'Recordatorio consolidado.',
        ceCreatedAt: '2030-03-03T12:00:00.000Z',
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
      expect(getButtonByText(document.body, showSystemEmailsLabel)).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, showSystemEmailsLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Recordatorio consolidado.');
      expect(document.body.textContent).toContain('Recordatorio de pago');
      expect(document.body.textContent).toContain('Enviado');
      expect(countOccurrences(document.body, 'Recordatorio consolidado.')).toBe(1);
      expect(countOccurrences(document.body, 'Recordatorio de pago')).toBe(1);
      expect(countOccurrences(document.body, 'Enviado')).toBe(1);
      expect(document.body.textContent).not.toContain('Estado desconocido');
    });

    await cleanup();
  });

  it('surfaces a failed system-email preload as one explicit retry action', async () => {
    listRegistrationEmailsMock
      .mockRejectedValueOnce(new Error('Email service unavailable'))
      .mockResolvedValueOnce([
        buildEmailEvent({
          ceMessage: 'Recordatorio reenviado.',
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
      expect(getButtonByText(document.body, retrySystemEmailsLabel)).toBeTruthy();
      expect(countButtonsByText(document.body, retrySystemEmailsLabel)).toBe(1);
      expect(countButtonsByText(document.body, showSystemEmailsLabel)).toBe(0);
      expect(document.body.textContent).not.toContain('No se pudo cargar el historial');
      expect(document.body.querySelector('[aria-label="Refrescar expediente y correos"]')).toBeNull();
      expect(listRegistrationEmailsMock).toHaveBeenCalledTimes(1);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, retrySystemEmailsLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationEmailsMock).toHaveBeenCalledTimes(2);
      expect(getButtonByText(document.body, hideSystemEmailsLabel)).toBeTruthy();
      expect(document.body.textContent).toContain('Recordatorio reenviado.');
      expect(document.body.textContent).not.toContain('No se pudo cargar el historial');
      expect(countButtonsByText(document.body, retrySystemEmailsLabel)).toBe(0);
    });

    await cleanup();
  });

  it('groups the only remaining dossier context action when system-email history is primary', async () => {
    const registrationWithNotes = buildRegistration({
      crAdminNotes: 'Confirmó pago por transferencia.',
    });
    listRegistrationsMock.mockResolvedValue([registrationWithNotes]);
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: registrationWithNotes,
        crdFollowUps: [],
      }),
    );
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
      expect(countButtonsByText(actions!, showSystemEmailsLabel)).toBe(1);
      expect(countButtonsByText(actions!, compactOptionalDossierContextActionsLabel)).toBe(1);
      expect(getButtonByText(actions!, compactOptionalDossierContextActionsLabel).getAttribute('aria-label')).toBe(
        optionalDossierFollowUpActionLabel,
      );
      expect(getButtonByText(actions!, compactOptionalDossierContextActionsLabel).getAttribute('title')).toBe(
        optionalDossierFollowUpActionLabel,
      );
      expect(countButtonsByText(actions!, optionalDossierFollowUpActionLabel)).toBe(0);
      expect(countButtonsByText(actions!, optionalDossierContextActionsLabel)).toBe(0);
      expect(countButtonsByText(actions!, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(actions!, 'Agregar seguimiento')).toBe(0);
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, compactOptionalDossierContextActionsLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getMenuItemByText(document.body, optionalDossierFollowUpActionLabel)).toBeTruthy();
      expect(
        Array.from(document.body.querySelectorAll('[role="menuitem"]')).some(
          (item) => (item.textContent ?? '').trim() === 'Agregar seguimiento',
        ),
      ).toBe(false);
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, optionalDossierFollowUpActionLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(firstFollowUpComposerHelpText);
      expect(countButtonsByText(document.body, compactOptionalDossierContextActionsLabel)).toBe(0);
      expect(countButtonsByText(document.body, optionalDossierFollowUpActionLabel)).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(0);
      expect(
        Array.from(document.body.querySelectorAll('[role="menuitem"]')).some(
          (item) => (item.textContent ?? '').trim() === 'Agregar seguimiento',
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('names the lone internal-note action explicitly when follow-up history already exists', async () => {
    const registrationWithoutNotes = buildRegistration({ crAdminNotes: null });
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: registrationWithoutNotes,
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
      const actions = document.body.querySelector<HTMLElement>('[data-testid="course-registration-dossier-actions"]');

      expect(actions).toBeTruthy();
      expect(countButtonsByText(actions!, optionalDossierNotesActionLabel)).toBe(1);
      expect(countButtonsByText(actions!, optionalDossierContextActionsLabel)).toBe(0);
      expect(countButtonsByText(actions!, 'Agregar nota')).toBe(0);
      expect(getButtonByText(document.body, 'Agregar seguimiento')).toBeTruthy();
      expect(document.body.textContent).not.toContain(
        'Aún no hay notas internas. Registra la primera solo cuando necesites dejar contexto, acuerdos o próximos pasos.',
      );
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, optionalDossierNotesActionLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'Notas internas')).toBe(true);
      expect(countButtonsByText(document.body, 'Guardar notas')).toBe(0);
      expect(document.body.textContent).toContain('Escribe una nota para mostrar Guardar notas.');
      expect(countButtonsByText(document.body, optionalDossierNotesActionLabel)).toBe(0);
    });

    await cleanup();
  });

  it('summarizes shared system-email dates once instead of repeating them per email', async () => {
    const sharedEmailCreatedAt = '2030-03-03T12:00:00.000Z';
    const sharedEmailCreatedLabel = formatTimestampForDisplay(sharedEmailCreatedAt, '-');
    listRegistrationEmailsMock.mockResolvedValue([
      buildEmailEvent({
        ceId: 501,
        ceEventType: 'registration_confirmation',
        ceMessage: 'Confirmación enviada.',
        ceCreatedAt: sharedEmailCreatedAt,
      }),
      buildEmailEvent({
        ceId: 502,
        ceEventType: 'payment_reminder',
        ceMessage: 'Recordatorio enviado.',
        ceCreatedAt: sharedEmailCreatedAt,
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
      expect(getButtonByText(document.body, showSystemEmailsLabel)).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, showSystemEmailsLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(`Correos registrados: ${sharedEmailCreatedLabel}`);
      expect(document.body.textContent).toContain('Confirmación enviada.');
      expect(document.body.textContent).toContain('Recordatorio enviado.');
      expect(document.body.textContent).toContain('Confirmación de inscripción');
      expect(document.body.textContent).toContain('Recordatorio de pago');
      expect(countOccurrences(document.body, sharedEmailCreatedLabel)).toBe(1);
    });

    await cleanup();
  });

  it('summarizes repeated system-email types once instead of repeating the same chip per card', async () => {
    listRegistrationEmailsMock.mockResolvedValue([
      buildEmailEvent({
        ceId: 501,
        ceMessage: 'Primer recordatorio enviado.',
        ceCreatedAt: '2030-03-03T12:00:00.000Z',
      }),
      buildEmailEvent({
        ceId: 502,
        ceMessage: 'Segundo recordatorio enviado.',
        ceCreatedAt: '2030-03-04T12:00:00.000Z',
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
      expect(getButtonByText(document.body, showSystemEmailsLabel)).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, showSystemEmailsLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Tipo de correo: Recordatorio de pago');
      expect(document.body.textContent).toContain('Primer recordatorio enviado.');
      expect(document.body.textContent).toContain('Segundo recordatorio enviado.');
      expect(countOccurrences(document.body, 'Recordatorio de pago')).toBe(1);
    });

    await cleanup();
  });

  it('summarizes repeated system-email statuses once instead of repeating the same chip per card', async () => {
    listRegistrationEmailsMock.mockResolvedValue([
      buildEmailEvent({
        ceId: 501,
        ceEventType: 'registration_confirmation',
        ceMessage: 'Confirmación lista.',
        ceCreatedAt: '2030-03-03T12:00:00.000Z',
      }),
      buildEmailEvent({
        ceId: 502,
        ceEventType: 'payment_reminder',
        ceMessage: 'Recordatorio listo.',
        ceCreatedAt: '2030-03-04T12:00:00.000Z',
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
      expect(getButtonByText(document.body, showSystemEmailsLabel)).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, showSystemEmailsLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Estado de correos: Enviado');
      expect(document.body.textContent).toContain('Confirmación de inscripción');
      expect(document.body.textContent).toContain('Recordatorio de pago');
      expect(document.body.textContent).toContain('Confirmación lista.');
      expect(document.body.textContent).toContain('Recordatorio listo.');
      expect(countOccurrences(document.body, 'Enviado')).toBe(1);
    });

    await cleanup();
  });

  it('summarizes blank system-email statuses once instead of repeating unknown-state chips', async () => {
    listRegistrationEmailsMock.mockResolvedValue([
      buildEmailEvent({
        ceId: 501,
        ceStatus: '   ',
        ceEventType: 'registration_confirmation',
        ceMessage: 'Confirmación sin estado.',
        ceCreatedAt: '2030-03-03T12:00:00.000Z',
      }),
      buildEmailEvent({
        ceId: 502,
        ceStatus: '',
        ceEventType: 'payment_reminder',
        ceMessage: 'Recordatorio sin estado.',
        ceCreatedAt: '2030-03-04T12:00:00.000Z',
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
      expect(getButtonByText(document.body, showSystemEmailsLabel)).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, showSystemEmailsLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Estado de correos: Estado no registrado');
      expect(document.body.textContent).toContain('Confirmación sin estado.');
      expect(document.body.textContent).toContain('Recordatorio sin estado.');
      expect(countOccurrences(document.body, 'Estado no registrado')).toBe(1);
      expect(document.body.textContent).not.toContain('Estado desconocido');
    });

    await cleanup();
  });

  it('combines shared system-email type and date into one summary line', async () => {
    const sharedEmailCreatedAt = '2030-03-03T12:00:00.000Z';
    const sharedEmailCreatedLabel = formatTimestampForDisplay(sharedEmailCreatedAt, '-');
    listRegistrationEmailsMock.mockResolvedValue([
      buildEmailEvent({
        ceId: 501,
        ceMessage: 'Primer recordatorio enviado.',
        ceCreatedAt: sharedEmailCreatedAt,
      }),
      buildEmailEvent({
        ceId: 502,
        ceMessage: 'Segundo recordatorio enviado.',
        ceCreatedAt: sharedEmailCreatedAt,
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
      expect(getButtonByText(document.body, showSystemEmailsLabel)).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, showSystemEmailsLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(`Resumen: Recordatorio de pago · Enviado · ${sharedEmailCreatedLabel}`);
      expect(document.body.textContent).not.toContain('Estado de correos:');
      expect(document.body.textContent).not.toContain('Correos registrados:');
      expect(document.body.textContent).not.toContain('Tipo de correo:');
      expect(document.body.textContent).toContain('Primer recordatorio enviado.');
      expect(document.body.textContent).toContain('Segundo recordatorio enviado.');
      expect(countOccurrences(document.body, 'Recordatorio de pago')).toBe(1);
      expect(countOccurrences(document.body, 'Enviado')).toBe(1);
      expect(countOccurrences(document.body, sharedEmailCreatedLabel)).toBe(1);
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
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(0);
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
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(0);
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
      const compactAction = getButtonByText(actions ?? document.body, compactOptionalDossierContextActionsLabel);

      expect(actions).toBeTruthy();
      expect(countButtonsByText(actions!, 'Marcar pagado')).toBe(1);
      expect(countButtonsByText(actions!, compactOptionalDossierContextActionsLabel)).toBe(1);
      expect(countButtonsByText(actions!, optionalDossierContextActionsLabel)).toBe(0);
      expect(countButtonsByText(actions!, 'Más contexto')).toBe(0);
      expect(countButtonsByText(actions!, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(actions!, 'Agregar seguimiento')).toBe(0);
      expect(compactAction.getAttribute('aria-label')).toBe(optionalDossierContextActionsLabel);
      expect(compactAction.getAttribute('title')).toBe(optionalDossierContextActionsLabel);
      expect(actions?.textContent).not.toContain(emptySystemEmailHistoryMessage);
      expect(actions?.textContent).not.toContain(showSystemEmailsLabel);
      expect(emptyHint).toBeNull();
      expect(document.body.textContent).not.toContain(emptySystemEmailHistoryMessage);
      expect(document.body.textContent).toContain(emptyReceiptEvidenceAlertMessage);
      expect(document.body.textContent).not.toContain(emptyReceiptAlertMessage);
      expect(document.body.textContent).not.toContain('habilitar Marcar pagado');
      expect(document.body.textContent).not.toContain('El primer comprobante queda como evidencia');
      expect(document.body.textContent).not.toContain('enlace y acciones para revisarlo');
      expect(document.body.querySelector('[aria-label="Refrescar expediente"]')).toBeNull();
      expect(document.body.querySelector('[aria-label="Refrescar expediente y correos"]')).toBeNull();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, compactOptionalDossierContextActionsLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const notesItem = getMenuItemByText(document.body, optionalDossierNotesActionLabel);
      const followUpItem = getMenuItemByText(document.body, optionalDossierFollowUpActionLabel);

      expect(notesItem).toBeTruthy();
      expect(notesItem.getAttribute('aria-label')).toBe(optionalDossierNotesActionLabel);
      expect(notesItem.getAttribute('title')).toBe(optionalDossierNotesActionLabel);
      expect(followUpItem).toBeTruthy();
      expect(followUpItem.getAttribute('aria-label')).toBe(optionalDossierFollowUpActionLabel);
      expect(followUpItem.getAttribute('title')).toBe(optionalDossierFollowUpActionLabel);
      expect(
        Array.from(document.body.querySelectorAll('[role="menuitem"]')).some(
          (item) => (item.textContent ?? '').trim() === 'Agregar nota',
        ),
      ).toBe(false);
      expect(
        Array.from(document.body.querySelectorAll('[role="menuitem"]')).some(
          (item) => (item.textContent ?? '').trim() === 'Agregar seguimiento',
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('groups a single optional dossier action when payment is the primary action', async () => {
    const registration = buildRegistration({
      crAdminNotes: 'Confirmó pago por transferencia.',
    });
    getRegistrationDossierMock.mockResolvedValue(buildDossier({
      crdRegistration: registration,
      crdCanMarkPaid: true,
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
      const actions = document.body.querySelector<HTMLElement>('[data-testid="course-registration-dossier-actions"]');

      expect(actions).toBeTruthy();
      expect(countButtonsByText(actions!, 'Marcar pagado')).toBe(1);
      expect(countButtonsByText(actions!, compactOptionalDossierContextActionsLabel)).toBe(1);
      expect(getButtonByText(actions!, compactOptionalDossierContextActionsLabel).getAttribute('aria-label')).toBe(
        optionalDossierFollowUpActionLabel,
      );
      expect(getButtonByText(actions!, compactOptionalDossierContextActionsLabel).getAttribute('title')).toBe(
        optionalDossierFollowUpActionLabel,
      );
      expect(countButtonsByText(actions!, optionalDossierFollowUpActionLabel)).toBe(0);
      expect(countButtonsByText(actions!, optionalDossierContextActionsLabel)).toBe(0);
      expect(countButtonsByText(actions!, 'Agregar seguimiento')).toBe(0);
      expect(document.body.textContent).toContain('Confirmó pago por transferencia.');
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(
        Array.from(document.body.querySelectorAll('[role="menuitem"]')).some(
          (item) => (item.textContent ?? '').trim() === 'Agregar seguimiento',
        ),
      ).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, compactOptionalDossierContextActionsLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getMenuItemByText(document.body, optionalDossierFollowUpActionLabel)).toBeTruthy();
      expect(
        Array.from(document.body.querySelectorAll('[role="menuitem"]')).some(
          (item) => (item.textContent ?? '').trim() === 'Agregar seguimiento',
        ),
      ).toBe(false);
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, optionalDossierFollowUpActionLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'Nota de seguimiento')).toBe(true);
      expect(countButtonsByText(document.body, compactOptionalDossierContextActionsLabel)).toBe(0);
      expect(countButtonsByText(document.body, optionalDossierFollowUpActionLabel)).toBe(0);
      expect(document.body.textContent).toContain(firstFollowUpComposerHelpText);
    });

    await cleanup();
  });

  it('localizes dossier activity labels so admins do not see raw internal event jargon', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdFollowUps: [
          buildFollowUp({
            crfEntryType: 'status-change',
            crfSubject: null,
          }),
        ],
      }),
    );
    listRegistrationEmailsMock.mockResolvedValue([
      buildEmailEvent({
        ceEventType: 'registration-confirmation',
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
      expect(document.body.textContent).not.toContain('Status-change');
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
      expect(document.body.textContent).not.toContain('Registration-confirmation');
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

  it('keeps the dossier refresh out of the title while an inline receipt editor is open', async () => {
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
      expect(getButtonByText(document.body, 'Agregar comprobante')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.querySelector('[aria-label="Refrescar expediente"]')).toBeNull();
      expect(document.body.querySelector('[aria-label="Refrescar expediente y correos"]')).toBeNull();
      expect(countButtonsByText(document.body, 'Guardar comprobante')).toBe(0);
      expect(getButtonByText(document.body, 'Cancelar comprobante')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Cancelar comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByAriaLabel(document.body, 'Refrescar expediente')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Guardar comprobante')).toBe(0);
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
      const firstReceiptAction = getButtonByText(document.body, 'Agregar comprobante');
      expect(firstReceiptAction).toBeTruthy();
      expect(firstReceiptAction.getAttribute('aria-label')).toBe('Agregar primer comprobante');
      expect(firstReceiptAction.getAttribute('title')).toBe('Agregar primer comprobante');
      expect(countButtonsByText(document.body, 'Agregar primer comprobante')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar evidencia')).toBe(0);
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Agregar primer comprobante',
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
      expect(document.body.textContent).not.toContain('El primer comprobante queda como evidencia');
      expect(document.body.textContent).not.toContain('enlace y acciones para revisarlo');
      expect(countButtonsByText(document.body, 'Agregar evidencia')).toBe(1);
      expect(countButtonsByText(document.body, 'Agregar primer comprobante')).toBe(0);
      expect(countButtonsByText(document.body, 'Marcar pagado')).toBe(0);
    });

    await cleanup();
  });

  it('keeps cancelled dossiers focused by hiding empty payment-evidence prompts', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration({ crStatus: 'cancelled' }),
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
      expect(document.body.textContent).toContain('Cancelado');
      expect(document.body.textContent).not.toContain('Comprobantes de pago');
      expect(document.body.textContent).not.toContain(emptyReceiptEvidenceAlertMessage);
      expect(document.body.textContent).not.toContain(emptyReceiptAlertMessage);
      expect(countButtonsByText(document.body, 'Agregar primer comprobante')).toBe(0);
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
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
      expect(getButtonByText(document.body, 'Agregar comprobante')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(firstReceiptComposerHelpText);
      expect(document.body.textContent).not.toContain(emptyReceiptAlertMessage);
      expect(countButtonsByText(document.body, 'Guardar comprobante')).toBe(0);
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
      clickButton(getButtonByText(document.body, 'Agregar comprobante'));
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

  it('keeps saved receipts easy to scan while the receipt form owns the active actions', async () => {
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
      expect(() =>
        getButtonByAriaLabel(document.body, 'Abrir acciones para comprobante receipt.pdf'),
      ).toThrow();
      expect(document.body.textContent).not.toContain(editingReceiptComposerHelpText);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Cancelar comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.querySelector('[data-testid="course-registration-receipt-composer-pane"]')).toBeNull();
      expect(getButtonByAriaLabel(document.body, 'Abrir acciones para comprobante receipt.pdf')).toBeTruthy();
      expect(document.body.textContent).not.toContain(receiptComposerHelpText);
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

  it('omits missing receipt upload dates instead of rendering placeholder text', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [buildReceipt({ crrCreatedAt: undefined })],
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
      expect(document.body.textContent).toContain('receipt.pdf');
      expect(document.body.textContent).not.toContain('Subido: -');
      expect(document.body.textContent).not.toContain('Subido:');
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
      expect(getButtonByText(document.body, 'Agregar comprobante')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar comprobante'));
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
      expect(hasLabel(document.body, 'Nombre visible')).toBe(false);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(false);
      expect(document.body.querySelector('[data-testid="mock-receipt-upload"]')).toBeNull();
      expect(document.body.textContent).toContain(
        'Pega un enlace existente; si prefieres subir un archivo, oculta este campo.',
      );
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
      expect(document.body.querySelector('[data-testid="mock-receipt-upload"]')).not.toBeNull();
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Ocultar enlace existente',
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('removes the alternate receipt-link action once a receipt file is ready', async () => {
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
      expect(getButtonByText(document.body, 'Agregar comprobante')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir archivo')).toBeTruthy();
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(false);
    });

    const uploadButton = document.body.querySelector<HTMLButtonElement>('[data-testid="mock-receipt-upload"]');
    if (!uploadButton) throw new Error('Mock receipt upload not found');

    await act(async () => {
      clickButton(uploadButton);
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countButtonsByText(document.body, 'Usar enlace existente en lugar de subir archivo')).toBe(0);
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(false);
      expect(hasLabel(document.body, 'Nombre visible')).toBe(true);
      expect(getInputByLabel(document.body, 'Nombre visible').value).toBe('mock-receipt.pdf');
      expect(getButtonByText(document.body, 'Guardar comprobante').disabled).toBe(false);
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
      expect(countButtonsByText(document.body, 'Guardar comprobante')).toBe(0);
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
      expect(document.body.textContent).not.toContain(markPaidEmptyNotesHelperText);
      expect(document.body.textContent).not.toContain(markPaidEmptyFollowUpHelperText);
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(document.body, markPaidOptionalFollowUpActionLabel)).toBe(0);
      const dialogHeadings = Array.from(getDialog().querySelectorAll('h6')).map((element) => (element.textContent ?? '').trim());
      expect(dialogHeadings.indexOf('Comprobantes de pago')).toBeGreaterThan(-1);
      expect(dialogHeadings).not.toContain('Notas internas (opcional)');
      expect(dialogHeadings).not.toContain('Seguimiento (opcional)');
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Usar enlace existente en lugar de subir archivo'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(true);
      expect(hasLabel(document.body, 'Nombre visible')).toBe(false);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(false);
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

  it('keeps one explicit close action while the payment workflow is still loading', async () => {
    getRegistrationDossierMock.mockImplementation(() => new Promise(() => undefined));

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
        expect(document.body.textContent).toContain('Cargando expediente…');
        expect(countButtonsByText(document.body, 'Cerrar')).toBe(1);
        expect(countButtonsByText(document.body, 'Cerrar pago')).toBe(0);
        expect(countButtonsByText(document.body, 'Guardar comprobante')).toBe(0);
        expect(countButtonsByText(document.body, 'Agregar primer comprobante')).toBe(0);
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

  it('keeps optional notes hidden while the first payment receipt is still missing', async () => {
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
      expect(document.body.textContent).toContain('Registrar pago de inscripción');
      expect(document.body.textContent).toContain(
        'Este formulario ya está abierto para registrar el primer comprobante. Guárdalo y luego podrás marcar la inscripción como pagada.',
      );
      expect(document.body.textContent).not.toContain(markPaidEmptyNotesHelperText);
      expect(hasExactText(document.body, 'Notas internas (opcional)')).toBe(false);
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar primera nota')).toBe(0);
      expect(document.body.textContent).not.toContain(
        'Aún no hay notas internas. Registra la primera solo cuando necesites dejar contexto, acuerdos o próximos pasos.',
      );
      expect(hasLabel(document.body, 'Notas internas')).toBe(false);
    });

    await cleanup();
  });

  it('keeps optional follow-up hidden while the first payment receipt is still missing', async () => {
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
      expect(document.body.textContent).toContain('Registrar pago de inscripción');
      expect(hasExactText(document.body, 'Seguimiento (opcional)')).toBe(false);
      expect(document.body.textContent).not.toContain(markPaidEmptyFollowUpHelperText);
      expect(countButtonsByText(document.body, markPaidOptionalFollowUpActionLabel)).toBe(0);
      expect(countButtonsByText(document.body, markPaidOptionalFollowUpAccessibleLabel)).toBe(0);
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(0);
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(hasLabel(document.body, 'Nota de seguimiento')).toBe(false);
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
      const dialog = getDialog();

      expect(document.body.textContent).toContain('Confirmar pago de inscripción');
      expect(document.body.textContent).not.toContain('Expediente de inscripción');
      expect(getButtonByText(document.body, 'Marcar pagado')).toBeTruthy();
      expect(hasExactText(dialog, 'Pendiente de pago')).toBe(false);
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
        const dialog = getDialog();
        expect(dialog.textContent).toContain('Pago registrado');
        expect(dialog.textContent).not.toContain('Confirmar pago de inscripción');
        expect(hasExactText(dialog, 'Pagado')).toBe(true);
        expect(hasExactText(dialog, 'Pendiente de pago')).toBe(false);
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
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(0);
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

    await openDossierContextAction('Agregar nota');

    await waitForExpectation(() => {
      const dialogHeadings = Array.from(getDialog().querySelectorAll('h6')).map((element) => (element.textContent ?? '').trim());
      expect(dialogHeadings).toContain('Notas internas');
      expect(hasLabel(document.body, 'Notas internas')).toBe(true);
      expect(countButtonsByText(document.body, 'Guardar notas')).toBe(0);
      expect(document.body.textContent).toContain('Escribe una nota para mostrar Guardar notas.');
    });

    await act(async () => {
      setInputValue(getInputByLabel(document.body, 'Notas internas'), 'Confirmó pago por WhatsApp.');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Guardar notas').disabled).toBe(false);
      expect(document.body.textContent).not.toContain('Escribe una nota para mostrar Guardar notas.');
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
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(0);
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

    await openDossierContextAction('Agregar seguimiento');

    await waitForExpectation(() => {
      expect(hasExactText(document.body, 'Registrar seguimiento')).toBe(false);
      expect(document.body.textContent).not.toContain(emptyFollowUpAlertMessage);
      expect(document.body.textContent).toContain(firstFollowUpComposerHelpText);
      expect(document.body.textContent).not.toContain(
        'Abre el formulario solo cuando necesites documentar una llamada, mensaje o próximo paso.',
      );
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar seguimiento')).toBe(0);
      expect(countButtonsByText(document.body, 'Agregar nota')).toBe(0);
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
      expect(countButtonsByText(document.body, 'Guardar seguimiento')).toBe(0);
      expect(getButtonByText(document.body, 'Cancelar seguimiento')).toBeTruthy();
    });

    await act(async () => {
      setInputValue(getInputByLabel(document.body, 'Nota de seguimiento'), 'Confirmó que enviará el comprobante.');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Guardar seguimiento')).toBeTruthy();
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

  it('limits new follow-up type choices to manual contact actions', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(getButtonByText(container, 'Expediente')).toBeTruthy();
      });

      await act(async () => {
        clickButton(getButtonByText(container, 'Expediente'));
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
      });

      await openDossierContextAction('Agregar seguimiento');

      await act(async () => {
        clickButton(getButtonByText(document.body, 'Agregar detalles opcionales'));
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(hasLabel(document.body, 'Tipo')).toBe(true);
      });

      await act(async () => {
        mouseDownElement(getComboboxByLabel(document.body, 'Tipo'));
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        const options = Array.from(document.body.querySelectorAll<HTMLElement>('[role="option"]'))
          .map((option) => (option.textContent ?? '').trim())
          .filter(Boolean);

        expect(options).toEqual(['Nota', 'Llamada', 'WhatsApp', 'Correo']);
        expect(options).not.toContain('Comprobante de pago');
        expect(options).not.toContain('Cambio de estado');
        expect(options).not.toContain('Inscripción');
      });
    } finally {
      await cleanup();
    }
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
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
      expect(document.body.querySelector('[data-testid="course-registration-follow-up-list-pane"]')).toBeNull();
    });

    await openDossierContextAction('Agregar seguimiento');

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
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
      expect(hasExactText(document.body, 'Seguimiento')).toBe(false);
      expect(hasExactText(document.body, 'Historial de seguimiento')).toBe(false);
      expect(hasExactText(document.body, 'Primer seguimiento')).toBe(false);
    });

    await openDossierContextAction('Agregar seguimiento');

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
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
    });

    await openDossierContextAction('Agregar seguimiento');

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

  it('uses one hide action when the empty follow-up link field is the only optional detail', async () => {
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
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
    });

    await openDossierContextAction('Agregar seguimiento');

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Agregar detalles opcionales')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar detalles opcionales'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir adjunto')).toBeTruthy();
      expect(getButtonByText(document.body, 'Ocultar detalles opcionales')).toBeTruthy();
      expect(hasLabel(document.body, 'URL del adjunto')).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Usar enlace existente en lugar de subir adjunto'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'URL del adjunto')).toBe(true);
      expect(document.body.querySelector('[data-testid="mock-follow-up-upload"]')).toBeNull();
      expect(getButtonByText(document.body, 'Ocultar detalles opcionales')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Ocultar enlace existente')).toBe(0);
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
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
    });

    await openDossierContextAction('Agregar seguimiento');

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
      expect(document.body.querySelector('[data-testid="mock-follow-up-upload"]')).toBeNull();
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
      expect(document.body.querySelector('[data-testid="mock-follow-up-upload"]')).not.toBeNull();
      expect(countButtonsByText(document.body, 'Ocultar enlace existente')).toBe(0);
    });

    await cleanup();
  });

  it('removes the alternate follow-up link action once an attachment is ready', async () => {
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
      expect(getButtonByText(document.body, optionalDossierContextActionsLabel)).toBeTruthy();
    });

    await openDossierContextAction('Agregar seguimiento');

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Agregar detalles opcionales')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar detalles opcionales'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir adjunto')).toBeTruthy();
      expect(hasLabel(document.body, 'URL del adjunto')).toBe(false);
      expect(document.body.querySelector('[data-testid="mock-follow-up-upload"]')).not.toBeNull();
    });

    const uploadButton = document.body.querySelector<HTMLButtonElement>('[data-testid="mock-follow-up-upload"]');
    if (!uploadButton) throw new Error('Mock follow-up upload not found');

    await act(async () => {
      clickButton(uploadButton);
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countButtonsByText(document.body, 'Usar enlace existente en lugar de subir adjunto')).toBe(0);
      expect(hasLabel(document.body, 'URL del adjunto')).toBe(false);
      expect(hasLabel(document.body, 'Asunto')).toBe(true);
      expect(countButtonsByText(document.body, 'Ocultar detalles opcionales')).toBe(0);
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
      const editFollowUpItem = getMenuItemByText(document.body, 'Editar seguimiento');
      const deleteFollowUpItem = getMenuItemByText(document.body, 'Eliminar seguimiento');

      expect(editFollowUpItem).toBeTruthy();
      expect(deleteFollowUpItem).toBeTruthy();
      expect(editFollowUpItem.getAttribute('aria-label')).toBe('Editar seguimiento Confirmó transferencia');
      expect(editFollowUpItem.getAttribute('title')).toBe('Editar seguimiento Confirmó transferencia');
      expect(deleteFollowUpItem.getAttribute('aria-label')).toBe('Eliminar seguimiento Confirmó transferencia');
      expect(deleteFollowUpItem.getAttribute('title')).toBe('Eliminar seguimiento Confirmó transferencia');
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
      expect(hasExactText(document.body, 'En edición')).toBe(true);
      expect(() =>
        getButtonByAriaLabel(document.body, 'Abrir acciones para seguimiento Confirmó transferencia'),
      ).toThrow();
      expect(
        Array.from(document.body.querySelectorAll('[role="menuitem"]')).some(
          (el) => (el.textContent ?? '').trim() === 'Editar seguimiento',
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('keeps saved follow-up menus hidden while a new follow-up form owns the actions', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdFollowUps: [
          buildFollowUp(),
          buildFollowUp({
            crfId: 402,
            crfSubject: 'Pidió factura',
            crfNotes: 'Quiere factura a nombre de empresa.',
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
        document.body.querySelectorAll('button[aria-label^="Abrir acciones para seguimiento "]'),
      ).toHaveLength(2);
      expect(getButtonByText(document.body, 'Agregar seguimiento')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar seguimiento'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Cancelar seguimiento')).toBeTruthy();
      expect(hasLabel(document.body, 'Nota de seguimiento')).toBe(true);
      expect(
        document.body.querySelectorAll('button[aria-label^="Abrir acciones para seguimiento "]'),
      ).toHaveLength(0);
      expect(document.body.querySelector('button[aria-label^="Editar seguimiento "]')).toBeNull();
      expect(document.body.querySelector('button[aria-label^="Eliminar seguimiento "]')).toBeNull();
      expect(document.body.textContent).toContain('Dijo que enviará el comprobante hoy.');
      expect(document.body.textContent).toContain('Quiere factura a nombre de empresa.');
      expect(hasExactText(document.body, 'En edición')).toBe(false);
    });

    await cleanup();
  });

  it('falls back to follow-up type and date when a saved follow-up subject is blank', async () => {
    const createdAt = '2030-02-05T10:30:00.000Z';
    const fallbackActionLabel = `Abrir acciones para seguimiento Llamada del ${formatTimestampForDisplay(createdAt, '-')}`;

    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdFollowUps: [
          buildFollowUp({
            crfSubject: '   ',
            crfCreatedAt: createdAt,
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
      expect(getButtonByAriaLabel(document.body, fallbackActionLabel)).toBeTruthy();
      expect(
        document.body.querySelector('button[aria-label="Abrir acciones para seguimiento    "]'),
      ).toBeNull();
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(document.body, fallbackActionLabel));
      await flushPromises();
      await flushPromises();
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, 'Editar seguimiento'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect((getInputByLabel(document.body, 'Asunto') as HTMLInputElement).value).toBe('');
    });

    await cleanup();
  });

  it('uses a generic follow-up label instead of placeholder chrome when saved metadata is blank', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdFollowUps: [
          buildFollowUp({
            crfEntryType: '   ',
            crfSubject: '   ',
            crfCreatedAt: '   ',
            crfUpdatedAt: '   ',
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
      expect(hasExactText(document.body, 'Seguimiento')).toBe(true);
      expect(getButtonByAriaLabel(document.body, 'Abrir acciones para seguimiento Seguimiento')).toBeTruthy();
      expect(document.body.querySelector('button[aria-label="Abrir acciones para seguimiento  del -"]')).toBeNull();
      expect(document.body.textContent).not.toContain(' del -');
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
      expect(getButtonByText(document.body, 'Agregar comprobante')).toBeTruthy();
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
      const editReceiptItem = getMenuItemByText(document.body, 'Editar comprobante');
      const deleteReceiptItem = getMenuItemByText(document.body, 'Eliminar comprobante');

      expect(editReceiptItem).toBeTruthy();
      expect(deleteReceiptItem).toBeTruthy();
      expect(editReceiptItem.getAttribute('aria-label')).toBe('Editar comprobante receipt.pdf');
      expect(editReceiptItem.getAttribute('title')).toBe('Editar comprobante receipt.pdf');
      expect(deleteReceiptItem.getAttribute('aria-label')).toBe('Eliminar comprobante receipt.pdf');
      expect(deleteReceiptItem.getAttribute('title')).toBe('Eliminar comprobante receipt.pdf');
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, 'Editar comprobante'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Actualizar comprobante')).toBeTruthy();
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(true);
      expect(hasExactText(document.body, 'En edición')).toBe(true);
      expect(() =>
        getButtonByAriaLabel(document.body, 'Abrir acciones para comprobante receipt.pdf'),
      ).toThrow();
      expect(
        Array.from(document.body.querySelectorAll('[role="menuitem"]')).some(
          (el) => (el.textContent ?? '').trim() === 'Editar comprobante',
        ),
      ).toBe(false);
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

  it('summarizes shared saved-activity dates once instead of repeating them on every dossier item', async () => {
    const sharedReceiptCreatedAt = '2030-03-01T12:00:00.000Z';
    const sharedFollowUpCreatedAt = '2030-03-02T12:00:00.000Z';
    const sharedReceiptCreatedLabel = formatTimestampForDisplay(sharedReceiptCreatedAt, '-');
    const sharedFollowUpCreatedLabel = formatTimestampForDisplay(sharedFollowUpCreatedAt, '-');

    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [
          buildReceipt({ crrCreatedAt: sharedReceiptCreatedAt }),
          buildReceipt({
            crrId: 302,
            crrCreatedAt: sharedReceiptCreatedAt,
            crrFileName: 'receipt-2.pdf',
          }),
        ],
        crdFollowUps: [
          buildFollowUp({ crfCreatedAt: sharedFollowUpCreatedAt }),
          buildFollowUp({
            crfId: 402,
            crfCreatedAt: sharedFollowUpCreatedAt,
            crfSubject: 'Pidió confirmación final',
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
      expect(document.body.textContent).toContain(`Todos subidos: ${sharedReceiptCreatedLabel}`);
      expect(countOccurrences(document.body, `Subido: ${sharedReceiptCreatedLabel}`)).toBe(0);
      expect(countOccurrences(document.body, sharedReceiptCreatedLabel)).toBe(1);
      expect(document.body.textContent).toContain(`Resumen: Llamada · registrados ${sharedFollowUpCreatedLabel}`);
      expect(document.body.textContent).not.toContain('Todos registrados:');
      expect(document.body.textContent).not.toContain('Tipo de seguimiento:');
      expect(countOccurrences(document.body, sharedFollowUpCreatedLabel)).toBe(1);
      expect(countOccurrences(document.body, 'Llamada')).toBe(1);
      expect(document.body.textContent).toContain('receipt.pdf');
      expect(document.body.textContent).toContain('receipt-2.pdf');
      expect(document.body.textContent).toContain('Confirmó transferencia');
      expect(document.body.textContent).toContain('Pidió confirmación final');
    });

    await cleanup();
  });

  it('summarizes repeated follow-up due dates once instead of repeating reminder chips', async () => {
    const sharedNextFollowUpAt = '2030-03-05T12:00:00.000Z';
    const sharedNextFollowUpLabel = formatTimestampForDisplay(sharedNextFollowUpAt, '-');
    const sharedFollowUpCreatedLabel = formatTimestampForDisplay('2030-01-04T03:04:05.000Z', '-');

    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdFollowUps: [
          buildFollowUp({ crfNextFollowUpAt: sharedNextFollowUpAt }),
          buildFollowUp({
            crfId: 402,
            crfSubject: 'Confirmar cupo',
            crfNextFollowUpAt: sharedNextFollowUpAt,
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
      expect(document.body.textContent).toContain(
        `Resumen: Llamada · próximo ${sharedNextFollowUpLabel} · registrados ${sharedFollowUpCreatedLabel}`,
      );
      expect(document.body.textContent).not.toContain('Próximo seguimiento:');
      expect(document.body.textContent).not.toContain('Tipo de seguimiento:');
      expect(document.body.textContent).not.toContain('Todos registrados:');
      expect(countOccurrences(document.body, sharedNextFollowUpLabel)).toBe(1);
      expect(countOccurrences(document.body, 'Próximo:')).toBe(0);
      expect(countOccurrences(document.body, 'Llamada')).toBe(1);
      expect(document.body.textContent).toContain('Confirmó transferencia');
      expect(document.body.textContent).toContain('Confirmar cupo');
    });

    await cleanup();
  });

  it('summarizes repeated receipt notes once instead of repeating them on every saved proof', async () => {
    const sharedReceiptNote = 'Transferencia confirmada por coordinación.';

    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [
          buildReceipt({
            crrNotes: sharedReceiptNote,
            crrCreatedAt: '2030-03-01T12:00:00.000Z',
          }),
          buildReceipt({
            crrId: 302,
            crrFileName: 'receipt-2.pdf',
            crrNotes: sharedReceiptNote,
            crrCreatedAt: '2030-03-02T12:00:00.000Z',
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
      const sharedNote = document.body.querySelector<HTMLElement>(
        '[data-testid="course-registration-shared-receipt-notes"]',
      );

      expect(sharedNote?.textContent).toBe(`Nota de comprobantes: ${sharedReceiptNote}`);
      expect(countOccurrences(document.body, sharedReceiptNote)).toBe(1);
      expect(document.body.textContent).toContain('receipt.pdf');
      expect(document.body.textContent).toContain('receipt-2.pdf');
      expect(getButtonByAriaLabel(document.body, 'Abrir acciones para comprobante receipt.pdf')).toBeTruthy();
      expect(getButtonByAriaLabel(document.body, 'Abrir acciones para comprobante receipt-2.pdf')).toBeTruthy();
    });

    await cleanup();
  });

  it('combines shared receipt date and note into one proof summary', async () => {
    const sharedReceiptCreatedAt = '2030-03-01T12:00:00.000Z';
    const sharedReceiptCreatedLabel = formatTimestampForDisplay(sharedReceiptCreatedAt, '-');
    const sharedReceiptNote = 'Transferencia confirmada por coordinación.';

    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [
          buildReceipt({
            crrCreatedAt: sharedReceiptCreatedAt,
            crrNotes: sharedReceiptNote,
          }),
          buildReceipt({
            crrId: 302,
            crrFileName: 'receipt-2.pdf',
            crrCreatedAt: sharedReceiptCreatedAt,
            crrNotes: sharedReceiptNote,
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
      const summary = document.body.querySelector<HTMLElement>(
        '[data-testid="course-registration-shared-receipt-summary"]',
      );

      expect(summary?.textContent).toBe(
        `Resumen: subidos ${sharedReceiptCreatedLabel} · nota: ${sharedReceiptNote}`,
      );
      expect(document.body.textContent).not.toContain('Todos subidos:');
      expect(document.body.textContent).not.toContain('Nota de comprobantes:');
      expect(countOccurrences(document.body, sharedReceiptCreatedLabel)).toBe(1);
      expect(countOccurrences(document.body, sharedReceiptNote)).toBe(1);
      expect(document.body.textContent).toContain('receipt.pdf');
      expect(document.body.textContent).toContain('receipt-2.pdf');
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

    await act(async () => {
      clickButton(getButtonByAriaLabel(document.body, 'Abrir acciones para comprobante receipt.pdf · #302'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const editReceiptItem = getMenuItemByText(document.body, 'Editar comprobante');
      const deleteReceiptItem = getMenuItemByText(document.body, 'Eliminar comprobante');

      expect(editReceiptItem.getAttribute('aria-label')).toBe('Editar comprobante receipt.pdf · #302');
      expect(deleteReceiptItem.getAttribute('aria-label')).toBe('Eliminar comprobante receipt.pdf · #302');
      expect(editReceiptItem.textContent?.trim()).toBe('Editar comprobante');
      expect(deleteReceiptItem.textContent?.trim()).toBe('Eliminar comprobante');
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

    await act(async () => {
      clickButton(getButtonByAriaLabel(document.body, 'Abrir acciones para seguimiento Confirmó transferencia · #402'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const editFollowUpItem = getMenuItemByText(document.body, 'Editar seguimiento');
      const deleteFollowUpItem = getMenuItemByText(document.body, 'Eliminar seguimiento');

      expect(editFollowUpItem.getAttribute('aria-label')).toBe('Editar seguimiento Confirmó transferencia · #402');
      expect(deleteFollowUpItem.getAttribute('aria-label')).toBe('Eliminar seguimiento Confirmó transferencia · #402');
      expect(editFollowUpItem.textContent?.trim()).toBe('Editar seguimiento');
      expect(deleteFollowUpItem.textContent?.trim()).toBe('Eliminar seguimiento');
    });

    await cleanup();
  });

  it('uses the follow-up label for unnamed attachments instead of repeating a generic link', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdFollowUps: [
          buildFollowUp({
            crfAttachmentUrl: 'https://example.com/follow-up-401.png',
            crfAttachmentName: '   ',
          }),
          buildFollowUp({
            crfId: 402,
            crfSubject: 'Confirmó transferencia',
            crfNotes: 'Pidió que confirmemos recepción.',
            crfAttachmentUrl: 'https://example.com/follow-up-402.png',
            crfAttachmentName: null,
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
      expect(document.body.textContent).toContain('Adjunto de Confirmó transferencia · #401');
      expect(document.body.textContent).toContain('Adjunto de Confirmó transferencia · #402');
      expect(document.body.textContent).not.toContain('Abrir adjunto');
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

  it('merges duplicate follow-up records before falling back to generic action labels', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdFollowUps: [
          buildFollowUp({
            crfSubject: '   ',
            crfNotes: '   ',
            crfAttachmentUrl: null,
            crfAttachmentName: null,
          }),
          buildFollowUp({
            crfSubject: 'Confirmó transferencia',
            crfNotes: 'Pidió confirmación final.',
            crfAttachmentUrl: 'https://example.com/evidencia.png',
            crfAttachmentName: 'evidencia.png',
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
      expect(document.body.textContent).not.toContain('2 entradas');
      expect(document.body.textContent).toContain('Confirmó transferencia');
      expect(document.body.textContent).toContain('Pidió confirmación final.');
      expect(document.body.textContent).toContain('evidencia.png');
      expect(
        document.body.querySelectorAll('button[aria-label="Abrir acciones para seguimiento Confirmó transferencia"]'),
      ).toHaveLength(1);
      expect(
        document.body.querySelector('button[aria-label^="Abrir acciones para seguimiento Llamada del"]'),
      ).toBeNull();
    });

    await cleanup();
  });

  it('keeps image receipt previews uncropped so admins can inspect proof before opening the file', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [
          buildReceipt({
            crrFileUrl: 'https://example.com/transferencia.png',
            crrFileName: 'transferencia.png',
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
      const imagePreview = document.body.querySelector<HTMLImageElement>('img[alt="transferencia.png"]');
      expect(imagePreview).not.toBeNull();
      expect(imagePreview?.getAttribute('src')).toBe('https://example.com/transferencia.png');
      expect(imagePreview?.style.objectFit).toBe('contain');
      expect(getButtonByAriaLabel(document.body, 'Abrir acciones para comprobante transferencia.png')).toBeTruthy();
    });

    await cleanup();
  });

  it('merges duplicate receipt records before falling back to generic receipt labels', async () => {
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({
        crdRegistration: buildRegistration(),
        crdReceipts: [
          buildReceipt({
            crrFileName: null,
            crrNotes: null,
          }),
          buildReceipt({
            crrFileName: 'transferencia-produbanco.pdf',
            crrNotes: 'Transferencia confirmada por coordinación.',
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
      expect(document.body.textContent).not.toContain('Comprobante #301');
      expect(document.body.textContent).toContain('transferencia-produbanco.pdf');
      expect(document.body.textContent).toContain('Transferencia confirmada por coordinación.');
      expect(
        document.body.querySelectorAll(
          'button[aria-label="Abrir acciones para comprobante transferencia-produbanco.pdf"]',
        ),
      ).toHaveLength(1);
      expect(
        document.body.querySelector('button[aria-label="Abrir acciones para comprobante Comprobante #301"]'),
      ).toBeNull();
      expect(document.body.textContent).not.toContain('2 guardados');
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
      expect(hasLabel(container, loadLimitLabel)).toBe(true);
      expect(hasLabel(container, 'Límite')).toBe(false);
      expect(container.textContent).toContain(loadLimitHelperText);
      expect(container.textContent).not.toContain('Máximo de filas a cargar');
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, loadLimitLabel), '50');
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

  it('keeps exact-threshold limit-only views free of default CSV export clutter', async () => {
    const exactThresholdRegistrations = buildRegistrations(8);
    listRegistrationsMock.mockImplementation((params) =>
      Promise.resolve(exactThresholdRegistrations.slice(0, params?.limit ?? exactThresholdRegistrations.length)),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?limit=8');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: undefined,
        limit: 8,
      });
      expect(getButtonByText(container, 'Ajustar límite (8)')).toBeTruthy();
      expect(getButtonByText(container, 'Restablecer límite')).toBeTruthy();
      expect(countButtonsByText(container, copyVisibleCsvLabel(8))).toBe(0);
      expect(container.querySelector('button[aria-label="Copiar 8 inscripciones visibles como CSV"]')).toBeNull();
    });

    await cleanup();
  });

  it('hides the limit adjust toggle when a custom limit is not constraining the loaded view', async () => {
    listRegistrationsMock.mockResolvedValue([buildRegistration()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?limit=50');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: undefined,
        limit: 50,
      });
      expect(container.textContent).toContain('Límite actual: hasta 50 inscripciones.');
      expect(countButtonsByText(container, 'Restablecer límite')).toBe(1);
      expect(countButtonsByText(container, 'Ajustar límite (50)')).toBe(0);
      expect(countButtonsByText(container, 'Ocultar límite')).toBe(0);
      expect(hasLabel(container, loadLimitLabel)).toBe(false);
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')?.textContent).toContain(
        'Beatmaking 101 · Pendiente de pago',
      );
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
      expect(container.textContent).not.toContain('Límite actual:');
      expect(countButtonsByText(container, 'Restablecer límite')).toBe(0);
      expect(countButtonsByText(container, 'Ajustar límite')).toBe(0);
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
    });

    await cleanup();
  });

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
        'No hay inscripciones en la vista actual: estado pagado · límite 50.',
      );
      expect(container.textContent).not.toContain('cohorte Beatmaking 101 (beatmaking-101) · estado pagado');
      expect(container.textContent).not.toContain('Revisa los filtros o restablece la vista si esperabas resultados.');
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
        'No hay inscripciones con el límite actual de hasta 50 inscripciones. Usa refrescar si esperabas resultados.',
      );
      expect(container.textContent).not.toContain('límite actual: límite 50');
      expect(countButtonsByText(container, 'Restablecer límite')).toBe(0);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(1);
    });

    await cleanup();
  });

  it('keeps a limit-only first-run without configured courses focused on setup instead of refresh', async () => {
    listCohortsMock.mockResolvedValue([]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?limit=50');

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');

      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: undefined,
        limit: 50,
      });
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(initialEmptyStateConfigMessage);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateConfigActionLabel);
      expect(container.textContent).not.toContain('No hay inscripciones con el límite actual');
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(countButtonsByText(container, 'Restablecer límite')).toBe(0);
      expect(hasLabel(container, loadLimitLabel)).toBe(false);
      expect(container.querySelector('[data-testid="course-registration-results-panel"]')).toBeNull();
    });

    await cleanup();
  });

  it('keeps a limit-only first-run with multiple configured courses focused on one course-management action', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?limit=50');

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');

      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: undefined,
        limit: 50,
      });
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(initialEmptyStateMultiCohortMessage);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateMultiCohortActionLabel);
      expect(container.textContent).not.toContain('No hay inscripciones con el límite actual');
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(countButtonsByText(container, 'Restablecer límite')).toBe(0);
      expect(hasLabel(container, loadLimitLabel)).toBe(false);
      expect(container.querySelector('[data-testid="course-registration-results-panel"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
    });

    await cleanup();
  });

  it('keeps repeated first-run form labels compact without listing every extra course choice', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'beatmaking-early', ccTitle: 'Formulario público - Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
      { ccSlug: 'live-production', ccTitle: 'Producción en vivo' },
      { ccSlug: 'live-production-night', ccTitle: 'Formulario público del curso Producción en vivo' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');

      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Hay 5 formularios públicos listos para recibir la primera inscripción: Beatmaking 101, Mixing Bootcamp y 1 curso más.',
      );
      expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
      expect(emptyState?.textContent).not.toContain('Producción en vivo');
      expect(emptyState?.textContent).not.toContain('y 2 más');
      expect(emptyState?.textContent).not.toContain('y 1 más');
      expect(emptyState?.textContent).not.toContain('Formulario público - Beatmaking 101');
      expect(emptyState?.textContent).not.toContain('Formulario público del curso Producción en vivo');
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateMultiCohortActionLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips pre-sale wrappers from first-run form labels', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Pre-sale registration page - Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Página de preventa de inscripción - Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');

      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Hay 2 formularios públicos listos para recibir la primera inscripción: Beatmaking 101 y Mixing Bootcamp.',
      );
      expect(emptyState?.textContent).not.toContain('Pre-sale registration page');
      expect(emptyState?.textContent).not.toContain('Página de preventa de inscripción');
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateMultiCohortActionLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('keeps limit-only cohort failures focused on the cohort retry instead of duplicate list refresh', async () => {
    listCohortsMock.mockRejectedValueOnce(new Error('Cohort service unavailable'));
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
      const errorState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-cohort-error"]');
      expect(errorState?.textContent).toContain(initialCohortErrorMessage);
      expect(container.textContent).not.toContain('No hay inscripciones con el límite actual');
      expect(container.textContent).not.toContain('Usa refrescar si esperabas resultados.');
      expect(countButtonsByText(container, initialCohortRetryLabel)).toBe(1);
      expect(countButtonsByText(container, 'Reintentar cohortes')).toBe(0);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(countButtonsByText(container, 'Restablecer límite')).toBe(0);
      expect(container.querySelector('[data-testid="course-registration-results-panel"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-header-actions"]')).toBeNull();
      expect(hasLabel(container, loadLimitLabel)).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
    });

    await act(async () => {
      clickButton(getButtonByText(container, initialCohortRetryLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listCohortsMock).toHaveBeenCalledTimes(2);
      expect(listRegistrationsMock).toHaveBeenCalledTimes(1);
      expect(countButtonsByText(container, initialCohortRetryLabel)).toBe(0);
      expect(container.querySelector('[data-testid="course-registration-initial-cohort-error"]')).toBeNull();
    });

    await cleanup();
  });

  it('treats a redundant single-cohort limit as passive in the first-run empty state', async () => {
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?slug=beatmaking-101&limit=50');

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');

      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: 'beatmaking-101',
        status: undefined,
        limit: 50,
      });
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(container.textContent).not.toContain('No hay inscripciones con el límite actual');
      expect(container.textContent).not.toContain('Revisa los filtros o restablece la vista');
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(countButtonsByText(container, 'Restablecer límite')).toBe(0);
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(hasLabel(container, loadLimitLabel)).toBe(false);
    });

    await cleanup();
  });

  it('treats an empty URL-scoped configured cohort as first-run form guidance', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?slug=beatmaking-101');

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');

      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: 'beatmaking-101',
        status: undefined,
        limit: 200,
      });
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain(initialEmptyStateMultiCohortMessage);
      expect(emptyState?.textContent).not.toContain('No hay inscripciones con los filtros actuales');
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(container.querySelector('[data-testid="course-registration-results-panel"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(countButtonsByText(container, 'Mostrar todas las cohortes')).toBe(0);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
    });

    await cleanup();
  });

  it('strips provider-form wrappers from single-cohort first-run guidance', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Meta Lead Form - Beatmaking 101' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?slug=beatmaking-101');

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      const publicFormLink = emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]');

      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Meta Lead Form');
      expect(publicFormLink?.textContent?.trim()).toBe(initialEmptyStateFormActionLabel);
      expect(publicFormLink?.getAttribute('aria-label')).toBe('Abrir formulario público de Beatmaking 101');
      expect(publicFormLink?.getAttribute('title')).toBe('Abrir formulario público de Beatmaking 101 en una pestaña nueva');
      expect(container.querySelector('[data-testid="course-registration-results-panel"]')).toBeNull();
    });

    await cleanup();
  });

  it('keeps a URL-only cohort slug as a readable filter instead of treating it as a configured form', async () => {
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
        'No hay inscripciones con los filtros actuales: cohorte Archived Course.',
      );
      expect(container.textContent).not.toContain('cohorte archived-course');
      expect(container.textContent).not.toContain('Revisa los filtros o restablece la vista si esperabas resultados.');
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
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]')?.getAttribute('aria-label'),
      ).toBe(initialEmptyStateConfigActionAriaLabel);
      expect(emptyState?.querySelector('a[href^="/inscripcion/"]')).toBeNull();
    });

    await cleanup();
  });

  it('keeps tiny limit-only views focused on reset instead of export chrome', async () => {
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
      expect(container.textContent).toContain('Beatmaking 101 · Pendiente de pago');
      expect(container.textContent).toContain('Límite actual: hasta 50 inscripciones.');
      expect(container.textContent).not.toContain('Mostrando 2 inscripciones.');
      expect(container.querySelector('[data-testid="course-registration-inline-reset"]')?.textContent?.trim()).toBe('Restablecer límite');
      expect(countButtonsByText(container, 'Restablecer límite')).toBe(1);
      expect(countButtonsByText(container, copyVisibleCsvLabel(2))).toBe(0);
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(countButtonsByText(container, 'Copiar CSV filtrado')).toBe(0);
      expect(countButtonsByText(container, 'Copiar CSV')).toBe(0);
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
      expect(contextSummary?.textContent).toContain('Límite actual: hasta 50 inscripciones.');
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
      expect(container.textContent).not.toContain('Beatmaking 101 · Pendiente de pago');
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
      const contextSummary = container.querySelector<HTMLElement>('[data-testid="course-registration-single-choice-context"]');

      expect(container.textContent).not.toContain('Total: 2');
      expect(contextSummary?.textContent).toContain('Mostrando 2 inscripciones en esta vista.');
      expect(contextSummary?.textContent).toContain(dossierLinkScopeHint);
      expect(contextSummary?.textContent).not.toContain(paymentWorkflowDossierScopeHint);
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(container.textContent).not.toContain(`Creado: ${formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-')}`);
      expect(countButtonsByText(container, paymentStatusMenuButtonLabel)).toBe(2);
      expect(countButtonsByText(container, openPaymentWorkflowLabel)).toBe(0);
      expect(countButtonsByText(container, copyVisibleCsvLabel(2))).toBe(0);
      expect(
        Array.from(container.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Copiar CSV (2 filas)',
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
      const contextSummary = container.querySelector<HTMLElement>('[data-testid="course-registration-single-choice-context"]');

      expect(contextSummary?.textContent).toContain('Mostrando 2 inscripciones en esta vista.');
      expect(contextSummary?.textContent).toContain(dossierLinkScopeHint);
      expect(contextSummary?.textContent).not.toContain(paymentWorkflowDossierScopeHint);
      expect(container.querySelectorAll('button[aria-label="Abrir expediente de Ada Lovelace"]')).toHaveLength(1);
      expect(container.querySelectorAll(`button[aria-label="${paymentStatusMenuButtonAriaLabel('Ada Lovelace')}"]`)).toHaveLength(1);
      expect(container.querySelectorAll('button[aria-label="Abrir expediente de Grace Hopper"]')).toHaveLength(1);
      expect(countButtonsByText(container, paymentStatusMenuButtonLabel)).toBe(2);
      expect(countButtonsByText(container, openPaymentWorkflowLabel)).toBe(0);
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
      expect(container.textContent).toContain('Fuente: Instagram · Notas internas');
      expect(container.querySelectorAll(`button[aria-label="${paymentStatusMenuButtonAriaLabel('Ada Lovelace')}"]`)).toHaveLength(1);
      expect(countButtonsByText(container, copyVisibleCsvLabel(2))).toBe(0);
    });

    await cleanup();
  });

  it('keeps a custom acquisition source when a duplicate registration also carries the default public-form source', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crSource: 'landing',
      }),
      buildRegistration({
        crId: 101,
        crSource: 'instagram_story',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crSource: 'landing',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(container.textContent).toContain('Fuente: Instagram story');
      expect(container.textContent).not.toContain('Fuente: landing');
      expect(container.querySelectorAll('button[aria-label="Abrir expediente de Ada Lovelace"]')).toHaveLength(1);
      expect(countButtonsByText(container, copyVisibleCsvLabel(2))).toBe(0);
    });

    await cleanup();
  });

  it('merges duplicate registration status before deciding whether status filters are useful', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: '   ',
        crEmail: '   ',
        crPhoneE164: null,
        crStatus: '   ',
      }),
      buildRegistration({
        crId: 101,
        crFullName: 'Ada Lovelace',
        crEmail: 'ada@example.com',
        crStatus: 'pending_payment',
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
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pendiente de pago').textContent?.trim()).toBe('Pendiente de pago (1)');
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pagado').textContent?.trim()).toBe('Pagado (1)');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
      expect(container.querySelector('[data-testid="course-registration-status-filter-unavailable"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-single-custom-status-summary"]')).toBeNull();
      expect(container.textContent).not.toContain(customStatusFilterUnavailableMessage);
      expect(container.textContent).not.toContain('Estado desconocido');
      expect(countOccurrences(container, 'Registro #101')).toBe(0);
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

  it('keeps busy mixed-source rows from repeating the same creation date', async () => {
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, (index) => ({
        crSource: index % 2 === 0 ? 'instagram' : 'referral',
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(container.textContent).toContain('Fuente: Instagram');
      expect(container.textContent).toContain('Fuente: referral');
      expect(countOccurrences(container, 'Fuente:')).toBe(9);
      expect(countOccurrences(container, 'Creado:')).toBe(0);
      expect(container.textContent).not.toContain(
        `Creado: ${formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-')}`,
      );
    });

    await cleanup();
  });

  it('uses local search as the first-run helper for busy mixed views instead of duplicating filter guidance', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'live-production', ccTitle: 'Producción en vivo' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Ada Lovelace',
        crEmail: 'ada@example.com',
        crCourseSlug: 'beatmaking-101',
        crStatus: 'pending_payment',
      }),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crCourseSlug: 'live-production',
        crStatus: 'paid',
      }),
      ...buildRegistrations(7, (index) => ({
        crId: 201 + index,
        crPartyId: 31 + index,
        crCourseSlug: index % 2 === 0 ? 'beatmaking-101' : 'live-production',
        crStatus: index % 3 === 0 ? 'pending_payment' : 'cancelled',
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe('Nombre o contacto');
      expect(container.textContent).toContain('Busca dentro de las 9 inscripciones cargadas.');
      expect(container.textContent).not.toContain('Busca dentro de las 9 inscripciones cargadas sin cambiar filtros.');
      expect(container.textContent).not.toContain('Los filtros se aplican automáticamente al cambiar.');
      expect(hasExactText(container, 'Filtrar por estado')).toBe(false);
      expect(container.querySelector('[role="group"][aria-label="Filtros de estado de inscripciones"]')).not.toBeNull();
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(container.textContent).toContain(dossierScopeHint);
      expect(container.textContent).not.toContain(dossierOnlyScopeHint);
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pendiente de pago')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pagado')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Cancelado')).toBeTruthy();
      expect(container.querySelectorAll('button[aria-label^="Registrar pago o cambiar estado para "]')).toHaveLength(4);
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(0);
      expect(getButtonByAriaLabel(container, 'Registrar pago o cambiar estado para Ada Lovelace').textContent?.trim()).toBe('');
      expect(countOccurrences(container, 'Pendiente de pago')).toBe(1);
      expect(countOccurrences(container, 'Pagado')).toBe(1);
      expect(countOccurrences(container, 'Cancelado')).toBe(1);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    await cleanup();
  });

  it('folds a passive single cohort into busy-list search guidance when status filters still matter', async () => {
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
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.querySelector('[data-testid="course-registration-single-cohort-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Cohorte disponible');
      expect(container.textContent).not.toContain('Cohorte única por ahora.');
      expect(container.textContent).toContain('Beatmaking 101. Busca dentro de las 9 inscripciones cargadas.');
      expect(container.textContent).not.toContain(
        'Beatmaking 101. Busca dentro de las 9 inscripciones cargadas sin cambiar filtros.',
      );
      expect(hasExactText(container, 'Filtrar por estado')).toBe(false);
      expect(container.querySelector('[role="group"][aria-label="Filtros de estado de inscripciones"]')).not.toBeNull();
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pendiente de pago')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pagado')).toBeTruthy();
    });

    await cleanup();
  });

  it('folds a shared source into busy single-cohort search guidance without restoring source chrome', async () => {
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, (index) => ({
        crSource: 'meta_ads',
        crStatus: index === 8 ? 'paid' : 'pending_payment',
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.querySelector('[data-testid="course-registration-single-cohort-summary"]')).toBeNull();
      expect(container.textContent).toContain(
        'Beatmaking 101 · Fuente visible: Meta ads. Busca dentro de las 9 inscripciones cargadas.',
      );
      expect(countOccurrences(container, 'Fuente visible: Meta ads.')).toBe(1);
      expect(container.textContent).not.toContain('Fuente: Meta ads');
      expect(container.textContent).not.toContain('Mostrando una sola fuente: Meta ads.');
      expect(container.querySelector('[role="group"][aria-label="Filtros de estado de inscripciones"]')).not.toBeNull();
    });

    await cleanup();
  });

  it('keeps busy single-choice defaults focused on search and compact row status actions', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(9));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(container.textContent).toContain(
        'Beatmaking 101 · Pendiente de pago. Busca dentro de las 9 inscripciones cargadas.',
      );
      expect(container.textContent).not.toContain(
        'Beatmaking 101 · Pendiente de pago. Busca dentro de las 9 inscripciones cargadas sin cambiar filtros.',
      );
      expect(container.textContent).toContain(paymentWorkflowDossierScopeHint);
      expect(container.textContent).not.toContain('Cambiar estado incluye Registrar pago y acciones.');
      expect(container.textContent).not.toContain('el botón de estado');
      expect(container.textContent).not.toContain(dossierScopeHint);
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(0);
      expect(container.querySelectorAll('button[aria-label^="Registrar pago o cambiar estado para "]')).toHaveLength(9);
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(0);
      expect(getButtonByAriaLabel(container, 'Registrar pago o cambiar estado para Estudiante 1').getAttribute('title')).toBe(
        'Registrar pago o cambiar estado para Estudiante 1; actual: Pendiente de pago',
      );
      expect(countButtonsByText(container, openPaymentWorkflowLabel)).toBe(0);
      expect(countOccurrences(container, 'Pendiente de pago')).toBe(1);
    });

    await cleanup();
  });

  it('uses direct paid recovery actions when the busy list already states paid status', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(9, () => ({ crStatus: 'paid' })));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(container.textContent).toContain(
        'Beatmaking 101 · Pagado. Busca dentro de las 9 inscripciones cargadas.',
      );
      expect(container.textContent).toContain(
        'Usa el nombre para abrir expediente; Marcar pago pendiente devuelve la inscripción a pendiente.',
      );
      expect(countButtonsByText(container, compactPaymentPendingActionLabel)).toBe(9);
      expect(countButtonsByText(container, markPaymentPendingLabel)).toBe(0);
      expect(getButtonByAriaLabel(container, 'Marcar pago pendiente para Estudiante 1')).toBeTruthy();
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(0);
      expect(countOccurrences(container, 'Pagado')).toBe(1);
      expect(container.textContent).not.toContain('Reabrir vuelve a pendiente.');
    });

    await cleanup();
  });

  it('matches busy cancelled-list guidance to the direct reopen row action', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(9, () => ({ crStatus: 'cancelled' })));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(container.textContent).toContain(
        `Beatmaking 101 · Cancelado. Busca dentro de las 9 inscripciones cargadas. ${pendingRecoveryScopeHint}`,
      );
      expect(container.textContent).not.toContain(
        `Beatmaking 101 · Cancelado. Busca dentro de las 9 inscripciones cargadas. ${dossierScopeHint}`,
      );
      expect(countButtonsByText(container, 'Reabrir')).toBe(9);
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(0);
      expect(countOccurrences(container, 'Cancelado')).toBe(1);
    });

    await cleanup();
  });

  it('adds local search for busy loaded lists without re-querying filters or repeating the matching status chip', async () => {
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
        'Nombre o contacto',
      );
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pagado')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Registrar pago o cambiar estado para Estudiante 1').textContent?.trim()).toBe('');
      const paidRecoveryAction = getButtonByAriaLabel(container, 'Marcar pago pendiente para Nina Simone');
      expect(paidRecoveryAction.textContent?.trim()).toBe(compactPaymentPendingActionLabel);
      expect(paidRecoveryAction.getAttribute('aria-haspopup')).toBeNull();
      expect(container.querySelector('button[aria-label="Cambiar estado para Nina Simone"]')).toBeNull();
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
      expect(container.textContent).toContain(dossierScopeHint);
      expect(container.textContent).not.toContain(dossierOnlyScopeHint);
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
      expect(container.textContent).toContain('Usa el nombre para abrir expediente;');
      expect(hasExactText(container, 'Filtrar por estado')).toBe(false);
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pendiente de pago')).toBeTruthy();
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Pagado"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-single-cohort-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Mostrando 9 inscripciones en esta vista.');
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-local-search-utilities"]')).toBeNull();
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
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps a narrowed busy single-choice search focused on the matching row', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(9));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).toContain(
        'Beatmaking 101 · Pendiente de pago. Busca dentro de las 9 inscripciones cargadas.',
      );
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(0);
      expect(container.querySelectorAll('button[aria-label^="Registrar pago o cambiar estado para "]')).toHaveLength(9);
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(0);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'estudiante 9');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Estudiante 9');
      expect(container.textContent).toContain(
        `Mostrando 1 de 9 inscripciones cargadas. ${paymentWorkflowDossierScopeHint}`,
      );
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Estudiante 9').textContent?.trim()).toBe(
        'Pendiente de pago',
      );
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(0);
      expect(countButtonsByText(container, openPaymentWorkflowLabel)).toBe(0);
      expect(countOccurrences(container, 'Pendiente de pago')).toBe(1);
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('uses the direct recovery hint when busy-list search leaves one cancelled registration', async () => {
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, (index) => (
        index === 8
          ? {
            crFullName: 'Nina Simone',
            crEmail: 'nina@example.com',
            crStatus: 'cancelled',
          }
          : {}
      )),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'nina');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain(
        `Mostrando 1 de 9 inscripciones cargadas. ${pendingRecoveryScopeHint}`,
      );
      expect(container.textContent).not.toContain(
        dossierOnlyScopeHint,
      );
      expect(getButtonByAriaLabel(container, 'Reabrir como pendiente para Nina Simone').textContent?.trim()).toBe(
        'Reabrir como pendiente',
      );
      expect(getButtonByAriaLabel(container, 'Reabrir como pendiente para Nina Simone').getAttribute('aria-haspopup')).toBeNull();
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('uses the direct paid-recovery action when busy-list search leaves one paid registration', async () => {
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
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'nina');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain(
        `Mostrando 1 de 9 inscripciones cargadas. ${paidRecoveryScopeHint}`,
      );
      const paidRecoveryAction = getButtonByAriaLabel(container, 'Marcar pago pendiente para Nina Simone');
      expect(paidRecoveryAction.textContent?.trim()).toBe(markPaymentPendingLabel);
      expect(paidRecoveryAction.getAttribute('aria-haspopup')).toBeNull();
      expect(container.querySelector('button[aria-label="Cambiar estado para Nina Simone"]')).toBeNull();
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Marcar pago pendiente para Nina Simone'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Estado actualizado para Nina Simone.');
      expect(document.body.querySelectorAll('[role="menuitem"]')).toHaveLength(0);
    });

    await cleanup();
  });

  it('keeps single missing-contact search guidance in the helper instead of row fallback copy', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Nina Sin Contacto',
        crEmail: null,
        crPhoneE164: null,
      }),
      ...buildRegistrations(8, (index) => ({
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

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'nina');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Nina Sin Contacto');
      expect(container.textContent).toContain(
        'Mostrando 1 de 9 inscripciones cargadas. Contacto pendiente en esta inscripción.',
      );
      expect(countOccurrences(container, 'Contacto pendiente en esta inscripción.')).toBe(1);
      expect(container.textContent).not.toContain('Sin correo ni teléfono');
      expect(container.querySelector('[data-testid="course-registration-local-search-utilities"]')).toBeNull();
      expect(countButtonsByText(container, copyVisibleCsvLabel(1))).toBe(0);
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('folds all-missing-contact busy-list context into the search helper without reopening row placeholders', async () => {
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, () => ({
        crEmail: null,
        crPhoneE164: null,
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.textContent).toContain(
        'Beatmaking 101 · Pendiente de pago. Contacto pendiente en todas las inscripciones visibles. Busca dentro de las 9 inscripciones cargadas.',
      );
      expect(countOccurrences(container, 'Contacto pendiente en todas las inscripciones visibles.')).toBe(1);
      expect(container.textContent).not.toContain('Sin correo ni teléfono');
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-page-intro"]')).toBeNull();
    });

    await cleanup();
  });

  it('lets admins search the shared contact-pending hint without adding row placeholders', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Nina Sin Contacto',
        crEmail: null,
        crPhoneE164: null,
      }),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: 'Camila Sin Contacto',
        crEmail: '   ',
        crPhoneE164: null,
      }),
      ...buildRegistrations(7, (index) => ({
        crId: 201 + index,
        crPartyId: 30 + index,
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
      expect(container.textContent).toContain('2 inscripciones visibles con contacto pendiente.');
      expect(container.textContent).not.toContain('Sin correo ni teléfono');
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'contacto pendiente');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(container.textContent).toContain('Nina Sin Contacto');
      expect(container.textContent).toContain('Camila Sin Contacto');
      expect(container.textContent).not.toContain('Estudiante 1');
      expect(container.textContent).toContain('Mostrando 2 de 9 inscripciones cargadas.');
      expect(container.textContent).toContain('Contacto pendiente en todas las inscripciones visibles.');
      expect(container.textContent).not.toContain('Sin correo ni teléfono');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('names the exact contact field in busy-list local search prompts', async () => {
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, () => ({
        crPhoneE164: null,
      })),
    );

    const emailOnlyContainer = document.createElement('div');
    document.body.appendChild(emailOnlyContainer);
    const emailOnlyPage = await renderPage(emailOnlyContainer);

    await waitForExpectation(() => {
      const searchInput = getInputByLabel(emailOnlyContainer, localSearchLabel);
      expect(searchInput.getAttribute('placeholder')).toBe('Nombre o correo');
      expect(searchInput.getAttribute('placeholder')).not.toBe('Nombre o contacto');
    });

    await emailOnlyPage.cleanup();

    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, () => ({
        crEmail: '   ',
      })),
    );

    const phoneOnlyContainer = document.createElement('div');
    document.body.appendChild(phoneOnlyContainer);
    const phoneOnlyPage = await renderPage(phoneOnlyContainer);

    await waitForExpectation(() => {
      const searchInput = getInputByLabel(phoneOnlyContainer, localSearchLabel);
      expect(searchInput.getAttribute('placeholder')).toBe('Nombre o teléfono');
      expect(searchInput.getAttribute('placeholder')).not.toBe('Nombre o contacto');
    });

    await phoneOnlyPage.cleanup();
  });

  it('keeps duplicate identity contacts out of the busy-list search placeholder', async () => {
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, (index) => {
        if (index === 8) {
          return {
            crFullName: '999000111',
            crEmail: null,
            crPhoneE164: '+593 999 000 111',
          };
        }

        return {
          crFullName: `student${index + 1}@example.com`,
          crEmail: `student${index + 1}@example.com`,
          crPhoneE164: null,
        };
      }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const searchInput = getInputByLabel(container, localSearchLabel);
      expect(searchInput.getAttribute('placeholder')).toBe('Nombre');
      expect(searchInput.getAttribute('placeholder')).not.toContain('correo');
      expect(searchInput.getAttribute('placeholder')).not.toContain('teléfono');
      expect(searchInput.getAttribute('placeholder')).not.toContain('contacto');
      expect(countOccurrences(container, 'student1@example.com')).toBe(1);
      expect(countOccurrences(container, '999000111')).toBe(1);
      expect(container.textContent).not.toContain('+593 999 000 111');
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'student3@example.com');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('student3@example.com');
      expect(container.textContent).not.toContain('student1@example.com');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps busy-list search prompts limited to identity fields that actually exist', async () => {
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, () => ({
        crEmail: '   ',
        crPhoneE164: null,
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const searchInput = getInputByLabel(container, localSearchLabel);
      expect(searchInput.getAttribute('placeholder')).toBe('Nombre');
      expect(searchInput.getAttribute('placeholder')).not.toBe('Nombre o contacto');
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'estudiante 9');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Estudiante 9');
      expect(container.textContent).not.toContain('Estudiante 1');
      expect(container.textContent).toContain('Mostrando 1 de 9 inscripciones cargadas.');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps dense busy-list search prompts compact while naming the record fallback', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'live-production', ccTitle: 'Producción en vivo' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Ada Lovelace',
        crEmail: 'ada@example.com',
        crPhoneE164: '+593999000111',
        crCourseSlug: 'beatmaking-101',
        crSource: 'instagram_story',
        crStatus: 'waiting_list',
        crAdminNotes: 'Pidió horario de fin de semana.',
      }),
      buildRegistration({
        crId: 102,
        crPartyId: null,
        crFullName: '   ',
        crEmail: '   ',
        crPhoneE164: null,
        crCourseSlug: 'live-production',
        crSource: 'referral',
        crStatus: 'manual_review',
      }),
      ...buildRegistrations(7, (index) => ({
        crId: 201 + index,
        crPartyId: 30 + index,
        crCourseSlug: index % 2 === 0 ? 'beatmaking-101' : 'live-production',
        crSource: index % 2 === 0 ? 'landing' : 'referral',
        crStatus: index % 2 === 0 ? 'paid' : 'cancelled',
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const searchInput = getInputByLabel(container, localSearchLabel);
      expect(searchInput.getAttribute('placeholder')).toBe('Nombre, contacto, registro u otros datos');
      expect(searchInput.getAttribute('placeholder')).not.toContain('nota');
      expect(searchInput.getAttribute('placeholder')).not.toContain('estado');
      expect(searchInput.getAttribute('placeholder')).not.toContain('fuente');
      expect(searchInput.getAttribute('placeholder')).not.toContain('curso');
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.textContent).toContain('Registro #102');
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'manual review');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(getButtonByAriaLabel(container, 'Abrir expediente de registro #102')).toBeTruthy();
      expect(container.textContent).toContain('Registro #102');
      expect(container.textContent).not.toContain('Ada Lovelace');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'registro 102');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(getButtonByAriaLabel(container, 'Abrir expediente de registro #102')).toBeTruthy();
      expect(container.textContent).toContain('Registro #102');
      expect(container.textContent).not.toContain('Ada Lovelace');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('collapses extra spaces in busy-list search so pasted names do not hit empty recovery', async () => {
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
      setInputValue(getInputByLabel(container, localSearchLabel), '  estudiante    9  ');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Estudiante 9');
      expect(container.textContent).toContain('Mostrando 1 de 9 inscripciones cargadas.');
      expect(container.textContent).not.toContain('No hay coincidencias');
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('matches formatted phone numbers from pasted digits so admins avoid a false empty-search recovery', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(9, (index) => ({
      crPhoneE164: index === 4 ? '+593 99 900-0111' : `+593 98 000-00${index + 1}`,
    })));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), '999000111');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Estudiante 5');
      expect(container.textContent).toContain('Mostrando 1 de 9 inscripciones cargadas.');
      expect(container.textContent).not.toContain('No hay coincidencias para "999000111"');
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('matches local leading-zero phone searches against E.164 registration phones', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(9, (index) => ({
      crPhoneE164: index === 4 ? '+593 99 900-0111' : `+593 98 000-00${index + 1}`,
    })));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), '0999000111');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Estudiante 5');
      expect(container.textContent).toContain('Mostrando 1 de 9 inscripciones cargadas.');
      expect(container.textContent).not.toContain('No hay coincidencias para "0999000111"');
      expect(container.textContent).not.toContain('Para buscar por teléfono, usa al menos 4 dígitos del número.');
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps short E.164 phone fragments from flooding the visible registration list', async () => {
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
      setInputValue(getInputByLabel(container, localSearchLabel), '999');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        'Para buscar por teléfono, usa al menos 4 dígitos del número.',
      );
      expect(container.textContent).not.toContain('Estudiante 1');
      expect(container.textContent).not.toContain('Mostrando 9 de 9 inscripciones cargadas.');
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(1);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).toBeNull();
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('matches natural feminine status searches against standard registration states', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Ana Torres',
        crEmail: 'ana@example.com',
        crStatus: 'cancelled',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Brenda Mora',
        crEmail: 'brenda@example.com',
        crStatus: 'cancelled',
      }),
      ...buildRegistrations(7, (index) => ({
        crId: 201 + index,
        crPartyId: 30 + index,
        crStatus: index % 2 === 0 ? 'paid' : 'pending_payment',
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'cancelada');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(container.textContent).toContain('Ana Torres');
      expect(container.textContent).toContain('Brenda Mora');
      expect(container.textContent).not.toContain('Estudiante 1');
      expect(container.textContent).toContain('Mostrando 2 de 9 inscripciones cargadas.');
      expect(container.textContent).toContain('Estado visible: Cancelado.');
      expect(container.textContent).not.toContain('No hay coincidencias para "cancelada"');
      expect(countButtonsByText(container, 'Reabrir')).toBe(2);
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('matches natural plural status searches so admins avoid a false empty-search recovery', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Nina Simone',
        crEmail: 'nina1@example.com',
        crStatus: 'paid',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Nina Garcia',
        crEmail: 'nina2@example.com',
        crStatus: 'paid',
      }),
      ...buildRegistrations(7, (index) => ({
        crId: 201 + index,
        crPartyId: 30 + index,
        crStatus: index % 2 === 0 ? 'pending_payment' : 'cancelled',
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'pagadas');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(container.textContent).toContain('Nina Simone');
      expect(container.textContent).toContain('Nina Garcia');
      expect(container.textContent).not.toContain('Estudiante 1');
      expect(container.textContent).toContain('Mostrando 2 de 9 inscripciones cargadas.');
      expect(container.textContent).toContain('Estado visible: Pagado.');
      expect(container.textContent).not.toContain('No hay coincidencias para "pagadas"');
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('matches hyphenated registration names from spaced search so admins avoid a false empty recovery', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(9, (index) => (
      index === 4
        ? {
          crFullName: 'Nina-Simone',
          crEmail: 'nina@example.com',
        }
        : {}
    )));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'nina simone');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Nina-Simone');
      expect(container.textContent).toContain('Mostrando 1 de 9 inscripciones cargadas.');
      expect(container.textContent).not.toContain('No hay coincidencias para "nina simone"');
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('explains short phone searches instead of leaving admins with generic empty-search recovery', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(9, (index) => ({
      crPhoneE164: index === 4 ? '+593 99 900-0111' : `+593 98 000-00${index + 1}`,
    })));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), '999');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        'Para buscar por teléfono, usa al menos 4 dígitos del número.',
      );
      expect(container.textContent).not.toContain('No hay coincidencias para "999"');
      expect(container.textContent).not.toContain('Mostrando 0 de 9 inscripciones cargadas.');
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(1);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).toBeNull();
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('uses generic empty-search recovery for short numeric queries when no loaded registrations have phones', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(9, () => ({
      crPhoneE164: null,
    })));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe('Nombre o correo');
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), '999');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        'No hay coincidencias para "999" en las 9 inscripciones cargadas.',
      );
      expect(container.textContent).not.toContain(
        'Para buscar por teléfono, usa al menos 4 dígitos del número.',
      );
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(1);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).toBeNull();
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps hidden default and empty sources out of busy-list local search', async () => {
    const defaultSources = [
      'landing',
      'web',
      'website',
      'web_form',
      'website-form',
      'course_website',
      'public-website',
      'landing_page',
      'course_landing_page',
      'public_form',
      'public_course_form',
      'public_course_page',
      'public_course_signup_form',
      'public_registration_form',
      'public-course-registration-form',
      'public_enrollment_form',
      'public-course-enrollment-form',
      'formulario-publico',
      'formulario_publico_de_inscripcion',
      'formulario_de_inscripcion_publica',
      'formulario_web',
      'sitio-web',
      'pagina_web_del_curso',
      'formulario_publico_del_curso',
      'pagina-publica-de-curso',
      'pagina_de_inscripcion',
      'pagina_de_registro',
      'registration_form',
      'course_registration_page',
      'registration_portal',
      'course_registration_portal',
      'course_signup_page',
      'enrollment_form',
      'enrollment_portal',
      'course_enrollment_portal',
      'student_enrollment_portal',
      'student_registration_form',
      'application_form',
      'solicitud_de_inscripcion',
      'portal_de_inscripcion',
      'portal_de_registro',
      'ficha_de_inscripcion',
      null,
    ] as const;
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(defaultSources.length, (index) => ({
        crSource: defaultSources[index % defaultSources.length],
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre o contacto',
      );
      expect(container.textContent).not.toContain('Fuente: landing');
      expect(container.textContent).not.toContain('Fuente: web');
      expect(container.textContent).not.toContain('Fuente: Website');
      expect(container.textContent).not.toContain('Fuente: Web form');
      expect(container.textContent).not.toContain('Fuente: Course website');
      expect(container.textContent).not.toContain('Fuente: Public website');
      expect(container.textContent).not.toContain('Fuente: Landing page');
      expect(container.textContent).not.toContain('Fuente: Course landing page');
      expect(container.textContent).not.toContain('Fuente: Public form');
      expect(container.textContent).not.toContain('Fuente: Public course form');
      expect(container.textContent).not.toContain('Fuente: Public course page');
      expect(container.textContent).not.toContain('Fuente: Public course signup form');
      expect(container.textContent).not.toContain('Fuente: Public registration form');
      expect(container.textContent).not.toContain('Fuente: Public course registration form');
      expect(container.textContent).not.toContain('Fuente: Public enrollment form');
      expect(container.textContent).not.toContain('Fuente: Public course enrollment form');
      expect(container.textContent).not.toContain('Fuente: Formulario publico');
      expect(container.textContent).not.toContain('Fuente: Formulario publico de inscripcion');
      expect(container.textContent).not.toContain('Fuente: Formulario de inscripcion publica');
      expect(container.textContent).not.toContain('Fuente: Formulario web');
      expect(container.textContent).not.toContain('Fuente: Sitio web');
      expect(container.textContent).not.toContain('Fuente: Pagina web del curso');
      expect(container.textContent).not.toContain('Fuente: Formulario publico del curso');
      expect(container.textContent).not.toContain('Fuente: Pagina publica de curso');
      expect(container.textContent).not.toContain('Fuente: Pagina de inscripcion');
      expect(container.textContent).not.toContain('Fuente: Pagina de registro');
      expect(container.textContent).not.toContain('Fuente: Registration form');
      expect(container.textContent).not.toContain('Fuente: Course registration page');
      expect(container.textContent).not.toContain('Fuente: Registration portal');
      expect(container.textContent).not.toContain('Fuente: Course registration portal');
      expect(container.textContent).not.toContain('Fuente: Course signup page');
      expect(container.textContent).not.toContain('Fuente: Enrollment form');
      expect(container.textContent).not.toContain('Fuente: Enrollment portal');
      expect(container.textContent).not.toContain('Fuente: Course enrollment portal');
      expect(container.textContent).not.toContain('Fuente: Student enrollment portal');
      expect(container.textContent).not.toContain('Fuente: Student registration form');
      expect(container.textContent).not.toContain('Fuente: Application form');
      expect(container.textContent).not.toContain('Fuente: Solicitud de inscripcion');
      expect(container.textContent).not.toContain('Fuente: Portal de inscripcion');
      expect(container.textContent).not.toContain('Fuente: Portal de registro');
      expect(container.textContent).not.toContain('Fuente: Ficha de inscripcion');
      expect(container.textContent).not.toContain('Fuente: Sin fuente');
      expect(container.textContent).not.toContain('Fuente visible: landing.');
      expect(container.textContent).not.toContain('Fuente visible: web.');
      expect(container.textContent).not.toContain('Fuente visible: Website.');
      expect(container.textContent).not.toContain('Fuente visible: Web form.');
      expect(container.textContent).not.toContain('Fuente visible: Course website.');
      expect(container.textContent).not.toContain('Fuente visible: Public website.');
      expect(container.textContent).not.toContain('Fuente visible: Landing page.');
      expect(container.textContent).not.toContain('Fuente visible: Course landing page.');
      expect(container.textContent).not.toContain('Fuente visible: Public form.');
      expect(container.textContent).not.toContain('Fuente visible: Public course form.');
      expect(container.textContent).not.toContain('Fuente visible: Public course page.');
      expect(container.textContent).not.toContain('Fuente visible: Public course signup form.');
      expect(container.textContent).not.toContain('Fuente visible: Public registration form.');
      expect(container.textContent).not.toContain('Fuente visible: Public course registration form.');
      expect(container.textContent).not.toContain('Fuente visible: Public enrollment form.');
      expect(container.textContent).not.toContain('Fuente visible: Public course enrollment form.');
      expect(container.textContent).not.toContain('Fuente visible: Formulario publico.');
      expect(container.textContent).not.toContain('Fuente visible: Formulario publico de inscripcion.');
      expect(container.textContent).not.toContain('Fuente visible: Formulario de inscripcion publica.');
      expect(container.textContent).not.toContain('Fuente visible: Formulario web.');
      expect(container.textContent).not.toContain('Fuente visible: Sitio web.');
      expect(container.textContent).not.toContain('Fuente visible: Pagina web del curso.');
      expect(container.textContent).not.toContain('Fuente visible: Formulario publico del curso.');
      expect(container.textContent).not.toContain('Fuente visible: Pagina publica de curso.');
      expect(container.textContent).not.toContain('Fuente visible: Pagina de inscripcion.');
      expect(container.textContent).not.toContain('Fuente visible: Pagina de registro.');
      expect(container.textContent).not.toContain('Fuente visible: Registration form.');
      expect(container.textContent).not.toContain('Fuente visible: Course registration page.');
      expect(container.textContent).not.toContain('Fuente visible: Registration portal.');
      expect(container.textContent).not.toContain('Fuente visible: Course registration portal.');
      expect(container.textContent).not.toContain('Fuente visible: Course signup page.');
      expect(container.textContent).not.toContain('Fuente visible: Enrollment form.');
      expect(container.textContent).not.toContain('Fuente visible: Enrollment portal.');
      expect(container.textContent).not.toContain('Fuente visible: Course enrollment portal.');
      expect(container.textContent).not.toContain('Fuente visible: Student enrollment portal.');
      expect(container.textContent).not.toContain('Fuente visible: Student registration form.');
      expect(container.textContent).not.toContain('Fuente visible: Application form.');
      expect(container.textContent).not.toContain('Fuente visible: Solicitud de inscripcion.');
      expect(container.textContent).not.toContain('Fuente visible: Portal de inscripcion.');
      expect(container.textContent).not.toContain('Fuente visible: Portal de registro.');
      expect(container.textContent).not.toContain('Fuente visible: Ficha de inscripcion.');
      expect(getDossierTriggers(container)).toHaveLength(defaultSources.length);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'course signup page');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        `No hay coincidencias para "course signup page" en las ${defaultSources.length} inscripciones cargadas.`,
      );
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('treats plural public-form source containers as default plumbing in busy lists', async () => {
    const defaultSources = [
      'course_registration_pages',
      'registration_portals',
      'course_enrollment_portals',
      'public_course_pages',
      'student_registration_forms',
      'application_pages',
      'intake_pages',
      'admissions_pages',
      'landing_pages',
    ] as const;
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(defaultSources.length, (index) => ({
        crSource: defaultSources[index],
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre o contacto',
      );
      expect(container.textContent).not.toContain('Fuente: Course registration pages');
      expect(container.textContent).not.toContain('Fuente: Registration portals');
      expect(container.textContent).not.toContain('Fuente: Course enrollment portals');
      expect(container.textContent).not.toContain('Fuente: Public course pages');
      expect(container.textContent).not.toContain('Fuente: Student registration forms');
      expect(container.textContent).not.toContain('Fuente: Application pages');
      expect(container.textContent).not.toContain('Fuente: Intake pages');
      expect(container.textContent).not.toContain('Fuente: Admissions pages');
      expect(container.textContent).not.toContain('Fuente: Landing pages');
      expect(getDossierTriggers(container)).toHaveLength(defaultSources.length);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'registration pages');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        `No hay coincidencias para "registration pages" en las ${defaultSources.length} inscripciones cargadas.`,
      );
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('treats direct and organic website source labels as default plumbing in busy lists', async () => {
    const defaultSources = [
      'direct',
      'direct_web',
      'direct-website',
      'organic',
      'organic_web',
      'organic-website',
      'website_organic',
      'trafico_directo',
      'trafico_organico',
    ] as const;
    const hiddenSourceLabels = [
      'direct',
      'Direct web',
      'Direct website',
      'organic',
      'Organic web',
      'Organic website',
      'Website organic',
      'Trafico directo',
      'Trafico organico',
    ];

    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(defaultSources.length, (index) => ({
        crSource: defaultSources[index],
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre o contacto',
      );
      hiddenSourceLabels.forEach((sourceLabel) => {
        expect(container.textContent).not.toContain(`Fuente: ${sourceLabel}`);
        expect(container.textContent).not.toContain(`Fuente visible: ${sourceLabel}.`);
      });
      expect(container.textContent).not.toContain('Fuente visible:');
      expect(getDossierTriggers(container)).toHaveLength(defaultSources.length);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'organic website');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        `No hay coincidencias para "organic website" en las ${defaultSources.length} inscripciones cargadas.`,
      );
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('treats generic lead-capture source containers as default plumbing in busy lists', async () => {
    const defaultSources = [
      'lead_capture_form',
      'lead_capture_pages',
      'lead_generation_page',
      'lead_generation_forms',
      'lead_gen_forms',
      'captacion_de_leads',
      'captura_de_prospectos',
      'formulario_de_captura_de_prospectos',
      'formulario_de_generacion_de_interesados',
    ] as const;
    const hiddenSourceLabels = [
      'Lead capture form',
      'Lead capture pages',
      'Lead generation page',
      'Lead generation forms',
      'Lead gen forms',
      'Captacion de leads',
      'Captura de prospectos',
      'Formulario de captura de prospectos',
      'Formulario de generacion de interesados',
    ];

    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(defaultSources.length, (index) => ({
        crSource: defaultSources[index],
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre o contacto',
      );
      hiddenSourceLabels.forEach((sourceLabel) => {
        expect(container.textContent).not.toContain(`Fuente: ${sourceLabel}`);
        expect(container.textContent).not.toContain(`Fuente visible: ${sourceLabel}.`);
      });
      expect(container.textContent).not.toContain('Fuente visible:');
      expect(getDossierTriggers(container)).toHaveLength(defaultSources.length);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'lead capture');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        `No hay coincidencias para "lead capture" en las ${defaultSources.length} inscripciones cargadas.`,
      );
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('treats placeholder source values as empty source noise in busy lists', async () => {
    const placeholderSources = [
      'unknown',
      'UNKNOWN',
      'n/a',
      'not_set',
      'sin_fuente',
      'none',
      'null',
      'undefined',
      '-',
    ] as const;
    const hiddenSourceLabels = [
      'unknown',
      'UNKNOWN',
      'N a',
      'Not set',
      'Sin fuente',
      'none',
      'null',
      'undefined',
      '-',
    ];
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(placeholderSources.length, (index) => ({
        crSource: placeholderSources[index],
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre o contacto',
      );
      hiddenSourceLabels.forEach((sourceLabel) => {
        expect(container.textContent).not.toContain(`Fuente: ${sourceLabel}`);
        expect(container.textContent).not.toContain(`Fuente visible: ${sourceLabel}.`);
      });
      expect(container.textContent).not.toContain('Fuente visible:');
      expect(getDossierTriggers(container)).toHaveLength(placeholderSources.length);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'unknown');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        `No hay coincidencias para "unknown" en las ${placeholderSources.length} inscripciones cargadas.`,
      );
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('treats generic intake and admissions sources as default form traffic', async () => {
    const defaultSources = [
      'intake_form',
      'intake_page',
      'course_intake',
      'course_intake_page',
      'student_intake_form',
      'student_intake_page',
      'admission_page',
      'admissions_page',
      'course_admission_page',
      'course_admissions_page',
      'application_page',
      'course_application_page',
      'formulario_de_ingreso',
      'pagina_de_admision',
      'pagina_de_admisiones',
      'pagina_de_ingreso',
      'solicitud_de_ingreso',
    ] as const;
    const hiddenSourceLabels = [
      'Intake form',
      'Intake page',
      'Course intake',
      'Course intake page',
      'Student intake form',
      'Student intake page',
      'Admission page',
      'Admissions page',
      'Course admission page',
      'Course admissions page',
      'Application page',
      'Course application page',
      'Formulario de ingreso',
      'Pagina de admision',
      'Pagina de admisiones',
      'Pagina de ingreso',
      'Solicitud de ingreso',
    ];
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(defaultSources.length, (index) => ({
        crSource: defaultSources[index],
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre o contacto',
      );
      hiddenSourceLabels.forEach((sourceLabel) => {
        expect(container.textContent).not.toContain(`Fuente: ${sourceLabel}`);
        expect(container.textContent).not.toContain(`Fuente visible: ${sourceLabel}.`);
      });
      expect(getDossierTriggers(container)).toHaveLength(defaultSources.length);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'intake form');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        `No hay coincidencias para "intake form" en las ${defaultSources.length} inscripciones cargadas.`,
      );
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('treats form-provider sources as default public-form plumbing in busy lists', async () => {
    const defaultSources = [
      'google_forms',
      'ms_forms',
      'typeform',
      'tally_form',
      'jotform',
      'jotform_form',
      'microsoft_forms',
      'airtable_form',
      'hubspot_form',
      'hubspot_forms',
      'go_high_level_form',
      'gohighlevel_form',
      'high_level_form',
      'mailchimp_form',
      'mailchimp_forms',
      'manychat_form',
      'paper_form',
      'paper_forms',
      'paperforms',
      'survey_monkey_form',
      'surveymonkey_form',
      'surveymonkey_forms',
      'squarespace_form',
      'leadpages_landing_page',
      'lead_pages_landing_page',
      'notion_form',
      'fillout_forms',
      'cognito_form',
      'convertkit_form',
      'brevo_form',
      'sendinblue_form',
      'flodesk_form',
      'mailerlite_form',
      'klaviyo_form',
      'activecampaign_form',
      'constant_contact_form',
      'keap_form',
      'infusionsoft_form',
      'meta_instant_form',
      'instagram_instant_form',
      'fb_instant_forms',
      'wufoo',
      'zoho_form',
      'gravity_form',
      'webflow_forms',
      'wix_forms',
      'forms_app',
      'forms.app_form',
      'formspree_form',
      'formsite_form',
      '123_form_builder_form',
      '123formbuilder_form',
    ] as const;
    const hiddenSourceLabels = [
      'Google forms',
      'Ms forms',
      'Typeform',
      'Tally form',
      'Jotform',
      'Jotform form',
      'Microsoft forms',
      'Airtable form',
      'Hubspot form',
      'Hubspot forms',
      'Go high level form',
      'Gohighlevel form',
      'High level form',
      'Mailchimp form',
      'Mailchimp forms',
      'Manychat form',
      'Paper form',
      'Paper forms',
      'paperforms',
      'Survey monkey form',
      'Surveymonkey form',
      'Surveymonkey forms',
      'Squarespace form',
      'Leadpages landing page',
      'Lead pages landing page',
      'Notion form',
      'Fillout forms',
      'Cognito form',
      'Convertkit form',
      'Brevo form',
      'Sendinblue form',
      'Flodesk form',
      'Mailerlite form',
      'Klaviyo form',
      'Activecampaign form',
      'Constant contact form',
      'Keap form',
      'Infusionsoft form',
      'Meta instant form',
      'Instagram instant form',
      'Fb instant forms',
      'wufoo',
      'Zoho form',
      'Gravity form',
      'Webflow forms',
      'Wix forms',
      'Forms app',
      'Forms app form',
      'Formspree form',
      'Formsite form',
      '123 form builder form',
      '123formbuilder form',
    ];
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(defaultSources.length, (index) => ({
        crSource: defaultSources[index],
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre o contacto',
      );
      hiddenSourceLabels.forEach((sourceLabel) => {
        expect(container.textContent).not.toContain(`Fuente: ${sourceLabel}`);
        expect(container.textContent).not.toContain(`Fuente visible: ${sourceLabel}.`);
      });
      expect(getDossierTriggers(container)).toHaveLength(defaultSources.length);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'google forms');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        `No hay coincidencias para "google forms" en las ${defaultSources.length} inscripciones cargadas.`,
      );
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('treats social lead-form sources as default public-form plumbing in busy lists', async () => {
    const defaultSources = [
      'facebook_lead_ad',
      'facebook_lead_ads_form',
      'facebook_lead_form',
      'fb_leads',
      'fb_lead_form',
      'instagram_lead_ad_form',
      'instagram_lead_form',
      'ig_lead_ads',
      'ig_lead_form',
      'meta_lead_form',
      'meta_leads',
      'linkedin_lead_gen_form',
      'tiktok_lead_form',
      'whatsapp_lead_form',
      'leads_de_instagram',
    ] as const;
    const hiddenSourceLabels = [
      'Facebook lead ad',
      'Facebook lead ads form',
      'Facebook lead form',
      'Fb leads',
      'Fb lead form',
      'Instagram lead ad form',
      'Instagram lead form',
      'Ig lead ads',
      'Ig lead form',
      'Meta lead form',
      'Meta leads',
      'LinkedIn lead gen form',
      'TikTok lead form',
      'WhatsApp lead form',
      'Leads de Instagram',
    ];
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(defaultSources.length, (index) => ({
        crSource: defaultSources[index],
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre o contacto',
      );
      hiddenSourceLabels.forEach((sourceLabel) => {
        expect(container.textContent).not.toContain(`Fuente: ${sourceLabel}`);
        expect(container.textContent).not.toContain(`Fuente visible: ${sourceLabel}.`);
      });
      expect(getDossierTriggers(container)).toHaveLength(defaultSources.length);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'facebook lead ad');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        `No hay coincidencias para "facebook lead ad" en las ${defaultSources.length} inscripciones cargadas.`,
      );
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('treats checkout and payment source wrappers as default public-form plumbing in busy lists', async () => {
    const defaultSources = [
      'stripe_checkout',
      'datafast_payment_link',
      'kushki_payment_link',
      'paymentez_checkout',
      'deuna_payment_link',
      'payphone_payment_button',
      'online_payment_form',
      'checkout_portal',
      'formulario_de_pago',
    ] as const;
    const hiddenSourceLabels = [
      'Stripe checkout',
      'Datafast payment link',
      'Kushki payment link',
      'Paymentez checkout',
      'Deuna payment link',
      'Payphone payment button',
      'Online payment form',
      'Checkout portal',
      'Formulario de pago',
    ];
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(defaultSources.length, (index) => ({
        crSource: defaultSources[index],
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre o contacto',
      );
      hiddenSourceLabels.forEach((sourceLabel) => {
        expect(container.textContent).not.toContain(`Fuente: ${sourceLabel}`);
        expect(container.textContent).not.toContain(`Fuente visible: ${sourceLabel}.`);
      });
      expect(getDossierTriggers(container)).toHaveLength(defaultSources.length);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'payment link');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        `No hay coincidencias para "payment link" en las ${defaultSources.length} inscripciones cargadas.`,
      );
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('humanizes technical source slugs in busy-list rows and search', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crSource: 'instagram_story' }),
      ...buildRegistrations(8, (index) => ({
        crId: 102 + index,
        crPartyId: 10 + index,
        crFullName: `Estudiante ${index + 2}`,
        crEmail: `student${index + 2}@example.com`,
        crSource: index % 2 === 0 ? 'landing' : null,
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre, contacto o fuente',
      );
      expect(container.textContent).toContain('Fuente: Instagram story');
      expect(container.textContent).not.toContain('Fuente: instagram_story');
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'instagram story');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).not.toContain('Estudiante 2');
      expect(container.textContent).toContain('Fuente visible: Instagram story.');
      expect(container.textContent).not.toContain('instagram_story');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('normalizes uppercase technical source slugs before showing them to first-time admins', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crSource: 'INSTAGRAM_STORY' }),
      ...buildRegistrations(8, (index) => ({
        crId: 102 + index,
        crPartyId: 10 + index,
        crFullName: `Estudiante ${index + 2}`,
        crEmail: `student${index + 2}@example.com`,
        crSource: index % 2 === 0 ? 'landing' : null,
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre, contacto o fuente',
      );
      expect(container.textContent).toContain('Fuente: Instagram story');
      expect(container.textContent).not.toContain('Fuente: INSTAGRAM STORY');
      expect(container.textContent).not.toContain('Fuente: INSTAGRAM_STORY');
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'instagram story');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Fuente visible: Instagram story.');
      expect(container.textContent).not.toContain('INSTAGRAM_STORY');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps known source acronyms readable in busy-list rows and search', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crSource: 'api_referral' }),
      ...buildRegistrations(8, (index) => ({
        crId: 102 + index,
        crPartyId: 10 + index,
        crFullName: `Estudiante ${index + 2}`,
        crEmail: `student${index + 2}@example.com`,
        crSource: index % 2 === 0 ? 'landing' : null,
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre, contacto o fuente',
      );
      expect(container.textContent).toContain('Fuente: API referral');
      expect(container.textContent).not.toContain('Fuente: Api referral');
      expect(container.textContent).not.toContain('Fuente: api_referral');
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'api_referral');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).not.toContain('Estudiante 2');
      expect(container.textContent).toContain('Fuente visible: API referral.');
      expect(container.textContent).not.toContain('api_referral');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps social source brand casing readable in busy-list rows and search summaries', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crSource: 'tiktok_ad' }),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: 'Brenda Lee',
        crEmail: 'brenda@example.com',
        crSource: 'whatsapp_campaign',
      }),
      buildRegistration({
        crId: 103,
        crPartyId: 11,
        crFullName: 'Claudia Jones',
        crEmail: 'claudia@example.com',
        crSource: 'ig_story',
      }),
      buildRegistration({
        crId: 104,
        crPartyId: 12,
        crFullName: 'David Bowie',
        crEmail: 'david@example.com',
        crSource: 'fb_story',
      }),
      ...buildRegistrations(5, (index) => ({
        crId: 105 + index,
        crPartyId: 13 + index,
        crFullName: `Estudiante ${index + 5}`,
        crEmail: `student${index + 5}@example.com`,
        crSource: index % 2 === 0 ? 'landing' : null,
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre, contacto o fuente',
      );
      expect(container.textContent).toContain('Fuente: TikTok ad');
      expect(container.textContent).toContain('Fuente: WhatsApp campaign');
      expect(container.textContent).toContain('Fuente: Instagram story');
      expect(container.textContent).toContain('Fuente: Facebook story');
      expect(container.textContent).not.toContain('Fuente: Tiktok ad');
      expect(container.textContent).not.toContain('Fuente: Whatsapp campaign');
      expect(container.textContent).not.toContain('Fuente: Ig story');
      expect(container.textContent).not.toContain('Fuente: Fb story');
      expect(container.textContent).not.toContain('Fuente: tiktok_ad');
      expect(container.textContent).not.toContain('Fuente: whatsapp_campaign');
      expect(container.textContent).not.toContain('Fuente: ig_story');
      expect(container.textContent).not.toContain('Fuente: fb_story');
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'whatsapp campaign');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Brenda Lee');
      expect(container.textContent).not.toContain('Ada Lovelace');
      expect(container.textContent).toContain('Fuente visible: WhatsApp campaign.');
      expect(container.textContent).not.toContain('whatsapp_campaign');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps a shared humanized source out of the busy-list search placeholder', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crSource: 'instagram_story' }),
      ...buildRegistrations(8, (index) => ({
        crId: 102 + index,
        crPartyId: 10 + index,
        crFullName: `Estudiante ${index + 2}`,
        crEmail: `student${index + 2}@example.com`,
        crSource: index % 2 === 0 ? 'Instagram story' : 'instagram-story',
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const searchInput = getInputByLabel(container, localSearchLabel);
      expect(searchInput.getAttribute('placeholder')).toBe('Nombre o contacto');
      expect(searchInput.getAttribute('placeholder')).not.toContain('fuente');
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).toContain(
        'Beatmaking 101 · Pendiente de pago. Fuente visible: Instagram story. Busca dentro de las 9 inscripciones cargadas.',
      );
      expect(countOccurrences(container, 'Fuente visible: Instagram story.')).toBe(1);
      expect(container.textContent).toContain('Fuente visible: Instagram story.');
      expect(container.textContent).not.toContain('Fuente: Instagram story');
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'instagram story');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.textContent).toContain('Sin cambios: la búsqueda coincide con las 9 inscripciones cargadas.');
      expect(container.textContent).not.toContain('No hay coincidencias para "instagram story"');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('normalizes dotted and slash-delimited source variants before deciding whether to repeat row sources', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crSource: 'instagram.story' }),
      ...buildRegistrations(8, (index) => ({
        crId: 102 + index,
        crPartyId: 10 + index,
        crFullName: `Estudiante ${index + 2}`,
        crEmail: `student${index + 2}@example.com`,
        crSource: index % 2 === 0 ? 'instagram/story' : 'instagram_story',
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const searchInput = getInputByLabel(container, localSearchLabel);
      expect(searchInput.getAttribute('placeholder')).toBe('Nombre o contacto');
      expect(searchInput.getAttribute('placeholder')).not.toContain('fuente');
      expect(container.textContent).toContain('Fuente visible: Instagram story.');
      expect(container.textContent).not.toContain('Fuente: Instagram story');
      expect(container.textContent).not.toContain('Fuente: instagram.story');
      expect(container.textContent).not.toContain('Fuente: instagram/story');
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'instagram story');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.textContent).toContain('Sin cambios: la búsqueda coincide con las 9 inscripciones cargadas.');
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
      expect(hasLabel(container, 'Curso / cohorte')).toBe(true);
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre o contacto',
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
      expect(container.textContent).toContain('Mostrando una sola cohorte: Producción en vivo.');
      expect(container.textContent).not.toContain('No hay coincidencias para "produccion"');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('summarizes repeated created dates once in locally narrowed filtered lists', async () => {
    const sharedCreatedAt = '2030-02-03T03:04:05.000Z';
    const sharedCreatedAtLabel = formatTimestampForDisplay(sharedCreatedAt, '-');

    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Nina Simone',
        crEmail: 'nina1@example.com',
        crStatus: 'paid',
        crCreatedAt: sharedCreatedAt,
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Nina Garcia',
        crEmail: 'nina2@example.com',
        crStatus: 'paid',
        crCreatedAt: sharedCreatedAt,
      }),
      ...buildRegistrations(7, (index) => ({
        crId: 201 + index,
        crPartyId: 30 + index,
        crFullName: `Estudiante ${index + 1}`,
        crEmail: `student${index + 1}@example.com`,
        crStatus: 'paid',
        crCreatedAt: `2030-02-${String(index + 10).padStart(2, '0')}T03:04:05.000Z`,
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?status=paid');

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
      const activeStatusSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-active-status-summary"]',
      );
      expect(activeStatusSummary?.textContent).toContain('Estado filtrado');
      expect(activeStatusSummary?.textContent).toContain('Pagado');
      expect(container.querySelector(`[aria-label="${clearPaidStatusFilterLabel}"]`)).toBeNull();
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
      expect(container.querySelector('[data-testid="course-registration-shared-created-at-summary"]')?.textContent?.trim()).toBe(
        `Misma fecha de registro: ${sharedCreatedAtLabel}.`,
      );
      expect(countOccurrences(container, `Creado: ${sharedCreatedAtLabel}`)).toBe(0);
    });

    await cleanup();
  });

  it('keeps shared created dates visible in default local search without repeating row date chrome', async () => {
    const sharedCreatedAt = '2030-03-04T03:04:05.000Z';
    const sharedCreatedAtLabel = formatTimestampForDisplay(sharedCreatedAt, '-');

    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Nina Simone',
        crEmail: 'nina1@example.com',
        crCreatedAt: sharedCreatedAt,
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Nina Garcia',
        crEmail: 'nina2@example.com',
        crCreatedAt: sharedCreatedAt,
      }),
      ...buildRegistrations(7, (index) => ({
        crId: 201 + index,
        crPartyId: 30 + index,
        crFullName: `Estudiante ${index + 1}`,
        crEmail: `student${index + 1}@example.com`,
        crCreatedAt: `2030-03-${String(index + 10).padStart(2, '0')}T03:04:05.000Z`,
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
      expect(container.textContent).toContain('Nina Simone');
      expect(container.textContent).toContain('Nina Garcia');
      expect(container.textContent).toContain('Mostrando 2 de 9 inscripciones cargadas.');
      expect(container.querySelector('[data-testid="course-registration-shared-created-at-summary"]')?.textContent?.trim()).toBe(
        `Misma fecha de registro: ${sharedCreatedAtLabel}.`,
      );
      expect(countOccurrences(container, `Creado: ${sharedCreatedAtLabel}`)).toBe(0);
    });

    await cleanup();
  });

  it('uses explicit registration-number copy as the busy-list search fallback when rows lack contact identity', async () => {
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
      expect(searchInput.getAttribute('placeholder')).toBe('Número de registro');
      expect(searchInput.getAttribute('placeholder')).not.toBe('Registro');
      expect(searchInput.getAttribute('placeholder')).not.toBe('Nombre o contacto');
      expect(container.textContent).toContain(
        `Busca dentro de las 9 inscripciones cargadas. ${recordPaymentWorkflowDossierScopeHint}`,
      );
      expect(container.textContent).not.toContain('sin cambiar filtros');
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(0);
      expect(container.querySelectorAll('button[aria-label^="Registrar pago o cambiar estado para registro #"]')).toHaveLength(9);
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para registro #"]')).toHaveLength(0);
      expect(countButtonsByText(container, openPaymentWorkflowLabel)).toBe(0);
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
      expect(hasLabel(container, 'Curso / cohorte')).toBe(true);
      expect(searchInput.getAttribute('placeholder')).toBe('Nombre, contacto o fuente');
      expect(searchInput.getAttribute('placeholder')).not.toContain('curso');
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
        'Estado visible: Pagado. Mostrando una sola cohorte: Mixing Bootcamp. Fuente visible: referral.',
      );
      expect(container.querySelector('[role="group"][aria-label="Filtros de estado de inscripciones"]')).not.toBeNull();
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pendiente de pago')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Cancelado')).toBeTruthy();
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Pagado"]')).toBeNull();
      expect(getButtonByAriaLabel(container, 'Marcar pago pendiente para Nina Simone').textContent?.trim()).toBe(
        compactPaymentPendingActionLabel,
      );
      expect(getButtonByAriaLabel(container, 'Marcar pago pendiente para Nina Garcia').textContent?.trim()).toBe(
        compactPaymentPendingActionLabel,
      );
      expect(getButtonByAriaLabel(container, 'Marcar pago pendiente para Nina Simone').getAttribute('aria-haspopup')).toBeNull();
      expect(container.querySelector('button[aria-label="Cambiar estado para Nina Simone"]')).toBeNull();
      expect(container.querySelector('button[aria-label="Cambiar estado para Nina Garcia"]')).toBeNull();
      expect(countButtonsByText(container, 'Cambiar estado')).toBe(0);
      expect(countButtonsByText(container, 'Pagado')).toBe(0);
      expect(container.textContent).not.toContain('Cohorte: Mixing Bootcamp');
      expect(container.textContent).not.toContain('Fuente: referral');
    });

    await cleanup();
  });

  it('hides the redundant shared-status chip after local search when only the load limit is custom', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Ada Lovelace',
        crEmail: 'ada@example.com',
        crStatus: 'pending_payment',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Nina Simone',
        crEmail: 'nina1@example.com',
        crStatus: 'paid',
      }),
      buildRegistration({
        crId: 103,
        crFullName: 'Nina Garcia',
        crEmail: 'nina2@example.com',
        crStatus: 'paid',
      }),
      ...buildRegistrations(6, (index) => ({
        crId: 200 + index,
        crPartyId: 40 + index,
        crFullName: `Estudiante ${index + 1}`,
        crEmail: `student${index + 1}@example.com`,
        crStatus: index % 2 === 0 ? 'pending_payment' : 'cancelled',
      })),
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
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pagado')).toBeTruthy();
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'nina');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(container.textContent).toContain('Estado visible: Pagado.');
      expect(container.querySelector('[aria-label="Filtrar inscripciones por estado Pagado"]')).toBeNull();
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pendiente de pago')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Cancelado')).toBeTruthy();
    });

    await cleanup();
  });

  it('summarizes shared internal notes once after local search narrows a mixed list', async () => {
    const sharedCreatedAtLabel = formatTimestampForDisplay('2030-01-02T03:04:05.000Z', '-');

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
      expect(hasExactText(
        container,
        `Misma fecha de registro: ${sharedCreatedAtLabel}. Notas internas en todas las inscripciones visibles.`,
      )).toBe(true);
      expect(countOccurrences(container, `Creado: ${sharedCreatedAtLabel}`)).toBe(0);
      expect(countOccurrences(container, 'Notas internas')).toBe(1);
      expect(hasExactText(container, 'Cohorte: Beatmaking 101 (beatmaking-101) · Notas internas')).toBe(false);
      expect(hasExactText(container, 'Cohorte: Mixing Bootcamp (mixing-bootcamp) · Notas internas')).toBe(false);
      expect(container.textContent).toContain('Nina Simone');
      expect(container.textContent).toContain('Nina Garcia');
      expect(listRegistrationsMock).toHaveBeenCalledTimes(1);
    });

    await cleanup();
  });

  it('keeps shared internal notes out of the busy-list search placeholder', async () => {
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, () => ({
        crAdminNotes: 'Todos pidieron factura.',
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const searchInput = getInputByLabel(container, localSearchLabel);
      expect(searchInput.getAttribute('placeholder')).toBe('Nombre o contacto');
      expect(searchInput.getAttribute('placeholder')).not.toContain('nota');
      expect(hasExactText(container, 'Notas internas en todas las inscripciones visibles.')).toBe(true);
      expect(countOccurrences(container, 'Notas internas')).toBe(1);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'todos pidieron factura');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.textContent).toContain('Sin cambios: la búsqueda coincide con las 9 inscripciones cargadas.');
      expect(container.textContent).toContain('Coinciden con nota interna.');
      expect(container.textContent).not.toContain('No hay coincidencias para "todos pidieron factura"');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps internal-note search matches explained by the helper instead of row note chrome', async () => {
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
        'Nombre, contacto o nota',
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
      expect(container.textContent).toContain('Mostrando 1 de 9 inscripciones cargadas. Coincide con nota interna.');
      expect(container.textContent).not.toContain('Notas internas');
      expect(container.textContent).not.toContain('Necesita beca parcial antes de confirmar.');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('lets admins find campaign-attributed registrations without adding row attribution chrome', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Camila Vega',
        crEmail: 'camila@example.com',
        crHowHeard: 'TikTok orgánico',
        crUtmCampaign: 'curso_abril',
        crUtmContent: 'video_largo',
      }),
      ...buildRegistrations(8, (index) => ({
        crId: 200 + index,
        crPartyId: 40 + index,
        crFullName: `Estudiante ${index + 1}`,
        crEmail: `student${index + 1}@example.com`,
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getInputByLabel(container, localSearchLabel).getAttribute('placeholder')).toBe(
        'Nombre, contacto u origen',
      );
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'curso abril');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Camila Vega');
      expect(container.textContent).not.toContain('Estudiante 1');
      expect(container.textContent).toContain('Mostrando 1 de 9 inscripciones cargadas. Coincide con origen o campaña.');
      expect(container.textContent).not.toContain('TikTok orgánico');
      expect(container.textContent).not.toContain('curso_abril');
      expect(container.textContent).not.toContain('video_largo');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('collapses source and campaign search prompts into one origin hint without losing matches', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Camila Vega',
        crEmail: 'camila@example.com',
        crSource: 'instagram_story',
        crHowHeard: 'TikTok orgánico',
        crUtmCampaign: 'curso_abril',
      }),
      ...buildRegistrations(8, (index) => ({
        crId: 200 + index,
        crPartyId: 40 + index,
        crFullName: `Estudiante ${index + 1}`,
        crEmail: `student${index + 1}@example.com`,
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const searchInput = getInputByLabel(container, localSearchLabel);
      expect(searchInput.getAttribute('placeholder')).toBe('Nombre, contacto u origen');
      expect(searchInput.getAttribute('placeholder')).not.toContain('fuente');
      expect(searchInput.getAttribute('placeholder')).not.toContain('origen o fuente');
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'instagram story');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Camila Vega');
      expect(container.textContent).toContain('Fuente visible: Instagram story.');
      expect(container.textContent).not.toContain('Estudiante 1');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'curso abril');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Camila Vega');
      expect(container.textContent).toContain('Mostrando 1 de 9 inscripciones cargadas. Coincide con origen o campaña.');
      expect(container.textContent).not.toContain('Estudiante 1');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('collapses multiple secondary search prompts into one other-data hint without losing matches', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Camila Vega',
        crEmail: 'camila@example.com',
        crSource: 'instagram_story',
        crAdminNotes: 'Pidio beca parcial.',
      }),
      ...buildRegistrations(8, (index) => ({
        crId: 200 + index,
        crPartyId: 40 + index,
        crFullName: `Estudiante ${index + 1}`,
        crEmail: `student${index + 1}@example.com`,
        crSource: 'landing',
        crAdminNotes: null,
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const searchInput = getInputByLabel(container, localSearchLabel);
      expect(searchInput.getAttribute('placeholder')).toBe('Nombre, contacto u otros datos');
      expect(searchInput.getAttribute('placeholder')).not.toContain('nota');
      expect(searchInput.getAttribute('placeholder')).not.toContain('fuente');
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
      expect(container.textContent).toContain('Camila Vega');
      expect(container.textContent).toContain('Mostrando 1 de 9 inscripciones cargadas. Coincide con nota interna.');
      expect(container.textContent).not.toContain('Pidio beca parcial.');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'instagram story');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Camila Vega');
      expect(container.textContent).toContain('Fuente visible: Instagram story.');
      expect(container.textContent).not.toContain('No hay coincidencias para "instagram story"');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps shared campaign context out of the busy-list search placeholder', async () => {
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, () => ({
        crHowHeard: 'TikTok orgánico',
        crUtmCampaign: 'curso_abril',
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const searchInput = getInputByLabel(container, localSearchLabel);
      expect(searchInput.getAttribute('placeholder')).toBe('Nombre o contacto');
      expect(searchInput.getAttribute('placeholder')).not.toContain('origen');
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'curso abril');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.textContent).toContain('Sin cambios: la búsqueda coincide con las 9 inscripciones cargadas.');
      expect(container.textContent).toContain('Coinciden con origen o campaña.');
      expect(container.textContent).not.toContain('No hay coincidencias para "curso abril"');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('uses the single visible identity type after local search narrows a mixed busy list', async () => {
    const mixedBusySearchHint = mixedIdentityPaymentWorkflowDossierScopeHint;
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Ada Lovelace',
        crEmail: 'ada@example.com',
      }),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: '   ',
        crEmail: 'contacto@example.com',
        crPhoneE164: '+593999000222',
      }),
      buildRegistration({
        crId: 103,
        crPartyId: 11,
        crFullName: null,
        crEmail: null,
        crPhoneE164: null,
      }),
      ...buildRegistrations(6, (index) => ({
        crId: 201 + index,
        crPartyId: 31 + index,
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
      expect(container.textContent).toContain(mixedBusySearchHint);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'contacto@example.com');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(getButtonByAriaLabel(container, 'Abrir expediente de contacto@example.com')).toBeTruthy();
      expect(container.textContent).toContain(emailPaymentWorkflowDossierScopeHint);
      expect(container.textContent).not.toContain(mixedBusySearchHint);
      expect(container.textContent).toContain('Mostrando 1 de 9 inscripciones cargadas.');
      expect(countButtonsByText(container, openPaymentWorkflowLabel)).toBe(0);
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
      const pendingStatusFilter = getButtonByAriaLabel(container, 'Filtrar inscripciones por estado Pendiente de pago');
      expect(pendingStatusFilter.getAttribute('title')).toBe(
        'Filtrar inscripciones por estado Pendiente de pago y limpiar la búsqueda actual.',
      );
      expect(container.textContent).not.toContain('limpiar la búsqueda actual');
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

  it('clears stale status feedback when local search changes the visible list', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(9));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Registrar pago o cambiar estado para Estudiante 1'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getMenuItemByText(document.body, 'Cancelar inscripción')).toBeTruthy();
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, 'Cancelar inscripción'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Estado actualizado para Estudiante 1.');
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'estudiante 2');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).toContain('Estudiante 2');
      expect(container.textContent).not.toContain('Estado actualizado para Estudiante 1.');
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('clears stale status feedback when the admin starts another row action', async () => {
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
      expect(getButtonByAriaLabel(container, paymentStatusMenuButtonAriaLabel('Ada Lovelace'))).toBeTruthy();
      expect(getButtonByAriaLabel(container, paymentStatusMenuButtonAriaLabel('Grace Hopper'))).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, paymentStatusMenuButtonAriaLabel('Ada Lovelace')));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getMenuItemByText(document.body, 'Cancelar inscripción')).toBeTruthy();
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, 'Cancelar inscripción'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Estado actualizado para Ada Lovelace.');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, paymentStatusMenuButtonAriaLabel('Grace Hopper')));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(container.textContent).not.toContain('Estado actualizado para Ada Lovelace.');
      expect(getMenuItemByText(document.body, openPaymentWorkflowLabel)).toBeTruthy();
      expect(getMenuItemByText(document.body, 'Cancelar inscripción')).toBeTruthy();
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
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(countButtonsByText(container, copyVisibleCsvLabel(9))).toBe(0);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), '  sin   coincidencias  ');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        'Beatmaking 101 · Pendiente de pago. No hay coincidencias para "sin coincidencias" en las 9 inscripciones cargadas.',
      );
      expect(container.textContent).toContain('No hay coincidencias para "sin coincidencias" en las 9 inscripciones cargadas.');
      expect(container.textContent).not.toContain('sin   coincidencias');
      expect(container.textContent).not.toContain('Búsqueda local en el lote cargado (9 inscripciones).');
      expect(container.textContent).not.toContain('Mostrando 0 de 9 inscripciones cargadas.');
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Vista única por ahora');
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
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps long empty-search queries compact while preserving the full input', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(9));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
    });

    const searchInput = getInputByLabel(container, localSearchLabel);
    const longQuery = 'permisos administrativos pendientes para revisar inscripciones sin coincidencias exactas externas';

    await act(async () => {
      setInputValue(searchInput, longQuery);
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const emptySearch = container.querySelector<HTMLElement>('[data-testid="course-registration-empty-local-search"]');

      expect(emptySearch).not.toBeNull();
      expect(searchInput.value).toBe(longQuery);
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        'No hay coincidencias para "permisos administrativos pendientes para revisar inscripciones..." en las 9 inscripciones cargadas.',
      );
      expect(container.textContent).not.toContain(`No hay coincidencias para "${longQuery}"`);
      expect(emptySearch?.getAttribute('title')).toBeNull();
      expect(emptySearch?.getAttribute('aria-label')).toBe(
        `Beatmaking 101 · Pendiente de pago. No hay coincidencias para "${longQuery}" en las 9 inscripciones cargadas.`,
      );
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(1);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).toBeNull();
      expect(listRegistrationsMock).toHaveBeenCalledTimes(1);
    });

    await cleanup();
  });

  it('keeps empty filtered searches owned by the search clear action instead of repeating filter reset recovery', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue(
      buildRegistrations(9, () => ({
        crCourseSlug: 'beatmaking-101',
        crStatus: 'paid',
      })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?slug=beatmaking-101&status=paid');

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(getButtonByText(container, 'Restablecer vista')).toBeTruthy();
      expect(container.textContent).toContain('Vista filtrada: cohorte Beatmaking 101.');
      expect(container.textContent).toContain('Estado filtrado');
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'sin coincidencias');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      expect(container.textContent).toContain(
        'No hay coincidencias para "sin coincidencias" en las 9 inscripciones cargadas.',
      );
      expect(container.textContent).toContain('Vista filtrada: cohorte Beatmaking 101.');
      expect(container.textContent).toContain('Estado filtrado');
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(1);
      expect(countButtonsByText(container, 'Restablecer vista')).toBe(0);
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
      expect(getButtonByText(container, 'Restablecer vista')).toBeTruthy();
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('treats whitespace-only local search as empty so admins do not see a false no-results recovery', async () => {
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
      setInputValue(getInputByLabel(container, localSearchLabel), '   ');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect((getInputByLabel(container, localSearchLabel) as HTMLInputElement).value).toBe('');
      expect(container.textContent).toContain(
        'Beatmaking 101 · Pendiente de pago. Busca dentro de las 9 inscripciones cargadas.',
      );
      expect(container.textContent).toContain(
        `Beatmaking 101 · Pendiente de pago. Busca dentro de las 9 inscripciones cargadas. ${paymentWorkflowDossierScopeHint}`,
      );
      expect(container.textContent).not.toContain(
        'Busca dentro de las 9 inscripciones cargadas sin cambiar filtros.',
      );
      expect(container.textContent).not.toContain('No hay coincidencias para');
      expect(container.textContent).not.toContain('Mostrando 0 de 9 inscripciones cargadas.');
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-local-search-utilities"]')).toBeNull();
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps narrowed-search export utilities separate from the field-owned clear action', async () => {
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
      const searchUtilities = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-local-search-utilities"]',
      );

      expect(searchUtilities).not.toBeNull();
      const copyButton = getButtonByText(searchUtilities!, copyVisibleSearchCsvLabel);
      expect(copyButton).toBeTruthy();
      expect(copyButton.getAttribute('aria-label')).toBe('Copiar 2 inscripciones visibles como CSV');
      expect(copyButton.getAttribute('title')).toBe('Copia solo los resultados visibles de la búsqueda.');
      expect(countButtonsByText(searchUtilities!, staleCopyVisibleSearchCsvLabel)).toBe(0);
      expect(countButtonsByText(searchUtilities!, copyVisibleCsvLabel(2))).toBe(0);
      expect(countButtonsByText(searchUtilities!, 'Limpiar búsqueda')).toBe(0);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(container.textContent).toContain('Mostrando 2 de 9 inscripciones cargadas.');
    });

    await cleanup();
  });

  it('keeps broad local searches field-scoped without reopening export chrome', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(9));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(countButtonsByText(container, copyVisibleCsvLabel(9))).toBe(0);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'Estudiante');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const searchUtilities = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-local-search-utilities"]',
      );

      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.textContent).toContain('Sin cambios: la búsqueda coincide con las 9 inscripciones cargadas.');
      expect(container.textContent).not.toContain('Mostrando 9 de 9 inscripciones cargadas.');
      expect(container.textContent).not.toContain('Busca dentro de este lote sin cambiar los filtros de cohorte o estado.');
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista actual');
      expect(countButtonsByText(container, copyVisibleCsvLabel(9))).toBe(0);
      expect(searchUtilities).toBeNull();
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(listRegistrationsMock).not.toHaveBeenCalled();
    });

    await cleanup();
  });

  it('keeps broad filtered searches from moving filtered export into search utilities', async () => {
    listRegistrationsMock.mockResolvedValue(buildRegistrations(9));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?status=pending_payment');

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
      const activeStatusSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-active-status-summary"]',
      );
      expect(activeStatusSummary?.textContent).toContain('Estado filtrado');
      expect(activeStatusSummary?.textContent).toContain('Pendiente de pago');
      expect(container.querySelector(`[aria-label="${clearPendingStatusFilterLabel}"]`)).toBeNull();
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'Estudiante');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const filterUtilities = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-filter-utilities"]',
      );

      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.textContent).toContain('Sin cambios: la búsqueda coincide con las 9 inscripciones cargadas.');
      expect(filterUtilities).not.toBeNull();
      expect(getButtonByText(filterUtilities!, copyVisibleCsvLabel(9))).toBeTruthy();
      expect(container.querySelector('[data-testid="course-registration-local-search-utilities"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(countButtonsByText(container, copyVisibleCsvLabel(9))).toBe(1);
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
      expect(listRegistrationsMock).toHaveBeenCalledTimes(1);
    });

    await cleanup();
  });

  it('keeps filtered reset actions hidden while local search already owns the narrowed registration view', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Nina Simone',
        crEmail: 'nina1@example.com',
        crCourseSlug: 'beatmaking-101',
        crStatus: 'paid',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Nina Garcia',
        crEmail: 'nina2@example.com',
        crCourseSlug: 'beatmaking-101',
        crStatus: 'paid',
      }),
      ...buildRegistrations(7, (index) => ({
        crId: 201 + index,
        crPartyId: 20 + index,
        crFullName: `Estudiante ${index + 1}`,
        crEmail: `student${index + 1}@example.com`,
        crCourseSlug: 'beatmaking-101',
        crStatus: 'paid',
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?slug=beatmaking-101&status=paid');

    await waitForExpectation(() => {
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(getButtonByText(container, 'Restablecer vista')).toBeTruthy();
      expect(container.textContent).toContain('Vista filtrada: cohorte Beatmaking 101.');
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'nina');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const filterUtilities = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-filter-utilities"]',
      );
      const searchUtilities = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-local-search-utilities"]',
      );

      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(container.textContent).toContain('Mostrando 2 de 9 inscripciones cargadas.');
      expect(filterUtilities).not.toBeNull();
      expect(filterUtilities?.textContent).toContain('Vista filtrada: cohorte Beatmaking 101.');
      expect(filterUtilities?.textContent).not.toContain('(beatmaking-101)');
      expect(countButtonsByText(container, 'Restablecer vista')).toBe(0);
      expect(searchUtilities).not.toBeNull();
      expect(getButtonByText(searchUtilities!, copyVisibleSearchCsvLabel)).toBeTruthy();
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
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
        crPhoneE164: '+593999000101',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Nina Garcia',
        crEmail: 'nina2@example.com',
        crPhoneE164: '+593999000102',
        crStatus: 'paid',
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
      const copyButton = getButtonByText(container, copyVisibleSearchCsvLabel);
      expect(copyButton).toBeTruthy();
      expect(copyButton.textContent?.trim()).toBe(copyVisibleSearchCsvLabel);
      expect(copyButton.getAttribute('aria-label')).toBe('Copiar 2 inscripciones visibles como CSV');
      expect(copyButton.getAttribute('title')).toBe('Copia solo los resultados visibles de la búsqueda.');
      expect(countButtonsByText(container, staleCopyVisibleSearchCsvLabel)).toBe(0);
      expect(container.textContent).toContain('Mostrando 2 de 9 inscripciones cargadas.');
      expect(container.textContent).not.toContain('Mostrando 9 inscripciones en esta vista.');
      expect(countButtonsByText(container, copyVisibleCsvLabel(2))).toBe(0);
    });

    await act(async () => {
      clickButton(getButtonByText(container, copyVisibleSearchCsvLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(writeTextMock).toHaveBeenCalledTimes(1);
      const csv = writeTextMock.mock.calls[0]?.[0] ?? '';
      expect(csv.split('\n')).toHaveLength(3);
      expect(csv.split('\n')[0]).toBe('"id","slug","nombre","email","telefono","estado","creado"');
      expect(csv).toContain('"Nina Simone"');
      expect(csv).toContain('"Nina Garcia"');
      expect(csv).toContain('"+593999000101"');
      expect(csv).toContain('"+593999000102"');
      expect(csv).toContain('"Pendiente de pago"');
      expect(csv).toContain('"Pagado"');
      expect(csv).not.toContain('"pending_payment"');
      expect(csv).not.toContain('"Estudiante 1"');
      expect(container.textContent).toContain('CSV copiado');
      expect(container.textContent).not.toContain('CSV copiado (2 inscripciones)');
      expect(countButtonsByText(container, copyVisibleSearchCsvLabel)).toBe(0);
    });

    await cleanup();
  });

  it('keeps failed CSV copy feedback as one utility message instead of repeating the copy action', async () => {
    const writeTextMock = jest.fn<(text: string) => Promise<void>>().mockRejectedValue(new Error('clipboard denied'));
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
      expect(getButtonByText(container, copyVisibleSearchCsvLabel)).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, copyVisibleSearchCsvLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const searchUtilities = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-local-search-utilities"]',
      );

      expect(writeTextMock).toHaveBeenCalledTimes(1);
      expect(searchUtilities?.textContent).toContain('No se pudo copiar el CSV');
      expect(countButtonsByText(searchUtilities!, copyVisibleSearchCsvLabel)).toBe(0);
      expect(countButtonsByText(searchUtilities!, copyVisibleCsvLabel(2))).toBe(0);
      expect(container.textContent).not.toContain('CSV copiado');
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
      expect(getButtonByText(container, copyVisibleSearchCsvLabel)).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, copyVisibleSearchCsvLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(writeTextMock).toHaveBeenCalledTimes(1);
      expect(container.textContent).toContain('CSV copiado');
      expect(container.textContent).not.toContain('CSV copiado (2 inscripciones)');
      expect(countButtonsByText(container, copyVisibleSearchCsvLabel)).toBe(0);
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Limpiar búsqueda'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect((getInputByLabel(container, localSearchLabel) as HTMLInputElement).value).toBe('');
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(container.textContent).not.toContain('CSV copiado');
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(countButtonsByText(container, copyVisibleCsvLabel(9))).toBe(0);
      expect(listRegistrationsMock).toHaveBeenCalledTimes(1);
    });

    await cleanup();
  });

  it('clears stale CSV feedback when visible rows update under the same filters', async () => {
    const writeTextMock = jest.fn<(text: string) => Promise<void>>().mockResolvedValue(undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText: writeTextMock },
    });
    const firstNina = buildRegistration({
      crId: 101,
      crFullName: 'Nina Simone',
      crEmail: 'nina1@example.com',
    });
    const secondNina = buildRegistration({
      crId: 102,
      crFullName: 'Nina Garcia',
      crEmail: 'nina2@example.com',
    });

    listRegistrationsMock.mockResolvedValue([
      firstNina,
      secondNina,
      ...buildRegistrations(7, (index) => ({
        crId: 201 + index,
        crPartyId: 20 + index,
        crFullName: `Estudiante ${index + 1}`,
        crEmail: `student${index + 1}@example.com`,
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup, queryClient } = await renderPage(container);

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
      expect(getButtonByText(container, copyVisibleSearchCsvLabel)).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, copyVisibleSearchCsvLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(writeTextMock).toHaveBeenCalledTimes(1);
      expect(container.textContent).toContain('CSV copiado');
      expect(container.textContent).not.toContain('CSV copiado (2 inscripciones)');
      expect(countButtonsByText(container, copyVisibleSearchCsvLabel)).toBe(0);
    });

    await act(async () => {
      queryClient.setQueryData(
        ['admin', 'course-registrations', { slug: '', status: 'all', limit: 200 }],
        [
          firstNina,
          ...buildRegistrations(7, (index) => ({
            crId: 301 + index,
            crPartyId: 40 + index,
            crFullName: `Estudiante actualizado ${index + 1}`,
            crEmail: `updated${index + 1}@example.com`,
          })),
        ],
      );
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(1);
      expect(container.textContent).not.toContain('CSV copiado');
      expect(countButtonsByText(container, copyVisibleSearchCsvLabel)).toBe(0);
      expect(listRegistrationsMock).toHaveBeenCalledTimes(1);
    });

    await cleanup();
  });

  it('keeps capped default-list guidance scoped to search and empty-search states', async () => {
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
        'Busca dentro de las 200 inscripciones cargadas.',
      );
      expect(container.textContent).not.toContain(
        'Busca dentro de las 200 inscripciones cargadas. Usa Ajustar límite si necesitas revisar más registros.',
      );
      expect(container.textContent).not.toContain('Se cargó el límite de 200 inscripciones');
      expect(
        Array.from(container.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === copyVisibleCsvLabel(200),
        ),
      ).toBe(false);
      expect(listUtilities).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-header-actions"]')).toBeNull();
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'sin coincidencias');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(0);
      const emptySearch = container.querySelector<HTMLElement>('[data-testid="course-registration-empty-local-search"]');
      expect(emptySearch).not.toBeNull();
      expect(emptySearch?.textContent).toContain(
        'No hay coincidencias para "sin coincidencias" en las 200 inscripciones cargadas.',
      );
      expect(emptySearch?.textContent).not.toContain(
        'Aumenta el límite si el registro puede estar fuera del lote cargado.',
      );
      expect(countButtonsByText(emptySearch!, emptySearchLimitRecoveryLabel)).toBe(1);
      expect(countButtonsByText(emptySearch!, 'Buscar en más registros')).toBe(0);
      expect(countButtonsByText(emptySearch!, 'Ajustar límite')).toBe(0);
      expect(countButtonsByText(emptySearch!, 'Limpiar búsqueda')).toBe(0);
      expect(countButtonsByText(container, emptySearchLimitRecoveryLabel)).toBe(1);
      expect(countButtonsByText(container, 'Ajustar límite')).toBe(0);
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
      expect(container.querySelector('[data-testid="course-registration-single-cohort-summary"]')).toBeNull();
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.textContent).not.toContain('Filtrar por estado');
      expect(listRegistrationsMock).toHaveBeenCalledTimes(1);
    });

    await act(async () => {
      clickButton(getButtonByText(container, emptySearchLimitRecoveryLabel));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const emptySearch = container.querySelector<HTMLElement>('[data-testid="course-registration-empty-local-search"]');

      expect(hasLabel(container, loadLimitLabel)).toBe(true);
      expect(countButtonsByText(container, emptySearchLimitRecoveryLabel)).toBe(0);
      expect(countButtonsByText(container, 'Ajustar límite')).toBe(0);
      expect(countButtonsByText(container, 'Ocultar límite')).toBe(0);
      expect(countButtonsByText(emptySearch!, 'Limpiar búsqueda')).toBe(1);
      expect(listRegistrationsMock).toHaveBeenCalledTimes(1);
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Limpiar búsqueda'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(200);
      expect(container.textContent).not.toContain('No hay coincidencias para "sin coincidencias"');
      expect(hasLabel(container, loadLimitLabel)).toBe(true);
      expect(listRegistrationsMock).toHaveBeenCalledTimes(1);
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, loadLimitLabel), '50');
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
      expect(countButtonsByText(container, 'Copiar CSV filtrado')).toBe(0);
      expect(countButtonsByText(container, 'Copiar CSV')).toBe(0);
      expect(
        Array.from(container.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Copiar CSV filtrado (50 filas)',
        ),
      ).toBe(false);
    });

    await cleanup();
  }, 20_000);

  it('keeps capped cohort empty-search recovery to one limit action', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    const registrations = buildRegistrations(200);
    listRegistrationsMock.mockImplementation((params) => Promise.resolve(
      registrations.slice(0, params?.limit ?? 200),
    ));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?slug=beatmaking-101');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: 'beatmaking-101',
        status: undefined,
        limit: 200,
      });
      expect(hasLabel(container, localSearchLabel)).toBe(true);
      expect(container.querySelector('[data-testid="course-registration-single-status-summary"]')?.textContent).toContain(
        'Pendiente de pago',
      );
      expect(countButtonsByText(container, 'Ajustar límite')).toBe(1);
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, localSearchLabel), 'sin coincidencias');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const emptySearch = container.querySelector<HTMLElement>('[data-testid="course-registration-empty-local-search"]');
      const statusSummary = container.querySelector<HTMLElement>('[data-testid="course-registration-single-status-summary"]');

      expect(emptySearch).not.toBeNull();
      expect(statusSummary).not.toBeNull();
      expect(emptySearch?.textContent).toContain(
        'No hay coincidencias para "sin coincidencias" en las 200 inscripciones cargadas.',
      );
      expect(countButtonsByText(emptySearch!, emptySearchLimitRecoveryLabel)).toBe(1);
      expect(countButtonsByText(emptySearch!, 'Ajustar límite')).toBe(0);
      expect(countButtonsByText(statusSummary!, 'Ajustar límite')).toBe(0);
      expect(countButtonsByText(container, emptySearchLimitRecoveryLabel)).toBe(1);
      expect(countButtonsByText(container, 'Ajustar límite')).toBe(0);
      expect(countButtonsByText(container, 'Limpiar búsqueda')).toBe(0);
      expect(container.querySelector('button[aria-label="Limpiar búsqueda"]')).not.toBeNull();
    });

    await cleanup();
  });

  it('keeps the initial loading state focused on the first-result setup instead of filters or refresh actions', async () => {
    listRegistrationsMock.mockImplementation(() => new Promise(() => undefined));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const loadingState = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-initial-registration-loading"]',
      );

      expect(loadingState?.textContent).toContain(initialRegistrationLoadingMessage);
      expect(container.querySelector('[data-testid="course-registration-results-panel"]')).toBeNull();
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
      expect(container.textContent).not.toContain('Todavía no hay inscripciones para mostrar en esta vista.');
      expect(container.textContent).not.toContain('Vista actual');
      expect(container.textContent).not.toContain('Cohorte disponible');
      expect(container.textContent).not.toContain('Estado disponible');
      expect(container.querySelector('[data-testid="course-registration-header-actions"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
    });

    await cleanup();
  });

  it('keeps stale registration refresh errors next to the error while preserving the visible list', async () => {
    const registrations = buildRegistrations(9);
    listRegistrationsMock
      .mockResolvedValueOnce(registrations)
      .mockRejectedValueOnce(new Error('Backend unavailable'))
      .mockResolvedValueOnce(registrations);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup, queryClient } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(listRegistrationsMock).toHaveBeenCalledTimes(1);
    });

    await act(async () => {
      await queryClient.invalidateQueries({ queryKey: ['admin', 'course-registrations'] });
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledTimes(2);
      expect(container.textContent).toContain('No se pudieron cargar las inscripciones: Backend unavailable');
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(countButtonsByText(container, 'Reintentar inscripciones')).toBe(1);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(container.querySelector('[data-testid="course-registration-header-actions"]')).toBeNull();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Reintentar inscripciones'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledTimes(3);
      expect(container.textContent).not.toContain('No se pudieron cargar las inscripciones');
      expect(getDossierTriggers(container)).toHaveLength(9);
      expect(countButtonsByText(container, 'Reintentar inscripciones')).toBe(0);
    });

    await cleanup();
  });

  it('keeps filtered registration errors focused on retry and one reset path', async () => {
    listRegistrationsMock.mockRejectedValue(new Error('Backend unavailable'));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(
      container,
      '/inscripciones-curso?slug=beatmaking-101&status=paid&limit=50',
    );

    await waitForExpectation(() => {
      expect(container.textContent).toContain('No se pudieron cargar las inscripciones: Backend unavailable');
      expect(countButtonsByText(container, 'Reintentar inscripciones')).toBe(1);
      expect(countButtonsByText(container, 'Restablecer vista')).toBe(1);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(hasLabel(container, loadLimitLabel)).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(0);
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(container.textContent).not.toContain('Vista filtrada:');
      expect(container.textContent).not.toContain('No hay inscripciones con los filtros actuales');
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
    });

    await cleanup();
  });

  it('keeps cohort retry inside the failed cohort filter when registrations already loaded', async () => {
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
      expect(cohortFallback?.textContent).not.toContain('reintenta cohortes');
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelector('[data-testid="course-registration-header-actions"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-single-status-summary"]')).toBeNull();
      expect(container.textContent).not.toContain('Estado disponible');
      expect(getButtonByText(cohortFallback!, 'Reintentar cohortes')).toBeTruthy();
      expect(getButtonByText(container, 'Reintentar cohortes')).toBeTruthy();
      expect(countButtonsByText(container, 'Reintentar cohortes')).toBe(1);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
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

  it('keeps single-status guidance passive when the cohort filter is unavailable', async () => {
    listCohortsMock.mockRejectedValueOnce(new Error('Cohort service unavailable'));
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({ crStatus: 'paid' }),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'paid',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const cohortFallback = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-cohort-filter-unavailable"]',
      );
      const statusSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-single-status-summary"]',
      );

      expect(cohortFallback?.textContent).toContain(cohortFilterUnavailableMessage);
      expect(statusSummary).not.toBeNull();
      expect(statusSummary?.textContent).toContain('Estado disponible');
      expect(statusSummary?.textContent).toContain('Pagado');
      expect(statusSummary?.textContent).not.toContain('Usa cohorte');
      expect(statusSummary?.textContent).not.toContain('Ajustar límite');
      expect(countButtonsByText(container, 'Reintentar cohortes')).toBe(1);
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Cambiar estado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Cambiar estado');
    });

    await cleanup();
  });

  it('keeps loaded registrations usable while cohort filters are still loading', async () => {
    listCohortsMock.mockImplementation(() => new Promise<CourseCohortOptionDTO[]>(() => undefined));
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crStatus: 'paid',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const cohortLoading = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-cohort-filter-loading"]',
      );

      expect(cohortLoading).not.toBeNull();
      expect(cohortLoading?.textContent).toContain('Cohortes cargando');
      expect(cohortLoading?.textContent).toContain(cohortFilterLoadingMessage);
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.textContent).not.toContain('Cargando cohortes…');
      expect(container.querySelector('[data-testid="course-registration-initial-cohort-loading"]')).toBeNull();
      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Grace Hopper')).toBeTruthy();
    });

    await cleanup();
  });

  it('folds empty cohort setup guidance into one inline note when registrations are loaded', async () => {
    listCohortsMock.mockResolvedValue([]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crCourseSlug: 'archived-course',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const cohortSummaryBlock = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-empty-cohort-filter"]',
      );
      const cohortSummary = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-empty-cohort-filter-inline"]',
      );

      expect(cohortSummaryBlock).toBeNull();
      expect(cohortSummary).not.toBeNull();
      expect(cohortSummary?.textContent).toContain(emptyCohortFilterMessage);
      expect(cohortSummary?.textContent).not.toContain('Cohortes no configuradas');
      expect(countOccurrences(container, emptyCohortFilterMessage)).toBe(1);
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelector('[data-testid="course-registration-cohort-filter-loading"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-initial-empty-state"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-single-status-summary"]')).not.toBeNull();
      expect(container.querySelector('[data-testid="course-registration-single-status-summary"]')?.textContent).toContain(
        'Pendiente de pago',
      );
      expect(getDossierTriggers(container)).toHaveLength(2);
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace')).toBeTruthy();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Grace Hopper')).toBeTruthy();
    });

    await cleanup();
  });

  it('does not point first-time admins to a cohort filter when no course cohorts are configured', async () => {
    listCohortsMock.mockResolvedValue([]);
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crPartyId: 10,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
        crCourseSlug: 'archived-course',
        crStatus: 'paid',
      }),
      buildRegistration({
        crId: 103,
        crPartyId: 11,
        crFullName: 'Katherine Johnson',
        crEmail: 'katherine@example.com',
        crCourseSlug: 'legacy-course',
        crStatus: 'cancelled',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(countOccurrences(container, emptyCohortFilterMessage)).toBe(1);
      expect(container.querySelector('[role="group"][aria-label="Filtros de estado de inscripciones"]')).not.toBeNull();
      expect(container.textContent).toContain(
        'Cambia Estado para actualizar la lista. Ajustar límite aparecerá cuando se llene el lote.',
      );
      expect(container.textContent).not.toContain('Los filtros se aplican automáticamente al cambiar.');
      expect(container.textContent).not.toContain('Empieza por cohorte y estado');
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
      expect(emptyState?.textContent).not.toContain('Beatmaking 101 (beatmaking-101)');
      expect(emptyState?.textContent).not.toContain('Comparte el formulario público');
      expect(emptyState?.textContent).not.toContain('pago, seguimiento y correos');
      expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
      expect(emptyState?.textContent).not.toContain('Abre el formulario público y comparte el enlace');
      expect(emptyState?.textContent).not.toContain('Abre la página pública');
      expect(countOccurrences(emptyState!, initialEmptyStateFormActionLabel)).toBe(1);
      const formAction = emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]');
      expect(
        formAction?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(formAction?.getAttribute('aria-label')).toBe('Abrir formulario público de Beatmaking 101');
      expect(formAction?.getAttribute('aria-describedby')).toBe(initialEmptyStateNewTabDescriptionId);
      expect(formAction?.getAttribute('title')).toBe('Abrir formulario público de Beatmaking 101 en una pestaña nueva');
      expect(formAction?.getAttribute('target')).toBe('_blank');
      expect(formAction?.getAttribute('rel')).toBe('noreferrer');
      expect(
        emptyState?.querySelector<HTMLElement>(`#${initialEmptyStateNewTabDescriptionId}`)?.textContent?.trim(),
      ).toBe(initialEmptyStateNewTabDescription);
      expect(
        formAction?.querySelector('[data-testid="course-registration-initial-empty-state-new-tab-icon"]'),
      ).not.toBeNull();
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
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
          (el) => (el.textContent ?? '').trim().startsWith('Copiar CSV'),
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('turns slug-only first-run cohort labels into readable course names', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'beatmaking-101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        singleCohortInitialEmptyStateMessage,
      );
      expect(emptyState?.textContent).not.toContain(
        'Todavía no hay inscripciones para beatmaking-101.',
      );
      expect(emptyState?.textContent).not.toContain('beatmaking-101 (beatmaking-101)');
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
      ).toBe('Abrir formulario público de Beatmaking 101');
    });

    await cleanup();
  });

  it('strips cohort wrappers from first-run course labels without adding extra setup actions', async () => {
    const titles = [
      'Cohorte: Beatmaking 101',
      'Cohort - Beatmaking 101',
      'Grupo: Beatmaking 101',
      'Group - Beatmaking 101',
      'Batch - Beatmaking 101',
      'Ciclo - Beatmaking 101',
      'Edición - Beatmaking 101',
      'Beatmaking 101 - cohort',
      'Beatmaking 101 - grupo',
      'Beatmaking 101 - batch',
      'Beatmaking 101 - edición',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(`Todavía no hay inscripciones para ${title}.`);
        expect(emptyState?.textContent).not.toMatch(
          /Todavía no hay inscripciones para (Cohorte|Cohort|Grupo|Group|Batch|Ciclo|Edici[oó]n)/i,
        );
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips generic course noun wrappers from first-run cohort copy', async () => {
    const titles = [
      'Curso: Beatmaking 101',
      'Course - Beatmaking 101',
      'Clase de Beatmaking 101',
      'Program for Beatmaking 101',
      'Beatmaking 101 - curso',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(
          /Todavía no hay inscripciones para (Curso|Course|Clase|Program)/i,
        );
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips developer task prefixes from first-run cohort labels', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'TODO: Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'FIXME - Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      const configAction = emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]');

      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Hay 2 formularios públicos listos para recibir la primera inscripción: Beatmaking 101 y Mixing Bootcamp.',
      );
      expect(emptyState?.textContent).not.toMatch(/TODO|FIXME/);
      expect(configAction?.textContent?.trim()).toBe(initialEmptyStateMultiCohortActionLabel);
      expect(configAction?.getAttribute('title')).toBe(
        'Elegir entre 2 formularios públicos: Beatmaking 101 y Mixing Bootcamp.',
      );
      expect(configAction?.getAttribute('aria-label')).toBe(initialEmptyStateMultiCohortActionAriaLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips scholarship and financial-aid wrappers from first-run cohort copy', async () => {
    const titles = [
      'Scholarship application - Beatmaking 101',
      'Grant request page for Beatmaking 101',
      'Beatmaking 101 - scholarship form',
      'Bursary portal - Beatmaking 101',
      'Formulario de beca para Beatmaking 101',
      'Solicitud de ayuda financiera - Beatmaking 101',
      'Beatmaking 101 - portal de apoyo económico',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/Scholarship|Grant request|Bursary|beca|ayuda financiera|apoyo econ[oó]mico/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('falls back to a readable cohort label when first-run titles are only generic form descriptors', async () => {
    const titles = ['Formulario público', 'Public form', 'Google Forms'];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(
          singleCohortInitialEmptyStateMessage,
        );
        expect(emptyState?.textContent).not.toContain(
          'Todavía no hay inscripciones para beatmaking-101.',
        );
        expect(emptyState?.textContent).not.toContain(`Todavía no hay inscripciones para ${title}.`);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('keeps option-style cohort labels from repeating the slug in the first-run empty state', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101 (beatmaking-101)' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Beatmaking 101 (beatmaking-101)');
      expect(countOccurrences(emptyState!, 'beatmaking-101')).toBe(0);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('keeps suffix-style cohort titles from repeating the slug in the first-run empty state', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101 - beatmaking-101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Beatmaking 101 - beatmaking-101');
      expect(countOccurrences(emptyState!, 'beatmaking-101')).toBe(0);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
    });

    await cleanup();
  });

  it('keeps public-form descriptor titles from duplicating the first-run form action', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Formulario público - Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Formulario público - Beatmaking 101');
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips public-form descriptor prefixes without punctuation from first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Formulario público Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Formulario público Beatmaking 101');
      expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips plain form connector prefixes from first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Formulario para Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Formulario para Beatmaking 101');
      expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario');
      expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
      ).toBe('Abrir formulario público de Beatmaking 101');
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips public-form course descriptors from first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Formulario público del curso Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Formulario público del curso Beatmaking 101');
      expect(emptyState?.textContent).not.toContain('para curso Beatmaking 101');
      expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips public-registration descriptors from first-run cohort copy', async () => {
    const titles = [
      'Formulario de inscripción pública - Beatmaking 101',
      'Formulario público de inscripción - Beatmaking 101',
      'Página pública para inscripciones - Beatmaking 101',
      'Beatmaking 101 - página de inscripción pública',
      'Beatmaking 101 - página pública de inscripción',
      'Public registration form - Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Página pública');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - página');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para inscripciones');
        expect(emptyState?.textContent).not.toContain('Public registration form');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips online-registration descriptors from first-run cohort copy', async () => {
    const titles = [
      'Inscripción en línea - Beatmaking 101',
      'Registro online para Beatmaking 101',
      'Beatmaking 101 - online enrollment',
      'Beatmaking 101 - matrícula en línea',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Inscripción');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Registro online');
        expect(emptyState?.textContent).not.toContain('online enrollment');
        expect(emptyState?.textContent?.toLocaleLowerCase('es')).not.toContain('matrícula en línea');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips contact-form descriptors from first-run cohort copy', async () => {
    const titles = [
      'Formulario de contacto para Beatmaking 101',
      'Formulario de consulta - Beatmaking 101',
      'Contact form for Beatmaking 101',
      'Beatmaking 101 - inquiry form',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario de contacto');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario de consulta');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Contact form');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - inquiry');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips info-session descriptors from first-run cohort copy', async () => {
    const titles = [
      'Formulario de sesión informativa - Beatmaking 101',
      'Beatmaking 101 - information session form',
      'Orientation signup for Beatmaking 101',
      'Beatmaking 101 - página de clase abierta',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('sesión informativa');
        expect(emptyState?.textContent).not.toContain('information session');
        expect(emptyState?.textContent).not.toContain('Orientation signup');
        expect(emptyState?.textContent).not.toContain('página de clase abierta');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips webinar, seminar, and meeting action wrappers from first-run cohort copy', async () => {
    const titles = [
      'Zoom registration page - Beatmaking 101',
      'Beatmaking 101 - Zoom webinar registration',
      'Seminar registration form - Beatmaking 101',
      'Beatmaking 101 - seminar sign-up page',
      'Google Meet sign-up link for Beatmaking 101',
      'Beatmaking 101 - webinar waitlist',
      'Live session booking link - Beatmaking 101',
      'Beatmaking 101 - meeting RSVP page',
      'Formulario de registro para webinar - Beatmaking 101',
      'Formulario de inscripción para seminario - Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(
          /Zoom|Google Meet|webinar registration|seminar|seminario|sign-up link|waitlist|booking link|RSVP page/i,
        );
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips course-form descriptors from first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Formulario del curso - Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Formulario del curso - Beatmaking 101');
      expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario del curso');
      expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips English public-form descriptor prefixes from first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Public form for Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Public form for Beatmaking 101');
      expect(countOccurrences(emptyState!, 'Public form')).toBe(0);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips English public-form descriptor suffixes from first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101 public form' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Beatmaking 101 public form');
      expect(countOccurrences(emptyState!, 'public form')).toBe(0);
      expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
      ).toBe('Abrir formulario público de Beatmaking 101');
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips course-website descriptors from first-run cohort copy', async () => {
    const titles = [
      'Course website - Beatmaking 101',
      'Course website for Beatmaking 101',
      'Beatmaking 101 - course website',
      'Course microsite - Beatmaking 101',
      'Microsite for Beatmaking 101',
      'Beatmaking 101 - course microsite',
      'Página web del curso - Beatmaking 101',
      'Sitio web para Beatmaking 101',
      'Micrositio del curso - Beatmaking 101',
      'Beatmaking 101 - sitio web del curso',
      'Beatmaking 101 - micrositio del curso',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/course website|course microsite|microsite|micrositio|p[aá]gina web|sitio web/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips Google Forms descriptors from first-run cohort copy', async () => {
    const titles = [
      'Google Form - Beatmaking 101',
      'Google Forms for Beatmaking 101',
      'Formulario de Google para Beatmaking 101',
      'Formularios de Google para Beatmaking 101',
      'Beatmaking 101 - Google form',
      'Beatmaking 101 - Formularios de Google',
      'Instagram Lead Form - Beatmaking 101',
      'Beatmaking 101 - IG lead form',
      'Meta instant form - Beatmaking 101',
      'Beatmaking 101 - Instagram instant form',
      'FB instant forms for Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Google');
        expect(emptyState?.textContent).not.toContain('Instagram Lead Form');
        expect(emptyState?.textContent).not.toContain('IG lead form');
        expect(emptyState?.textContent).not.toContain('Meta instant form');
        expect(emptyState?.textContent).not.toContain('Instagram instant form');
        expect(emptyState?.textContent).not.toContain('FB instant forms');
        expect(countOccurrences(emptyState!, 'Google')).toBe(0);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips common form-provider descriptors from first-run cohort copy', async () => {
    const titles = [
      'Typeform - Beatmaking 101',
      'Typeform.com - Beatmaking 101',
      'Formulario de Typeform para Beatmaking 101',
      'Formulario de Typeform.com para Beatmaking 101',
      'Tally form for Beatmaking 101',
      'Tally.so - Beatmaking 101',
      'Beatmaking 101 - formulario de Tally',
      'Beatmaking 101 - Tally.so',
      'Jotform - Beatmaking 101',
      'Formulario de Jotform - Beatmaking 101',
      'Microsoft Forms - Beatmaking 101',
      'Beatmaking 101 - Typeform',
      'Beatmaking 101 - MS Form',
      'WhatsApp form - Beatmaking 101',
      'Beatmaking 101 - WhatsApp lead form',
      'Formulario de WhatsApp para Beatmaking 101',
      'ManyChat lead form - Beatmaking 101',
      'Beatmaking 101 - ManyChat form',
      'Formulario de ManyChat para Beatmaking 101',
      'Airtable - Beatmaking 101',
      'Airtable form - Beatmaking 101',
      'HubSpot Forms for Beatmaking 101',
      'Mailchimp signup form - Beatmaking 101',
      'Beatmaking 101 - Mailchimp form',
      'Beatmaking 101 - Paperform',
      'SurveyMonkey form - Beatmaking 101',
      'Beatmaking 101 - SurveyMonkey',
      'Fillout form - Beatmaking 101',
      'Formulario de Fillout para Beatmaking 101',
      'Beatmaking 101 - Cognito Forms',
      'Forms.app registration form - Beatmaking 101',
      'Beatmaking 101 - Forms.app form',
      'Formspree form - Beatmaking 101',
      'Formulario de Formspree para Beatmaking 101',
      'Beatmaking 101 - Formsite form',
      '123FormBuilder - Beatmaking 101',
      'Beatmaking 101 - 123 Forms Builder',
      'Wufoo form for Beatmaking 101',
      'Formstack - Beatmaking 101',
      'Zoho Forms for Beatmaking 101',
      'Beatmaking 101 - Gravity Forms',
      'Webflow form - Beatmaking 101',
      'Beatmaking 101 - Wix form',
      'Squarespace form for Beatmaking 101',
      'Leadpages landing page - Beatmaking 101',
      'Formulario de Leadpages para Beatmaking 101',
      'Beatmaking 101 - Leadpages',
      'Beatmaking 101 - Notion form',
      'ConvertKit form - Beatmaking 101',
      'Brevo registration page for Beatmaking 101',
      'Formulario de Flodesk para Beatmaking 101',
      'Beatmaking 101 - MailerLite signup form',
      'Klaviyo lead form - Beatmaking 101',
      'Beatmaking 101 - Klaviyo signup form',
      'ActiveCampaign form - Beatmaking 101',
      'Constant Contact signup page for Beatmaking 101',
      'Beatmaking 101 - Keap intake form',
      'Infusionsoft registration page - Beatmaking 101',
      'CRM lead form - Beatmaking 101',
      'CRM registration page for Beatmaking 101',
      'Beatmaking 101 - CRM intake form',
      'Formulario CRM de leads - Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Typeform');
        expect(emptyState?.textContent).not.toContain('Tally');
        expect(emptyState?.textContent).not.toContain('Jotform');
        expect(emptyState?.textContent).not.toContain('Microsoft Forms');
        expect(emptyState?.textContent).not.toContain('MS Form');
        expect(emptyState?.textContent).not.toContain('WhatsApp');
        expect(emptyState?.textContent).not.toContain('ManyChat');
        expect(emptyState?.textContent).not.toContain('Airtable');
        expect(emptyState?.textContent).not.toContain('HubSpot');
        expect(emptyState?.textContent).not.toContain('Mailchimp');
        expect(emptyState?.textContent).not.toContain('Paperform');
        expect(emptyState?.textContent).not.toContain('SurveyMonkey');
        expect(emptyState?.textContent).not.toContain('Fillout');
        expect(emptyState?.textContent).not.toContain('Cognito');
        expect(emptyState?.textContent).not.toContain('Forms.app');
        expect(emptyState?.textContent).not.toContain('Formspree');
        expect(emptyState?.textContent).not.toContain('Formsite');
        expect(emptyState?.textContent).not.toMatch(/123\s*Forms?\s*Builder/i);
        expect(emptyState?.textContent).not.toContain('Wufoo');
        expect(emptyState?.textContent).not.toContain('Formstack');
        expect(emptyState?.textContent).not.toContain('Zoho');
        expect(emptyState?.textContent).not.toContain('Gravity Forms');
        expect(emptyState?.textContent).not.toContain('Webflow');
        expect(emptyState?.textContent).not.toContain('Wix');
        expect(emptyState?.textContent).not.toContain('Squarespace');
        expect(emptyState?.textContent).not.toContain('Leadpages');
        expect(emptyState?.textContent).not.toContain('Notion form');
        expect(emptyState?.textContent).not.toMatch(/ConvertKit|Brevo|Flodesk|MailerLite|Klaviyo|ActiveCampaign|Constant Contact|Keap|Infusionsoft/i);
        expect(emptyState?.textContent).not.toContain('CRM');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips direct-message automation wrappers from first-run cohort copy', async () => {
    const titles = [
      'Instagram DM automation - Beatmaking 101',
      'ManyChat DM flow for Beatmaking 101',
      'Beatmaking 101 - WhatsApp message flow',
      'Beatmaking 101 Messenger bot',
      'ManyChat automation - Beatmaking 101',
      'WhatsApp automation flow for Beatmaking 101',
      'Beatmaking 101 - Instagram automation funnel',
      'Automatización de WhatsApp para Beatmaking 101',
      'Instagram DM keyword - Beatmaking 101',
      'Palabra clave de Instagram para Beatmaking 101',
      'Beatmaking 101 - WhatsApp keyword trigger',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/DM automation|DM keyword|ManyChat|WhatsApp (?:message flow|automation|keyword trigger)|Instagram automation|Messenger bot|Automatización de WhatsApp|Palabra clave de Instagram/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips stacked form-provider and registration descriptors from first-run cohort copy', async () => {
    const titles = [
      'Typeform registration page - Beatmaking 101',
      'Jotform application link for Beatmaking 101',
      'Beatmaking 101 - Tally registration portal',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/Typeform|Jotform|Tally/i);
        expect(emptyState?.textContent).not.toMatch(/registration page|application link|registration portal/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips community-channel wrappers from first-run cohort copy', async () => {
    const titles = [
      'WhatsApp group - Beatmaking 101',
      'Grupo de WhatsApp para Beatmaking 101',
      'Discord community for Beatmaking 101',
      'Beatmaking 101 - Discord community',
      'Telegram chat: Beatmaking 101',
      'Beatmaking 101 - comunidad de Telegram',
      'Beatmaking 101 WhatsApp group',
      'Slack community - Beatmaking 101',
      'Beatmaking 101 Facebook community',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/WhatsApp group|Grupo de WhatsApp|Discord community|Telegram chat|comunidad de Telegram|Slack community|Facebook community/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips course-platform checkout wrappers from first-run cohort copy', async () => {
    const titles = [
      'Kajabi enrollment page - Beatmaking 101',
      'Teachable course signup form for Beatmaking 101',
      'Beatmaking 101 - Thinkific registration portal',
      'Moodle enrollment portal - Beatmaking 101',
      'Hotmart checkout for Beatmaking 101',
      'Beatmaking 101 - Podia checkout',
      'LearnWorlds course page - Beatmaking 101',
      'ClickFunnels checkout - Beatmaking 101',
      'GoHighLevel registration funnel for Beatmaking 101',
      'Beatmaking 101 - Kartra enrollment page',
      'Systeme.io course checkout for Beatmaking 101',
      'Mighty Networks enrollment page - Beatmaking 101',
      'Beatmaking 101 - Mighty Networks registration portal',
      'Skool course signup page - Beatmaking 101',
      'Beatmaking 101 - Circle.so enrollment portal',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/Kajabi|Teachable|Thinkific|Moodle|Hotmart|Podia|LearnWorlds|ClickFunnels|GoHighLevel|Kartra|Systeme\.io|Mighty Networks|Skool|Circle\.so/i);
        expect(emptyState?.textContent).not.toMatch(/checkout|enrollment page|registration portal|course signup|registration funnel/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips event-platform wrappers from first-run cohort copy', async () => {
    const titles = [
      'Eventbrite - Beatmaking 101',
      'Eventbrite event - Beatmaking 101',
      'Eventbrite registration page - Beatmaking 101',
      'Event registration page - Beatmaking 101',
      'Beatmaking 101 - Lu.ma signup page',
      'Beatmaking 101 - event signup page',
      'Beatmaking 101 - Lu.ma event page',
      'Meetup event registration page for Beatmaking 101',
      'Formulario de Eventbrite para Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/Eventbrite|Lu\.?ma|Meetup/i);
        expect(emptyState?.textContent).not.toMatch(/event registration|signup page/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips scheduling-provider reservation descriptors from first-run cohort copy', async () => {
    const titles = [
      'Calendly booking page - Beatmaking 101',
      'Acuity scheduling reservation link for Beatmaking 101',
      'Beatmaking 101 - Cal.com booking URL',
      'Formulario de reserva de Calendly - Beatmaking 101',
      'Beatmaking 101 - pagina de reserva en Setmore',
      'YouCanBook.me appointment page - Beatmaking 101',
      'Beatmaking 101 - TidyCal booking link',
      'Google Calendar appointment schedule for Beatmaking 101',
      'Formulario de reserva de YouCanBookMe - Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/Calendly|Acuity|Cal\.?com|Setmore|YouCanBook|TidyCal|Google Calendar/i);
        expect(emptyState?.textContent).not.toMatch(/booking|reservation|appointment|schedule|reserva/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips consultation-call booking wrappers from first-run cohort copy', async () => {
    const titles = [
      'Discovery call booking page - Beatmaking 101',
      'Consultation call registration for Beatmaking 101',
      'Beatmaking 101 - strategy call request',
      'Formulario de llamada de consulta - Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/discovery call|consultation call|strategy call|llamada de consulta/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('keeps consultation-call words when they are part of the course name', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'strategy-call-fundamentals', ccTitle: 'Strategy Call Fundamentals' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Todavía no hay inscripciones para Strategy Call Fundamentals. La página pública ya está lista para recibir la primera.',
      );
      expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Fundamentals');
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/strategy-call-fundamentals"]')?.getAttribute('aria-label'),
      ).toBe('Abrir formulario público de Strategy Call Fundamentals');
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('keeps event-platform words when they are part of the course name', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'meetup-music-production', ccTitle: 'Meetup Music Production' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Todavía no hay inscripciones para Meetup Music Production. La página pública ya está lista para recibir la primera.',
      );
      expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Music Production.');
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/meetup-music-production"]')?.getAttribute('aria-label'),
      ).toBe('Abrir formulario público de Meetup Music Production');
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips provider-prefixed registration descriptors from first-run cohort copy', async () => {
    const titles = [
      'Google application form - Beatmaking 101',
      'Google application link - Beatmaking 101',
      'Instagram registration page - Beatmaking 101',
      'Instagram registration URL - Beatmaking 101',
      'Meta enrollment portal - Beatmaking 101',
      'Beatmaking 101 - WhatsApp registration form',
      'Beatmaking 101 - WhatsApp enrollment link',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/Google application|Instagram registration|Meta enrollment|WhatsApp registration/i);
        expect(emptyState?.textContent).not.toMatch(/registration page|enrollment portal|registration form|application form|application link|registration URL|enrollment link/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips social lead-source descriptors from first-run cohort copy', async () => {
    const titles = [
      'Facebook Lead Ad - Beatmaking 101',
      'Instagram lead ad form for Beatmaking 101',
      'Beatmaking 101 - Meta lead ads',
      'Facebook leads - Beatmaking 101',
      'Leads de Instagram para Beatmaking 101',
      'Beatmaking 101 - Meta leads',
      'LinkedIn Lead Gen Form - Beatmaking 101',
      'TikTok lead form for Beatmaking 101',
      'Beatmaking 101 - LinkedIn leads',
      'Leads de TikTok para Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/lead ads?|leads? de|leads?/i);
        expect(emptyState?.textContent).not.toMatch(/Facebook|Instagram|Meta|LinkedIn|TikTok/);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips ad-campaign wrappers from first-run cohort copy', async () => {
    const titles = [
      'Campaña de inscripción - Beatmaking 101',
      'Facebook ad campaign for Beatmaking 101',
      'Email campaign landing page - Beatmaking 101',
      'Campaña de email para Beatmaking 101',
      'Beatmaking 101 - campaign landing page',
      'Beatmaking 101 - newsletter campaign',
      'Beatmaking 101 - campaña de leads',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/campaña|campaign|ad campaign|newsletter/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips ad-asset wrappers from first-run cohort copy', async () => {
    const titles = [
      'Meta ad set - Beatmaking 101',
      'Facebook ad creative for Beatmaking 101',
      'LinkedIn ad group: Beatmaking 101',
      'Beatmaking 101 - Google ad',
      'Conjunto de anuncios para Beatmaking 101',
      'Beatmaking 101 - anuncio de Instagram',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/ad set|ad creative|ad group|google ad|conjunto de anuncios|anuncio/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('keeps ad-topic course names in first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'meta-ads-masterclass', ccTitle: 'Meta Ads Masterclass' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Todavía no hay inscripciones para Meta Ads Masterclass. La página pública ya está lista para recibir la primera.',
      );
      expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Masterclass.');
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/meta-ads-masterclass"]')?.getAttribute('aria-label'),
      ).toBe('Abrir formulario público de Meta Ads Masterclass');
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips course-registration descriptor prefixes from first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Course registration - Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Course registration - Beatmaking 101');
      expect(countOccurrences(emptyState!, 'Course registration')).toBe(0);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips English enrollment-form descriptors from first-run cohort copy', async () => {
    const titles = [
      'Course enrollment form - Beatmaking 101',
      'Course inscription form - Beatmaking 101',
      'Beatmaking 101 - inscription page',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(countOccurrences(emptyState!, 'Course enrollment')).toBe(0);
        expect(countOccurrences(emptyState!, 'Course inscription')).toBe(0);
        expect(emptyState?.textContent).not.toContain('inscription page');
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
        ).toBe(initialEmptyStateFormActionLabel);
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips registration-portal descriptors from first-run cohort copy', async () => {
    const titles = [
      'Course registration portal - Beatmaking 101',
      'Beatmaking 101 - enrollment portal',
      'Application portal for Beatmaking 101',
      'Portal de inscripción - Beatmaking 101',
      'Beatmaking 101 - portal de inscripción',
      'Portal público de inscripciones del curso - Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/portal/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips registration-link descriptors from first-run cohort copy', async () => {
    const titles = [
      'Registration link - Beatmaking 101',
      'Beatmaking 101 - enrollment link',
      'Registration URL - Beatmaking 101',
      'Beatmaking 101 - enrollment URL',
      'Course sign-up link - Beatmaking 101',
      'Link de inscripción - Beatmaking 101',
      'Beatmaking 101 - link de inscripción',
      'Enlace de inscripción - Beatmaking 101',
      'Beatmaking 101 - enlace de inscripción',
      'Enlace público de inscripción - Beatmaking 101',
      'Beatmaking 101 - URL pública para inscripción',
      'URL de inscripción - Beatmaking 101',
      'Beatmaking 101 - URL de inscripción',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/(?:registration|enrollment|sign[-\s]?up)\s+(?:link|url)/i);
        expect(emptyState?.textContent).not.toMatch(/(?:link|enlace|url)\s+de\s+inscripci[oó]n/i);
        expect(emptyState?.textContent).not.toMatch(/(?:link|enlace|url)\s+p[uú]blic[oa]s?\s+(?:de|para)\s+inscripci[oó]n/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips QR-code registration wrappers from first-run cohort copy', async () => {
    const titles = [
      'QR registration link - Beatmaking 101',
      'QR code registration form for Beatmaking 101',
      'Código QR de inscripción - Beatmaking 101',
      'Beatmaking 101 - QR code enrollment URL',
      'Beatmaking 101 - código QR para inscripción',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/qr|c[oó]digo\s+qr/i);
        expect(emptyState?.textContent).not.toMatch(/(?:registration|enrollment)\s+(?:form|link|url)/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips payment and checkout descriptors from first-run cohort copy', async () => {
    const titles = [
      'Checkout - Beatmaking 101',
      'Stripe checkout form for Beatmaking 101',
      'Datafast payment link - Beatmaking 101',
      'Kushki payment link - Beatmaking 101',
      'Paymentez checkout for Beatmaking 101',
      'Beatmaking 101 - Deuna payment page',
      'Shopify checkout - Beatmaking 101',
      'WooCommerce checkout for Beatmaking 101',
      'Lemon Squeezy payment link - Beatmaking 101',
      'Beatmaking 101 - Gumroad checkout',
      'Beatmaking 101 - payment page',
      'Payment button for Beatmaking 101',
      'Online payment form - Beatmaking 101',
      'Formulario de pago - Beatmaking 101',
      'Formulario de pago en línea - Beatmaking 101',
      'Botón de pago - Beatmaking 101',
      'Beatmaking 101 - página de pago',
      'Beatmaking 101 - página de checkout online',
      'Enlace de pago para Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/checkout|payment|stripe|datafast|kushki|paymentez|deuna|shopify|woocommerce|lemon\s+squeezy|gumroad|pago/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips sales and ticket-page descriptors from first-run cohort copy', async () => {
    const titles = [
      'Sales page - Beatmaking 101',
      'Order form for Beatmaking 101',
      'Beatmaking 101 - purchase page',
      'Página de ventas - Beatmaking 101',
      'Formulario de compra para Beatmaking 101',
      'Beatmaking 101 - enlace de venta',
      'Tickets - Beatmaking 101',
      'Ticket page for Beatmaking 101',
      'Beatmaking 101 - tickets link',
      'Página de tickets - Beatmaking 101',
      'Beatmaking 101 - entradas',
      'Beatmaking 101 - enlace de entradas',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(
          /sales page|order form|purchase page|tickets?|entradas?|p[aá]gina de ventas|formulario de compra|enlace de venta/i,
        );
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('keeps legitimate ticket course titles in first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'tickets-live-music', ccTitle: 'Tickets for Live Music Teams' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Todavía no hay inscripciones para Tickets for Live Music Teams. La página pública ya está lista para recibir la primera.',
      );
      expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Live Music Teams.');
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/tickets-live-music"]')?.getAttribute('aria-label'),
      ).toBe('Abrir formulario público de Tickets for Live Music Teams');
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips signup-form descriptors from first-run cohort copy', async () => {
    const titles = [
      'Signup form - Beatmaking 101',
      'Sign-up form - Beatmaking 101',
      'Course sign up page - Beatmaking 101',
      'Beatmaking 101 - sign-up form',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/sign[-\s]?up/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
        ).toBe(initialEmptyStateFormActionLabel);
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips intake-form descriptors from first-run cohort copy', async () => {
    const titles = [
      'Course intake form - Beatmaking 101',
      'Beatmaking 101 - intake form',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Course intake');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - intake');
        expect(countOccurrences(emptyState!, 'intake form')).toBe(0);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips landing-page wrappers from first-run cohort copy', async () => {
    const titles = [
      'Landing de curso - Beatmaking 101',
      'Página landing del curso - Beatmaking 101',
      'Beatmaking 101 - página landing',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Landing de curso');
        expect(emptyState?.textContent).not.toMatch(/p[aá]gina landing/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
        ).toBe(initialEmptyStateFormActionLabel);
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips registration-form descriptor prefixes from first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Formulario de inscripción - Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Formulario de inscripción - Beatmaking 101');
      expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario de inscripción');
      expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips registration-page descriptor prefixes from first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Página de inscripción - Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Página de inscripción - Beatmaking 101');
      expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Página de inscripción');
      expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips generated form descriptors from slug-only first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'registration-page-beatmaking-101', ccTitle: null },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Registration Page Beatmaking 101');
      expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Registration Page');
      expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
      expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/registration-page-beatmaking-101"]')?.getAttribute('aria-label'),
      ).toBe('Abrir formulario público de Beatmaking 101');
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips course-page descriptor prefixes from first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Página del curso - Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Página del curso - Beatmaking 101');
      expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Página del curso');
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
      ).toBe('Abrir formulario público de Beatmaking 101');
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips registration descriptor synonyms from first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Formulario de registro - Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Formulario de registro - Beatmaking 101');
      expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario de registro');
      expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateFormActionLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips English request and submission descriptors from first-run cohort copy', async () => {
    const titles = [
      'Enrollment request - Beatmaking 101',
      'Beatmaking 101 - registration request',
      'Course submission form - Beatmaking 101',
      'Beatmaking 101 - student submission page',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Enrollment request');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - registration request');
        expect(emptyState?.textContent).not.toContain('Course submission form');
        expect(emptyState?.textContent).not.toContain('student submission page');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips seat-request descriptors from first-run cohort copy', async () => {
    const titles = [
      'Formulario de reserva de cupo - Beatmaking 101',
      'Solicitud de cupo - Beatmaking 101',
      'Página de cupos - Beatmaking 101',
      'Beatmaking 101 - reserva de cupo',
      'Beatmaking 101 - solicitud de cupo',
      'Beatmaking 101 - formulario de cupos',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario de reserva');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Solicitud de cupo');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - reserva');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - solicitud');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips standalone course-registration descriptors from first-run cohort copy', async () => {
    const titles = [
      'Registro del curso - Beatmaking 101',
      'Beatmaking 101 - registro del curso',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Registro');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - registro');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips pre-registration descriptors from first-run cohort copy', async () => {
    const titles = [
      'Formulario de preinscripción - Beatmaking 101',
      'Beatmaking 101 - pre-registration form',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario de preinscripción');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - pre-registration');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips sign-up sheet descriptors from first-run cohort copy', async () => {
    const titles = [
      'Sign-up sheet - Beatmaking 101',
      'Google registration sheet - Beatmaking 101',
      'Beatmaking 101 - Microsoft enrollment spreadsheet',
      'Beatmaking 101 - hoja de inscripción',
      'Hoja de cálculo de inscripción - Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Sign-up sheet');
        expect(emptyState?.textContent).not.toContain('Google registration sheet');
        expect(emptyState?.textContent).not.toContain('Microsoft enrollment spreadsheet');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - hoja');
        expect(emptyState?.textContent).not.toContain('Hoja de cálculo');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips admission-form descriptors from first-run cohort copy', async () => {
    const titles = [
      'Formulario de admisión - Beatmaking 101',
      'Portal de admisiones - Beatmaking 101',
      'Portal de ingreso para Beatmaking 101',
      'Admisiones del curso - Beatmaking 101',
      'Solicitud de ingreso - Beatmaking 101',
      'Beatmaking 101 - admission form',
      'Beatmaking 101 - formulario de admisiones',
      'Beatmaking 101 - portal de ingreso',
      'Beatmaking 101 - ingreso al curso',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario de admisión');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Portal de admisiones');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Portal de ingreso');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Admisiones');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Solicitud de ingreso');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - ingreso');
        expect(emptyState?.textContent).not.toContain('portal de ingreso');
        expect(emptyState?.textContent).not.toContain('formulario de admisiones');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - admission');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips admission-packet descriptors from first-run cohort copy', async () => {
    const titles = [
      'Admission packet for Beatmaking 101',
      'Student admissions packet - Beatmaking 101',
      'Beatmaking 101 - admissions packet',
      'Paquete de admisión - Beatmaking 101',
      'Beatmaking 101 - paquete de ingreso',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/admissions?\s+packet|paquete de (?:admisi[oó]n|ingreso)/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips audition-form descriptors from first-run cohort copy', async () => {
    const titles = [
      'Formulario de audición - Beatmaking 101',
      'Página de audiciones para Beatmaking 101',
      'Solicitud de casting - Beatmaking 101',
      'Audition form for Beatmaking 101',
      'Beatmaking 101 - casting registration',
      'Beatmaking 101 - audition page',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario de audición');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Página de audiciones');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Solicitud de casting');
        expect(emptyState?.textContent).not.toMatch(/audition|casting|audici[oó]n/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips placement-assessment descriptors from first-run cohort copy', async () => {
    const titles = [
      'Placement test form - Beatmaking 101',
      'Course assessment page for Beatmaking 101',
      'Beatmaking 101 - diagnostic quiz',
      'Beatmaking 101 - formulario de diagnóstico',
      'Prueba de nivel - Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/placement test|assessment page|diagnostic quiz|formulario de diagn[oó]stico|prueba de nivel/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('keeps legitimate ingreso course titles in first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'ingreso-programacion', ccTitle: 'Ingreso a la programación' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Todavía no hay inscripciones para Ingreso a la programación. La página pública ya está lista para recibir la primera.',
      );
      expect(emptyState?.textContent).not.toContain('ingreso-programacion');
    });

    await cleanup();
  });

  it('strips application-form descriptors from first-run cohort copy', async () => {
    const titles = [
      'Application form for Beatmaking 101',
      'Student application form for Beatmaking 101',
      'Beatmaking 101 - postulación al curso',
      'Beatmaking 101 - student application page',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Application form');
        expect(emptyState?.textContent).not.toContain('Student application');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - postulación');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips onboarding-form wrappers from first-run cohort copy', async () => {
    const titles = [
      'Student onboarding form for Beatmaking 101',
      'Course onboarding page - Beatmaking 101',
      'Formulario de onboarding para Beatmaking 101',
      'Beatmaking 101 - student onboarding portal',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/onboarding\s+(?:form|page|portal)|student onboarding|formulario de onboarding/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips application-packet wrappers from first-run cohort copy', async () => {
    const titles = [
      'Application packet for Beatmaking 101',
      'Beatmaking 101 - student application packet',
      'Paquete de postulación para Beatmaking 101',
      'Beatmaking 101 - paquete de aplicación',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent?.toLocaleLowerCase('es')).not.toMatch(
          /application packet|paquete de (?:aplicaci[oó]n|postulaci[oó]n)/,
        );
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('keeps legitimate student-application course titles in first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'student-applications', ccTitle: 'Student Applications in Music Production' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Todavía no hay inscripciones para Student Applications in Music Production. La página pública ya está lista para recibir la primera.',
      );
      expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Music Production.');
    });

    await cleanup();
  });

  it('strips course-connector enrollment descriptors from first-run cohort copy', async () => {
    const titles = [
      'Formulario de inscripción al curso - Beatmaking 101',
      'Inscripción para el curso - Beatmaking 101',
      'Beatmaking 101 - solicitud de inscripción del curso',
      'Beatmaking 101 - ficha de preinscripción para el curso',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario de inscripción');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Inscripción para el curso');
        expect(emptyState?.textContent).not.toContain('solicitud de inscripción');
        expect(emptyState?.textContent).not.toContain('preinscripción');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips student-registration descriptors from first-run cohort copy', async () => {
    const titles = [
      'Student registration form for Beatmaking 101',
      'Beatmaking 101 - student enrollment page',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Student');
        expect(emptyState?.textContent).not.toContain('student enrollment');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips leftover Spanish course connectors from first-run cohort copy', async () => {
    const titles = [
      'Formulario del curso de Beatmaking 101',
      'Página del curso de Beatmaking 101',
      'Solicitud de inscripción para el curso de Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para de Beatmaking 101.');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips standalone Spanish enrollment descriptors from first-run cohort copy', async () => {
    const titles = [
      'Inscripción - Beatmaking 101',
      'Formulario para inscripción - Beatmaking 101',
      'Beatmaking 101 - formulario para inscripción',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Inscripción');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario');
        expect(emptyState?.textContent).not.toContain('formulario para inscripción');
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips Spanish matrícula descriptors from first-run cohort copy', async () => {
    const titles = [
      'Matrícula del curso - Beatmaking 101',
      'Beatmaking 101 - matrícula del curso',
      'Formulario de matrícula - Beatmaking 101',
      'Página de matrícula - Beatmaking 101',
      'Beatmaking 101 - página de matrícula',
      'Formulario de pre-matrícula - Beatmaking 101',
      'Pre-matrícula del curso - Beatmaking 101',
      'Beatmaking 101 - pre matrícula del curso',
      'Beatmaking 101 - página de prematrícula',
      'Formulario de matriculación - Beatmaking 101',
      'Beatmaking 101 - formulario de matriculación',
      'Matriculación del curso - Beatmaking 101',
      'Beatmaking 101 - matriculación del curso',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Matrícula');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - matrícula');
        expect(emptyState?.textContent?.toLocaleLowerCase('es')).not.toMatch(/pre[-\s]?matr[ií]cula/);
        expect(emptyState?.textContent?.toLocaleLowerCase('es')).not.toMatch(/matriculaci[oó]n/);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips Spanish registration-request descriptors from first-run cohort copy', async () => {
    const titles = [
      'Solicitud de inscripción - Beatmaking 101',
      'Beatmaking 101 - solicitud de inscripción',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Solicitud');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - solicitud');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips Spanish pre-registration descriptors from first-run cohort copy', async () => {
    const titles = [
      'Pre-registro - Beatmaking 101',
      'Formulario de pre-registro - Beatmaking 101',
      'Beatmaking 101 - formulario de pre-registro',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent?.toLocaleLowerCase('es')).not.toMatch(/pre[-\s]?registro/);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips course-enrollment descriptor prefixes from first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Inscripciones del curso - Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Inscripciones del curso - Beatmaking 101');
      expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Inscripciones del curso');
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
      ).toBe('Abrir formulario público de Beatmaking 101');
    });

    await cleanup();
  });

  it('strips singular course-enrollment descriptors from first-run cohort copy', async () => {
    const titles = [
      'Inscripción al curso - Beatmaking 101',
      'Beatmaking 101 - inscripción al curso',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Inscripción');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - inscripción');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips ficha-style registration descriptors from first-run cohort copy', async () => {
    const titles = [
      'Ficha de inscripción - Beatmaking 101',
      'Beatmaking 101 - ficha de inscripción',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Ficha');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - ficha');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips workshop-form descriptors from first-run cohort copy', async () => {
    const titles = [
      'Formulario del taller - Beatmaking 101',
      'Solicitud de inscripción al taller - Beatmaking 101',
      'Workshop application form - Beatmaking 101',
      'Beatmaking 101 - workshop registration form',
      'Beatmaking 101 - workshop application form',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario del taller');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Solicitud de inscripción al taller');
        expect(emptyState?.textContent).not.toContain('workshop registration');
        expect(emptyState?.textContent).not.toContain('workshop application');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips class-registration descriptors from first-run cohort copy', async () => {
    const titles = [
      'Class registration form - Beatmaking 101',
      'Masterclass registration form - Beatmaking 101',
      'Beatmaking 101 - class enrollment page',
      'Beatmaking 101 - masterclass enrollment page',
      'Formulario de inscripción a la clase - Beatmaking 101',
      'Beatmaking 101 - página de inscripción de clase',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('class registration');
        expect(emptyState?.textContent).not.toContain('Masterclass registration');
        expect(emptyState?.textContent).not.toContain('class enrollment');
        expect(emptyState?.textContent).not.toContain('masterclass enrollment');
        expect(emptyState?.textContent).not.toContain('inscripción a la clase');
        expect(emptyState?.textContent).not.toContain('inscripción de clase');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips trial-class wrappers from first-run cohort copy', async () => {
    const titles = [
      'Trial lesson form - Beatmaking 101',
      'Free trial class registration - Beatmaking 101',
      'Beatmaking 101 - forms for trial class',
      'Formulario de clase de prueba - Beatmaking 101',
      'Beatmaking 101 - inscripción para la sesión de prueba',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Trial lesson form');
        expect(emptyState?.textContent).not.toContain('trial class');
        expect(emptyState?.textContent).not.toContain('clase de prueba');
        expect(emptyState?.textContent).not.toContain('sesión de prueba');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips program-registration descriptors from first-run cohort copy', async () => {
    const titles = [
      'Program registration form - Beatmaking 101',
      'Training enrollment page - Beatmaking 101',
      'Beatmaking 101 - program enrollment page',
      'Formulario de inscripción al programa - Beatmaking 101',
      'Beatmaking 101 - matrícula del programa',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('program registration');
        expect(emptyState?.textContent).not.toContain('training enrollment');
        expect(emptyState?.textContent).not.toContain('program enrollment');
        expect(emptyState?.textContent).not.toContain('inscripción al programa');
        expect(emptyState?.textContent).not.toContain('matrícula del programa');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips waitlist-form descriptors from first-run cohort copy', async () => {
    const titles = [
      'Lista de espera - Beatmaking 101',
      'Waiting list form - Beatmaking 101',
      'Beatmaking 101 - waitlist form',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Lista de espera');
        expect(emptyState?.textContent).not.toContain('waiting list');
        expect(emptyState?.textContent).not.toContain('waitlist form');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips interest-list descriptors from first-run cohort copy', async () => {
    const titles = [
      'Lista de interesados - Beatmaking 101',
      'Interest list form - Beatmaking 101',
      'Beatmaking 101 - formulario para interesados',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Lista de interesados');
        expect(emptyState?.textContent).not.toContain('interest list');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - formulario');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips interest-signup descriptors from first-run cohort copy', async () => {
    const titles = [
      'Course interest sign-up - Beatmaking 101',
      'Interested sign-up form - Beatmaking 101',
      'Beatmaking 101 - interest signup page',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/interest(?:ed)?\s+sign[-\s]?up/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips newsletter-list descriptors from first-run cohort copy', async () => {
    const titles = [
      'Newsletter signup form - Beatmaking 101',
      'Beatmaking 101 - mailing list signup page',
      'Formulario de newsletter - Beatmaking 101',
      'Beatmaking 101 - suscripción al boletín',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/newsletter|mailing list|bolet[ií]n/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips lead-capture descriptors from first-run cohort copy', async () => {
    const titles = [
      'Lead capture form - Beatmaking 101',
      'Beatmaking 101 - lead capture page',
      'Formulario de captación de leads - Beatmaking 101',
      'Captura de prospectos - Beatmaking 101',
      'Beatmaking 101 - captura de leads',
      'Beatmaking 101 - página de captación de interesados',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Lead capture');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario de captación');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Captura de prospectos');
        expect(emptyState?.textContent).not.toContain('captura de leads');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Beatmaking 101 - página');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips lead-generation descriptors from first-run cohort copy', async () => {
    const titles = [
      'Lead gen form - Beatmaking 101',
      'Lead generation page - Beatmaking 101',
      'Beatmaking 101 - lead gen form',
      'Beatmaking 101 - lead generation page',
      'Formulario de generación de leads - Beatmaking 101',
      'Beatmaking 101 - generación de prospectos',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/lead[-\s]+gen(?:eration)?|generaci[oó]n de (?:leads?|prospectos|interesad[oa]s)/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips lead-magnet wrappers from first-run cohort copy', async () => {
    const titles = [
      'Lead magnet form - Beatmaking 101',
      'Beatmaking 101 - lead magnet download page',
      'Formulario de recurso gratuito - Beatmaking 101',
      'Beatmaking 101 - descarga de recurso gratuito',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/lead magnet|recurso gratuito/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips downloadable resource wrappers from first-run cohort copy', async () => {
    const titles = [
      'Free guide download - Beatmaking 101',
      'Beatmaking 101 - ebook request form',
      'Checklist opt-in page - Beatmaking 101',
      'Descarga de guía - Beatmaking 101',
      'Beatmaking 101 - descarga de plantilla',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/free guide|ebook|checklist|gu[ií]a|plantilla/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips brochure and syllabus download wrappers from first-run cohort copy', async () => {
    const titles = [
      'Course brochure download - Beatmaking 101',
      'Beatmaking 101 - syllabus request form',
      'Folleto del curso - Beatmaking 101',
      'Beatmaking 101 - descarga de folleto',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/brochure|syllabus|folleto/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips opt-in page wrappers from first-run cohort copy', async () => {
    const titles = [
      'Opt-in page - Beatmaking 101',
      'Beatmaking 101 - squeeze page',
      'Formulario de opt-in - Beatmaking 101',
      'Beatmaking 101 - página de opt-in',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/opt[-\s]?in|squeeze/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips course inquiry descriptors from first-run cohort copy', async () => {
    const titles = [
      'Course inquiry form - Beatmaking 101',
      'Course enquiry form - Beatmaking 101',
      'Course interest form - Beatmaking 101',
      'Beatmaking 101 - course inquiry page',
      'Beatmaking 101 - course enquiry page',
      'Beatmaking 101 - course interest page',
      'Formulario de consulta del curso - Beatmaking 101',
      'Beatmaking 101 - formulario de interés del curso',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Course inquiry');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Course enquiry');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Course interest');
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Formulario de consulta');
        expect(emptyState?.textContent).not.toContain('formulario de interés');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips survey and questionnaire descriptors from first-run cohort copy', async () => {
    const titles = [
      'Course survey form - Beatmaking 101',
      'Beatmaking 101 - registration questionnaire',
      'Cuestionario de inscripción - Beatmaking 101',
      'Beatmaking 101 - encuesta del curso',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/survey|questionnaire|encuesta|cuestionario/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips prospect-form descriptors from first-run cohort copy', async () => {
    const titles = [
      'Prospect form - Beatmaking 101',
      'Beatmaking 101 - prospects page',
      'Formulario de prospectos - Beatmaking 101',
      'Beatmaking 101 - página de prospectos',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones para Prospect');
        expect(emptyState?.textContent).not.toContain('prospects page');
        expect(emptyState?.textContent).not.toContain('Formulario de prospectos');
        expect(emptyState?.textContent).not.toContain('página de prospectos');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips reservation and RSVP form descriptors from first-run cohort copy', async () => {
    const titles = [
      'Booking form for Beatmaking 101',
      'Beatmaking 101 - reservation page',
      'RSVP form for Beatmaking 101',
      'Beatmaking 101 - RSVP page',
      'Formulario de reserva - Beatmaking 101',
      'Formulario de RSVP - Beatmaking 101',
      'Beatmaking 101 - solicitud de reserva',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toContain('Booking form');
        expect(emptyState?.textContent).not.toContain('reservation page');
        expect(emptyState?.textContent).not.toMatch(/rsvp/i);
        expect(emptyState?.textContent).not.toContain('Formulario de reserva');
        expect(emptyState?.textContent).not.toContain('solicitud de reserva');
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips scheduling-provider link wrappers from first-run cohort copy', async () => {
    const titles = [
      'Calendly link - Beatmaking 101',
      'Beatmaking 101 - Cal.com URL',
      'Enlace de Calendly - Beatmaking 101',
      'Beatmaking 101 - portal de Google Calendar',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        const copy = emptyState?.textContent ?? '';
        expect(emptyState).not.toBeNull();
        expect(copy).toContain(singleCohortInitialEmptyStateMessage);
        expect(copy).not.toContain(title);
        expect(copy).not.toMatch(/calendly|cal\.com|google calendar/i);
        expect(copy).not.toMatch(/enlace|portal/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips first-run form descriptors across decorative separators', async () => {
    const titles = [
      'Formulario público \u2014 Beatmaking 101',
      'Beatmaking 101 \u00b7 formulario público',
      'Registration page \u2013 Beatmaking 101',
      'Beatmaking 101 \u2022 enrollment portal',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/[\u00b7\u2022\u2013\u2014]/);
        expect(emptyState?.textContent).not.toMatch(/registration page|enrollment portal/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips inline HTML tags from first-run course names before showing public-form guidance', async () => {
    const titles = [
      '<strong>Registration page - Beatmaking 101</strong>',
      '<span class="label">Formulario público - Beatmaking 101</span>',
      '<em>Beatmaking 101 - enrollment portal</em>',
      '<a href="/inscripcion/beatmaking-101">Beatmaking 101</a>',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain('<strong>');
        expect(emptyState?.textContent).not.toContain('<span');
        expect(emptyState?.textContent).not.toContain('<em>');
        expect(emptyState?.textContent).not.toContain('<a');
        expect(emptyState?.textContent).not.toContain('</a>');
        expect(emptyState?.textContent).not.toContain('href=');
        expect(emptyState?.textContent).not.toMatch(/registration page|enrollment portal/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('decodes escaped provider title text before showing first-run course guidance', async () => {
    const cases = [
      {
        slug: 'beatmaking-produccion',
        title: 'Registration&nbsp;page - Beatmaking &amp; Producción',
        label: 'Beatmaking & Producción',
      },
      {
        slug: 'mezcla-mastering',
        title: 'Formulario&nbsp;público - Mezcla &#38; Mastering',
        label: 'Mezcla & Mastering',
      },
      {
        slug: 'sintesis-y-samples',
        title: '&lt;strong&gt;Registration&nbsp;page - Síntesis&#x20;y&#x20;Samples&lt;/strong&gt;',
        label: 'Síntesis y Samples',
      },
    ];

    for (const { label, slug, title } of cases) {
      listCohortsMock.mockResolvedValue([{ ccSlug: slug, ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        const formAction = emptyState?.querySelector<HTMLAnchorElement>(`a[href="/inscripcion/${slug}"]`);

        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(
          `Todavía no hay inscripciones para ${label}. La página pública ya está lista para recibir la primera.`,
        );
        expect(emptyState?.textContent).not.toMatch(/&(?:amp|nbsp|#38|#x20|lt|gt);/i);
        expect(emptyState?.textContent).not.toMatch(/registration page/i);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(countOccurrences(emptyState!, label)).toBe(1);
        expect(formAction?.textContent?.trim()).toBe(initialEmptyStateFormActionLabel);
        expect(formAction?.getAttribute('aria-label')).toBe(`Abrir formulario público de ${label}`);
        expect(formAction?.getAttribute('title')).toBe(`Abrir formulario público de ${label} en una pestaña nueva`);
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips decorative emoji wrappers from first-run cohort copy', async () => {
    const titles = [
      '🎧 Beatmaking 101 ✨',
      '🚀 Registration page - Beatmaking 101 ✅',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/🎧|✨|🚀|✅/u);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips draft markers before provider descriptors in first-run cohort copy', async () => {
    const titles = [
      '[DRAFT] Typeform registration page - Beatmaking 101',
      'BORRADOR: Formulario de Tally para Beatmaking 101',
      'WIP - Google Forms - Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toMatch(/DRAFT|BORRADOR|WIP|Typeform|Tally|Google Forms|registration page/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips enrollment workflow descriptors from first-run cohort copy', async () => {
    const titles = [
      'Enrollment funnel - Beatmaking 101',
      'Beatmaking 101 - registration flow',
      'Course registration workflow - Beatmaking 101',
      'Beatmaking 101 - enrollment workflow',
      'Registration landing page - Beatmaking 101',
      'Beatmaking 101 - enrollment landing page',
      'Application landing page for Beatmaking 101',
      'Flujo de inscripción - Beatmaking 101',
      'Beatmaking 101 - embudo de admisiones',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(
          /enrollment funnel|registration flow|registration workflow|enrollment workflow|landing page|flujo de inscripci[oó]n|embudo de admisiones/i,
        );
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips early-bird offer descriptors from first-run cohort copy', async () => {
    const titles = [
      'Early bird registration form - Beatmaking 101',
      'Beatmaking 101 - early bird signup page',
      'Discount enrollment page - Beatmaking 101',
      'Formulario de descuento - Beatmaking 101',
      'Beatmaking 101 - promoción de inscripción',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/early[-\s]?bird|discount|descuento|promoci[oó]n/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips apply-now wrappers from first-run cohort copy', async () => {
    const titles = [
      'Apply now - Beatmaking 101',
      'Register here - Beatmaking 101',
      'Beatmaking 101 - enroll now',
      'Beatmaking 101 - sign up here',
      'Inscríbete ahora - Beatmaking 101',
      'Regístrate aquí - Beatmaking 101',
      'Register for Beatmaking 101',
      'Register for the course - Beatmaking 101',
      'Inscríbete al curso - Beatmaking 101',
      'Beatmaking 101 - regístrate ahora',
      'Beatmaking 101 - inscríbete aquí',
      'Beatmaking 101 - regístrate al curso',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(
          /apply now|register here|register for|enroll now|sign up here|inscr[ií]bete ahora|inscr[ií]bete al curso|reg[ií]strate aqu[ií]|reg[ií]strate ahora|reg[ií]strate al curso|inscr[ií]bete aqu[ií]|the course/i,
        );
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips open-enrollment wrappers from first-run cohort copy', async () => {
    const titles = [
      'Inscripciones abiertas - Beatmaking 101',
      'Inscripción abierta: Beatmaking 101',
      'Open enrollment for Beatmaking 101',
      'Beatmaking 101 - open registration',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain(title);
        expect(emptyState?.textContent).not.toMatch(/inscripci[oó]n(?:es)?\s+abiertas?|open\s+(?:enrollment|registration)|registration\s+open/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips priority waitlist wrappers from first-run cohort copy', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101-vip', ccTitle: 'VIP waitlist - Beatmaking 101' },
      { ccSlug: 'beatmaking-101-prioridad', ccTitle: 'Lista prioritaria - beatmaking 101' },
      { ccSlug: 'mixing-bootcamp-priority', ccTitle: 'Priority list - Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      const configAction = emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]');

      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Hay 3 formularios públicos listos para recibir la primera inscripción: Beatmaking 101 y Mixing Bootcamp.',
      );
      expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
      expect(countOccurrences(emptyState!, 'Mixing Bootcamp')).toBe(1);
      expect(emptyState?.textContent).not.toMatch(/VIP waitlist|Lista prioritaria|Priority list/i);
      expect(configAction?.textContent?.trim()).toBe(initialEmptyStateMultiCohortActionLabel);
      expect(configAction?.getAttribute('title')).toBe(
        'Elegir entre 3 formularios públicos: Beatmaking 101 y Mixing Bootcamp.',
      );
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('unwraps formatted first-run course names after stripping form descriptors', async () => {
    const titles = [
      'Formulario público (Beatmaking 101)',
      'Registration page [Beatmaking 101]',
      '(Beatmaking 101) enrollment portal',
      '[Beatmaking 101](https://example.test/beatmaking-101)',
      'Formulario público [Beatmaking 101](https://example.test/beatmaking-101)',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        expect(emptyState).not.toBeNull();
        expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
        expect(emptyState?.textContent).not.toContain('(Beatmaking 101)');
        expect(emptyState?.textContent).not.toContain('[Beatmaking 101]');
        expect(emptyState?.textContent).not.toContain('https://example.test/beatmaking-101');
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips standalone public-page wrappers from first-run cohort copy', async () => {
    const titles = [
      'Página pública - Beatmaking 101',
      'Beatmaking 101 - página pública',
      'Public page: Beatmaking 101',
      'Beatmaking 101 public portal',
      'Portal público - Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        const copy = emptyState?.textContent ?? '';
        expect(emptyState).not.toBeNull();
        expect(copy).toContain(singleCohortInitialEmptyStateMessage);
        expect(copy).not.toContain(title);
        expect(copy).not.toMatch(/p[aá]gina\s+p[uú]blica\s*[-:/|]/i);
        expect(copy).not.toMatch(/[-:/|]\s*p[aá]gina\s+p[uú]blica/i);
        expect(copy).not.toMatch(/public\s+(?:page|portal)/i);
        expect(copy.toLocaleLowerCase('es').split('página pública')).toHaveLength(2);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips bio-link wrappers from first-run cohort copy', async () => {
    const titles = [
      'Linktree - Beatmaking 101',
      'Beatmaking 101 - link in bio',
      'Bio link: Beatmaking 101',
      'Enlace en bio para Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        const copy = emptyState?.textContent ?? '';
        expect(emptyState).not.toBeNull();
        expect(copy).toContain(singleCohortInitialEmptyStateMessage);
        expect(copy).not.toContain(title);
        expect(copy).not.toMatch(/link\s*tree|linktree|bio\s+link|link\s+in\s+bio|enlace\s+en\s+bio/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.textContent?.trim(),
        ).toBe(initialEmptyStateFormActionLabel);
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('deduplicates link-in-bio provider variants in first-run copy', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-beacons', ccTitle: 'Beacons page - Beatmaking 101' },
      { ccSlug: 'beatmaking-stan-store', ccTitle: 'Stan Store - Beatmaking 101' },
      { ccSlug: 'beatmaking-koji', ccTitle: 'Beatmaking 101 - Koji link' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      const configAction = emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]');
      const copy = emptyState?.textContent ?? '';

      expect(emptyState).not.toBeNull();
      expect(copy).toContain(
        'Hay 3 variantes públicas de Beatmaking 101 listas para recibir la primera inscripción.',
      );
      expect(copy).not.toMatch(/Beacons|Stan Store|Koji/i);
      expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
      expect(countOccurrences(emptyState!, initialEmptyStateSingleCourseVariantActionLabel)).toBe(1);
      expect(countOccurrences(emptyState!, initialEmptyStateMultiCohortActionLabel)).toBe(0);
      expect(configAction?.getAttribute('aria-label')).toBe(initialEmptyStateSingleCourseVariantActionAriaLabel);
      expect(configAction?.getAttribute('title')).toBe('Elegir entre 3 variantes públicas para Beatmaking 101.');
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('strips course-community wrappers from first-run cohort copy', async () => {
    const titles = [
      'Course community - Beatmaking 101',
      'Beatmaking 101 - student group chat',
      'Grupo del curso - Beatmaking 101',
      'Beatmaking 101 - comunidad de estudiantes',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        const copy = emptyState?.textContent ?? '';
        expect(emptyState).not.toBeNull();
        expect(copy).toContain(singleCohortInitialEmptyStateMessage);
        expect(copy).not.toContain(title);
        expect(copy).not.toMatch(/course\s+community|student\s+group\s+chat|grupo\s+del\s+curso|comunidad\s+de\s+estudiantes/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips learning-portal wrappers from first-run cohort copy', async () => {
    const titles = [
      'Student portal - Beatmaking 101',
      'Beatmaking 101 - learner dashboard',
      'LMS portal: Beatmaking 101',
      'Campus virtual - Beatmaking 101',
      'Beatmaking 101 - aula virtual',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        const copy = emptyState?.textContent ?? '';
        expect(emptyState).not.toBeNull();
        expect(copy).toContain(singleCohortInitialEmptyStateMessage);
        expect(copy).not.toContain(title);
        expect(copy).not.toMatch(/student\s+portal|learner\s+dashboard|lms\s+portal|campus\s+virtual|aula\s+virtual/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips profile-link wrappers from first-run cohort copy', async () => {
    const titles = [
      'Profile link - Beatmaking 101',
      'Beatmaking 101 - bio page',
      'Enlace de perfil para Beatmaking 101',
      'Página de bio - Beatmaking 101',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        const copy = emptyState?.textContent ?? '';
        expect(emptyState).not.toBeNull();
        expect(copy).toContain(singleCohortInitialEmptyStateMessage);
        expect(copy).not.toContain(title);
        expect(copy).not.toMatch(/profile\s+link|bio\s+page|enlace\s+de\s+perfil|p[aá]gina\s+de\s+bio/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips web-portal wrappers from first-run cohort copy', async () => {
    const titles = [
      'Course portal - Beatmaking 101',
      'Beatmaking 101 course portal',
      'Web portal: Beatmaking 101',
      'Beatmaking 101 - portal web',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        const copy = emptyState?.textContent ?? '';
        expect(emptyState).not.toBeNull();
        expect(copy).toContain(singleCohortInitialEmptyStateMessage);
        expect(copy).not.toContain(title);
        expect(copy).not.toMatch(/course\s+portal|web\s+portal|portal\s+web/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips course-catalog wrappers from first-run cohort copy', async () => {
    const titles = [
      'Course catalog page - Beatmaking 101',
      'Beatmaking 101 - course listing page',
      'Catálogo de cursos - Beatmaking 101',
      'Beatmaking 101 - listado de cursos',
    ];

    for (const title of titles) {
      listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: title }]);
      listRegistrationsMock.mockResolvedValue([]);

      const container = document.createElement('div');
      document.body.appendChild(container);
      const { cleanup } = await renderPage(container);

      await waitForExpectation(() => {
        const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
        const copy = emptyState?.textContent ?? '';
        expect(emptyState).not.toBeNull();
        expect(copy).toContain(singleCohortInitialEmptyStateMessage);
        expect(copy).not.toContain(title);
        expect(copy).not.toMatch(/course\s+(?:catalog|listing)|cat[aá]logo de cursos|listado de cursos/i);
        expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
        expect(
          emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]')?.getAttribute('aria-label'),
        ).toBe('Abrir formulario público de Beatmaking 101');
        expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      });

      await cleanup();
    }
  });

  it('strips wrapping quotes from first-run cohort labels', async () => {
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: '“Beatmaking 101”' }]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      const formAction = emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]');

      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('“Beatmaking 101”');
      expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
      expect(formAction?.textContent?.trim()).toBe(initialEmptyStateFormActionLabel);
      expect(formAction?.getAttribute('aria-label')).toBe('Abrir formulario público de Beatmaking 101');
      expect(formAction?.getAttribute('title')).toBe('Abrir formulario público de Beatmaking 101 en una pestaña nueva');
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
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
      expect(loadingState?.textContent).not.toContain('cohortes');
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
          (el) => (el.textContent ?? '').trim().startsWith('Copiar CSV'),
        ),
      ).toBe(false);
    });

    await cleanup();
  });

  it('waits for cohort context before turning a limit-only first-run into list recovery', async () => {
    listCohortsMock.mockImplementation(() => new Promise<CourseCohortOptionDTO[]>(() => undefined));
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?limit=50');

    await waitForExpectation(() => {
      const loadingState = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-initial-cohort-loading"]',
      );

      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: undefined,
        limit: 50,
      });
      expect(loadingState?.textContent).toContain(initialCohortResolutionMessage);
      expect(container.textContent).not.toContain('No hay inscripciones con el límite actual');
      expect(container.querySelector('[data-testid="course-registration-results-panel"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-initial-empty-state"]')).toBeNull();
      expect(container.querySelector('[data-testid="course-registration-filter-utilities"]')).toBeNull();
      expect(hasLabel(container, loadLimitLabel)).toBe(false);
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
      expect(countButtonsByText(container, 'Restablecer límite')).toBe(0);
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
      expect(errorState?.textContent).not.toContain('cohortes');
      expect(errorState?.textContent).not.toContain('filtrar o revisar la lista');
      expect(countButtonsByText(container, initialCohortRetryLabel)).toBe(1);
      expect(countButtonsByText(container, 'Reintentar cohortes')).toBe(0);
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
      clickButton(getButtonByText(container, initialCohortRetryLabel));
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
      expect(countOccurrences(emptyState!, 'formulario público')).toBe(1);
      expect(emptyState?.textContent).not.toContain('Configura el primer formulario público');
      expect(emptyState?.textContent).not.toContain('para empezar a recibirlas aquí');
      expect(countButtonsByText(emptyState!, 'Configurar formulario')).toBe(0);
      expect(emptyState?.textContent).not.toContain('pago, seguimiento y correos');
      expect(emptyState?.textContent).not.toContain('curso inicial');
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]')?.textContent?.trim(),
      ).toBe(initialEmptyStateConfigActionLabel);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]')?.getAttribute('aria-label'),
      ).toBe(initialEmptyStateConfigActionAriaLabel);
      expect(
        emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]')?.getAttribute('title'),
      ).toBe(initialEmptyStateConfigActionAriaLabel);
      expect(emptyState?.querySelector('[data-testid="course-registration-initial-empty-state-new-tab-icon"]')).toBeNull();
      expect(emptyState?.querySelector('a[href^="/inscripcion/"]')).toBeNull();
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
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
      expect(emptyState?.textContent).not.toContain('Todavía no hay inscripciones. Hay');
      expect(emptyState?.textContent).not.toContain('elige cuál compartir primero');
      expect(emptyState?.textContent).not.toContain('pago, seguimiento y correos');
      expect(emptyState?.textContent).toContain('Beatmaking 101 y Mixing Bootcamp');
      expect(emptyState?.textContent).not.toContain(initialEmptyStateMultiCohortActionAriaLabel);
      expect(emptyState?.textContent).not.toContain('Elige en Configuración de cursos');
      expect(emptyState?.textContent).not.toContain('Elegir curso');
      expect(emptyState?.textContent).not.toContain('Elegir en cursos');
      expect(emptyState?.textContent).not.toContain('Revisar cursos');
      expect(emptyState?.textContent).not.toMatch(/revisa cursos/i);
      expect(emptyState?.textContent).not.toContain('copiar o abrir');
      expect(emptyState?.textContent).not.toContain('Ver cohortes');
      expect(emptyState?.textContent).not.toContain('Elegir enlace');
      expect(emptyState?.textContent).not.toContain('Elegir cuál compartir');
      expect(emptyState?.textContent).not.toContain('Gestionar cursos');
      expect(emptyState?.textContent).not.toContain('Ver cursos configurados');
      expect(countOccurrences(emptyState!, initialEmptyStateMultiCohortActionLabel)).toBe(1);
      expect(countOccurrences(emptyState!, 'formularios públicos')).toBe(1);
      const configAction = emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]');
      expect(configAction?.textContent?.trim()).toBe(initialEmptyStateMultiCohortActionLabel);
      expect(configAction?.getAttribute('aria-label')).toBe(initialEmptyStateMultiCohortActionAriaLabel);
      expect(configAction?.getAttribute('title')).toBe(
        'Elegir entre 2 formularios públicos: Beatmaking 101 y Mixing Bootcamp.',
      );
      expect(emptyState?.querySelector('[data-testid="course-registration-initial-empty-state-new-tab-icon"]')).toBeNull();
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
      expect(emptyState?.querySelector('a[href^="/inscripcion/"]')).toBeNull();
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.querySelector('[data-testid="course-registration-current-view-summary"]')).toBeNull();
      expect(countButtonsByText(container, 'Refrescar lista')).toBe(0);
    });

    await cleanup();
  });

  it('keeps hidden multi-cohort options in the action tooltip instead of expanding first-run copy', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
      { ccSlug: 'produccion-vocal', ccTitle: 'Producción Vocal' },
      { ccSlug: 'dj-lab', ccTitle: 'DJ Lab' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      const configAction = emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Hay 4 formularios públicos listos para recibir la primera inscripción: Beatmaking 101, Mixing Bootcamp y 2 cursos más.',
      );
      expect(emptyState?.textContent).not.toContain('Producción Vocal');
      expect(emptyState?.textContent).not.toContain('DJ Lab');
      expect(configAction?.textContent?.trim()).toBe(initialEmptyStateMultiCohortActionLabel);
      expect(configAction?.getAttribute('title')).toBe(
        'Elegir entre 4 formularios públicos: Beatmaking 101, Mixing Bootcamp, Producción Vocal y DJ Lab.',
      );
      expect(configAction?.getAttribute('aria-label')).toBe(initialEmptyStateMultiCohortActionAriaLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('summarizes very long multi-cohort action tooltips in the first-run empty state', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
      { ccSlug: 'produccion-vocal', ccTitle: 'Producción Vocal' },
      { ccSlug: 'dj-lab', ccTitle: 'DJ Lab' },
      { ccSlug: 'live-sound', ccTitle: 'Live Sound' },
      { ccSlug: 'music-business', ccTitle: 'Music Business' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      const configAction = emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Hay 6 formularios públicos listos para recibir la primera inscripción: Beatmaking 101, Mixing Bootcamp y 4 cursos más.',
      );
      expect(emptyState?.textContent).not.toContain('Producción Vocal');
      expect(emptyState?.textContent).not.toContain('DJ Lab');
      expect(configAction?.textContent?.trim()).toBe(initialEmptyStateMultiCohortActionLabel);
      expect(configAction?.getAttribute('title')).toBe(
        'Elegir entre 6 formularios públicos: Beatmaking 101, Mixing Bootcamp, Producción Vocal y 3 cursos más.',
      );
      expect(configAction?.getAttribute('title')).not.toContain('DJ Lab');
      expect(configAction?.getAttribute('title')).not.toContain('Live Sound');
      expect(configAction?.getAttribute('title')).not.toContain('Music Business');
      expect(configAction?.getAttribute('aria-label')).toBe(initialEmptyStateMultiCohortActionAriaLabel);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('deduplicates repeated multi-cohort preview labels while keeping the form count', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'beatmaking-101-alt', ccTitle: 'Formulario público - beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Hay 3 formularios públicos listos para recibir la primera inscripción: Beatmaking 101 y Mixing Bootcamp.',
      );
      expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
      expect(emptyState?.textContent).not.toContain('Beatmaking 101, beatmaking 101');
      expect(emptyState?.textContent).not.toContain('y 1 más');
      expect(countOccurrences(emptyState!, initialEmptyStateMultiCohortActionLabel)).toBe(1);
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('deduplicates first-run form labels that only add trailing punctuation', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101-main', ccTitle: 'Beatmaking 101!' },
      { ccSlug: 'beatmaking-101-alt', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'mixing-bootcamp-main', ccTitle: 'Mixing Bootcamp:' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      const configAction = emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Hay 3 formularios públicos listos para recibir la primera inscripción: Beatmaking 101 y Mixing Bootcamp.',
      );
      expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
      expect(countOccurrences(emptyState!, 'Mixing Bootcamp')).toBe(1);
      expect(emptyState?.textContent).not.toContain('Beatmaking 101!');
      expect(emptyState?.textContent).not.toContain('Mixing Bootcamp:');
      expect(configAction?.textContent?.trim()).toBe(initialEmptyStateMultiCohortActionLabel);
      expect(configAction?.getAttribute('title')).toBe(
        'Elegir entre 3 formularios públicos: Beatmaking 101 y Mixing Bootcamp.',
      );
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('keeps duplicate multi-cohort form variants out of the first-run preview', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'beatmaking-101-weekend', ccTitle: 'Formulario público - beatmaking 101' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Mixing Bootcamp' },
      { ccSlug: 'mixing-bootcamp-night', ccTitle: 'Inscripción - Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Hay 4 formularios públicos listos para recibir la primera inscripción: Beatmaking 101 y Mixing Bootcamp.',
      );
      expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
      expect(countOccurrences(emptyState!, 'Mixing Bootcamp')).toBe(1);
      expect(emptyState?.textContent).not.toContain('y 2 más');
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('deduplicates A/B test cohort variants in first-run copy', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101-a', ccTitle: 'A/B test - Beatmaking 101' },
      { ccSlug: 'beatmaking-101-b', ccTitle: 'Beatmaking 101 - Variante B' },
      { ccSlug: 'mixing-bootcamp', ccTitle: 'Split test - Mixing Bootcamp' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      const configAction = emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]');

      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Hay 3 formularios públicos listos para recibir la primera inscripción: Beatmaking 101 y Mixing Bootcamp.',
      );
      expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
      expect(emptyState?.textContent).not.toMatch(/A\/B test|Variante B|Split test/i);
      expect(configAction?.textContent?.trim()).toBe(initialEmptyStateMultiCohortActionLabel);
      expect(configAction?.getAttribute('title')).toBe(
        'Elegir entre 3 formularios públicos: Beatmaking 101 y Mixing Bootcamp.',
      );
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('clarifies when all first-run form labels are variants of one course', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' },
      { ccSlug: 'beatmaking-101-weekend', ccTitle: 'Formulario público - beatmaking 101' },
      { ccSlug: 'beatmaking-101-late', ccTitle: 'Beatmaking 101 (beatmaking-101-late)' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      const configAction = emptyState?.querySelector<HTMLAnchorElement>('a[href="/configuracion/cursos"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(
        'Hay 3 variantes públicas de Beatmaking 101 listas para recibir la primera inscripción.',
      );
      expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
      expect(countOccurrences(emptyState!, initialEmptyStateSingleCourseVariantActionLabel)).toBe(1);
      expect(countOccurrences(emptyState!, initialEmptyStateMultiCohortActionLabel)).toBe(0);
      expect(configAction?.getAttribute('aria-label')).toBe(initialEmptyStateSingleCourseVariantActionAriaLabel);
      expect(configAction?.getAttribute('title')).toBe('Elegir entre 3 variantes públicas para Beatmaking 101.');
      expect(emptyState?.textContent).not.toContain(
        'Hay 3 formularios públicos listos para recibir la primera inscripción: Beatmaking 101.',
      );
      expect(emptyState?.textContent).not.toContain(
        'Hay 3 formularios públicos para Beatmaking 101 listos para recibir la primera inscripción.',
      );
      expect(emptyState?.textContent).not.toContain('y 2 más');
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('deduplicates repeated first-run course title segments before showing the empty state', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101 - Beatmaking 101' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      const publicFormAction = emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]');

      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Beatmaking 101 - Beatmaking 101');
      expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
      expect(publicFormAction?.getAttribute('aria-label')).toBe('Abrir formulario público de Beatmaking 101');
      expect(publicFormAction?.getAttribute('title')).toBe(
        'Abrir formulario público de Beatmaking 101 en una pestaña nueva',
      );
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
    });

    await cleanup();
  });

  it('deduplicates bracketed first-run course title repeats before showing the empty state', async () => {
    listCohortsMock.mockResolvedValue([
      { ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101 (Beatmaking 101)' },
    ]);
    listRegistrationsMock.mockResolvedValue([]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const emptyState = container.querySelector<HTMLElement>('[data-testid="course-registration-initial-empty-state"]');
      const publicFormAction = emptyState?.querySelector<HTMLAnchorElement>('a[href="/inscripcion/beatmaking-101"]');

      expect(emptyState).not.toBeNull();
      expect(emptyState?.textContent).toContain(singleCohortInitialEmptyStateMessage);
      expect(emptyState?.textContent).not.toContain('Beatmaking 101 (Beatmaking 101)');
      expect(countOccurrences(emptyState!, 'Beatmaking 101')).toBe(1);
      expect(publicFormAction?.getAttribute('aria-label')).toBe('Abrir formulario público de Beatmaking 101');
      expect(publicFormAction?.getAttribute('title')).toBe(
        'Abrir formulario público de Beatmaking 101 en una pestaña nueva',
      );
      expect(emptyState?.querySelectorAll('a')).toHaveLength(1);
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

  it('compacts repeated payment-status row actions once the default pending list gets busy', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
      buildRegistration({
        crId: 102,
        crFullName: 'Grace Hopper',
        crEmail: 'grace@example.com',
      }),
      buildRegistration({
        crId: 103,
        crFullName: 'Katherine Johnson',
        crEmail: 'katherine@example.com',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const currentViewContext = container.querySelector<HTMLElement>(
        '[data-testid="course-registration-single-choice-context"]',
      );
      expect(currentViewContext?.textContent).toContain('Mostrando 3 inscripciones en esta vista.');
      expect(currentViewContext?.textContent).toContain(paymentWorkflowDossierScopeHint);
      expect(countButtonsByText(container, paymentStatusMenuButtonLabel)).toBe(0);
      expect(countButtonsByText(container, openPaymentWorkflowLabel)).toBe(0);
      expect(container.querySelectorAll('button[aria-label^="Registrar pago o cambiar estado para "]')).toHaveLength(3);

      const firstStatusAction = getButtonByAriaLabel(
        container,
        'Registrar pago o cambiar estado para Ada Lovelace',
      );
      expect(firstStatusAction.textContent?.trim()).toBe('');
      expect(firstStatusAction.getAttribute('aria-haspopup')).toBe('menu');
      expect(firstStatusAction.getAttribute('title')).toBe(
        'Registrar pago o cambiar estado para Ada Lovelace; actual: Pendiente de pago',
      );
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Registrar pago o cambiar estado para Ada Lovelace'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(
        document.body.querySelector('[role="menuitem"][aria-label="Registrar pago para Ada Lovelace"]'),
      ).not.toBeNull();
      expect(getMenuItemByText(document.body, openPaymentWorkflowLabel)).toBeTruthy();
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
        container.querySelector<HTMLElement>('[data-testid="course-registration-single-choice-context"]')?.textContent,
      ).toContain('Mostrando 2 inscripciones en esta vista.');
      expect(
        container.querySelector<HTMLElement>('[data-testid="course-registration-single-choice-context"]')?.textContent,
      ).toContain(dossierLinkScopeHint);
      expect(
        container.querySelector<HTMLElement>('[data-testid="course-registration-single-choice-context"]')?.textContent,
      ).not.toContain(paymentWorkflowDossierScopeHint);
      expect(container.querySelector('[data-testid="course-registration-list-utilities"]')).toBeNull();
      expect(getButtonByAriaLabel(container, 'Abrir expediente de Ada Lovelace')).toBeTruthy();
      expect(countButtonsByText(container, paymentStatusMenuButtonLabel)).toBe(2);
      expect(countButtonsByText(container, openPaymentWorkflowLabel)).toBe(0);
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
