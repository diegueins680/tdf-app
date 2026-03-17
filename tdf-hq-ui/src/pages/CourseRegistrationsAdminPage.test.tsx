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
  CourseRegistrationReceiptDTO,
} from '../api/courses';

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
const rowActionsHint = 'Abre el expediente para notas, comprobantes, seguimiento y correos. Haz clic en el estado actual para ver solo los cambios posibles.';

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

const countOccurrences = (root: ParentNode, text: string) =>
  (root.textContent ?? '').split(text).length - 1;

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

  it('consolidates row details into the dossier and refetches when the limit changes', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(
        'Los filtros se aplican automáticamente al cambiar. Empieza por cohorte y estado; usa Más filtros solo cuando necesites ajustar el tamaño del lote. Abre el expediente para gestionar notas, comprobantes, seguimiento y correos. Usa refrescar si necesitas volver a consultar.',
      );
      expect(container.textContent).toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).not.toContain('Slug: beatmaking-101');
      expect(container.textContent).not.toContain('Aplicar filtros');
      expect(container.textContent).toContain('Abrir expediente');
      expect(container.textContent).not.toContain('Ver correos');
      expect(hasLabel(container, 'Límite')).toBe(false);
      expect(getButtonByText(container, 'Más filtros')).toBeTruthy();
      expect(
        Array.from(container.querySelectorAll('button')).some((el) => {
          const label = (el.textContent ?? '').trim();
          return label === 'Copiar CSV filtrado' || label === 'Copiar CSV de esta vista';
        }),
      ).toBe(false);
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: undefined,
        limit: 200,
      });
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      clickButton(getButtonByText(container, 'Más filtros'));
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
      expect(listRegistrationsMock).toHaveBeenCalled();
      expect(listRegistrationsMock).toHaveBeenLastCalledWith({
        slug: undefined,
        status: undefined,
        limit: 50,
      });
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Abrir expediente'));
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Expediente de inscripción');
      expect(document.body.textContent).toContain('Ver correos');
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(
        'Todavía no hay comprobantes. Agrega el primero para documentar el pago y habilitar Marcar pagado.',
      );
      expect(document.body.textContent).not.toContain(
        'Sube un comprobante o pega una URL existente para habilitar Marcar pagado.',
      );
      expect(document.body.textContent).not.toContain('0 guardados');
      expect(getButtonByText(document.body, 'Agregar primer comprobante')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(1);
      expect(document.body.textContent).toContain(
        'Aún no hay seguimiento manual. Documenta llamadas, correos o próximos pasos desde aquí. Los cambios de estado y los comprobantes nuevos también quedarán registrados aquí.',
      );
      expect(document.body.textContent).not.toContain('0 entradas');
      expect(document.body.textContent).not.toContain('Registrar seguimiento');
      expect(document.body.textContent).not.toContain(
        'Abre el formulario solo cuando necesites documentar una llamada, correo o próximo paso.',
      );
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
      expect(document.body.textContent).not.toContain(
        'Aún no hay seguimiento manual. Documenta llamadas, correos o próximos pasos desde aquí. Los cambios de estado y los comprobantes nuevos también quedarán registrados aquí.',
      );
      expect(document.body.textContent).toContain(
        'Abre el formulario solo cuando necesites documentar una llamada, correo o próximo paso.',
      );
      expect(document.body.textContent).toContain(
        'Primero elige el archivo o pega un enlace; luego podras ajustar el nombre visible y las notas.',
      );
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(0);
      expect(hasLabel(document.body, 'Nombre visible')).toBe(false);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(false);
      expect(hasLabel(document.body, 'Asunto')).toBe(false);
      expect(hasLabel(document.body, 'Próximo seguimiento')).toBe(false);
      expect(document.body.textContent).toContain('Agrega asunto, recordatorio o evidencia solo si hacen falta.');
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

  it('keeps the limit filter inside advanced controls unless a custom limit is already active', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Límite')).toBe(false);
      expect(getButtonByText(container, 'Más filtros')).toBeTruthy();
      expect(container.textContent).not.toContain('Límite activo:');
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Más filtros'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Límite')).toBe(true);
      expect(getButtonByText(container, 'Ocultar filtros avanzados')).toBeTruthy();
      expect(container.textContent).toContain(
        'Máximo de filas a cargar en esta vista. Déjalo en 200 salvo que necesites revisar un lote distinto.',
      );
    });

    await cleanup();

    listRegistrationsMock.mockClear();

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const secondRender = await renderPage(secondContainer, '/inscripciones-curso?limit=50');

    await waitForExpectation(() => {
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: undefined,
        limit: 50,
      });
      expect(hasLabel(secondContainer, 'Límite')).toBe(true);
      expect(getButtonByText(secondContainer, 'Ocultar filtros avanzados')).toBeTruthy();
      expect(secondContainer.textContent).not.toContain('Límite activo: 50');
    });

    await secondRender.cleanup();
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
      expect(container.textContent).toContain('Vista filtrada: cohorte Beatmaking 101 (beatmaking-101).');
      expect(container.textContent).not.toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).not.toContain('Slug: beatmaking-101');
      expect(container.textContent).toContain('Fuente: landing');
    });

    await cleanup();
  });

  it('replaces a single cohort selector with context copy and restores it when multiple cohorts exist', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Curso / cohorte')).toBe(false);
      expect(container.textContent).toContain('Cohorte disponible');
      expect(container.textContent).toContain('Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).toContain('No hace falta filtrarla: es la unica cohorte disponible ahora mismo.');
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

  it('uses the course label inside the dossier instead of raw slug jargon', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Abrir expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Abrir expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Curso: Beatmaking 101 (beatmaking-101) · Fuente: landing');
      expect(document.body.textContent).not.toContain('Slug: beatmaking-101');
    });

    await cleanup();
  });

  it('summarizes a shared visible cohort once when the page still offers multiple cohort choices', async () => {
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
      expect(container.textContent).toContain('Mostrando una sola cohorte: Beatmaking 101 (beatmaking-101).');
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
      expect(container.textContent).toContain('Mostrando una sola fuente: landing.');
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
      expect(container.textContent).toContain('Todas las inscripciones visibles están sin fuente registrada.');
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

    listRegistrationsMock.mockImplementation(async (params) => {
      if (params?.status === 'paid') return [paidRegistration];
      if (params?.status === 'cancelled') return [cancelledRegistration];
      if (params?.status === 'pending_payment') return [pendingRegistration];
      return [pendingRegistration, paidRegistration, cancelledRegistration];
    });

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Estado')).toBe(false);
      expect(container.querySelectorAll('[aria-label^="Filtrar inscripciones por estado "]')).toHaveLength(4);
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
      expect(container.textContent).toContain('Grace Hopper');
      expect(container.textContent).not.toContain('Ada Lovelace');
      expect(container.textContent).not.toContain('Katherine Johnson');
      expect(container.textContent).toContain('Vista filtrada: estado pagado.');
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
      expect(container.textContent).toContain('Ada Lovelace');
      expect(container.textContent).toContain('Grace Hopper');
      expect(container.textContent).toContain('Katherine Johnson');
      expect(container.textContent).not.toContain('Vista filtrada:');
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
      expect(container.textContent).toContain('No hace falta filtrarlo: es el unico estado presente en esta vista.');
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
      expect(container.textContent).toContain(rowActionsHint);
      expect(countOccurrences(container, rowActionsHint)).toBe(1);
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(3);
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').textContent?.trim()).toBe('Pendiente de pago');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper').textContent?.trim()).toBe('Pagado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Katherine Johnson').textContent?.trim()).toBe('Cancelado');
      expect(getButtonByAriaLabel(container, 'Cambiar estado para Ada Lovelace').getAttribute('aria-haspopup')).toBe('menu');
      expect(container.textContent).not.toContain('Cambiar estado:');
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
      expect(document.body.textContent).toContain('Subir comprobante para marcar pagado');
      expect(document.body.textContent).toContain('Cancelar inscripción');
      expect(document.body.textContent).not.toContain('Marcar pendiente');
      expect(document.body.textContent).not.toContain('Estado actual:');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Marcar pendiente');
      expect(document.body.textContent).toContain('Cancelar inscripción');
      expect(document.body.textContent).not.toContain('Subir comprobante para marcar pagado');
      expect(document.body.textContent).not.toContain('Estado actual:');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Katherine Johnson'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Subir comprobante para marcar pagado');
      expect(document.body.textContent).toContain('Marcar pendiente');
      expect(document.body.textContent).not.toContain('Cancelar inscripción');
      expect(document.body.textContent).not.toContain('Estado actual:');
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
      expect(getButtonByText(container, 'Abrir expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Abrir expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Ver correos')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Cerrar')).toBe(1);
      expect(document.body.textContent).not.toContain(
        'Historial persistente por inscripción. Usa el refresco del expediente para volver a consultarlo.',
      );
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Ver correos'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(listRegistrationEmailsMock).toHaveBeenCalledWith(101, 200);
      expect(getButtonByText(document.body, 'Ocultar correos')).toBeTruthy();
      expect(getButtonByAriaLabel(document.body, 'Refrescar expediente y correos')).toBeTruthy();
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Actualizar correos',
        ),
      ).toBe(false);
      expect(document.body.textContent).toContain(
        'Historial persistente por inscripción. Usa el refresco del expediente para volver a consultarlo.',
      );
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

  it('uses a scoped dossier refresh control instead of a generic footer action', async () => {
    getRegistrationDossierMock.mockResolvedValue(buildDossier());

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Abrir expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Abrir expediente'));
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

  it('opens the receipt composer directly from the mark-paid flow without duplicating the hint', async () => {
    const markPaidReceiptHint = 'Sube un comprobante o pega una URL existente para habilitar Marcar pagado.';
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
      expect(getMenuItemByText(document.body, 'Subir comprobante para marcar pagado')).toBeTruthy();
    });

    await act(async () => {
      clickElement(getMenuItemByText(document.body, 'Subir comprobante para marcar pagado'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(markPaidReceiptHint);
      expect(countOccurrences(document.body, markPaidReceiptHint)).toBe(1);
      expect(hasLabel(document.body, 'Nombre visible')).toBe(true);
      expect(hasLabel(document.body, 'Notas del comprobante')).toBe(true);
      expect(getButtonByText(document.body, 'Guardar comprobante')).toBeTruthy();
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Agregar comprobante',
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
      expect(getButtonByText(container, 'Abrir expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Abrir expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(
        'Aún no hay notas internas. Ábrelas solo cuando necesites dejar contexto, acuerdos o próximos pasos.',
      );
      expect(hasLabel(document.body, 'Notas internas')).toBe(false);
      expect(getButtonByText(document.body, 'Agregar notas')).toBeTruthy();
      expect(
        Array.from(document.body.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Guardar notas',
        ),
      ).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Agregar notas'));
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
      expect(getButtonByText(container, 'Abrir expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Abrir expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Registrar primer seguimiento')).toBeTruthy();
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(1);
      expect(document.body.textContent).toContain(
        'Aún no hay seguimiento manual. Documenta llamadas, correos o próximos pasos desde aquí. Los cambios de estado y los comprobantes nuevos también quedarán registrados aquí.',
      );
      expect(document.body.textContent).not.toContain('0 entradas');
      expect(document.body.textContent).not.toContain('Registrar seguimiento');
      expect(document.body.textContent).not.toContain(
        'Abre el formulario solo cuando necesites documentar una llamada, correo o próximo paso.',
      );
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
      expect(document.body.textContent).not.toContain(
        'Aún no hay seguimiento manual. Documenta llamadas, correos o próximos pasos desde aquí. Los cambios de estado y los comprobantes nuevos también quedarán registrados aquí.',
      );
      expect(document.body.textContent).toContain(
        'Abre el formulario solo cuando necesites documentar una llamada, correo o próximo paso.',
      );
      expect(countButtonsByText(document.body, 'Registrar primer seguimiento')).toBe(0);
      expect(hasLabel(document.body, 'Tipo')).toBe(true);
      expect(hasLabel(document.body, 'Nota de seguimiento')).toBe(true);
      expect(hasLabel(document.body, 'Asunto')).toBe(false);
      expect(hasLabel(document.body, 'Próximo seguimiento')).toBe(false);
      expect(document.body.textContent).toContain('Agrega asunto, recordatorio o evidencia solo si hacen falta.');
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
      expect(hasLabel(document.body, 'Asunto')).toBe(true);
      expect(hasLabel(document.body, 'Próximo seguimiento')).toBe(true);
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir adjunto')).toBeTruthy();
    });

    await cleanup();
  });

  it('shows the mark-paid action only when the dossier can actually use it', async () => {
    const markPaidReceiptHint = 'Sube un comprobante o pega una URL existente para habilitar Marcar pagado.';
    const emptyReceiptHelpText = 'Todavía no hay comprobantes. Agrega el primero para documentar el pago y habilitar Marcar pagado.';
    const registration = buildRegistration();
    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({ crdRegistration: registration, crdCanMarkPaid: false }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Abrir expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Abrir expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain(emptyReceiptHelpText);
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
      expect(getButtonByText(secondContainer, 'Abrir expediente')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(secondContainer, 'Abrir expediente'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Marcar pagado')).toBeTruthy();
      expect(document.body.textContent).not.toContain(markPaidReceiptHint);
    });

    await secondRender.cleanup();
  });

  it('shows when the list is filtered and lets admins reset filters in one step', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).not.toContain('Vista filtrada:');
      expect(Array.from(container.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Restablecer filtros')).toBe(false);
    });

    listRegistrationsMock.mockClear();

    await act(async () => {
      clickButton(getButtonByText(container, 'Más filtros'));
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
      expect(container.textContent).toContain('Vista filtrada: límite 50.');
      expect(container.textContent).not.toContain('Límite activo: 50');
      expect(getButtonByText(container, 'Restablecer filtros')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Ocultar filtros avanzados'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Más filtros')).toBeTruthy();
      expect(container.textContent).toContain('Vista filtrada: límite 50.');
      expect(container.textContent).not.toContain('Límite activo: 50');
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
      expect(container.textContent).not.toContain('Vista filtrada:');
      expect(getButtonByText(container, 'Más filtros')).toBeTruthy();
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
        'No hay inscripciones con los filtros actuales: cohorte Beatmaking 101 (beatmaking-101) · estado pagado · límite 50. Restablece filtros o usa refrescar si esperabas resultados.',
      );
      expect(container.textContent).not.toContain('Vista filtrada:');
      expect(container.textContent).not.toContain('No hay inscripciones para esta vista.');
      expect(countButtonsByText(container, 'Restablecer filtros')).toBe(1);
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
      expect(container.textContent).toContain('No hay inscripciones para esta vista.');
      expect(container.textContent).not.toContain('No hay inscripciones con los filtros actuales:');
      expect(Array.from(container.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Restablecer filtros')).toBe(false);
    });

    await cleanup();
  });

  it('keeps the header comparative by hiding redundant single-status totals', async () => {
    listRegistrationsMock.mockResolvedValue([
      buildRegistration(),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Total: 1');
      expect(container.textContent).not.toContain('Pendientes: 1');
      expect(container.textContent).not.toContain('Pagadas: 0');
      expect(container.textContent).not.toContain('Canceladas: 0');
      expect(container.textContent).toContain('Estado disponible');
      expect(container.textContent).toContain('Pendiente de pago');
      expect(container.textContent).not.toContain(
        'Los totales de arriba resumen esta vista y usan los mismos colores que cada estado.',
      );
    });

    await cleanup();
  });

  it('keeps CSV export focused on real list views and labels filtered exports clearly', async () => {
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
      expect(container.textContent).toContain('Total: 3');
      expect(container.textContent).toContain('Pagadas: 1');
      expect(container.textContent).toContain('Pendientes: 1');
      expect(container.textContent).toContain('Canceladas: 1');
      expect(container.textContent).toContain(
        'Los totales de arriba resumen esta vista y usan los mismos colores que cada estado.',
      );
      expect(container.textContent).not.toContain('Leyenda de estados:');
      expect(getButtonByText(container, 'Copiar CSV de esta vista')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(container, 'Más filtros'));
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
      expect(getButtonByText(container, 'Copiar CSV filtrado')).toBeTruthy();
      expect(container.textContent).toContain('Vista filtrada: límite 50.');
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
        'Los filtros se aplican automáticamente al cambiar. Empieza por cohorte y estado; usa Más filtros solo cuando necesites ajustar el tamaño del lote. Ajusta la vista o usa refrescar si esperabas resultados.',
      );
      expect(container.textContent).toContain('No hay inscripciones para esta vista.');
      expect(container.textContent).not.toContain(rowActionsHint);
      expect(container.textContent).not.toContain('Leyenda de estados:');
      expect(container.textContent).not.toContain('Total: 0');
      expect(container.textContent).not.toContain('Pagadas: 0');
      expect(container.textContent).not.toContain('Pendientes: 0');
      expect(container.textContent).not.toContain('Canceladas: 0');
      expect(
        Array.from(container.querySelectorAll('button')).some(
          (el) => (el.textContent ?? '').trim() === 'Copiar CSV filtrado',
        ),
      ).toBe(false);
      expect(getButtonByAriaLabel(container, 'Refrescar inscripciones')).toBeTruthy();
    });

    await cleanup();
  });
});
