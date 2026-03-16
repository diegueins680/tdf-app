import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type {
  CourseCohortOptionDTO,
  CourseRegistrationDTO,
  CourseRegistrationDossierDTO,
} from '../api/courses';

const listCohortsMock = jest.fn<() => Promise<CourseCohortOptionDTO[]>>();
const listRegistrationsMock = jest.fn<
  (params?: { slug?: string; status?: string; limit?: number }) => Promise<CourseRegistrationDTO[]>
>();
const getRegistrationDossierMock = jest.fn<
  (slug: string, registrationId: number) => Promise<CourseRegistrationDossierDTO | null>
>();

jest.unstable_mockModule('../api/courses', () => ({
  Courses: {
    listCohorts: () => listCohortsMock(),
    listRegistrations: (params?: { slug?: string; status?: string; limit?: number }) => listRegistrationsMock(params),
    getRegistrationDossier: (slug: string, registrationId: number) =>
      getRegistrationDossierMock(slug, registrationId),
    listRegistrationEmails: jest.fn(() => Promise.resolve([])),
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

const buildDossier = (
  overrides: Partial<CourseRegistrationDossierDTO> = {},
): CourseRegistrationDossierDTO => ({
  crdRegistration: buildRegistration(),
  crdReceipts: [],
  crdFollowUps: [],
  crdCanMarkPaid: false,
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
    if (input instanceof HTMLInputElement) return input;
  }
  const fallback = label.parentElement?.querySelector<HTMLInputElement>('input,textarea');
  if (!fallback) throw new Error(`Input not found for label: ${labelText}`);
  return fallback;
};

const hasLabel = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('label')).some((el) => {
    const text = (el.textContent ?? '').replace('*', '').trim();
    return text === labelText;
  });

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

const clickButton = (button: HTMLButtonElement) => {
  button.dispatchEvent(new MouseEvent('click', { bubbles: true }));
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
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([buildRegistration()]);
    getRegistrationDossierMock.mockResolvedValue(null);
  });

  it('consolidates row details into the dossier and refetches when the limit changes', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(
        'Los filtros se aplican automáticamente al cambiar. Abre el expediente para gestionar notas, comprobantes, seguimiento y correos. Usa refrescar si necesitas volver a consultar.',
      );
      expect(container.textContent).toContain('Cohorte: Beatmaking 101 (beatmaking-101)');
      expect(container.textContent).not.toContain('Slug: beatmaking-101');
      expect(container.textContent).not.toContain('Aplicar filtros');
      expect(container.textContent).toContain('Abrir expediente');
      expect(container.textContent).not.toContain('Ver correos');
      expect(listRegistrationsMock).toHaveBeenCalledWith({
        slug: undefined,
        status: undefined,
        limit: 200,
      });
    });

    listRegistrationsMock.mockClear();

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
      expect(document.body.textContent).toContain('Usar enlace existente en lugar de subir archivo');
      expect(document.body.textContent).toContain('Agregar seguimiento');
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(false);
      expect(hasLabel(document.body, 'URL del adjunto')).toBe(false);
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Usar enlace existente en lugar de subir archivo'));
      clickButton(getButtonByText(document.body, 'Agregar seguimiento'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(document.body, 'Usar enlace existente en lugar de subir adjunto')).toBeTruthy();
    });

    await act(async () => {
      clickButton(getButtonByText(document.body, 'Usar enlace existente en lugar de subir adjunto'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasLabel(document.body, 'URL del comprobante')).toBe(true);
      expect(hasLabel(document.body, 'URL del adjunto')).toBe(true);
    });

    await cleanup();
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

  it('summarizes a shared cohort once when every visible row belongs to it', async () => {
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
      expect(container.textContent).toContain(
        'Cada fila concentra los cambios de estado en "Cambiar estado"; el menu solo muestra opciones válidas para esa inscripción.',
      );
      expect(container.querySelectorAll('button[aria-label^="Cambiar estado para "]')).toHaveLength(3);
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
      expect(document.body.textContent).toContain('Estado actual: Pendiente de pago');
      expect(document.body.textContent).toContain('Subir comprobante para marcar pagado');
      expect(document.body.textContent).toContain('Cancelar inscripción');
      expect(document.body.textContent).not.toContain('Marcar pendiente');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Grace Hopper'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Estado actual: Pagado');
      expect(document.body.textContent).toContain('Marcar pendiente');
      expect(document.body.textContent).toContain('Cancelar inscripción');
      expect(document.body.textContent).not.toContain('Subir comprobante para marcar pagado');
    });

    await act(async () => {
      clickButton(getButtonByAriaLabel(container, 'Cambiar estado para Katherine Johnson'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Estado actual: Cancelado');
      expect(document.body.textContent).toContain('Subir comprobante para marcar pagado');
      expect(document.body.textContent).toContain('Marcar pendiente');
      expect(document.body.textContent).not.toContain('Cancelar inscripción');
    });

    await cleanup();
  });

  it('keeps the follow-up composer collapsed until admins explicitly open it', async () => {
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
        'Abre el formulario solo cuando necesites documentar una llamada, correo o próximo paso.',
      );
      expect(getButtonByText(document.body, 'Agregar seguimiento')).toBeTruthy();
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
      expect(hasLabel(document.body, 'Tipo')).toBe(true);
      expect(hasLabel(document.body, 'Nota de seguimiento')).toBe(true);
      expect(getButtonByText(document.body, 'Guardar seguimiento')).toBeTruthy();
      expect(getButtonByText(document.body, 'Cancelar')).toBeTruthy();
    });

    await cleanup();
  });

  it('shows the mark-paid action only when the dossier can actually use it', async () => {
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
      expect(document.body.textContent).toContain('Sube un comprobante para habilitar Marcar pagado');
      expect(Array.from(document.body.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Marcar pagado')).toBe(false);
    });

    await cleanup();

    getRegistrationDossierMock.mockResolvedValue(
      buildDossier({ crdRegistration: registration, crdCanMarkPaid: true }),
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
      expect(document.body.textContent).not.toContain('Sube un comprobante para habilitar Marcar pagado');
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
      expect(container.textContent).not.toContain('Vista filtrada:');
      expect(Array.from(container.querySelectorAll('button')).some((el) => (el.textContent ?? '').trim() === 'Restablecer filtros')).toBe(false);
    });

    await cleanup();
  });
});
