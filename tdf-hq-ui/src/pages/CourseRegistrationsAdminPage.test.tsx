import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { CourseCohortOptionDTO, CourseRegistrationDTO } from '../api/courses';

const listCohortsMock = jest.fn<() => Promise<CourseCohortOptionDTO[]>>();
const listRegistrationsMock = jest.fn<
  (params?: { slug?: string; status?: string; limit?: number }) => Promise<CourseRegistrationDTO[]>
>();

jest.unstable_mockModule('../api/courses', () => ({
  Courses: {
    listCohorts: () => listCohortsMock(),
    listRegistrations: (params?: { slug?: string; status?: string; limit?: number }) => listRegistrationsMock(params),
    getRegistrationDossier: jest.fn(() => Promise.resolve(null)),
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

const renderPage = async (container: HTMLElement) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter
        initialEntries={['/inscripciones-curso']}
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
    listCohortsMock.mockResolvedValue([{ ccSlug: 'beatmaking-101', ccTitle: 'Beatmaking 101' }]);
    listRegistrationsMock.mockResolvedValue([buildRegistration()]);
  });

  it('consolidates row details into the dossier and refetches when the limit changes', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(
        'Los filtros se aplican automáticamente al cambiar. Abre el expediente para gestionar notas, comprobantes, seguimiento y correos. Usa refrescar si necesitas volver a consultar.',
      );
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

    await cleanup();
  });
});
