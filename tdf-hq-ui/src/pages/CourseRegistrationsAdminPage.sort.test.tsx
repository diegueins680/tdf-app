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
  crReceiptCount: 0,
  crFollowUpCount: 0,
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

const hasLabel = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('label')).some((el) => {
    const text = (el.textContent ?? '').replace('*', '').trim();
    return text === labelText;
  });

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

const getPopupOptionByText = (root: ParentNode, labelText: string) => {
  const items = Array.from(root.querySelectorAll('[role="option"], [role="menuitem"]'));
  const item = items.find((el) => (el.textContent ?? '').trim() === labelText);
  if (!(item instanceof HTMLElement)) {
    throw new Error(`Popup option not found: ${labelText}`);
  }
  return item;
};

const clickElement = (element: Element) => {
  element.dispatchEvent(new MouseEvent('click', { bubbles: true }));
};

const mouseDownElement = (element: Element) => {
  element.dispatchEvent(new MouseEvent('mousedown', { bubbles: true }));
};

const selectComboboxOption = async (root: ParentNode, labelText: string, optionText: string) => {
  await act(async () => {
    mouseDownElement(getComboboxByLabel(root, labelText));
    await flushPromises();
    await flushPromises();
  });

  await act(async () => {
    clickElement(getPopupOptionByText(document.body, optionText));
    await flushPromises();
    await flushPromises();
  });
};

const getDossierTargetLabels = (root: ParentNode) =>
  Array.from(root.querySelectorAll<HTMLButtonElement>('button[aria-label^="Abrir expediente de "]')).map((button) => (
    button.getAttribute('aria-label') ?? ''
  ).replace(/^Abrir expediente de /, ''));

const getButtonByText = (root: ParentNode, labelText: string) => {
  const button = Array.from(root.querySelectorAll<HTMLButtonElement>('button')).find(
    (candidate) => (candidate.textContent ?? '').trim() === labelText,
  );
  if (!(button instanceof HTMLButtonElement)) {
    throw new Error(`Button not found: ${labelText}`);
  }
  return button;
};

describe('CourseRegistrationsAdminPage sorting', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
    jest.clearAllMocks();
    listCohortsMock.mockResolvedValue([]);
    getRegistrationDossierMock.mockResolvedValue(null);
    listRegistrationEmailsMock.mockResolvedValue([]);
  });

  it('orders loaded registrations by name or registration date and exports the ordered rows', async () => {
    const writeTextMock = jest.fn<(text: string) => Promise<void>>().mockResolvedValue(undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText: writeTextMock },
    });
    listRegistrationsMock.mockResolvedValue([
      buildRegistration({
        crId: 101,
        crFullName: 'Brenda Mora',
        crEmail: 'brenda@example.com',
        crCreatedAt: '2030-01-02T03:04:05.000Z',
      }),
      buildRegistration({
        crId: 102,
        crFullName: 'Ana Torres',
        crEmail: 'ana@example.com',
        crCreatedAt: '2030-01-03T03:04:05.000Z',
      }),
      buildRegistration({
        crId: 103,
        crFullName: 'Carlos Vega',
        crEmail: 'carlos@example.com',
        crCreatedAt: '2030-01-01T03:04:05.000Z',
      }),
      ...[
        'Diego Paz',
        'Elena Ruiz',
        'Fabio Leon',
        'Gabriela Sol',
        'Hugo Luna',
        'Iris Mar',
      ].map((name, index) => buildRegistration({
        crId: 104 + index,
        crPartyId: 12 + index,
        crFullName: name,
        crEmail: `${name.toLowerCase().replace(/\s+/g, '.')}@example.com`,
        crCreatedAt: `2030-01-${String(4 + index).padStart(2, '0')}T03:04:05.000Z`,
      })),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/inscripciones-curso?limit=9');

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Ordenar por')).toBe(true);
      expect(hasLabel(container, 'Dirección')).toBe(false);
      expect(getDossierTargetLabels(container).slice(0, 3)).toEqual([
        'Brenda Mora',
        'Ana Torres',
        'Carlos Vega',
      ]);
    });

    await selectComboboxOption(container, 'Ordenar por', 'Nombre');

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Dirección')).toBe(true);
      expect(getDossierTargetLabels(container).slice(0, 3)).toEqual([
        'Ana Torres',
        'Brenda Mora',
        'Carlos Vega',
      ]);
    });

    await selectComboboxOption(container, 'Ordenar por', 'Fecha de registro');

    await waitForExpectation(() => {
      expect(getDossierTargetLabels(container).slice(0, 3)).toEqual([
        'Iris Mar',
        'Hugo Luna',
        'Gabriela Sol',
      ]);
    });

    await selectComboboxOption(container, 'Dirección', 'Ascendente');

    await waitForExpectation(() => {
      expect(getDossierTargetLabels(container).slice(0, 3)).toEqual([
        'Carlos Vega',
        'Brenda Mora',
        'Ana Torres',
      ]);
      expect(getButtonByText(container, 'Copiar CSV (9 inscripciones)')).toBeTruthy();
    });

    await act(async () => {
      clickElement(getButtonByText(container, 'Copiar CSV (9 inscripciones)'));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(writeTextMock).toHaveBeenCalledTimes(1);
      const csvRows = (writeTextMock.mock.calls[0]?.[0] ?? '').split('\n');
      expect(csvRows[1]).toContain('"Carlos Vega"');
      expect(csvRows[2]).toContain('"Brenda Mora"');
      expect(csvRows[3]).toContain('"Ana Torres"');
    });

    await cleanup();
  });
});
