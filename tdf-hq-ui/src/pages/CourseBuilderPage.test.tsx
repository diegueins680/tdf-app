import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import type { ReactNode } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import type { CourseMetadata, CourseUpsert } from '../api/courses';

const getMetadataMock = jest.fn<(slug: string) => Promise<CourseMetadata>>();
const upsertMock = jest.fn<(payload: CourseUpsert) => Promise<CourseMetadata>>();

jest.unstable_mockModule('@mui/x-date-pickers/LocalizationProvider', () => ({
  LocalizationProvider: ({ children }: { children: ReactNode }) => <>{children}</>,
}));

jest.unstable_mockModule('@mui/x-date-pickers/AdapterLuxon', () => ({
  AdapterLuxon: class AdapterLuxon {},
}));

jest.unstable_mockModule('@mui/x-date-pickers/DatePicker', () => ({
  DatePicker: ({
    label,
    value,
  }: {
    label: string;
    value: { toISODate?: () => string | null } | null;
  }) => (
    <label>
      {label}
      <input aria-label={label} value={value?.toISODate?.() ?? ''} readOnly />
    </label>
  ),
}));

jest.unstable_mockModule('../api/courses', () => ({
  Courses: {
    getMetadata: (slug: string) => getMetadataMock(slug),
    upsert: (payload: CourseUpsert) => upsertMock(payload),
  },
}));

const { default: CourseBuilderPage } = await import('./CourseBuilderPage');

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

const buildMetadata = (overrides: Partial<CourseMetadata> = {}): CourseMetadata => ({
  slug: 'produccion-musical-abr-2026',
  title: 'Curso de Producción Musical',
  subtitle: 'Presencial',
  format: 'Presencial',
  duration: 'Cuatro sábados',
  price: 150,
  currency: 'USD',
  capacity: 16,
  remaining: 16,
  locationLabel: 'TDF Records',
  locationMapUrl: '',
  whatsappCtaUrl: '',
  landingUrl: '',
  daws: ['Logic'],
  includes: ['Certificado'],
  sessions: [{ label: 'Sábado 1', date: '2030-05-02' }],
  syllabus: [{ title: 'Introducción', topics: ['Conceptos básicos'] }],
  sessionStartHour: 15,
  sessionDurationHours: 4,
  instructorName: 'Esteban Muñoz',
  instructorBio: 'Productor en TDF Records.',
  instructorAvatarUrl: '',
  ...overrides,
} as CourseMetadata);

const renderPage = async (container: HTMLElement) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <QueryClientProvider client={qc}>
        <CourseBuilderPage />
      </QueryClientProvider>,
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

const text = (element: Element | null | undefined) => (element?.textContent ?? '').replace(/\s+/g, ' ').trim();

const getButtonByText = (root: ParentNode, labelText: string) => {
  const button = Array.from(root.querySelectorAll<HTMLElement>('button, [role="button"]')).find(
    (element) => text(element) === labelText,
  );
  if (!(button instanceof HTMLElement)) throw new Error(`Button not found: ${labelText}`);
  return button;
};

const clickButton = async (button: HTMLElement) => {
  await act(async () => {
    button.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
  });
};

describe('CourseBuilderPage', () => {
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
    window.localStorage.clear();
    getMetadataMock.mockReset();
    upsertMock.mockReset();
    getMetadataMock.mockResolvedValue(buildMetadata());
    upsertMock.mockResolvedValue(buildMetadata());
  });

  it('keeps the course-builder shortcuts aligned with their sections and avoids repeating the slug in the header', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);
    const scrolledSectionIds: string[] = [];
    const originalScrollIntoView = HTMLElement.prototype.scrollIntoView;
    HTMLElement.prototype.scrollIntoView = function scrollIntoView() {
      scrolledSectionIds.push(this.id);
    };

    try {
      await waitForExpectation(() => {
        expect(text(container)).toContain('Crear curso');
        expect(text(container)).not.toContain('Slug:');
        expect(text(document.getElementById('detalles'))).toContain('Slug (auto)');
        expect(text(document.getElementById('detalles'))).toContain('Instructor principal');
        expect(text(document.getElementById('detalles'))).not.toContain('Cargar curso existente');
        expect(text(document.getElementById('sesiones'))).toContain('Sesiones');
        expect(text(document.getElementById('sesiones'))).toContain('Añadir sesión');
        expect(text(document.getElementById('temario'))).toContain('Temario');
        expect(text(document.getElementById('temario'))).toContain('Añadir sección');
        expect(text(document.getElementById('publicacion'))).toContain('Revisar y publicar');
        expect(text(document.getElementById('publicacion'))).toContain('Publicar curso');
      });

      await clickButton(getButtonByText(container, 'Detalles'));
      await clickButton(getButtonByText(container, 'Sesiones'));
      await clickButton(getButtonByText(container, 'Temario'));
      await clickButton(getButtonByText(container, 'Publicación'));

      expect(scrolledSectionIds).toEqual(['detalles', 'sesiones', 'temario', 'publicacion']);
    } finally {
      if (originalScrollIntoView) {
        HTMLElement.prototype.scrollIntoView = originalScrollIntoView;
      } else {
        delete (HTMLElement.prototype as { scrollIntoView?: Element['scrollIntoView'] }).scrollIntoView;
      }
      await cleanup();
    }
  });
});
