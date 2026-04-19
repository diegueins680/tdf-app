import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { CourseMetadata, CourseRegistrationRequest } from '../api/courses';

const getMetadataMock = jest.fn<(slug: string) => Promise<CourseMetadata>>();
const registerMock = jest.fn<
  (slug: string, payload: CourseRegistrationRequest) => Promise<{ id: number; status: string }>
>();

jest.unstable_mockModule('../api/courses', () => ({
  Courses: {
    getMetadata: (slug: string) => getMetadataMock(slug),
    register: (slug: string, payload: CourseRegistrationRequest) => registerMock(slug, payload),
  },
}));

jest.unstable_mockModule('../hooks/useCmsContent', () => ({
  useCmsContent: () => ({ data: undefined }),
}));

jest.unstable_mockModule('../components/PublicBrandBar', () => ({
  default: ({ tagline }: { tagline?: string }) => <div>{tagline}</div>,
}));

jest.unstable_mockModule('../components/EnrollmentSuccessDialog', () => ({
  default: ({ open }: { open: boolean }) => (open ? <div>Inscripcion recibida</div> : null),
}));

const { default: CourseProductionLandingPage } = await import('./CourseProductionLandingPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

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

const renderPage = async (container: HTMLElement, initialEntry: string) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <QueryClientProvider client={qc}>
        <MemoryRouter initialEntries={[initialEntry]}>
          <Routes>
            <Route path="/curso/:slug" element={<CourseProductionLandingPage />} />
          </Routes>
        </MemoryRouter>
      </QueryClientProvider>,
    );
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

describe('CourseProductionLandingPage', () => {
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
    getMetadataMock.mockReset();
    registerMock.mockReset();
    getMetadataMock.mockResolvedValue(buildMetadata());
    registerMock.mockResolvedValue({ id: 7, status: 'pending_payment' });
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

  it('submits public registrations to the selected generic course slug', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/curso/bateria-guillermo-diaz-abr-2026?utm_source=ig');

    try {
      await waitForExpectation(() => {
        expect(text(container)).toContain('Reserva tu cupo');
      });

      const inputs = Array.from(container.querySelectorAll<HTMLInputElement>('input'));
      const textarea = container.querySelector<HTMLTextAreaElement>('textarea');
      if (!inputs[0] || !inputs[1] || !inputs[2]) throw new Error('Expected registration inputs');
      await setInputValue(inputs[0], 'Ana Torres');
      await setInputValue(inputs[1], 'ana@example.com');
      await setInputValue(inputs[2], '+593999001122');
      if (textarea) await setInputValue(textarea, 'Instagram');

      const form = container.querySelector('form');
      if (!form) throw new Error('Registration form not found');

      await act(async () => {
        form.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(registerMock).toHaveBeenCalledWith('bateria-guillermo-diaz-abr-2026', {
          fullName: 'Ana Torres',
          email: 'ana@example.com',
          phoneE164: '+593999001122',
          source: 'landing',
          howHeard: 'Instagram',
          utm: { source: 'ig', medium: undefined, campaign: undefined, content: undefined },
        });
      });
    } finally {
      await cleanup();
    }
  });
});
