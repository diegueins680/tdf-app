import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { CmsContentDTO } from '../api/cms';

jest.setTimeout(15000);

const listMock = jest.fn<(params?: { slug?: string; locale?: string }) => Promise<CmsContentDTO[]>>();
const getPublicMock = jest.fn<(slug: string, locale?: string) => Promise<CmsContentDTO>>();

jest.unstable_mockModule('../api/cms', () => ({
  Cms: {
    list: (params?: { slug?: string; locale?: string }) => listMock(params),
    getPublic: (slug: string, locale?: string) => getPublicMock(slug, locale),
    create: jest.fn(() => Promise.resolve(null)),
    publish: jest.fn(() => Promise.resolve(null)),
    remove: jest.fn(() => Promise.resolve(null)),
  },
}));

jest.unstable_mockModule('../components/ApiErrorNotice', () => ({
  default: () => null,
}));

jest.unstable_mockModule('../components/SessionGate', () => ({
  SessionGate: ({ children }: { children: React.ReactNode }) => <>{children}</>,
}));

const { default: CmsAdminPage } = await import('./CmsAdminPage');

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

const buildContent = (overrides: Partial<CmsContentDTO> = {}): CmsContentDTO => ({
  ccdId: 101,
  ccdSlug: 'records-public',
  ccdLocale: 'es',
  ccdVersion: 4,
  ccdStatus: 'published',
  ccdTitle: 'Landing principal',
  ccdPayload: { heroTitle: 'Lanzamientos destacados' },
  ccdCreatedAt: '2030-01-02T03:04:05.000Z',
  ccdPublishedAt: '2030-01-03T03:04:05.000Z',
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
        initialEntries={['/cms/admin']}
        future={{ v7_startTransition: true, v7_relativeSplatPath: true }}
      >
        <QueryClientProvider client={qc}>
          <CmsAdminPage />
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
  const label = labels.find((candidate) => {
    const text = (candidate.textContent ?? '').replace('*', '').trim();
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

const setInputValue = (input: HTMLInputElement | HTMLTextAreaElement, value: string) => {
  const prototype =
    input instanceof HTMLTextAreaElement ? HTMLTextAreaElement.prototype : HTMLInputElement.prototype;
  const descriptor = Object.getOwnPropertyDescriptor(prototype, 'value');
  if (descriptor?.set) {
    descriptor.set.call(input, value);
  } else {
    input.value = value;
  }
  input.dispatchEvent(new Event('input', { bubbles: true }));
  input.dispatchEvent(new Event('change', { bubbles: true }));
};

const countActionsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('button,a')).filter((el) => (el.textContent ?? '').trim() === labelText).length;

const countExactText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('*')).filter((el) => (el.textContent ?? '').trim() === labelText).length;

const countLabelsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('label')).filter((el) => (el.textContent ?? '').replace('*', '').trim() === labelText).length;

const getButtonByText = (root: ParentNode, labelText: string) => {
  const button = Array.from(root.querySelectorAll('button')).find((el) => (el.textContent ?? '').trim() === labelText);
  if (!(button instanceof HTMLButtonElement)) {
    throw new Error(`Button not found: ${labelText}`);
  }
  return button;
};

describe('CmsAdminPage', () => {
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
    listMock.mockReset();
    getPublicMock.mockReset();
    window.localStorage.clear();

    listMock.mockResolvedValue([buildContent(), buildContent({
      ccdId: 102,
      ccdVersion: 3,
      ccdStatus: 'draft',
      ccdPublishedAt: null,
    })]);
    getPublicMock.mockResolvedValue(buildContent());
  });

  it('replaces the manual local-save action with autosave guidance that matches the persisted draft behavior', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(
        'El borrador se guarda automáticamente en este navegador por slug y locale mientras editas.',
      );
      expect(container.textContent).not.toContain('Guardar borrador local');
    });

    const titleInput = getInputByLabel(container, 'Título');
    const payloadInput = getInputByLabel(container, 'Payload JSON');
    const updatedPayload = JSON.stringify({ heroTitle: 'Landing actualizada' }, null, 2);

    await act(async () => {
      setInputValue(titleInput, 'Landing actualizada');
      setInputValue(payloadInput, updatedPayload);
      await new Promise((resolve) => setTimeout(resolve, 450));
    });

    await waitForExpectation(() => {
      expect(window.localStorage.getItem('tdf-cms-admin:draft:records-public:es')).toBe(
        JSON.stringify({
          title: 'Landing actualizada',
          payload: updatedPayload,
          status: 'draft',
        }),
      );
    });

    await cleanup();
  });

  it('keeps the live-page action in a single primary place instead of duplicating it in the live summary card', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Abrir página en vivo')).toBe(1);
      expect(container.textContent).not.toContain('Ver en vivo');
      expect(container.textContent).toContain('La página pública se abre con el botón principal de arriba.');
      expect(countActionsByText(container, 'Editar en formulario')).toBe(2);
    });

    await cleanup();
  });

  it('keeps a single live-to-editor action in the editor instead of repeating load-live buttons across the page', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
      expect(countActionsByText(container, 'Cargar en formulario')).toBe(0);
      expect(container.textContent).not.toContain('Cargar última publicada');
      expect(container.textContent).not.toContain('Revertir a en vivo');
      expect(container.textContent).toContain(
        'Para editar lo publicado, usa el botón del editor para traer la versión en vivo.',
      );
    });

    await cleanup();
  });

  it('replaces the duplicate payload preview grid with one compare hint inside the editor', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Payload JSON');
      expect(container.textContent).toContain('Payload actual');
      expect(container.textContent).not.toContain('Payload (borrador)');
      expect(container.textContent).not.toContain('Payload en vivo');
      expect(container.textContent).toContain(
        'El payload editable está arriba. La versión en vivo ya se muestra en la columna izquierda; usa Comparar con live si necesitas revisar cambios línea por línea.',
      );
    });

    await cleanup();
  });

  it('shows shared slug and locale context once above the versions list instead of repeating them on each row', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Contexto compartido: slug records-public · locale es.');
      expect(countExactText(container, 'records-public')).toBe(1);
      expect(countExactText(container, 'es')).toBe(1);
      expect(countActionsByText(container, 'Editar en formulario')).toBe(2);
    });

    await cleanup();
  });

  it('keeps the loaded-version context beside save instead of repeating it in the versions header', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Editar en formulario')).toBe(2);
    });

    await act(async () => {
      getButtonByText(container, 'Editar en formulario').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Cargar versión en el formulario');
      expect(getButtonByText(document.body, 'Cargar en formulario')).toBeTruthy();
    });

    await act(async () => {
      getButtonByText(document.body, 'Cargar en formulario').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const pageText = container.textContent ?? '';
      expect(pageText).toContain('Base: v4 · ID 101');
      expect(pageText.split('Base: v4 · ID 101').length - 1).toBe(1);
      expect(pageText).not.toContain('Editando desde ID');
    });

    await cleanup();
  });

  it('hides version filters until the CMS history has enough entries to compare', async () => {
    listMock.mockResolvedValue([buildContent()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(
        'Los filtros aparecerán cuando exista más historial para comparar versiones.',
      );
      expect(countLabelsByText(container, 'Estado')).toBe(1);
      expect(countLabelsByText(container, 'Versión mínima')).toBe(0);
      expect(countActionsByText(container, 'Limpiar filtros')).toBe(0);
    });

    await cleanup();
  });
});
