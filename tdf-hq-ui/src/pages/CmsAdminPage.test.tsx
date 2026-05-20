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
  default: ({ error, title, helper }: { error: unknown; title?: string; helper?: React.ReactNode }) => (
    <div>
      {title}
      {error instanceof Error ? `: ${error.message}` : ''}
      {helper}
    </div>
  ),
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
  Array.from(root.querySelectorAll('*')).filter((el) => {
    const text = (el.textContent ?? '').trim();
    const childHasText = Array.from(el.children).some((child) => (child.textContent ?? '').trim().length > 0);
    return text === labelText && !childHasText;
  }).length;

const countLabelsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('label')).filter((el) => (el.textContent ?? '').replace('*', '').trim() === labelText).length;

const getButtonByText = (root: ParentNode, labelText: string) => {
  const button = Array.from(root.querySelectorAll('button')).find((el) => (el.textContent ?? '').trim() === labelText);
  if (!(button instanceof HTMLButtonElement)) {
    throw new Error(`Button not found: ${labelText}`);
  }
  return button;
};

const normalizeElementText = (element: Element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim();

const selectComboboxOption = async (root: ParentNode, currentText: string, optionText: string) => {
  const combobox = Array.from(root.querySelectorAll<HTMLElement>('[role="combobox"]')).find(
    (element) => normalizeElementText(element) === currentText,
  );
  if (!combobox) throw new Error(`Combobox not found: ${currentText}`);

  await act(async () => {
    combobox.dispatchEvent(new MouseEvent('mousedown', { bubbles: true }));
    await flushPromises();
  });

  const option = Array.from(document.body.querySelectorAll<HTMLElement>('[role="option"]')).find(
    (element) => normalizeElementText(element) === optionText,
  );
  if (!option) throw new Error(`Combobox option not found: ${optionText}`);

  await act(async () => {
    option.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
    await flushPromises();
  });
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
        'El borrador se guarda automáticamente en este navegador por slug e idioma mientras editas.',
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

  it('labels CMS language context for admins instead of leaking locale jargon', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countLabelsByText(container, 'Idioma')).toBe(1);
      expect(countLabelsByText(container, 'Locale')).toBe(0);
      expect(container.textContent).toContain('Español (es)');
      expect(container.textContent).toContain(
        'Contexto compartido: título Landing principal · slug records-public · idioma Español (es).',
      );
      expect(container.textContent).not.toContain('slug y locale');
      expect(container.textContent).not.toContain('locale es');
    });

    await cleanup();
  });

  it('keeps the live-start action label on the button instead of repeating it in helper copy', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const guidance = container.querySelector<HTMLElement>('[data-testid="cms-admin-editor-guidance"]');
      expect(guidance).not.toBeNull();
      expect(guidance?.textContent?.trim()).toBe(
        'El borrador se guarda automáticamente en este navegador por slug e idioma mientras editas. El payload editable está arriba. Escribe tu propio JSON solo si vas a reemplazar la estructura publicada.',
      );
      expect(guidance?.textContent).not.toContain('Usar versión en vivo');
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
      expect(container.textContent).toContain(
        'Esta página ya tiene contenido publicado. Parte de la versión en vivo para mantener la estructura real antes de escribir JSON nuevo.',
      );
      expect(container.textContent).not.toContain(
        'Esta página ya tiene una versión en vivo. Usa "Usar versión en vivo" para traer la estructura real al editor.',
      );
    });

    await cleanup();
  });

  it('keeps the live-page action beside the live summary instead of pointing admins to another part of the page', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const liveCard = container.querySelector<HTMLElement>('[data-testid="cms-admin-live-content-card"]');
      expect(liveCard).not.toBeNull();
      expect(countActionsByText(container, 'Abrir página en vivo')).toBe(1);
      expect(countActionsByText(liveCard!, 'Abrir página en vivo')).toBe(1);
      expect(container.textContent).not.toContain('Ver en vivo');
      expect(container.textContent).not.toContain('La página pública se abre con el botón principal de arriba.');
      expect(countActionsByText(container, 'Editar en formulario')).toBe(1);
      expect(countExactText(container, 'En vivo')).toBe(1);
    });

    await cleanup();
  });

  it('keeps the current live status and publish timestamp in the live summary instead of repeating them in history', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Publicado:');
      expect(container.textContent).not.toContain('pub:');
      expect(countExactText(container, 'Publicado')).toBe(1);
      expect(countExactText(container, 'En vivo')).toBe(1);
    });

    await cleanup();
  });

  it('hides the destructive delete action from the current live version row', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countExactText(container, 'En vivo')).toBe(1);
      expect(countActionsByText(container, 'Borrar')).toBe(1);
    });

    await cleanup();
  });

  it('routes stale draft history through the editor instead of offering direct publish', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const history = container.querySelector<HTMLElement>('[data-testid="cms-admin-version-history"]');
      expect(history).not.toBeNull();
      expect(history?.textContent).toContain(
        'Las versiones anteriores a la versión en vivo se revisan en el formulario antes de publicarlas.',
      );
      expect(history?.querySelector('[data-testid="cms-admin-stale-version-publish-guidance"]')).not.toBeNull();
      expect(countActionsByText(history!, 'Publicar')).toBe(0);
      expect(countActionsByText(history!, 'Editar en formulario')).toBe(1);
      expect(countActionsByText(history!, 'Borrar')).toBe(1);
    });

    await cleanup();
  });

  it('keeps a current-live draft row passive instead of offering a duplicate publish action', async () => {
    listMock.mockResolvedValue([
      buildContent({
        ccdId: 201,
        ccdVersion: 5,
        ccdStatus: 'draft',
        ccdPublishedAt: null,
      }),
      buildContent({
        ccdId: 202,
        ccdVersion: 4,
        ccdStatus: 'published',
      }),
    ]);
    getPublicMock.mockResolvedValue(buildContent({
      ccdId: 201,
      ccdVersion: 5,
      ccdStatus: 'draft',
      ccdPublishedAt: null,
    }));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countExactText(container, 'En vivo')).toBe(1);
      expect(countActionsByText(container, 'Publicar')).toBe(0);
      expect(countActionsByText(container, 'Borrar')).toBe(1);
      expect(countActionsByText(container, 'Editar en formulario')).toBe(1);
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
        'Esta página ya tiene contenido publicado. Parte de la versión en vivo para mantener la estructura real antes de escribir JSON nuevo.',
      );
      expect(container.textContent).not.toContain('La página pública se abre con el botón principal de arriba.');
      expect(container.textContent).not.toContain('Para editar lo publicado');
    });

    await cleanup();
  });

  it('drops the generic example action when a live version already exists so the editor keeps one real starting point', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
      expect(countActionsByText(container, 'Cargar ejemplo')).toBe(0);
      expect(container.textContent).toContain(
        'Esta página ya tiene contenido publicado. Parte de la versión en vivo para mantener la estructura real antes de escribir JSON nuevo.',
      );
      expect(container.textContent).not.toContain(
        'Usa el botón "Cargar ejemplo" para ver la estructura sugerida del payload para este slug (no valida contra un esquema aún).',
      );
    });

    await cleanup();
  });

  it('hides the example action once the suggested payload is already loaded', async () => {
    listMock.mockResolvedValue([]);
    getPublicMock.mockImplementation(() => Promise.resolve(null as unknown as CmsContentDTO));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Cargar ejemplo')).toBe(1);
      expect(container.textContent).toContain(
        'Usa el botón "Cargar ejemplo" para ver la estructura sugerida del payload para este slug (no valida contra un esquema aún).',
      );
    });

    await act(async () => {
      getButtonByText(container, 'Cargar ejemplo').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Cargar ejemplo')).toBe(0);
      expect(getInputByLabel(container, 'Título').value).toBe('Lanzamientos destacados');
      expect(getInputByLabel(container, 'Payload JSON').value).toBe(
        JSON.stringify(
          {
            heroTitle: 'Lanzamientos destacados',
            heroSubtitle: 'Explora los releases recientes del sello.',
            locale: 'es',
          },
          null,
          2,
        ),
      );
      expect(container.textContent).toContain(
        'El ejemplo sugerido ya está cargado. Ajusta título y payload antes de guardar.',
      );
      expect(container.textContent).not.toContain(
        'Usa el botón "Cargar ejemplo" para ver la estructura sugerida del payload para este slug (no valida contra un esquema aún).',
      );
    });

    await cleanup();
  });

  it('keeps live lookup failure guidance in one place instead of repeating a generic editor alert', async () => {
    getPublicMock.mockRejectedValue(new Error('live unavailable'));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 1200));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getPublicMock).toHaveBeenCalledWith('records-public', 'es');
      expect(countActionsByText(container, 'Cargar ejemplo')).toBe(0);
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(0);
      expect(container.textContent).toContain(
        'No pudimos cargar el contenido publicado: live unavailable',
      );
      expect(countExactText(
        container,
        'Reintenta esta carga antes de partir de un ejemplo genérico.',
      )).toBe(1);
      expect(container.textContent).not.toContain(
        'No pudimos confirmar si ya existe una versión en vivo. Reintenta la carga en vivo antes de partir de un ejemplo.',
      );
      expect(container.textContent).not.toContain(
        'Usa el botón "Cargar ejemplo" para ver la estructura sugerida del payload para este slug (no valida contra un esquema aún).',
      );
    });

    await cleanup();
  });

  it('removes the live-start helper once the editor already matches live', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
      expect(countActionsByText(container, 'Guardar borrador')).toBe(0);
      expect(container.textContent).toContain(
        'Esta página ya tiene contenido publicado. Parte de la versión en vivo para mantener la estructura real antes de escribir JSON nuevo.',
      );
    });

    await act(async () => {
      getButtonByText(container, 'Usar versión en vivo').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(0);
      expect(countActionsByText(container, 'Guardar y publicar')).toBe(0);
      expect(countActionsByText(container, 'Guardar borrador')).toBe(0);
      expect(container.textContent).toContain('Editor coincide con versión en vivo');
      expect(container.textContent).toContain(
        'El payload editable ya coincide con la versión en vivo. El comparador aparecerá cuando vuelvas a modificarlo.',
      );
      expect(container.textContent).not.toContain(
        'Esta página ya tiene contenido publicado. Parte de la versión en vivo para mantener la estructura real antes de escribir JSON nuevo.',
      );
    });

    await cleanup();
  });

  it('keeps metadata-only edits from reopening live-start and payload-comparison guidance', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
    });

    await act(async () => {
      getButtonByText(container, 'Usar versión en vivo').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Editor coincide con versión en vivo');
      expect(container.textContent).not.toContain(
        'Esta página ya tiene contenido publicado. Parte de la versión en vivo para mantener la estructura real antes de escribir JSON nuevo.',
      );
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, 'Título'), 'Landing retitulada');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const guidance = container.querySelector<HTMLElement>('[data-testid="cms-admin-editor-guidance"]');
      expect(guidance).not.toBeNull();
      expect(guidance?.textContent?.trim()).toBe(
        'El borrador se guarda automáticamente en este navegador por slug e idioma mientras editas. El payload editable ya coincide con la versión en vivo. Vuelve a cargar la versión en vivo para descartar cambios de título o estado.',
      );
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
      expect(container.textContent).not.toContain(
        'Esta página ya tiene contenido publicado. Parte de la versión en vivo para mantener la estructura real antes de escribir JSON nuevo.',
      );
      expect(guidance?.textContent).not.toContain('El comparador aparecerá');
      expect(countActionsByText(container, 'Comparar cambios')).toBe(0);
    });

    await cleanup();
  });

  it('keeps the live payload behind the live-to-editor action until the editor uses it', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(
        'Carga la versión en vivo para editar la estructura publicada desde el formulario.',
      );
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
      expect(countActionsByText(container, 'Ver payload en vivo')).toBe(0);
      expect(countActionsByText(container, 'Ocultar payload en vivo')).toBe(0);
      expect(countLabelsByText(container, 'Payload actual')).toBe(0);
      expect(container.textContent).not.toContain('Payload en vivo disponible.');
    });

    await act(async () => {
      getButtonByText(container, 'Usar versión en vivo').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(container.textContent).toContain('El editor ya usa el payload en vivo.');
      expect(container.textContent).toContain('Editor coincide con versión en vivo');
      expect(countActionsByText(container, 'Ver payload en vivo')).toBe(0);
      expect(countActionsByText(container, 'Ocultar payload en vivo')).toBe(0);
      expect(countLabelsByText(container, 'Payload actual')).toBe(0);
    });

    await cleanup();
  });

  it('keeps live-and-loaded version state in one passive history label', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
      expect(countExactText(container, 'En vivo')).toBe(1);
      expect(countExactText(container, 'En formulario')).toBe(0);
      expect(countExactText(container, 'En vivo y en formulario')).toBe(0);
    });

    await act(async () => {
      getButtonByText(container, 'Usar versión en vivo').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(0);
      expect(countExactText(container, 'En vivo y en formulario')).toBe(1);
      expect(countExactText(container, 'En vivo')).toBe(0);
      expect(countExactText(container, 'En formulario')).toBe(0);
      expect(countActionsByText(container, 'Editar en formulario')).toBe(1);
    });

    await cleanup();
  });

  it('keeps the clear-payload fallback for new content until the editor has JSON to clear', async () => {
    getPublicMock.mockImplementation(() => Promise.resolve(null as unknown as CmsContentDTO));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Limpiar')).toBe(0);
      expect(getInputByLabel(container, 'Payload JSON').value.trim()).toBe('{}');
    });

    await act(async () => {
      setInputValue(
        getInputByLabel(container, 'Payload JSON'),
        JSON.stringify({ heroTitle: 'Landing actualizada' }, null, 2),
      );
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Limpiar')).toBe(1);
    });

    await act(async () => {
      getButtonByText(container, 'Limpiar').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getInputByLabel(container, 'Payload JSON').value.trim()).toBe('{}');
      expect(countActionsByText(container, 'Limpiar')).toBe(0);
    });

    await cleanup();
  });

  it('keeps one reset path after a custom new-page payload has started', async () => {
    listMock.mockResolvedValue([]);
    getPublicMock.mockImplementation(() => Promise.resolve(null as unknown as CmsContentDTO));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Cargar ejemplo')).toBe(1);
      expect(countActionsByText(container, 'Limpiar')).toBe(0);
      expect(container.textContent).toContain(
        'Usa el botón "Cargar ejemplo" para ver la estructura sugerida del payload para este slug (no valida contra un esquema aún).',
      );
    });

    await act(async () => {
      setInputValue(
        getInputByLabel(container, 'Payload JSON'),
        JSON.stringify({ heroTitle: 'Landing propia' }, null, 2),
      );
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Cargar ejemplo')).toBe(0);
      expect(countActionsByText(container, 'Limpiar')).toBe(1);
      expect(container.textContent).toContain(
        'Ya hay contenido en el editor. Usa "Limpiar" si quieres volver a partir de un ejemplo sugerido.',
      );
      expect(container.textContent).not.toContain(
        'Usa el botón "Cargar ejemplo" para ver la estructura sugerida del payload para este slug (no valida contra un esquema aún).',
      );
    });

    await cleanup();
  });

  it('hides the generic example action once a new-page title draft has started', async () => {
    listMock.mockResolvedValue([]);
    getPublicMock.mockImplementation(() => Promise.resolve(null as unknown as CmsContentDTO));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Cargar ejemplo')).toBe(1);
      expect(countActionsByText(container, 'Limpiar')).toBe(0);
      expect(countActionsByText(container, 'Guardar borrador')).toBe(0);
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, 'Título'), 'Landing propia');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Cargar ejemplo')).toBe(0);
      expect(countActionsByText(container, 'Limpiar')).toBe(1);
      expect(countActionsByText(container, 'Guardar borrador')).toBe(1);
      expect(container.textContent).toContain(
        'Ya hay contenido en el editor. Usa "Limpiar" si quieres volver a partir de un ejemplo sugerido.',
      );
      expect(container.textContent).not.toContain(
        'Usa el botón "Cargar ejemplo" para ver la estructura sugerida del payload para este slug (no valida contra un esquema aún).',
      );
    });

    await act(async () => {
      getButtonByText(container, 'Limpiar').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getInputByLabel(container, 'Título').value).toBe('');
      expect(getInputByLabel(container, 'Payload JSON').value.trim()).toBe('{}');
      expect(countActionsByText(container, 'Cargar ejemplo')).toBe(1);
      expect(countActionsByText(container, 'Limpiar')).toBe(0);
      expect(countActionsByText(container, 'Guardar borrador')).toBe(0);
    });

    await cleanup();
  });

  it('hides the generic clear action when a live version is available to restore instead', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
      expect(countActionsByText(container, 'Limpiar')).toBe(0);
    });

    await act(async () => {
      setInputValue(
        getInputByLabel(container, 'Payload JSON'),
        JSON.stringify({ heroTitle: 'Landing actualizada' }, null, 2),
      );
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
      expect(countActionsByText(container, 'Limpiar')).toBe(0);
      expect(container.textContent).toContain(
        'El payload editable está arriba. Usa Comparar cambios para revisar el borrador contra la versión en vivo sin abrir un segundo visor de payload.',
      );
    });

    await cleanup();
  });

  it('only shows the JSON format action when formatting would change the payload', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Formatear JSON')).toBe(0);
      expect(getInputByLabel(container, 'Payload JSON').value.trim()).toBe('{}');
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, 'Payload JSON'), '{"heroTitle":"Landing actualizada"}');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Formatear JSON')).toBe(1);
    });

    await act(async () => {
      getButtonByText(container, 'Formatear JSON').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getInputByLabel(container, 'Payload JSON').value).toBe(
        JSON.stringify({ heroTitle: 'Landing actualizada' }, null, 2),
      );
      expect(countActionsByText(container, 'Formatear JSON')).toBe(0);
    });

    await cleanup();
  });

  it('keeps invalid JSON recovery inline instead of allowing a save-alert detour', async () => {
    const alertMock = jest.spyOn(window, 'alert').mockImplementation(() => undefined);
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(countActionsByText(container, 'Guardar borrador')).toBe(0);
        expect(container.textContent).toContain(
          'Parte de la versión en vivo o empieza un borrador propio antes de guardar.',
        );
      });

      await act(async () => {
        setInputValue(getInputByLabel(container, 'Payload JSON'), '{"heroTitle":');
        await flushPromises();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(container.textContent).toContain('Error:');
        expect(getButtonByText(container, 'Guardar borrador').disabled).toBe(true);
        expect(countActionsByText(container, 'Formatear JSON')).toBe(0);
        expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
      });

      await act(async () => {
        getButtonByText(container, 'Guardar borrador').click();
        await flushPromises();
        await flushPromises();
      });

      expect(alertMock).not.toHaveBeenCalled();
    } finally {
      alertMock.mockRestore();
      await cleanup();
    }
  });

  it('keeps first-time live payload recovery to one visible action', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Payload JSON');
      expect(container.textContent).toContain(
        'Carga la versión en vivo para editar la estructura publicada desde el formulario.',
      );
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
      expect(countActionsByText(container, 'Guardar borrador')).toBe(0);
      expect(countActionsByText(container, 'Guardar y publicar')).toBe(0);
      expect(countActionsByText(container, 'Ver payload en vivo')).toBe(0);
      expect(countActionsByText(container, 'Ocultar payload en vivo')).toBe(0);
      expect(container.textContent).not.toContain('Payload actual');
      expect(countLabelsByText(container, 'Payload actual')).toBe(0);
      expect(container.textContent).not.toContain('Payload (borrador)');
      expect(container.textContent).toContain(
        'Parte de la versión en vivo o empieza un borrador propio antes de guardar.',
      );
      expect(container.textContent).toContain(
        'El payload editable está arriba. Escribe tu propio JSON solo si vas a reemplazar la estructura publicada.',
      );
    });

    await cleanup();
  });

  it('lets compare own changed-payload review instead of reopening the live payload inspector', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
      expect(countActionsByText(container, 'Comparar cambios')).toBe(0);
      expect(countActionsByText(container, 'Ver payload en vivo')).toBe(0);
      expect(container.textContent).not.toContain('Payload modificado vs en vivo');
      expect(container.textContent).toContain(
        'El payload editable está arriba. Escribe tu propio JSON solo si vas a reemplazar la estructura publicada.',
      );
    });

    await act(async () => {
      getButtonByText(container, 'Usar versión en vivo').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Comparar cambios')).toBe(0);
      expect(countActionsByText(container, 'Ver payload en vivo')).toBe(0);
      expect(container.textContent).not.toContain('Payload modificado vs en vivo');
      expect(container.textContent).toContain(
        'El payload editable ya coincide con la versión en vivo. El comparador aparecerá cuando vuelvas a modificarlo.',
      );
    });

    await act(async () => {
      setInputValue(
        getInputByLabel(container, 'Payload JSON'),
        JSON.stringify({ heroTitle: 'Landing actualizada' }, null, 2),
      );
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Comparar cambios')).toBe(1);
      expect(countActionsByText(container, 'Ver payload en vivo')).toBe(0);
      expect(countActionsByText(container, 'Ocultar payload en vivo')).toBe(0);
      expect(container.textContent).not.toContain('Payload modificado vs en vivo');
      const liveCard = container.querySelector<HTMLElement>('[data-testid="cms-admin-live-content-card"]');
      expect(liveCard?.textContent).toContain('El comparador revisa el payload en vivo contra este borrador.');
      expect(liveCard?.textContent).not.toContain('El editor ya usa el payload en vivo.');
      expect(container.textContent).toContain(
        'El payload editable está arriba. Usa Comparar cambios para revisar el borrador contra la versión en vivo sin abrir un segundo visor de payload.',
      );
    });

    await act(async () => {
      getButtonByText(container, 'Comparar cambios').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Comparar borrador con versión en vivo');
      expect(document.body.textContent).not.toContain('Comparar borrador vs. live');
    });

    await cleanup();
  });

  it('hides the example action for custom slugs and replaces it with custom-slug guidance', async () => {
    window.localStorage.setItem(
      'tdf-cms-admin:last-selection',
      JSON.stringify({ slug: 'promo-landing', locale: 'es' }),
    );
    listMock.mockResolvedValue([
      buildContent({ ccdSlug: 'promo-landing' }),
      buildContent({ ccdId: 102, ccdSlug: 'promo-landing', ccdVersion: 3, ccdStatus: 'draft', ccdPublishedAt: null }),
    ]);
    getPublicMock.mockResolvedValue(buildContent({ ccdSlug: 'promo-landing' }));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(listMock).toHaveBeenCalledWith({ slug: 'promo-landing', locale: 'es' });
      expect(getPublicMock).toHaveBeenCalledWith('promo-landing', 'es');
      expect(countActionsByText(container, 'Cargar ejemplo')).toBe(0);
      expect(container.textContent).toContain(
        'Este slug no tiene un ejemplo sugerido todavía. Empieza con tu propio JSON o trae la versión en vivo si ya existe.',
      );
      expect(container.textContent).toContain(
        'Estructura JSON del bloque (usa objetos/arrays). Para slugs nuevos, parte de tu propio JSON o trae la versión en vivo si ya existe.',
      );
    });

    await cleanup();
  });

  it('starts a different built-in slug from a blank editor when no local draft exists', async () => {
    listMock.mockImplementation((params) => Promise.resolve([
      buildContent({
        ccdSlug: params?.slug ?? 'records-public',
        ccdTitle: params?.slug === 'fan-hub' ? 'Fan Hub publicado' : 'Landing principal',
      }),
    ]));
    getPublicMock.mockImplementation((slug) => Promise.resolve(buildContent({
      ccdSlug: slug,
      ccdTitle: slug === 'fan-hub' ? 'Fan Hub publicado' : 'Landing principal',
      ccdPayload: {
        heroTitle: slug === 'fan-hub' ? 'Fans destacados' : 'Lanzamientos destacados',
      },
    })));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
    });

    await act(async () => {
      getButtonByText(container, 'Usar versión en vivo').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getInputByLabel(container, 'Título').value).toBe('Landing principal');
      expect(getInputByLabel(container, 'Payload JSON').value).toContain('Lanzamientos destacados');
      expect(container.textContent).toContain('Base: v4');
    });

    await selectComboboxOption(container, 'records-public', 'fan-hub');

    await waitForExpectation(() => {
      expect(listMock).toHaveBeenCalledWith({ slug: 'fan-hub', locale: 'es' });
      expect(getPublicMock).toHaveBeenCalledWith('fan-hub', 'es');
      expect(getInputByLabel(container, 'Título').value).toBe('');
      expect(getInputByLabel(container, 'Payload JSON').value.trim()).toBe('{}');
      expect(container.textContent).not.toContain('Base: v4');
      expect(countActionsByText(container, 'Guardar borrador')).toBe(0);
      expect(container.textContent).toContain(
        'Esta página ya tiene contenido publicado. Parte de la versión en vivo para mantener la estructura real antes de escribir JSON nuevo.',
      );
    });

    await cleanup();
  });

  it('replaces blank custom-slug dead ends with one helper and keeps save disabled until the slug exists', async () => {
    window.localStorage.setItem(
      'tdf-cms-admin:last-selection',
      JSON.stringify({ slug: '', locale: 'es' }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(listMock).not.toHaveBeenCalled();
      expect(getPublicMock).not.toHaveBeenCalled();
      expect(countActionsByText(container, 'Abrir página en vivo')).toBe(0);
      expect(container.textContent).toContain(
        'Completa este slug para habilitar el guardado y Abrir página en vivo.',
      );
      expect(container.textContent).toContain(
        'Elige un slug para consultar la versión publicada de esa página.',
      );
      expect(container.textContent).toContain('Completa el slug para habilitar el estado y el guardado.');
      expect(container.textContent).not.toContain('Sin contenido publicado');
      expect(container.querySelector('[data-testid="cms-admin-version-history"]')).toBeNull();
      expect(container.querySelector('[data-testid="cms-admin-first-version-history-guidance"]')).toBeNull();
      expect(countLabelsByText(container, 'Estado')).toBe(0);
      expect(countActionsByText(container, 'Guardar borrador')).toBe(0);
      expect(countActionsByText(container, 'Guardar y publicar')).toBe(0);
    });

    await cleanup();
  });

  it('hides the compare action until a live version exists so new slugs do not show a dead-end control', async () => {
    getPublicMock.mockImplementation(() => Promise.resolve(null as unknown as CmsContentDTO));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Sin contenido publicado');
      expect(container.textContent).toContain(
        'Publica una versión para activar la vista previa en vivo y el enlace a la página pública.',
      );
      expect(countActionsByText(container, 'Abrir página en vivo')).toBe(0);
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(0);
      expect(countActionsByText(container, 'Comparar cambios')).toBe(0);
      expect(container.textContent).toContain(
        'El payload editable está arriba. Cuando exista una versión en vivo, la verás en la columna izquierda, aparecerá el botón "Usar versión en vivo" y podrás compararla desde aquí.',
      );
    });

    await cleanup();
  });

  it('hides empty version history until a first save creates something to compare', async () => {
    listMock.mockResolvedValue([]);
    getPublicMock.mockImplementation(() => Promise.resolve(null as unknown as CmsContentDTO));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Sin contenido publicado');
      expect(container.querySelector('[data-testid="cms-admin-version-history"]')).toBeNull();
      expect(container.querySelector('[data-testid="cms-admin-first-version-history-guidance"]')).toBeNull();
      expect(container.textContent).not.toContain(
        'El historial de versiones aparecerá debajo de este editor cuando guardes la primera versión.',
      );
      expect(container.textContent).not.toContain('No hay versiones guardadas todavía.');
      expect(countLabelsByText(container, 'Estado del historial')).toBe(0);
      expect(countLabelsByText(container, 'Versión mínima')).toBe(0);
      expect(container.textContent).toContain(
        'Agrega un título o payload antes de guardar la primera versión.',
      );
    });

    await act(async () => {
      getButtonByText(container, 'Cargar ejemplo').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(container.querySelector('[data-testid="cms-admin-version-history"]')).toBeNull();
      expect(container.querySelector('[data-testid="cms-admin-first-version-history-guidance"]')).not.toBeNull();
      expect(container.textContent).toContain(
        'El historial de versiones aparecerá debajo de este editor cuando guardes la primera versión.',
      );
    });

    await cleanup();
  });

  it('hides the first-version save action until the editor has real content', async () => {
    listMock.mockResolvedValue([]);
    getPublicMock.mockImplementation(() => Promise.resolve(null as unknown as CmsContentDTO));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Sin contenido publicado');
      expect(countActionsByText(container, 'Guardar borrador')).toBe(0);
      expect(countLabelsByText(container, 'Estado')).toBe(0);
      expect(container.querySelector('[data-testid="cms-admin-first-version-save-guidance"]')).not.toBeNull();
      expect(container.textContent).toContain(
        'Agrega un título o payload antes de guardar la primera versión.',
      );
      expect(countActionsByText(container, 'Cargar ejemplo')).toBe(1);
    });

    await act(async () => {
      getButtonByText(container, 'Cargar ejemplo').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Guardar borrador')).toBe(1);
      expect(countLabelsByText(container, 'Estado')).toBe(1);
      expect(container.querySelector('[data-testid="cms-admin-first-version-save-guidance"]')).toBeNull();
      expect(container.textContent).toContain(
        'Guardará esta versión como borrador sin cambiar la página en vivo.',
      );
      expect(container.textContent).not.toContain(
        'Agrega un título o payload antes de guardar la primera versión.',
      );
    });

    await cleanup();
  });

  it('keeps first-version save controls hidden while the live lookup is still unresolved', async () => {
    listMock.mockResolvedValue([]);
    getPublicMock.mockImplementation(() => new Promise<CmsContentDTO>(() => undefined));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(listMock).toHaveBeenCalledWith({ slug: 'records-public', locale: 'es' });
      expect(getPublicMock).toHaveBeenCalledWith('records-public', 'es');
      expect(container.textContent).toContain(
        'Confirmando si ya existe una versión en vivo antes de mostrar ejemplos genéricos.',
      );
      expect(container.querySelector('[data-testid="cms-admin-first-version-save-guidance"]')).not.toBeNull();
      expect(container.textContent).toContain(
        'Agrega un título o payload antes de guardar la primera versión.',
      );
      expect(countActionsByText(container, 'Guardar borrador')).toBe(0);
      expect(countActionsByText(container, 'Guardar y publicar')).toBe(0);
      expect(countLabelsByText(container, 'Estado')).toBe(0);
      expect(countActionsByText(container, 'Cargar ejemplo')).toBe(0);
      expect(container.querySelector('[data-testid="cms-admin-version-history"]')).toBeNull();
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, 'Título'), 'Landing en revisión');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Guardar borrador')).toBe(0);
      expect(countActionsByText(container, 'Guardar y publicar')).toBe(0);
      expect(countLabelsByText(container, 'Estado')).toBe(0);
      expect(container.querySelector('[data-testid="cms-admin-version-history"]')).toBeNull();
      expect(container.textContent).toContain(
        'Espera a que termine la búsqueda en vivo antes de guardar la primera versión.',
      );
      expect(container.textContent).not.toContain(
        'Guardará esta versión como borrador sin cambiar la página en vivo.',
      );
    });

    await cleanup();
  });

  it('keeps version-history load failures separate from the first-version empty state', async () => {
    listMock.mockRejectedValue(new Error('versions unavailable'));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const history = container.querySelector<HTMLElement>('[data-testid="cms-admin-version-history"]');
      expect(history).not.toBeNull();
      expect(history?.textContent).toContain('No pudimos cargar la lista de versiones: versions unavailable');
      expect(history?.textContent).not.toContain('No hay versiones guardadas todavía.');
      expect(history?.textContent).not.toContain(
        'El historial de versiones aparecerá debajo de este editor cuando guardes la primera versión.',
      );
    });

    await cleanup();
  });

  it('keeps the load-version dialog single-column when nothing is published yet', async () => {
    listMock.mockResolvedValue([
      buildContent({ ccdId: 201, ccdVersion: 1, ccdStatus: 'draft', ccdPublishedAt: null }),
    ]);
    getPublicMock.mockImplementation(() => Promise.resolve(null as unknown as CmsContentDTO));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Editar en formulario')).toBe(1);
    });

    await act(async () => {
      getButtonByText(container, 'Editar en formulario').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Cargar versión en el formulario');
      expect(document.body.textContent).toContain('Live actual: no hay versión publicada');
      expect(document.body.textContent).toContain(
        'Todavía no hay una versión publicada. Revisa el payload que vas a cargar antes de sobrescribir el editor.',
      );
      expect(document.body.textContent).toContain('Payload a cargar');
      expect(document.body.textContent).not.toContain('Payload en vivo');
      expect(document.body.textContent).not.toContain('El payload coincide con la versión en vivo.');
    });

    await cleanup();
  });

  it('keeps the load-version dialog single-column when the selected payload already matches live', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Editar en formulario')).toBe(1);
    });

    await act(async () => {
      getButtonByText(container, 'Editar en formulario').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      const dialog = document.body.querySelector<HTMLElement>('[role="dialog"]');
      expect(dialog).not.toBeNull();
      expect(dialog?.textContent).toContain('El payload coincide con la versión en vivo.');
      expect(dialog?.textContent).toContain('Payload de la versión seleccionada');
      expect(dialog?.textContent).not.toContain('Payload en vivo');
    });

    await cleanup();
  });

  it('shows shared title, slug, and language context once above the versions list instead of repeating them on each row', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const history = container.querySelector<HTMLElement>('[data-testid="cms-admin-version-history"]');
      expect(history).not.toBeNull();
      expect(history?.textContent).toContain(
        'Contexto compartido: título Landing principal · slug records-public · idioma Español (es).',
      );
      expect((history?.textContent ?? '').split('Landing principal').length - 1).toBe(1);
      expect(history?.textContent).toContain('Versión v4');
      expect(history?.textContent).toContain('Versión v3');
      expect(countExactText(container, 'records-public')).toBe(1);
      expect(countExactText(container, 'Español (es)')).toBe(1);
      expect(countActionsByText(container, 'Editar en formulario')).toBe(1);
      expect(countExactText(container, 'En vivo')).toBe(1);
    });

    await cleanup();
  });

  it('keeps the two-version history focused on rows without showing premature status controls', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countLabelsByText(container, 'Estado')).toBe(0);
      expect(countLabelsByText(container, 'Estado del historial')).toBe(0);
      expect(container.querySelector('[data-testid="cms-admin-first-version-save-guidance"]')).not.toBeNull();
      expect(container.textContent).toContain(
        'Parte de la versión en vivo o empieza un borrador propio antes de guardar.',
      );
      expect(container.textContent).toContain('2 versiones');
      expect(countActionsByText(container, 'Editar en formulario')).toBe(1);
    });

    await cleanup();
  });

  it('makes the primary save action explicit so first-time admins can tell draft saves from live publishes', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    let rendered = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Guardar borrador')).toBe(0);
    });

    await act(async () => {
      setInputValue(getInputByLabel(container, 'Título'), 'Landing actualizada');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(getButtonByText(container, 'Guardar borrador')).toBeTruthy();
      expect(container.textContent).toContain(
        'Guardará esta versión como borrador sin cambiar la página en vivo.',
      );
      expect(container.textContent).not.toContain('Guardar versión');
    });

    await rendered.cleanup();

    window.localStorage.setItem(
      'tdf-cms-admin:draft:records-public:es',
      JSON.stringify({
        title: 'Landing publicada',
        payload: JSON.stringify({ heroTitle: 'Landing publicada' }, null, 2),
        status: 'published',
      }),
    );

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    rendered = await renderPage(secondContainer);

    await waitForExpectation(() => {
      expect(getButtonByText(secondContainer, 'Guardar y publicar')).toBeTruthy();
      expect(secondContainer.textContent).toContain(
        'Publicará esta versión al guardar y actualizará la página en vivo.',
      );
      expect(secondContainer.textContent).not.toContain('Guardar versión');
    });

    await rendered.cleanup();
  });

  it('keeps the loaded-version context beside save instead of repeating it in the versions header', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Editar en formulario')).toBe(1);
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
      expect(pageText).toContain('Base: v3');
      expect(pageText.split('Base: v3').length - 1).toBe(1);
      expect(pageText).not.toContain('Base: v3 · ID 102');
      expect(pageText).not.toContain('Editando desde ID');
    });

    await cleanup();
  });

  it('replaces the duplicate load action with passive row state once a version is already in the editor', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Editar en formulario')).toBe(1);
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
      expect(countActionsByText(container, 'Editar en formulario')).toBe(0);
      expect(countActionsByText(container, 'Publicar')).toBe(0);
      expect(countActionsByText(container, 'Borrar')).toBe(0);
      expect(countExactText(container, 'En formulario')).toBe(1);
      expect(countExactText(container, 'En vivo')).toBe(1);
      expect(container.textContent).toContain('Base: v3');
      expect(container.textContent).not.toContain('Base: v3 · ID 102');
    });

    await cleanup();
  });

  it('replaces fraction-style version counts with plain-language summary text', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('2 versiones');
      expect(container.textContent).not.toContain('2/2');
    });

    await cleanup();
  });

  it('hides the version history panel until a live page has additional saved history to compare', async () => {
    listMock.mockResolvedValue([buildContent()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelector('[data-testid="cms-admin-version-history"]')).toBeNull();
      expect(container.textContent).not.toContain('Versiones');
      expect(container.textContent).not.toContain(
        'La única versión guardada ya está resumida arriba; el historial aparecerá cuando guardes otra versión.',
      );
      expect(container.textContent).not.toContain(
        'Los filtros aparecerán cuando exista más historial para comparar versiones.',
      );
      expect(countExactText(container, 'v4')).toBe(1);
      expect(countExactText(container, 'En vivo')).toBe(0);
      expect(countActionsByText(container, 'Editar en formulario')).toBe(0);
      expect(container.textContent).not.toContain('1/1');
      expect(container.textContent).not.toContain('1 versión');
      expect(countLabelsByText(container, 'Estado')).toBe(0);
      expect(countLabelsByText(container, 'Versión mínima')).toBe(0);
      expect(countActionsByText(container, 'Limpiar filtros')).toBe(0);
    });

    await cleanup();
  });

  it('keeps a two-version history focused on the rows when every saved version already shares the same status', async () => {
    listMock.mockResolvedValue([
      buildContent(),
      buildContent({
        ccdId: 102,
        ccdVersion: 3,
        ccdStatus: 'published',
        ccdPublishedAt: '2030-01-02T03:04:05.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('2 versiones');
      expect(countLabelsByText(container, 'Estado del historial')).toBe(0);
      expect(countLabelsByText(container, 'Versión mínima')).toBe(0);
    });

    await cleanup();
  });

  it('shows the minimum-version filter once the history is long enough to narrow meaningfully', async () => {
    listMock.mockResolvedValue([
      buildContent(),
      buildContent({
        ccdId: 102,
        ccdVersion: 3,
        ccdStatus: 'published',
        ccdPublishedAt: '2030-01-02T03:04:05.000Z',
      }),
      buildContent({
        ccdId: 103,
        ccdVersion: 2,
        ccdStatus: 'published',
        ccdPublishedAt: '2030-01-01T03:04:05.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('3 versiones');
      expect(countLabelsByText(container, 'Estado del historial')).toBe(0);
      expect(countLabelsByText(container, 'Versión mínima')).toBe(1);
    });

    await cleanup();
  });

  it('shows the history status filter once enough mixed-status versions exist to narrow meaningfully', async () => {
    listMock.mockResolvedValue([
      buildContent(),
      buildContent({
        ccdId: 102,
        ccdVersion: 3,
        ccdStatus: 'draft',
        ccdPublishedAt: null,
      }),
      buildContent({
        ccdId: 103,
        ccdVersion: 2,
        ccdStatus: 'archived',
        ccdPublishedAt: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('3 versiones');
      expect(countLabelsByText(container, 'Estado')).toBe(0);
      expect(countLabelsByText(container, 'Estado del historial')).toBe(1);
    });

    await cleanup();
  });

  it('keeps filtered-empty version history to one clear reset action', async () => {
    listMock.mockResolvedValue([
      buildContent(),
      buildContent({
        ccdId: 102,
        ccdVersion: 3,
        ccdStatus: 'draft',
        ccdPublishedAt: null,
      }),
      buildContent({
        ccdId: 103,
        ccdVersion: 2,
        ccdStatus: 'published',
        ccdPublishedAt: '2030-01-01T03:04:05.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countLabelsByText(container, 'Estado del historial')).toBe(1);
      expect(container.textContent).toContain('3 versiones');
    });

    await selectComboboxOption(container, 'Todos', 'Archivados');

    await waitForExpectation(() => {
      const history = container.querySelector<HTMLElement>('[data-testid="cms-admin-version-history"]');
      expect(history).not.toBeNull();
      expect(history?.textContent).toContain(
        'Ninguna versión coincide con los filtros actuales. Limpia los filtros para volver a ver el historial completo.',
      );
      expect(countActionsByText(history!, 'Limpiar filtros')).toBe(1);
      expect(countActionsByText(container, 'Limpiar filtros')).toBe(1);
      expect(container.textContent).toContain('0 de 3');
    });

    await cleanup();
  });

  it('shows shared version status once in the history context instead of repeating identical row chips', async () => {
    listMock.mockResolvedValue([
      buildContent(),
      buildContent({
        ccdId: 102,
        ccdVersion: 3,
        ccdStatus: 'published',
        ccdPublishedAt: '2030-01-02T03:04:05.000Z',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(
        'Contexto compartido: título Landing principal · slug records-public · idioma Español (es) · estado Publicado.',
      );
      expect(countExactText(container, 'Publicado')).toBe(1);
      expect(countLabelsByText(container, 'Estado del historial')).toBe(0);
      expect(countActionsByText(container, 'Editar en formulario')).toBe(1);
    });

    await cleanup();
  });

  it('collapses published history row state and timestamp into one clear chip', async () => {
    listMock.mockResolvedValue([
      buildContent(),
      buildContent({
        ccdId: 102,
        ccdVersion: 3,
        ccdStatus: 'published',
        ccdPublishedAt: '2030-01-02T03:04:05.000Z',
      }),
      buildContent({
        ccdId: 103,
        ccdVersion: 2,
        ccdStatus: 'draft',
        ccdPublishedAt: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const history = container.querySelector<HTMLElement>('[data-testid="cms-admin-version-history"]');
      expect(history).not.toBeNull();
      expect(history?.textContent).toContain('Publicado:');
      expect(history?.textContent).not.toContain('pub:');
      expect(countExactText(history!, 'Publicado')).toBe(0);
      expect(countExactText(history!, 'Borrador')).toBe(1);
    });

    await cleanup();
  });
});
