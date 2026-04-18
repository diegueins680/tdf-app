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

  it('keeps editor guidance in one helper block instead of stacking separate autosave and compare lines', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      const guidance = container.querySelector<HTMLElement>('[data-testid="cms-admin-editor-guidance"]');
      expect(guidance).not.toBeNull();
      expect(guidance?.textContent?.trim()).toBe(
        'El borrador se guarda automáticamente en este navegador por slug y locale mientras editas. El payload editable está arriba. La versión en vivo ya se muestra en la columna izquierda; usa Comparar con live si necesitas revisar cambios línea por línea.',
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
      expect(countActionsByText(container, 'Editar en formulario')).toBe(1);
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

  it('drops the generic example action when a live version already exists so the editor keeps one real starting point', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(1);
      expect(countActionsByText(container, 'Cargar ejemplo')).toBe(0);
      expect(container.textContent).toContain(
        'Esta página ya tiene una versión en vivo. Usa "Usar versión en vivo" para traer la estructura real al editor.',
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
      expect(container.textContent).toContain(
        'Esta página ya tiene una versión en vivo. Usa "Usar versión en vivo" para traer la estructura real al editor.',
      );
    });

    await act(async () => {
      getButtonByText(container, 'Usar versión en vivo').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Usar versión en vivo')).toBe(0);
      expect(container.textContent).toContain('Editor coincide con live');
      expect(container.textContent).toContain(
        'El payload editable ya coincide con la versión en vivo. El comparador aparecerá cuando vuelvas a modificarlo.',
      );
      expect(container.textContent).not.toContain(
        'Esta página ya tiene una versión en vivo. Usa "Usar versión en vivo" para traer la estructura real al editor.',
      );
    });

    await cleanup();
  });

  it('hides the clear-payload action until the editor has JSON to clear', async () => {
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

  it('keeps the compare action as the only draft-vs-live status control', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Comparar con live')).toBe(1);
      expect(container.textContent).not.toContain('Payload modificado vs en vivo');
      expect(container.textContent).toContain(
        'El payload editable está arriba. La versión en vivo ya se muestra en la columna izquierda; usa Comparar con live si necesitas revisar cambios línea por línea.',
      );
    });

    await act(async () => {
      getButtonByText(container, 'Usar versión en vivo').click();
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countActionsByText(container, 'Comparar con live')).toBe(0);
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
      expect(countActionsByText(container, 'Comparar con live')).toBe(1);
      expect(container.textContent).not.toContain('Payload modificado vs en vivo');
      expect(container.textContent).toContain(
        'El payload editable está arriba. La versión en vivo ya se muestra en la columna izquierda; usa Comparar con live si necesitas revisar cambios línea por línea.',
      );
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

  it('replaces blank custom-slug dead ends with one helper and keeps save disabled until the slug exists', async () => {
    window.localStorage.setItem(
      'tdf-cms-admin:last-selection',
      JSON.stringify({ slug: '', locale: 'es' }),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(listMock).toHaveBeenCalledWith({ slug: '', locale: 'es' });
      expect(getPublicMock).not.toHaveBeenCalled();
      expect(countActionsByText(container, 'Abrir página en vivo')).toBe(0);
      expect(container.textContent).toContain(
        'Completa este slug para habilitar el guardado y Abrir página en vivo.',
      );
      expect(getButtonByText(container, 'Guardar borrador').disabled).toBe(true);
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
      expect(countActionsByText(container, 'Comparar con live')).toBe(0);
      expect(container.textContent).toContain(
        'El payload editable está arriba. Cuando exista una versión en vivo, la verás en la columna izquierda, aparecerá el botón "Usar versión en vivo" y podrás compararla desde aquí.',
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

  it('shows shared slug and locale context once above the versions list instead of repeating them on each row', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Contexto compartido: slug records-public · locale es.');
      expect(countExactText(container, 'records-public')).toBe(1);
      expect(countExactText(container, 'es')).toBe(1);
      expect(countActionsByText(container, 'Editar en formulario')).toBe(1);
      expect(countExactText(container, 'En vivo')).toBe(1);
    });

    await cleanup();
  });

  it('keeps the editor status field distinct from the versions filter so first-time admins do not see two ambiguous Estado controls', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(countLabelsByText(container, 'Estado')).toBe(1);
      expect(countLabelsByText(container, 'Estado del historial')).toBe(1);
    });

    await cleanup();
  });

  it('makes the primary save action explicit so first-time admins can tell draft saves from live publishes', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    let rendered = await renderPage(container);

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
      JSON.stringify({ status: 'published' }),
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
      expect(pageText).toContain('Base: v3 · ID 102');
      expect(pageText.split('Base: v3 · ID 102').length - 1).toBe(1);
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
      expect(container.textContent).toContain('Base: v3 · ID 102');
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

  it('hides version filters until the CMS history has enough entries to compare', async () => {
    listMock.mockResolvedValue([buildContent()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(
        'Los filtros aparecerán cuando exista más historial para comparar versiones.',
      );
      expect(container.textContent).not.toContain('1/1');
      expect(container.textContent).not.toContain('1 versión');
      expect(countLabelsByText(container, 'Estado')).toBe(1);
      expect(countLabelsByText(container, 'Versión mínima')).toBe(0);
      expect(countActionsByText(container, 'Limpiar filtros')).toBe(0);
    });

    await cleanup();
  });

  it('hides the history status filter when every saved version already shares the same status', async () => {
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
      expect(countLabelsByText(container, 'Versión mínima')).toBe(1);
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
        'Contexto compartido: slug records-public · locale es · estado Publicado.',
      );
      expect(countExactText(container, 'Publicado')).toBe(1);
      expect(countLabelsByText(container, 'Estado del historial')).toBe(0);
      expect(countActionsByText(container, 'Editar en formulario')).toBe(1);
    });

    await cleanup();
  });
});
