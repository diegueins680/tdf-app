import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';

import PageShell, { SkeletonCards } from './PageShell';

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const render = async (container: HTMLElement, node: React.ReactNode) => {
  let root: Root | null = createRoot(container);
  await act(async () => {
    root?.render(
      <MemoryRouter future={{ v7_startTransition: true, v7_relativeSplatPath: true }}>
        {node}
      </MemoryRouter>,
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
      document.body.removeChild(container);
    },
  };
};

describe('PageShell loading accessibility', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('announces the header loading state to assistive tech', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await render(
      container,
      <PageShell title="Artistas" loading>
        <div>contenido</div>
      </PageShell>,
    );

    try {
      const status = container.querySelector('[role="status"]');
      expect(status).not.toBeNull();
      expect(status?.getAttribute('aria-busy')).toBe('true');
      expect(status?.getAttribute('aria-label')).toBe('Cargando…');
    } finally {
      await cleanup();
    }
  });

  it('does not expose a loading status once content is ready', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await render(
      container,
      <PageShell title="Artistas">
        <div>contenido</div>
      </PageShell>,
    );

    try {
      expect(container.querySelector('[role="status"]')).toBeNull();
      expect(container.textContent).toContain('Artistas');
    } finally {
      await cleanup();
    }
  });
});

describe('SkeletonCards accessibility', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('exposes an aria-busy status region while loading', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await render(container, <SkeletonCards count={2} />);

    try {
      const status = container.querySelector('[role="status"]');
      expect(status).not.toBeNull();
      expect(status?.getAttribute('aria-busy')).toBe('true');
      expect(status?.getAttribute('aria-label')).toBe('Cargando contenido…');
    } finally {
      await cleanup();
    }
  });
});
