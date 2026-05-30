import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';

import PageShell, { EmptyState, SkeletonCards } from './PageShell';

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const render = async (mountNode: HTMLElement, node: React.ReactNode) => {
  let root: Root | null = createRoot(mountNode);
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
      document.body.removeChild(mountNode);
    },
  };
};

describe('PageShell loading accessibility', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('announces the header loading state to assistive tech', async () => {
    const loadingShellNode = document.createElement('div');
    document.body.appendChild(loadingShellNode);
    const { cleanup } = await render(
      loadingShellNode,
      <PageShell title="Artistas" loading>
        <div>contenido</div>
      </PageShell>,
    );

    try {
      const loadingStatus = loadingShellNode.querySelector('[role="status"]');
      expect(loadingStatus).not.toBeNull();
      expect(loadingStatus?.getAttribute('aria-busy')).toBe('true');
      expect(loadingStatus?.getAttribute('aria-label')).toBe('Cargando…');
    } finally {
      await cleanup();
    }
  });

  it('does not expose a loading status once content is ready', async () => {
    const readyShellNode = document.createElement('div');
    document.body.appendChild(readyShellNode);
    const { cleanup } = await render(
      readyShellNode,
      <PageShell title="Artistas">
        <div>contenido</div>
      </PageShell>,
    );

    try {
      expect(readyShellNode.querySelector('[role="status"]')).toBeNull();
      expect(readyShellNode.textContent).toContain('Artistas');
    } finally {
      await cleanup();
    }
  });
});

describe('EmptyState', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('renders a decorative default illustration when no icon is provided', async () => {
    const emptyStateNode = document.createElement('div');
    document.body.appendChild(emptyStateNode);
    const { cleanup } = await render(
      emptyStateNode,
      <EmptyState title="Sin artistas" description="Crea el primero para empezar." />,
    );

    try {
      // The decorative icon container is always present and hidden from AT,
      // while the title/description carry the meaning.
      expect(emptyStateNode.querySelector('[aria-hidden="true"]')).not.toBeNull();
      expect(emptyStateNode.textContent).toContain('Sin artistas');
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
    const skeletonCardsNode = document.createElement('div');
    document.body.appendChild(skeletonCardsNode);
    const { cleanup } = await render(skeletonCardsNode, <SkeletonCards count={2} />);

    try {
      const skeletonStatus = skeletonCardsNode.querySelector('[role="status"]');
      expect(skeletonStatus).not.toBeNull();
      expect(skeletonStatus?.getAttribute('aria-busy')).toBe('true');
      expect(skeletonStatus?.getAttribute('aria-label')).toBe('Cargando contenido…');
    } finally {
      await cleanup();
    }
  });
});
