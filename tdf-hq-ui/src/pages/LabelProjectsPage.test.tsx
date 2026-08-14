import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';

const listProjectNotesMock = jest.fn<() => Promise<unknown[]>>();
const createProjectNoteMock = jest.fn<(text: string) => Promise<unknown>>();
const updateProjectNoteMock = jest.fn<(id: string, payload: unknown) => Promise<unknown>>();
const deactivateProjectNoteMock = jest.fn<(id: string) => Promise<void>>();

jest.unstable_mockModule('../api/label', () => ({
  Label: {
    listProjectNotes: listProjectNotesMock,
    createProjectNote: createProjectNoteMock,
    updateProjectNote: updateProjectNoteMock,
    deactivateProjectNote: deactivateProjectNoteMock,
  },
}));

const { default: LabelProjectsPage } = await import('./LabelProjectsPage');

const flush = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const waitForText = async (container: HTMLElement, text: string) => {
  for (let attempt = 0; attempt < 20; attempt += 1) {
    if (container.textContent?.includes(text)) return;
    await act(async () => {
      await flush();
    });
  }
  throw new Error(`Text not found: ${text}`);
};

const setInputValue = (input: HTMLInputElement, value: string) => {
  Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value')?.set?.call(input, value);
  input.dispatchEvent(new Event('input', { bubbles: true }));
  input.dispatchEvent(new Event('change', { bubbles: true }));
};

describe('LabelProjectsPage', () => {
  let container: HTMLDivElement;
  let root: Root;
  let queryClient: QueryClient;

  beforeEach(async () => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
    jest.clearAllMocks();
    listProjectNotesMock.mockResolvedValue([
      {
        lpnId: '11111111-1111-4111-8111-111111111111',
        lpnText: 'Distribuir master',
        lpnCompleted: false,
        lpnCreatedAt: '2030-01-01T00:00:00Z',
        lpnUpdatedAt: '2030-01-01T00:00:00Z',
        lpnVersion: 3,
      },
    ]);
    createProjectNoteMock.mockResolvedValue(undefined);
    updateProjectNoteMock.mockResolvedValue(undefined);
    deactivateProjectNoteMock.mockResolvedValue();
    queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    container = document.createElement('div');
    document.body.appendChild(container);
    root = createRoot(container);
    await act(async () => {
      root.render(
        <MemoryRouter>
          <QueryClientProvider client={queryClient}>
            <LabelProjectsPage />
          </QueryClientProvider>
        </MemoryRouter>,
      );
      await flush();
      await flush();
    });
  });

  afterEach(async () => {
    await act(async () => {
      root.unmount();
      await flush();
    });
    queryClient.clear();
    container.remove();
  });

  it('uses canonical project-note CRUD and optimistic versions without CMS slugs', async () => {
    await waitForText(container, 'Distribuir master');
    expect(container.textContent).toContain('Distribuir master');

    const checkbox = container.querySelector<HTMLInputElement>('input[type="checkbox"]');
    expect(checkbox).not.toBeNull();
    await act(async () => {
      checkbox?.click();
      await flush();
    });
    expect(updateProjectNoteMock).toHaveBeenCalledWith(
      '11111111-1111-4111-8111-111111111111',
      { lpnuCompleted: true, lpnuExpectedVersion: 3 },
    );

    const input = container.querySelector<HTMLInputElement>('input[placeholder="Idea, estado o pendiente"]');
    expect(input).not.toBeNull();
    await act(async () => {
      if (input) setInputValue(input, 'Revisar créditos');
      await flush();
    });
    const addButton = Array.from(container.querySelectorAll('button')).find(
      (button) => button.textContent?.trim() === 'Agregar',
    );
    await act(async () => {
      addButton?.click();
      await flush();
    });
    expect(createProjectNoteMock.mock.calls[0]?.[0]).toBe('Revisar créditos');

    const deleteButton = container.querySelector<HTMLButtonElement>('[data-project-note-delete-action]');
    await act(async () => {
      deleteButton?.click();
      await flush();
    });
    expect(deactivateProjectNoteMock.mock.calls[0]?.[0]).toBe('11111111-1111-4111-8111-111111111111');
  });
});
