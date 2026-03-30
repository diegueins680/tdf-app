import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';

interface TrialSubjectItem {
  subjectId: number;
  name: string;
  active: boolean;
  roomIds: string[];
}

const listSubjectsMock = jest.fn<() => Promise<TrialSubjectItem[]>>(
  () => Promise.resolve([{ subjectId: 1, name: 'Producción Musical', active: true, roomIds: [] }]),
);
const listSlotsMock = jest.fn<(subjectId?: number) => Promise<unknown[]>>(() => Promise.resolve([]));
const createRequestMock = jest.fn<(payload: unknown) => Promise<{ requestId: number; status: string }>>(
  () => Promise.resolve({ requestId: 123, status: 'requested' }),
);

jest.unstable_mockModule('../api/trials', () => ({
  Trials: {
    listSubjects: () => listSubjectsMock(),
    listSlots: (subjectId?: number) => listSlotsMock(subjectId),
    createRequest: (payload: unknown) => createRequestMock(payload),
  },
}));

jest.unstable_mockModule('../components/PublicBrandBar', () => ({
  default: () => null,
}));

const {
  default: TrialsPage,
  applySuggestedSlot,
  parsePositiveSubjectId,
  toFriendlyTrialError,
} = await import('../pages/TrialsPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const renderPage = async (container: HTMLElement) => {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter future={{ v7_startTransition: true, v7_relativeSplatPath: true }}>
        <QueryClientProvider client={queryClient}>
          <TrialsPage />
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
      queryClient.clear();
    },
  };
};

const clickButtonByText = (container: HTMLElement, label: string) => {
  const button = Array.from(container.querySelectorAll('button')).find(
    (candidate) => candidate.textContent?.trim() === label,
  );
  if (!button) throw new Error(`Button not found: ${label}`);
  button.click();
};

describe('TrialsPage', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    listSubjectsMock.mockClear();
    listSlotsMock.mockClear();
    createRequestMock.mockClear();
  });

  it('accepts numeric subject ids from the MUI select value', () => {
    expect(parsePositiveSubjectId(4)).toBe(4);
    expect(parsePositiveSubjectId('7')).toBe(7);
    expect(parsePositiveSubjectId('')).toBe('');
    expect(parsePositiveSubjectId('abc')).toBe('');
  });

  it('fills the next empty preferred slot and avoids duplicates when using suggestions', () => {
    const firstFill = applySuggestedSlot(
      [{ start: '' }, { start: '' }, { start: '' }],
      '2030-01-01T10:00',
    );
    expect(firstFill).toEqual([
      { start: '2030-01-01T10:00' },
      { start: '' },
      { start: '' },
    ]);

    const secondFill = applySuggestedSlot(firstFill, '2030-01-01T11:00');
    expect(secondFill).toEqual([
      { start: '2030-01-01T10:00' },
      { start: '2030-01-01T11:00' },
      { start: '' },
    ]);

    expect(applySuggestedSlot(secondFill, '2030-01-01T11:00')).toEqual(secondFill);
  });

  it('maps backend availability errors to friendly copy', () => {
    expect(toFriendlyTrialError(new Error('No hay profesores disponibles en el horario solicitado'))).toBe(
      'Ese horario ya no está disponible. Prueba uno de los sugeridos o agrega otra hora.',
    );
    expect(toFriendlyTrialError(new Error('Correo requerido para crear la cuenta'))).toBe(
      'Déjanos un correo válido para poder confirmar tu clase de prueba.',
    );
  });

  it('starts with one preferred slot and progressively reveals optional alternatives', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    expect(container.textContent).toContain('Horario preferido');
    expect(container.textContent).not.toContain('Horario alternativo 1');
    expect(container.textContent).toContain('Agregar otro horario');

    await act(async () => {
      clickButtonByText(container, 'Agregar otro horario');
      await flushPromises();
    });

    expect(container.textContent).toContain('Horario alternativo 1');

    await act(async () => {
      clickButtonByText(container, 'Agregar otro horario');
      await flushPromises();
    });

    expect(container.textContent).toContain('Horario alternativo 2');
    expect(Array.from(container.querySelectorAll('button')).some((button) => button.textContent?.trim() === 'Agregar otro horario')).toBe(false);

    await cleanup();
    document.body.removeChild(container);
  });
});
