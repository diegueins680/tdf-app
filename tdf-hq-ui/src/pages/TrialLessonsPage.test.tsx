import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { RoomDTO } from '../api/types';
import type { ClassSessionDTO, StudentDTO, TeacherDTO, TrialSubject } from '../api/trials';

type ClassSessionFilters = {
  subjectId?: number;
  teacherId?: number;
  studentId?: number;
  from?: string;
  to?: string;
  status?: string;
};

const listSubjectsMock = jest.fn<() => Promise<TrialSubject[]>>();
const listTeachersMock = jest.fn<() => Promise<TeacherDTO[]>>();
const listRoomsMock = jest.fn<() => Promise<RoomDTO[]>>();
const listStudentsMock = jest.fn<() => Promise<StudentDTO[]>>();
const listClassSessionsMock = jest.fn<(params?: ClassSessionFilters) => Promise<ClassSessionDTO[]>>();
const createClassSessionMock = jest.fn<(payload: unknown) => Promise<unknown>>();
const updateClassSessionMock = jest.fn<(classId: number, payload: unknown) => Promise<ClassSessionDTO>>();
const attendClassSessionMock = jest.fn<(classId: number, payload: unknown) => Promise<unknown>>();
const createStudentMock = jest.fn<(payload: unknown) => Promise<StudentDTO>>();
const updateBookingMock = jest.fn<(bookingId: number, payload: unknown) => Promise<unknown>>();

jest.unstable_mockModule('../api/trials', () => ({
  Trials: {
    listSubjects: () => listSubjectsMock(),
    listTeachers: () => listTeachersMock(),
    listClassSessions: (params?: ClassSessionFilters) => listClassSessionsMock(params),
    createClassSession: (payload: unknown) => createClassSessionMock(payload),
    updateClassSession: (classId: number, payload: unknown) => updateClassSessionMock(classId, payload),
    attendClassSession: (classId: number, payload: unknown) => attendClassSessionMock(classId, payload),
    listStudents: () => listStudentsMock(),
    createStudent: (payload: unknown) => createStudentMock(payload),
  },
}));

jest.unstable_mockModule('../api/rooms', () => ({
  Rooms: {
    list: () => listRoomsMock(),
  },
}));

jest.unstable_mockModule('../api/bookings', () => ({
  Bookings: {
    update: (bookingId: number, payload: unknown) => updateBookingMock(bookingId, payload),
  },
}));

const { default: TrialLessonsPage } = await import('./TrialLessonsPage');

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

const renderPage = async (container: HTMLElement) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter future={{ v7_startTransition: true, v7_relativeSplatPath: true }}>
        <QueryClientProvider client={qc}>
          <TrialLessonsPage />
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

const buttonText = (element: Element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim();

const hasButton = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('button')).some((button) => buttonText(button) === labelText);

const buildClassSession = (overrides: Partial<ClassSessionDTO> = {}): ClassSessionDTO => ({
  classSessionId: 301,
  teacherId: 7,
  teacherName: 'Ada Lovelace',
  subjectId: 1,
  subjectName: 'Produccion Musical',
  studentId: 11,
  studentName: 'Grace Hopper',
  startAt: '2030-01-02T15:00:00.000Z',
  endAt: '2030-01-02T15:45:00.000Z',
  status: 'programada',
  roomId: '1',
  roomName: 'Sala A',
  bookingId: null,
  notes: null,
  updatedAt: null,
  ...overrides,
});

describe('TrialLessonsPage', () => {
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
    window.sessionStorage.clear();
    listSubjectsMock.mockReset();
    listTeachersMock.mockReset();
    listRoomsMock.mockReset();
    listStudentsMock.mockReset();
    listClassSessionsMock.mockReset();
    createClassSessionMock.mockReset();
    updateClassSessionMock.mockReset();
    attendClassSessionMock.mockReset();
    createStudentMock.mockReset();
    updateBookingMock.mockReset();

    listSubjectsMock.mockResolvedValue([
      { subjectId: 1, name: 'Produccion Musical', active: true, roomIds: ['1'] },
    ]);
    listTeachersMock.mockResolvedValue([
      { teacherId: 7, teacherName: 'Ada Lovelace', subjects: [{ subjectId: 1, name: 'Produccion Musical' }] },
    ]);
    listRoomsMock.mockResolvedValue([{ roomId: '1', rName: 'Sala A', rBookable: true }]);
    listStudentsMock.mockResolvedValue([{ studentId: 11, displayName: 'Grace Hopper', email: 'grace@example.com' }]);
    listClassSessionsMock.mockResolvedValue([]);
    createClassSessionMock.mockResolvedValue({ classSessionId: 301, consumedMinutes: 45 });
    updateClassSessionMock.mockResolvedValue(buildClassSession());
    attendClassSessionMock.mockResolvedValue({ classSessionId: 301, consumedMinutes: 45 });
    createStudentMock.mockResolvedValue({ studentId: 12, displayName: 'Katherine Johnson' });
    updateBookingMock.mockResolvedValue({});
  });

  it('replaces the empty export action with first-run guidance when there are no trial lessons', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listClassSessionsMock).toHaveBeenCalled();
        expect(container.querySelector('[data-testid="trial-lessons-empty-state"]')).not.toBeNull();
        expect(container.textContent).toContain('Primeros pasos');
        expect(container.textContent).toContain('Aún no hay clases de prueba en este rango.');
        expect(container.textContent).not.toContain('No hay clases de prueba para este filtro.');
        expect(hasButton(container, 'Exportar CSV')).toBe(false);
        expect(hasButton(container, 'Refrescar')).toBe(false);
        expect(hasButton(container, 'Nuevo alumno')).toBe(true);
        expect(hasButton(container, 'Nueva clase')).toBe(true);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps export available once there are trial lessons to download', async () => {
    listClassSessionsMock.mockResolvedValue([buildClassSession()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('[data-testid="trial-lessons-empty-state"]')).toBeNull();
        expect(container.textContent).toContain('Grace Hopper');
        expect(container.textContent).toContain('Produccion Musical');
        expect(hasButton(container, 'Exportar CSV')).toBe(true);
        expect(hasButton(container, 'Refrescar')).toBe(true);
      });
    } finally {
      await cleanup();
    }
  });
});
