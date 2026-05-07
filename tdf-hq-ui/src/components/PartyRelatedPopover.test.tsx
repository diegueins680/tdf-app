import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { PartyDTO, PartyRelatedDTO } from '../api/types';

const relatedMock = jest.fn<(id: number) => Promise<PartyRelatedDTO>>();

jest.unstable_mockModule('../api/parties', () => ({
  Parties: {
    related: (id: number) => relatedMock(id),
  },
}));

const { default: PartyRelatedPopover } = await import('./PartyRelatedPopover');

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

const buttonText = (element: Element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim();

const getButtonsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('button')).filter((element) => buttonText(element) === labelText);

const buildParty = (overrides: Partial<PartyDTO> = {}) => ({
  partyId: 7,
  displayName: 'Ada Lovelace',
  isOrg: false,
  primaryEmail: 'ada@example.com',
  instagram: '@ada',
  ...overrides,
}) satisfies PartyDTO;

const buildRelated = (overrides: Partial<PartyRelatedDTO> = {}) => ({
  prPartyId: 7,
  prBookings: [],
  prClassSessions: [],
  prLabelTracks: [],
  ...overrides,
}) satisfies PartyRelatedDTO;

const renderPopover = async (related: PartyRelatedDTO) => {
  relatedMock.mockResolvedValue(related);

  const anchor = document.createElement('button');
  anchor.textContent = 'open';
  anchor.getBoundingClientRect = () => ({
    x: 8,
    y: 8,
    top: 8,
    left: 8,
    bottom: 32,
    right: 80,
    width: 72,
    height: 24,
    toJSON: () => ({}),
  });
  document.body.appendChild(anchor);

  const container = document.createElement('div');
  document.body.appendChild(container);
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter future={{ v7_startTransition: true, v7_relativeSplatPath: true }}>
        <QueryClientProvider client={queryClient}>
          <PartyRelatedPopover party={buildParty()} anchorEl={anchor} onClose={() => undefined} />
        </QueryClientProvider>
      </MemoryRouter>,
    );
    await flushPromises();
    await flushPromises();
  });

  return {
    cleanup: async () => {
      if (root) {
        await act(async () => {
          root?.unmount();
          await flushPromises();
        });
        root = null;
      }
      queryClient.clear();
      document.body.removeChild(container);
      document.body.removeChild(anchor);
    },
  };
};

describe('PartyRelatedPopover', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    relatedMock.mockReset();
  });

  it('collapses an empty related history into one message and one useful destination', async () => {
    const { cleanup } = await renderPopover(buildRelated());

    try {
      await waitForExpectation(() => {
        expect(relatedMock).toHaveBeenCalledWith(7);
        expect(document.body.textContent).toContain(
          'No hay historial relacionado todavía. Usa Perfil para revisar o completar este contacto.',
        );
        expect(getButtonsByText(document.body, 'Perfil')).toHaveLength(1);
        expect(getButtonsByText(document.body, 'Reservas (cliente)')).toHaveLength(0);
        expect(getButtonsByText(document.body, 'Reservas (ingeniero)')).toHaveLength(0);
        expect(getButtonsByText(document.body, 'Clases (estudiante)')).toHaveLength(0);
        expect(getButtonsByText(document.body, 'Clases (profesor)')).toHaveLength(0);
        expect(getButtonsByText(document.body, 'Tracks')).toHaveLength(0);
        expect(document.body.textContent).not.toContain('No hay reservas asociadas.');
        expect(document.body.textContent).not.toContain('No hay clases asociadas.');
        expect(document.body.textContent).not.toContain('No hay tracks asociados.');
      });
    } finally {
      await cleanup();
    }
  });

  it('shows only history actions and sections that have related records', async () => {
    const { cleanup } = await renderPopover(buildRelated({
      prBookings: [
        {
          prbBookingId: 31,
          prbRole: 'cliente',
          prbTitle: 'Sesión vocal',
          prbServiceType: 'Grabación',
          prbStartsAt: '2026-05-05T16:00:00Z',
          prbEndsAt: '2026-05-05T18:00:00Z',
          prbStatus: 'confirmada',
        },
      ],
      prClassSessions: [
        {
          prcClassSessionId: 44,
          prcRole: 'profesor',
          prcSubjectId: 2,
          prcSubjectName: 'Piano',
          prcTeacherId: 7,
          prcTeacherName: 'Ada Lovelace',
          prcStudentId: 9,
          prcStudentName: 'Grace Hopper',
          prcStartAt: '2026-05-06T14:00:00Z',
          prcEndAt: '2026-05-06T15:00:00Z',
          prcStatus: 'programada',
        },
      ],
      prLabelTracks: [
        {
          prtId: 'track-1',
          prtTitle: 'Pulso',
          prtStatus: 'mix',
          prtCreatedAt: '2026-05-01T10:00:00Z',
          prtUpdatedAt: '2026-05-04T10:00:00Z',
        },
      ],
    }));

    try {
      await waitForExpectation(() => {
        expect(getButtonsByText(document.body, 'Perfil')).toHaveLength(1);
        expect(getButtonsByText(document.body, 'Reservas (cliente)')).toHaveLength(1);
        expect(getButtonsByText(document.body, 'Clases (profesor)')).toHaveLength(1);
        expect(getButtonsByText(document.body, 'Tracks')).toHaveLength(1);
        expect(getButtonsByText(document.body, 'Reservas (ingeniero)')).toHaveLength(0);
        expect(getButtonsByText(document.body, 'Clases (estudiante)')).toHaveLength(0);
        expect(document.body.textContent).toContain('Reservas');
        expect(document.body.textContent).toContain('Cliente (1)');
        expect(document.body.textContent).toContain('Grabación');
        expect(document.body.textContent).toContain('Clases');
        expect(document.body.textContent).toContain('Profesor (1)');
        expect(document.body.textContent).toContain('Piano · Grace Hopper');
        expect(document.body.textContent).toContain('Tracks');
        expect(document.body.textContent).toContain('Pulso');
        expect(document.body.textContent).not.toContain('No hay historial relacionado todavía.');
        expect(document.body.textContent).not.toContain('Ingeniero (0)');
        expect(document.body.textContent).not.toContain('Estudiante (0)');
      });
    } finally {
      await cleanup();
    }
  });
});
