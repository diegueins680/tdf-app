import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

import type {
  ArtistInvitationLinkCreate,
  ArtistInvitationLinkDTO,
  ArtistInvitationLinkIssued,
} from '../api/artistInvitations';

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

const listMock = jest.fn<() => Promise<ArtistInvitationLinkDTO[]>>();
const createMock = jest.fn<(payload: ArtistInvitationLinkCreate) => Promise<ArtistInvitationLinkIssued>>();
const revokeMock = jest.fn<(id: number) => Promise<ArtistInvitationLinkDTO>>();

jest.unstable_mockModule('../api/artistInvitations', () => ({
  ArtistInvitations: {
    list: () => listMock(),
    create: (payload: ArtistInvitationLinkCreate) => createMock(payload),
    revoke: (id: number) => revokeMock(id),
  },
}));

const { default: ArtistInvitationLinksPanel } = await import('./ArtistInvitationLinksPanel');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const invitation = (overrides: Partial<ArtistInvitationLinkDTO> = {}): ArtistInvitationLinkDTO => ({
  id: 3,
  inviteeLabel: 'Banda Sur',
  campaign: 'tu_escena_conectada_piloto',
  status: 'active',
  createdAt: '2026-10-07T12:00:00Z',
  expiresAt: '2026-11-06T12:00:00Z',
  redeemedAt: null,
  redeemedByPartyId: null,
  redeemedByName: null,
  revokedAt: null,
  ...overrides,
});

async function renderPanel() {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  let root: Root | null = createRoot(container);
  await act(async () => {
    root?.render(
      <QueryClientProvider client={queryClient}>
        <ArtistInvitationLinksPanel />
      </QueryClientProvider>,
    );
    await flushPromises();
    await flushPromises();
  });
  return {
    container,
    cleanup: async () => {
      await act(async () => {
        root?.unmount();
        await flushPromises();
      });
      root = null;
      queryClient.clear();
      container.remove();
    },
  };
}

const findButton = (container: HTMLElement, label: string) =>
  Array.from(container.querySelectorAll<HTMLButtonElement>('button')).find((button) => button.textContent === label);

describe('ArtistInvitationLinksPanel', () => {
  beforeEach(() => {
    listMock.mockReset();
    createMock.mockReset();
    revokeMock.mockReset();
  });

  it('issues a personal link for one invitee and shows it once', async () => {
    listMock.mockResolvedValue([]);
    createMock.mockResolvedValue({ invitation: invitation(), token: '3f2504e0-4f89-41d3-9a0c-0305e82c3301' });
    const view = await renderPanel();
    try {
      const input = view.container.querySelector<HTMLInputElement>('input');
      await act(async () => {
        const setter = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value')?.set;
        setter?.call(input, '  Banda Sur ');
        input?.dispatchEvent(new Event('input', { bubbles: true }));
        await flushPromises();
      });
      await act(async () => {
        findButton(view.container, 'Crear enlace')?.click();
        await flushPromises();
        await flushPromises();
      });
      expect(createMock).toHaveBeenCalledWith({ inviteeLabel: 'Banda Sur', campaign: 'tu_escena_conectada_piloto' });
      const link = view.container.querySelector('[data-testid="artist-invitation-link"]')?.textContent ?? '';
      const url = new URL(link);
      expect(url.origin).toBe('https://www.tdfrecords.net');
      expect(url.searchParams.get('invite')).toBe('3f2504e0-4f89-41d3-9a0c-0305e82c3301');
    } finally {
      await view.cleanup();
    }
  });

  it('lists link status and allows revoking only active links', async () => {
    listMock.mockResolvedValue([
      invitation(),
      invitation({ id: 4, inviteeLabel: 'Dúo Norte', status: 'redeemed', redeemedAt: '2026-10-08T12:00:00Z', redeemedByName: 'Ana' }),
    ]);
    revokeMock.mockResolvedValue(invitation({ status: 'revoked', revokedAt: '2026-10-08T13:00:00Z' }));
    const view = await renderPanel();
    try {
      await act(async () => {
        await flushPromises();
        await flushPromises();
      });
      expect(view.container.textContent).toContain('Activo');
      expect(view.container.textContent).toContain('Usado');
      expect(view.container.textContent).toContain('por Ana');
      const revokeButtons = Array.from(view.container.querySelectorAll('button')).filter((b) => b.textContent === 'Revocar');
      expect(revokeButtons).toHaveLength(1);
      await act(async () => {
        revokeButtons[0]?.click();
        await flushPromises();
      });
      expect(revokeMock).toHaveBeenCalledWith(3);
    } finally {
      await view.cleanup();
    }
  });
});
