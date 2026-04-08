import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { SocialMessage } from '../api/socialInbox';

const listInstagramMessagesMock = jest.fn<(filters?: { direction?: 'incoming' | 'outgoing' | 'all' }) => Promise<SocialMessage[]>>();
const listFacebookMessagesMock = jest.fn<(filters?: { direction?: 'incoming' | 'outgoing' | 'all' }) => Promise<SocialMessage[]>>();
const listWhatsAppMessagesMock = jest.fn<(filters?: { direction?: 'incoming' | 'outgoing' | 'all' }) => Promise<SocialMessage[]>>();

jest.unstable_mockModule('../api/socialInbox', () => ({
  SocialInboxAPI: {
    listInstagramMessages: (filters?: { direction?: 'incoming' | 'outgoing' | 'all' }) => listInstagramMessagesMock(filters),
    listFacebookMessages: (filters?: { direction?: 'incoming' | 'outgoing' | 'all' }) => listFacebookMessagesMock(filters),
    listWhatsAppMessages: (filters?: { direction?: 'incoming' | 'outgoing' | 'all' }) => listWhatsAppMessagesMock(filters),
  },
}));

const { default: AdminDiagnosticsPage } = await import('./AdminDiagnosticsPage');

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
      <MemoryRouter
        initialEntries={['/admin/diagnosticos']}
        future={{ v7_startTransition: true, v7_relativeSplatPath: true }}
      >
        <QueryClientProvider client={qc}>
          <AdminDiagnosticsPage />
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

const buildMessage = (overrides: Partial<SocialMessage> = {}): SocialMessage => ({
  externalId: 'msg-1',
  senderId: 'sender-1',
  senderName: 'Ada Lovelace',
  text: 'Hola, quiero info',
  metadata: null,
  direction: 'incoming',
  repliedAt: '2030-01-03T03:04:05.000Z',
  replyText: 'Claro, te comparto los detalles.',
  replyError: null,
  createdAt: '2030-01-03T03:00:00.000Z',
  ...overrides,
});

const countOccurrences = (root: ParentNode, text: string) =>
  (root.textContent ?? '').split(text).length - 1;

describe('AdminDiagnosticsPage', () => {
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
    listInstagramMessagesMock.mockReset();
    listFacebookMessagesMock.mockReset();
    listWhatsAppMessagesMock.mockReset();

    listInstagramMessagesMock.mockResolvedValue([]);
    listFacebookMessagesMock.mockResolvedValue([]);
    listWhatsAppMessagesMock.mockResolvedValue([]);
    window.localStorage.clear();
  });

  it('replaces repeated quiet-channel empty states with one first-run summary for the whole section', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(listInstagramMessagesMock).toHaveBeenCalledWith({ direction: 'incoming' });
        expect(listFacebookMessagesMock).toHaveBeenCalledWith({ direction: 'incoming' });
        expect(listWhatsAppMessagesMock).toHaveBeenCalledWith({ direction: 'incoming' });
        expect(container.textContent).toContain(
          'Todavía no hay mensajes entrantes en Instagram, Facebook ni WhatsApp. Cuando llegue el primero, aquí verás el historial respondido por canal.',
        );
        expect(countOccurrences(container, 'Todavía no hay mensajes entrantes en este canal.')).toBe(0);
        expect(container.querySelectorAll('thead')).toHaveLength(0);
        expect(container.textContent).not.toContain('Sin mensajes respondidos.');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps the detailed history table only for channels that already have replied messages', async () => {
    listInstagramMessagesMock.mockResolvedValue([buildMessage()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelectorAll('thead')).toHaveLength(1);
        expect(container.textContent).toContain('Ada Lovelace');
        expect(container.textContent).toContain('Claro, te comparto los detalles.');
        expect(countOccurrences(container, 'Todavía no hay mensajes entrantes en este canal.')).toBe(2);
      });
    } finally {
      await cleanup();
    }
  });
});
