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

const getSocialChannelCardByLabel = (root: ParentNode, labelText: string) => {
  const card = Array.from(root.querySelectorAll<HTMLElement>('[data-testid="admin-diagnostics-social-channel-card"]'))
    .find((candidate) => candidate.textContent?.includes(labelText));
  if (!card) throw new Error(`Social channel card not found: ${labelText}`);
  return card;
};

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

  it('replaces empty calendar dash rows with one setup hint', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('[data-testid="admin-diagnostics-calendar-empty"]')).not.toBeNull();
        expect(container.textContent).toContain(
          'Todavía no hay calendario configurado. Conecta Google Calendar para activar el diagnóstico de sincronización.',
        );
        expect(container.textContent).toContain('Conectar calendario');
        expect(container.querySelector('a[href="/configuracion/integraciones/calendario"]')).not.toBeNull();
        expect(container.textContent).not.toContain('Abrir página de sincronización');
        expect(container.textContent).not.toContain('Calendar ID: —');
        expect(container.textContent).not.toContain('Última sincronización: —');
      });
    } finally {
      await cleanup();
    }
  });

  it('treats blank stored calendar sync values as unconfigured setup state', async () => {
    window.localStorage.setItem('calendar-sync.calendarId', '   ');
    window.localStorage.setItem('calendar-sync.lastSyncAt', '\n\t');

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('[data-testid="admin-diagnostics-calendar-empty"]')).not.toBeNull();
        expect(container.textContent).toContain(
          'Todavía no hay calendario configurado. Conecta Google Calendar para activar el diagnóstico de sincronización.',
        );
        expect(container.textContent).toContain('Conectar calendario');
        expect(container.textContent).not.toContain('Calendar ID:');
        expect(container.textContent).not.toContain('Última sincronización:');
        expect(container.textContent).not.toContain('Aún no se registra una sincronización.');
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps partial calendar setup focused on the next missing sync state', async () => {
    window.localStorage.setItem('calendar-sync.calendarId', 'primary-calendar');

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Calendar ID: primary-calendar');
        expect(container.querySelector('[data-testid="admin-diagnostics-calendar-sync-pending"]')).not.toBeNull();
        expect(container.textContent).toContain('Aún no se registra una sincronización.');
        expect(container.textContent).toContain('Abrir sincronización');
        expect(container.textContent).not.toContain('Última sincronización: —');
        expect(container.textContent).not.toContain('Calendar ID: —');
        expect(container.textContent).not.toContain('Conectar calendario');
      });
    } finally {
      await cleanup();
    }
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
        expect(container.querySelector('[data-testid="admin-diagnostics-social-quiet-summary"]')).not.toBeNull();
        expect(container.querySelectorAll('[data-testid="admin-diagnostics-social-channel-card"]')).toHaveLength(0);
        expect(container.textContent).toContain(
          'Todavía no hay mensajes entrantes en Instagram, Facebook ni WhatsApp. Cuando llegue el primero, aquí verás el historial respondido por canal.',
        );
        expect(container.textContent).not.toContain('Actualizar mensajes');
        expect(container.textContent).not.toContain('Entrantes: 0');
        expect(container.textContent).not.toContain('Respondidos: 0');
        expect(container.textContent).not.toContain('Pendientes: 0');
        expect(container.textContent).not.toContain('Fallidos: 0');
        expect(countOccurrences(container, 'Todavía no hay mensajes entrantes en este canal.')).toBe(0);
        expect(container.querySelectorAll('thead')).toHaveLength(0);
        expect(container.textContent).not.toContain('Sin mensajes respondidos.');
      });
    } finally {
      await cleanup();
    }
  });

  it('collapses first-load social checks into one loading summary', async () => {
    listInstagramMessagesMock.mockImplementation(() => new Promise<SocialMessage[]>(() => undefined));
    listFacebookMessagesMock.mockImplementation(() => new Promise<SocialMessage[]>(() => undefined));
    listWhatsAppMessagesMock.mockImplementation(() => new Promise<SocialMessage[]>(() => undefined));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelector('[data-testid="admin-diagnostics-social-loading-summary"]')).not.toBeNull();
        expect(container.querySelectorAll('[data-testid="admin-diagnostics-social-channel-card"]')).toHaveLength(0);
        expect(container.textContent).toContain('Cargando mensajes de Instagram, Facebook y WhatsApp…');
        expect(container.textContent).not.toContain('Actualizar mensajes');
        expect(container.textContent).not.toContain('Entrantes: 0');
        expect(container.querySelectorAll('thead')).toHaveLength(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps social inbox errors focused on recovery instead of empty channel cards', async () => {
    listInstagramMessagesMock.mockRejectedValue(new Error('instagram unavailable'));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const errorSummary = container.querySelector<HTMLElement>(
          '[data-testid="admin-diagnostics-social-error-summary"]',
        );

        expect(errorSummary).not.toBeNull();
        expect(errorSummary?.textContent).toContain(
          'No se pudieron cargar mensajes de Instagram. Usa Actualizar mensajes para reintentar.',
        );
        expect(errorSummary?.getAttribute('title')).toBe('Instagram: instagram unavailable');
        expect(container.textContent).toContain('Actualizar mensajes');
        expect(container.querySelectorAll('[data-testid="admin-diagnostics-social-channel-card"]')).toHaveLength(0);
        expect(container.textContent).not.toContain('Instagram: instagram unavailable');
        expect(container.textContent).not.toContain('Entrantes: 0');
        expect(countOccurrences(container, 'Todavía no hay mensajes entrantes en este canal.')).toBe(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('groups multiple social inbox errors into one recovery cue', async () => {
    listInstagramMessagesMock.mockRejectedValue(new Error('instagram unavailable'));
    listFacebookMessagesMock.mockRejectedValue(new Error('facebook unavailable'));
    listWhatsAppMessagesMock.mockRejectedValue(new Error('whatsapp unavailable'));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const errorSummary = container.querySelector<HTMLElement>(
          '[data-testid="admin-diagnostics-social-error-summary"]',
        );

        expect(errorSummary).not.toBeNull();
        expect(errorSummary?.textContent).toContain(
          'No se pudieron cargar mensajes de Instagram, Facebook y WhatsApp. Usa Actualizar mensajes para reintentar.',
        );
        expect(errorSummary?.getAttribute('title')).toBe(
          'Instagram: instagram unavailable · Facebook: facebook unavailable · WhatsApp: whatsapp unavailable',
        );
        expect(container.textContent).toContain('Actualizar mensajes');
        expect(container.querySelectorAll('[data-testid="admin-diagnostics-social-channel-card"]')).toHaveLength(0);
        expect(container.textContent).not.toContain('Instagram: instagram unavailable');
        expect(container.textContent).not.toContain('Facebook: facebook unavailable');
        expect(container.textContent).not.toContain('WhatsApp: whatsapp unavailable');
        expect(container.textContent).not.toContain('Entrantes: 0');
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
        expect(container.querySelectorAll('[data-testid="admin-diagnostics-social-channel-card"]')).toHaveLength(1);
        expect(container.querySelectorAll('thead')).toHaveLength(1);
        expect(container.textContent).toContain('Actualizar mensajes');
        expect(container.textContent).toContain(
          'Sin mensajes entrantes en Facebook y WhatsApp.',
        );
        expect(container.textContent).toContain('Ada Lovelace');
        expect(container.textContent).toContain('Claro, te comparto los detalles.');
        expect(container.textContent).not.toContain('Entrantes: 0');
        expect(countOccurrences(container, 'Todavía no hay mensajes entrantes en este canal.')).toBe(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('keeps partially loading social channels from showing confirmed zero-count chips', async () => {
    listInstagramMessagesMock.mockResolvedValue([buildMessage()]);
    listFacebookMessagesMock.mockImplementation(() => new Promise<SocialMessage[]>(() => undefined));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelectorAll('[data-testid="admin-diagnostics-social-channel-card"]')).toHaveLength(2);
        expect(getSocialChannelCardByLabel(container, 'Instagram').textContent).toContain('Respondidos: 1');
        expect(getSocialChannelCardByLabel(container, 'Facebook').textContent).not.toContain('Entrantes: 0');
        expect(container.textContent).toContain('Sin mensajes entrantes en WhatsApp.');
        expect(container.textContent).not.toContain('Entrantes: 0');
      });
    } finally {
      await cleanup();
    }
  });

  it('replaces repeated pre-history guidance with one shared summary when multiple channels are still awaiting their first reply', async () => {
    listInstagramMessagesMock.mockResolvedValue([
      buildMessage({
        externalId: 'instagram-pending',
        repliedAt: null,
        replyText: null,
      }),
    ]);
    listFacebookMessagesMock.mockResolvedValue([
      buildMessage({
        externalId: 'facebook-failed',
        repliedAt: null,
        replyText: null,
        replyError: 'timeout',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelectorAll('[data-testid="admin-diagnostics-social-channel-card"]')).toHaveLength(2);
        expect(container.querySelector('[data-testid="admin-diagnostics-social-awaiting-reply-summary"]')).not.toBeNull();
        expect(container.textContent).toContain(
          'Todavía no hay mensajes respondidos en Instagram y Facebook. El historial detallado aparecerá por canal cuando exista la primera respuesta.',
        );
        expect(container.textContent).toContain('Sin mensajes entrantes en WhatsApp.');
        expect(countOccurrences(container, 'El historial detallado aparecerá aquí cuando exista al menos un mensaje respondido.')).toBe(0);
        expect(container.querySelectorAll('thead')).toHaveLength(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('shows only non-zero social state chips so mixed channel panels stay scannable', async () => {
    listInstagramMessagesMock.mockResolvedValue([buildMessage({ externalId: 'instagram-replied' })]);
    listFacebookMessagesMock.mockResolvedValue([
      buildMessage({
        externalId: 'facebook-pending',
        repliedAt: null,
        replyText: null,
      }),
    ]);
    listWhatsAppMessagesMock.mockResolvedValue([
      buildMessage({
        externalId: 'whatsapp-failed',
        repliedAt: null,
        replyText: null,
        replyError: 'timeout',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.querySelectorAll('[data-testid="admin-diagnostics-social-channel-card"]')).toHaveLength(3);
        expect(container.textContent).toContain('Respondidos: 1');
        expect(container.textContent).toContain('Pendientes: 1');
        expect(container.textContent).toContain('Fallidos: 1');
        expect(container.textContent).not.toContain('Respondidos: 0');
        expect(container.textContent).not.toContain('Pendientes: 0');
        expect(container.textContent).not.toContain('Fallidos: 0');
      });
    } finally {
      await cleanup();
    }
  });

  it('hides the incoming total when a single channel state already says the same count', async () => {
    listInstagramMessagesMock.mockResolvedValue([buildMessage({ externalId: 'instagram-replied' })]);
    listFacebookMessagesMock.mockResolvedValue([
      buildMessage({ externalId: 'facebook-replied' }),
      buildMessage({
        externalId: 'facebook-pending',
        repliedAt: null,
        replyText: null,
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        const instagramCard = getSocialChannelCardByLabel(container, 'Instagram');
        const facebookCard = getSocialChannelCardByLabel(container, 'Facebook');

        expect(instagramCard.textContent).toContain('Respondidos: 1');
        expect(instagramCard.textContent).not.toContain('Entrantes: 1');
        expect(facebookCard.textContent).toContain('Entrantes: 2');
        expect(facebookCard.textContent).toContain('Respondidos: 1');
        expect(facebookCard.textContent).toContain('Pendientes: 1');
      });
    } finally {
      await cleanup();
    }
  });
});
