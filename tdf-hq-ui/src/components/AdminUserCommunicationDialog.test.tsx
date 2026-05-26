import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type {
  AdminUser,
  AdminWhatsAppResendPayload,
  AdminWhatsAppSendPayload,
  AdminWhatsAppSendResponse,
  UserCommunicationHistory,
  WhatsAppMessageAdmin,
} from '../api/admin';

const getUserCommunicationHistoryMock = jest.fn<(userId: number, limit?: number) => Promise<UserCommunicationHistory>>();
const sendUserWhatsAppMock = jest.fn<(
  userId: number,
  payload: AdminWhatsAppSendPayload,
) => Promise<AdminWhatsAppSendResponse>>();
const resendWhatsAppMessageMock = jest.fn<(
  messageId: number,
  payload?: AdminWhatsAppResendPayload,
) => Promise<AdminWhatsAppSendResponse>>();

jest.unstable_mockModule('../api/admin', () => ({
  Admin: {
    getUserCommunicationHistory: (userId: number, limit?: number) =>
      getUserCommunicationHistoryMock(userId, limit),
    sendUserWhatsApp: (userId: number, payload: AdminWhatsAppSendPayload) =>
      sendUserWhatsAppMock(userId, payload),
    resendWhatsAppMessage: (messageId: number, payload?: AdminWhatsAppResendPayload) =>
      resendWhatsAppMessageMock(messageId, payload),
  },
}));

const { default: AdminUserCommunicationDialog } = await import('./AdminUserCommunicationDialog');

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
  Array.from(root.querySelectorAll<HTMLElement>('button, a')).filter(
    (element) => buttonText(element) === labelText || element.getAttribute('aria-label') === labelText,
  );

const buildUser = (overrides: Partial<AdminUser> = {}): AdminUser => ({
  userId: 101,
  partyId: 9,
  partyName: 'Ada Lovelace',
  username: 'ada-admin',
  primaryEmail: 'ada@example.com',
  primaryPhone: '+593999000111',
  whatsapp: '+593999000111',
  active: true,
  roles: ['Admin'],
  modules: ['admin'],
  ...overrides,
});

const buildHistory = (overrides: Partial<UserCommunicationHistory> = {}): UserCommunicationHistory => ({
  userId: 101,
  partyId: 9,
  partyName: 'Ada Lovelace',
  username: 'ada-admin',
  primaryEmail: 'ada@example.com',
  primaryPhone: '+593999000111',
  whatsapp: '+593999000111',
  messages: [],
  ...overrides,
});

const buildMessage = (overrides: Partial<WhatsAppMessageAdmin> = {}): WhatsAppMessageAdmin => ({
  id: 501,
  externalId: 'wa-501',
  partyId: 9,
  actorPartyId: null,
  senderId: '+593999000111',
  senderName: 'Ada Lovelace',
  phoneE164: '+593999000111',
  contactEmail: 'ada@example.com',
  text: 'Hola, necesito ayuda con mi reserva.',
  direction: 'incoming',
  replyStatus: 'pending',
  deliveryStatus: 'received',
  source: 'whatsapp',
  createdAt: '2030-01-02T12:00:00.000Z',
  ...overrides,
});

const renderDialog = async (container: HTMLElement, user = buildUser()) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter future={{ v7_startTransition: true, v7_relativeSplatPath: true }}>
        <QueryClientProvider client={qc}>
          <AdminUserCommunicationDialog open user={user} onClose={() => undefined} />
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

describe('AdminUserCommunicationDialog', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    getUserCommunicationHistoryMock.mockReset();
    sendUserWhatsAppMock.mockReset();
    resendWhatsAppMessageMock.mockReset();
    getUserCommunicationHistoryMock.mockResolvedValue(buildHistory());
    sendUserWhatsAppMock.mockResolvedValue({
      status: 'ok',
      deliveryStatus: 'sent',
      message: 'Notificación enviada.',
    });
    resendWhatsAppMessageMock.mockResolvedValue({
      status: 'ok',
      deliveryStatus: 'sent',
      message: 'Mensaje reenviado.',
    });
  });

  it('keeps the empty WhatsApp composer focused on the available notify action', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderDialog(container);

    try {
      await waitForExpectation(() => {
        expect(getUserCommunicationHistoryMock).toHaveBeenCalledWith(101, undefined);
        expect(document.body.textContent).toContain('No hay mensajes registrados para este usuario.');
        expect(getButtonsByText(document.body, 'Notificar')).toHaveLength(1);
        expect(getButtonsByText(document.body, 'Responder')).toHaveLength(0);
      });
    } finally {
      await cleanup();
    }
  });

  it('lets keyboard users cancel a selected reply target from the composer', async () => {
    getUserCommunicationHistoryMock.mockResolvedValue(buildHistory({
      messages: [buildMessage()],
    }));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderDialog(container);

    try {
      await waitForExpectation(() => {
        expect(getButtonsByText(document.body, 'Responder')).toHaveLength(1);
      });

      await act(async () => {
        getButtonsByText(document.body, 'Responder')[0]?.click();
        await flushPromises();
      });

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Responderás al mensaje del');
        expect(getButtonsByText(document.body, 'Cancelar respuesta')).toHaveLength(1);
        expect(getButtonsByText(document.body, 'Responder')).toHaveLength(2);
      });

      const composer = document.body.querySelector<HTMLTextAreaElement>('textarea');
      if (!composer) throw new Error('Composer textarea not found');

      await waitForExpectation(() => {
        expect(document.activeElement).toBe(composer);
      });

      await act(async () => {
        composer.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true, cancelable: true }));
        await flushPromises();
      });

      await waitForExpectation(() => {
        const activeComposer = document.body.querySelector<HTMLTextAreaElement>('textarea');
        expect(document.body.textContent).not.toContain('Responderás al mensaje del');
        expect(getButtonsByText(document.body, 'Cancelar respuesta')).toHaveLength(0);
        expect(getButtonsByText(document.body, 'Responder')).toHaveLength(1);
        expect(document.activeElement).toBe(activeComposer);
      });
    } finally {
      await cleanup();
    }
  });
});
