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
});
