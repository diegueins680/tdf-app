import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { SocialMessage } from '../api/socialInbox';
import type { InstagramOAuthExchangeResponse } from '../api/instagramOAuth';
import type { MetaReviewAssetSelection } from '../services/instagramAuth';

const listInstagramMessagesMock = jest.fn<
  (filters?: { limit?: number; direction?: 'incoming' | 'outgoing' | 'all'; repliedOnly?: boolean }) => Promise<SocialMessage[]>
>();
const listFacebookMessagesMock = jest.fn<
  (filters?: { limit?: number; direction?: 'incoming' | 'outgoing' | 'all'; repliedOnly?: boolean }) => Promise<SocialMessage[]>
>();
const listWhatsAppMessagesMock = jest.fn<
  (filters?: { limit?: number; direction?: 'incoming' | 'outgoing' | 'all'; repliedOnly?: boolean }) => Promise<SocialMessage[]>
>();
const getInstagramOAuthProviderMock = jest.fn<() => 'facebook' | 'instagram'>();
const getInstagramRequestedScopesMock = jest.fn<() => string[]>();
const getMetaReviewAssetSelectionMock = jest.fn<() => MetaReviewAssetSelection | null>();
const getStoredInstagramResultMock = jest.fn<() => InstagramOAuthExchangeResponse | null>();

jest.unstable_mockModule('../api/socialInbox', () => ({
  SocialInboxAPI: {
    listInstagramMessages: (filters?: { limit?: number; direction?: 'incoming' | 'outgoing' | 'all'; repliedOnly?: boolean }) =>
      listInstagramMessagesMock(filters),
    listFacebookMessages: (filters?: { limit?: number; direction?: 'incoming' | 'outgoing' | 'all'; repliedOnly?: boolean }) =>
      listFacebookMessagesMock(filters),
    listWhatsAppMessages: (filters?: { limit?: number; direction?: 'incoming' | 'outgoing' | 'all'; repliedOnly?: boolean }) =>
      listWhatsAppMessagesMock(filters),
    sendReply: jest.fn(() => Promise.resolve({ status: 'ok' })),
    suggestReply: jest.fn(() => Promise.resolve('Reply drafted by AI.')),
  },
}));

jest.unstable_mockModule('../services/instagramAuth', () => ({
  getInstagramOAuthProvider: () => getInstagramOAuthProviderMock(),
  getInstagramRequestedScopes: () => getInstagramRequestedScopesMock(),
  getMetaReviewAssetSelection: () => getMetaReviewAssetSelectionMock(),
  getStoredInstagramResult: () => getStoredInstagramResultMock(),
}));

const { default: SocialInboxPage, SocialMessageDialog } = await import('./SocialInboxPage');

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

const renderPage = async (container: HTMLElement, initialEntry = '/social/inbox?review=1') => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter
        initialEntries={[initialEntry]}
        future={{ v7_startTransition: true, v7_relativeSplatPath: true }}
      >
        <QueryClientProvider client={qc}>
          <SocialInboxPage />
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

const renderDialog = async (container: HTMLElement, selection: { channel: 'instagram'; message: SocialMessage }) => {
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <SocialMessageDialog
        selection={selection}
        reviewMode
        activeAsset={null}
        onClose={() => undefined}
        onRefresh={() => undefined}
      />,
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
      document.body.removeChild(container);
    },
  };
};

const countInstagramSetupLinks = (root: ParentNode) =>
  root.querySelectorAll('a[href="/social/instagram?review=1"]').length;

const countButtonsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('button')).filter((candidate) => (candidate.textContent ?? '').trim() === labelText).length;

const countInteractiveElementsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('button, a')).filter((candidate) => (candidate.textContent ?? '').trim() === labelText).length;

const countTextOccurrences = (root: ParentNode, text: string) =>
  ((root.textContent ?? '').split(text).length - 1);

const getLinkByText = (root: ParentNode, labelText: string) => {
  const link = Array.from(root.querySelectorAll('a')).find((candidate) => (candidate.textContent ?? '').trim() === labelText);
  if (!(link instanceof HTMLAnchorElement)) {
    throw new Error(`Link not found: ${labelText}`);
  }
  return link;
};

const buildMessage = (overrides: Partial<SocialMessage> = {}): SocialMessage => ({
  externalId: 'msg-1',
  senderId: 'sender-1',
  senderName: 'Ada',
  text: 'Hello there',
  metadata: null,
  direction: 'incoming',
  repliedAt: null,
  replyText: null,
  replyError: null,
  createdAt: '2030-01-02T03:04:05.000Z',
  ...overrides,
});

const hasLabel = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('label')).some((el) => {
    const text = (el.textContent ?? '').replace('*', '').trim();
    return text === labelText;
  });

const hasTableHeader = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('th')).some((cell) => (cell.textContent ?? '').trim() === labelText);

const queryFilterChip = (root: ParentNode, labelText: string) =>
  root.querySelector(`[aria-label="Filter inbox by ${labelText}"]`);

const getTextControlByLabel = (root: ParentNode, labelText: string) => {
  const label = Array.from(root.querySelectorAll('label')).find((candidate) => {
    const text = (candidate.textContent ?? '').replace('*', '').trim();
    return text === labelText;
  });
  const control = label instanceof HTMLLabelElement ? label.control : null;
  if (control instanceof HTMLInputElement || control instanceof HTMLTextAreaElement) {
    return control;
  }
  throw new Error(`Text control not found: ${labelText}`);
};

const setTextControlValue = (control: HTMLInputElement | HTMLTextAreaElement, value: string) => {
  const prototype = control instanceof HTMLTextAreaElement ? HTMLTextAreaElement.prototype : HTMLInputElement.prototype;
  const valueSetter = Object.getOwnPropertyDescriptor(prototype, 'value')?.set?.bind(control);
  valueSetter?.(value);
  control.dispatchEvent(new Event('input', { bubbles: true }));
};

describe('SocialInboxPage', () => {
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
    getInstagramOAuthProviderMock.mockReset();
    getInstagramRequestedScopesMock.mockReset();
    getMetaReviewAssetSelectionMock.mockReset();
    getStoredInstagramResultMock.mockReset();

    listInstagramMessagesMock.mockResolvedValue([]);
    listFacebookMessagesMock.mockResolvedValue([]);
    listWhatsAppMessagesMock.mockResolvedValue([]);
    getInstagramOAuthProviderMock.mockReturnValue('facebook');
    getInstagramRequestedScopesMock.mockReturnValue([
      'instagram_basic',
      'instagram_manage_messages',
      'pages_show_list',
      'pages_read_engagement',
    ]);
    getMetaReviewAssetSelectionMock.mockReturnValue(null);
    getStoredInstagramResultMock.mockReturnValue(null);
  });

  it('shows a single setup CTA for first-time review runs', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(
        'Step 1/3: select the exact Page + professional/business account for this review run.',
      );
      expect(container.textContent).toContain('Review run: Facebook Login');
      expect(container.textContent).toContain(
        'Requested scopes: instagram_basic, instagram_manage_messages, pages_show_list, pages_read_engagement',
      );
      expect(container.textContent).toContain(
        'No asset selected yet. Go to Instagram setup and select the exact Page + professional/business account first.',
      );
      expect(container.textContent).not.toContain('Proof order: open the inbound thread');
      expect(countInstagramSetupLinks(container)).toBe(1);
      expect(getLinkByText(container, 'Select asset in Instagram setup').getAttribute('href')).toBe('/social/instagram?review=1');
      expect(container.textContent).not.toContain('Recording checklist');
      expect(container.textContent).not.toContain('App Review mode auto-refreshes every 5 seconds');
      expect(container.textContent).not.toContain('Open Instagram setup');
      expect(container.textContent).not.toContain('Re-select asset');
      expect(container.textContent).not.toContain('Change selected asset');
    });

    await cleanup();
  });

  it('keeps selected-asset empty review runs focused on the next inbound message', async () => {
    getMetaReviewAssetSelectionMock.mockReturnValue({
      pageId: 'page-1',
      pageName: 'TDF Review Page',
      instagramUserId: 'ig-user-1',
      instagramUsername: 'tdfreview',
      selectedAt: 1_763_000_000_000,
    });

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(
        'Step 1/3 complete: send one inbound test message to the selected professional/business account.',
      );
      expect(container.textContent).toContain('Review run: Facebook Login');
      expect(container.textContent).not.toContain('Proof order: open the inbound thread');
      expect(container.textContent).not.toContain('Recording checklist');
      expect(container.textContent).toContain(
        'Selected professional/business Instagram messaging asset: TDF Review Page (Page ID: page-1',
      );
      expect(container.textContent).toContain('@tdfreview');
      expect(container.textContent).toContain('IG User ID: ig-user-1');
      expect(container.textContent).toContain('No inbound messages yet.');
      expect(container.textContent).toContain(
        'Send one test message to the selected professional/business account. Status filters and channel panels appear here after the first inbound message arrives.',
      );
      expect(container.textContent).not.toContain(
        'App Review mode auto-refreshes every 5 seconds so deleted or unsent messages disappear from the inbox without a manual reload.',
      );
      expect(countTextOccurrences(
        container,
        'App Review mode auto-refreshes every 5 seconds so deleted or unsent messages disappear from the inbox without a manual reload.',
      )).toBe(0);
      expect(container.textContent).not.toContain(
        'The inbox updates automatically; status filters and channel panels appear after the first inbound message arrives.',
      );
      expect(countInstagramSetupLinks(container)).toBe(1);
      expect(getLinkByText(container, 'Change selected asset').getAttribute('href')).toBe('/social/instagram?review=1');
      expect(container.textContent).not.toContain('Select asset in Instagram setup');
      expect(container.textContent).not.toContain('Open Instagram setup');
      expect(container.textContent).not.toContain('Re-select asset');
    });

    await cleanup();
  });

  it('keeps first-time App Review setup focused on asset selection instead of duplicate refresh guidance', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(
        'Step 1/3: select the exact Page + professional/business account for this review run.',
      );
      expect(container.textContent).not.toContain(
        'App Review mode auto-refreshes every 5 seconds so deleted or unsent messages disappear from the inbox without a manual reload.',
      );
      expect(countButtonsByText(container, 'Refresh')).toBe(0);
      expect(countButtonsByText(container, 'Actualizar')).toBe(0);
    });

    await cleanup();
  });

  it('keeps the manual refresh action in the normal inbox where auto-refresh is off', async () => {
    listInstagramMessagesMock.mockResolvedValue([
      buildMessage(),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/social/inbox');

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Inbox social');
      expect(countButtonsByText(container, 'Actualizar')).toBe(1);
      expect(countButtonsByText(container, 'Refresh')).toBe(0);
      expect(container.textContent).not.toContain('App Review mode auto-refreshes every 5 seconds');
    });

    await cleanup();
  });

  it('moves normal first-run refresh into the empty inbox guidance instead of a generic header action', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container, '/social/inbox');

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Inbox social');
      expect(container.textContent).toContain('Todavia no hay mensajes entrantes.');
      expect(container.textContent).toContain(
        'Cuando llegue el primer mensaje entrante, aparecera aqui y se activaran los filtros por estado. Usa Actualizar inbox si esperabas uno ahora.',
      );
      expect(countButtonsByText(container, 'Actualizar inbox')).toBe(1);
      expect(countButtonsByText(container, 'Actualizar')).toBe(0);
      expect(container.querySelectorAll('table')).toHaveLength(0);
      expect(container.querySelectorAll('[aria-label^="Filtrar inbox por "]')).toHaveLength(0);
    });

    await cleanup();
  });

  it('keeps the first empty review run focused on setup until an asset is selected', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelectorAll('[aria-label^="Filter inbox by "]')).toHaveLength(0);
      expect(container.querySelectorAll('table')).toHaveLength(0);
      expect(hasLabel(container, 'Limit')).toBe(false);
      expect(container.textContent).toContain(
        'Step 1/3: select the exact Page + professional/business account for this review run.',
      );
      expect(container.textContent).toContain(
        'No asset selected yet. Go to Instagram setup and select the exact Page + professional/business account first.',
      );
      expect(container.textContent).not.toContain('Recording checklist');
      expect(container.textContent).not.toContain('No inbound messages yet.');
      expect(container.textContent).not.toContain(
        'Select the review asset, send one test message, and wait a few seconds. The inbox updates automatically; status filters and channel panels appear after the first inbound message arrives.',
      );
      expect(container.textContent).not.toContain('then refresh');
      expect(container.textContent).not.toContain('No messages for this filter.');
      expect(container.textContent).not.toContain('Status available');
    });

    await cleanup();
  });

  it('hides inactive filters and empty channel tables when channel fetches fail before any messages load', async () => {
    getMetaReviewAssetSelectionMock.mockReturnValue({
      pageId: 'page-1',
      pageName: 'TDF Review Page',
      instagramUserId: 'ig-user-1',
      instagramUsername: 'tdfreview',
      selectedAt: 1_763_000_000_000,
    });
    listInstagramMessagesMock.mockRejectedValue(new Error('403 forbidden'));
    listFacebookMessagesMock.mockRejectedValue(new Error('403 forbidden'));
    listWhatsAppMessagesMock.mockRejectedValue(new Error('403 forbidden'));

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Selected professional/business Instagram messaging asset: TDF Review Page');
      expect(container.textContent).toContain('Instagram: Cannot load messages: insufficient permissions (403).');
      expect(container.textContent).toContain('Facebook: Cannot load messages: insufficient permissions (403).');
      expect(container.textContent).toContain('WhatsApp: Cannot load messages: insufficient permissions (403).');
      expect(container.textContent).toContain('Review channel credentials and app/page permissions.');
      expect(container.querySelectorAll('[aria-label^="Filter inbox by "]')).toHaveLength(0);
      expect(container.querySelectorAll('table')).toHaveLength(0);
      expect(container.textContent).not.toContain('No messages for this filter.');
      expect(container.textContent).not.toContain('Only statuses with inbound messages in this view are shown.');
    });

    await cleanup();
  });

  it('keeps zero-result filters hidden while reserving per-channel status chips for the all-messages view', async () => {
    listInstagramMessagesMock.mockResolvedValue([
      buildMessage(),
    ]);
    listFacebookMessagesMock.mockResolvedValue([
      buildMessage({
        externalId: 'msg-2',
        senderId: 'sender-2',
        senderName: 'Grace',
        repliedAt: '2030-01-03T03:04:05.000Z',
        replyText: 'Done.',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Limit')).toBe(false);
      expect(queryFilterChip(container, 'All')).not.toBeNull();
      expect(queryFilterChip(container, 'Pending')).not.toBeNull();
      expect(queryFilterChip(container, 'Replied')).not.toBeNull();
      expect(queryFilterChip(container, 'Failed')).toBeNull();
      expect(queryFilterChip(container, 'All')?.textContent).toContain('All (2)');
      expect(queryFilterChip(container, 'Pending')?.textContent).toContain('Pending (1)');
      expect(queryFilterChip(container, 'Replied')?.textContent).toContain('Replied (1)');
      expect(container.textContent).toContain('Only statuses with inbound messages in this view are shown.');
      expect(container.textContent).toContain('Inbound: 1');
      expect(container.textContent).not.toContain('Pending: 1');
      expect(container.textContent).not.toContain('Replied: 1');
      expect(container.textContent).not.toContain('Pending: 0');
      expect(container.textContent).not.toContain('Replied: 0');
      expect(container.textContent).not.toContain('Failed: 0');
    });

    await act(async () => {
      queryFilterChip(container, 'All')?.dispatchEvent(new MouseEvent('click', { bubbles: true }));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Pending: 1');
      expect(container.textContent).toContain('Replied: 1');
    });

    await cleanup();
  });

  it('opens the full inbox instead of an empty default status when only non-pending messages exist', async () => {
    listInstagramMessagesMock.mockResolvedValue([
      buildMessage({
        repliedAt: '2030-01-03T03:04:05.000Z',
        replyText: 'Done.',
      }),
    ]);
    listFacebookMessagesMock.mockResolvedValue([
      buildMessage({
        externalId: 'msg-2',
        senderId: 'sender-2',
        senderName: 'Grace',
        replyError: 'Delivery failed.',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(queryFilterChip(container, 'All')).not.toBeNull();
      expect(queryFilterChip(container, 'All')?.getAttribute('aria-pressed')).toBe('true');
      expect(queryFilterChip(container, 'Pending')).toBeNull();
      expect(queryFilterChip(container, 'Replied')).not.toBeNull();
      expect(queryFilterChip(container, 'Failed')).not.toBeNull();
      expect(container.textContent).toContain('Ada');
      expect(container.textContent).toContain('Grace');
      expect(container.textContent).not.toContain(
        'No messages match Pending in this view. Use All or a status with a count to see existing inbound messages.',
      );
    });

    await cleanup();
  });

  it('shows the limit control only after the inbox actually reaches the current fetch cap', async () => {
    listInstagramMessagesMock.mockResolvedValue(
      Array.from({ length: 100 }, (_, index) =>
        buildMessage({
          externalId: `msg-${index + 1}`,
          senderId: `sender-${index + 1}`,
          senderName: `Sender ${index + 1}`,
        })),
    );

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(hasLabel(container, 'Limit')).toBe(true);
      expect(container.querySelectorAll('[aria-label^="Filter inbox by "]')).toHaveLength(0);
      expect(container.textContent).toContain('Status available');
      expect(container.textContent).toContain('Pending');
      expect(container.textContent).toContain('Instagram');
      expect(container.textContent).toContain('Inbound: 100');
    });

    await cleanup();
  });

  it('replaces a single real inbox filter with context copy when the current view does not need status filtering', async () => {
    listInstagramMessagesMock.mockResolvedValue([
      buildMessage(),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelectorAll('[aria-label^="Filter inbox by "]')).toHaveLength(0);
      expect(container.textContent).toContain('Status available');
      expect(container.textContent).toContain('Pending');
      expect(container.textContent).toContain('No need to filter it: it is the only inbound status in this view.');
      expect(container.textContent).not.toContain('Pending: 1');
      expect(container.textContent).not.toContain('Only statuses with inbound messages in this view are shown.');
    });

    await cleanup();
  });

  it('hides reply-only columns until the current view actually includes reply data', async () => {
    listInstagramMessagesMock.mockResolvedValue([
      buildMessage(),
      buildMessage({
        externalId: 'msg-2',
        senderId: 'sender-2',
        senderName: 'Grace',
        repliedAt: '2030-01-03T03:04:05.000Z',
        replyText: 'Done.',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(queryFilterChip(container, 'Pending')).not.toBeNull();
      expect(hasTableHeader(container, 'Received')).toBe(true);
      expect(hasTableHeader(container, 'Sender')).toBe(true);
      expect(hasTableHeader(container, 'Message')).toBe(true);
      expect(hasTableHeader(container, 'Replied')).toBe(false);
      expect(hasTableHeader(container, 'Reply / Error')).toBe(false);
      expect(container.textContent).toContain('Ada');
      expect(container.textContent).not.toContain('Grace');
    });

    await act(async () => {
      queryFilterChip(container, 'Replied')?.dispatchEvent(new MouseEvent('click', { bubbles: true }));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(hasTableHeader(container, 'Replied')).toBe(true);
      expect(hasTableHeader(container, 'Reply / Error')).toBe(true);
      expect(container.textContent).toContain('Grace');
      expect(container.textContent).not.toContain('Ada');
    });

    await cleanup();
  });

  it('hides duplicate empty channel panels when another channel already has messages', async () => {
    listInstagramMessagesMock.mockResolvedValue([
      buildMessage(),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelectorAll('table')).toHaveLength(1);
      expect(container.textContent).toContain(
        'Showing only channels with messages in this view. No messages right now: Facebook, WhatsApp.',
      );
      expect(container.textContent).not.toContain('No messages for this filter.');
    });

    await cleanup();
  });

  it('keeps a single native-client CTA in the review dialog once a reply already exists', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderDialog(container, {
      channel: 'instagram',
      message: buildMessage({
        repliedAt: '2030-01-03T03:04:05.000Z',
        replyText: 'Done.',
      }),
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Reply from app UI');
      expect(document.body.textContent).toContain(
        'Step 3 of 3: show this exact text in the native client (Instagram/Messenger/WhatsApp): “Done.”',
      );
      expect(countInteractiveElementsByText(document.body, 'Open native client')).toBe(1);
    });

    await cleanup();
  });

  it('treats an already delivered reply as proof instead of preloading it as a duplicate send draft', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderDialog(container, {
      channel: 'instagram',
      message: buildMessage({
        repliedAt: '2030-01-03T03:04:05.000Z',
        replyText: 'Done.',
      }),
    });

    await waitForExpectation(() => {
      const followUpInput = getTextControlByLabel(document.body, 'Follow-up message');
      const sendButton = Array.from(document.body.querySelectorAll('button')).find(
        (candidate) => (candidate.textContent ?? '').trim() === 'Send message',
      );
      expect(followUpInput.value).toBe('');
      expect(sendButton).toBeInstanceOf(HTMLButtonElement);
      expect((sendButton as HTMLButtonElement).disabled).toBe(true);
      expect(countButtonsByText(document.body, 'Copy')).toBe(0);
      expect(countButtonsByText(document.body, 'Clear')).toBe(0);
      expect(hasLabel(document.body, 'AI instructions (optional)')).toBe(false);
      expect(countButtonsByText(document.body, 'Generate with AI')).toBe(0);
      expect(document.body.textContent).not.toContain('Step 2 of 3: keep this dialog visible, click Send');
      expect(document.body.textContent).not.toContain('Explain each button while recording');
      expect(countTextOccurrences(document.body, 'Step 3 of 3:')).toBe(1);
      expect(document.body.textContent).toContain(
        'Already replied. Send a follow-up only if the review run needs a second app message.',
      );
      expect(document.body.textContent).toContain(
        'Step 3 of 3: show this exact text in the native client (Instagram/Messenger/WhatsApp): “Done.”',
      );
    });

    await cleanup();
  });

  it('keeps the review reply dialog footer focused on sending instead of duplicating close actions', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderDialog(container, {
      channel: 'instagram',
      message: buildMessage(),
    });

    await waitForExpectation(() => {
      expect(document.body.querySelectorAll('button[aria-label="Close"]')).toHaveLength(1);
      expect(countButtonsByText(document.body, 'Close')).toBe(0);
      expect(countButtonsByText(document.body, 'Send message')).toBe(1);
    });

    await cleanup();
  });

  it('hides the AI draft controls for attachment-only review messages', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderDialog(container, {
      channel: 'instagram',
      message: buildMessage({
        text: '[attachment]',
        metadata: JSON.stringify({
          attachments: [
            {
              type: 'image',
              payload: { url: 'https://example.com/proof.jpg' },
            },
          ],
        }),
      }),
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Attachments');
      expect(document.body.textContent).toContain(
        'Explain the attachment, message textarea, and Send action while recording. AI draft is hidden because this message has no text body.',
      );
      expect(hasLabel(document.body, 'AI instructions (optional)')).toBe(false);
      expect(hasLabel(document.body, 'Outgoing message')).toBe(true);
      expect(countButtonsByText(document.body, 'Generate with AI')).toBe(0);
      expect(countButtonsByText(document.body, 'Send message')).toBe(1);
    });

    await cleanup();
  });

  it('hides reply draft utilities until there is draft text to copy or clear', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderDialog(container, {
      channel: 'instagram',
      message: buildMessage(),
    });

    await waitForExpectation(() => {
      expect(document.body.textContent).toContain('Reply from app UI');
      expect(countButtonsByText(document.body, 'Copy')).toBe(0);
      expect(countButtonsByText(document.body, 'Clear')).toBe(0);
    });

    await act(async () => {
      setTextControlValue(getTextControlByLabel(document.body, 'Outgoing message'), 'Thanks for reaching out.');
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countButtonsByText(document.body, 'Copy')).toBe(1);
      expect(countButtonsByText(document.body, 'Clear')).toBe(1);
    });

    await act(async () => {
      const clearButton = Array.from(document.body.querySelectorAll('button')).find(
        (candidate) => (candidate.textContent ?? '').trim() === 'Clear',
      );
      if (!clearButton) throw new Error('Clear button not found');
      clearButton.dispatchEvent(new MouseEvent('click', { bubbles: true }));
      await flushPromises();
      await flushPromises();
    });

    await waitForExpectation(() => {
      expect(countButtonsByText(document.body, 'Copy')).toBe(0);
      expect(countButtonsByText(document.body, 'Clear')).toBe(0);
    });

    await cleanup();
  });

  it('shows the only available status immediately when the default filter has no messages', async () => {
    listInstagramMessagesMock.mockResolvedValue([
      buildMessage({
        repliedAt: '2030-01-03T03:04:05.000Z',
        replyText: 'Done.',
      }),
    ]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelectorAll('[aria-label^="Filter inbox by "]')).toHaveLength(0);
      expect(queryFilterChip(container, 'All')).toBeNull();
      expect(queryFilterChip(container, 'Pending')).toBeNull();
      expect(queryFilterChip(container, 'Replied')).toBeNull();
      expect(container.textContent).toContain('Status available');
      expect(container.textContent).toContain('Replied');
      expect(container.textContent).toContain('No need to filter it: it is the only inbound status in this view.');
      expect(container.querySelectorAll('table')).toHaveLength(1);
      expect(hasTableHeader(container, 'Replied')).toBe(true);
      expect(hasTableHeader(container, 'Reply / Error')).toBe(true);
      expect(container.textContent).toContain('Ada');
      expect(container.textContent).toContain('Done.');
      expect(container.textContent).toContain(
        'Showing only channels with messages in this view. No messages right now: Facebook, WhatsApp.',
      );
      expect(container.textContent).not.toContain('No messages for this filter.');
      expect(container.textContent).not.toContain(
        'No messages match Pending in this view. Use All or a status with a count to see existing inbound messages.',
      );
      expect(container.textContent).not.toContain('Only statuses with inbound messages in this view are shown.');
    });

    await cleanup();
  });
});
