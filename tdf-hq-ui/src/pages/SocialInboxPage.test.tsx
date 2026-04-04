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
  getMetaReviewAssetSelection: () => getMetaReviewAssetSelectionMock(),
  getStoredInstagramResult: () => getStoredInstagramResultMock(),
}));

const { default: SocialInboxPage } = await import('./SocialInboxPage');

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

const countInstagramSetupLinks = (root: ParentNode) =>
  root.querySelectorAll('a[href="/social/instagram?review=1"]').length;

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
    getMetaReviewAssetSelectionMock.mockReset();
    getStoredInstagramResultMock.mockReset();

    listInstagramMessagesMock.mockResolvedValue([]);
    listFacebookMessagesMock.mockResolvedValue([]);
    listWhatsAppMessagesMock.mockResolvedValue([]);
    getMetaReviewAssetSelectionMock.mockReturnValue(null);
    getStoredInstagramResultMock.mockReturnValue(null);
  });

  it('shows a single setup CTA for first-time review runs', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain('Recording checklist');
      expect(container.textContent).toContain('No asset selected yet. Go to Instagram setup and select the Page/account first.');
      expect(countInstagramSetupLinks(container)).toBe(1);
      expect(getLinkByText(container, 'Select asset in Instagram setup').getAttribute('href')).toBe('/social/instagram?review=1');
      expect(container.textContent).not.toContain('Open Instagram setup');
      expect(container.textContent).not.toContain('Re-select asset');
      expect(container.textContent).not.toContain('Change selected asset');
    });

    await cleanup();
  });

  it('keeps one setup CTA after an asset is already selected', async () => {
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
        'Selected professional/business Instagram messaging asset: TDF Review Page (Page ID: page-1',
      );
      expect(container.textContent).toContain('@tdfreview');
      expect(container.textContent).toContain('IG User ID: ig-user-1');
      expect(countInstagramSetupLinks(container)).toBe(1);
      expect(getLinkByText(container, 'Change selected asset').getAttribute('href')).toBe('/social/instagram?review=1');
      expect(container.textContent).not.toContain('Select asset in Instagram setup');
      expect(container.textContent).not.toContain('Open Instagram setup');
      expect(container.textContent).not.toContain('Re-select asset');
    });

    await cleanup();
  });

  it('shows one guided empty state instead of empty filters and channel tables when inbox is empty', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.querySelectorAll('[aria-label^="Filter inbox by "]')).toHaveLength(0);
      expect(container.querySelectorAll('table')).toHaveLength(0);
      expect(hasLabel(container, 'Limit')).toBe(false);
      expect(container.textContent).toContain('No inbound messages yet.');
      expect(container.textContent).toContain(
        'Select the review asset, send one test message, then refresh. Status filters and channel panels appear after the first inbound message arrives.',
      );
      expect(container.textContent).not.toContain('No messages for this filter.');
      expect(container.textContent).not.toContain('Status available');
    });

    await cleanup();
  });

  it('hides zero-result inbox filters once inbound statuses are known', async () => {
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
      expect(container.textContent).toContain('Pending: 1');
      expect(container.textContent).toContain('Replied: 1');
      expect(container.textContent).not.toContain('Pending: 0');
      expect(container.textContent).not.toContain('Replied: 0');
      expect(container.textContent).not.toContain('Failed: 0');
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

  it('keeps inbox filters visible when the default filter would hide the only available status', async () => {
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
      expect(queryFilterChip(container, 'All')).not.toBeNull();
      expect(queryFilterChip(container, 'Pending')).not.toBeNull();
      expect(queryFilterChip(container, 'Replied')).not.toBeNull();
      expect(queryFilterChip(container, 'Failed')).toBeNull();
      expect(container.textContent).toContain('Only statuses with inbound messages in this view are shown.');
      expect(container.textContent).toContain('No messages for this filter.');
      expect(container.textContent).not.toContain('Status available');
    });

    await cleanup();
  });
});
