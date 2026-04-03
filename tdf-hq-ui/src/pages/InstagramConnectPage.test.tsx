import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { InstagramOAuthExchangeResponse } from '../api/instagramOAuth';
import type { MetaReviewAssetSelection } from '../services/instagramAuth';

const startAuthMock = jest.fn<(returnTo?: string) => void>();
const resetAuthMock = jest.fn<() => void>();
const getInstagramOAuthProviderMock = jest.fn<() => 'facebook' | 'instagram'>(() => 'facebook');
const getInstagramRequestedScopesMock = jest.fn<() => string[]>(() => [
  'instagram_basic',
  'instagram_manage_messages',
  'pages_show_list',
  'pages_read_engagement',
]);
const getMetaReviewAssetSelectionMock = jest.fn<() => MetaReviewAssetSelection | null>(() => null);
const getStoredInstagramResultMock = jest.fn<() => InstagramOAuthExchangeResponse | null>(() => null);
const setMetaReviewAssetSelectionMock = jest.fn<(selection: unknown) => void>();

jest.unstable_mockModule('../hooks/useInstagramAuth', () => ({
  useInstagramAuth: () => ({
    status: 'ready' as const,
    error: null,
    startAuth: startAuthMock,
    resetAuth: resetAuthMock,
  }),
}));

jest.unstable_mockModule('../services/instagramAuth', () => ({
  getInstagramOAuthProvider: () => getInstagramOAuthProviderMock(),
  getInstagramRequestedScopes: () => getInstagramRequestedScopesMock(),
  getMetaReviewAssetSelection: () => getMetaReviewAssetSelectionMock(),
  getStoredInstagramResult: () => getStoredInstagramResultMock(),
  setMetaReviewAssetSelection: (selection: unknown) => setMetaReviewAssetSelectionMock(selection),
}));

const { default: InstagramConnectPage } = await import('./InstagramConnectPage');

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

const renderPage = async (container: HTMLElement, initialEntry = '/social/instagram?review=1') => {
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <MemoryRouter initialEntries={[initialEntry]}>
        <InstagramConnectPage />
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
      document.body.removeChild(container);
    },
  };
};

describe('InstagramConnectPage', () => {
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
    startAuthMock.mockReset();
    resetAuthMock.mockReset();
    getInstagramOAuthProviderMock.mockReset();
    getInstagramRequestedScopesMock.mockReset();
    getMetaReviewAssetSelectionMock.mockReset();
    getStoredInstagramResultMock.mockReset();
    setMetaReviewAssetSelectionMock.mockReset();

    getInstagramOAuthProviderMock.mockReturnValue('facebook');
    getInstagramRequestedScopesMock.mockReturnValue([
      'instagram_basic',
      'instagram_manage_messages',
      'pages_show_list',
      'pages_read_engagement',
    ]);
    getMetaReviewAssetSelectionMock.mockReturnValue(null);
    getStoredInstagramResultMock.mockReturnValue({
      userId: 'user-1',
      userName: 'TDF Review',
      tokenType: 'Bearer',
      expiresIn: 3600,
      pages: [
        {
          pageId: '169846481310276',
          pageName: 'TDF Studio',
          instagramUserId: '17841445628242005',
          instagramUsername: 'tdf.records.label',
        },
      ],
      instagramUserId: '17841445628242005',
      instagramUsername: 'tdf.records.label',
      media: [],
    });
  });

  it('surfaces explicit business-account proof in review mode', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    await waitForExpectation(() => {
      expect(container.textContent).toContain(
        'Reviewer proof: the Instagram account below is the professional/business messaging account used in this flow. It is linked to the selected Facebook Page shown here.',
      );
      expect(container.textContent).toContain('Selected Facebook Page: TDF Studio');
      expect(container.textContent).toContain('Professional/business Instagram account: @tdf.records.label');
      expect(container.textContent).toContain('Instagram User ID: 17841445628242005');
    });

    await cleanup();
  });
});
