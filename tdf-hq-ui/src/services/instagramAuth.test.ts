import { jest } from '@jest/globals';

interface InstagramAuthModule {
  getInstagramOAuthProvider: () => 'facebook' | 'instagram';
  getInstagramRequestedScopes: () => string[];
  buildInstagramAuthUrl: (returnTo?: string) => string;
  parseInstagramState: (state: string | null) => Record<string, unknown> | null;
}

const loadInstagramAuth = async (envValues: Record<string, string | undefined>) => {
  jest.resetModules();

  const envReadMock = jest.fn<(key: string) => string | undefined>((key) => envValues[key]);

  jest.unstable_mockModule('../utils/env', () => ({
    env: {
      read: envReadMock,
    },
  }));

  const exchangeMock = jest.fn();
  jest.unstable_mockModule('../api/instagramOAuth', () => ({
    InstagramOAuthAPI: {
      exchange: exchangeMock,
    },
  }));

  const module = (await import('./instagramAuth')) as unknown as InstagramAuthModule;
  return { module, envReadMock };
};

describe('instagramAuth service', () => {
  beforeEach(() => {
    sessionStorage.clear();
    localStorage.clear();
  });

  it('falls back to classic Facebook Instagram scopes when business-only scopes cannot use Instagram Login', async () => {
    const { module } = await loadInstagramAuth({
      VITE_FACEBOOK_APP_ID: 'fb-app-id',
      VITE_INSTAGRAM_SCOPES: 'instagram_business_basic instagram_business_manage_messages',
      VITE_INSTAGRAM_OAUTH_PROVIDER: 'instagram',
    });

    expect(module.getInstagramOAuthProvider()).toBe('facebook');

    const scopes = module.getInstagramRequestedScopes();
    expect(scopes).toEqual(
      expect.arrayContaining([
        'instagram_basic',
        'instagram_manage_messages',
        'pages_show_list',
        'pages_read_engagement',
      ]),
    );
    expect(scopes).not.toEqual(expect.arrayContaining(['instagram_business_basic']));
    expect(scopes).not.toEqual(expect.arrayContaining(['instagram_business_manage_messages']));
  });

  it('keeps only business scopes when Instagram Login is available', async () => {
    const { module } = await loadInstagramAuth({
      VITE_INSTAGRAM_CLIENT_ID: 'ig-app-id',
      VITE_INSTAGRAM_SCOPES: 'instagram_business_basic, instagram_business_manage_messages',
    });

    expect(module.getInstagramOAuthProvider()).toBe('instagram');
    expect(module.getInstagramRequestedScopes()).toEqual([
      'instagram_business_basic',
      'instagram_business_manage_messages',
    ]);
  });

  it('generates a fixed-length hexadecimal nonce even without Web Crypto APIs', async () => {
    const cryptoDescriptor = Object.getOwnPropertyDescriptor(globalThis, 'crypto');
    Object.defineProperty(globalThis, 'crypto', {
      value: undefined,
      configurable: true,
    });
    const mathRandomSpy = jest.spyOn(Math, 'random').mockReturnValue(0.123456789);

    try {
      const { module } = await loadInstagramAuth({
        VITE_FACEBOOK_APP_ID: 'fb-app-id',
      });

      const authUrl = module.buildInstagramAuthUrl('/social/instagram');
      const state = new URL(authUrl).searchParams.get('state');
      const parsedState = module.parseInstagramState(state) as { nonce?: string } | null;

      expect(parsedState?.nonce).toMatch(/^[0-9a-f]{32}$/);
    } finally {
      mathRandomSpy.mockRestore();
      if (cryptoDescriptor) {
        Object.defineProperty(globalThis, 'crypto', cryptoDescriptor);
      } else {
        delete (globalThis as { crypto?: unknown }).crypto;
      }
    }
  });
});
