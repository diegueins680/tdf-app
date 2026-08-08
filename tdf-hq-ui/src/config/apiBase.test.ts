import { jest } from '@jest/globals';

const envReadMock = jest.fn<(key: string) => string | undefined>(() => undefined);

jest.unstable_mockModule('../utils/env', () => ({
  env: {
    read: envReadMock,
  },
}));

const { DEFAULT_DEPLOYED_API_BASE, resolveApiBase } = await import('./apiBase');

describe('resolveApiBase', () => {
  beforeEach(() => {
    envReadMock.mockReset().mockReturnValue(undefined);
  });

  it('uses an explicit VITE_API_BASE and normalizes trailing slashes', () => {
    envReadMock.mockReturnValue('  https://api.tdf.test///  ');

    expect(resolveApiBase({ hostname: 'tdf-app.pages.dev' })).toBe('https://api.tdf.test');
  });

  it('ignores the retired Koyeb API value on Cloudflare Pages builds', () => {
    envReadMock.mockReturnValue('https://the-dream-factory.koyeb.app/');

    expect(resolveApiBase({ hostname: 'tdf-app.pages.dev' })).toBe(DEFAULT_DEPLOYED_API_BASE);
    expect(resolveApiBase({ hostname: 'feature.tdf-app.pages.dev' })).toBe(DEFAULT_DEPLOYED_API_BASE);
    expect(resolveApiBase({ hostname: 'localhost' })).toBe('https://the-dream-factory.koyeb.app');
  });

  it('falls back to the Fly backend on the public Cloudflare Pages host', () => {
    expect(resolveApiBase({ hostname: 'tdf-app.pages.dev' })).toBe(DEFAULT_DEPLOYED_API_BASE);
  });

  it('falls back to the Fly backend on Cloudflare Pages preview hosts', () => {
    expect(resolveApiBase({ hostname: 'preview.tdf-app.pages.dev' })).toBe(DEFAULT_DEPLOYED_API_BASE);
  });

  it('keeps same-origin requests for local and unknown hosts without VITE_API_BASE', () => {
    expect(resolveApiBase({ hostname: 'localhost' })).toBe('');
    expect(resolveApiBase({ hostname: 'example.com' })).toBe('');
  });
});
