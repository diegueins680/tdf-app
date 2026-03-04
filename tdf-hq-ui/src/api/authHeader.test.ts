import { jest } from '@jest/globals';

const getStoredSessionTokenMock = jest.fn<() => string | null>(() => null);

jest.unstable_mockModule('../session/SessionContext', () => ({
  getStoredSessionToken: getStoredSessionTokenMock,
}));

const { buildAuthorizationHeader } = await import('./authHeader');

describe('buildAuthorizationHeader', () => {
  beforeEach(() => {
    getStoredSessionTokenMock.mockReset();
  });

  it('returns undefined when token is missing or blank', () => {
    getStoredSessionTokenMock.mockReturnValueOnce(null).mockReturnValueOnce('   ');

    expect(buildAuthorizationHeader()).toBeUndefined();
    expect(buildAuthorizationHeader()).toBeUndefined();
  });

  it('adds a Bearer prefix to plain tokens', () => {
    getStoredSessionTokenMock.mockReturnValue('token-123');

    expect(buildAuthorizationHeader()).toBe('Bearer token-123');
  });

  it('normalizes existing bearer tokens and trims extra whitespace', () => {
    getStoredSessionTokenMock.mockReturnValue('  bearer    token-abc   ');

    expect(buildAuthorizationHeader()).toBe('Bearer token-abc');
  });

  it('returns undefined for bearer keyword without credentials', () => {
    getStoredSessionTokenMock.mockReturnValue('Bearer');

    expect(buildAuthorizationHeader()).toBeUndefined();
  });
});
