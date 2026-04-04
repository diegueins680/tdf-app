import { getStoredSessionToken, parseStoredSession, SESSION_STORAGE_KEY, setTransientApiToken } from './SessionContext';

describe('getStoredSessionToken', () => {
  beforeEach(() => {
    window.localStorage.removeItem(SESSION_STORAGE_KEY);
    window.sessionStorage.removeItem(SESSION_STORAGE_KEY);
    setTransientApiToken(null);
  });

  afterEach(() => {
    setTransientApiToken(null);
  });

  it('does not infer a token from persisted session metadata', () => {
    window.localStorage.setItem(
      SESSION_STORAGE_KEY,
      JSON.stringify({
        username: 'alice',
        displayName: 'Alice',
        roles: ['Admin'],
        modules: ['CRM'],
      }),
    );

    expect(getStoredSessionToken()).toBeNull();
  });

  it('uses a transient token for public pages without storing it in browser storage', () => {
    setTransientApiToken(' transient-token ');

    expect(getStoredSessionToken()).toBe('transient-token');
    expect(window.localStorage.getItem(SESSION_STORAGE_KEY)).toBeNull();
    expect(window.sessionStorage.getItem(SESSION_STORAGE_KEY)).toBeNull();
  });
});

describe('parseStoredSession', () => {
  it('returns null when username is blank', () => {
    const parsed = parseStoredSession(
      JSON.stringify({
        username: '   ',
        roles: ['Admin'],
      }),
    );

    expect(parsed).toBeNull();
  });

  it('trims and sanitizes persisted fields while dropping stored api tokens', () => {
    const parsed = parseStoredSession(
      JSON.stringify({
        username: '  alice  ',
        displayName: '  Alice Doe ',
        roles: [' Admin ', '', 'Admin', 'admin', 'fan', ' Fan '],
        modules: [' bookings ', '', 'bookings', 'Reports', 'reports', 9],
        apiToken: '  token-123  ',
        partyId: '42',
      }),
    );

    expect(parsed).toEqual({
      username: 'alice',
      displayName: 'Alice Doe',
      roles: ['admin', 'fan'],
      modules: ['bookings', 'reports'],
      partyId: 42,
    });
  });

  it('drops invalid party ids and defaults displayName to username', () => {
    const parsedWithDecimal = parseStoredSession(
      JSON.stringify({
        username: 'bob',
        roles: ['Fan'],
        partyId: 1.5,
      }),
    );

    const parsedWithNegative = parseStoredSession(
      JSON.stringify({
        username: 'carla',
        roles: ['Admin'],
        partyId: -7,
      }),
    );

    const parsedWithInvalidText = parseStoredSession(
      JSON.stringify({
        username: 'dani',
        roles: ['Fan'],
        partyId: '12x',
      }),
    );

    expect(parsedWithDecimal).toEqual({ username: 'bob', displayName: 'bob', roles: ['fan'] });
    expect(parsedWithNegative).toEqual({ username: 'carla', displayName: 'carla', roles: ['admin'] });
    expect(parsedWithInvalidText).toEqual({ username: 'dani', displayName: 'dani', roles: ['fan'] });
  });
});
