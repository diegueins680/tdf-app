import { jest } from '@jest/globals';

const captureMock = jest.fn();
jest.unstable_mockModule('./posthog', () => ({
  getAnalyticsClient: () => ({ ready: true, capture: captureMock }),
}));

const { reportClientError, redactErrorText, __resetClientErrorReportsForTests } = await import('./errorReporting');

const flush = () => new Promise((resolve) => setTimeout(resolve, 0));

describe('client error reporting', () => {
  let consoleError: jest.SpiedFunction<typeof console.error>;
  beforeEach(() => {
    __resetClientErrorReportsForTests();
    captureMock.mockClear();
    consoleError = jest.spyOn(console, 'error').mockImplementation(() => undefined);
  });
  afterEach(() => consoleError.mockRestore());

  it('redacts emails, JWTs, secrets and opaque identifiers', () => {
    const text = redactErrorText(
      'login failed for ana@example.com token=abc123 eyJhbGciOi.eyJzdWIiOiIx.c2lnbmF0dXJl id 0123456789abcdef0123456789abcdef01',
    );
    expect(text).not.toContain('ana@example.com');
    expect(text).not.toContain('abc123');
    expect(text).not.toContain('eyJhbGciOi');
    expect(text).not.toContain('0123456789abcdef0123456789abcdef01');
    expect(text).toContain('[email]');
  });

  it.each([
    'password="private alpha beta"',
    "'password': 'private alpha beta'",
    'access_token=private-token',
    'refreshToken: private-token',
    'Authorization: Bearer private-token',
    'api_key=private-token',
    'clientSecret="private alpha beta"',
    'password="private alpha beta',
    '"password": "private \\"alpha\\" beta"',
  ])('excludes recognized credentials from console and capture: %s', async (message) => {
    reportClientError('auth_flow', new Error(message), { method: message });
    await flush();
    const output = JSON.stringify([consoleError.mock.calls, captureMock.mock.calls]);
    for (const sentinel of ['private', 'alpha', 'beta']) expect(output).not.toContain(sentinel);
    expect(captureMock).toHaveBeenCalledTimes(1);
    expect(output).toContain('[redacted]');
  });

  it('captures a sanitized client_error event with route path only', async () => {
    window.history.pushState({}, '', '/fans?redirect=%2Fsecret#token');
    reportClientError('app_render', new TypeError('boom for ana@example.com'), { method: 'google' });
    await flush();
    expect(captureMock).toHaveBeenCalledWith('client_error', expect.objectContaining({
      kind: 'app_render',
      error_name: 'TypeError',
      error_message: 'boom for [email]',
      route: '/fans',
      method: 'google',
    }));
  });

  it('deduplicates identical failures and caps reports per page', async () => {
    reportClientError('window_error', new Error('same'));
    reportClientError('window_error', new Error('same'));
    for (let index = 0; index < 40; index += 1) reportClientError('window_error', new Error(`e${index}`));
    await flush();
    expect(captureMock).toHaveBeenCalledTimes(25);
  });

  it('never logs the original error, stack, cause or attached account data', async () => {
    const error = new Error('Failure for ana@example.com at https://tdf.test/reset?code=short-code#private-fragment');
    error.name = 'Failure for ana@example.com';
    error.stack = 'private-stack-sentinel';
    Object.assign(error, { cause: { password: 'private-cause-sentinel' }, account: 'private-account-sentinel' });
    reportClientError('auth_flow', error);
    await flush();
    expect(consoleError.mock.calls.some((call) => call.includes(error))).toBe(false);
    const output = JSON.stringify([consoleError.mock.calls, captureMock.mock.calls]);
    for (const sentinel of ['ana@example.com', 'short-code', 'private-fragment', 'private-stack-sentinel', 'private-cause-sentinel', 'private-account-sentinel']) {
      expect(output).not.toContain(sentinel);
    }
    expect(captureMock).toHaveBeenCalledTimes(1);
  });

  it('allows diagnostic context fields without permitting reserved fields or arbitrary data', async () => {
    window.history.replaceState({}, '', '/fans?token=private-query');
    const context = {
      method: 'google', timed_out: true,
      kind: 'injected', route: '/reset?code=private-code',
      error_message: 'raw-private-message', error_name: 'raw-private-name',
      commit: 'raw-private-commit', token: 'private-token', account: 'private-account',
      online: !navigator.onLine, visibility: 'raw-private-visibility',
      component_stack: 'at https://tdf.test/component?code=private-stack-code for ana@example.com',
    };
    reportClientError('auth_flow', new Error('benign'), context);
    await flush();
    const properties = captureMock.mock.calls[0]?.[1] as Record<string, unknown>;
    expect(properties).toMatchObject({
      kind: 'auth_flow', route: '/fans', error_name: 'Error', error_message: 'benign',
      method: 'google', timed_out: true, commit: typeof __APP_COMMIT__ === 'string' ? __APP_COMMIT__ : 'dev',
      online: navigator.onLine, visibility: document.visibilityState,
    });
    for (const sentinel of ['private-', 'raw-private-', 'ana@example.com']) {
      expect(JSON.stringify(properties)).not.toContain(sentinel);
    }
    expect(properties).not.toHaveProperty('token');
    expect(properties).not.toHaveProperty('account');
  });

  it('redacts every selected text field and rejects nonboolean diagnostic flags', async () => {
    const context = Object.fromEntries(
      ['boundary', 'component_stack', 'source', 'watchdog_route', 'method'].map((key) => [key,
        'ana@example.com password=private-password https://tdf.test/reset?code=private-code#private-fragment']),
    );
    reportClientError('session_bootstrap', new Error('benign'), {
      ...context, timed_out: 'private-flag', had_stored_session: 1, creating_account: false, linking: true,
    });
    await flush();
    const properties = captureMock.mock.calls[0]?.[1] as Record<string, unknown>;
    expect(JSON.stringify(properties)).not.toContain('private-');
    expect(JSON.stringify(properties)).not.toContain('ana@example.com');
    expect(properties).toMatchObject({ creating_account: false, linking: true });
    expect(properties['timed_out']).toBeUndefined();
    expect(properties['had_stored_session']).toBeUndefined();
  });

  it.each([
    'Failed https://tdf.test/reset?code=short-secret#private-fragment',
    'Failed /orders/short-secret?lookup=private-fragment',
    'Failed tdf://tickets/short-secret#private-fragment',
  ])('removes embedded diagnostic URLs: %s', (message) => {
    const redacted = redactErrorText(message);
    expect(redacted).not.toContain('short-secret');
    expect(redacted).not.toContain('private-fragment');
    expect(redacted).toContain('[url]');
  });

  it('still reports when console logging fails', async () => {
    consoleError.mockImplementation(() => { throw new Error('Console unavailable'); });
    expect(() => reportClientError('app_render', new Error('benign'))).not.toThrow();
    await flush();
    expect(captureMock).toHaveBeenCalledTimes(1);
  });

  it('does not throw while describing an error with a failing message getter', async () => {
    const error = Object.defineProperty({}, 'message', { get: () => { throw new Error('Getter unavailable'); } });
    expect(() => reportClientError('window_error', error)).not.toThrow();
    await flush();
    expect(captureMock).toHaveBeenCalledTimes(1);
  });

  it('routes section crashes through the filtered reporter instead of native exception capture', async () => {
    const { default: RouteErrorBoundary } = await import('../components/RouteErrorBoundary');
    const captureException = jest.fn();
    const previous = Object.getOwnPropertyDescriptor(window, 'posthog');
    Object.defineProperty(window, 'posthog', { configurable: true, value: { captureException } });
    window.history.replaceState({}, '', '/fans?code=short-private-code#private-fragment');
    const error = new Error('Failure for ana@example.com');
    error.stack = 'private-stack-sentinel';
    try {
      const boundary = new RouteErrorBoundary({ children: null });
      boundary.componentDidCatch(error, { componentStack: 'private-component-stack' });
      await flush();
      expect(captureException).not.toHaveBeenCalled();
      expect(captureMock).toHaveBeenCalledWith('client_error', expect.objectContaining({ kind: 'app_render', route: '/fans', boundary: 'route' }));
      const output = JSON.stringify([consoleError.mock.calls, captureMock.mock.calls]);
      for (const sentinel of ['ana@example.com', 'short-private-code', 'private-fragment', 'private-stack-sentinel', 'private-component-stack']) expect(output).not.toContain(sentinel);
    } finally {
      if (previous) Object.defineProperty(window, 'posthog', previous);
      else Reflect.deleteProperty(window, 'posthog');
    }
  });

});
