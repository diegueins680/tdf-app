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
});
