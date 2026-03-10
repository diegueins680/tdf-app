import { inferDemoToken } from './appConfig';

describe('inferDemoToken', () => {
  it('normalizes localhost hostnames consistently across URL forms', () => {
    const localToken = inferDemoToken('localhost');

    expect(inferDemoToken('LOCALHOST:5173')).toBe(localToken);
    expect(inferDemoToken('https://localhost:5173/auth')).toBe(localToken);
    expect(inferDemoToken('http://127.0.0.1:3000')).toBe(localToken);
  });

  it('accepts IPv6 loopback hosts used by local dev servers', () => {
    const localToken = inferDemoToken('localhost');

    expect(inferDemoToken('[::1]')).toBe(localToken);
    expect(inferDemoToken('[::1]:5173')).toBe(localToken);
    expect(inferDemoToken('http://[::1]:5173')).toBe(localToken);
    expect(inferDemoToken('::1')).toBe(localToken);
  });

  it('does not infer a token for unknown hosts unless a global override is configured', () => {
    const unknownHostToken = inferDemoToken('example.com');

    if (unknownHostToken !== '') {
      expect(unknownHostToken).toBe(inferDemoToken('localhost'));
      return;
    }

    expect(unknownHostToken).toBe('');
  });

  it('treats the public Pages host like an unknown host by default', () => {
    const pagesToken = inferDemoToken('tdf-app.pages.dev');
    const unknownHostToken = inferDemoToken('example.com');

    expect(pagesToken).toBe(unknownHostToken);
  });
});
