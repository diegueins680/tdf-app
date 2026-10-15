import { describe, it, expect, afterEach } from '@jest/globals';
import { requestedAuthLocale } from './index';

const visit = (path: string) => window.history.replaceState({}, '', path);

describe('requestedAuthLocale', () => {
  afterEach(() => visit('/'));

  it('honours ?lang= on the auth pages and on the app landing the smart-link aliases redirect to', () => {
    visit('/app?surface=share&lang=en');
    expect(requestedAuthLocale()).toBe('en');
    visit('/login?lang=es');
    expect(requestedAuthLocale()).toBe('es');
  });

  it('ignores the parameter elsewhere and rejects unsupported values', () => {
    visit('/tdf?lang=en');
    expect(requestedAuthLocale()).toBeNull();
    visit('/app?lang=fr');
    expect(requestedAuthLocale()).toBeNull();
  });
});
