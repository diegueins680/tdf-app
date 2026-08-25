import {
  hasInstagramTrafficSignal,
  isInstagramSourceValue,
  readStoredInstagramTraffic,
  referrerIsInstagram,
  rememberInstagramTraffic,
  searchHasInstagramSource,
} from './instagramTraffic';

describe('instagramTraffic', () => {
  it('detects Instagram source values from common campaign formats', () => {
    expect(isInstagramSourceValue('instagram')).toBe(true);
    expect(isInstagramSourceValue('ig')).toBe(true);
    expect(isInstagramSourceValue('paid_ig_reels')).toBe(true);
    expect(isInstagramSourceValue('newsletter')).toBe(false);
  });

  it('detects Instagram source hints from query strings and referrers', () => {
    expect(searchHasInstagramSource('?utm_source=instagram')).toBe(true);
    expect(searchHasInstagramSource('?source=ig')).toBe(true);
    expect(searchHasInstagramSource('?utm_campaign=tdf_ig_reels')).toBe(true);
    expect(searchHasInstagramSource('?utm_source=email')).toBe(false);
    expect(referrerIsInstagram('https://l.instagram.com/?u=https%3A%2F%2Ftdf-app.pages.dev')).toBe(true);
    expect(referrerIsInstagram('https://example.com')).toBe(false);
    expect(
      hasInstagramTrafficSignal({
        search: '?utm_source=email',
        referrer: 'https://instagram.com/tdf.records.label',
      }),
    ).toBe(true);
  });

  it('stores the Instagram traffic hint for the current browser session', () => {
    const storage = new Map<string, string>();
    const fakeStorage = {
      getItem: (key: string) => storage.get(key) ?? null,
      setItem: (key: string, value: string) => {
        storage.set(key, value);
      },
      removeItem: (key: string) => {
        storage.delete(key);
      },
      clear: () => storage.clear(),
      key: (index: number) => Array.from(storage.keys())[index] ?? null,
      get length() {
        return storage.size;
      },
    } as Storage;

    expect(readStoredInstagramTraffic(fakeStorage)).toBe(false);
    rememberInstagramTraffic(fakeStorage);
    expect(readStoredInstagramTraffic(fakeStorage)).toBe(true);
  });
});
