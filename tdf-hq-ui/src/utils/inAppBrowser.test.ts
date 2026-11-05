import { buildOpenInSystemBrowserHref, detectInAppBrowser } from './inAppBrowser';

const INSTAGRAM_IOS = 'Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/21F90 Instagram 334.0.0.0.0 (iPhone15,3; iOS 17_5; es_EC; es; scale=3.00; 1290x2796; 591234567)';
const INSTAGRAM_ANDROID = 'Mozilla/5.0 (Linux; Android 14; SM-S918B Build/UP1A.231005.007; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/124.0.6367.179 Mobile Safari/537.36 Instagram 334.0.0.0.0 Android';
const FACEBOOK_IOS = 'Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/21F90 [FBAN/FBIOS;FBAV/467.0.0.0.0;FBBV/0;FBDV/iPhone15,3;FBMD/iPhone;FBSN/iOS;FBSV/17.5;FBSS/3;FBID/phone;FBLC/es_EC;FBOP/5]';
const SAFARI_IOS = 'Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Mobile/15E148 Safari/604.1';
const CHROME_ANDROID = 'Mozilla/5.0 (Linux; Android 14; Pixel 8) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Mobile Safari/537.36';
const CHROME_DESKTOP = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36';

describe('detectInAppBrowser', () => {
  it('recognises social in-app browsers', () => {
    expect(detectInAppBrowser(INSTAGRAM_IOS)).toBe('instagram');
    expect(detectInAppBrowser(INSTAGRAM_ANDROID)).toBe('instagram');
    expect(detectInAppBrowser(FACEBOOK_IOS)).toBe('facebook');
    expect(detectInAppBrowser('Mozilla/5.0 (Linux; Android 13; wv) AppleWebKit/537.36 Chrome/120.0 Mobile Safari/537.36 [FB_IAB/MESSENGER;FBAV/400.0.0.0;]')).toBe('messenger');
    expect(detectInAppBrowser('Mozilla/5.0 (Linux; Android 13; wv) AppleWebKit/537.36 Chrome/120.0 Mobile Safari/537.36 musical_ly_2023 BytedanceWebview/d8a21c6')).toBe('tiktok');
  });

  it('recognises generic WebViews without a known host', () => {
    expect(detectInAppBrowser('Mozilla/5.0 (Linux; Android 13; Pixel 7 Build/TQ3A; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/120.0 Mobile Safari/537.36')).toBe('other');
    expect(detectInAppBrowser('Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/15E148')).toBe('other');
  });

  it('leaves real browsers alone', () => {
    expect(detectInAppBrowser(SAFARI_IOS)).toBeNull();
    expect(detectInAppBrowser(CHROME_ANDROID)).toBeNull();
    expect(detectInAppBrowser(CHROME_DESKTOP)).toBeNull();
    expect(detectInAppBrowser('')).toBeNull();
    expect(detectInAppBrowser(undefined)).toBeNull();
  });
});

describe('buildOpenInSystemBrowserHref', () => {
  const url = 'https://www.tdfrecords.net/login?signup=1&redirect=%2Feventos%2F211';

  it('hands iOS off to Safari through the x-safari-https scheme', () => {
    expect(buildOpenInSystemBrowserHref(url, INSTAGRAM_IOS)).toBe('x-safari-https://www.tdfrecords.net/login?signup=1&redirect=%2Feventos%2F211');
  });

  it('hands Android off to Chrome through an intent with an https fallback', () => {
    expect(buildOpenInSystemBrowserHref(url, INSTAGRAM_ANDROID)).toBe(
      `intent://www.tdfrecords.net/login?signup=1&redirect=%2Feventos%2F211#Intent;scheme=https;package=com.android.chrome;S.browser_fallback_url=${encodeURIComponent(url)};end`,
    );
  });

  it('offers no hand-off on desktop or for non-https links', () => {
    expect(buildOpenInSystemBrowserHref(url, CHROME_DESKTOP)).toBeNull();
    expect(buildOpenInSystemBrowserHref('http://www.tdfrecords.net/login', INSTAGRAM_IOS)).toBeNull();
    expect(buildOpenInSystemBrowserHref('not a url', INSTAGRAM_IOS)).toBeNull();
  });
});
