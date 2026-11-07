import { detectPlatform } from '../mobile/distribution';

/**
 * Embedded browsers inside social apps (Instagram, Facebook, Messenger,
 * TikTok, LINE…) cannot complete Google Sign-In reliably: Google blocks
 * OAuth in embedded WebViews and passkeys/WebAuthn are unavailable there, so
 * a person who normally signs in to Google with a passkey is asked for a
 * password instead. We detect those browsers and steer the person to the
 * system browser (or to email + password) before they hit that wall.
 */
export type InAppBrowserHost = 'instagram' | 'facebook' | 'messenger' | 'tiktok' | 'line' | 'snapchat' | 'other';

const HOST_MARKERS: readonly (readonly [InAppBrowserHost, RegExp])[] = [
  ['instagram', /\bInstagram\b/i],
  ['messenger', /\b(?:FB_IAB\/MESSENGER|Messenger)\b/i],
  ['facebook', /\b(?:FBAN|FBAV|FB_IAB|FB4A|FBIOS)\b/i],
  ['tiktok', /\b(?:TikTok|BytedanceWebview|musical_ly)\b/i],
  ['line', /\bLine\/\d/i],
  ['snapchat', /\bSnapchat\b/i],
];

export function detectInAppBrowser(userAgent: string | null | undefined): InAppBrowserHost | null {
  const ua = userAgent ?? '';
  if (!ua) return null;
  for (const [host, marker] of HOST_MARKERS) {
    if (marker.test(ua)) return host;
  }
  // Generic Android WebView ("; wv)") and iOS WebViews (Mobile/… without the
  // Safari token). Real Chrome/Safari/Firefox on mobile carry "Safari".
  if (/\bwv\b/.test(ua) && /Android/i.test(ua)) return 'other';
  if (/\b(?:iPhone|iPad|iPod)\b/i.test(ua) && /\bMobile\/\w+/i.test(ua) && !/\bSafari\//i.test(ua)) return 'other';
  return null;
}

/**
 * A link that asks the operating system to open `url` in its real browser
 * from inside an in-app browser. Android: a Chrome intent with a plain
 * https fallback. iOS: the `x-safari-https:` scheme that Instagram/Facebook
 * hand off to Safari. Elsewhere there is no escape hatch and the caller
 * should offer "copy link" instead.
 */
export function buildOpenInSystemBrowserHref(url: string, userAgent: string | null | undefined, touchPoints = 0): string | null {
  let parsed: URL;
  try { parsed = new URL(url); } catch { return null; }
  if (parsed.protocol !== 'https:') return null;
  const platform = detectPlatform(userAgent ?? '', touchPoints);
  if (platform === 'android') {
    const rest = `${parsed.host}${parsed.pathname}${parsed.search}`;
    return `intent://${rest}#Intent;scheme=https;package=com.android.chrome;S.browser_fallback_url=${encodeURIComponent(parsed.toString())};end`;
  }
  if (platform === 'ios') return `x-safari-https://${parsed.host}${parsed.pathname}${parsed.search}${parsed.hash}`;
  return null;
}
