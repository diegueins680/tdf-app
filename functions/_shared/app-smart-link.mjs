// The canonical TDF Mobile landing. Short aliases (/descargar, /download) redirect here on the edge so the
// SPA keeps a single route and every shared link lands on the same measured page.
export const APP_LANDING_PATH = '/app';
export const APP_LANDING_ORIGIN = 'https://www.tdfrecords.net';

// Only campaign labels and the entry surface survive the redirect. Anything else (tokens, emails, free text)
// is dropped so an alias can never be used to smuggle parameters onto the landing.
const KEPT_PARAMS = ['utm_source', 'utm_medium', 'utm_campaign', 'utm_content', 'utm_term', 'surface', 'feedback', 'lang'];
const LABEL = /^[A-Za-z0-9_-]{1,80}$/;

export function smartLinkTarget(requestUrl, origin = APP_LANDING_ORIGIN) {
  const incoming = new URL(requestUrl);
  const target = new URL(APP_LANDING_PATH, origin);
  for (const key of KEPT_PARAMS) {
    const value = incoming.searchParams.get(key);
    if (value !== null && LABEL.test(value)) target.searchParams.set(key, value);
  }
  return target.toString();
}

export function redirectToAppLanding(context) {
  const location = smartLinkTarget(context.request.url, new URL(context.request.url).origin);
  return new Response(null, {
    status: 302,
    headers: {
      Location: location,
      // Aliases are campaign entry points: never let an edge cache pin one visitor's tags for the next.
      'Cache-Control': 'no-store',
      Vary: 'Accept-Encoding',
    },
  });
}
