import { PUBLIC_ORIGIN } from './_shared/event-preview.mjs';

// The production Pages hostname is not a public URL: links shared from it
// carried the wrong domain into previews and analytics. Send it to the
// canonical site, keeping path and query (OAuth callbacks registered on the
// old host keep working). Branch previews (*.tdf-app.pages.dev) are untouched,
// and /.well-known stays so app-link verification for old links still works.
export const LEGACY_PRODUCTION_HOST = 'tdf-app.pages.dev';

export function canonicalRedirectLocation(requestUrl) {
  const url = new URL(requestUrl);
  if (url.hostname !== LEGACY_PRODUCTION_HOST || url.pathname.startsWith('/.well-known/')) return null;
  return new URL(`${url.pathname}${url.search}${url.hash}`, PUBLIC_ORIGIN).toString();
}

export async function onRequest(context) {
  const location = canonicalRedirectLocation(context.request.url);
  return location ? Response.redirect(location, 301) : context.next();
}
