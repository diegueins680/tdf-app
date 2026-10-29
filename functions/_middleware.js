import { PUBLIC_ORIGIN } from './_shared/event-preview.mjs';
import { reportHeaders } from './_shared/event-report.mjs';

// The production Pages hostname is not a public URL: links shared from it
// carried the wrong domain into previews and analytics. Send it to the
// canonical site, keeping path and query (OAuth callbacks registered on the
// old host keep working). Branch previews (*.tdf-app.pages.dev) are untouched,
// and /.well-known stays so app-link verification for old links still works.
export const LEGACY_PRODUCTION_HOST = 'tdf-app.pages.dev';

export function canonicalRedirectLocation(requestUrl) {
  const url = new URL(requestUrl);
  if (url.hostname !== LEGACY_PRODUCTION_HOST || url.pathname.startsWith('/.well-known/')) return null;
  // Assign the parts instead of resolving a relative reference: a path that
  // starts with `//` would otherwise be read as another host.
  const target = new URL(PUBLIC_ORIGIN);
  target.pathname = url.pathname;
  target.search = url.search;
  target.hash = url.hash;
  return target.toString();
}

export async function onRequest(context) {
  const location = canonicalRedirectLocation(context.request.url);
  const reportPath = /^\/reportes\/eventos(?:\/|$)/.test(new URL(context.request.url).pathname);
  if (location) {
    return reportPath
      ? new Response(null, { status: 302, headers: { ...reportHeaders, Location: location } })
      : Response.redirect(location, 301);
  }
  if (!reportPath) return context.next();
  // Unknown or extra path segments must not fall through to the public SPA,
  // whose analytics and navigation were not designed to receive capabilities.
  const path = new URL(context.request.url).pathname;
  if (!/^\/reportes\/eventos\/[A-Za-z0-9_-]{43}\/?$/.test(path)) {
    return new Response(context.request.method === 'HEAD' ? null : 'Not found', {
      status: 404, headers: reportHeaders,
    });
  }
  const response = await context.next();
  const protectedResponse = new Response(response.body, response);
  for (const [name, value] of Object.entries(reportHeaders)) protectedResponse.headers.set(name, value);
  return protectedResponse;
}
