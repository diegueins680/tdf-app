import { injectEventPreview, isSafeEventId, renderEventMetadata } from '../_shared/event-preview.mjs';

export async function onRequest(context) {
  const eventId = String(context.params.eventId ?? '').trim();
  if (!isSafeEventId(eventId)) return new Response('Not found', { status: 404 });

  const apiBase = String(context.env.PUBLIC_API_BASE ?? 'https://tdf-hq.fly.dev').replace(/\/$/, '');
  const [assetResponse, eventResponse] = await Promise.all([
    context.next(),
    fetch(`${apiBase}/directory/events/${eventId}`, {
      headers: { Accept: 'application/json', 'User-Agent': 'tdf-pages-event-preview/1.0' },
    }),
  ]);

  if (!eventResponse.ok) {
    return new Response('Not found', {
      status: eventResponse.status === 404 || eventResponse.status === 403 ? 404 : 503,
      headers: { 'Cache-Control': 'no-store' },
    });
  }

  try {
    const event = await eventResponse.json();
    const preview = renderEventMetadata(event, eventId, new URL(context.request.url).origin);
    const html = injectEventPreview(await assetResponse.text(), preview);
    const headers = new Headers(assetResponse.headers);
    headers.set('Content-Type', 'text/html; charset=utf-8');
    // Visibility can be revoked at any moment. Never let an edge cache keep a
    // formerly public title, venue, image, or schedule available afterwards.
    headers.set('Cache-Control', 'no-store');
    headers.set('Vary', 'Accept-Encoding');
    return new Response(html, { status: assetResponse.status, headers });
  } catch {
    return new Response('Not found', {
      status: 404,
      headers: { 'Cache-Control': 'no-store' },
    });
  }
}
