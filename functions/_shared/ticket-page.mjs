import { isSafeEventId } from './event-preview.mjs';

// Transactional pages are not independent event listings. Apply this boundary
// to the initial HTTP response as well as the hydrated React document.
export async function ticketPageResponse(context) {
  const headers = {
    'X-Robots-Tag': 'noindex, follow',
    'Cache-Control': 'private, no-store',
    'Referrer-Policy': 'no-referrer',
  };
  if (!isSafeEventId(context.params.eventId)
      || (context.params.orderId !== undefined && !isSafeEventId(context.params.orderId))) {
    return new Response('Not found', { status: 404, headers });
  }
  const asset = await context.next();
  const response = new Response(asset.body, asset);
  for (const [name, value] of Object.entries(headers)) response.headers.set(name, value);
  return response;
}
