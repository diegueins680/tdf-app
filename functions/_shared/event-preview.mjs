const PUBLIC_ORIGIN = 'https://tdf-app.pages.dev';
const SAFE_EVENT_ID = /^[1-9][0-9]{0,18}$/;

export function isSafeEventId(value) {
  return SAFE_EVENT_ID.test(String(value ?? '').trim());
}

export function safeAbsoluteImage(value, origin = PUBLIC_ORIGIN) {
  try {
    const url = new URL(String(value ?? ''), origin);
    return url.protocol === 'https:' && !url.username && !url.password ? url.toString() : null;
  } catch {
    return null;
  }
}

function escapeHtml(value) {
  return String(value ?? '')
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&#39;');
}

function jsonForHtml(value) {
  return JSON.stringify(value)
    .replaceAll('<', '\\u003c')
    .replaceAll('>', '\\u003e')
    .replaceAll('&', '\\u0026')
    .replaceAll('\u2028', '\\u2028')
    .replaceAll('\u2029', '\\u2029');
}

export function renderEventMetadata(event, eventId, origin = PUBLIC_ORIGIN) {
  if (!isSafeEventId(eventId) || event?.isPublic !== true) {
    throw new Error('Event is not visible as a public preview.');
  }
  const canonical = new URL(`/eventos/${eventId}`, origin).toString();
  const title = String(event.title ?? '').trim().slice(0, 180);
  if (!title) throw new Error('Public event title is missing.');
  const description = String(event.description ?? `Detalles de ${title} en TDF Records.`).trim().slice(0, 300);
  const image = safeAbsoluteImage(event.imageUrl, origin)
    ?? new URL('/tdf-app-icon-1024.png', origin).toString();
  const venueName = String(event.venue?.name ?? '').trim();
  const city = String(event.location?.city ?? '').trim();
  const jsonLd = {
    '@context': 'https://schema.org',
    '@type': 'MusicEvent',
    name: title,
    description,
    startDate: event.startTime,
    ...(event.endTime ? { endDate: event.endTime } : {}),
    eventStatus: event.workflowStateCode === 'cancelled'
      ? 'https://schema.org/EventCancelled'
      : 'https://schema.org/EventScheduled',
    eventAttendanceMode: 'https://schema.org/OfflineEventAttendanceMode',
    image: [image],
    url: canonical,
    ...(venueName || city ? {
      location: {
        '@type': 'Place',
        name: venueName || city,
        ...(city ? { address: { '@type': 'PostalAddress', addressLocality: city } } : {}),
      },
    } : {}),
  };
  const tags = [
    `<meta data-tdf-event-preview property="og:type" content="event" />`,
    `<meta data-tdf-event-preview property="og:title" content="${escapeHtml(title)}" />`,
    `<meta data-tdf-event-preview property="og:description" content="${escapeHtml(description)}" />`,
    `<meta data-tdf-event-preview property="og:url" content="${escapeHtml(canonical)}" />`,
    `<meta data-tdf-event-preview property="og:image" content="${escapeHtml(image)}" />`,
    `<meta data-tdf-event-preview property="event:start_time" content="${escapeHtml(event.startTime)}" />`,
    `<meta data-tdf-event-preview name="twitter:card" content="summary_large_image" />`,
    `<meta data-tdf-event-preview name="twitter:title" content="${escapeHtml(title)}" />`,
    `<meta data-tdf-event-preview name="twitter:description" content="${escapeHtml(description)}" />`,
    `<meta data-tdf-event-preview name="twitter:image" content="${escapeHtml(image)}" />`,
    `<meta data-tdf-event-preview name="description" content="${escapeHtml(description)}" />`,
    `<link data-tdf-event-preview rel="canonical" href="${escapeHtml(canonical)}" />`,
    `<script data-tdf-event-preview type="application/ld+json">${jsonForHtml(jsonLd)}</script>`,
  ].join('\n    ');
  return { title, description, canonical, image, tags };
}

export function injectEventPreview(html, preview) {
  const withoutManaged = html
    .replace(/\s*<(?:meta|link)[^>]*data-tdf-event-preview[^>]*>/gi, '')
    .replace(/\s*<script[^>]*data-tdf-event-preview[^>]*>[\s\S]*?<\/script>/gi, '');
  const withTitle = /<title>[\s\S]*?<\/title>/i.test(withoutManaged)
    ? withoutManaged.replace(/<title>[\s\S]*?<\/title>/i, `<title>${escapeHtml(preview.title)} · TDF Records</title>`)
    : withoutManaged;
  return withTitle.replace('</head>', `    ${preview.tags}\n  </head>`);
}
