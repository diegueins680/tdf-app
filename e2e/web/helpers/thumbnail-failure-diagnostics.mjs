// Test-only, bounded metadata for the synthetic records-thumbnails fixture.
export const THUMBNAIL_DIAGNOSTIC_LIMIT = 64;

export function syntheticThumbnailPath(raw, videoIds) {
  try {
    const url = new URL(raw);
    const match = /^\/vi\/([A-Za-z0-9_-]{11})\/(hqdefault|mqdefault)\.jpg$/.exec(url.pathname);
    return url.protocol === 'https:' && url.hostname === 'i.ytimg.com'
      && !url.username && !url.password && !url.search && !url.hash
      && match && videoIds.has(match[1]) ? url.pathname : null;
  } catch { return null; }
}

export function shouldCaptureThumbnailFailure(info) {
  return info.status !== info.expectedStatus
    && (info.status === 'failed' || info.status === 'timedOut');
}

export async function beginThumbnailDiagnostics(page, videoIds) {
  const ids = new Set(videoIds);
  const recorder = { network: [], dropped: 0 };
  const note = (type, raw, status) => {
    const path = syntheticThumbnailPath(raw, ids);
    if (!path) return;
    if (recorder.network.length >= THUMBNAIL_DIAGNOSTIC_LIMIT) { recorder.dropped++; return; }
    recorder.network.push({ type, path, at: Date.now(), ...(status === undefined ? {} : { status }) });
  };
  recorder.noteRequest = raw => note('request', raw);
  page.on('response', response => note('response', response.url(), response.status()));
  // Read-only load/error events distinguish repeated requests on one image from
  // new image instances. No MutationObserver, DOM writes, decode(), layout reads,
  // session/storage access, or application/network state changes occur here.
  await page.addInitScript(({ allowed, limit }) => {
    const ids = new Set(allowed), nodes = new WeakMap(), events = [];
    let next = 0, dropped = 0;
    const path = raw => {
      try {
        const url = new URL(raw), match = /^\/vi\/([A-Za-z0-9_-]{11})\/(hqdefault|mqdefault)\.jpg$/.exec(url.pathname);
        return url.protocol === 'https:' && url.hostname === 'i.ytimg.com'
          && !url.username && !url.password && !url.search && !url.hash
          && match && ids.has(match[1]) ? url.pathname : null;
      } catch { return null; }
    };
    const image = node => {
      if (!(node instanceof HTMLImageElement) || !path(node.src)) return null;
      if (!nodes.has(node)) nodes.set(node, ++next);
      return { id: nodes.get(node), path: path(node.src), currentPath: path(node.currentSrc),
        connected: node.isConnected, loading: node.loading,
        width: node.naturalWidth, height: node.naturalHeight };
    };
    const observe = event => {
      const metadata = image(event.target);
      if (!metadata) return;
      if (events.length >= limit) { dropped++; return; }
      events.push({ type: event.type, at: Date.now(), relative: performance.now(), scrollY, ...metadata });
    };
    document.addEventListener('load', observe, true);
    document.addEventListener('error', observe, true);
    Object.defineProperty(window, '__tdfSyntheticThumbnailFailure', { value: () => ({
      events: events.slice(), dropped,
      // These reads occur only after the existing test has already failed.
      currentImages: Array.from(document.images).map(image).filter(Boolean).slice(0, limit),
      resources: performance.getEntriesByType('resource').filter(entry => path(entry.name)).slice(0, limit)
        .map(entry => ({ path: path(entry.name), start: entry.startTime, duration: entry.duration,
          responseEnd: entry.responseEnd, transferSize: entry.transferSize,
          decodedBodySize: entry.decodedBodySize, responseStatus: entry.responseStatus, initiator: entry.initiatorType })),
      viewport: { width: innerWidth, height: innerHeight, scrollX, scrollY }, frame: { top: window === window.top },
    }) });
  }, { allowed: videoIds, limit: THUMBNAIL_DIAGNOSTIC_LIMIT });
  return recorder;
}

export async function captureThumbnailFailure(page, info, recorder) {
  if (!recorder || !shouldCaptureThumbnailFailure(info)) return false;
  let browser;
  try { browser = await page.evaluate(() => window.__tdfSyntheticThumbnailFailure?.() ?? { unavailable: true }); }
  catch { browser = { unavailable: true }; } // Diagnostic failure never catches the primary assertion.
  await info.attach('synthetic-thumbnail-failure', { contentType: 'application/json',
    body: JSON.stringify({ network: recorder.network, dropped: recorder.dropped, browser }, null, 2) });
  return true;
}
