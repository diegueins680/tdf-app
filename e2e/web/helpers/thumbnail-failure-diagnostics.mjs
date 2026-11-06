// Test-only, bounded metadata for the synthetic records-thumbnails fixture.
export const THUMBNAIL_DIAGNOSTIC_LIMIT = 64;
// Total traversal budget per document, shared by all inserted/removed subtrees.
export const THUMBNAIL_MUTATION_NODE_LIMIT = 2048;

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
  const recorder = { network: [], dropped: 0, errors: 0 };
  const requests = new WeakMap();
  let nextRequest = 0;
  const requestId = request => {
    if (!request || typeof request !== 'object') return undefined;
    if (!requests.has(request)) requests.set(request, ++nextRequest);
    return requests.get(request);
  };
  const note = (type, raw, metadata = {}) => {
    const path = syntheticThumbnailPath(raw, ids);
    if (!path) return;
    if (recorder.network.length >= THUMBNAIL_DIAGNOSTIC_LIMIT) { recorder.dropped++; return; }
    recorder.network.push({ type, path, at: Date.now(), ...metadata });
  };
  // Never retain Request objects, headers, query strings, credentials or raw errors.
  const safe = callback => value => { try { callback(value); } catch { recorder.errors++; } };
  recorder.noteRequest = safe(request => note('request', typeof request === 'string' ? request : request.url(),
    typeof request === 'string' ? {} : { requestId: requestId(request), resourceType: request.resourceType() }));
  page.on('response', safe(response => note('response', response.url(),
    { requestId: requestId(response.request?.()), status: response.status() })));
  page.on('requestfinished', safe(request => {
    const timing = request.timing();
    const elapsed = Number.isFinite(timing.responseEnd) && timing.responseEnd >= 0 ? timing.responseEnd : null;
    note('finished', request.url(), { requestId: requestId(request), responseEnd: elapsed });
  }));
  page.on('requestfailed', safe(request => {
    const error = request.failure()?.errorText;
    note('failed', request.url(), { requestId: requestId(request),
      failureCode: typeof error === 'string' && /^(?:net::)?ERR_[A-Z_]+$/.test(error) ? error : 'unavailable' });
  }));
  // Passive synthetic-only observations. No DOM writes, decode(), layout reads,
  // prototype interception, session/storage access or application state changes.
  await page.addInitScript(({ allowed, limit, nodeLimit }) => {
    const ids = new Set(allowed), nodes = new WeakMap(), events = [], mutations = [], observedImages = [];
    let next = 0, dropped = 0, mutationDropped = 0, nodesVisited = 0, truncatedSubtrees = 0;
    let recordsVisited = 0, recordsDropped = 0, observerErrors = 0, imageSnapshotDropped = 0;
    const path = raw => {
      try {
        const url = new URL(raw), match = /^\/vi\/([A-Za-z0-9_-]{11})\/(hqdefault|mqdefault)\.jpg$/.exec(url.pathname);
        return url.protocol === 'https:' && url.hostname === 'i.ytimg.com'
          && !url.username && !url.password && !url.search && !url.hash
          && match && ids.has(match[1]) ? url.pathname : null;
      } catch { return null; }
    };
    const image = (node, firstSeen, previous = null) => {
      if (!(node instanceof HTMLImageElement) || (!path(node.src) && !previous && !nodes.has(node))) return null;
      if (!nodes.has(node)) {
        nodes.set(node, { id: ++next, firstSeen });
        if (observedImages.length < limit) observedImages.push(node); else imageSnapshotDropped++;
      }
      return { ...nodes.get(node), path: path(node.src), currentPath: path(node.currentSrc),
        connected: node.isConnected, loading: node.loading,
        width: node.naturalWidth, height: node.naturalHeight };
    };
    const mutation = (type, node, attribute, oldValue) => {
      const previousPath = path(oldValue);
      const metadata = image(node, type, previousPath);
      if (!metadata) return;
      if (mutations.length >= limit) { mutationDropped++; return; }
      mutations.push({ type, at: Date.now(), relative: performance.now(), ...metadata,
        ...(attribute ? { attribute, previousPath } : {}) });
    };
    // Explicit bounded walk: both visited nodes and queued siblings share the
    // document-wide limit. Truncation is reported, never presented as absence.
    const subtree = (root, type) => {
      const stack = [root];
      while (stack.length && nodesVisited < nodeLimit) {
        const node = stack.pop(); nodesVisited++;
        mutation(type, node);
        let child = node.lastChild;
        while (child && nodesVisited + stack.length < nodeLimit) {
          stack.push(child); child = child.previousSibling;
        }
        if (child) truncatedSubtrees++;
      }
      if (stack.length) truncatedSubtrees++;
    };
    const observer = new MutationObserver(records => {
      try {
        const count = Math.min(records.length, limit, nodeLimit - recordsVisited);
        recordsDropped += records.length - count;
        recordsVisited += count;
        for (let i = 0; i < count; i++) {
          const record = records[i];
          if (record.type === 'attributes') mutation('attribute', record.target, record.attributeName, record.oldValue);
          else {
            for (const [list, type] of [[record.addedNodes, 'insert'], [record.removedNodes, 'remove']]) {
              let j = 0;
              for (; j < list.length && nodesVisited < nodeLimit; j++) subtree(list[j], type);
              if (j < list.length) truncatedSubtrees++;
            }
          }
        }
      } catch { observerErrors++; }
    });
    observer.observe(document, { subtree: true, childList: true, attributes: true,
      attributeOldValue: true, attributeFilter: ['src', 'srcset', 'loading', 'decoding'] });
    const observe = event => {
      const metadata = image(event.target, event.type);
      if (!metadata) return;
      if (events.length >= limit) { dropped++; return; }
      events.push({ type: event.type, at: Date.now(), relative: performance.now(), scrollY, ...metadata });
    };
    document.addEventListener('load', observe, true);
    document.addEventListener('error', observe, true);
    Object.defineProperty(window, '__tdfSyntheticThumbnailFailure', { value: () => ({
      events: events.slice(), dropped, mutations: mutations.slice(), mutationDropped, nodesVisited, truncatedSubtrees, recordsVisited, recordsDropped, observerErrors, imageSnapshotDropped,
      // These reads occur only after the existing test has already failed.
      // Only already observed synthetic nodes: bounded even on image-heavy pages.
      currentImages: observedImages.filter(node => node.isConnected).map(node => image(node, 'snapshot')),
      imageSnapshotScope: 'observed-synthetic-nodes-only',
      resources: performance.getEntriesByType('resource').filter(entry => path(entry.name)).slice(0, limit)
        .map(entry => ({ path: path(entry.name), start: entry.startTime, duration: entry.duration,
          responseEnd: entry.responseEnd, transferSize: entry.transferSize,
          decodedBodySize: entry.decodedBodySize, responseStatus: entry.responseStatus, initiator: entry.initiatorType })),
      viewport: { width: innerWidth, height: innerHeight, scrollX, scrollY }, frame: { top: window === window.top },
    }) });
  }, { allowed: videoIds, limit: THUMBNAIL_DIAGNOSTIC_LIMIT, nodeLimit: THUMBNAIL_MUTATION_NODE_LIMIT });
  return recorder;
}

export async function captureThumbnailFailure(page, info, recorder) {
  if (!recorder || !shouldCaptureThumbnailFailure(info)) return false;
  let browser;
  try { browser = await page.evaluate(() => window.__tdfSyntheticThumbnailFailure?.() ?? { unavailable: true }); }
  catch { browser = { unavailable: true }; } // Diagnostic failure never catches the primary assertion.
  await info.attach('synthetic-thumbnail-failure', { contentType: 'application/json',
    body: JSON.stringify({ network: recorder.network, dropped: recorder.dropped, errors: recorder.errors, browser }, null, 2) });
  return true;
}
