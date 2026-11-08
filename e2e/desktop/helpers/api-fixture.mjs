// Stateful synthetic TDF API for the desktop shell journeys. Every request the
// hosted application makes to the API origin is answered here; nothing reaches
// a live service. Foreign origins are refused so analytics or provider SDKs
// cannot leak out of the test.
import { API_ORIGIN } from './shell.mjs';

export const STAFF_ACCOUNT = Object.freeze({
  username: 'staff-desktop-fixture', displayName: 'Operación TDF (sintética)', partyId: 7001,
  roles: ['admin'], modules: ['admin', 'scheduling', 'crm', 'invoicing', 'packages', 'label', 'ops', 'events'],
  featureFlags: [], preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' },
});
export const ARTIST_ACCOUNT = Object.freeze({
  username: 'artist-desktop-fixture', displayName: 'Artista sintética TDF', partyId: 101,
  roles: ['artist'], modules: ['label'], featureFlags: [], preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' },
});
export const CUSTOMER_ACCOUNT = Object.freeze({
  username: 'fan-desktop-fixture', displayName: 'Fan sintética', partyId: 4242,
  roles: ['customer'], modules: [], featureFlags: [], preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' },
});

export const RELEASE = Object.freeze({
  releaseId: '10000000-0000-4000-8000-000000000001',
  versionId: '20000000-0000-4000-8000-000000000001',
  slug: 'album-sintetico-desktop',
  recordingId: '40000000-0000-4000-8000-000000000001',
  secondRecordingId: '40000000-0000-4000-8000-000000000002',
  assetId: '50000000-0000-4000-8000-000000000001',
  secondAssetId: '50000000-0000-4000-8000-000000000002',
  brokenAssetId: '50000000-0000-4000-8000-00000000dead',
  coverAssetId: '60000000-0000-4000-8000-000000000001',
});

export function silentWavDataUrl(seconds = 2) {
  const sampleRate = 8_000;
  const dataSize = sampleRate * 2 * seconds;
  const wav = Buffer.alloc(44 + dataSize);
  wav.write('RIFF', 0); wav.writeUInt32LE(36 + dataSize, 4); wav.write('WAVEfmt ', 8); wav.writeUInt32LE(16, 16);
  wav.writeUInt16LE(1, 20); wav.writeUInt16LE(1, 22); wav.writeUInt32LE(sampleRate, 24); wav.writeUInt32LE(sampleRate * 2, 28);
  wav.writeUInt16LE(2, 32); wav.writeUInt16LE(16, 34); wav.write('data', 36); wav.writeUInt32LE(dataSize, 40);
  return { dataUrl: `data:audio/wav;base64,${wav.toString('base64')}`, bytes: wav };
}

const transparentGif = 'data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw==';
const nowIso = () => new Date().toISOString();

function publicRelease(appOrigin) {
  const summary = {
    id: RELEASE.releaseId, artistPartyId: 101, slug: RELEASE.slug, kind: 'album', versionId: RELEASE.versionId, versionNumber: 1,
    title: 'Álbum sintético de escritorio', displayArtist: 'Artista sintética TDF', releaseAtUtc: '2026-09-13T05:00:00Z',
    labelName: 'Sello sintético', publishedAt: '2026-09-13T05:00:01Z', coverAssetId: RELEASE.coverAssetId,
  };
  const track = (trackId, recordingId, number, title, assetId, artist = 'Artista sintética TDF') => ({
    trackId, recordingId, discNumber: 1, trackNumber: number, title, displayArtist: artist, durationMs: 2_000, explicitContent: 'not_explicit',
    sources: [{ assetId, role: 'stream_audio', mediaType: 'audio/wav', technicalMetadata: { bitrate_kbps: 256, loudness_lufs: -14 } }],
  });
  return {
    summary,
    release: {
      ...summary, subtitle: null, versionTitle: null, explicitContent: 'not_explicit', originalReleaseDate: '2026-09-13', catalogNumber: 'SYN-DESK-001',
      tracks: [
        track('30000000-0000-4000-8000-000000000001', RELEASE.recordingId, 1, 'Primera pista sintética', RELEASE.assetId),
        track('30000000-0000-4000-8000-000000000002', RELEASE.secondRecordingId, 2, 'Segunda pista sintética', RELEASE.secondAssetId, 'Otra artista sintética'),
        track('30000000-0000-4000-8000-000000000003', '40000000-0000-4000-8000-000000000003', 3, 'Pista con fuente caída', RELEASE.brokenAssetId),
      ],
      coverAssets: [{ assetId: RELEASE.coverAssetId, role: 'cover_display', mediaType: 'image/gif', technicalMetadata: { width: 1, height: 1 } }],
      availability: [{ ruleId: '70000000-0000-4000-8000-000000000001', trackId: null, territoryMode: 'include', territories: ['EC'],
        startsAt: '2026-09-13T05:00:00Z', endsAt: null, listeningPolicy: 'full', downloadPolicy: 'none', purchasable: false, priceMinor: null, currency: null }],
    },
    radioStreamUrl: `${appOrigin}/fixture-radio.wav`,
  };
}

function studioVersion(state = 'draft', assets = []) {
  return {
    id: RELEASE.versionId, releaseId: RELEASE.releaseId, versionNumber: 1, state, title: 'Borrador de escritorio',
    subtitle: null, versionTitle: null, displayArtist: 'Artista sintética TDF', titleLanguage: 'es', titleScript: null,
    primaryGenreId: null, secondaryGenreId: null, primaryGenre: null, secondaryGenre: null, explicitContent: 'unknown',
    originalReleaseDate: null, releaseAtUtc: null, releaseTimezone: null, embargoUntilUtc: null, labelName: null, catalogNumber: null,
    recordingCopyrightText: null, workCopyrightText: null,
    validation: { metadata: true, assets: true, rights: true, access: true, errors: [] },
    tracks: [{ trackId: '30000000-0000-4000-8000-000000000001', recordingId: RELEASE.recordingId, discNumber: 1, trackNumber: 1,
      displayArtist: 'Artista sintética TDF', isPrimaryResource: true, previewStartMs: null, previewDurationMs: null,
      title: 'Pista sintética', subtitle: null, versionTitle: null, titleLanguage: 'es', titleScript: null, durationMs: 60_000, explicitContent: 'not_explicit' }],
    parties: [], credits: [], identifiers: [], rights: [], availability: [], terms: [], assets, comments: [], updatedAt: '2026-11-01T00:00:00Z',
  };
}

/**
 * Creates the fixture. `install(context, appOrigin)` wires it into the Electron
 * browser context. Journeys mutate `state` to simulate sign-in, expiry,
 * connection loss (`failNext`) and inspect `requests`/`writes`.
 */
export function createApiFixture({ account = STAFF_ACCOUNT } = {}) {
  const wav = silentWavDataUrl();
  const state = {
    account, authenticated: false, expired: false, requests: [], writes: [], failures: [],
    uploadSessions: new Map(), uploadSessionsByKey: new Map(), uploadedParts: new Map(),
    transitions: new Map(), studio: studioVersion(), bookings: [
      { bookingId: 501, title: 'Grabación — Banda sintética', startsAt: '2030-01-15T14:00:00Z', endsAt: '2030-01-15T16:00:00Z', status: 'Confirmed',
        notes: null, partyId: 9001, engineerPartyId: null, engineerName: null, serviceOfferingId: null, serviceType: 'Grabación', customerName: 'Banda sintética',
        partyDisplayName: 'Banda sintética', resources: [{ resourceId: 'room-a', resourceName: 'Estudio A' }] },
    ],
    payments: [{ payId: 9101, payPartyId: 9001, payPartyDisplayName: 'Banda sintética', payOrderId: null, payInvoiceId: null, payAmountCents: 12_050,
      payCurrency: 'USD', payMethod: 'Cash', payReference: 'REC-0001', payPaidAt: '2026-11-01T15:00:00Z', payConcept: 'Sesión de grabación', payPeriod: null, payAttachment: null }],
    events: [{ eventId: 'evt-desktop-1', eventOrganizerPartyId: String(account.partyId), eventTitle: 'Noche sintética en el Domo', eventDescription: 'Evento de prueba',
      eventStart: '2030-10-03T20:00:00Z', eventEnd: '2030-10-04T01:00:00Z', eventTimezone: 'America/Guayaquil', eventVenueId: null, eventPriceCents: 1500, eventCapacity: 120,
      eventTypeId: null, eventWorkflowStateId: null, eventWorkflowStateCode: 'announced', eventWorkflowStateNameEs: 'Anunciado', eventWorkflowStateNameEn: 'Announced',
      eventPublicListable: true, eventRsvpEligible: true, eventTicketPurchaseEnabled: false, eventCurrency: 'USD', eventBudgetCents: null, eventTicketUrl: null,
      eventImageUrl: null, eventIsPublic: true, eventArtists: [] }],
  };

  /** Fails the next request matching `test` once: mode 'network' aborts, a number answers that status. */
  const failNext = (test, mode = 'network') => { state.failures.push({ test, mode }); };

  const sessionJson = () => ({ ...state.account });
  const unauthenticated = (route) => route.fulfill({ status: 401, json: { error: 'missing or invalid auth token' } });

  async function handleApi(route, request, url, appOrigin) {
    const method = request.method();
    const path = url.pathname;
    state.requests.push({ method, path, search: url.search, headers: request.headers() });
    const failure = state.failures.find((candidate) => candidate.test(method, path, request));
    if (failure) {
      state.failures.splice(state.failures.indexOf(failure), 1);
      if (failure.mode === 'network') return route.abort('internetdisconnected');
      return route.fulfill({ status: failure.mode, json: { error: `synthetic failure ${failure.mode}` } });
    }
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/version') return route.fulfill({ json: { name: 'tdf-hq', version: 'synthetic', commit: 'synthetic', buildTime: nowIso() } });
    if (path === '/login' && method === 'POST') {
      const body = request.postDataJSON();
      state.writes.push({ method, path, body });
      if (body?.password === 'incorrecta') return route.fulfill({ status: 401, json: { error: 'Credenciales inválidas' } });
      state.authenticated = true; state.expired = false;
      return route.fulfill({ json: { token: 'synthetic-desktop-token', partyId: state.account.partyId, roles: state.account.roles, modules: state.account.modules } });
    }
    if (path === '/session/logout' && method === 'POST') { state.authenticated = false; state.writes.push({ method, path }); return route.fulfill({ status: 204, body: '' }); }
    if (path === '/session') {
      if (state.expired) return route.fulfill({ status: 401, json: { error: 'invalid or inactive session token' } });
      return state.authenticated ? route.fulfill({ json: sessionJson() }) : unauthenticated(route);
    }
    if (state.authenticated && state.expired && !path.startsWith('/music/releases') && !path.startsWith('/fans/artists')) {
      return route.fulfill({ status: 401, json: { error: 'invalid or inactive session token' } });
    }
    if (path.startsWith('/session/onboarding')) {
      return route.fulfill({ json: { newlyCompleted: false, progress: { eligible: false, completedAt: '2026-10-07T00:00:00Z' }, eligible: false, completedAt: '2026-10-07T00:00:00Z' } });
    }
    if (path === '/fans/artists') return route.fulfill({ json: [] });
    if (['/fans/me/follows', '/fans/me/clubs', '/fans/me/notifications', '/access-requests', '/navigation/preferences', '/engineers', '/service-orders'].includes(path)) return route.fulfill({ json: [] });
    if (path === '/fans/me/notifications/count') return route.fulfill({ json: { count: 0 } });
    if (path === '/marketplace') return route.fulfill({ json: [] });
    if (path === '/records/feed') return route.fulfill({ json: { collections: [], releases: [], recordings: [], sessions: [] } });
    if (path === '/catalogs/batch') return route.fulfill({ json: {} });
    if (path.startsWith('/catalogs/') || path.startsWith('/catalog/')) return route.fulfill({ json: { items: [], total: 0 } });
    if (path === '/radio/streams') return route.fulfill({ json: [{ rsId: 1, rsName: 'Emisora sintética', rsActive: true, rsStreamUrl: `${appOrigin}/fixture-radio.wav` }] });
    if (path === '/radio/auto-stop-options') return route.fulfill({ json: { options: [] } });
    if (path === '/radio/streams/now-playing') return route.fulfill({ json: {} });
    if (path === '/radio/presence') return route.fulfill({ json: null });
    if (path === '/rooms') return route.fulfill({ json: [{ roomId: 'room-a', rName: 'Estudio A', rBookable: true }] });
    if (path.startsWith('/services/catalog')) return route.fulfill({ json: { sceSchemaVersion: 1, sceRevision: 1, sceLocale: 'es', sceItems: [] } });
    if (path === '/bookings' && method === 'GET') return route.fulfill({ json: state.bookings });
    if (path.startsWith('/bookings/') && method === 'PUT') {
      const body = request.postDataJSON(); state.writes.push({ method, path, body });
      const id = Number(path.split('/')[2]);
      state.bookings = state.bookings.map((booking) => booking.bookingId === id ? { ...booking, status: body?.ubStatus ?? booking.status, notes: body?.ubNotes ?? booking.notes } : booking);
      return route.fulfill({ json: state.bookings.find((booking) => booking.bookingId === id) });
    }
    if (path === '/parties' && method === 'GET') {
      return route.fulfill({ json: [{ partyId: 9001, displayName: 'Banda sintética', legalName: 'Banda Sintética S.A.', isOrg: true, roles: ['Customer'], primaryEmail: 'banda@example.invalid', primaryPhone: null, whatsapp: null, instagram: '@bandasintetica', hasUserAccount: false }] });
    }
    if (path === '/payments' && method === 'GET') return route.fulfill({ json: state.payments });
    if (path === '/payments' && method === 'POST') {
      const body = request.postDataJSON(); state.writes.push({ method, path, body, idempotencyKey: request.headers()['idempotency-key'] ?? null });
      const created = { payId: 9102 + state.writes.length, payPartyId: body.pcPartyId, payPartyDisplayName: 'Banda sintética', payOrderId: null, payInvoiceId: null,
        payAmountCents: body.pcAmountCents, payCurrency: body.pcCurrency, payMethod: body.pcMethod, payReference: body.pcReference ?? null, payPaidAt: body.pcPaidAt, payConcept: body.pcConcept, payPeriod: null, payAttachment: null };
      state.payments = [created, ...state.payments];
      return route.fulfill({ json: created });
    }
    if (path === '/social-events/events' && method === 'GET') return route.fulfill({ json: state.events });
    if (path.startsWith('/social-events/venues')) return route.fulfill({ json: [] });
    if (path.startsWith('/social-events/events/') && method === 'GET') {
      if (path.endsWith('/invitations') || path.endsWith('/ticket-tiers') || path.endsWith('/ticket-orders')) return route.fulfill({ json: [] });
    }
    if (path.startsWith('/interactions/')) return route.fulfill({ json: { items: [], total: 0 } });

    // Public music catalog and asset authorization.
    const music = publicRelease(appOrigin);
    if (path === '/music/releases' && method === 'GET') return route.fulfill({ json: [music.summary] });
    if (path === `/music/releases/${RELEASE.slug}`) return route.fulfill({ json: music.release });
    if (path === '/music/playback-events') { state.writes.push({ method, path, body: request.postDataJSON() }); return route.fulfill({ status: 202, json: { accepted: true } }); }
    if (['/music/favorites', '/music/playlists', '/music/history', '/music/entitlements'].includes(path)) return route.fulfill({ json: [] });
    const access = path.match(/^\/music\/assets\/([^/]+)\/access$/)?.[1];
    if (access) {
      if (access === RELEASE.brokenAssetId) return route.fulfill({ status: 403, json: { error: 'asset not authorized' } });
      const isCover = access === RELEASE.coverAssetId;
      return route.fulfill({ json: { url: isCover ? transparentGif : wav.dataUrl, expiresAt: '2030-01-01T00:00:00Z', mediaType: isCover ? 'image/gif' : 'audio/wav',
        byteSize: isCover ? 43 : wav.bytes.length, sha256: 'a'.repeat(64), acceptRanges: !isCover } });
    }

    // Release Studio: draft, uploads, validation, lifecycle.
    const versionBase = `/music/releases/${RELEASE.releaseId}/versions/${RELEASE.versionId}`;
    if (path === versionBase && method === 'GET') return route.fulfill({ json: state.studio });
    if (path === `${versionBase}/ddex-exports` && method === 'GET') return route.fulfill({ json: [] });
    if (path === '/music/infringement-reports' || path === '/music/ddex/parties') return route.fulfill({ json: [] });
    if (path === `/music/analytics/releases/${RELEASE.releaseId}`) {
      const totals = { playStarts: 0, eligiblePlays: 0, completions: 0, skips: 0, listenedMs: 0, uniqueListeners: 0, purchases: 0, downloads: 0 };
      return route.fulfill({ json: { releaseId: RELEASE.releaseId, disclaimer: 'Métricas sintéticas', totals, daily: [], tracks: [], territories: [] } });
    }
    if (path === `${versionBase}/draft` && method === 'PUT') {
      const body = request.postDataJSON(); state.writes.push({ method, path, body });
      state.studio = { ...state.studio, title: body.title ?? state.studio.title, updatedAt: nowIso() };
      return route.fulfill({ json: state.studio });
    }
    if (path === `${versionBase}/content` && method === 'PUT') {
      state.writes.push({ method, path, body: request.postDataJSON() });
      state.studio = { ...state.studio, updatedAt: nowIso() };
      return route.fulfill({ json: state.studio });
    }
    if (path === `${versionBase}/uploads` && method === 'POST') {
      const body = request.postDataJSON();
      const key = request.headers()['idempotency-key'] ?? null;
      state.writes.push({ method, path, body, idempotencyKey: key });
      let session = key ? state.uploadSessionsByKey.get(key) : undefined;
      if (!session) {
        session = { id: `upload-${state.uploadSessions.size + 1}`, status: 'initiated', partSizeBytes: 5_000_000, providerUploadIdBound: true, createMultipartUrl: null,
          expectedSha256: body.expectedSha256, originalFilename: body.originalFilename, parts: [] };
        state.uploadSessions.set(session.id, session);
        if (key) state.uploadSessionsByKey.set(key, session);
      }
      return route.fulfill({ json: { ...session, parts: state.uploadedParts.get(session.id) ?? [] } });
    }
    const uploadMatch = path.match(/^\/music\/uploads\/([^/]+)(\/.*)?$/);
    if (uploadMatch) {
      const [, sessionId, rest = ''] = uploadMatch;
      const session = state.uploadSessions.get(sessionId);
      if (!session) return route.fulfill({ status: 404, json: { error: 'unknown upload session' } });
      const partMatch = rest.match(/^\/parts\/(\d+)$/);
      if (partMatch && method === 'GET') return route.fulfill({ json: { url: `${API_ORIGIN}/synthetic-storage/${sessionId}/part/${partMatch[1]}`, partNumber: Number(partMatch[1]) } });
      if (partMatch && method === 'PUT') {
        const body = request.postDataJSON(); state.writes.push({ method, path, body });
        const parts = state.uploadedParts.get(sessionId) ?? [];
        parts.push({ partNumber: Number(partMatch[1]), byteSize: body.byteSize, etag: body.etag, sha256: body.sha256, uploadedAt: nowIso() });
        state.uploadedParts.set(sessionId, parts);
        return route.fulfill({ json: {} });
      }
      if (rest === '/completion') return route.fulfill({ json: { url: `${API_ORIGIN}/synthetic-storage/${sessionId}/complete`, method: 'POST', contentType: 'application/xml', body: '<CompleteMultipartUpload/>' } });
      if (rest === '/confirm' && method === 'POST') {
        state.writes.push({ method, path, body: request.postDataJSON() });
        session.status = 'completed';
        state.studio = { ...state.studio, state: 'processing', assets: [{ id: '50000000-0000-4000-8000-0000000000aa', recordingId: RELEASE.recordingId, parentAssetId: null, role: 'master_audio',
          originalFilename: session.originalFilename, mediaType: 'audio/wav', byteSize: 4, sha256: session.expectedSha256, processingState: 'queued', technicalMetadata: {}, createdAt: nowIso(), readyAt: null }], updatedAt: nowIso() };
        return route.fulfill({ json: {} });
      }
      if (rest === '' && method === 'DELETE') { session.status = 'cancelled'; return route.fulfill({ json: { abortUrl: null } }); }
    }
    const storage = path.match(/^\/synthetic-storage\/([^/]+)\/(part\/(\d+)|complete)$/);
    if (storage) {
      state.writes.push({ method, path, bytes: request.postDataBuffer()?.length ?? 0 });
      if (storage[2] === 'complete') {
        return route.fulfill({ status: 200, contentType: 'application/xml', headers: { 'Access-Control-Expose-Headers': 'ETag', ETag: '"final-etag"' },
          body: '<CompleteMultipartUploadResult><ETag>"final-etag"</ETag></CompleteMultipartUploadResult>' });
      }
      return route.fulfill({ status: 200, headers: { 'Access-Control-Expose-Headers': 'ETag', ETag: `"etag-${storage[3]}"` }, body: '' });
    }
    if (path === `${versionBase}/terms` && method === 'POST') { state.writes.push({ method, path, body: request.postDataJSON() }); return route.fulfill({ json: {} }); }
    if (path === `${versionBase}/validate` && method === 'POST') { state.writes.push({ method, path }); return route.fulfill({ json: { valid: true, errors: [] } }); }
    if (path === `${versionBase}/transition` && method === 'POST') {
      const body = request.postDataJSON();
      const key = request.headers()['idempotency-key'] ?? null;
      state.writes.push({ method, path, body, idempotencyKey: key });
      if (key && state.transitions.has(key)) return route.fulfill({ json: state.transitions.get(key) });
      const nextState = body.targetState === 'ready_for_review' ? 'in_review' : body.targetState;
      state.studio = { ...state.studio, state: nextState, updatedAt: nowIso() };
      if (key) state.transitions.set(key, state.studio);
      return route.fulfill({ json: state.studio });
    }
    if (path.startsWith('/music/studio/releases')) return route.fulfill({ json: [] });
    return route.fulfill({ status: 404, json: { error: `No synthetic fixture for ${method} ${path}` } });
  }

  async function install(context, appOrigin) {
    await context.route('**/*', async (route) => {
      const request = route.request();
      const url = new URL(request.url());
      if (url.origin === API_ORIGIN) return handleApi(route, request, url, appOrigin);
      if (url.origin === appOrigin) {
        if (url.pathname === '/fixture-radio.wav') return route.fulfill({ contentType: 'audio/wav', body: wav.bytes });
        return route.continue();
      }
      return route.abort('blockedbyclient');
    });
  }

  return { state, install, failNext, requestsTo: (path) => state.requests.filter((entry) => entry.path === path), writesTo: (path) => state.writes.filter((entry) => entry.path === path) };
}
