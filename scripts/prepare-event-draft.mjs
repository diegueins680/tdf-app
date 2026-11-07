/** Prepare a reviewed event through existing APIs; never publish, charge or notify. */
import { readFile, writeFile } from 'node:fs/promises';
import { resolve, dirname } from 'node:path';
import { pathToFileURL } from 'node:url';

export function assertPrivateDraft(event) {
  if (!event.eventId || event.eventIsPublic !== false || event.eventPublicListable !== false
      || event.eventTicketPurchaseEnabled !== false || event.eventWorkflowStateCode !== 'planning') {
    throw new Error('Event is not an explicitly private, non-selling planning draft');
  }
}
export function validateDraftPlan(plan) {
  if (plan.schemaVersion === 2) {
    return validateReviewPlan(plan);
  }
  if (plan.schemaVersion !== 1 || plan.approval !== 'draft-authorized'
      || plan.event.eventIsPublic !== false || !Array.isArray(plan.artists)
      || plan.tier.ticketTierActive !== false || plan.tier.ticketTierQuantitySold !== 0) {
    throw new Error('Only reviewed private drafts with an inactive tier are accepted');
  }
  if (plan.event.eventCapacity !== plan.tier.ticketTierQuantityTotal
      || plan.event.eventPriceCents !== plan.tier.ticketTierPriceCents
      || plan.event.eventCurrency !== plan.tier.ticketTierCurrency) {
    throw new Error('Event and tier dimensions disagree');
  }
}

function validateReviewPlan(plan) {
  const { event, tiers } = plan;
  if (plan.approval !== 'review-only' || event?.eventIsPublic !== false
      || event.eventTicketUrl != null || event.eventImageUrl != null
      || !Array.isArray(event.eventArtists) || !Array.isArray(tiers) || tiers.length === 0) {
    throw new Error('Review plans must be private and must not contain public asset or ticket URLs');
  }
  const eventStart = Date.parse(event.eventStart);
  const eventEnd = Date.parse(event.eventEnd);
  if (!Number.isFinite(eventStart) || !Number.isFinite(eventEnd) || eventEnd <= eventStart
      || !Number.isSafeInteger(event.eventCapacity) || event.eventCapacity < 1) {
    throw new Error('Review plans require an ordered event window and positive integer capacity');
  }
  const codes = new Set();
  let capacity = 0;
  let lowestPrice = Infinity;
  for (const tier of tiers) {
    const code = tier.ticketTierCode?.trim().toUpperCase();
    const saleStart = tier.ticketTierSalesStart == null ? null : Date.parse(tier.ticketTierSalesStart);
    const saleEnd = Date.parse(tier.ticketTierSalesEnd);
    if (!code || codes.has(code) || tier.ticketTierActive !== false || tier.ticketTierQuantitySold !== 0
        || tier.ticketTierCurrency !== event.eventCurrency
        || !Number.isSafeInteger(tier.ticketTierPriceCents) || tier.ticketTierPriceCents < 0
        || !Number.isSafeInteger(tier.ticketTierQuantityTotal) || tier.ticketTierQuantityTotal < 1
        || !Number.isFinite(saleEnd) || saleEnd > eventEnd
        || (saleStart !== null && (!Number.isFinite(saleStart) || saleStart >= saleEnd))) {
      throw new Error('Review tiers require unique codes, inactive zero-sale inventory, matching currency and a valid sale window');
    }
    codes.add(code);
    capacity += tier.ticketTierQuantityTotal;
    lowestPrice = Math.min(lowestPrice, tier.ticketTierPriceCents);
  }
  if (!Number.isSafeInteger(capacity) || capacity > event.eventCapacity || event.eventPriceCents !== lowestPrice) {
    throw new Error('Review tiers exceed event capacity or the event price differs from the lowest tier price');
  }
}
const normalize = (text) => String(text ?? '').normalize('NFD').replace(/\p{M}/gu, '').trim().toLowerCase();
async function main() {
  const validateOnly = process.argv[2] === '--validate-only';
  const planPath = resolve(process.argv[validateOnly ? 3 : 2] ?? '');
  const plan = JSON.parse(await readFile(planPath, 'utf8'));
  validateDraftPlan(plan);
  if (validateOnly) {
    console.log(JSON.stringify({ valid: true, mode: 'offline', schemaVersion: plan.schemaVersion, mutations: 0 }));
    return;
  }
  if (plan.schemaVersion === 2) {
    throw new Error('Review plans are offline only. Resolve the organizer, venue, artists and operational inventory before importing through TDF.');
  }
  // Validate the local asset before any profile/event mutation.
  const flyerBytes = await readFile(resolve(dirname(planPath), plan.flyer));
  const token = process.env.ADMIN_TOKEN;
  if (!token) throw new Error('Authorized ADMIN_TOKEN required');
  const origin = 'https://api.tdfrecords.net';
  let mutations = 0;
  async function api(path, method = 'GET', payload) {
    const isForm = payload instanceof FormData;
    const response = await fetch(`${origin}${path}`, {
      method, redirect: 'error', signal: AbortSignal.timeout(60000),
      headers: { Authorization: `Bearer ${token}`, ...(isForm ? {} : { 'Content-Type': 'application/json' }) },
      body: payload == null ? undefined : isForm ? payload : JSON.stringify(payload),
    });
    if (!response.ok) throw new Error(`Draft operation failed: ${method} ${path} (${response.status}); inspect existing draft before retry`);
    if (method !== 'GET') mutations++;
    return response.json();
  }
  async function all(kind) {
    const rows = [];
    for (let offset = 0; offset < 25000; offset += 500) {
      const page = await api(`/social-events/${kind}?limit=500&offset=${offset}`);
      if (!Array.isArray(page)) throw new Error(`Invalid ${kind} page`);
      rows.push(...page);
      if (page.length < 500) return rows;
    }
    throw new Error('Incomplete deduplication');
  }
  const [events, venues, artists, catalog, lifecycle] = await Promise.all([
    all('events'), all('venues'), all('artists'), api('/catalogs/event-types/items?pageSize=100'),
    api('/catalogs/workflows/social-event-lifecycle/states'),
  ]);
  const initial = lifecycle.states.filter((s) => s.active && s.initialContexts.includes('initial'));
  if (initial.length !== 1 || initial[0].code !== 'planning' || initial[0].capabilities.length) {
    throw new Error('Initial event state is not an isolated planning state');
  }
  const types = catalog.items.filter((x) => x.code === plan.eventTypeCode && x.active && x.workflowState === 'published');
  if (types.length !== 1) throw new Error('Event type missing or ambiguous');
  const matches = events.filter((e) => normalize(e.eventTitle) === normalize(plan.event.eventTitle));
  if (matches.length > 1) throw new Error('Duplicate event candidates require resolution');
  let event = matches[0];
  if (event) {
    assertPrivateDraft(event);
    if (event.eventStart !== plan.event.eventStart || !event.eventDescription?.includes(plan.sourceUrl)) {
      throw new Error('Existing event does not match the reviewed source and date');
    }
  }
  const venueMatches = venues.filter((v) => normalize(v.venueName).includes(normalize(plan.venueMatch)));
  if (venueMatches.length > 1) throw new Error('Ambiguous venue');
  const venue = venueMatches[0] ?? await api('/social-events/venues', 'POST', plan.venue);
  if (normalize(venue.venueCity) !== normalize(plan.venue.venueCity)) throw new Error('Venue city mismatch');
  const linked = [];
  for (const name of plan.artists) {
    const found = artists.filter((a) => normalize(a.artistName) === normalize(name));
    if (found.length > 1) throw new Error('Ambiguous artist; do not guess an identity');
    const artist = found[0] ?? await api('/social-events/artists', 'POST', { artistName: name, artistGenreIds: [] });
    linked.push({ artistId: artist.artistId, artistName: artist.artistName, artistGenreIds: [] });
  }
  if (!event) {
    event = await api('/social-events/events', 'POST', {
      ...plan.event, eventTypeId: types[0].id, eventWorkflowStateId: initial[0].id,
      eventVenueId: venue.venueId, eventArtists: linked,
    });
    // A failed projection check must stop all subsequent writes.
    assertPrivateDraft(event);
  }
  const path = `/social-events/events/${encodeURIComponent(event.eventId)}`;
  if (!event.eventImageUrl) {
    const form = new FormData();
    form.append('file', new Blob([flyerBytes], { type: 'image/png' }), 'patch-culture-original-frame.png');
    await api(`${path}/image`, 'POST', form);
  }
  const tiers = await api(`${path}/ticket-tiers`);
  const matchingTiers = tiers.filter((t) => t.ticketTierCode === plan.tier.ticketTierCode);
  if (matchingTiers.length > 1) throw new Error('Duplicate draft tiers');
  const tier = matchingTiers[0] ?? await api(`${path}/ticket-tiers`, 'POST', plan.tier);
  for (const field of ['ticketTierActive', 'ticketTierPriceCents', 'ticketTierCurrency', 'ticketTierQuantityTotal', 'ticketTierQuantitySold']) {
    if (tier[field] !== plan.tier[field]) throw new Error('Existing tier differs from the reviewed inactive plan');
  }
  const verified = await api(path);
  assertPrivateDraft(verified);
  const report = { checkedAt: new Date().toISOString(), eventId: verified.eventId, venueId: venue.venueId,
    artistIds: linked.map((a) => a.artistId), tierId: tier.ticketTierId, mutations,
    workflow: verified.eventWorkflowStateCode, public: false, salesEnabled: false,
    imageUrl: verified.eventImageUrl, candidateUrl: `https://www.tdfrecords.net/eventos/${verified.eventId}`,
    unlinkedParticipants: plan.unlinkedParticipants,
  };
  await writeFile('ticketing-event-draft.json', `${JSON.stringify(report, null, 2)}\n`);
  console.log(`Verified private draft ${verified.eventId}; tier ${tier.ticketTierId} inactive; no payment or notification initiated.`);
}
if (process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href) await main();
