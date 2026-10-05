/** Read-only preflight. The repository secret stays inside its existing Actions runner. */
import { writeFile } from 'node:fs/promises';

const origin = 'https://api.tdfrecords.net';
const token = process.env.ADMIN_TOKEN;
if (!token) throw new Error('ADMIN_TOKEN is not available; no API mutation attempted');
const normalized = (value) => String(value ?? '').normalize('NFD').replace(/\p{M}/gu, '').toLowerCase();
async function get(path, allowUndeployed = false) {
  const response = await fetch(`${origin}${path}`, {
    redirect: 'error', signal: AbortSignal.timeout(30000),
    headers: { Authorization: `Bearer ${token}`, Accept: 'application/json' },
  });
  if (response.status === 404 && allowUndeployed) return null;
  if (!response.ok) throw new Error(`Read-only API preflight failed: ${response.status} at ${path.split('?')[0]}`);
  return response.json();
}
async function all(kind) {
  const result = [];
  for (let offset = 0; offset < 25000; offset += 500) {
    const page = await get(`/social-events/${kind}?limit=500&offset=${offset}`);
    if (!Array.isArray(page)) throw new Error(`Invalid ${kind} response`);
    result.push(...page);
    if (page.length < 500) return result;
  }
  throw new Error('Pagination bound reached; deduplication is not complete');
}
const [events, venues, artists] = await Promise.all([all('events'), all('venues'), all('artists')]);
const paymentOverview = await get('/admin/commerce/overview', true);
const report = {
  checkedAt: new Date().toISOString(), apiOrigin: origin, mode: 'read-only', mutations: 0,
  providerReadinessAvailable: paymentOverview !== null,
  providerReadinessLimitation: paymentOverview === null ? 'Canonical API returned 404 for the payment overview; provider readiness is unverified, not absent or approved.' : null,
  providerReadiness: paymentOverview?.cpoProviderAccounts.map((account) => ({
    provider: account.cpaProvider, environment: account.cpaEnvironment,
    status: account.cpaStatus, contractStatus: account.cpaContractStatus,
    credentialStatus: account.cpaCredentialStatus, enabled: account.cpaEnabled,
    verifiedAt: account.cpaVerifiedAt,
  })) ?? [],
  events: events.filter((x) => normalized(x.eventTitle).includes('patch culture')).map((x) => ({
    id: x.eventId, workflow: x.eventWorkflowStateCode, public: x.eventIsPublic,
    purchaseEnabled: x.eventTicketPurchaseEnabled,
  })),
  venues: venues.filter((x) => normalized(x.venueName).includes('andes')).map((x) => ({ id: x.venueId, name: x.venueName, city: x.venueCity })),
  artists: artists.filter((x) => ['llama este pez', 'kevin montenegro', 'diego saa', 'emanuele pilo-pais']
    .includes(normalized(x.artistName))).map((x) => ({ id: x.artistId, name: x.artistName, hasPartyLink: Boolean(x.artistPartyId) })),
};
await writeFile('ticketing-event-access.json', `${JSON.stringify(report, null, 2)}\n`);
console.log(`Read-only preflight complete: ${report.events.length} event, ${report.venues.length} venue and ${report.artists.length} artist candidates. No mutations.`);
