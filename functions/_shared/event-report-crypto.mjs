const TOKEN = /^[A-Za-z0-9_-]{43}$/;
const DIGEST = /^[a-f0-9]{64}$/;
const BASE64URL = /^[A-Za-z0-9_-]+$/;
const AAD = new TextEncoder().encode('tdf-event-report:v1');

export function encodeBase64Url(bytes) {
  let binary = '';
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return btoa(binary).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
}

function decodeBase64Url(value) {
  if (typeof value !== 'string' || !BASE64URL.test(value)) throw new Error('Invalid encoding');
  const binary = atob(value.replace(/-/g, '+').replace(/_/g, '/') + '='.repeat((4 - value.length % 4) % 4));
  const bytes = Uint8Array.from(binary, character => character.charCodeAt(0));
  if (encodeBase64Url(bytes) !== value) throw new Error('Noncanonical encoding');
  return bytes;
}

export async function tokenDigest(token) {
  const bytes = await crypto.subtle.digest('SHA-256', new TextEncoder().encode(token));
  return Array.from(new Uint8Array(bytes), byte => byte.toString(16).padStart(2, '0')).join('');
}

async function capabilityKey(token, usage) {
  if (typeof token !== 'string' || !TOKEN.test(token)) throw new Error('Invalid capability');
  const bytes = decodeBase64Url(token);
  if (bytes.byteLength !== 32) throw new Error('Invalid capability');
  return crypto.subtle.importKey('raw', bytes, 'AES-GCM', false, [usage]);
}

export function isEventReportPayload(report) {
  if (!report || typeof report !== 'object' || Array.isArray(report)) return false;
  const fields = ['eventId', 'title', 'date', 'venue', 'snapshotDate', 'currency',
    'venueAmountLabel', 'profitExplanation', 'beerCaption', 'beerExplanation',
    'reconciliation', 'accounting', 'source'];
  if (!fields.every(field => typeof report[field] === 'string' && report[field].length <= 16000)) return false;
  if (!/^\d+$/.test(report.eventId) || !/^[A-Z]{3}$/.test(report.currency)) return false;
  if (!['reportedGrossCents', 'owedToVenueCents', 'provisionalProfitCents', 'provisionalShareCents']
    .every(field => Number.isSafeInteger(report[field]))) return false;
  if (!Array.isArray(report.beer) || report.beer.length > 100 || !report.beer.every(row =>
    row && typeof row.label === 'string' && row.label.length <= 16000
    && Number.isSafeInteger(row.pints) && row.pints >= 0 && Number.isSafeInteger(row.costCents))) return false;
  return Array.isArray(report.pending) && report.pending.length <= 100
    && report.pending.every(item => typeof item === 'string' && item.length <= 16000);
}

// Offline authoring only. A new nonce is generated for every sealed revision.
export async function sealEventReport(report, token) {
  if (!isEventReportPayload(report)) throw new Error('Invalid report payload');
  const plaintext = new TextEncoder().encode(JSON.stringify(report));
  if (plaintext.byteLength > 131072) throw new Error('Report payload too large');
  const nonce = crypto.getRandomValues(new Uint8Array(12));
  const key = await capabilityKey(token, 'encrypt');
  const ciphertext = await crypto.subtle.encrypt({ name: 'AES-GCM', iv: nonce, additionalData: AAD, tagLength: 128 }, key, plaintext);
  return { version: 1, tokenSha256: await tokenDigest(token), nonce: encodeBase64Url(nonce),
    ciphertext: encodeBase64Url(new Uint8Array(ciphertext)) };
}

export async function openEventReport(envelope, token) {
  try {
    if (!envelope || envelope.version !== 1 || !DIGEST.test(envelope.tokenSha256)
      || typeof envelope.ciphertext !== 'string' || envelope.ciphertext.length > 180000) return null;
    if (await tokenDigest(token) !== envelope.tokenSha256) return null;
    const nonce = decodeBase64Url(envelope.nonce), ciphertext = decodeBase64Url(envelope.ciphertext);
    if (nonce.byteLength !== 12 || ciphertext.byteLength < 17 || ciphertext.byteLength > 131088) return null;
    const key = await capabilityKey(token, 'decrypt');
    const plaintext = await crypto.subtle.decrypt({ name: 'AES-GCM', iv: nonce, additionalData: AAD, tagLength: 128 }, key, ciphertext);
    const report = JSON.parse(new TextDecoder('utf-8', { fatal: true }).decode(plaintext));
    return isEventReportPayload(report) ? report : null;
  } catch {
    // Authentication, decoding and payload failures share the same denial.
    return null;
  }
}
