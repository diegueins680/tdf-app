import { studioAccount } from '../../_shared/studio-account-data.mjs';
import { reportHeaders, tokenDigest } from '../../_shared/event-report.mjs';

const bytes = (base64) => Uint8Array.from(atob(base64), char => char.charCodeAt(0));

export async function onRequest({ request, params }) {
  if (!['GET', 'HEAD'].includes(request.method)) {
    return new Response(null, { status: 405, headers: { ...reportHeaders, Allow: 'GET, HEAD' } });
  }
  const token = params.token;
  if (/^[A-Za-z0-9_-]{43}$/.test(token) && await tokenDigest(token) === studioAccount.digest) {
    const key = await crypto.subtle.importKey('raw', bytes(token.replace(/-/g, '+').replace(/_/g, '/')), 'AES-GCM', false, ['decrypt']);
    const content = await crypto.subtle.decrypt({ name: 'AES-GCM', iv: bytes(studioAccount.nonce) }, key, bytes(studioAccount.html));
    return new Response(request.method === 'HEAD' ? null : content, { headers: reportHeaders });
  }
  return new Response(null, { status: 404, headers: reportHeaders });
}
