export function onRequestGet({ env }) {
  const fingerprints = String(env.ANDROID_APP_LINK_SHA256_CERT_FINGERPRINTS ?? '')
    .split(',')
    .map((value) => value.trim().toUpperCase())
    .filter((value) => /^([0-9A-F]{2}:){31}[0-9A-F]{2}$/.test(value));
  if (fingerprints.length === 0) {
    return Response.json({ error: 'ANDROID_APP_LINK_SHA256_CERT_FINGERPRINTS is not configured' }, { status: 503 });
  }
  return Response.json([{
    relation: ['delegate_permission/common.handle_all_urls'],
    target: {
      namespace: 'android_app',
      package_name: 'com.tdf.records',
      sha256_cert_fingerprints: fingerprints,
    },
  }], {
    headers: { 'Cache-Control': 'public, max-age=3600', 'Content-Type': 'application/json' },
  });
}
