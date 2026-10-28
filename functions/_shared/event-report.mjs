import { patchCultureReport } from './event-report-data.mjs';

const TOKEN = /^[A-Za-z0-9_-]{43}$/;
const DIGEST = /^[a-f0-9]{64}$/;

export const reportHeaders = Object.freeze({
  'Content-Type': 'text/html; charset=utf-8',
  'Cache-Control': 'private, no-store, max-age=0',
  'CDN-Cache-Control': 'no-store',
  'X-Robots-Tag': 'noindex, nofollow, noarchive, nosnippet',
  'Referrer-Policy': 'no-referrer',
  'X-Content-Type-Options': 'nosniff',
  'X-Frame-Options': 'DENY',
  'Content-Security-Policy': "default-src 'none'; img-src 'self'; style-src 'unsafe-inline'; base-uri 'none'; form-action 'none'; frame-ancestors 'none'",
});

export async function tokenDigest(token) {
  const bytes = await crypto.subtle.digest('SHA-256', new TextEncoder().encode(token));
  return Array.from(new Uint8Array(bytes), (byte) => byte.toString(16).padStart(2, '0')).join('');
}

// The complete capability is checked at the edge before any report content is
// rendered. No event-id/name lookup, listing, cookie or login grants access.
export async function resolveEventReport(token, env = {}) {
  if (typeof token !== 'string' || !TOKEN.test(token)) return null;
  if (env.EVENT_REPORT_LINKS_ENABLED !== undefined && env.EVENT_REPORT_LINKS_ENABLED !== 'true') return null;
  const expected = env.PATCH_CULTURE_REPORT_TOKEN_SHA256 === undefined
    ? patchCultureReport.tokenSha256 : env.PATCH_CULTURE_REPORT_TOKEN_SHA256;
  if (typeof expected !== 'string' || !DIGEST.test(expected)) return null;
  return await tokenDigest(token) === expected ? patchCultureReport : null;
}

const escapeHtml = (value) => String(value).replace(/[&<>"']/g, (char) => ({
  '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;',
})[char]);

export function renderEventReport(report) {
  const money = (cents) => escapeHtml(new Intl.NumberFormat('es-EC', {
    style: 'currency', currency: report.currency,
  }).format(cents / 100));
  const e = escapeHtml;
  const metrics = [
    ['Ingresos reportados', report.reportedGrossCents],
    ['Adeudado a Andes', report.owedToVenueCents],
    ['Utilidad provisional', report.provisionalProfitCents],
  ];
  return `<!doctype html>
<html lang="es"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<meta name="robots" content="noindex,nofollow,noarchive,nosnippet"><meta name="referrer" content="no-referrer">
<title>${e(report.title)} · Reporte del evento · TDF Records</title>
<link rel="icon" href="/favicon.ico">
<style>
:root{color-scheme:light;font:16px/1.6 system-ui,sans-serif;color:#192b2f;background:#f4f6f4}
*{box-sizing:border-box}body{margin:0}a{color:#075c57;text-underline-offset:4px}a:focus-visible{outline:3px solid #94651b;outline-offset:5px}
header{background:#fff;border-bottom:1px solid #d6dedb}nav{max-width:1000px;margin:auto;padding:12px 24px;display:flex;align-items:center;justify-content:space-between;gap:16px}
.brand{display:inline-flex;background:#fff}.brand img{width:112px;height:64px;object-fit:cover}main{max-width:1000px;margin:auto;padding:36px 24px 64px}
h1{font-size:clamp(2rem,5vw,3.4rem);line-height:1.15;margin:12px 0}h2{font-size:1.3rem;margin:0 0 12px}p{margin:8px 0 16px}.muted{color:#465a5c}.badge{display:inline-block;border:1px solid #bbcfc7;border-radius:24px;padding:4px 12px;background:#e5f0e9;font-size:.875rem}
.metrics{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:16px;margin:28px 0}.metric,section{background:white;border:1px solid #d6dedb;border-radius:14px;padding:24px}.metric dt{color:#465a5c}.metric dd{margin:8px 0 0;font-size:2rem;font-weight:700;color:#075c57}section{margin:20px 0}.notice{background:#fff7e8;border-color:#d4b779}
table{width:100%;border-collapse:collapse;text-align:left}th,td{padding:12px 8px;border-bottom:1px solid #d6dedb}th[scope=row]{font-weight:500}th:nth-child(2),td:nth-child(2){white-space:nowrap}td:last-child,th:last-child{text-align:right}tfoot{font-weight:700}caption{text-align:left;padding:0 0 12px;color:#465a5c}ul{padding-left:24px}li{margin:8px 0}footer{margin-top:28px;font-size:.875rem;color:#465a5c}.skip{position:absolute;left:12px;top:-80px;padding:8px;background:white}.skip:focus{top:8px}p,li,th{overflow-wrap:anywhere}
@media(max-width:600px){main{padding:24px 16px 40px}.metrics{grid-template-columns:1fr;gap:10px}.metric,section{padding:18px}.metric dd{margin-top:0}nav{padding:8px 16px}th,td{padding:10px 4px}table{font-size:.9rem}}
@media print{header,.skip{display:none}body{background:white}main{padding:0}.metrics{grid-template-columns:repeat(3,1fr)}section,.metric{break-inside:avoid}a{color:inherit}a:after{content:none}}
</style></head><body>
<a class="skip" href="#reporte">Saltar al reporte</a>
<header><nav aria-label="TDF"><a class="brand" href="/tdf" aria-label="TDF Records"><img src="/tdf-logo-wordmark.svg" alt="TDF Records" width="112" height="64"></a><a href="/eventos/${e(report.eventId)}">Ver evento</a></nav></header>
<main id="reporte"><span class="badge">Cierre financiero provisional</span>
<h1>${e(report.title)}</h1><p class="muted">${e(report.date)} · ${e(report.venue)}</p>
<p>Acceso: cualquier persona con este enlace puede ver el reporte. Solo lectura.</p>
<dl class="metrics">${metrics.map(([label, cents]) => `<div class="metric"><dt>${e(label)}</dt><dd>${money(cents)}</dd></div>`).join('')}</dl>
<section class="notice" aria-labelledby="estado"><h2 id="estado">Balance pendiente de conciliación</h2>
<p>Los ${money(report.provisionalProfitCents)} de utilidad son provisionales y no incluyen la comisión de Meet2Go. El reparto propuesto es 50/50: ${money(report.provisionalShareCents)} por parte, antes de conciliar comisiones y fondos.</p></section>
<section aria-labelledby="cervezas"><h2 id="cervezas">Consumo de cerveza</h2>
<table><caption>Siete pintas a USD5 de costo unitario</caption><thead><tr><th scope="col">Destino</th><th scope="col">Pintas</th><th scope="col">Costo USD</th></tr></thead>
<tbody>${report.beer.map((row) => `<tr><th scope="row">${e(row.label)}</th><td>${e(row.pints)}</td><td>${money(row.costCents)}</td></tr>`).join('')}</tbody>
<tfoot><tr><th scope="row">Total</th><td>7</td><td>${money(report.owedToVenueCents)}</td></tr></tfoot></table>
<p>Las dos ventas de cerveza representan USD10. La pinta personal es consumo, sin ingreso de venta. Los USD20 de las pintas incluidas ya forman parte de los USD35; no deben descontarse nuevamente.</p></section>
<section aria-labelledby="diferencia"><h2 id="diferencia">Diferencia por conciliar</h2><p>${e(report.reconciliation)}</p></section>
<section aria-labelledby="pendientes"><h2 id="pendientes">Movimientos pendientes</h2><ul>${report.pending.map((item) => `<li>${e(item)}</li>`).join('')}</ul></section>
<section aria-labelledby="registro"><h2 id="registro">Registro en TDF</h2><p>${e(report.accounting)}</p></section>
<section aria-labelledby="fuente"><h2 id="fuente">Fuente y fecha del cierre</h2><p>${e(report.source)}</p></section>
<footer>Copia del cierre al ${e(report.snapshotDate)}. Puedes usar Imprimir en tu navegador para guardar una copia en PDF.</footer>
</main></body></html>`;
}

export async function eventReportResponse(context) {
  const { request, params, env } = context;
  const headers = { ...reportHeaders };
  if (request.method !== 'GET' && request.method !== 'HEAD') {
    return new Response('Method not allowed', { status: 405, headers: { ...headers, Allow: 'GET, HEAD' } });
  }
  const report = await resolveEventReport(params.token, env);
  const status = report ? 200 : 404;
  const body = request.method === 'HEAD' ? null : report ? renderEventReport(report) : 'Not found';
  return new Response(body, { status, headers });
}
