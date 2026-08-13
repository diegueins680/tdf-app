import { readdirSync, readFileSync } from 'node:fs';
import { gzipSync } from 'node:zlib';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const SCRIPT_DIR = path.dirname(fileURLToPath(import.meta.url));
const DIST_DIR = path.resolve(SCRIPT_DIR, '..', 'dist');
const INDEX_PATH = path.join(DIST_DIR, 'index.html');
const MAX_INITIAL_GZIP_BYTES = 400 * 1024;
const MAX_MODULE_PRELOADS = 8;
const ROUTE_CHUNK_PATTERN = /(?:^|\/)(?:fullcalendar|mui-x|dnd|qrcode)-|Page-/;
const SECRET_PATTERNS = [
  { label: 'OpenAI-style API key', pattern: /\bsk-(?:proj-|svcacct-)?[A-Za-z0-9_-]{20,}\b/ },
];

const listJavaScriptFiles = (directory) =>
  readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const entryPath = path.join(directory, entry.name);
    if (entry.isDirectory()) return listJavaScriptFiles(entryPath);
    return entry.isFile() && entry.name.endsWith('.js') ? [entryPath] : [];
  });

const html = readFileSync(INDEX_PATH, 'utf8');
const preloadPaths = [...html.matchAll(/<link[^>]+rel="modulepreload"[^>]+href="([^"]+\.js)"/g)]
  .map((match) => match[1]);
const entryPaths = [...html.matchAll(/<script[^>]+src="([^"]+\.js)"/g)]
  .map((match) => match[1]);
const initialPaths = [...new Set([...entryPaths, ...preloadPaths])];

const disallowedPreloads = preloadPaths.filter((assetPath) => ROUTE_CHUNK_PATTERN.test(assetPath));
const gzipBytes = initialPaths.reduce((total, assetPath) => {
  const normalizedPath = assetPath.replace(/^\//, '');
  return total + gzipSync(readFileSync(path.join(DIST_DIR, normalizedPath))).byteLength;
}, 0);

const failures = [];
if (preloadPaths.length > MAX_MODULE_PRELOADS) {
  failures.push(`module preloads ${preloadPaths.length} exceed ${MAX_MODULE_PRELOADS}`);
}
if (disallowedPreloads.length > 0) {
  failures.push(`route-only chunks were preloaded: ${disallowedPreloads.join(', ')}`);
}
if (gzipBytes > MAX_INITIAL_GZIP_BYTES) {
  failures.push(`initial JS ${gzipBytes} bytes gzip exceeds ${MAX_INITIAL_GZIP_BYTES}`);
}
for (const assetPath of listJavaScriptFiles(DIST_DIR)) {
  const contents = readFileSync(assetPath, 'utf8');
  for (const { label, pattern } of SECRET_PATTERNS) {
    if (pattern.test(contents)) {
      failures.push(`potential ${label} embedded in ${path.relative(DIST_DIR, assetPath)}`);
    }
  }
}

if (failures.length > 0) {
  console.error(`[bundle-budget] ${failures.join('; ')}`);
  process.exitCode = 1;
} else {
  console.log(`[bundle-budget] ${preloadPaths.length} preloads, ${gzipBytes} bytes gzip initial JS`);
}
