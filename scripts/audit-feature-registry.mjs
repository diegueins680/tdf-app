import { createHash } from 'node:crypto';
import { readdir, readFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const workspaceRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const registryPath = path.join(workspaceRoot, 'tdf-hq/assets/feature-registry.json');
const publicRoutesPath = path.join(workspaceRoot, 'tdf-hq-ui/src/routes/publicRoutes.tsx');
const protectedRoutesPath = path.join(workspaceRoot, 'tdf-hq-ui/src/routes/protectedRoutes.tsx');
const mobileAppPath = path.join(workspaceRoot, 'tdf-mobile/app');
const generatedMobilePath = path.join(workspaceRoot, 'tdf-mobile/src/features/generatedFeatureRegistry.ts');

const failures = [];
const fail = (message) => failures.push(message);
const normalizedRoute = (route) => {
  if (route === '/') return route;
  return route.replace(/[?#].*$/, '').replace(/\/$/, '');
};
const routeShape = (route) => normalizedRoute(route)
  .replace(/\[[^/]+\]/g, ':param')
  .replace(/:[^/]+/g, ':param');

function mergeFeature(defaults, feature) {
  return {
    ...defaults,
    ...feature,
    description: feature.description ?? defaults.description,
    permissions: feature.permissions ?? defaults.permissions,
    mobilePresentation: feature.mobilePresentation ?? defaults.mobilePresentation,
    mobileAliases: feature.mobileAliases ?? defaults.mobileAliases ?? [],
    telemetryId: feature.telemetryId ?? feature.id,
  };
}

function combineRoute(parent, child) {
  if (!child) return parent;
  if (child.startsWith('/')) return normalizedRoute(child);
  return normalizedRoute(`${parent && parent !== '/' ? parent : ''}/${child}`);
}

function extractReactRoutes(source, sourceName) {
  const routes = [];
  const stack = [];
  for (const [index, rawLine] of source.split('\n').entries()) {
    const line = rawLine.trim();
    if (line.startsWith('</Route')) {
      stack.pop();
      continue;
    }
    if (!line.startsWith('<Route')) continue;
    const pathMatch = line.match(/\bpath=(?:"([^"]+)"|'([^']+)')/);
    const route = pathMatch?.[1] ?? pathMatch?.[2] ?? null;
    const parent = [...stack].reverse().find((entry) => entry !== null) ?? '';
    const combined = route ? combineRoute(parent, route) : parent;
    if (route && route !== '*') routes.push({ route: combined, source: `${sourceName}:${index + 1}` });
    if (!line.endsWith('/>')) stack.push(combined || null);
  }
  return routes;
}

async function collectFiles(directory) {
  const result = [];
  for (const entry of await readdir(directory, { withFileTypes: true })) {
    const absolute = path.join(directory, entry.name);
    if (entry.isDirectory()) result.push(...await collectFiles(absolute));
    else result.push(absolute);
  }
  return result;
}

function expoRouteForFile(file) {
  const relative = path.relative(mobileAppPath, file).replaceAll(path.sep, '/');
  if (!/\.[cm]?[jt]sx?$/.test(relative)) return null;
  const withoutExtension = relative.replace(/\.[^.]+$/, '');
  const segments = withoutExtension.split('/');
  if (segments.at(-1) === '_layout' || segments.at(-1) === '+html') return null;
  const visible = segments.filter((segment) => !/^\(.+\)$/.test(segment));
  if (visible.at(-1) === 'index') visible.pop();
  return normalizedRoute(`/${visible.join('/')}`);
}

function nativeRouteShape(route) {
  const withoutGroups = route.split('/').filter((segment) => !/^\(.+\)$/.test(segment)).join('/');
  return routeShape(withoutGroups.startsWith('/') ? withoutGroups : `/${withoutGroups}`);
}

const rawRegistry = await readFile(registryPath, 'utf8');
const registry = JSON.parse(rawRegistry);
const features = registry.features.map((feature) => mergeFeature(registry.defaults ?? {}, feature));
const featureIds = new Set();
const telemetryIds = new Set();
const groupIds = new Set(registry.groups.map((group) => group.id));
const classificationValues = new Set([
  'primary-destination',
  'important-submodule',
  'record-detail-contextual-action',
  'technical-route',
]);
const actionValues = new Set([
  'discover', 'view', 'create', 'edit', 'delete', 'archive', 'deactivate', 'import',
  'export', 'submit', 'validate', 'approve', 'reject', 'assign', 'publish', 'report',
  'administer',
]);

for (const feature of features) {
  const prefix = `feature ${feature.id ?? '<missing-id>'}`;
  if (!feature.id || typeof feature.id !== 'string') fail(`${prefix}: id is required`);
  if (featureIds.has(feature.id)) fail(`${prefix}: duplicate id`);
  featureIds.add(feature.id);
  if (!feature.telemetryId || telemetryIds.has(feature.telemetryId)) {
    fail(`${prefix}: telemetryId must be present and unique`);
  }
  telemetryIds.add(feature.telemetryId);
  if (!classificationValues.has(feature.classification)) fail(`${prefix}: invalid classification ${feature.classification}`);
  if (!feature.label?.es?.trim() || !feature.label?.en?.trim()) fail(`${prefix}: Spanish and English labels are required`);
  if (!Array.isArray(feature.synonyms?.es) || !Array.isArray(feature.synonyms?.en)) fail(`${prefix}: bilingual synonyms are required`);
  if (!Array.isArray(feature.keywords) || feature.keywords.length === 0) fail(`${prefix}: search keywords are required`);
  if (!feature.description?.es?.trim() || !feature.description?.en?.trim()) fail(`${prefix}: bilingual descriptions are required`);
  if (!feature.icon?.trim()) fail(`${prefix}: icon is required`);
  if (!Array.isArray(feature.breadcrumb)) fail(`${prefix}: breadcrumb hierarchy is required`);
  if (!['public', 'optional', 'authenticated'].includes(feature.requiredAuth)) fail(`${prefix}: invalid requiredAuth`);
  if (!actionValues.has(feature.routeAction)) fail(`${prefix}: invalid routeAction ${feature.routeAction}`);
  if (!feature.permissions?.[feature.routeAction]) fail(`${prefix}: routeAction ${feature.routeAction} lacks a permission rule`);
  if (feature.navigationGroup && !groupIds.has(feature.navigationGroup)) fail(`${prefix}: unknown navigation group ${feature.navigationGroup}`);
  if (!feature.mobilePresentation?.kind) fail(`${prefix}: mobile presentation metadata is required`);
  if (feature.mobilePresentation?.kind === 'native' && !feature.mobilePresentation.destination) fail(`${prefix}: native feature lacks a destination`);
  if (['web-only', 'security-concealed'].includes(feature.mobilePresentation?.kind)
    && !feature.mobilePresentation.documentedException) {
    fail(`${prefix}: ${feature.mobilePresentation.kind} mobile treatment needs a documented exception`);
  }
  if (feature.technical && (feature.globalMenu || feature.searchable || feature.quickCreate)) {
    fail(`${prefix}: technical routes must never be displayed or searched`);
  }
  if (feature.classification === 'technical-route' && !feature.technical) fail(`${prefix}: technical classification must set technical=true`);
  if (feature.technical && feature.classification !== 'technical-route') fail(`${prefix}: technical feature needs technical-route classification`);
  if (feature.globalMenu && feature.classification === 'record-detail-contextual-action') fail(`${prefix}: contextual routes cannot be global-menu entries`);
  if (feature.quickCreate) {
    if (!actionValues.has(feature.quickCreate.action) || !feature.permissions?.[feature.quickCreate.action]) {
      fail(`${prefix}: quick-create action lacks an exact permission rule`);
    }
    if (!feature.quickCreate.label?.es?.trim() || !feature.quickCreate.label?.en?.trim()) {
      fail(`${prefix}: quick-create needs bilingual labels`);
    }
    if (!feature.quickCreate.webDestination && !feature.quickCreate.mobileDestination) {
      fail(`${prefix}: quick-create lacks a destination`);
    }
  }
  for (const scope of feature.apiScopes ?? []) {
    if (!scope.pathPrefix?.startsWith('/')) fail(`${prefix}: API scope needs an absolute pathPrefix`);
    for (const [method, action] of Object.entries(scope.methods ?? {})) {
      if (!/^(GET|POST|PUT|PATCH|DELETE|RAW)$/.test(method)) fail(`${prefix}: invalid API method ${method}`);
      if (!actionValues.has(action)) fail(`${prefix}: API method ${method} maps to invalid action ${action}`);
      if (!feature.technical && !feature.permissions?.[action]) {
        fail(`${prefix}: API method ${method} maps to ${action} without a permission rule`);
      }
    }
  }
}

for (const feature of features) {
  if (feature.parentId && !featureIds.has(feature.parentId)) fail(`feature ${feature.id}: unknown parent ${feature.parentId}`);
  for (const crumb of feature.breadcrumb) {
    if (!featureIds.has(crumb)) fail(`feature ${feature.id}: unknown breadcrumb feature ${crumb}`);
  }
}

const webSources = await Promise.all([
  readFile(publicRoutesPath, 'utf8'),
  readFile(protectedRoutesPath, 'utf8'),
]);
const actualWebRoutes = webSources.flatMap((source, index) =>
  extractReactRoutes(source, index === 0 ? 'publicRoutes.tsx' : 'protectedRoutes.tsx'));
const actualWebShapes = new Set(actualWebRoutes.map(({ route }) => routeShape(route)));
const registeredWebRoutes = features.flatMap((feature) => [feature.webRoute, ...(feature.aliases ?? [])]
  .filter((route) => typeof route === 'string' && route.startsWith('/'))
  .map((route) => ({ route, featureId: feature.id })));
const registeredWebShapes = new Set(registeredWebRoutes.map(({ route }) => routeShape(route)));

for (const { route, source } of actualWebRoutes) {
  if (!registeredWebShapes.has(routeShape(route))) fail(`web route ${route} (${source}) is missing from the registry`);
}
for (const { route, featureId } of registeredWebRoutes) {
  if (!actualWebShapes.has(routeShape(route))) fail(`feature ${featureId}: registered web route/alias ${route} does not exist`);
}

const mobileFiles = await collectFiles(mobileAppPath);
const actualMobileRoutes = mobileFiles.map((file) => ({ file, route: expoRouteForFile(file) }))
  .filter(({ route }) => route !== null);
const actualMobileShapes = new Set(actualMobileRoutes.map(({ route }) => nativeRouteShape(route)));
const registeredMobileRoutes = features.flatMap((feature) => [
  feature.mobilePresentation?.destination,
  ...(feature.mobileAliases ?? []),
].filter((route) => typeof route === 'string' && route.startsWith('/') && !route.startsWith('//'))
  .map((route) => ({ route, featureId: feature.id })));
const registeredMobileShapes = new Set(registeredMobileRoutes.map(({ route }) => nativeRouteShape(route)));

for (const { route, file } of actualMobileRoutes) {
  if (!registeredMobileShapes.has(nativeRouteShape(route))) {
    fail(`mobile route ${route} (${path.relative(workspaceRoot, file)}) is missing from the registry`);
  }
}
for (const { route, featureId } of registeredMobileRoutes) {
  if (!actualMobileShapes.has(nativeRouteShape(route))) fail(`feature ${featureId}: mobile destination/alias ${route} does not exist`);
}

for (const feature of features.filter((entry) => entry.quickCreate)) {
  const webDestination = feature.quickCreate.webDestination;
  const mobileDestination = feature.quickCreate.mobileDestination;
  if (webDestination && !actualWebShapes.has(routeShape(webDestination))) {
    fail(`feature ${feature.id}: quick-create web destination ${webDestination} does not exist`);
  }
  if (mobileDestination && !actualMobileShapes.has(nativeRouteShape(mobileDestination))) {
    fail(`feature ${feature.id}: quick-create mobile destination ${mobileDestination} does not exist`);
  }
}

const generatedMobile = await readFile(generatedMobilePath, 'utf8');
const generatedDigest = generatedMobile.match(/MOBILE_FEATURE_REGISTRY_SOURCE_SHA256 = '([a-f0-9]+)'/)?.[1];
const sourceDigest = createHash('sha256').update(rawRegistry).digest('hex');
if (generatedDigest !== sourceDigest) fail('generated mobile registry is stale; run npm run generate:features');

if (failures.length > 0) {
  console.error(`Feature registry audit failed with ${failures.length} issue(s):`);
  for (const failure of failures) console.error(`- ${failure}`);
  process.exitCode = 1;
} else {
  console.log(`Feature registry audit passed: ${features.length} features, ${actualWebRoutes.length} web routes, ${actualMobileRoutes.length} mobile routes.`);
}
