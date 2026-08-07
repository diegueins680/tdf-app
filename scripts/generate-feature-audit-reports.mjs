import { mkdir, readFile, readdir, writeFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const workspaceRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const registryPath = path.join(workspaceRoot, 'tdf-hq/assets/feature-registry.json');
const backendSourcePath = path.join(workspaceRoot, 'tdf-hq/src');
const reportPath = path.join(workspaceRoot, 'docs/feature-discoverability-audit/2026-08-06');
const actions = [
  'discover', 'view', 'create', 'edit', 'delete', 'archive', 'deactivate', 'import',
  'export', 'submit', 'validate', 'approve', 'reject', 'assign', 'publish', 'report',
  'administer',
];

const roleModules = {
  Admin: ['CRM', 'Scheduling', 'Packages', 'Invoicing', 'Admin', 'Internships', 'Ops', 'Catalog'],
  Manager: ['CRM', 'Scheduling', 'Packages', 'Invoicing', 'Internships', 'Ops', 'Catalog'],
  StudioManager: ['CRM', 'Scheduling', 'Packages', 'Invoicing', 'Admin', 'Internships', 'Ops', 'Catalog'],
  Reception: ['CRM', 'Scheduling'], Accounting: ['Invoicing'], Engineer: ['Scheduling'],
  Teacher: ['Scheduling'], LiveSessionsProducer: ['CRM', 'Scheduling'], Intern: ['Internships'],
  Artist: ['Scheduling', 'Packages'], Artista: ['Scheduling', 'Packages'],
  Webmaster: ['Admin', 'CRM'], Producer: ['CRM', 'Scheduling'], LabelRep: ['Catalog'],
  AandR: ['CRM', 'Scheduling', 'Catalog'], Student: ['Scheduling'], Vendor: ['Packages'],
  Customer: ['Packages'], ReadOnly: ['CRM', 'Catalog'], Maintenance: ['Packages', 'Scheduling', 'Ops'],
  Fan: [], Promotor: [], Promoter: [], Agency: [], Songwriter: [], DJ: [], Publicist: [],
  TourManager: [], StageManager: [], RoadCrew: [], Photographer: [],
};

const fixtures = [
  { userType: 'visitor', authenticated: false, roles: [] },
  { userType: 'fan-client', authenticated: true, roles: ['Fan', 'Customer'] },
  { userType: 'artist', authenticated: true, roles: ['Artist', 'Fan', 'Customer'] },
  { userType: 'professor', authenticated: true, roles: ['Teacher', 'Fan', 'Customer'] },
  { userType: 'intern', authenticated: true, roles: ['Intern', 'Fan', 'Customer'] },
  { userType: 'reception', authenticated: true, roles: ['Reception', 'Fan', 'Customer'] },
  { userType: 'producer-ar', authenticated: true, roles: ['Producer', 'AandR', 'Fan', 'Customer'] },
  { userType: 'studio-manager', authenticated: true, roles: ['StudioManager', 'Fan', 'Customer'] },
  { userType: 'maintenance', authenticated: true, roles: ['Maintenance', 'Fan', 'Customer'] },
  { userType: 'webmaster', authenticated: true, roles: ['Webmaster', 'Fan', 'Customer'] },
  { userType: 'administrator', authenticated: true, roles: ['Admin', 'Fan', 'Customer'] },
].map((fixture) => ({
  ...fixture,
  modules: [...new Set(fixture.roles.flatMap((role) => roleModules[role] ?? []))].sort(),
}));

const quoteCsv = (value) => {
  const text = value === null || value === undefined ? '' : String(value);
  return /[",\n]/.test(text) ? `"${text.replaceAll('"', '""')}"` : text;
};
const csv = (headers, rows) => `${headers.map(quoteCsv).join(',')}\n${rows
  .map((row) => headers.map((header) => quoteCsv(row[header])).join(','))
  .join('\n')}\n`;
const normalize = (value) => value.toLowerCase().normalize('NFD')
  .replace(/[\u0300-\u036f]/g, '').replace(/[^a-z0-9]/g, '');
const normalizedSet = (values) => new Set(values.map(normalize));

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

function featureAccess(feature, fixture, action) {
  if (feature.technical || ['incomplete', 'broken'].includes(feature.maturity)) return 'concealed';
  if (feature.requiredAuth === 'authenticated' && !fixture.authenticated) {
    return feature.safeLockedDisclosure ? 'login-required' : 'concealed';
  }
  if (feature.featureFlag) return 'flag-required';
  const rule = feature.permissions[action];
  if (!rule) return 'not-supported';
  const roles = normalizedSet(fixture.roles);
  const modules = normalizedSet(fixture.modules);
  const rolesAny = (rule.rolesAny?.length ? rule.rolesAny : feature.requiredRoles ?? []).map(normalize);
  const rolesAll = (rule.rolesAll ?? []).map(normalize);
  const modulesAny = (rule.modulesAny ?? []).map(normalize);
  const modulesAll = [...(feature.requiredModules ?? []), ...(rule.modulesAll ?? [])].map(normalize);
  const strictAdminRoles = new Set(['admin', 'fan', 'customer']);
  const strictAdmin = !rule.strictAdmin
    || (roles.has('admin') && [...roles].every((role) => strictAdminRoles.has(role)));
  const allowed = strictAdmin
    && rolesAll.every((role) => roles.has(role))
    && (rolesAny.length === 0 || rolesAny.some((role) => roles.has(role)))
    && modulesAll.every((moduleName) => modules.has(moduleName))
    && (modulesAny.length === 0 || modulesAny.some((moduleName) => modules.has(moduleName)));
  if (allowed) return 'allowed';
  return feature.safeLockedDisclosure && feature.accessRequestEligible ? 'locked-requestable' : 'concealed';
}

function platformAvailability(feature, platform) {
  if (platform === 'web') return feature.webRoute ? 'available' : 'not-available';
  const kind = feature.mobilePresentation.kind;
  if (['security-concealed', 'technical'].includes(kind)) return 'not-available';
  if (kind === 'native' && feature.mobilePresentation.destination) return 'native';
  if ((kind === 'web-only' || kind === 'external-web')
    && (feature.mobilePresentation.destination || feature.webRoute)) return 'web-fallback';
  return 'not-available';
}

async function collectFiles(directory) {
  const files = [];
  for (const entry of await readdir(directory, { withFileTypes: true })) {
    const absolute = path.join(directory, entry.name);
    if (entry.isDirectory()) files.push(...await collectFiles(absolute));
    else files.push(absolute);
  }
  return files;
}

function stripComments(source) {
  return source.replace(/\{-[^]*?-\}/g, '').replace(/--.*$/gm, '');
}

function extractTypeAliases(source, relativeFile) {
  const aliases = [];
  const lines = stripComments(source).split('\n');
  let current = null;
  const flush = () => {
    if (!current) return;
    aliases.push({ ...current, expression: current.parts.join(' ').trim() });
    current = null;
  };
  for (const [index, line] of lines.entries()) {
    const match = line.match(/^type\s+([A-Z][A-Za-z0-9_']*)(?:\s+[^=]*)?\s*=\s*(.*)$/);
    if (match) {
      flush();
      current = { name: match[1], parts: [match[2]], file: relativeFile, line: index + 1 };
    } else if (current && (line.trim() === '' || /^\s/.test(line))) {
      current.parts.push(line.trim());
    } else if (current) {
      flush();
    }
  }
  flush();
  return aliases;
}

function splitTopLevel(expression, operator) {
  const result = [];
  let round = 0;
  let square = 0;
  let inString = false;
  let start = 0;
  for (let index = 0; index < expression.length; index += 1) {
    const character = expression[index];
    if (character === '"' && expression[index - 1] !== '\\') inString = !inString;
    if (inString) continue;
    if (character === '(') round += 1;
    else if (character === ')') round -= 1;
    else if (character === '[') square += 1;
    else if (character === ']') square -= 1;
    if (round === 0 && square === 0 && expression.startsWith(operator, index)) {
      result.push(expression.slice(start, index).trim());
      index += operator.length - 1;
      start = index + 1;
    }
  }
  result.push(expression.slice(start).trim());
  return result.filter(Boolean);
}

function unwrapParentheses(expression) {
  let value = expression.trim();
  while (value.startsWith('(') && value.endsWith(')')) {
    let depth = 0;
    let encloses = true;
    for (let index = 0; index < value.length; index += 1) {
      if (value[index] === '(') depth += 1;
      if (value[index] === ')') depth -= 1;
      if (depth === 0 && index < value.length - 1) { encloses = false; break; }
    }
    if (!encloses) break;
    value = value.slice(1, -1).trim();
  }
  return value;
}

function endpointMethod(component) {
  const value = unwrapParentheses(component);
  const simple = value.match(/^(Get|Post|Put|Patch|Delete)(?:NoContent|Created|Accepted)?\b/i)?.[1];
  if (simple) return simple.toUpperCase();
  const verb = value.match(/^(?:Verb|NoContentVerb)\s+'?([A-Z]+)/)?.[1];
  if (verb) return verb.toUpperCase();
  if (/^Raw\b/.test(value)) return 'RAW';
  return null;
}

function buildEndpointInventory(aliasMap) {
  const endpoints = [];
  const unresolved = [];
  function expand(expression, state, stack) {
    const value = unwrapParentheses(expression);
    const alternatives = splitTopLevel(value, ':<|>');
    if (alternatives.length > 1) {
      for (const alternative of alternatives) expand(alternative, state, stack);
      return;
    }
    const chain = splitTopLevel(value, ':>');
    let current = { ...state, path: [...state.path] };
    for (let index = 0; index < chain.length; index += 1) {
      const component = unwrapParentheses(chain[index]);
      const segment = component.match(/^"([^"]+)"$/)?.[1];
      const capture = component.match(/^Capture(?:All)?\s+"([^"]+)"/)?.[1];
      if (segment) { current.path.push(segment); continue; }
      if (capture) { current.path.push(`{${capture}}`); continue; }
      if (/^(AuthProtect|BasicAuth)\b/.test(component)) { current.authenticated = true; continue; }
      if (/^(QueryParam|QueryParams|Header|ReqBody|Summary|Description|RemoteHost|IsSecure|Vault|HttpVersion)\b/.test(component)) continue;
      const method = endpointMethod(component);
      if (method) {
        endpoints.push({
          method,
          path: `/${current.path.join('/')}`.replace(/\/$/, '') || '/',
          authenticated: current.authenticated,
          sourceAliases: stack.join(' > '),
          source: aliasMap.get(stack.at(-1))?.source ?? '',
        });
        return;
      }
      const aliasName = component.match(/^([A-Z][A-Za-z0-9_']*)\b/)?.[1];
      if (aliasName && aliasMap.has(aliasName)) {
        if (stack.includes(aliasName)) {
          unresolved.push({ component, reason: 'recursive alias', stack: stack.join(' > ') });
          return;
        }
        expand(aliasMap.get(aliasName).expression, current, [...stack, aliasName]);
        return;
      }
      if (splitTopLevel(component, ':<|>').length > 1 || splitTopLevel(component, ':>').length > 1) {
        expand(component, current, stack);
        return;
      }
      if (index === chain.length - 1) unresolved.push({ component, reason: 'unrecognized terminal', stack: stack.join(' > ') });
    }
  }
  expand(aliasMap.get('API').expression, { path: [], authenticated: false }, ['API']);
  return { endpoints, unresolved };
}

const rawRegistry = await readFile(registryPath, 'utf8');
const registry = JSON.parse(rawRegistry);
const features = registry.features.map((feature) => mergeFeature(registry.defaults ?? {}, feature));
await mkdir(reportPath, { recursive: true });

const inventoryRows = features.map((feature) => ({
  feature_id: feature.id,
  parent_feature: feature.parentId,
  classification: feature.classification,
  route_type: feature.routeType,
  web_route: feature.webRoute,
  mobile_kind: feature.mobilePresentation.kind,
  mobile_destination: feature.mobilePresentation.destination,
  mobile_exception: feature.mobilePresentation.documentedException,
  label_es: feature.label.es,
  label_en: feature.label.en,
  synonyms_es: feature.synonyms.es.join('|'),
  synonyms_en: feature.synonyms.en.join('|'),
  keywords: feature.keywords.join('|'),
  navigation_group: feature.navigationGroup,
  breadcrumb: feature.breadcrumb.join(' > '),
  authentication: feature.requiredAuth,
  roles: feature.requiredRoles.join('|'),
  modules: feature.requiredModules.join('|'),
  actions: Object.keys(feature.permissions).join('|'),
  route_action: feature.routeAction,
  record_scope: feature.recordScope,
  maturity: feature.maturity,
  feature_flag: feature.featureFlag,
  global_menu: feature.globalMenu,
  quick_create: feature.quickCreate?.action ?? '',
  favorite_eligible: feature.favoriteEligible,
  pin_eligible: feature.pinEligible,
  recent_behavior: feature.recentBehavior,
  telemetry_id: feature.telemetryId,
  access_request_eligible: feature.accessRequestEligible,
  interface_expected: feature.interfaceExpected,
  technical: feature.technical,
  searchable: feature.searchable,
}));
await writeFile(path.join(reportPath, 'feature-inventory.csv'), csv(Object.keys(inventoryRows[0]), inventoryRows));

const routeRows = features.map((feature) => ({
  feature_id: feature.id,
  classification: feature.classification,
  web_route: feature.webRoute,
  web_aliases: feature.aliases.join('|'),
  mobile_kind: feature.mobilePresentation.kind,
  mobile_destination: feature.mobilePresentation.destination,
  mobile_aliases: feature.mobileAliases.join('|'),
  parent_feature: feature.parentId,
  breadcrumb: feature.breadcrumb.join(' > '),
  global_discovery: feature.globalMenu,
  discovery_mechanism: feature.technical
    ? 'never-displayed'
    : feature.globalMenu
      ? 'global-menu'
      : feature.searchable
        ? 'command-palette-or-mobile-explorer'
        : feature.classification === 'record-detail-contextual-action'
          ? 'parent-record-context'
          : 'module-landing-context',
  maximum_interactions: feature.technical ? '' : feature.globalMenu ? 1 : 2,
  contextual_only: feature.classification === 'record-detail-contextual-action',
  technical_hidden: feature.technical,
}));
await writeFile(path.join(reportPath, 'route-and-mobile-mapping.csv'), csv(Object.keys(routeRows[0]), routeRows));

const matrixRows = [];
let authorizedPrimaryDiscoveryAssertions = 0;
for (const fixture of fixtures) {
  for (const feature of features) {
    for (const platform of ['web', 'mobile']) {
      const availability = platformAvailability(feature, platform);
      const row = {
        user_type: fixture.userType,
        authenticated: fixture.authenticated,
        effective_roles: fixture.roles.join('|'),
        effective_modules: fixture.modules.join('|'),
        feature_id: feature.id,
        platform,
        platform_treatment: availability,
      };
      for (const action of actions) {
        row[action] = availability === 'not-available' ? 'not-available' : featureAccess(feature, fixture, action);
      }
      matrixRows.push(row);
      if (feature.classification === 'primary-destination'
        && availability !== 'not-available'
        && featureAccess(feature, fixture, 'discover') === 'allowed'
        && featureAccess(feature, fixture, 'view') === 'allowed') {
        authorizedPrimaryDiscoveryAssertions += 1;
        if (!feature.globalMenu && !feature.searchable) {
          throw new Error(`${fixture.userType}/${platform}: authorized primary feature ${feature.id} has no <=2 interaction discovery entry`);
        }
      }
    }
  }
}
await writeFile(path.join(reportPath, 'role-module-feature-action-platform-matrix.csv'), csv(Object.keys(matrixRows[0]), matrixRows));

const haskellFiles = (await collectFiles(backendSourcePath)).filter((file) => file.endsWith('.hs'));
const aliases = [];
for (const file of haskellFiles) {
  const relative = path.relative(workspaceRoot, file);
  aliases.push(...extractTypeAliases(await readFile(file, 'utf8'), relative));
}
const aliasMap = new Map();
for (const alias of aliases) {
  if (aliasMap.has(alias.name)) {
    const existing = aliasMap.get(alias.name);
    if (alias.name.endsWith('API') && existing.expression !== alias.expression) {
      throw new Error(`Ambiguous backend API type alias ${alias.name}`);
    }
    continue;
  }
  aliasMap.set(alias.name, { ...alias, source: `${alias.file}:${alias.line}` });
}
if (!aliasMap.has('API')) throw new Error('Backend root API type alias was not found');
const { endpoints, unresolved } = buildEndpointInventory(aliasMap);
if (unresolved.length > 0) {
  const details = unresolved.slice(0, 20).map((item) => JSON.stringify(item)).join('\n');
  throw new Error(`Backend endpoint parser left ${unresolved.length} terminal(s) unresolved:\n${details}`);
}
const endpointKeys = new Set();
const uniqueEndpoints = endpoints.filter((endpoint) => {
  const key = `${endpoint.method} ${endpoint.path}`;
  if (endpointKeys.has(key)) return false;
  endpointKeys.add(key);
  return true;
}).sort((left, right) => left.path.localeCompare(right.path) || left.method.localeCompare(right.method));

const scopes = features.flatMap((feature) => (feature.apiScopes ?? []).map((scope) => ({ feature, scope })));
function matchScope(endpoint) {
  return scopes.filter(({ scope }) => endpoint.path.startsWith(scope.pathPrefix)
    && (!scope.pathExact || endpoint.path === scope.pathExact)
    && (!scope.pathContains || endpoint.path.includes(scope.pathContains))
    && Boolean(scope.methods?.[endpoint.method]))
    .sort((left, right) => {
      const leftSpecificity = left.scope.pathPrefix.length + (left.scope.pathExact ? 1000 : 0) + (left.scope.pathContains ? 100 : 0);
      const rightSpecificity = right.scope.pathPrefix.length + (right.scope.pathExact ? 1000 : 0) + (right.scope.pathContains ? 100 : 0);
      return rightSpecificity - leftSpecificity;
    })[0] ?? null;
}
const technicalPattern = /\/(?:health|version|login|password|session|oauth|webhooks?|callback|seed|mcp)(?:\/|$)/;
const technicalApiOnlyPattern = /^(?:\/hooks\/whatsapp|\/assets\/serve|\/inventory)$/;
const endpointRows = uniqueEndpoints.map((endpoint) => {
  const matched = matchScope(endpoint);
  const action = matched?.scope.methods?.[endpoint.method] ?? '';
  const feature = matched?.feature;
  let disposition = 'API-only pending explicit product/security disposition';
  if (feature?.interfaceExpected && (feature.webRoute || ['native', 'native-contextual'].includes(feature.mobilePresentation.kind))) disposition = 'user/admin interface mapped';
  else if (feature && !feature.interfaceExpected) disposition = 'documented API-only capability';
  else if (feature?.mobilePresentation.kind === 'security-concealed') disposition = 'security-sensitive concealed capability; interface decision documented';
  else if (!endpoint.authenticated && technicalPattern.test(endpoint.path)) disposition = 'technical/API-only; never display';
  else if (!endpoint.authenticated && technicalApiOnlyPattern.test(endpoint.path)) disposition = 'technical/static/API-only; never display';
  return {
    method: endpoint.method,
    path: endpoint.path,
    authentication_boundary: endpoint.authenticated ? 'authenticated-root' : 'public-root',
    mapped_feature_id: feature?.id ?? '',
    mapped_action: action,
    action_rule_present: action ? Boolean(feature?.permissions?.[action]) : false,
    interface_disposition: disposition,
    authorization_evidence: endpoint.authenticated
      ? 'root bearer/session authentication; handler/action/record-scope review still required'
      : 'public route; handler validation/signature/rate-limit review still required',
    source_aliases: endpoint.sourceAliases,
    source: endpoint.source,
  };
});
await writeFile(path.join(reportPath, 'backend-capability-inventory.csv'), csv(Object.keys(endpointRows[0]), endpointRows));

const summary = {
  generatedAt: '2026-08-06',
  registryVersion: registry.registryVersion,
  featureCount: features.length,
  primaryDestinations: features.filter((feature) => feature.classification === 'primary-destination').length,
  importantSubmodules: features.filter((feature) => feature.classification === 'important-submodule').length,
  contextualRoutes: features.filter((feature) => feature.classification === 'record-detail-contextual-action').length,
  technicalRoutes: features.filter((feature) => feature.classification === 'technical-route').length,
  webRoutes: features.filter((feature) => feature.webRoute).length,
  nativeMobileFeatures: features.filter((feature) => feature.mobilePresentation.kind === 'native').length,
  roleFixtures: fixtures.length,
  matrixRows: matrixRows.length,
  authorizedPrimaryDiscoveryAssertions,
  backendEndpoints: endpointRows.length,
  backendEndpointsMappedToFeatures: endpointRows.filter((endpoint) => endpoint.mapped_feature_id).length,
  backendEndpointsPendingDisposition: endpointRows.filter((endpoint) => endpoint.interface_disposition.includes('pending')).length,
  backendEndpointDispositions: Object.fromEntries(
    [...new Set(endpointRows.map((endpoint) => endpoint.interface_disposition))]
      .sort()
      .map((disposition) => [
        disposition,
        endpointRows.filter((endpoint) => endpoint.interface_disposition === disposition).length,
      ]),
  ),
};
if (summary.backendEndpointsPendingDisposition > 0) {
  throw new Error(`${summary.backendEndpointsPendingDisposition} backend endpoint(s) still lack an explicit disposition`);
}
await writeFile(path.join(reportPath, 'generated-summary.json'), `${JSON.stringify(summary, null, 2)}\n`);
console.log(JSON.stringify(summary, null, 2));
