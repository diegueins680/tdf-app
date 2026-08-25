import rawRegistry from '../../../tdf-hq/assets/feature-registry.json';

export type FeatureAction =
  | 'discover'
  | 'view'
  | 'create'
  | 'edit'
  | 'delete'
  | 'archive'
  | 'deactivate'
  | 'import'
  | 'export'
  | 'submit'
  | 'validate'
  | 'approve'
  | 'reject'
  | 'assign'
  | 'publish'
  | 'report'
  | 'administer';

export type FeatureAccessState = 'allowed' | 'locked' | 'concealed';
export type FeatureLocale = 'es' | 'en';

export interface AccessRule {
  rolesAny?: string[];
  rolesAll?: string[];
  modulesAny?: string[];
  modulesAll?: string[];
  strictAdmin?: boolean;
  recordScope?: string;
}

export interface LocalizedText {
  es: string;
  en: string;
}

export interface MobilePresentation {
  kind: string;
  destination: string | null;
  documentedException: string | null;
}

export interface QuickCreateMetadata {
  action: FeatureAction;
  label: LocalizedText;
  webDestination: string;
  mobileDestination: string | null;
}

export interface FeatureDefinition {
  id: string;
  parentId: string | null;
  webRoute: string | null;
  routeType: string;
  classification: string;
  label: LocalizedText;
  synonyms: { es: string[]; en: string[] };
  keywords: string[];
  description: LocalizedText;
  navigationGroup: string | null;
  icon: string;
  breadcrumb: string[];
  requiredAuth: 'public' | 'optional' | 'authenticated';
  routeAction: FeatureAction;
  requiredRoles: string[];
  requiredModules: string[];
  permissions: Partial<Record<FeatureAction, AccessRule>>;
  recordScope: string;
  featureFlag: string | null;
  maturity: string;
  globalMenu: boolean;
  quickCreate: QuickCreateMetadata | null;
  favoriteEligible: boolean;
  pinEligible: boolean;
  recentBehavior: string;
  mobilePresentation: MobilePresentation;
  mobileAliases: string[];
  telemetryId: string;
  accessRequestEligible: boolean;
  interfaceExpected: boolean;
  technical: boolean;
  searchable: boolean;
  safeLockedDisclosure: boolean;
  aliases: string[];
  apiScopes: unknown[];
}

export interface FeatureGroup {
  id: string;
  labelEs: string;
  labelEn: string;
  icon: string;
  order: number;
}

interface RawFeatureRegistry {
  schemaVersion: number;
  registryVersion: string;
  defaults: Partial<FeatureDefinition>;
  groups: FeatureGroup[];
  features: (Partial<FeatureDefinition> & Pick<FeatureDefinition, 'id' | 'label'>)[];
}

export interface FeatureSession {
  authenticated: boolean;
  roles?: readonly string[];
  modules?: readonly string[];
  featureFlags?: readonly string[];
}

export interface FeatureAccessDecision {
  state: FeatureAccessState;
  feature: FeatureDefinition;
  missingRoles: string[];
  missingModules: string[];
  reason: 'allowed' | 'authentication' | 'role' | 'module' | 'feature-flag' | 'concealed';
}

const registrySource = rawRegistry as RawFeatureRegistry;

const requireNonEmptyText = (value: unknown, field: string): string => {
  if (typeof value !== 'string' || value.trim() === '') {
    throw new Error(`Feature registry field ${field} must be a non-empty string.`);
  }
  return value;
};

const registryIds = new Set<string>();

export const featureRegistry: readonly FeatureDefinition[] = registrySource.features.map((entry) => {
  const id = requireNonEmptyText(entry.id, 'id');
  if (registryIds.has(id)) throw new Error(`Duplicate feature registry id: ${id}`);
  registryIds.add(id);

  const merged = {
    ...registrySource.defaults,
    ...entry,
    description: entry.description ?? registrySource.defaults.description,
    permissions: entry.permissions ?? registrySource.defaults.permissions,
    mobilePresentation: entry.mobilePresentation ?? registrySource.defaults.mobilePresentation,
    mobileAliases: entry.mobileAliases ?? registrySource.defaults.mobileAliases,
    telemetryId: entry.telemetryId ?? entry.id,
  } as FeatureDefinition;

  requireNonEmptyText(merged.label?.es, `${id}.label.es`);
  requireNonEmptyText(merged.label?.en, `${id}.label.en`);
  if (merged.technical && (merged.globalMenu || merged.searchable)) {
    throw new Error(`Technical feature ${id} cannot be navigable or searchable.`);
  }
  return merged;
});

export const featureGroups: readonly FeatureGroup[] = [...registrySource.groups].sort(
  (left, right) => left.order - right.order,
);

const featureById = new Map(featureRegistry.map((feature) => [feature.id, feature]));

for (const feature of featureRegistry) {
  if (feature.parentId && !featureById.has(feature.parentId)) {
    throw new Error(`Feature ${feature.id} has unknown parent ${feature.parentId}.`);
  }
  for (const breadcrumbId of feature.breadcrumb) {
    if (!featureById.has(breadcrumbId)) {
      throw new Error(`Feature ${feature.id} has unknown breadcrumb ${breadcrumbId}.`);
    }
  }
}

const ROLE_ALIASES: Record<string, string> = {
  'a&r': 'aandr',
  'a and r': 'aandr',
  ar: 'aandr',
  'live sessions producer': 'livesessionsproducer',
  'live-sessions-producer': 'livesessionsproducer',
  'studio manager': 'studiomanager',
  'studio-manager': 'studiomanager',
};

export function normalizeFeatureToken(value: string): string {
  const normalized = value.trim().toLocaleLowerCase('en').normalize('NFD').replace(/[\u0300-\u036f]/g, '');
  return ROLE_ALIASES[normalized] ?? normalized.replace(/[^a-z0-9]/g, '');
}

const normalizedSet = (values: readonly string[] | undefined): Set<string> =>
  new Set((values ?? []).map(normalizeFeatureToken).filter(Boolean));

function matchesRule(
  rule: AccessRule,
  roles: ReadonlySet<string>,
  modules: ReadonlySet<string>,
): { allowed: boolean; missingRoles: string[]; missingModules: string[] } {
  const rolesAny = (rule.rolesAny ?? []).map(normalizeFeatureToken);
  const rolesAll = (rule.rolesAll ?? []).map(normalizeFeatureToken);
  const modulesAny = (rule.modulesAny ?? []).map(normalizeFeatureToken);
  const modulesAll = (rule.modulesAll ?? []).map(normalizeFeatureToken);
  const strictAdminRoles = new Set(['admin', 'fan', 'customer']);
  const strictAdminSatisfied = !rule.strictAdmin
    || (roles.has('admin') && Array.from(roles).every((role) => strictAdminRoles.has(role)));

  const missingRoles = Array.from(new Set([
    ...(!strictAdminSatisfied ? ['strict-admin'] : []),
    ...rolesAll.filter((role) => !roles.has(role)),
    ...(rolesAny.length > 0 && !rolesAny.some((role) => roles.has(role)) ? rolesAny : []),
  ]));
  const missingModules = Array.from(new Set([
    ...modulesAll.filter((moduleName) => !modules.has(moduleName)),
    ...(modulesAny.length > 0 && !modulesAny.some((moduleName) => modules.has(moduleName))
      ? modulesAny
      : []),
  ]));

  return {
    allowed: missingRoles.length === 0 && missingModules.length === 0,
    missingRoles,
    missingModules,
  };
}

export function getFeatureById(id: string): FeatureDefinition | null {
  return featureById.get(id) ?? null;
}

const escapeRegex = (value: string) => value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');

function routePatternToRegex(pattern: string): RegExp {
  if (pattern === '/') return /^\/$/;
  const segments = pattern.split('/').filter(Boolean);
  const body = segments.map((segment) => {
    if (segment === '*') return '(?:/.*)?';
    if (!segment.startsWith(':')) return `/${escapeRegex(segment)}`;
    return segment.endsWith('?') ? '(?:/[^/]+)?' : '/[^/]+';
  }).join('');
  return new RegExp(`^${body}/?$`);
}

const routeCandidates = featureRegistry.flatMap((feature) =>
  [feature.webRoute, ...feature.aliases]
    .filter((route): route is string => typeof route === 'string' && route.startsWith('/'))
    .map((route) => ({ feature, route, matcher: routePatternToRegex(route) })),
).sort((left, right) => {
  const leftDynamic = (left.route.match(/:/g) ?? []).length;
  const rightDynamic = (right.route.match(/:/g) ?? []).length;
  return leftDynamic - rightDynamic || right.route.length - left.route.length;
});

export function getFeatureByPath(pathname: string): FeatureDefinition | null {
  const pathnameOnly = pathname.split(/[?#]/, 1)[0] || '/';
  return routeCandidates.find(({ matcher }) => matcher.test(pathnameOnly))?.feature ?? null;
}

export function evaluateFeatureAccess(
  featureOrId: FeatureDefinition | string,
  session: FeatureSession,
  action: FeatureAction = 'view',
): FeatureAccessDecision {
  const resolvedFeature = typeof featureOrId === 'string' ? getFeatureById(featureOrId) : featureOrId;
  if (!resolvedFeature) throw new Error(`Unknown feature: ${featureOrId as string}`);

  if (resolvedFeature.technical || resolvedFeature.maturity === 'incomplete' || resolvedFeature.maturity === 'broken') {
    return { state: 'concealed', feature: resolvedFeature, missingRoles: [], missingModules: [], reason: 'concealed' };
  }
  if (resolvedFeature.requiredAuth === 'authenticated' && !session.authenticated) {
    return {
      state: resolvedFeature.safeLockedDisclosure ? 'locked' : 'concealed',
      feature: resolvedFeature,
      missingRoles: [],
      missingModules: [],
      reason: 'authentication',
    };
  }
  if (resolvedFeature.featureFlag && !normalizedSet(session.featureFlags).has(normalizeFeatureToken(resolvedFeature.featureFlag))) {
    return { state: 'concealed', feature: resolvedFeature, missingRoles: [], missingModules: [], reason: 'feature-flag' };
  }

  const roles = normalizedSet(session.roles);
  const modules = normalizedSet(session.modules);
  const actionRule = resolvedFeature.permissions[action];
  if (!actionRule) {
    return { state: 'concealed', feature: resolvedFeature, missingRoles: [], missingModules: [], reason: 'concealed' };
  }
  const baseRule: AccessRule = {
    ...(resolvedFeature.requiredRoles.length > 0 ? { rolesAny: resolvedFeature.requiredRoles } : {}),
    ...(resolvedFeature.requiredModules.length > 0 ? { modulesAll: resolvedFeature.requiredModules } : {}),
  };
  const result = matchesRule(
    {
      rolesAny: actionRule.rolesAny ?? baseRule.rolesAny,
      rolesAll: [...(baseRule.rolesAll ?? []), ...(actionRule.rolesAll ?? [])],
      modulesAny: actionRule.modulesAny ?? baseRule.modulesAny,
      modulesAll: [...(baseRule.modulesAll ?? []), ...(actionRule.modulesAll ?? [])],
      strictAdmin: actionRule.strictAdmin,
    },
    roles,
    modules,
  );

  if (result.allowed) {
    return { state: 'allowed', feature: resolvedFeature, missingRoles: [], missingModules: [], reason: 'allowed' };
  }
  const state = resolvedFeature.safeLockedDisclosure && resolvedFeature.accessRequestEligible ? 'locked' : 'concealed';
  return {
    state,
    feature: resolvedFeature,
    missingRoles: result.missingRoles,
    missingModules: result.missingModules,
    reason: result.missingModules.length > 0 ? 'module' : 'role',
  };
}

export function evaluatePathAccess(
  pathname: string,
  session: FeatureSession,
  action?: FeatureAction,
): FeatureAccessDecision | null {
  const pathFeature = getFeatureByPath(pathname);
  return pathFeature ? evaluateFeatureAccess(pathFeature, session, action ?? pathFeature.routeAction) : null;
}

export function featureLabel(feature: FeatureDefinition, locale: string | null | undefined): string {
  return locale?.toLowerCase().startsWith('en') ? feature.label.en : feature.label.es;
}

export function featureSearchText(feature: FeatureDefinition): string {
  return normalizeFeatureToken([
    feature.label.es,
    feature.label.en,
    ...feature.synonyms.es,
    ...feature.synonyms.en,
    ...feature.keywords,
    feature.webRoute ?? '',
  ].join(' '));
}

export function searchFeatures(query: string, features: readonly FeatureDefinition[] = featureRegistry): FeatureDefinition[] {
  const terms = normalizeFeatureToken(query).split(/\s+/).filter(Boolean);
  if (terms.length === 0) return [...features];
  return features.filter((feature) => {
    if (!feature.searchable || feature.technical) return false;
    const haystack = featureSearchText(feature);
    return terms.every((term) => haystack.includes(term));
  });
}

export function featureBreadcrumbs(feature: FeatureDefinition): FeatureDefinition[] {
  return [...feature.breadcrumb, feature.id]
    .map((id) => getFeatureById(id))
    .filter((entry): entry is FeatureDefinition => entry !== null);
}

export function accessRequestPath(feature: FeatureDefinition, action: FeatureAction = 'view'): string {
  const params = new URLSearchParams({ feature: feature.id, action });
  return `/solicitudes-acceso/nueva?${params.toString()}`;
}

export const featureRegistryMetadata = {
  schemaVersion: registrySource.schemaVersion,
  registryVersion: registrySource.registryVersion,
} as const;
