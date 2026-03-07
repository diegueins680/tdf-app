const LOGIN_ROUTE = '/login';
const URL_BASE = 'https://tdf.local';

const normalizeTokens = (values: readonly string[] | undefined, { lowerCase = false } = {}): string[] => {
  if (!values || values.length === 0) return [];
  const seen = new Set<string>();
  const normalized: string[] = [];

  values.forEach((value) => {
    const trimmed = value.trim();
    if (trimmed === '') return;
    const normalizedValue = lowerCase ? trimmed.toLowerCase() : trimmed;
    const key = normalizedValue.toLowerCase();
    if (seen.has(key)) return;
    seen.add(key);
    normalized.push(normalizedValue);
  });

  return normalized;
};

const deriveModulesFromRoles = (roles: readonly string[]): string[] => {
  const moduleSet = new Set<string>();

  roles.forEach((role) => {
    if (role.includes('admin')) {
      moduleSet.add('admin');
      moduleSet.add('crm');
      moduleSet.add('scheduling');
      moduleSet.add('school');
      moduleSet.add('invoicing');
      moduleSet.add('packages');
      moduleSet.add('ops');
      moduleSet.add('label');
      moduleSet.add('internships');
      return;
    }

    if (role.includes('manager')) {
      moduleSet.add('crm');
      moduleSet.add('scheduling');
      moduleSet.add('school');
      moduleSet.add('invoicing');
      moduleSet.add('packages');
      moduleSet.add('ops');
      moduleSet.add('internships');
      return;
    }

    if (role.includes('reception')) {
      moduleSet.add('crm');
      moduleSet.add('scheduling');
      moduleSet.add('school');
      return;
    }

    if (role.includes('accounting')) {
      moduleSet.add('invoicing');
      return;
    }

    if (role.includes('engineer') || role.includes('scheduling')) {
      moduleSet.add('scheduling');
      return;
    }

    if (role.includes('teacher')) {
      moduleSet.add('scheduling');
      moduleSet.add('school');
      return;
    }

    if (role.includes('intern')) {
      moduleSet.add('internships');
      return;
    }

    if (role.includes('packages') || role.includes('package')) {
      moduleSet.add('packages');
      return;
    }

    if (role.includes('maintenance')) {
      moduleSet.add('packages');
      moduleSet.add('scheduling');
      moduleSet.add('ops');
      return;
    }

    if (role.includes('label')) {
      moduleSet.add('label');
      return;
    }

    if (
      role.includes('inventory')
      || role.includes('operacion')
      || role.includes('operation')
      || role.includes('ops')
    ) {
      moduleSet.add('ops');
      return;
    }

    if (role.includes('finance') || role.includes('billing')) {
      moduleSet.add('invoicing');
    }
  });

  return Array.from(moduleSet);
};

const buildModuleSet = (roles: readonly string[], modules: readonly string[] | undefined): Set<string> => {
  const normalizedModules = normalizeTokens(modules, { lowerCase: true });
  const derivedModules = deriveModulesFromRoles(roles);
  const moduleSet = new Set([...normalizedModules, ...derivedModules]);

  // Keep the login landing logic aligned with the nav access aliases.
  if (moduleSet.has('packages')) {
    moduleSet.add('ops');
    moduleSet.add('label');
  }

  return moduleSet;
};

export function pickLandingPath(roles: readonly string[], modules?: readonly string[]): string {
  const normalizedRoles = normalizeTokens(roles, { lowerCase: true });
  const moduleSet = buildModuleSet(normalizedRoles, modules);
  const hasRole = (...needles: string[]) =>
    needles.some((needle) => normalizedRoles.some((role) => role.includes(needle)));
  const hasModule = (needle: string) => moduleSet.has(needle);

  if (hasRole('admin') || hasModule('admin')) return '/configuracion/roles-permisos';
  if (hasRole('artist', 'artista')) return '/mi-artista';
  if (hasRole('teacher')) return '/mi-profesor';
  if (hasModule('scheduling')) return '/estudio/calendario';
  if (hasModule('crm')) return '/crm/contactos';
  if (hasModule('school')) return '/escuela/clases';
  if (hasModule('label')) return '/label/artistas';
  if (hasModule('ops')) return '/operacion/inventario';
  if (hasModule('internships')) return '/practicas';
  if (hasModule('invoicing')) return '/finanzas/pagos';
  if (hasRole('fan', 'customer')) return '/fans';
  return '/inicio';
}

export function sanitizeRedirectPath(value: string | null | undefined): string | null {
  if (typeof value !== 'string') return null;
  const trimmed = value.trim();
  if (trimmed === '' || !trimmed.startsWith('/') || trimmed.startsWith('//')) return null;

  try {
    const parsed = new URL(trimmed, URL_BASE);
    if (parsed.origin !== URL_BASE) return null;
    if (parsed.pathname === LOGIN_ROUTE) return null;
    return `${parsed.pathname}${parsed.search}${parsed.hash}`;
  } catch {
    return null;
  }
}

export function readSafeRedirectPath(search: string): string | null {
  const params = new URLSearchParams(search);
  return sanitizeRedirectPath(params.get('redirect'));
}

export function buildLoginRedirectPath(targetPath: string | null | undefined): string {
  const safeTarget = sanitizeRedirectPath(targetPath);
  if (!safeTarget) return LOGIN_ROUTE;

  const params = new URLSearchParams({ redirect: safeTarget });
  return `${LOGIN_ROUTE}?${params.toString()}`;
}
