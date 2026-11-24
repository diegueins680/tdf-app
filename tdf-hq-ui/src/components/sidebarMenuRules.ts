import { normalizeAccessRoles } from '../utils/accessControl';

// Menu rules from the cognitive-load audit (decisions D11–D14, D17 and the
// studio-pages rule). They decide what the sidebar lists by default for each
// kind of viewer. They never decide what a viewer may open: every destination
// keeps its route and its access check, stays findable through the menu
// search, and stays listed while it is the open page.

export type MenuFamilyId = 'contacts' | 'system' | 'accounts' | 'calendar';

interface MenuItemLike {
  label: string;
  path: string;
  searchOnly?: boolean;
}

interface MenuGroupLike<Item extends MenuItemLike> {
  title: string;
  items: Item[];
}

/** Pages that are views of one job share one menu entry and a tab strip. */
export function menuFamilyOf(path: string): MenuFamilyId | null {
  switch (path) {
    case '/crm/contactos':
    case '/crm/empresas':
    case '/crm/leads':
      return 'contacts';
    case '/configuracion/actividad':
    case '/configuracion/logs':
    case '/configuracion/estado':
    case '/admin/diagnosticos':
    case '/admin/commerce/provider-events':
      return 'system';
    case '/configuracion/usuarios-admin':
    case '/configuracion/roles-permisos':
      return 'accounts';
    case '/estudio/calendario':
    case '/estudio/ordenes':
      return 'calendar';
    default:
      return null;
  }
}

export function menuFamilyLabel(family: MenuFamilyId): string {
  switch (family) {
    case 'contacts':
      return 'Contactos';
    case 'system':
      return 'Sistema';
    case 'accounts':
      return 'Usuarios y permisos';
    case 'calendar':
      return 'Calendario';
  }
}

/** Tab wording inside a family, where the family name already gives the context. */
export function menuTabLabel(item: MenuItemLike): string {
  switch (item.path) {
    case '/estudio/ordenes':
      return 'Lista de sesiones';
    case '/configuracion/estado':
      return 'Estado';
    case '/configuracion/usuarios-admin':
      return 'Usuarios';
    default:
      return item.label;
  }
}

const STUDIO_TEAM_ROLE = /^(admin|manager|studiomanager|reception|accounting|livesessionsproducer|webmaster)$/;
const ENGINEER_SESSION_PAGE = /^\/estudio\/(ordenes|salas|live-sessions)$/;
const AVATAR_MENU_PAGE = /^\/(manual|docs|acerca|seguridad)$/;

/**
 * Whether the sidebar lists a destination by default for a viewer with these roles.
 * - `/buscar` is the same page as Inicio.
 * - Manual, documentation, about and security live in the avatar menu.
 * - Internal studio pages are listed for the studio team; engineers keep the
 *   session pages; everyone else with the scheduling module keeps the calendar.
 */
export function listedInMenuFor(path: string, roles: readonly string[] | undefined): boolean {
  if (path === '/buscar') return false;
  if (AVATAR_MENU_PAGE.test(path)) return false;
  if (path.startsWith('/estudio/') && path !== '/estudio/calendario') {
    const normalized = normalizeAccessRoles(roles);
    if (normalized.some((role) => STUDIO_TEAM_ROLE.test(role))) return true;
    return normalized.includes('engineer') && ENGINEER_SESSION_PAGE.test(path);
  }
  return true;
}

const isOpen = (activePath: string, path: string) => activePath === path || activePath.startsWith(`${path}/`);

/** The members of a family this viewer has listed, in menu order. */
export function menuFamilyMembers<Item extends MenuItemLike>(
  groups: readonly MenuGroupLike<Item>[],
  family: MenuFamilyId,
  roles: readonly string[] | undefined,
): Item[] {
  return groups.flatMap((group) => group.items.filter((item) => (
    menuFamilyOf(item.path) === family && !item.searchOnly && listedInMenuFor(item.path, roles)
  )));
}

/**
 * The default (no search) menu: one entry per family, role rules applied, and
 * an unlisted destination shown only while it is open.
 * @precondition `groups` holds only destinations the viewer may open.
 */
export function defaultMenuGroups<Item extends MenuItemLike>(
  groups: readonly MenuGroupLike<Item>[],
  roles: readonly string[] | undefined,
  activePath: string,
): MenuGroupLike<Item & { familyPaths?: string[] }>[] {
  const leads = new Map<MenuFamilyId, { path: string; paths: string[] }>();
  const folded = new Set<string>();
  for (const family of new Set(groups.flatMap((group) => group.items.map((item) => menuFamilyOf(item.path))))) {
    if (!family) continue;
    const members = menuFamilyMembers(groups, family, roles);
    const lead = members[0];
    if (!lead || members.length < 2) continue;
    leads.set(family, { path: lead.path, paths: members.map((member) => member.path) });
    members.slice(1).forEach((member) => folded.add(member.path));
  }
  return groups
    .map((group) => ({
      ...group,
      items: group.items.flatMap((item): (Item & { familyPaths?: string[] })[] => {
        if (folded.has(item.path)) return [];
        const family = menuFamilyOf(item.path);
        const lead = family ? leads.get(family) : undefined;
        if (family && lead?.path === item.path) {
          return [{ ...item, label: menuFamilyLabel(family), familyPaths: lead.paths }];
        }
        const listed = !item.searchOnly && listedInMenuFor(item.path, roles);
        return listed || isOpen(activePath, item.path) ? [item] : [];
      }),
    }))
    .filter((group) => group.items.length > 0);
}
