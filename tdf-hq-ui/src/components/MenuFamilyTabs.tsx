import { Tab, Tabs } from '@mui/material';
import { Link as RouterLink, useLocation } from 'react-router-dom';
import { useSession } from '../session/SessionContext';
import { canAccessPath } from '../utils/accessControl';
import { NAV_GROUPS } from './SidebarNav';
import { menuFamilyLabel, menuFamilyMembers, menuFamilyOf, menuTabLabel } from './sidebarMenuRules';

const opens = (activePath: string, path: string) => activePath === path || activePath.startsWith(`${path}/`);

/**
 * Pages that share one sidebar entry (contacts, system, accounts, calendar)
 * switch between each other here. Only the members the viewer may open are
 * offered, and nothing renders when there is a single one.
 */
export default function MenuFamilyTabs() {
  const { pathname } = useLocation();
  const { session } = useSession();
  if (!session) return null;
  const current = NAV_GROUPS.flatMap((group) => group.items).find((item) => menuFamilyOf(item.path) && opens(pathname, item.path));
  const family = current ? menuFamilyOf(current.path) : null;
  if (!current || !family) return null;
  const allowed = NAV_GROUPS.map((group) => ({
    ...group,
    items: group.items.filter((item) => canAccessPath(item.path, session.roles, session.modules, session.featureFlags)),
  }));
  const members = menuFamilyMembers(allowed, family, session.roles);
  if (members.length < 2 || !members.some((member) => member.path === current.path)) return null;
  return (
    <Tabs
      value={current.path}
      aria-label={menuFamilyLabel(family)}
      variant="scrollable"
      allowScrollButtonsMobile
      sx={{ mb: 2, borderBottom: 1, borderColor: 'divider' }}
    >
      {members.map((member) => (
        <Tab
          key={member.path}
          value={member.path}
          label={menuTabLabel(member)}
          component={RouterLink}
          to={member.path}
          sx={{ textTransform: 'none', minHeight: 44 }}
        />
      ))}
    </Tabs>
  );
}
