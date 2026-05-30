import { useEffect, useMemo, useRef, useState, useCallback } from 'react';
import {
  Badge,
  Box,
  Button,
  Collapse,
  IconButton,
  List,
  ListItemButton,
  ListItemText,
  Stack,
  Typography,
  TextField,
  InputAdornment,
} from '@mui/material';
import ExpandLessIcon from '@mui/icons-material/ExpandLess';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import FiberManualRecordIcon from '@mui/icons-material/FiberManualRecord';
import SearchIcon from '@mui/icons-material/Search';
import ClearIcon from '@mui/icons-material/Clear';
import AutoAwesomeOutlinedIcon from '@mui/icons-material/AutoAwesomeOutlined';
import PeopleAltOutlinedIcon from '@mui/icons-material/PeopleAltOutlined';
import AlbumOutlinedIcon from '@mui/icons-material/AlbumOutlined';
import Inventory2OutlinedIcon from '@mui/icons-material/Inventory2Outlined';
import AdminPanelSettingsOutlinedIcon from '@mui/icons-material/AdminPanelSettingsOutlined';
import BuildOutlinedIcon from '@mui/icons-material/BuildOutlined';
import HelpOutlineOutlinedIcon from '@mui/icons-material/HelpOutlineOutlined';
import type { KeyboardEvent as ReactKeyboardEvent, ReactNode } from 'react';
import { Link as RouterLink, useLocation } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import { useSession } from '../session/SessionContext';
import { useChatUnreadCount } from '../hooks/useChatUnreadCount';
import {
  canAccessPath,
  pathRequiresModule,
} from '../utils/accessControl';
import { COURSE_REGISTRATIONS_NAV_LABEL } from '../utils/navigationLabels';

export interface NavItem {
  label: string;
  path: string;
}

export interface NavGroup {
  title: string;
  items: NavItem[];
  icon?: ReactNode;
}

type NavGroupView = NavGroup & { restricted?: boolean; requiredModule?: string | null };
type NavShortcutItem = NavItem & { group: string };
type PositivePixelDimension = number;

interface SidebarNavIconLayoutContract {
  readonly baseGridUnitPx: PositivePixelDimension;
  readonly groupHeaderIconGridUnits: number;
  readonly groupHeaderIconSizePx: PositivePixelDimension;
}

const QUICK_RECENTS_KEY = 'tdf-quick-nav-recents';
const MAX_SHORTCUT_RECENTS = 6;

// Invariant: top-level group icons stay compact enough to align with caption
// headers without increasing the sidebar row height.
export const SIDEBAR_NAV_ICON_LAYOUT = {
  baseGridUnitPx: 4,
  groupHeaderIconGridUnits: 4,
  get groupHeaderIconSizePx() {
    return this.baseGridUnitPx * this.groupHeaderIconGridUnits;
  },
} as const satisfies SidebarNavIconLayoutContract;

function assertPositivePixelDimension(value: PositivePixelDimension, label: string): void {
  if (!Number.isFinite(value) || !(value > 0)) {
    throw new Error(`${label} must be a positive finite pixel value.`);
  }
}

function assertPositiveUnitCount(value: number, label: string): void {
  if (!Number.isSafeInteger(value) || !(value > 0)) {
    throw new Error(`${label} must be a positive safe integer.`);
  }
}

assertPositivePixelDimension(
  SIDEBAR_NAV_ICON_LAYOUT.baseGridUnitPx,
  'Sidebar nav base grid unit',
);
assertPositiveUnitCount(
  SIDEBAR_NAV_ICON_LAYOUT.groupHeaderIconGridUnits,
  'Sidebar nav group header icon grid units',
);
assertPositivePixelDimension(
  SIDEBAR_NAV_ICON_LAYOUT.groupHeaderIconSizePx,
  'Sidebar nav group header icon size',
);

const GROUP_HEADER_ICON_SX = {
  fontSize: SIDEBAR_NAV_ICON_LAYOUT.groupHeaderIconSizePx,
} as const;
const REQUEST_ACCESS_LABEL = 'Solicitar acceso';

export const NAV_GROUPS: NavGroup[] = [
  {
    title: 'CREAR',
    icon: <AutoAwesomeOutlinedIcon sx={GROUP_HEADER_ICON_SX} />,
    items: [
      { label: 'Inicio', path: '/inicio' },
      { label: 'Calendario', path: '/estudio/calendario' },
      { label: 'Salas y recursos', path: '/estudio/salas' },
      { label: 'Órdenes estudio', path: '/estudio/ordenes' },
      { label: 'Servicios', path: '/estudio/servicios' },
      { label: 'Pipelines', path: '/estudio/pipelines' },
      { label: 'Sesiones en vivo', path: '/estudio/live-sessions' },
      { label: 'Reportes', path: '/estudio/reportes' },
      { label: 'Profesores', path: '/escuela/profesores' },
      { label: 'Clases', path: '/escuela/clases' },
      { label: 'Clases de prueba', path: '/escuela/trial-lessons' },
      { label: 'Solicitudes de prueba', path: '/escuela/trial-queue' },
      { label: 'Portal del profesor', path: '/mi-profesor' },
      { label: 'Panel de pasantes', path: '/practicas' },
    ],
  },
  {
    title: 'GENTE',
    icon: <PeopleAltOutlinedIcon sx={GROUP_HEADER_ICON_SX} />,
    items: [
      { label: 'Comunidad', path: '/fans' },
      { label: 'Tienda', path: '/marketplace' },
      { label: 'Lanzamientos públicos', path: '/records' },
      { label: 'Reservar estudio', path: '/reservar' },
      { label: 'Agenda de pruebas', path: '/trials' },
      { label: 'Registro live sessions', path: '/live-sessions/registro' },
      { label: 'Donaciones', path: '/donar' },
      { label: 'Domo Pululahua', path: '/domo-del-pululahua' },
      { label: 'Conexiones', path: '/social' },
      { label: 'Instagram', path: '/social/instagram' },
      { label: 'Chat', path: '/chat' },
      { label: 'Inbox social', path: '/social/inbox' },
      { label: 'Eventos sociales', path: '/social/eventos' },
      { label: 'Contactos', path: '/crm/contactos' },
      { label: 'Empresas', path: '/crm/empresas' },
      { label: 'Leads', path: '/crm/leads' },
    ],
  },
  {
    title: 'SELLO',
    icon: <AlbumOutlinedIcon sx={GROUP_HEADER_ICON_SX} />,
    items: [
      { label: 'Artistas', path: '/label/artistas' },
      { label: 'Proyectos', path: '/label/proyectos' },
      { label: 'Lanzamientos', path: '/label/releases' },
      { label: 'Activos', path: '/label/assets' },
      { label: 'Pistas', path: '/label/tracks' },
    ],
  },
  {
    title: 'OPERAR',
    icon: <Inventory2OutlinedIcon sx={GROUP_HEADER_ICON_SX} />,
    items: [
      { label: 'Inventario', path: '/operacion/inventario' },
      { label: 'Órdenes tienda', path: '/operacion/ordenes-marketplace' },
      { label: 'Reservas equipo', path: '/operacion/reservas-equipo' },
      { label: 'Pagos', path: '/finanzas/pagos' },
    ],
  },
  {
    title: 'ADMIN',
    icon: <AdminPanelSettingsOutlinedIcon sx={GROUP_HEADER_ICON_SX} />,
    items: [
      { label: COURSE_REGISTRATIONS_NAV_LABEL, path: '/configuracion/inscripciones-curso' },
      { label: 'Cursos', path: '/configuracion/cursos' },
      { label: 'Logs', path: '/configuracion/logs' },
      { label: 'Estado sistema', path: '/configuracion/estado' },
      { label: 'Diagnósticos', path: '/admin/diagnosticos' },
      { label: 'Brain y RAG', path: '/configuracion/brain' },
      { label: 'Usuarios admin', path: '/configuracion/usuarios-admin' },
      { label: 'Roles y permisos', path: '/configuracion/roles-permisos' },
      { label: 'Calendario Google', path: '/configuracion/integraciones/calendario' },
      { label: 'CMS', path: '/configuracion/cms' },
      { label: 'WhatsApp consentimiento', path: '/configuracion/whatsapp-consentimiento' },
      { label: 'Opciones UX', path: '/configuracion/opciones-ux' },
      { label: 'Preferencias', path: '/configuracion/preferencias' },
    ],
  },
  {
    title: 'HERRAMIENTAS',
    icon: <BuildOutlinedIcon sx={GROUP_HEADER_ICON_SX} />,
    items: [
      { label: 'ChatKit', path: '/herramientas/chatkit' },
      { label: 'Agente Tidal', path: '/herramientas/tidal-agent' },
      { label: 'Creador musical', path: '/herramientas/creador-musical' },
      { label: 'Token API', path: '/herramientas/token-admin' },
    ],
  },
  {
    title: 'AYUDA',
    icon: <HelpOutlineOutlinedIcon sx={GROUP_HEADER_ICON_SX} />,
    items: [
      { label: 'Manual', path: '/manual' },
      { label: 'Documentación', path: '/docs' },
      { label: 'Acerca de', path: '/acerca' },
      { label: 'Seguridad', path: '/seguridad' },
      { label: 'Sugerencias', path: '/feedback' },
    ],
  },
];

interface SidebarNavProps {
  open: boolean;
  onNavigate?: () => void;
}

const readStoredPathList = (storageKey: string): string[] => {
  if (typeof window === 'undefined') return [];
  try {
    const raw = window.localStorage.getItem(storageKey);
    if (!raw) return [];
    const parsed = JSON.parse(raw) as unknown;
    if (!Array.isArray(parsed)) return [];
    return parsed.filter((value): value is string => typeof value === 'string' && value.trim().length > 0);
  } catch {
    return [];
  }
};

const isRouteActive = (currentPath: string, itemPath: string) =>
  currentPath === itemPath || currentPath.startsWith(`${itemPath}/`);

const chatBadgeCountForPath = (path: string, unreadCount: number) => (path === '/chat' ? unreadCount : 0);

interface SidebarFrameProps {
  open: boolean;
  children: ReactNode;
}

function SidebarFrame(props: SidebarFrameProps) {
  const { open, children } = props;

  return (
    <Box
      component="aside"
      sx={{
        width: open ? { xs: 260, md: 240 } : 0,
        transition: 'width 0.25s ease',
        bgcolor: 'background.paper',
        color: 'text.primary',
        borderRight: '1px solid',
        borderColor: 'divider',
        overflowX: 'hidden',
        overflowY: 'hidden',
        display: { xs: open ? 'flex' : 'none', lg: 'flex' },
        position: { xs: 'fixed', lg: 'sticky' },
        zIndex: 1200,
        height: '100vh',
        maxHeight: '100vh',
        top: 0,
        left: 0,
        flexShrink: 0,
        flexDirection: 'column',
      }}
    >
      {children}
    </Box>
  );
}

interface SidebarSearchProps {
  filter: string;
  searchRef: { current: HTMLInputElement | null };
  onChange: (value: string) => void;
  onKeyDown: (event: ReactKeyboardEvent) => void;
}

function SidebarSearch(props: SidebarSearchProps) {
  const { filter, searchRef, onChange, onKeyDown } = props;

  return (
    <Stack spacing={2} sx={{ px: 2.5, pt: 3, pb: 2, flexShrink: 0 }}>
      <Typography variant="caption" sx={{ color: 'text.disabled', letterSpacing: 2 }}>
        MENÚ
      </Typography>
      <TextField
        value={filter}
        onChange={(event) => onChange(event.target.value)}
        inputRef={searchRef}
        onKeyDown={onKeyDown}
        size="small"
        placeholder="Buscar sección (/)"
        fullWidth
        inputProps={{ 'aria-label': 'Buscar sección' }}
        InputProps={{
          startAdornment: (
            <InputAdornment position="start">
              <SearchIcon sx={{ color: 'text.disabled', fontSize: 18 }} />
            </InputAdornment>
          ),
          endAdornment: filter ? (
            <InputAdornment position="end">
              <IconButton
                tabIndex={0}
                onClick={() => {
                  onChange('');
                  searchRef.current?.focus();
                }}
                size="small"
                aria-label="Limpiar búsqueda"
                sx={{ color: 'text.secondary' }}
              >
                <ClearIcon fontSize="small" />
              </IconButton>
            </InputAdornment>
          ) : null,
          sx: {
            bgcolor: 'action.hover',
            borderRadius: 2,
            '& .MuiOutlinedInput-notchedOutline': { border: 'none' },
            '&:hover .MuiOutlinedInput-notchedOutline': { border: 'none' },
            '&.Mui-focused .MuiOutlinedInput-notchedOutline': { border: 'none' },
          },
        }}
      />
    </Stack>
  );
}

interface SectionCaptionProps {
  label: string;
  icon?: ReactNode;
}

function SectionCaption(props: SectionCaptionProps) {
  const { label, icon } = props;

  return (
    <Stack direction="row" alignItems="center" spacing={1} sx={{ minWidth: 0, color: 'text.disabled' }}>
      {icon ? (
        <Box component="span" aria-hidden="true" sx={{ display: 'inline-flex' }}>
          {icon}
        </Box>
      ) : null}
      <Typography variant="caption" sx={{ color: 'text.disabled', letterSpacing: 1 }}>
        {label}
      </Typography>
    </Stack>
  );
}

interface UnreadBadgeProps {
  count: number;
  children: ReactNode;
}

function UnreadBadge(props: UnreadBadgeProps) {
  const { count, children } = props;
  if (count <= 0) return <>{children}</>;

  return (
    <Badge
      color="error"
      badgeContent={count > 99 ? '99+' : count}
      sx={{ '& .MuiBadge-badge': { fontSize: 11, height: 18, minWidth: 18 } }}
    >
      <span>{children}</span>
    </Badge>
  );
}

interface HighlightedLabelProps {
  label: string;
  query: string;
}

function HighlightedLabel(props: HighlightedLabelProps) {
  const { label, query } = props;
  const matchIndex = query ? label.toLowerCase().indexOf(query.toLowerCase()) : -1;
  if (matchIndex === -1) return <>{label}</>;

  const before = label.slice(0, matchIndex);
  const match = label.slice(matchIndex, matchIndex + query.length);
  const after = label.slice(matchIndex + query.length);

  return (
    <span>
      {before}
      <Box component="span" sx={{ color: 'primary.main', fontWeight: 700 }}>
        {match}
      </Box>
      {after}
    </span>
  );
}

interface ShortcutSectionProps {
  items: NavShortcutItem[];
  activePath: string;
  chatUnreadCount: number;
  onVisit: (path: string) => void;
}

function ShortcutSection(props: ShortcutSectionProps) {
  const { items, activePath, chatUnreadCount, onVisit } = props;
  if (items.length === 0) return null;

  return (
    <Box sx={{ pb: 1.5 }}>
      <Stack sx={{ px: 1.5, py: 1 }}>
        <SectionCaption label="ATAJOS" />
      </Stack>
      <List disablePadding>
        {items.map((item, index) => (
          <ShortcutLink
            key={`shortcut-${item.path}`}
            item={item}
            isLast={index === items.length - 1}
            activePath={activePath}
            unreadCount={chatBadgeCountForPath(item.path, chatUnreadCount)}
            onVisit={onVisit}
          />
        ))}
      </List>
    </Box>
  );
}

interface ShortcutLinkProps {
  item: NavShortcutItem;
  isLast: boolean;
  activePath: string;
  unreadCount: number;
  onVisit: (path: string) => void;
}

function ShortcutLink(props: ShortcutLinkProps) {
  const { item, isLast, activePath, unreadCount, onVisit } = props;
  const active = isRouteActive(activePath, item.path);

  return (
    <ListItemButton
      tabIndex={0}
      onClick={(event) => {
        event.currentTarget.focus();
        onVisit(item.path);
      }}
      component={RouterLink}
      to={item.path}
      selected={active}
      aria-current={active ? 'page' : undefined}
      sx={{
        borderRadius: 1.5,
        mb: isLast ? 0 : 0.5,
        bgcolor: active ? 'action.selected' : 'transparent',
        color: active ? 'primary.main' : 'text.primary',
        '&:hover': { bgcolor: 'action.hover' },
      }}
    >
      <FiberManualRecordIcon sx={{ fontSize: 8, mr: 1.5, color: active ? 'primary.main' : 'text.disabled' }} />
      <ListItemText
        primary={(
          <UnreadBadge count={unreadCount}>
            {item.label}
          </UnreadBadge>
        )}
        secondary={`Reciente · ${item.group}`}
        primaryTypographyProps={{ fontSize: 13, fontWeight: 600 }}
        secondaryTypographyProps={{ fontSize: 11, color: 'text.secondary' }}
      />
    </ListItemButton>
  );
}

function NoResults() {
  return (
    <Typography variant="body2" sx={{ px: 2, py: 1.5, color: 'text.secondary' }}>
      Sin coincidencias.
    </Typography>
  );
}

interface NavGroupHeaderProps {
  group: NavGroupView;
  isExpanded: boolean;
  onToggle: (title: string) => void;
}

function NavGroupHeader(props: NavGroupHeaderProps) {
  const { group, isExpanded, onToggle } = props;
  const toggleLabel = isExpanded ? `Ocultar ${group.title}` : `Mostrar ${group.title}`;

  return (
    <Stack direction="row" alignItems="center" justifyContent="space-between" sx={{ px: 1.5, py: 1 }}>
      <SectionCaption label={group.title} icon={group.icon} />
      {group.items.length > 1 ? (
        <IconButton
          tabIndex={0}
          onClick={(event) => {
            event.currentTarget.focus();
            onToggle(group.title);
          }}
          size="small"
          aria-label={toggleLabel}
          aria-expanded={isExpanded}
          sx={{ color: 'text.secondary' }}
        >
          {isExpanded ? <ExpandLessIcon fontSize="small" /> : <ExpandMoreIcon fontSize="small" />}
        </IconButton>
      ) : null}
    </Stack>
  );
}

interface NavGroupSectionProps {
  activePath: string;
  chatUnreadCount: number;
  getAccessHref: (group: NavGroupView) => string;
  group: NavGroupView;
  highlightedPath: string | null;
  isExpanded: boolean;
  query: string;
  onToggle: (title: string) => void;
  onVisit: (path: string) => void;
}

function NavGroupSection(props: NavGroupSectionProps) {
  const {
    activePath,
    chatUnreadCount,
    getAccessHref,
    group,
    highlightedPath,
    isExpanded,
    query,
    onToggle,
    onVisit,
  } = props;

  return (
    <Box>
      <NavGroupHeader group={group} isExpanded={isExpanded} onToggle={onToggle} />
      <Collapse in={isExpanded} timeout="auto" unmountOnExit>
        <List disablePadding>
          {group.items.map((item) => (
            <NavItemLink
              key={item.path}
              item={item}
              groupTitle={group.title}
              activePath={activePath}
              highlighted={highlightedPath === item.path}
              query={query}
              unreadCount={chatBadgeCountForPath(item.path, chatUnreadCount)}
              onVisit={onVisit}
            />
          ))}
          {group.items.length === 0 && group.restricted ? (
            <RestrictedGroupNotice href={getAccessHref(group)} />
          ) : null}
        </List>
      </Collapse>
    </Box>
  );
}

interface NavItemLinkProps {
  activePath: string;
  groupTitle: string;
  highlighted: boolean;
  item: NavItem;
  query: string;
  unreadCount: number;
  onVisit: (path: string) => void;
}

function NavItemLink(props: NavItemLinkProps) {
  const { activePath, groupTitle, highlighted, item, query, unreadCount, onVisit } = props;
  const active = isRouteActive(activePath, item.path);
  const selected = highlighted || (!query && active);

  return (
    <ListItemButton
      tabIndex={0}
      onClick={(event) => {
        event.currentTarget.focus();
        onVisit(item.path);
      }}
      component={RouterLink}
      to={item.path}
      selected={selected}
      aria-current={active ? 'page' : undefined}
      sx={{
        borderRadius: 1.5,
        mb: 0.5,
        color: active ? 'primary.main' : 'text.primary',
        bgcolor: highlighted ? 'action.hover' : active ? 'action.selected' : 'transparent',
        '&:hover': { bgcolor: 'action.hover' },
      }}
    >
      <FiberManualRecordIcon sx={{ fontSize: 8, mr: 1.5, color: active ? 'primary.main' : 'text.disabled' }} />
      <ListItemText
        primary={(
          <Box component="span" sx={{ minWidth: 0, overflow: 'hidden', textOverflow: 'ellipsis' }}>
            <UnreadBadge count={unreadCount}>
              <HighlightedLabel label={item.label} query={query} />
            </UnreadBadge>
          </Box>
        )}
        secondary={query ? groupTitle : undefined}
        primaryTypographyProps={{ fontSize: 13 }}
        secondaryTypographyProps={{ fontSize: 11, color: 'text.secondary' }}
      />
    </ListItemButton>
  );
}

interface RestrictedGroupNoticeProps {
  href: string;
}

function RestrictedGroupNotice(props: RestrictedGroupNoticeProps) {
  const { href } = props;

  return (
    <Stack spacing={1} sx={{ px: 2, py: 1.5 }}>
      <Typography variant="body2" sx={{ color: 'text.secondary' }}>
        No tienes acceso a esta sección.
      </Typography>
      <Button size="small" variant="outlined" disabled={!href} sx={{ alignSelf: 'flex-start' }} href={href}>
        {REQUEST_ACCESS_LABEL}
      </Button>
    </Stack>
  );
}

export default function SidebarNav({ open, onNavigate }: SidebarNavProps) {
  const location = useLocation();
  const { session } = useSession();
  const navigate = useNavigate();
  const [filter, setFilter] = useState('');
  const [highlightIndex, setHighlightIndex] = useState(-1);
  const [recentPaths, setRecentPaths] = useState(() => readStoredPathList(QUICK_RECENTS_KEY));
  const searchRef = useRef(null as HTMLInputElement | null);
  const canUsePath = useCallback(
    (path: string) => canAccessPath(path, session?.roles, session?.modules),
    [session?.modules, session?.roles],
  );
  const { unreadCount: chatUnreadCount } = useChatUnreadCount({ enabled: open });

  const buildAccessMailto = useCallback(
    (group: NavGroupView) => {
      const module = group.requiredModule ?? 'acceso';
      const subject = encodeURIComponent(`Acceso ${group.title} (${module})`);
      const bodyLines = [
        'Hola equipo, necesito acceso a esta sección.',
        `Sección: ${group.title}`,
        `Módulo: ${module}`,
        session ? `Usuario: ${session.displayName} (${session.username})` : null,
      ].filter(Boolean);
      const body = encodeURIComponent(bodyLines.join('\n'));
      return `mailto:soporte@tdf.com?subject=${subject}&body=${body}`;
    },
    [session],
  );

  const allowedNavGroups = useMemo((): NavGroupView[] => {
    return NAV_GROUPS.map((group) => {
      const hiddenItems = group.items.filter((item) => !canUsePath(item.path));
      const filteredItems = group.items.filter((item) => canUsePath(item.path));
      const requiredModule = hiddenItems[0] ? pathRequiresModule(hiddenItems[0].path) : null;
      const restricted = filteredItems.length === 0 && hiddenItems.length > 0;
      return { ...group, items: filteredItems, restricted, requiredModule };
    }).filter((group) => group.items.length > 0 || group.restricted);
  }, [canUsePath]);

  const filteredNavGroups = useMemo((): NavGroupView[] => {
    const query = filter.trim().toLowerCase();
    if (!query) return allowedNavGroups;
    return allowedNavGroups
      .map((group) => ({
        ...group,
        items: group.items.filter(
          (item) =>
            item.label.toLowerCase().includes(query) ||
            item.path.toLowerCase().includes(query),
        ),
        restricted: group.restricted,
      }))
      .filter((group) => group.items.length > 0 || group.restricted);
  }, [allowedNavGroups, filter]);

  const flatFilteredItems = useMemo(
    () => filteredNavGroups.flatMap((group) => group.items),
    [filteredNavGroups],
  );
  const flatAllowedItems = useMemo(
    (): NavShortcutItem[] => allowedNavGroups.flatMap((group) => group.items.map((item) => ({ ...item, group: group.title }))),
    [allowedNavGroups],
  );
  const shortcutItems = useMemo((): NavShortcutItem[] => {
    const itemByPath = new Map(flatAllowedItems.map((item) => [item.path, item]));
    const currentPath = location.pathname;
    const seen = new Set();
    return recentPaths
      .filter((path) => path !== currentPath)
      .map((path) => itemByPath.get(path))
      .filter((item): item is NavShortcutItem => item != null)
      .filter((item) => {
        if (seen.has(item.path)) return false;
        seen.add(item.path);
        return true;
      })
      .slice(0, MAX_SHORTCUT_RECENTS);
  }, [flatAllowedItems, location.pathname, recentPaths]);

  const ensureExpandedDefaults = (groups: NavGroupView[]) => {
    const next = new Set();
    groups.forEach((group) => {
      const hasSingle = group.items.length === 0 || group.items.length === 1;
      const matchesRoute = group.items.some(
        (item) => isRouteActive(location.pathname, item.path),
      );
      if (hasSingle || matchesRoute || group.restricted) next.add(group.title);
    });
    return next;
  };

  const [expandedGroups, setExpandedGroups] = useState(() => ensureExpandedDefaults(allowedNavGroups));

  useEffect(() => {
    setExpandedGroups((prev) => {
      const next = new Set(prev);
      allowedNavGroups.forEach((group) => {
        const matchesRoute = group.items.some(
          (item) => isRouteActive(location.pathname, item.path),
        );
        if (matchesRoute || group.items.length === 0 || group.items.length === 1) {
          next.add(group.title);
        }
      });
      return next;
    });
  }, [allowedNavGroups, location.pathname]);

  useEffect(() => {
    if (flatFilteredItems.length === 0) {
      setHighlightIndex(-1);
      return;
    }
    setHighlightIndex(0);
  }, [filter, flatFilteredItems.length]);

  useEffect(() => {
    if (!open) return;
    const handler = (event: KeyboardEvent) => {
      const activeTag = (event.target as HTMLElement | null)?.tagName?.toLowerCase();
      if (activeTag === 'input' || activeTag === 'textarea' || (event.target as HTMLElement | null)?.isContentEditable) {
        return;
      }
      if (event.key === '/') {
        event.preventDefault();
        searchRef.current?.focus();
      }
    };
    window.addEventListener('keydown', handler);
    return () => window.removeEventListener('keydown', handler);
  }, [open]);

  useEffect(() => {
    if (open) return;
    setFilter('');
    setHighlightIndex(-1);
  }, [open]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      window.localStorage.setItem(QUICK_RECENTS_KEY, JSON.stringify(recentPaths));
    } catch {
      // ignore persistence issues
    }
  }, [recentPaths]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    const handleStorage = () => {
      setRecentPaths(readStoredPathList(QUICK_RECENTS_KEY));
    };
    window.addEventListener('storage', handleStorage);
    return () => {
      window.removeEventListener('storage', handleStorage);
    };
  }, []);

  const toggleGroup = (title: string) => {
    setExpandedGroups((prev) => {
      const next = new Set(prev);
      if (next.has(title)) {
        next.delete(title);
      } else {
        next.add(title);
      }
      return next;
    });
  };

  const registerRecentPath = (path: string) => {
    setRecentPaths((prev) => [path, ...prev.filter((existing) => existing !== path)].slice(0, MAX_SHORTCUT_RECENTS));
  };

  const handleVisit = (path: string) => {
    registerRecentPath(path);
    onNavigate?.();
  };

  const handleSearchKeyDown = (event: ReactKeyboardEvent) => {
    if (event.key === 'Escape') {
      event.preventDefault();
      setFilter('');
      setHighlightIndex(-1);
      searchRef.current?.blur();
      return;
    }

    if (flatFilteredItems.length === 0) return;

    if (event.key === 'ArrowDown') {
      event.preventDefault();
      setHighlightIndex((prev) => {
        const next = prev + 1;
        return next >= flatFilteredItems.length ? 0 : next;
      });
    } else if (event.key === 'ArrowUp') {
      event.preventDefault();
      setHighlightIndex((prev) => (prev > 0 ? prev - 1 : flatFilteredItems.length - 1));
    } else if (event.key === 'Enter' && highlightIndex >= 0) {
      event.preventDefault();
      const target = flatFilteredItems[highlightIndex];
      if (target) {
        registerRecentPath(target.path);
        navigate(target.path);
        onNavigate?.();
      }
    }
  };

  const query = filter.trim();
  const highlightedPath = highlightIndex >= 0 ? flatFilteredItems[highlightIndex]?.path ?? null : null;

  return (
    <SidebarFrame open={open}>
      <SidebarSearch
        filter={filter}
        searchRef={searchRef}
        onChange={setFilter}
        onKeyDown={handleSearchKeyDown}
      />
      <List disablePadding sx={{ flex: 1, overflowY: 'auto', px: 1.5 }}>
        {query ? null : (
          <ShortcutSection
            items={shortcutItems}
            activePath={location.pathname}
            chatUnreadCount={chatUnreadCount}
            onVisit={handleVisit}
          />
        )}
        {filteredNavGroups.length === 0 ? <NoResults /> : null}
        {filteredNavGroups.map((group) => {
          const isExpanded = query.length > 0 || expandedGroups.has(group.title) || Boolean(group.restricted);
          return (
            <NavGroupSection
              key={group.title}
              group={group}
              activePath={location.pathname}
              highlightedPath={highlightedPath}
              isExpanded={isExpanded}
              query={query}
              chatUnreadCount={chatUnreadCount}
              getAccessHref={buildAccessMailto}
              onToggle={toggleGroup}
              onVisit={handleVisit}
            />
          );
        })}
      </List>
    </SidebarFrame>
  );
}
