import { Component, useEffect, useMemo, useRef, useState } from 'react';
import {
  Badge,
  Box,
  Collapse,
  IconButton,
  List,
  ListItemButton,
  ListItemText,
  Stack,
  Typography,
  TextField,
  InputAdornment,
  Tooltip,
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
import LockOutlinedIcon from '@mui/icons-material/LockOutlined';
import StarBorderOutlinedIcon from '@mui/icons-material/StarBorderOutlined';
import StarOutlinedIcon from '@mui/icons-material/StarOutlined';
import PushPinOutlinedIcon from '@mui/icons-material/PushPinOutlined';
import PushPinIcon from '@mui/icons-material/PushPin';
import ArrowUpwardIcon from '@mui/icons-material/ArrowUpward';
import ArrowDownwardIcon from '@mui/icons-material/ArrowDownward';
import type { ErrorInfo, KeyboardEvent as ReactKeyboardEvent, ReactNode } from 'react';
import { Link as RouterLink, useLocation } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import { useSession } from '../session/SessionContext';
import { useChatUnreadCount } from '../hooks/useChatUnreadCount';
import {
  accessRequestPath,
  evaluateFeatureAccess,
  featureGroups,
  featureLabel,
  featureRegistry,
  featureSearchText,
  getFeatureById,
  normalizeFeatureToken,
} from '../features/featureRegistry';
import appI18n from '../i18n';
import { useNavigationPreferences } from '../hooks/useNavigationPreferences';
import type { NavigationPreferenceDTO } from '../api/navigationPreferences';
import { getAnalyticsClient } from '../analytics/posthog';

export interface NavItem {
  featureId: string;
  label: string;
  path: string;
  searchText: string;
  locked?: boolean;
  accessPath?: string;
  missingAccess?: string;
}
export interface NavGroup {
  title: string;
  items: NavItem[];
  icon?: ReactNode;
}

type NavGroupView = NavGroup;
type NavShortcutItem = NavItem & {
  group: string;
  shortcutKind?: 'pinned' | 'favorite' | 'recent';
  pinOrder?: number | null;
};
type PositivePixelDimension = number;
type PositiveSafeInteger = number;

interface SidebarNavIconLayoutContract {
  readonly baseGridUnitPx: PositivePixelDimension;
  readonly groupHeaderIconGridUnits: number;
  readonly groupHeaderIconSizePx: PositivePixelDimension;
}

interface UnreadBadgeDisplayContract {
  readonly maxExactCount: PositiveSafeInteger;
  readonly overflowLabel: string;
}

interface HighlightedLabelStyleContract {
  readonly matchFontWeight: PositiveSafeInteger;
}

const QUICK_RECENTS_KEY = 'tdf-quick-nav-recents';
const MAX_SHORTCUT_RECENTS = 6;
const UNREAD_BADGE_MAX_EXACT_COUNT = 99;
const SEARCH_MATCH_FONT_WEIGHT = 700;
export const SIDEBAR_NAV_NO_RESULTS_BOUNDARY_NAME = 'sidebar-nav-no-results';

// Invariant: top-level group icons stay compact enough to align with caption
// headers without increasing the sidebar row height.
export const SIDEBAR_NAV_ICON_LAYOUT = {
  baseGridUnitPx: 4,
  groupHeaderIconGridUnits: 4,
  get groupHeaderIconSizePx() {
    return this.baseGridUnitPx * this.groupHeaderIconGridUnits;
  },
} as const satisfies SidebarNavIconLayoutContract;

// Invariant: unread badges display exact values until the UI's compact badge
// width would become unstable, then switch to a fixed overflow label.
export const SIDEBAR_NAV_UNREAD_BADGE_DISPLAY = {
  maxExactCount: UNREAD_BADGE_MAX_EXACT_COUNT,
  overflowLabel: `${UNREAD_BADGE_MAX_EXACT_COUNT}+`,
} as const satisfies UnreadBadgeDisplayContract;

// Invariant: search-highlight emphasis uses one named weight so visual drift is
// detectable in tests instead of being hidden in inline sx literals.
export const SIDEBAR_NAV_HIGHLIGHTED_LABEL_STYLE = {
  matchFontWeight: SEARCH_MATCH_FONT_WEIGHT,
} as const satisfies HighlightedLabelStyleContract;

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
assertPositiveUnitCount(
  SIDEBAR_NAV_UNREAD_BADGE_DISPLAY.maxExactCount,
  'Sidebar nav unread badge exact-count limit',
);
assertPositiveUnitCount(
  SIDEBAR_NAV_HIGHLIGHTED_LABEL_STYLE.matchFontWeight,
  'Sidebar nav highlighted-label font weight',
);

const GROUP_HEADER_ICON_SX = {
  fontSize: SIDEBAR_NAV_ICON_LAYOUT.groupHeaderIconSizePx,
} as const;
const REQUEST_ACCESS_LABEL = 'Solicitar acceso';

const groupIcon = (groupId: string): ReactNode => {
  switch (groupId) {
    case 'create': return <AutoAwesomeOutlinedIcon sx={GROUP_HEADER_ICON_SX} />;
    case 'people': return <PeopleAltOutlinedIcon sx={GROUP_HEADER_ICON_SX} />;
    case 'label': return <AlbumOutlinedIcon sx={GROUP_HEADER_ICON_SX} />;
    case 'operate': return <Inventory2OutlinedIcon sx={GROUP_HEADER_ICON_SX} />;
    case 'admin': return <AdminPanelSettingsOutlinedIcon sx={GROUP_HEADER_ICON_SX} />;
    case 'tools': return <BuildOutlinedIcon sx={GROUP_HEADER_ICON_SX} />;
    case 'help': return <HelpOutlineOutlinedIcon sx={GROUP_HEADER_ICON_SX} />;
    default: return undefined;
  }
};

export function buildRegistryNavGroups(locale = 'es'): NavGroup[] {
  return featureGroups.map((group) => ({
    title: locale.toLowerCase().startsWith('en') ? group.labelEn : group.labelEs,
    icon: groupIcon(group.id),
    items: featureRegistry
      .filter((feature) =>
        feature.navigationGroup === group.id
        && feature.globalMenu
        && !feature.technical
        && typeof feature.webRoute === 'string',
      )
      .map((feature) => ({
        featureId: feature.id,
        label: featureLabel(feature, locale),
        path: feature.webRoute!,
        searchText: featureSearchText(feature),
      })),
  })).filter((group) => group.items.length > 0);
}

export const NAV_GROUPS: NavGroup[] = buildRegistryNavGroups('es');

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

/**
 * @precondition caller has already suppressed non-positive counts.
 * @postcondition returns an exact number up to maxExactCount and the fixed
 * overflow label for all larger counts.
 */
export const formatUnreadBadgeContent = (
  count: number,
): number | typeof SIDEBAR_NAV_UNREAD_BADGE_DISPLAY.overflowLabel =>
  count > SIDEBAR_NAV_UNREAD_BADGE_DISPLAY.maxExactCount
    ? SIDEBAR_NAV_UNREAD_BADGE_DISPLAY.overflowLabel
    : count;

interface SidebarFrameProps {
  open: boolean;
  children: ReactNode;
}

function SidebarFrame(props: SidebarFrameProps) {
  const { open, children } = props;

  return (
    <Box
      component="nav"
      id="app-sidebar"
      aria-label="Navegación de la aplicación"
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
      {open ? children : null}
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
      <Typography variant="caption" sx={{ color: 'text.secondary', letterSpacing: 2 }}>
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
                sx={{ color: 'text.secondary', width: 44, height: 44 }}
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
    <Stack direction="row" alignItems="center" spacing={1} sx={{ minWidth: 0, color: 'text.secondary' }}>
      {icon ? (
        <Box component="span" aria-hidden="true" sx={{ display: 'inline-flex' }}>
          {icon}
        </Box>
      ) : null}
      <Typography variant="caption" sx={{ color: 'text.secondary', letterSpacing: 1 }}>
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
      badgeContent={formatUnreadBadgeContent(count)}
      sx={{ '& .MuiBadge-badge': { fontSize: 11, height: 18, minWidth: 18 } }}
    >
      <span>{children}</span>
    </Badge>
  );
}

interface HighlightedLabelProps {
  label: string;
  searchQuery: string;
}

function HighlightedLabel(props: HighlightedLabelProps) {
  const { label, searchQuery } = props;
  const matchIndex = searchQuery ? label.toLowerCase().indexOf(searchQuery.toLowerCase()) : -1;
  if (matchIndex === -1) return <>{label}</>;

  const before = label.slice(0, matchIndex);
  const match = label.slice(matchIndex, matchIndex + searchQuery.length);
  const after = label.slice(matchIndex + searchQuery.length);

  return (
    <span>
      {before}
      <Box component="span" sx={{ color: 'primary.main', fontWeight: SIDEBAR_NAV_HIGHLIGHTED_LABEL_STYLE.matchFontWeight }}>
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
  onMovePin: (featureId: string, direction: -1 | 1) => void;
  onVisit: (featureId: string, path: string) => void;
}

function ShortcutSection(props: ShortcutSectionProps) {
  const { items, activePath, chatUnreadCount, onMovePin, onVisit } = props;
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
            onMovePin={onMovePin}
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
  onMovePin: (featureId: string, direction: -1 | 1) => void;
  onVisit: (featureId: string, path: string) => void;
}

function ShortcutLink(props: ShortcutLinkProps) {
  const { item, isLast, activePath, unreadCount, onMovePin, onVisit } = props;
  const isShortcutActive = isRouteActive(activePath, item.path);

  return (
    <Stack component="li" direction="row" alignItems="center" sx={{ mb: isLast ? 0 : 0.5, listStyle: 'none' }}>
      <ListItemButton
        tabIndex={0}
        onClick={(event) => {
          event.currentTarget.focus();
          onVisit(item.featureId, item.path);
        }}
        component={RouterLink}
        to={item.path}
        selected={isShortcutActive}
        aria-current={isShortcutActive ? 'page' : undefined}
        sx={{
          minWidth: 0,
          borderRadius: 1.5,
          bgcolor: isShortcutActive ? 'action.selected' : 'transparent',
          color: isShortcutActive ? 'primary.main' : 'text.primary',
          '&:hover': { bgcolor: 'action.hover' },
        }}
      >
        <FiberManualRecordIcon sx={{ fontSize: 8, mr: 1.5, color: isShortcutActive ? 'primary.main' : 'text.disabled' }} />
        <ListItemText
          primary={(
            <UnreadBadge count={unreadCount}>
              {item.label}
            </UnreadBadge>
          )}
          secondary={`${item.shortcutKind === 'pinned' ? 'Fijado' : item.shortcutKind === 'favorite' ? 'Favorito' : 'Reciente'} · ${item.group}`}
          primaryTypographyProps={{ fontSize: 13, fontWeight: 600, noWrap: true }}
          secondaryTypographyProps={{ fontSize: 11, color: 'text.secondary', noWrap: true }}
        />
      </ListItemButton>
      {item.shortcutKind === 'pinned' ? (
        <Stack>
          <IconButton size="small" aria-label={`Subir ${item.label}`} onClick={() => onMovePin(item.featureId, -1)} sx={{ width: 44, height: 44 }}>
            <ArrowUpwardIcon fontSize="small" />
          </IconButton>
          <IconButton size="small" aria-label={`Bajar ${item.label}`} onClick={() => onMovePin(item.featureId, 1)} sx={{ width: 44, height: 44 }}>
            <ArrowDownwardIcon fontSize="small" />
          </IconButton>
        </Stack>
      ) : null}
    </Stack>
  );
}

const SIDEBAR_NAV_NO_RESULTS_CONTENT = (
  <Typography variant="body2" sx={{ px: 2, py: 1.5, color: 'text.secondary' }}>
    Sin coincidencias.
  </Typography>
);

interface SidebarNavErrorBoundaryProps {
  boundaryName: typeof SIDEBAR_NAV_NO_RESULTS_BOUNDARY_NAME;
  children: ReactNode;
}

interface SidebarNavErrorBoundaryState {
  error: Error | null;
}

export class SidebarNavErrorBoundary extends Component<SidebarNavErrorBoundaryProps, SidebarNavErrorBoundaryState> {
  override state: SidebarNavErrorBoundaryState = { error: null };

  static getDerivedStateFromError(error: Error): SidebarNavErrorBoundaryState {
    return { error };
  }

  override componentDidCatch(error: Error, info: ErrorInfo) {
    console.error('Sidebar nav critical subtree failed', {
      boundaryName: this.props.boundaryName,
      error,
      componentStack: info.componentStack,
    });
  }

  override render() {
    if (!this.state.error) {
      return this.props.children;
    }

    return (
      <Typography role="status" variant="body2" sx={{ px: 2, py: 1.5, color: 'text.secondary' }}>
        No pudimos mostrar esta sección.
      </Typography>
    );
  }
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
  group: NavGroupView;
  highlightedPath: string | null;
  isExpanded: boolean;
  searchQuery: string;
  onToggle: (title: string) => void;
  locale: string;
  preferences: ReadonlyMap<string, NavigationPreferenceDTO>;
  onPreferenceChange: (featureId: string, kind: 'favorite' | 'pinned') => void;
  onVisit: (featureId: string, path: string) => void;
}

function NavGroupSection(props: NavGroupSectionProps) {
  const {
    activePath,
    chatUnreadCount,
    group,
    highlightedPath,
    isExpanded,
    searchQuery,
    onToggle,
    locale,
    preferences,
    onPreferenceChange,
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
              searchQuery={searchQuery}
              unreadCount={chatBadgeCountForPath(item.path, chatUnreadCount)}
              locale={locale}
              preference={preferences.get(item.featureId)}
              onPreferenceChange={onPreferenceChange}
              onVisit={onVisit}
            />
          ))}
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
  searchQuery: string;
  unreadCount: number;
  locale: string;
  preference?: NavigationPreferenceDTO;
  onPreferenceChange: (featureId: string, kind: 'favorite' | 'pinned') => void;
  onVisit: (featureId: string, path: string) => void;
}

function NavItemLink(props: NavItemLinkProps) {
  const { activePath, groupTitle, highlighted, item, searchQuery, unreadCount, locale, preference, onPreferenceChange, onVisit } = props;
  const destination = item.locked ? item.accessPath ?? item.path : item.path;
  const isNavItemActive = !item.locked && isRouteActive(activePath, item.path);
  const selected = highlighted || (!searchQuery && isNavItemActive);
  const feature = getFeatureById(item.featureId);
  const english = locale.toLowerCase().startsWith('en');

  return (
    <Stack component="li" direction="row" alignItems="center" sx={{ mb: 0.5, listStyle: 'none' }}>
      <ListItemButton
        tabIndex={0}
        onClick={(event) => {
          event.currentTarget.focus();
          onVisit(item.locked ? '' : item.featureId, destination);
        }}
        component={RouterLink}
        to={destination}
        selected={selected}
        aria-current={isNavItemActive ? 'page' : undefined}
        aria-label={item.locked ? `${item.label}. ${item.missingAccess ?? REQUEST_ACCESS_LABEL}` : undefined}
        sx={{
          minWidth: 0,
          borderRadius: 1.5,
          color: isNavItemActive ? 'primary.main' : 'text.primary',
          bgcolor: highlighted ? 'action.hover' : isNavItemActive ? 'action.selected' : 'transparent',
          '&:hover': { bgcolor: 'action.hover' },
        }}
      >
        {item.locked ? (
          <LockOutlinedIcon aria-hidden="true" sx={{ fontSize: 16, mr: 1.5, color: 'text.secondary' }} />
        ) : (
          <FiberManualRecordIcon sx={{ fontSize: 8, mr: 1.5, color: isNavItemActive ? 'primary.main' : 'text.disabled' }} />
        )}
        <ListItemText
          primary={(
            <Box component="span" sx={{ minWidth: 0, overflow: 'hidden', textOverflow: 'ellipsis' }}>
              <UnreadBadge count={unreadCount}>
                <HighlightedLabel label={item.label} searchQuery={searchQuery} />
              </UnreadBadge>
            </Box>
          )}
          secondary={item.locked ? item.missingAccess ?? REQUEST_ACCESS_LABEL : searchQuery ? groupTitle : undefined}
          primaryTypographyProps={{ fontSize: 13, noWrap: true }}
          secondaryTypographyProps={{ fontSize: 11, color: 'text.secondary' }}
        />
      </ListItemButton>
      {!item.locked && feature?.favoriteEligible ? (
        <Tooltip title={preference?.favorite ? (english ? 'Remove favorite' : 'Quitar favorito') : (english ? 'Favorite' : 'Favorito')}>
          <IconButton
            aria-label={preference?.favorite ? `${english ? 'Remove favorite' : 'Quitar favorito'} ${item.label}` : `${english ? 'Favorite' : 'Favorito'} ${item.label}`}
            onClick={() => onPreferenceChange(item.featureId, 'favorite')}
            sx={{ width: 44, height: 44 }}
          >
            {preference?.favorite ? <StarOutlinedIcon fontSize="small" /> : <StarBorderOutlinedIcon fontSize="small" />}
          </IconButton>
        </Tooltip>
      ) : null}
      {!item.locked && feature?.pinEligible ? (
        <Tooltip title={preference?.pinned ? (english ? 'Unpin' : 'Desfijar') : (english ? 'Pin' : 'Fijar')}>
          <IconButton
            aria-label={preference?.pinned ? `${english ? 'Unpin' : 'Desfijar'} ${item.label}` : `${english ? 'Pin' : 'Fijar'} ${item.label}`}
            onClick={() => onPreferenceChange(item.featureId, 'pinned')}
            sx={{ width: 44, height: 44 }}
          >
            {preference?.pinned ? <PushPinIcon fontSize="small" /> : <PushPinOutlinedIcon fontSize="small" />}
          </IconButton>
        </Tooltip>
      ) : null}
    </Stack>
  );
}

export default function SidebarNav({ open, onNavigate }: SidebarNavProps) {
  const location = useLocation();
  const { session } = useSession();
  const [featureLocale, setFeatureLocale] = useState(
    () => appI18n.resolvedLanguage ?? appI18n.language ?? 'es',
  );
  const navigate = useNavigate();
  const [filter, setFilter] = useState('');
  const [highlightIndex, setHighlightIndex] = useState(-1);
  const [recentPaths, setRecentPaths] = useState(() => readStoredPathList(QUICK_RECENTS_KEY));
  const searchRef = useRef(null as HTMLInputElement | null);
  const { unreadCount: chatUnreadCount } = useChatUnreadCount({ enabled: open });
  const navigationPreferences = useNavigationPreferences(Boolean(session));

  useEffect(() => {
    const handleLanguageChanged = (language: string) => setFeatureLocale(language || 'es');
    appI18n.on('languageChanged', handleLanguageChanged);
    return () => appI18n.off('languageChanged', handleLanguageChanged);
  }, []);

  const allowedNavGroups = useMemo((): NavGroupView[] => {
    const currentSession = {
      authenticated: Boolean(session),
      roles: session?.roles,
      modules: session?.modules,
      featureFlags: session?.featureFlags,
    };
    return buildRegistryNavGroups(featureLocale).map((group) => ({
      ...group,
      items: group.items.flatMap((item) => {
        const navFeature = getFeatureById(item.featureId);
        if (!navFeature) return [];
        const decision = evaluateFeatureAccess(navFeature, currentSession, 'discover');
        if (decision.state === 'concealed') return [];
        if (decision.state === 'allowed') return [item];
        const missingCategory = decision.missingModules[0] ?? decision.missingRoles[0] ?? 'permiso';
        return [{
          ...item,
          locked: true,
          accessPath: accessRequestPath(navFeature, 'view'),
          missingAccess: `Requiere ${missingCategory} · ${REQUEST_ACCESS_LABEL}`,
        }];
      }),
    })).filter((group) => group.items.length > 0);
  }, [featureLocale, session]);

  const filteredNavGroups = useMemo((): NavGroupView[] => {
    const normalizedSearchQuery = normalizeFeatureToken(filter);
    if (!normalizedSearchQuery) return allowedNavGroups;
    return allowedNavGroups
      .map((group) => ({
        ...group,
        items: group.items.filter(
          (item) =>
            item.searchText.includes(normalizedSearchQuery),
        ),
      }))
      .filter((group) => group.items.length > 0);
  }, [allowedNavGroups, filter]);

  const flatFilteredItems = useMemo(
    () => filteredNavGroups.flatMap((group) => group.items),
    [filteredNavGroups],
  );
  const flatAllowedItems = useMemo(
    (): NavShortcutItem[] => allowedNavGroups.flatMap((group) => group.items
      .filter((item) => !item.locked)
      .map((item) => ({ ...item, group: group.title }))),
    [allowedNavGroups],
  );
  const preferenceMap = useMemo(
    () => new Map((navigationPreferences.query.data ?? []).map((preference) => [preference.featureId, preference])),
    [navigationPreferences.query.data],
  );
  const shortcutItems = useMemo((): NavShortcutItem[] => {
    const itemByFeature = new Map(flatAllowedItems.map((item) => [item.featureId, item]));
    const itemByPath = new Map(flatAllowedItems.map((item) => [item.path, item]));
    const currentPath = location.pathname;
    const preferences = navigationPreferences.query.data ?? [];
    const serverShortcuts = [
      ...preferences.filter((preference) => preference.pinned).sort((left, right) => (left.pinOrder ?? 0) - (right.pinOrder ?? 0)).map((preference) => ({ preference, shortcutKind: 'pinned' as const })),
      ...preferences.filter((preference) => preference.favorite && !preference.pinned).sort((left, right) => right.updatedAt.localeCompare(left.updatedAt)).map((preference) => ({ preference, shortcutKind: 'favorite' as const })),
      ...preferences.filter((preference) => preference.lastVisitedAt && !preference.pinned && !preference.favorite).sort((left, right) => (right.lastVisitedAt ?? '').localeCompare(left.lastVisitedAt ?? '')).map((preference) => ({ preference, shortcutKind: 'recent' as const })),
    ].flatMap(({ preference, shortcutKind }) => {
      const item = itemByFeature.get(preference.featureId);
      return item && item.path !== currentPath ? [{ ...item, shortcutKind, pinOrder: preference.pinOrder }] : [];
    });
    const localFallback = preferences.length === 0
      ? recentPaths.flatMap((path) => {
          const recentItem = itemByPath.get(path);
          return recentItem && path !== currentPath ? [{ ...recentItem, shortcutKind: 'recent' as const }] : [];
        })
      : [];
    const seen = new Set<string>();
    return [...serverShortcuts, ...localFallback].filter((item) => {
      if (seen.has(item.featureId)) return false;
      seen.add(item.featureId);
      return true;
    }).slice(0, MAX_SHORTCUT_RECENTS);
  }, [flatAllowedItems, location.pathname, navigationPreferences.query.data, recentPaths]);

  const ensureExpandedDefaults = (groups: NavGroupView[]) => {
    const next = new Set();
    groups.forEach((group) => {
      const hasSingle = group.items.length === 0 || group.items.length === 1;
      const matchesRoute = group.items.some(
        (item) => isRouteActive(location.pathname, item.path),
      );
      if (hasSingle || matchesRoute) next.add(group.title);
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

  const handleVisit = (featureId: string, path: string) => {
    registerRecentPath(path);
    if (featureId) {
      getAnalyticsClient().capture('feature_navigation_selected', { feature_id: featureId, platform: 'web', source: 'sidebar' });
    }
    onNavigate?.();
  };

  const handlePreferenceChange = (featureId: string, kind: 'favorite' | 'pinned') => {
    const current = preferenceMap.get(featureId);
    const pinnedPreferences = (navigationPreferences.query.data ?? []).filter((preference) => preference.pinned);
    const nextPreference = {
      featureId,
      favorite: kind === 'favorite' ? !current?.favorite : Boolean(current?.favorite),
      pinned: kind === 'pinned' ? !current?.pinned : Boolean(current?.pinned),
      pinOrder: kind === 'pinned' && !current?.pinned
        ? Math.max(-1, ...pinnedPreferences.map((preference) => preference.pinOrder ?? 0)) + 1
        : current?.pinned ? current.pinOrder ?? 0 : null,
    };
    if (!nextPreference.pinned) nextPreference.pinOrder = null;
    navigationPreferences.update.mutate(nextPreference);
    getAnalyticsClient().capture(kind === 'favorite' ? 'feature_favorite_changed' : 'feature_pin_changed', {
      feature_id: featureId,
      enabled: kind === 'favorite' ? nextPreference.favorite : nextPreference.pinned,
      platform: 'web',
    });
  };

  const handleMovePin = (featureId: string, direction: -1 | 1) => {
    const pinned = (navigationPreferences.query.data ?? [])
      .filter((preference) => preference.pinned)
      .sort((left, right) => (left.pinOrder ?? 0) - (right.pinOrder ?? 0));
    const index = pinned.findIndex((preference) => preference.featureId === featureId);
    const swapIndex = index + direction;
    if (index < 0 || swapIndex < 0 || swapIndex >= pinned.length) return;
    const currentPreference = pinned[index];
    const other = pinned[swapIndex];
    if (!currentPreference || !other) return;
    const currentOrder = currentPreference.pinOrder ?? index;
    const otherOrder = other.pinOrder ?? swapIndex;
    void Promise.all([
      navigationPreferences.update.mutateAsync({ ...currentPreference, pinOrder: otherOrder }),
      navigationPreferences.update.mutateAsync({ ...other, pinOrder: currentOrder }),
    ]);
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
        handleVisit(target.locked ? '' : target.featureId, target.path);
        navigate(target.locked ? target.accessPath ?? target.path : target.path);
      }
    }
  };

  const searchQuery = filter.trim();
  const highlightedPath = highlightIndex >= 0 ? flatFilteredItems[highlightIndex]?.path ?? null : null;

  return (
    <SidebarFrame open={open}>
      <SidebarSearch
        filter={filter}
        searchRef={searchRef}
        onChange={setFilter}
        onKeyDown={handleSearchKeyDown}
      />
      <Box sx={{ flex: 1, overflowY: 'auto', px: 1.5 }}>
        {searchQuery ? null : (
          <ShortcutSection
            items={shortcutItems}
            activePath={location.pathname}
            chatUnreadCount={chatUnreadCount}
            onMovePin={handleMovePin}
            onVisit={handleVisit}
          />
        )}
        {filteredNavGroups.length === 0 ? (
          <SidebarNavErrorBoundary boundaryName={SIDEBAR_NAV_NO_RESULTS_BOUNDARY_NAME}>
            {SIDEBAR_NAV_NO_RESULTS_CONTENT}
          </SidebarNavErrorBoundary>
        ) : null}
        {filteredNavGroups.map((group) => {
          const isExpanded = searchQuery.length > 0 || expandedGroups.has(group.title);
          return (
            <NavGroupSection
              key={group.title}
              group={group}
              activePath={location.pathname}
              highlightedPath={highlightedPath}
              isExpanded={isExpanded}
              searchQuery={searchQuery}
              chatUnreadCount={chatUnreadCount}
              locale={featureLocale}
              preferences={preferenceMap}
              onPreferenceChange={handlePreferenceChange}
              onToggle={toggleGroup}
              onVisit={handleVisit}
            />
          );
        })}
      </Box>
    </SidebarFrame>
  );
}
