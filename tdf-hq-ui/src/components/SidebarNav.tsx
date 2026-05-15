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
}

type NavGroupView = NavGroup & { restricted?: boolean; requiredModule?: string | null };
type NavShortcutItem = NavItem & { group: string };

const QUICK_RECENTS_KEY = 'tdf-quick-nav-recents';
const MAX_SHORTCUT_RECENTS = 6;

export const NAV_GROUPS: NavGroup[] = [
  {
    title: 'CREAR',
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
    items: [
      { label: 'Comunidad', path: '/fans' },
      { label: 'Tienda', path: '/marketplace' },
      { label: 'Lanzamientos', path: '/records' },
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
    items: [
      { label: 'Inventario', path: '/operacion/inventario' },
      { label: 'Órdenes tienda', path: '/operacion/ordenes-marketplace' },
      { label: 'Reservas equipo', path: '/operacion/reservas-equipo' },
      { label: 'Pagos', path: '/finanzas/pagos' },
    ],
  },
  {
    title: 'ADMIN',
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
    items: [
      { label: 'ChatKit', path: '/herramientas/chatkit' },
      { label: 'Agente Tidal', path: '/herramientas/tidal-agent' },
      { label: 'Creador musical', path: '/herramientas/creador-musical' },
      { label: 'Token API', path: '/herramientas/token-admin' },
    ],
  },
  {
    title: 'AYUDA',
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

export default function SidebarNav({ open, onNavigate }: SidebarNavProps) {
  const location = useLocation();
  const { session } = useSession();
  const navigate = useNavigate();
  const [filter, setFilter] = useState('');
  const [highlightIndex, setHighlightIndex] = useState<number>(-1);
  const [recentPaths, setRecentPaths] = useState<string[]>(() => readStoredPathList(QUICK_RECENTS_KEY));
  const searchRef = useRef<HTMLInputElement | null>(null);
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

  const allowedNavGroups = useMemo<NavGroupView[]>(() => {
    return NAV_GROUPS.map((group) => {
      const hiddenItems = group.items.filter((item) => !canUsePath(item.path));
      const filteredItems = group.items.filter((item) => canUsePath(item.path));
      const requiredModule = hiddenItems[0] ? pathRequiresModule(hiddenItems[0].path) : null;
      const restricted = filteredItems.length === 0 && hiddenItems.length > 0;
      return { ...group, items: filteredItems, restricted, requiredModule };
    }).filter((group) => group.items.length > 0 || group.restricted);
  }, [canUsePath]);

  const filteredNavGroups = useMemo<NavGroupView[]>(() => {
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
  const flatAllowedItems = useMemo<NavShortcutItem[]>(
    () => allowedNavGroups.flatMap((group) => group.items.map((item) => ({ ...item, group: group.title }))),
    [allowedNavGroups],
  );
  const recentPathSet = useMemo(() => new Set(recentPaths), [recentPaths]);
  const shortcutItems = useMemo<NavShortcutItem[]>(() => {
    const itemByPath = new Map(flatAllowedItems.map((item) => [item.path, item]));
    const currentPath = location.pathname;
    const moduleShortcutCandidates = [
      '/inicio',
      '/crm/contactos',
      '/mi-profesor',
      '/escuela/clases',
      '/estudio/calendario',
      '/label/artistas',
      '/label/tracks',
      '/operacion/inventario',
      '/finanzas/pagos',
      '/practicas',
    ].filter((path) => canUsePath(path));
    const preferredPaths = [
      ...recentPaths.filter((path) => path !== currentPath),
      ...moduleShortcutCandidates.filter((path) => path !== currentPath),
    ];
    const seen = new Set<string>();
    return preferredPaths
      .map((path) => itemByPath.get(path))
      .filter((item): item is NavShortcutItem => item != null)
      .filter((item) => {
        if (seen.has(item.path)) return false;
        seen.add(item.path);
        return true;
      })
      .slice(0, 6);
  }, [canUsePath, flatAllowedItems, location.pathname, recentPaths]);

  const ensureExpandedDefaults = (groups: NavGroupView[]) => {
    const next = new Set<string>();
    groups.forEach((group) => {
      const hasSingle = group.items.length <= 1;
      const matchesRoute = group.items.some(
        (item) => location.pathname === item.path || location.pathname.startsWith(`${item.path}/`),
      );
      if (hasSingle || matchesRoute || group.restricted) next.add(group.title);
    });
    return next;
  };

  const [expandedGroups, setExpandedGroups] = useState<Set<string>>(() => ensureExpandedDefaults(allowedNavGroups));

  useEffect(() => {
    setExpandedGroups((prev) => {
      const next = new Set(prev);
      allowedNavGroups.forEach((group) => {
        const matchesRoute = group.items.some(
          (item) => location.pathname === item.path || location.pathname.startsWith(`${item.path}/`),
        );
        if (matchesRoute || group.items.length <= 1) {
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
      <Stack spacing={2} sx={{ px: 2.5, pt: 3, pb: 2, flexShrink: 0 }}>
        <Typography variant="caption" sx={{ color: 'text.disabled', letterSpacing: 2 }}>
          MENÚ
        </Typography>
        <TextField
          value={filter}
          onChange={(e) => setFilter(e.target.value)}
          inputRef={searchRef}
          onKeyDown={(event) => {
            if (flatFilteredItems.length === 0) return;
            if (event.key === 'ArrowDown') {
              event.preventDefault();
              setHighlightIndex((prev) => {
                const next = prev + 1;
                return next >= flatFilteredItems.length ? 0 : next;
              });
            } else if (event.key === 'ArrowUp') {
              event.preventDefault();
              setHighlightIndex((prev) => {
                if (prev <= 0) return flatFilteredItems.length - 1;
                return prev - 1;
              });
            } else if (event.key === 'Enter' && highlightIndex >= 0) {
              event.preventDefault();
              const target = flatFilteredItems[highlightIndex];
              if (target) {
                navigate(target.path);
                onNavigate?.();
              }
            } else if (event.key === 'Escape') {
              event.preventDefault();
              setFilter('');
              setHighlightIndex(-1);
              searchRef.current?.blur();
            }
          }}
          size="small"
          placeholder="Buscar sección (/)"
          fullWidth
          InputProps={{
            startAdornment: (
              <InputAdornment position="start">
                <SearchIcon sx={{ color: 'text.disabled', fontSize: 18 }} />
              </InputAdornment>
            ),
            endAdornment: filter ? (
              <InputAdornment position="end">
                <IconButton
                  size="small"
                  aria-label="Limpiar búsqueda"
                  onClick={() => setFilter('')}
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
      <List disablePadding sx={{ flex: 1, overflowY: 'auto', px: 1.5 }}>
        {!filter.trim() && shortcutItems.length > 0 && (
          <Box sx={{ pb: 1.5 }}>
            <Stack direction="row" alignItems="center" justifyContent="space-between" sx={{ px: 1.5, py: 1 }}>
              <Typography variant="caption" sx={{ color: 'text.disabled', letterSpacing: 1 }}>
                ATAJOS
              </Typography>
            </Stack>
            <List disablePadding>
              {shortcutItems.map((item, index) => {
                const active = location.pathname === item.path || location.pathname.startsWith(`${item.path}/`);
                const unreadBadge = item.path === '/chat' ? chatUnreadCount : 0;
                return (
                  <ListItemButton
                    key={`shortcut-${item.path}`}
                    component={RouterLink}
                    to={item.path}
                    onClick={() => {
                      registerRecentPath(item.path);
                      onNavigate?.();
                    }}
                    selected={active}
                    aria-current={active ? 'page' : undefined}
                    sx={{
                      borderRadius: 1.5,
                      mb: index === shortcutItems.length - 1 ? 0 : 0.5,
                      bgcolor: active ? 'action.selected' : 'transparent',
                      color: active ? 'primary.main' : 'text.primary',
                      '&:hover': { bgcolor: 'action.hover' },
                    }}
                  >
                    <FiberManualRecordIcon sx={{ fontSize: 8, mr: 1.5, color: active ? 'primary.main' : 'text.disabled' }} />
                    <ListItemText
                      primary={unreadBadge > 0 ? (
                        <Badge
                          color="error"
                          badgeContent={unreadBadge > 99 ? '99+' : unreadBadge}
                          sx={{ '& .MuiBadge-badge': { fontSize: 11, height: 18, minWidth: 18 } }}
                        >
                          <span>{item.label}</span>
                        </Badge>
                      ) : item.label}
                      secondary={recentPathSet.has(item.path) ? `Reciente · ${item.group}` : item.group}
                      primaryTypographyProps={{ fontSize: 13, fontWeight: 600 }}
                      secondaryTypographyProps={{ fontSize: 11, color: 'text.secondary' }}
                    />
                  </ListItemButton>
                );
              })}
            </List>
          </Box>
        )}
        {filteredNavGroups.length === 0 && (
          <Typography variant="body2" sx={{ px: 2, py: 1.5, color: 'text.secondary' }}>
            Sin coincidencias.
          </Typography>
        )}
        {filteredNavGroups.map((group, groupIdx) => {
          const isSearching = filter.trim().length > 0;
          const isExpanded = isSearching || expandedGroups.has(group.title) || group.restricted;
          return (
            <Box key={group.title}>
              <Stack direction="row" alignItems="center" justifyContent="space-between" sx={{ px: 1.5, py: 1 }}>
                <Typography variant="caption" sx={{ color: 'text.disabled', letterSpacing: 1 }}>
                  {group.title}
                </Typography>
                {group.items.length > 1 && (
                  <IconButton
                    size="small"
                    aria-label={`Mostrar u ocultar ${group.title}`}
                    aria-expanded={isExpanded}
                    onClick={() => toggleGroup(group.title)}
                    sx={{ color: 'text.secondary' }}
                  >
                    {isExpanded ? <ExpandLessIcon fontSize="small" /> : <ExpandMoreIcon fontSize="small" />}
                  </IconButton>
                )}
              </Stack>
              <Collapse in={isExpanded} timeout="auto" unmountOnExit>
                <List disablePadding>
                  {group.items.map((item, itemIdx) => {
                    const flatIndex = flatFilteredItems.findIndex((flat) => flat.path === item.path);
                    const active = location.pathname === item.path || location.pathname.startsWith(`${item.path}/`);
                    const hot = flatIndex === highlightIndex;
                    const unreadBadge = item.path === '/chat' ? chatUnreadCount : 0;
                    const renderLabel = () => {
                      const q = filter.trim();
                      if (!q) return item.label;
                      const idx = item.label.toLowerCase().indexOf(q.toLowerCase());
                      if (idx === -1) return item.label;
                      const before = item.label.slice(0, idx);
                      const match = item.label.slice(idx, idx + q.length);
                      const after = item.label.slice(idx + q.length);
                      return (
                        <span>
                          {before}
                          <Box component="span" sx={{ color: 'primary.main', fontWeight: 700 }}>{match}</Box>
                          {after}
                        </span>
                      );
                    };
                    return (
                      <ListItemButton
                        key={`${groupIdx}-${itemIdx}-${item.path}`}
                        component={RouterLink}
                        to={item.path}
                        onClick={() => {
                          registerRecentPath(item.path);
                          onNavigate?.();
                        }}
                        selected={hot || (!filter.trim() && active)}
                        aria-current={active ? 'page' : undefined}
                        sx={{
                          borderRadius: 1.5,
                          mb: 0.5,
                          color: active ? 'primary.main' : 'text.primary',
                          bgcolor: hot ? 'action.hover' : active ? 'action.selected' : 'transparent',
                          '&:hover': { bgcolor: 'action.hover' },
                        }}
                      >
                        <FiberManualRecordIcon sx={{ fontSize: 8, mr: 1.5, color: active ? 'primary.main' : 'text.disabled' }} />
                        <ListItemText
                          primaryTypographyProps={{ fontSize: 13 }}
                          secondaryTypographyProps={{ fontSize: 11, color: 'text.secondary' }}
                          primary={(
                            <Stack direction="row" alignItems="center" justifyContent="space-between" spacing={1}>
                              <Box component="span" sx={{ minWidth: 0, overflow: 'hidden', textOverflow: 'ellipsis' }}>
                                {unreadBadge > 0 ? (
                                  <Badge
                                    color="error"
                                    badgeContent={unreadBadge > 99 ? '99+' : unreadBadge}
                                    sx={{
                                      '& .MuiBadge-badge': {
                                        fontSize: 11,
                                        height: 18,
                                        minWidth: 18,
                                      },
                                    }}
                                  >
                                    <span>{renderLabel()}</span>
                                  </Badge>
                                ) : (
                                  renderLabel()
                                )}
                              </Box>
                            </Stack>
                          )}
                          secondary={filter.trim() ? group.title : undefined}
                        />
                      </ListItemButton>
                    );
                  })}
                  {group.items.length === 0 && group.restricted && (
                    <Stack spacing={1} sx={{ px: 2, py: 1.5 }}>
                      <Typography variant="body2" sx={{ color: 'text.secondary' }}>
                        No tienes acceso a esta sección.
                      </Typography>
                      <Button
                        size="small"
                        variant="outlined"
                        sx={{ alignSelf: 'flex-start' }}
                        href={buildAccessMailto(group)}
                      >
                        Solicitar acceso
                      </Button>
                    </Stack>
                  )}
                </List>
              </Collapse>
            </Box>
          );
        })}
      </List>
    </Box>
  );
}
