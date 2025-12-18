import { useEffect, useMemo, useRef, useState, useCallback } from 'react';
import { Box, Button, Collapse, IconButton, List, ListItemButton, ListItemText, Stack, Typography, TextField, InputAdornment } from '@mui/material';
import ExpandLessIcon from '@mui/icons-material/ExpandLess';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import FiberManualRecordIcon from '@mui/icons-material/FiberManualRecord';
import SearchIcon from '@mui/icons-material/Search';
import ClearIcon from '@mui/icons-material/Clear';
import { Link as RouterLink, useLocation } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import { useSession } from '../session/SessionContext';

export interface NavItem {
  label: string;
  path: string;
}

export interface NavGroup {
  title: string;
  items: NavItem[];
}

type NavGroupView = NavGroup & { restricted?: boolean; requiredModule?: string | null };

export const NAV_GROUPS: NavGroup[] = [
  {
    title: 'PÚBLICO',
    items: [
      { label: 'Inicio', path: '/inicio' },
      { label: 'Marketplace', path: '/marketplace' },
      { label: 'Fan Hub', path: '/fans' },
      { label: 'Records', path: '/records' },
    ],
  },
  {
    title: 'CRM',
    items: [
      { label: 'Conexiones', path: '/social' },
      { label: 'Eventos sociales', path: '/social/eventos' },
      { label: 'Contactos', path: '/crm/contactos' },
      { label: 'Empresas', path: '/crm/empresas' },
      { label: 'Leads', path: '/crm/leads' },
    ],
  },
  {
    title: 'ESTUDIO',
    items: [
      { label: 'Calendario', path: '/estudio/calendario' },
      { label: 'Salas y recursos', path: '/estudio/salas' },
      { label: 'Órdenes', path: '/estudio/ordenes' },
      { label: 'Servicios', path: '/estudio/servicios' },
      { label: 'Pipelines', path: '/estudio/pipelines' },
      { label: 'Live Sessions', path: '/estudio/live-sessions' },
      { label: 'Reportes', path: '/estudio/reportes' },
    ],
  },
  {
    title: 'ESCUELA',
    items: [
      { label: 'Portal del profesor', path: '/mi-profesor' },
      { label: 'Profesores', path: '/escuela/profesores' },
      { label: 'Clases', path: '/escuela/clases' },
      { label: 'Trial lessons', path: '/escuela/trial-lessons' },
      { label: 'Trial queue', path: '/escuela/trial-queue' },
    ],
  },
  {
    title: 'LABEL',
    items: [
      { label: 'Artistas', path: '/label/artistas' },
      { label: 'Proyectos', path: '/label/proyectos' },
      { label: 'Releases', path: '/label/releases' },
      { label: 'Assets', path: '/label/assets' },
      { label: 'Tracks', path: '/label/tracks' },
    ],
  },
  {
    title: 'OPERACIÓN',
    items: [
      { label: 'Inventario', path: '/operacion/inventario' },
      { label: 'Órdenes marketplace', path: '/operacion/ordenes-marketplace' },
      { label: 'Reservas equipo', path: '/operacion/reservas-equipo' },
    ],
  },
  {
    title: 'CONFIGURACIÓN',
    items: [
      { label: 'Inscripciones curso', path: '/configuracion/inscripciones-curso' },
      { label: 'Cursos', path: '/configuracion/cursos' },
      { label: 'Logs', path: '/configuracion/logs' },
      { label: 'Estado sistema', path: '/configuracion/estado' },
      { label: 'Usuarios admin', path: '/configuracion/usuarios-admin' },
      { label: 'Roles y permisos', path: '/configuracion/roles-permisos' },
      { label: 'Calendario Google', path: '/configuracion/integraciones/calendario' },
      { label: 'CMS', path: '/configuracion/cms' },
      { label: 'Opciones UX', path: '/configuracion/opciones-ux' },
      { label: 'Preferencias', path: '/configuracion/preferencias' },
    ],
  },
  {
    title: 'FINANZAS',
    items: [
      { label: 'Pagos', path: '/finanzas/pagos' },
    ],
  },
  {
    title: 'RECURSOS',
    items: [
      { label: 'Manual', path: '/manual' },
      { label: 'Acerca de', path: '/acerca' },
      { label: 'Seguridad', path: '/seguridad' },
      { label: 'Sugerencias', path: '/feedback' },
      { label: 'Tidal Agent', path: '/herramientas/tidal-agent' },
      { label: 'Creador musical', path: '/herramientas/creador-musical' },
    ],
  },
];

interface SidebarNavProps {
  open: boolean;
  onNavigate?: () => void;
}

export const deriveModulesFromRoles = (roles: string[] | undefined): string[] => {
  if (!roles || roles.length === 0) return [];
  const lowerRoles = roles.map((r) => r.toLowerCase());
  const moduleSet = new Set<string>();
  lowerRoles.forEach((role) => {
    if (role.includes('admin')) {
      moduleSet.add('admin');
      moduleSet.add('crm');
      moduleSet.add('scheduling');
      moduleSet.add('school');
      moduleSet.add('invoicing');
      moduleSet.add('packages');
      moduleSet.add('ops');
      moduleSet.add('label');
    } else if (role.includes('manager')) {
      moduleSet.add('crm');
      moduleSet.add('scheduling');
      moduleSet.add('school');
      moduleSet.add('invoicing');
      moduleSet.add('packages');
      moduleSet.add('ops');
    } else if (role.includes('reception')) {
      moduleSet.add('crm');
      moduleSet.add('scheduling');
      moduleSet.add('school');
    } else if (role.includes('accounting')) {
      moduleSet.add('invoicing');
    } else if (role.includes('engineer') || role.includes('scheduling')) {
      moduleSet.add('scheduling');
    } else if (role.includes('teacher')) {
      moduleSet.add('scheduling');
      moduleSet.add('school');
    } else if (role.includes('packages') || role.includes('package')) {
      moduleSet.add('packages');
    } else if (role.includes('maintenance')) {
      moduleSet.add('packages');
      moduleSet.add('scheduling');
      moduleSet.add('ops');
    } else if (role.includes('label')) {
      moduleSet.add('label');
    } else if (role.includes('inventory') || role.includes('operacion') || role.includes('operation') || role.includes('ops')) {
      moduleSet.add('ops');
    } else if (role.includes('finance') || role.includes('billing')) {
      moduleSet.add('invoicing');
    }
  });
  return Array.from(moduleSet);
};

export const pathRequiresModule = (path: string): string | null => {
  if (path.startsWith('/crm')) return 'crm';
  if (path.startsWith('/social')) return 'crm';
  if (path.startsWith('/estudio')) return 'scheduling';
  if (path.startsWith('/finanzas')) return 'invoicing';
  if (path.startsWith('/configuracion')) return 'admin';
  if (path.startsWith('/operacion')) return 'ops';
  if (path.startsWith('/label')) return 'label';
  if (path.startsWith('/escuela')) return 'school';
  if (path.startsWith('/mi-profesor')) return 'school';
  if (path.startsWith('/eventos')) return 'scheduling';
  return null;
};

export default function SidebarNav({ open, onNavigate }: SidebarNavProps) {
  const location = useLocation();
  const { session } = useSession();
  const navigate = useNavigate();
  const [filter, setFilter] = useState('');
  const [highlightIndex, setHighlightIndex] = useState<number>(-1);
  const searchRef = useRef<HTMLInputElement | null>(null);

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

  const modules = useMemo(() => {
    const provided = session?.modules ?? [];
    const fromRoles = deriveModulesFromRoles(session?.roles);
    const baseSet = new Set([...provided, ...fromRoles].map((m) => m.toLowerCase()));
    // Backward-compatible aliases so legacy "packages" access still unlocks Ops/Label.
    if (baseSet.has('packages')) {
      baseSet.add('ops');
      baseSet.add('label');
    }
    if (baseSet.has('ops')) {
      baseSet.add('packages');
    }
    return baseSet;
  }, [session?.modules, session?.roles]);

  const allowedNavGroups = useMemo<NavGroupView[]>(() => {
    return NAV_GROUPS.map((group) => {
      const requiredModule = group.items
        .map((item) => pathRequiresModule(item.path))
        .find((m) => m !== null && !modules.has(m));
      const filteredItems = group.items.filter((item) => {
        const required = pathRequiresModule(item.path);
        if (!required) return true;
        return modules.has(required);
      });
      const hadHidden = group.items.some((item) => {
        const required = pathRequiresModule(item.path);
        return required !== null && !modules.has(required);
      });
      const restricted = filteredItems.length === 0 && hadHidden;
      return { ...group, items: filteredItems, restricted, requiredModule };
    }).filter((group) => group.items.length > 0 || group.restricted);
  }, [modules]);

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

  // Keep active group expanded when route or available groups change.
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

  return (
    <Box
      component="aside"
      sx={{
        width: open ? 260 : 0,
        transition: 'width 0.3s ease',
        bgcolor: '#10131b',
        color: '#f8fafc',
        borderRight: '1px solid rgba(255,255,255,0.06)',
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
      <Stack spacing={2} sx={{ px: 3, pt: 4, pb: 3, flexShrink: 0 }}>
        <Typography variant="caption" sx={{ color: 'rgba(248,250,252,0.6)', letterSpacing: 2 }}>
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
            }
          }}
          size="small"
          placeholder="Buscar sección (/)"
          fullWidth
          InputProps={{
            startAdornment: (
              <InputAdornment position="start">
                <SearchIcon sx={{ color: 'rgba(248,250,252,0.55)' }} />
              </InputAdornment>
            ),
            endAdornment: filter ? (
              <InputAdornment position="end">
                <IconButton size="small" onClick={() => setFilter('')} sx={{ color: 'rgba(248,250,252,0.7)' }}>
                  <ClearIcon fontSize="small" />
                </IconButton>
              </InputAdornment>
            ) : null,
            sx: {
              '& input': { color: '#e2e8f0' },
              '& .MuiOutlinedInput-notchedOutline': { borderColor: 'rgba(255,255,255,0.08)' },
              '&:hover .MuiOutlinedInput-notchedOutline': { borderColor: 'rgba(255,255,255,0.16)' },
            },
          }}
        />
      </Stack>
      <List disablePadding sx={{ flex: 1, overflowY: 'auto', pr: 1 }}>
        {filteredNavGroups.length === 0 && (
          <Typography variant="body2" sx={{ px: 3, py: 1.5, color: 'rgba(248,250,252,0.6)' }}>
            Sin coincidencias.
          </Typography>
        )}
        {filteredNavGroups.map((group, groupIdx) => {
          const isSearching = filter.trim().length > 0;
          const isExpanded = isSearching || expandedGroups.has(group.title) || group.restricted;
          return (
            <Box key={group.title} sx={{ px: 1 }}>
              <Stack direction="row" alignItems="center" justifyContent="space-between" sx={{ px: 2, py: 1 }}>
                <Typography variant="caption" sx={{ color: 'rgba(248,250,252,0.55)', letterSpacing: 1 }}>
                  {group.title}
                </Typography>
                {group.items.length > 1 && (
                  <IconButton size="small" onClick={() => toggleGroup(group.title)} sx={{ color: 'rgba(248,250,252,0.6)' }}>
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
                          <span style={{ color: '#93c5fd', fontWeight: 700 }}>{match}</span>
                          {after}
                        </span>
                      );
                    };
                    return (
                      <ListItemButton
                        key={`${groupIdx}-${itemIdx}-${item.path}`}
                        component={RouterLink}
                        to={item.path}
                        onClick={onNavigate}
                        selected={hot}
                        sx={{
                          borderRadius: 2,
                          mx: 1.5,
                          mb: 0.5,
                          color: active ? '#ffffff' : 'rgba(248,250,252,0.6)',
                          bgcolor: hot
                            ? 'rgba(148, 163, 184, 0.12)'
                            : active
                              ? 'rgba(59,130,246,0.2)'
                              : 'transparent',
                          '&:hover': { bgcolor: 'rgba(255,255,255,0.05)' },
                        }}
                      >
                        <FiberManualRecordIcon sx={{ fontSize: 8, mr: 1.5 }} />
                        <ListItemText
                          primaryTypographyProps={{ fontSize: 14 }}
                          secondaryTypographyProps={{ fontSize: 11, color: 'rgba(226,232,240,0.75)' }}
                          primary={renderLabel()}
                          secondary={filter.trim() ? group.title : undefined}
                        />
                      </ListItemButton>
                    );
                  })}
                  {group.items.length === 0 && group.restricted && (
                  <Stack spacing={1} sx={{ px: 3, py: 1.5 }}>
                    <Typography variant="body2" sx={{ color: 'rgba(248,250,252,0.65)' }}>
                      No tienes acceso a esta sección.
                    </Typography>
                    <Button
                      size="small"
                      variant="outlined"
                      sx={{ color: '#cbd5e1', borderColor: 'rgba(248,250,252,0.3)', alignSelf: 'flex-start' }}
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
