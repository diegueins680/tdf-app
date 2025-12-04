import { useEffect, useMemo, useRef, useState } from 'react';
import { Box, Collapse, IconButton, List, ListItemButton, ListItemText, Stack, Typography, TextField, InputAdornment } from '@mui/material';
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
      { label: 'Profesores', path: '/escuela/profesores' },
      { label: 'Clases', path: '/escuela/clases' },
      { label: 'Trial lessons', path: '/escuela/trial-lessons' },
      { label: 'Trial queue', path: '/escuela/trial-queue' },
      { label: 'Programas', path: '/escuela/programas' },
      { label: 'Cursos', path: '/escuela/cursos' },
      { label: 'Cohortes', path: '/escuela/cohortes' },
      { label: 'Estudiantes', path: '/escuela/estudiantes' },
      { label: 'Inscripciones', path: '/escuela/inscripciones' },
      { label: 'Pagos', path: '/escuela/pagos' },
    ],
  },
  {
    title: 'LABEL',
    items: [
      { label: 'Artistas', path: '/label/artistas' },
      { label: 'Proyectos', path: '/label/proyectos' },
      { label: 'Releases', path: '/label/releases' },
      { label: 'Tracks', path: '/label/tracks' },
      { label: 'Assets', path: '/label/assets' },
      { label: 'Metadata', path: '/label/metadata' },
      { label: 'Contratos', path: '/label/contratos' },
      { label: 'Regalías', path: '/label/regalias' },
      { label: 'Marketing', path: '/label/marketing' },
    ],
  },
  {
    title: 'OPERACIÓN',
    items: [
      { label: 'Inventario', path: '/operacion/inventario' },
      { label: 'Calendario domo', path: '/operacion/calendario-domo' },
      { label: 'Reservas equipo', path: '/operacion/reservas-equipo' },
      { label: 'Mantenimiento', path: '/operacion/mantenimiento' },
      { label: 'Paquetes', path: '/operacion/paquetes' },
      { label: 'Paquetes / Resumen', path: '/operacion/paquetes/resumen' },
    ],
  },
  {
    title: 'CONFIGURACIÓN',
    items: [
      { label: 'Inscripciones curso', path: '/configuracion/inscripciones-curso' },
      { label: 'Logs', path: '/configuracion/logs' },
      { label: 'Estado sistema', path: '/configuracion/estado' },
      { label: 'Usuarios admin', path: '/configuracion/usuarios-admin' },
      { label: 'Roles y permisos', path: '/configuracion/roles-permisos' },
      { label: 'Impuestos y series', path: '/configuracion/impuestos-series' },
      { label: 'Unidades de negocio', path: '/configuracion/unidades-negocio' },
      { label: 'Sedes', path: '/configuracion/sedes' },
      { label: 'Marcas', path: '/configuracion/marcas' },
      { label: 'Integraciones', path: '/configuracion/integraciones' },
      { label: 'Calendario Google', path: '/configuracion/integraciones/calendario' },
      { label: 'CMS', path: '/configuracion/cms' },
      { label: 'Preferencias', path: '/configuracion/preferencias' },
    ],
  },
  {
    title: 'FINANZAS',
    items: [
      { label: 'Cotizaciones', path: '/finanzas/cotizaciones' },
      { label: 'Facturas', path: '/finanzas/facturas' },
      { label: 'Cobros', path: '/finanzas/cobros' },
      { label: 'Pagos', path: '/finanzas/pagos' },
      { label: 'Recibos', path: '/finanzas/recibos' },
      { label: 'Regalías', path: '/finanzas/regalias' },
    ],
  },
  {
    title: 'RECURSOS',
    items: [
      { label: 'Documentación', path: '/docs' },
      { label: 'Acerca de', path: '/acerca' },
      { label: 'Seguridad', path: '/seguridad' },
      { label: 'Sugerencias', path: '/feedback' },
      { label: 'Manual', path: '/manual' },
    ],
  },
];

interface SidebarNavProps {
  open: boolean;
  onNavigate?: () => void;
}

export default function SidebarNav({ open, onNavigate }: SidebarNavProps) {
  const location = useLocation();
  const { session } = useSession();
  const navigate = useNavigate();
  const [filter, setFilter] = useState('');
  const [highlightIndex, setHighlightIndex] = useState<number>(-1);
  const searchRef = useRef<HTMLInputElement | null>(null);
  const deriveModulesFromRoles = (roles: string[] | undefined): string[] => {
    if (!roles || roles.length === 0) return [];
    const lowerRoles = roles.map((r) => r.toLowerCase());
    const moduleSet = new Set<string>();
    lowerRoles.forEach((role) => {
      if (role.includes('admin')) {
        moduleSet.add('admin');
        moduleSet.add('crm');
        moduleSet.add('scheduling');
        moduleSet.add('invoicing');
        moduleSet.add('packages');
      } else if (role.includes('manager')) {
        moduleSet.add('crm');
        moduleSet.add('scheduling');
        moduleSet.add('invoicing');
        moduleSet.add('packages');
      } else if (role.includes('reception')) {
        moduleSet.add('crm');
        moduleSet.add('scheduling');
      } else if (role.includes('accounting')) {
        moduleSet.add('invoicing');
      } else if (role.includes('engineer') || role.includes('scheduling')) {
        moduleSet.add('scheduling');
      } else if (role.includes('packages') || role.includes('package')) {
        moduleSet.add('packages');
      }
    });
    return Array.from(moduleSet);
  };

  const modules = useMemo(() => {
    const provided = session?.modules ?? [];
    const fromRoles = deriveModulesFromRoles(session?.roles);
    return new Set([...provided, ...fromRoles].map((m) => m.toLowerCase()));
  }, [session?.modules, session?.roles]);

  const pathRequiresModule = (path: string): string | null => {
    if (path.startsWith('/crm')) return 'crm';
    if (path.startsWith('/estudio')) return 'scheduling';
    if (path.startsWith('/finanzas')) return 'invoicing';
    if (path.startsWith('/configuracion')) return 'admin';
    if (path.startsWith('/operacion')) return 'packages';
    if (path.startsWith('/label')) return 'packages';
    if (path.startsWith('/bar')) return 'packages';
    if (path.startsWith('/escuela')) return 'scheduling';
    if (path.startsWith('/eventos')) return 'scheduling';
    return null;
  };

  const allowedNavGroups = useMemo(() => {
    return NAV_GROUPS.map((group) => {
      const filteredItems = group.items.filter((item) => {
        const required = pathRequiresModule(item.path);
        if (!required) return true;
        return modules.has(required);
      });
      return { ...group, items: filteredItems };
    }).filter((group) => group.items.length > 0);
  }, [modules]);

  const filteredNavGroups = useMemo(() => {
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
      }))
      .filter((group) => group.items.length > 0);
  }, [allowedNavGroups, filter]);

  const flatFilteredItems = useMemo(
    () => filteredNavGroups.flatMap((group) => group.items),
    [filteredNavGroups],
  );

  const ensureExpandedDefaults = (groups: NavGroup[]) => {
    const next = new Set<string>();
    groups.forEach((group) => {
      const hasSingle = group.items.length <= 1;
      const matchesRoute = group.items.some(
        (item) => location.pathname === item.path || location.pathname.startsWith(`${item.path}/`),
      );
      if (hasSingle || matchesRoute) next.add(group.title);
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
      if (event.key === '/' || (event.key.toLowerCase() === 'k' && (event.metaKey || event.ctrlKey))) {
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
          placeholder="Buscar sección"
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
          const isExpanded = isSearching || expandedGroups.has(group.title);
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
                </List>
              </Collapse>
            </Box>
          );
        })}
      </List>
    </Box>
  );
}
