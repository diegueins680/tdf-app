import { useMemo, useState } from 'react';
import { Box, Collapse, IconButton, List, ListItemButton, ListItemText, Stack, Typography } from '@mui/material';
import ExpandLessIcon from '@mui/icons-material/ExpandLess';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import FiberManualRecordIcon from '@mui/icons-material/FiberManualRecord';
import { Link as RouterLink, useLocation } from 'react-router-dom';
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
  { title: 'INICIO', items: [{ label: 'Inicio', path: '/inicio' }] },
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
      { label: 'Pipelines', path: '/estudio/pipelines' },
      { label: 'Live Sessions', path: '/estudio/live-sessions' },
      { label: 'Reportes', path: '/estudio/reportes' },
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
    title: 'EVENTOS',
    items: [
      { label: 'Agenda', path: '/eventos/agenda' },
      { label: 'Fechas y tours', path: '/eventos/fechas-y-tours' },
      { label: 'Venues', path: '/eventos/venues' },
      { label: 'Staff', path: '/eventos/staff' },
      { label: 'Presupuestos', path: '/eventos/presupuestos' },
      { label: 'Post-mortem', path: '/eventos/post-mortem' },
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
    title: 'FINANZAS',
    items: [
      { label: 'Cotizaciones', path: '/finanzas/cotizaciones' },
      { label: 'Facturas', path: '/finanzas/facturas' },
      { label: 'Cobros', path: '/finanzas/cobros' },
      { label: 'Recibos', path: '/finanzas/recibos' },
      { label: 'Regalías', path: '/finanzas/regalias' },
    ],
  },
  {
    title: 'BAR',
    items: [
      { label: 'Sell', path: '/bar/sell' },
      { label: 'Register', path: '/bar/register' },
      { label: 'Inventory', path: '/bar/inventory' },
      { label: 'Staff', path: '/bar/staff' },
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
      { label: 'Roles y permisos', path: '/configuracion/roles-permisos' },
      { label: 'Impuestos y series', path: '/configuracion/impuestos-series' },
      { label: 'Unidades de negocio', path: '/configuracion/unidades-negocio' },
      { label: 'Sedes', path: '/configuracion/sedes' },
      { label: 'Marcas', path: '/configuracion/marcas' },
      { label: 'Integraciones', path: '/configuracion/integraciones' },
      { label: 'Preferencias', path: '/configuracion/preferencias' },
    ],
  },
  { title: 'INSIGHTS', items: [{ label: 'Insights', path: '/insights' }] },
  {
    title: 'RECURSOS',
    items: [
      { label: 'Documentación', path: '/docs' },
      { label: 'Acerca de', path: '/acerca' },
      { label: 'Seguridad', path: '/seguridad' },
      { label: 'Fan Hub', path: '/fans' },
    ],
  },
];

interface SidebarNavProps {
  open: boolean;
  onNavigate?: () => void;
}

export default function SidebarNav({ open, onNavigate }: SidebarNavProps) {
  const location = useLocation();
  const [expandedGroups, setExpandedGroups] = useState<Set<string>>(() => new Set(NAV_GROUPS.map((g) => g.title)));
  const { session } = useSession();
  const modules = useMemo(() => new Set((session?.modules ?? []).map((m) => m.toLowerCase())), [session?.modules]);

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
    const modulesProvided = modules.size > 0;
    return NAV_GROUPS.map((group) => {
      const filteredItems = group.items.filter((item) => {
        const required = pathRequiresModule(item.path);
        if (!required) return true;
        if (!modulesProvided) return true; // keep legacy behavior when API doesn't send modules
        return modules.has(required);
      });
      return { ...group, items: filteredItems };
    }).filter((group) => group.items.length > 0);
  }, [modules]);

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
        overflow: 'hidden',
        display: { xs: open ? 'block' : 'none', lg: 'block' },
        position: { xs: 'fixed', lg: 'sticky' },
        zIndex: 1200,
        height: '100vh',
        maxHeight: '100vh',
        top: 0,
        left: 0,
        flexShrink: 0,
        display: 'flex',
        flexDirection: 'column',
      }}
    >
      <Stack spacing={2} sx={{ px: 3, pt: 4, pb: 3 }}>
        <Typography variant="caption" sx={{ color: 'rgba(248,250,252,0.6)', letterSpacing: 2 }}>
          MENÚ
        </Typography>
      </Stack>
      <List disablePadding sx={{ flex: 1, overflowY: 'auto', pr: 1 }}>
        {allowedNavGroups.map((group) => {
          const isExpanded = expandedGroups.has(group.title);
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
                  {group.items.map((item) => {
                    const active = location.pathname === item.path || location.pathname.startsWith(`${item.path}/`);
                    return (
                      <ListItemButton
                        key={item.path}
                        component={RouterLink}
                        to={item.path}
                        onClick={onNavigate}
                        sx={{
                          borderRadius: 2,
                          mx: 1.5,
                          mb: 0.5,
                          color: active ? '#ffffff' : 'rgba(248,250,252,0.6)',
                          bgcolor: active ? 'rgba(59,130,246,0.2)' : 'transparent',
                          '&:hover': { bgcolor: 'rgba(255,255,255,0.05)' },
                        }}
                      >
                        <FiberManualRecordIcon sx={{ fontSize: 8, mr: 1.5 }} />
                        <ListItemText primaryTypographyProps={{ fontSize: 14 }} primary={item.label} />
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
