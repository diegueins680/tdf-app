import MenuIcon from '@mui/icons-material/Menu';
import MenuOpenIcon from '@mui/icons-material/MenuOpen';
import ShoppingCartOutlinedIcon from '@mui/icons-material/ShoppingCartOutlined';
import StarIcon from '@mui/icons-material/Star';
import StarBorderIcon from '@mui/icons-material/StarBorder';
import { useEffect, useMemo, useState, useCallback, useRef } from 'react';
import { AppBar, Box, Button, IconButton, Stack, Toolbar, Badge, Typography, Popover, Divider, Tooltip, Dialog, DialogTitle, DialogContent, TextField, InputAdornment, List, ListItemButton, ListItemText } from '@mui/material';
import Menu from '@mui/material/Menu';
import MenuItem from '@mui/material/MenuItem';
import { Link as RouterLink, useLocation, useNavigate } from 'react-router-dom';
import SessionMenu from './SessionMenu';
import { useSession } from '../session/SessionContext';
import BrandLogo from './BrandLogo';
import SearchIcon from '@mui/icons-material/Search';
import { NAV_GROUPS, deriveModulesFromRoles, isSchoolStaffRole, pathRequiresModule, pathRequiresSchoolStaff } from './SidebarNav';

interface TopBarProps {
  onToggleSidebar?: () => void;
  sidebarOpen?: boolean;
}

const CART_META_KEY = 'tdf-marketplace-cart-meta';
const CART_EVENT = 'tdf-cart-updated';
const QUICK_FAVORITES_KEY = 'tdf-quick-nav-favorites';
const QUICK_RECENTS_KEY = 'tdf-quick-nav-recents';
const MAX_QUICK_RECENTS = 10;

const FRIENDLY_SEGMENTS: Record<string, string> = {
  inicio: 'Inicio',
  marketplace: 'Tienda',
  fans: 'Comunidad',
  records: 'Lanzamientos',
  'mi-profesor': 'Portal del profesor',
  crm: 'CRM',
  contactos: 'Contactos',
  empresas: 'Empresas',
  leads: 'Leads',
  estudio: 'Estudio',
  salas: 'Salas',
  ordenes: 'Órdenes',
  servicios: 'Servicios',
  pipelines: 'Pipelines',
  'live-sessions': 'Sesiones en vivo',
  reportes: 'Reportes',
  escuela: 'Escuela',
  profesores: 'Profesores',
  clases: 'Clases',
  'trial-lessons': 'Clases de prueba',
  'trial-queue': 'Solicitudes de prueba',
  label: 'Label',
  artistas: 'Artistas',
  proyectos: 'Proyectos',
  releases: 'Lanzamientos',
  tracks: 'Pistas',
  assets: 'Activos',
  operacion: 'Operación',
  inventario: 'Inventario',
  'ordenes-marketplace': 'Órdenes tienda',
  'calendario-domo': 'Calendario domo',
  'reservas-equipo': 'Reservas equipo',
  mantenimiento: 'Mantenimiento',
  paquetes: 'Paquetes',
  configuracion: 'Configuración',
  'inscripciones-curso': 'Inscripciones curso',
  cursos: 'Cursos',
  logs: 'Logs',
  estado: 'Estado sistema',
  'usuarios-admin': 'Usuarios admin',
  'roles-permisos': 'Roles y permisos',
  diagnosticos: 'Diagnósticos',
  'impuestos-series': 'Impuestos y series',
  'unidades-negocio': 'Unidades de negocio',
  sedes: 'Sedes',
  marcas: 'Marcas',
  integraciones: 'Integraciones',
  calendario: 'Calendario Google',
  cms: 'CMS',
  preferencias: 'Preferencias',
  finanzas: 'Finanzas',
  cotizaciones: 'Cotizaciones',
  facturas: 'Facturas',
  cobros: 'Cobros',
  pagos: 'Pagos',
  recibos: 'Recibos',
  regalias: 'Regalías',
  docs: 'Documentación',
  acerca: 'Acerca de',
  manual: 'Manual',
  seguridad: 'Seguridad',
  feedback: 'Sugerencias',
  herramientas: 'Herramientas',
  'tidal-agent': 'Agente Tidal',
  'creador-musical': 'Creador musical',
  'token-admin': 'Token API',
};

const formatFriendlyPath = (path: string) => {
  const parts = path.split('/').filter(Boolean);
  if (parts.length === 0) return 'Inicio';
  return parts
    .map((part) => FRIENDLY_SEGMENTS[part] ?? part.replace(/-/g, ' '))
    .map((part) => (part.length > 0 ? part.charAt(0).toUpperCase() + part.slice(1) : part))
    .join(' / ');
};

interface CartPreviewItem {
  title: string;
  subtotal: string;
}

const sanitizeCartCount = (raw: unknown): number => {
  if (typeof raw !== 'number') return 0;
  if (!Number.isFinite(raw)) return 0;
  const floored = Math.floor(raw);
  return floored > 0 ? floored : 0;
};

const sanitizeCartPreview = (raw: unknown): CartPreviewItem[] => {
  if (!Array.isArray(raw)) return [];
  return raw
    .map((item) => {
      if (!item || typeof item !== 'object') return null;
      const title = typeof (item as { title?: unknown }).title === 'string' ? (item as { title: string }).title : '';
      const subtotal =
        typeof (item as { subtotal?: unknown }).subtotal === 'string'
          ? (item as { subtotal: string }).subtotal
          : '';
      if (!title.trim() || !subtotal.trim()) return null;
      return { title, subtotal };
    })
    .filter((item): item is CartPreviewItem => item != null)
    .slice(0, 5);
};

const readCartMeta = (): { cartId: string; count: number; preview: CartPreviewItem[] } => {
  try {
    const raw = localStorage.getItem(CART_META_KEY);
    if (!raw) return { cartId: '', count: 0, preview: [] };
    const parsed = JSON.parse(raw) as Partial<{ cartId: string; count: number; preview: CartPreviewItem[] }>;
    return {
      cartId: typeof parsed?.cartId === 'string' ? parsed.cartId : '',
      count: sanitizeCartCount(parsed?.count),
      preview: sanitizeCartPreview(parsed?.preview),
    };
  } catch {
    return { cartId: '', count: 0, preview: [] };
  }
};

const readStoredStringList = (storageKey: string): string[] => {
  try {
    const raw = localStorage.getItem(storageKey);
    if (!raw) return [];
    const parsed = JSON.parse(raw) as unknown;
    if (!Array.isArray(parsed)) return [];
    return parsed.filter((value): value is string => typeof value === 'string' && value.trim().length > 0);
  } catch {
    return [];
  }
};

export default function TopBar({ onToggleSidebar, sidebarOpen = true }: TopBarProps) {
  const { session, logout } = useSession();
  const navigate = useNavigate();
  const location = useLocation();
  const [quickNavOpen, setQuickNavOpen] = useState(false);
  const [quickQuery, setQuickQuery] = useState('');
  const [quickHighlight, setQuickHighlight] = useState(0);
  const [quickFavorites, setQuickFavorites] = useState<string[]>([]);
  const [quickRecents, setQuickRecents] = useState<string[]>([]);
  const [cartCount, setCartCount] = useState(0);
  const [cartPreview, setCartPreview] = useState<{ title: string; subtotal: string }[]>([]);
  const [cartAnchor, setCartAnchor] = useState<HTMLElement | null>(null);
  const [resourcesAnchor, setResourcesAnchor] = useState<null | HTMLElement>(null);
  const quickInputRef = useRef<HTMLInputElement | null>(null);
  const resourcesButtonRef = useRef<HTMLButtonElement | null>(null);

  useEffect(() => {
    setQuickFavorites(readStoredStringList(QUICK_FAVORITES_KEY));
    setQuickRecents(readStoredStringList(QUICK_RECENTS_KEY));
  }, []);

  useEffect(() => {
    try {
      localStorage.setItem(QUICK_FAVORITES_KEY, JSON.stringify(quickFavorites));
    } catch {
      // ignore persistence issues
    }
  }, [quickFavorites]);

  useEffect(() => {
    try {
      localStorage.setItem(QUICK_RECENTS_KEY, JSON.stringify(quickRecents));
    } catch {
      // ignore persistence issues
    }
  }, [quickRecents]);

  useEffect(() => {
    const meta = readCartMeta();
    setCartCount(meta.count);
    setCartPreview(meta.preview);
    const handler = () => {
      const next = readCartMeta();
      setCartCount(next.count);
      setCartPreview(next.preview);
    };
    window.addEventListener('storage', handler);
    window.addEventListener(CART_EVENT, handler as EventListener);
    return () => {
      window.removeEventListener('storage', handler);
      window.removeEventListener(CART_EVENT, handler as EventListener);
    };
  }, []);

  const handleLogout = () => {
    logout();
    navigate('/login', { replace: true });
  };

  const modules = useMemo(() => {
    const provided = session?.modules ?? [];
    const fromRoles = deriveModulesFromRoles(session?.roles);
    const baseSet = new Set([...provided, ...fromRoles].map((m) => m.toLowerCase()));
    if (baseSet.has('packages')) {
      baseSet.add('ops');
      baseSet.add('label');
    }
    if (baseSet.has('ops')) {
      baseSet.add('packages');
    }
    return baseSet;
  }, [session?.modules, session?.roles]);

  const hasAdmin = modules.has('admin');
  const isSchoolStaff = useMemo(() => isSchoolStaffRole(session?.roles), [session?.roles]);

  const quickNavItems = useMemo(() => {
    return NAV_GROUPS.flatMap((group) =>
      group.items
        .filter((item) => {
          if (pathRequiresSchoolStaff(item.path) && !isSchoolStaff) return false;
          const required = pathRequiresModule(item.path);
          if (!required) return true;
          return modules.has(required);
        })
        .map((item) => ({ ...item, group: group.title })),
    );
  }, [isSchoolStaff, modules]);

  const favoritePaths = useMemo(() => new Set(quickFavorites), [quickFavorites]);
  const recentPathIndex = useMemo(
    () => new Map(quickRecents.map((path, index) => [path, index])),
    [quickRecents],
  );

  const filteredQuickItems = useMemo(() => {
    const query = quickQuery.trim().toLowerCase();
    const rankItem = (item: { label: string; path: string }) => {
      const label = item.label.toLowerCase();
      let score = 0;
      if (favoritePaths.has(item.path)) score += 60;
      const recentIndex = recentPathIndex.get(item.path);
      if (recentIndex !== undefined) {
        score += Math.max(0, 25 - recentIndex);
      }
      if (location.pathname === item.path) score += 80;
      if (query) {
        if (label.startsWith(query)) score += 45;
        else if (label.includes(query)) score += 25;
        if (item.path.toLowerCase().includes(query)) score += 10;
      }
      return score;
    };
    const filtered = query
      ? quickNavItems.filter(
          (item) => item.label.toLowerCase().includes(query) || item.path.toLowerCase().includes(query),
        )
      : quickNavItems;
    return [...filtered].sort((a, b) => rankItem(b) - rankItem(a) || a.label.localeCompare(b.label));
  }, [favoritePaths, location.pathname, quickNavItems, quickQuery, recentPathIndex]);

  const registerQuickRecent = useCallback((path: string) => {
    setQuickRecents((prev) => [path, ...prev.filter((existing) => existing !== path)].slice(0, MAX_QUICK_RECENTS));
  }, []);

  const toggleQuickFavorite = useCallback((path: string) => {
    setQuickFavorites((prev) => (prev.includes(path) ? prev.filter((existing) => existing !== path) : [path, ...prev]));
  }, []);

  const openQuickNav = useCallback(() => {
    setQuickNavOpen(true);
    setQuickQuery('');
    setQuickHighlight(0);
  }, []);

  const closeQuickNav = () => {
    setQuickNavOpen(false);
    setQuickQuery('');
  };

  useEffect(() => {
    const handler = (event: KeyboardEvent) => {
      const tag = (event.target as HTMLElement | null)?.tagName?.toLowerCase();
      if (tag === 'input' || tag === 'textarea' || (event.target as HTMLElement | null)?.isContentEditable) return;
      if ((event.metaKey || event.ctrlKey) && event.key.toLowerCase() === 'k') {
        event.preventDefault();
        openQuickNav();
      } else if (event.altKey && event.key.toLowerCase() === 'r') {
        if (typeof window !== 'undefined' && window.innerWidth < 900) return;
        event.preventDefault();
        if (!resourcesButtonRef.current) return;
        setResourcesAnchor((prev) => (prev ? null : resourcesButtonRef.current));
      }
    };
    window.addEventListener('keydown', handler);
    return () => window.removeEventListener('keydown', handler);
  }, [openQuickNav]);

  useEffect(() => {
    setQuickHighlight(0);
  }, [quickQuery]);

  useEffect(() => {
    setResourcesAnchor(null);
    setCartAnchor(null);
  }, [location.pathname]);

  useEffect(() => {
    if (quickNavOpen) {
      setTimeout(() => quickInputRef.current?.focus(), 0);
    }
  }, [quickNavOpen]);

  const handleSelectQuick = (idx: number) => {
    const target = filteredQuickItems[idx];
    if (!target) return;
    registerQuickRecent(target.path);
    navigate(target.path);
    closeQuickNav();
  };

  const showQuickPathHints = quickQuery.trim().includes('/');
  const currentSectionLabel = useMemo(() => formatFriendlyPath(location.pathname), [location.pathname]);
  const contextualDefaultPaths = useMemo(() => {
    if (location.pathname.startsWith('/crm') || location.pathname.startsWith('/social')) {
      return ['/crm/contactos', '/crm/leads', '/social/inbox'];
    }
    if (location.pathname.startsWith('/estudio')) {
      return ['/estudio/calendario', '/estudio/ordenes', '/estudio/salas'];
    }
    if (location.pathname.startsWith('/escuela') || location.pathname.startsWith('/mi-profesor')) {
      return ['/escuela/clases', '/escuela/profesores', '/mi-profesor'];
    }
    if (location.pathname.startsWith('/label')) {
      return ['/label/artistas', '/label/releases', '/label/assets'];
    }
    if (location.pathname.startsWith('/operacion')) {
      return ['/operacion/inventario', '/operacion/ordenes-marketplace', '/operacion/reservas-equipo'];
    }
    if (location.pathname.startsWith('/configuracion') || location.pathname.startsWith('/admin')) {
      return ['/configuracion/estado', '/configuracion/cms', '/configuracion/preferencias'];
    }
    if (location.pathname.startsWith('/finanzas')) {
      return ['/finanzas/pagos'];
    }
    return ['/crm/contactos', '/label/releases', '/estudio/calendario', '/operacion/inventario'];
  }, [location.pathname]);
  const shortcutStripItems = useMemo(() => {
    const preferredPaths = [...contextualDefaultPaths, ...quickFavorites, ...quickRecents];
    const seen = new Set<string>();
    return preferredPaths
      .filter((path) => {
        if (!path || path === location.pathname) return false;
        if (seen.has(path)) return false;
        seen.add(path);
        return true;
      })
      .map((path) => quickNavItems.find((item) => item.path === path))
      .filter((item): item is (typeof quickNavItems)[number] => Boolean(item))
      .slice(0, 5);
  }, [contextualDefaultPaths, location.pathname, quickFavorites, quickNavItems, quickRecents]);

  const handleOpenCart = (event: React.MouseEvent<HTMLElement>) => {
    setCartAnchor(event.currentTarget);
  };
  const handleCloseCart = () => setCartAnchor(null);
  const cartOpen = Boolean(cartAnchor);
  const resourcesOpen = Boolean(resourcesAnchor);

  return (
    <AppBar
      elevation={0}
      position="sticky"
      sx={{
        bgcolor: '#0f1118',
        borderBottom: '1px solid rgba(255,255,255,0.08)',
      }}
    >
      <Toolbar
        sx={{
          minHeight: { xs: 80, md: 92 },
          px: { xs: 2, md: 4 },
        }}
      >
        <Tooltip title={sidebarOpen ? 'Ocultar menú' : 'Mostrar menú'}>
          <IconButton
            edge="start"
            onClick={onToggleSidebar}
            sx={{ color: '#f8fafc', mr: 1 }}
            aria-label={sidebarOpen ? 'Ocultar menú lateral' : 'Mostrar menú lateral'}
          >
            {sidebarOpen ? <MenuOpenIcon /> : <MenuIcon />}
          </IconButton>
        </Tooltip>
        <Box
          component={RouterLink}
          to="/inicio"
          sx={{
            display: 'inline-flex',
            alignItems: 'center',
            flexGrow: 1,
            mr: 1.5,
          }}
          aria-label="Ir al inicio"
        >
          <BrandLogo
            variant="wordmark"
            size={55}
            sx={{
              height: { xs: 32, sm: 44, md: 54 },
              filter: 'brightness(0) invert(1) drop-shadow(0 10px 26px rgba(0,0,0,0.45))',
            }}
          />
        </Box>

        <Stack
          direction="row"
          spacing={1}
          alignItems="center"
          sx={{ ml: 'auto' }}
        >
          <Tooltip title="Cmd/Ctrl + K para saltar a una sección">
            <Button
              color="inherit"
              variant="outlined"
              onClick={openQuickNav}
              startIcon={<SearchIcon fontSize="small" />}
              sx={{
                textTransform: 'none',
                borderColor: 'rgba(148,163,184,0.4)',
                minWidth: { xs: 40, sm: 'auto' },
                px: { xs: 1.25, sm: 1.75 },
              }}
              aria-keyshortcuts="Control+K Meta+K"
              aria-label="Buscar sección"
            >
              <Box component="span" sx={{ display: { xs: 'none', sm: 'inline' } }}>Buscar sección</Box>
            </Button>
          </Tooltip>
          <Tooltip title="Alt+R abre recursos">
            <Button
              color="inherit"
              onClick={(e) => setResourcesAnchor(e.currentTarget)}
              sx={{ textTransform: 'none', display: { xs: 'none', md: 'inline-flex' } }}
              aria-haspopup="true"
              aria-expanded={resourcesOpen ? 'true' : undefined}
              aria-label="Abrir recursos"
              aria-keyshortcuts="Alt+R"
              ref={resourcesButtonRef}
            >
              Recursos
            </Button>
          </Tooltip>
          <Button
            color="inherit"
            component={RouterLink}
            to="/marketplace"
            sx={{ textTransform: 'none', position: 'relative', minWidth: { xs: 44, sm: 'auto' }, px: { xs: 1.25, sm: 2 } }}
            onMouseEnter={handleOpenCart}
            onClick={handleOpenCart}
            aria-label="Abrir carrito"
          >
            <Badge
              color="secondary"
              badgeContent={cartCount > 9 ? '9+' : cartCount}
              invisible={cartCount <= 0}
              sx={{ '& .MuiBadge-badge': { right: -6, top: 6 } }}
            >
              <Box component="span" sx={{ display: { xs: 'none', sm: 'inline' } }}>
                Carrito
              </Box>
              <Box component="span" sx={{ display: { xs: 'inline-flex', sm: 'none' }, alignItems: 'center' }}>
                <ShoppingCartOutlinedIcon fontSize="small" />
              </Box>
            </Badge>
          </Button>

          {hasAdmin && (
            <Button
              color="inherit"
              variant="outlined"
              size="small"
              component={RouterLink}
              to="/configuracion/roles-permisos"
              sx={{
                textTransform: 'none',
                borderColor: 'rgba(59,130,246,0.35)',
                color: '#93c5fd',
                '&:hover': { borderColor: 'rgba(59,130,246,0.6)', bgcolor: 'rgba(59,130,246,0.08)' },
                display: { xs: 'none', md: 'inline-flex' },
              }}
            >
              Panel admin
            </Button>
          )}
          {session ? (
            <>
              <Button
                variant="contained"
                color="secondary"
                onClick={handleLogout}
                sx={{ textTransform: 'none', borderRadius: 999, display: { xs: 'none', sm: 'inline-flex' } }}
              >
                Salir
              </Button>
              <SessionMenu />
            </>
          ) : (
            <Button color="inherit" component={RouterLink} to="/login">
              Ingresar
            </Button>
          )}
        </Stack>
      </Toolbar>
      <Box
        sx={{
          px: { xs: 2, md: 4 },
          pb: 1.25,
          pt: 0.25,
          borderTop: '1px solid rgba(255,255,255,0.06)',
          bgcolor: 'rgba(15,17,24,0.94)',
        }}
      >
        <Stack
          direction={{ xs: 'column', lg: 'row' }}
          spacing={1}
          justifyContent="space-between"
          alignItems={{ xs: 'flex-start', lg: 'center' }}
          useFlexGap
        >
          <Stack direction="row" spacing={1} alignItems="center" sx={{ minWidth: 0 }}>
            <Typography variant="caption" sx={{ color: '#94a3b8', textTransform: 'uppercase', letterSpacing: 0.35 }}>
              Ahora
            </Typography>
            <Typography variant="body2" sx={{ color: '#f8fafc', fontWeight: 700 }}>
              {currentSectionLabel}
            </Typography>
          </Stack>
          <Stack
            direction="row"
            spacing={1}
            useFlexGap
            sx={{
              width: '100%',
              maxWidth: { xs: '100%', lg: 'auto' },
              overflowX: { xs: 'auto', lg: 'visible' },
              pb: { xs: 0.25, lg: 0 },
              '&::-webkit-scrollbar': { display: 'none' },
            }}
          >
            {shortcutStripItems.map((item) => {
              const favorite = favoritePaths.has(item.path);
              return (
                <Button
                  key={item.path}
                  size="small"
                  component={RouterLink}
                  to={item.path}
                  onClick={() => registerQuickRecent(item.path)}
                  variant={favorite ? 'contained' : 'outlined'}
                  color={favorite ? 'secondary' : 'inherit'}
                  sx={{
                    textTransform: 'none',
                    whiteSpace: 'nowrap',
                    borderColor: favorite ? undefined : 'rgba(148,163,184,0.35)',
                  }}
                >
                  {item.label}
                </Button>
              );
            })}
            <Button
              size="small"
              color="inherit"
              variant="text"
              onClick={openQuickNav}
              sx={{ textTransform: 'none', whiteSpace: 'nowrap' }}
            >
              Más secciones
            </Button>
          </Stack>
        </Stack>
      </Box>
      <Popover
        open={cartOpen}
        anchorEl={cartAnchor}
        onClose={handleCloseCart}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
        transformOrigin={{ vertical: 'top', horizontal: 'right' }}
        PaperProps={{ sx: { mt: 1, minWidth: 240, p: 1 } }}
      >
        <Typography variant="subtitle2" sx={{ px: 1, py: 0.5, fontWeight: 700 }}>
          Carrito
        </Typography>
        <Divider />
        {cartPreview.length === 0 ? (
          <Typography variant="body2" color="text.secondary" sx={{ px: 1, py: 1 }}>
            Vacío. Explora el marketplace.
          </Typography>
        ) : (
          <Stack sx={{ px: 1, py: 1 }} spacing={0.5}>
            {cartPreview.map((item, idx) => (
              <Stack key={`${item.title}-${idx}`} direction="row" justifyContent="space-between">
                <Typography variant="body2" sx={{ maxWidth: 160, overflow: 'hidden', textOverflow: 'ellipsis' }}>
                  {item.title}
                </Typography>
                <Typography variant="body2" fontWeight={700}>
                  {item.subtotal}
                </Typography>
              </Stack>
            ))}
            <Stack spacing={0.5}>
              <Button
                size="small"
                variant="contained"
                component={RouterLink}
                to="/marketplace"
                onClick={handleCloseCart}
              >
                Ir al carrito
              </Button>
              <Button
                size="small"
                variant="outlined"
                component={RouterLink}
                to="/marketplace"
                onClick={() => {
                  handleCloseCart();
                  localStorage.setItem('tdf-marketplace-payment', 'card');
                }}
              >
                Pagar con tarjeta
              </Button>
            </Stack>
          </Stack>
        )}
      </Popover>
      <Dialog
        open={quickNavOpen}
        onClose={closeQuickNav}
        fullWidth
        maxWidth="sm"
      >
        <DialogTitle>Ir a otra sección</DialogTitle>
        <DialogContent>
          <TextField
            fullWidth
            placeholder="Escribe para buscar (ej: inventario, leads, marketplace)"
            value={quickQuery}
            onChange={(e) => setQuickQuery(e.target.value)}
            inputRef={quickInputRef}
            onKeyDown={(event) => {
              if (event.key === 'ArrowDown') {
                event.preventDefault();
                setQuickHighlight((prev) => (prev + 1) % Math.max(filteredQuickItems.length, 1));
              } else if (event.key === 'ArrowUp') {
                event.preventDefault();
                setQuickHighlight((prev) => {
                  if (filteredQuickItems.length === 0) return 0;
                  return prev <= 0 ? filteredQuickItems.length - 1 : prev - 1;
                });
              } else if (event.key === 'Enter') {
                event.preventDefault();
                handleSelectQuick(quickHighlight);
              } else if (event.key === 'Escape') {
                closeQuickNav();
              }
            }}
            InputProps={{
              startAdornment: (
                <InputAdornment position="start">
                  <SearchIcon fontSize="small" />
                </InputAdornment>
              ),
            }}
            sx={{ mb: 1.25 }}
          />
          {!quickQuery.trim() && (quickFavorites.length > 0 || quickRecents.length > 0) && (
            <Typography variant="caption" color="text.secondary" sx={{ display: 'block', mb: 1 }}>
              Mostrando primero tus favoritos y secciones recientes.
            </Typography>
          )}
          {filteredQuickItems.length === 0 ? (
            <Typography variant="body2" color="text.secondary">
              Sin coincidencias. Prueba con otra palabra clave.
            </Typography>
          ) : (
            <List dense>
              {filteredQuickItems.slice(0, 30).map((item, idx) => (
                <ListItemButton
                  key={`${item.path}-${idx}`}
                  selected={idx === quickHighlight}
                  onClick={() => handleSelectQuick(idx)}
                >
                  <ListItemText
                    primary={item.label}
                    secondary={`${item.group}${recentPathIndex.has(item.path) ? ' · Reciente' : ''}`}
                    primaryTypographyProps={{ fontWeight: idx === quickHighlight ? 700 : 500 }}
                  />
                  <Stack direction="row" spacing={0.25} alignItems="center">
                    {showQuickPathHints && (
                      <Typography variant="caption" color="text.secondary">
                        {item.path}
                      </Typography>
                    )}
                    <Tooltip title={favoritePaths.has(item.path) ? 'Quitar de favoritos' : 'Agregar a favoritos'}>
                      <IconButton
                        size="small"
                        onClick={(event) => {
                          event.preventDefault();
                          event.stopPropagation();
                          toggleQuickFavorite(item.path);
                        }}
                        aria-label={favoritePaths.has(item.path) ? 'Quitar favorito' : 'Marcar favorito'}
                      >
                        {favoritePaths.has(item.path) ? (
                          <StarIcon fontSize="small" color="warning" />
                        ) : (
                          <StarBorderIcon fontSize="small" />
                        )}
                      </IconButton>
                    </Tooltip>
                  </Stack>
                </ListItemButton>
              ))}
            </List>
          )}
        </DialogContent>
      </Dialog>
      <Menu
        anchorEl={resourcesAnchor}
        open={resourcesOpen}
        onClose={() => setResourcesAnchor(null)}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
        transformOrigin={{ vertical: 'top', horizontal: 'right' }}
        MenuListProps={{ autoFocusItem: resourcesOpen }}
      >
        <MenuItem component={RouterLink} to="/manual" onClick={() => setResourcesAnchor(null)}>
          Manual
        </MenuItem>
        <MenuItem component={RouterLink} to="/docs" onClick={() => setResourcesAnchor(null)}>
          Docs
        </MenuItem>
        <MenuItem component={RouterLink} to="/acerca" onClick={() => setResourcesAnchor(null)}>
          Acerca de
        </MenuItem>
        <MenuItem component={RouterLink} to="/donar" onClick={() => setResourcesAnchor(null)}>
          Donar
        </MenuItem>
        <MenuItem component={RouterLink} to="/feedback" onClick={() => setResourcesAnchor(null)}>
          Sugerencias
        </MenuItem>
        <MenuItem component={RouterLink} to="/herramientas/chatkit" onClick={() => setResourcesAnchor(null)}>
          ChatKit
        </MenuItem>
        <MenuItem component={RouterLink} to="/herramientas/tidal-agent" onClick={() => setResourcesAnchor(null)}>
          Tidal Agent
        </MenuItem>
        <MenuItem component={RouterLink} to="/herramientas/creador-musical" onClick={() => setResourcesAnchor(null)}>
          Creador musical
        </MenuItem>
        {hasAdmin && (
          <MenuItem component={RouterLink} to="/herramientas/token-admin" onClick={() => setResourcesAnchor(null)}>
            Token API
          </MenuItem>
        )}
        <MenuItem component={RouterLink} to="/seguridad" onClick={() => setResourcesAnchor(null)}>
          Seguridad
        </MenuItem>
      </Menu>
    </AppBar>
  );
}
