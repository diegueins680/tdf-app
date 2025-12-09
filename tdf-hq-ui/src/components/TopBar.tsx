import MenuIcon from '@mui/icons-material/Menu';
import VpnKeyIcon from '@mui/icons-material/VpnKey';
import { useEffect, useMemo, useState, useCallback, useRef } from 'react';
import { AppBar, Box, Button, IconButton, Stack, Toolbar, Badge, Typography, Popover, Divider, Tooltip, Dialog, DialogTitle, DialogContent, TextField, InputAdornment, List, ListItemButton, ListItemText } from '@mui/material';
import Menu from '@mui/material/Menu';
import MenuItem from '@mui/material/MenuItem';
import { Link as RouterLink, useLocation, useNavigate } from 'react-router-dom';
import SessionMenu from './SessionMenu';
import { useSession } from '../session/SessionContext';
import ApiTokenDialog from './ApiTokenDialog';
import BrandLogo from './BrandLogo';
import SearchIcon from '@mui/icons-material/Search';
import { NAV_GROUPS, deriveModulesFromRoles, pathRequiresModule } from './SidebarNav';

interface TopBarProps {
  onToggleSidebar?: () => void;
}

const CART_META_KEY = 'tdf-marketplace-cart-meta';
const CART_EVENT = 'tdf-cart-updated';

const FRIENDLY_SEGMENTS: Record<string, string> = {
  inicio: 'Inicio',
  marketplace: 'Marketplace',
  fans: 'Fan Hub',
  records: 'Records',
  crm: 'CRM',
  contactos: 'Contactos',
  empresas: 'Empresas',
  leads: 'Leads',
  estudio: 'Estudio',
  salas: 'Salas',
  ordenes: 'Órdenes',
  servicios: 'Servicios',
  pipelines: 'Pipelines',
  'live-sessions': 'Live Sessions',
  reportes: 'Reportes',
  escuela: 'Escuela',
  profesores: 'Profesores',
  clases: 'Clases',
  'trial-lessons': 'Trial Lessons',
  'trial-queue': 'Trial Queue',
  label: 'Label',
  artistas: 'Artistas',
  proyectos: 'Proyectos',
  releases: 'Releases',
  tracks: 'Tracks',
  assets: 'Assets',
  operacion: 'Operación',
  inventario: 'Inventario',
  'ordenes-marketplace': 'Órdenes marketplace',
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
  'tidal-agent': 'Tidal Agent',
  'creador-musical': 'Creador musical',
};

interface CartPreviewItem {
  title: string;
  subtotal: string;
}
const readCartMeta = (): { cartId: string; count: number; preview: CartPreviewItem[] } => {
  try {
    const raw = localStorage.getItem(CART_META_KEY);
    if (!raw) return { cartId: '', count: 0, preview: [] };
    const parsed = JSON.parse(raw) as Partial<{ cartId: string; count: number; preview: CartPreviewItem[] }>;
    return {
      cartId: typeof parsed?.cartId === 'string' ? parsed.cartId : '',
      count: typeof parsed?.count === 'number' ? parsed.count : 0,
      preview: Array.isArray(parsed?.preview) ? parsed.preview : [],
    };
  } catch {
    return { cartId: '', count: 0, preview: [] };
  }
};

export default function TopBar({ onToggleSidebar }: TopBarProps) {
  const { session, logout } = useSession();
  const navigate = useNavigate();
  const location = useLocation();
  const [tokenDialogOpen, setTokenDialogOpen] = useState(false);
  const [quickNavOpen, setQuickNavOpen] = useState(false);
  const [quickQuery, setQuickQuery] = useState('');
  const [quickHighlight, setQuickHighlight] = useState(0);
  const [cartCount, setCartCount] = useState(0);
  const [cartPreview, setCartPreview] = useState<{ title: string; subtotal: string }[]>([]);
  const [cartAnchor, setCartAnchor] = useState<HTMLElement | null>(null);
  const [resourcesAnchor, setResourcesAnchor] = useState<null | HTMLElement>(null);
  const quickInputRef = useRef<HTMLInputElement | null>(null);
  const resourcesButtonRef = useRef<HTMLButtonElement | null>(null);

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

  const quickNavItems = useMemo(() => {
    return NAV_GROUPS.flatMap((group) =>
      group.items
        .filter((item) => {
          const required = pathRequiresModule(item.path);
          if (!required) return true;
          return modules.has(required);
        })
        .map((item) => ({ ...item, group: group.title })),
    );
  }, [modules]);

  const filteredQuickItems = useMemo(() => {
    const query = quickQuery.trim().toLowerCase();
    if (!query) return quickNavItems;
    return quickNavItems.filter(
      (item) => item.label.toLowerCase().includes(query) || item.path.toLowerCase().includes(query),
    );
  }, [quickNavItems, quickQuery]);

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
    if (quickNavOpen) {
      setTimeout(() => quickInputRef.current?.focus(), 0);
    }
  }, [quickNavOpen]);

  const handleSelectQuick = (idx: number) => {
    const target = filteredQuickItems[idx];
    if (!target) return;
    navigate(target.path);
    closeQuickNav();
  };

  const handleOpenCart = (event: React.MouseEvent<HTMLElement>) => {
    setCartAnchor(event.currentTarget);
  };
  const handleCloseCart = () => setCartAnchor(null);
  const cartOpen = Boolean(cartAnchor);
  const resourcesOpen = Boolean(resourcesAnchor);

  const renderBreadcrumb = () => {
    const parts = location.pathname.split('/').filter(Boolean);
    if (parts.length === 0) return null;
    const label = parts
      .map((p) => FRIENDLY_SEGMENTS[p] ?? p.replace(/-/g, ' '))
      .map((p) => (p.length > 0 ? p.charAt(0).toUpperCase() + p.slice(1) : p))
      .join(' / ');
    return (
      <Typography
        variant="caption"
        sx={{ color: '#cbd5e1', letterSpacing: 0.3, textTransform: 'uppercase' }}
      >
        {label}
      </Typography>
    );
  };

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
          minHeight: { xs: 96, md: 124 },
          px: { xs: 2, md: 4 },
        }}
      >
        <IconButton
          edge="start"
          onClick={onToggleSidebar}
          sx={{ display: { lg: 'none' }, color: '#f8fafc', mr: 1 }}
        >
          <MenuIcon />
        </IconButton>
        <Box
          component={RouterLink}
          to="/inicio"
          sx={{
            display: 'inline-flex',
            alignItems: 'center',
            flexGrow: { xs: 0, lg: 1 },
            mr: { xs: 1.5, lg: 0 },
          }}
          aria-label="Ir al inicio"
        >
          <BrandLogo
            variant="wordmark"
            size={55}
            sx={{
              height: { xs: 35, sm: 50, md: 65 },
              filter: 'brightness(0) invert(1) drop-shadow(0 10px 26px rgba(0,0,0,0.45))',
            }}
          />
        </Box>
        <Box sx={{ display: { xs: 'none', md: 'flex' }, flexDirection: 'column', ml: 2 }}>
          {renderBreadcrumb()}
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
              sx={{ textTransform: 'none', borderColor: 'rgba(148,163,184,0.4)' }}
              aria-keyshortcuts="Control+K Meta+K"
            >
              Ir a...
            </Button>
          </Tooltip>
          <Tooltip title="Alt+R abre recursos">
            <Button
              color="inherit"
              onClick={(e) => setResourcesAnchor(e.currentTarget)}
              sx={{ textTransform: 'none' }}
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
            sx={{ textTransform: 'none', position: 'relative' }}
            onMouseEnter={handleOpenCart}
            onClick={handleOpenCart}
          >
            <Badge
              color="secondary"
              badgeContent={cartCount > 9 ? '9+' : cartCount}
              invisible={cartCount <= 0}
              sx={{ '& .MuiBadge-badge': { right: -6, top: 6 } }}
            >
              Carrito
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
              }}
            >
              ADMIN
            </Button>
          )}
          <Button
            variant="outlined"
            color="inherit"
            startIcon={<VpnKeyIcon fontSize="small" />}
            onClick={() => setTokenDialogOpen(true)}
            sx={{ textTransform: 'none', borderColor: 'rgba(148,163,184,0.4)' }}
          >
            Token API
          </Button>

          {session ? (
            <>
              {hasAdmin && (
                <Button
                  variant="outlined"
                  color="info"
                  onClick={() => navigate('/configuracion/roles-permisos')}
                  sx={{ textTransform: 'none', borderColor: 'rgba(148,163,184,0.4)', color: '#f8fafc' }}
                >
                  Panel
                </Button>
              )}
              <Button
                variant="contained"
                color="secondary"
                onClick={handleLogout}
                sx={{ textTransform: 'none', borderRadius: 999 }}
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
            sx={{ mb: 2 }}
          />
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
                    secondary={item.group}
                    primaryTypographyProps={{ fontWeight: idx === quickHighlight ? 700 : 500 }}
                  />
                  <Typography variant="caption" color="text.secondary">
                    {item.path}
                  </Typography>
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
        <MenuItem component={RouterLink} to="/herramientas/tidal-agent" onClick={() => setResourcesAnchor(null)}>
          Tidal Agent
        </MenuItem>
        <MenuItem component={RouterLink} to="/herramientas/creador-musical" onClick={() => setResourcesAnchor(null)}>
          Creador musical
        </MenuItem>
        <MenuItem component={RouterLink} to="/seguridad" onClick={() => setResourcesAnchor(null)}>
          Seguridad
        </MenuItem>
      </Menu>
      <ApiTokenDialog open={tokenDialogOpen} onClose={() => setTokenDialogOpen(false)} />
    </AppBar>
  );
}
