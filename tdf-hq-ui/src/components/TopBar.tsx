import MenuIcon from '@mui/icons-material/Menu';
import VpnKeyIcon from '@mui/icons-material/VpnKey';
import { useEffect, useMemo, useState } from 'react';
import { AppBar, Box, Button, Chip, IconButton, Stack, Toolbar, Badge } from '@mui/material';
import { Link as RouterLink, useNavigate } from 'react-router-dom';
import SessionMenu from './SessionMenu';
import { useSession } from '../session/SessionContext';
import ApiTokenDialog from './ApiTokenDialog';
import BrandLogo from './BrandLogo';

interface TopBarProps {
  onToggleSidebar?: () => void;
}

const CART_META_KEY = 'tdf-marketplace-cart-meta';

const readCartMeta = () => {
  try {
    const raw = localStorage.getItem(CART_META_KEY);
    if (!raw) return { cartId: '', count: 0 };
    const parsed = JSON.parse(raw);
    return {
      cartId: typeof parsed?.cartId === 'string' ? parsed.cartId : '',
      count: typeof parsed?.count === 'number' ? parsed.count : 0,
    };
  } catch {
    return { cartId: '', count: 0 };
  }
};

export default function TopBar({ onToggleSidebar }: TopBarProps) {
  const { session, logout } = useSession();
  const navigate = useNavigate();
  const [tokenDialogOpen, setTokenDialogOpen] = useState(false);
  const [cartCount, setCartCount] = useState(0);
  const hasAdmin = useMemo(
    () => (session?.modules ?? []).some((m) => m.toLowerCase() === 'admin'),
    [session?.modules],
  );

  useEffect(() => {
    setCartCount(readCartMeta().count);
    const handler = () => setCartCount(readCartMeta().count);
    window.addEventListener('storage', handler);
    return () => window.removeEventListener('storage', handler);
  }, []);

  const handleLogout = () => {
    logout();
    navigate('/login', { replace: true });
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

        <Stack
          direction="row"
          spacing={1}
          alignItems="center"
          sx={{ ml: 'auto' }}
        >
          <Button color="inherit" component={RouterLink} to="/docs" sx={{ textTransform: 'none' }}>
            Docs
          </Button>
          <Button color="inherit" component={RouterLink} to="/acerca" sx={{ textTransform: 'none' }}>
            Acerca de
          </Button>
          <Button color="inherit" component={RouterLink} to="/seguridad" sx={{ textTransform: 'none' }}>
            Seguridad
          </Button>
          <Button
            color="inherit"
            component={RouterLink}
            to="/marketplace"
            sx={{ textTransform: 'none', position: 'relative' }}
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

          {hasAdmin ? (
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
          ) : (
            <Chip label="ADMIN" size="small" sx={{ bgcolor: 'rgba(59,130,246,0.15)', color: '#93c5fd' }} />
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
              <Button
                variant="outlined"
                color="info"
                onClick={() => navigate('/configuracion/roles-permisos')}
                sx={{ textTransform: 'none', borderColor: 'rgba(148,163,184,0.4)', color: '#f8fafc' }}
              >
                Panel
              </Button>
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
      <ApiTokenDialog open={tokenDialogOpen} onClose={() => setTokenDialogOpen(false)} />
    </AppBar>
  );
}
