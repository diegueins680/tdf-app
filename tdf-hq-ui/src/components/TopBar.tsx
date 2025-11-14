import MenuIcon from '@mui/icons-material/Menu';
import VpnKeyIcon from '@mui/icons-material/VpnKey';
import { useState } from 'react';
import { AppBar, Box, Button, Chip, IconButton, Stack, Toolbar } from '@mui/material';
import { Link as RouterLink, useNavigate } from 'react-router-dom';
import SessionMenu from './SessionMenu';
import { useSession } from '../session/SessionContext';
import ApiTokenDialog from './ApiTokenDialog';
import BrandLogo from './BrandLogo';

interface TopBarProps {
  onToggleSidebar?: () => void;
}

export default function TopBar({ onToggleSidebar }: TopBarProps) {
  const { session, logout } = useSession();
  const navigate = useNavigate();
  const [tokenDialogOpen, setTokenDialogOpen] = useState(false);

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
      <Toolbar sx={{ minHeight: 72, px: { xs: 2, md: 4 } }}>
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
            variant="alt"
            size={72}
            sx={{
              height: { xs: 42, sm: 52, md: 72 },
              filter: 'drop-shadow(0 4px 16px rgba(0,0,0,0.3))',
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

          <Chip label="ADMIN" size="small" sx={{ bgcolor: 'rgba(59,130,246,0.15)', color: '#93c5fd' }} />
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
