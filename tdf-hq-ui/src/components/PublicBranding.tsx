import { Box, Button, Container, IconButton, Menu, MenuItem, Stack, Tooltip, Typography } from '@mui/material';
import { Link as RouterLink, useLocation } from 'react-router-dom';
import BrandLogo from './BrandLogo';
import type { ReactNode } from 'react';
import { useMemo, useState } from 'react';
import MoreVertIcon from '@mui/icons-material/MoreVert';
import { STUDIO_WHATSAPP_URL } from '../config/appConfig';
import { buildLoginRedirectPath } from '../utils/loginRouting';

const PUBLIC_NAV_ITEMS = [
  { label: 'TDF', to: '/tdf' },
  { label: 'Comunidad', to: '/fans' },
  { label: 'Tienda', to: '/marketplace' },
  { label: 'Domo', to: '/domo-del-pululahua' },
  { label: 'Reservar', to: '/reservar' },
  { label: 'DJ Booth', to: '/dj-booth' },
  { label: 'Lanzamientos', to: '/records' },
] as const;

interface FooterAction {
  label: string;
  kind: 'route' | 'external';
  value: string;
}

export default function PublicBranding({
  children,
  showHeader = true,
  showLoginButton = true,
}: {
  children: ReactNode;
  showHeader?: boolean;
  showLoginButton?: boolean;
}) {
  const [menuAnchor, setMenuAnchor] = useState<null | HTMLElement>(null);
  const location = useLocation();
  const open = Boolean(menuAnchor);
  const contextualLoginPath = useMemo(
    () => buildLoginRedirectPath(`${location.pathname}${location.search}${location.hash}`),
    [location.hash, location.pathname, location.search],
  );
  const isActiveNavItem = (path: string) =>
    location.pathname === path || location.pathname.startsWith(`${path}/`);
  const footerPrimaryAction = useMemo<FooterAction>(() => {
    if (
      location.pathname.startsWith('/reservar') ||
      location.pathname.startsWith('/dj-booth') ||
      location.pathname.startsWith('/domo-del-pululahua')
    ) {
      return { label: 'WhatsApp reservas', kind: 'external', value: STUDIO_WHATSAPP_URL };
    }
    if (location.pathname.startsWith('/fans')) {
      return { label: 'Ver lanzamientos', kind: 'route', value: '/records' };
    }
    if (location.pathname.startsWith('/records')) {
      return { label: 'Reservar estudio', kind: 'route', value: '/reservar' };
    }
    if (location.pathname.startsWith('/marketplace')) {
      return { label: 'Necesito ayuda', kind: 'route', value: '/feedback' };
    }
    return { label: 'Crear cuenta', kind: 'route', value: '/login?signup=1&redirect=/fans' };
  }, [location.pathname]);
  const footerSecondaryAction = useMemo<FooterAction>(() => {
    if (location.pathname.startsWith('/reservar')) {
      return { label: 'Ingresar y autocompletar', kind: 'route', value: contextualLoginPath };
    }
    if (location.pathname.startsWith('/dj-booth')) {
      return { label: 'Reserva general', kind: 'route', value: '/reservar' };
    }
    if (location.pathname.startsWith('/domo-del-pululahua')) {
      return { label: 'Reservar estudio', kind: 'route', value: '/reservar' };
    }
    if (location.pathname.startsWith('/fans')) {
      return { label: 'Reservar estudio', kind: 'route', value: '/reservar' };
    }
    if (location.pathname.startsWith('/records')) {
      return { label: 'Abrir comunidad', kind: 'route', value: '/fans' };
    }
    if (location.pathname.startsWith('/marketplace')) {
      return { label: 'Reservar estudio', kind: 'route', value: '/reservar' };
    }
    return { label: 'WhatsApp', kind: 'external', value: STUDIO_WHATSAPP_URL };
  }, [contextualLoginPath, location.pathname]);

  return (
    <Box sx={{ minHeight: '100vh', bgcolor: 'background.default' }}>
      {showHeader && (
        <Box
          sx={{
            borderBottom: '1px solid',
            borderColor: 'divider',
            bgcolor: 'background.paper',
            py: 1.5,
          }}
        >
          <Container maxWidth="xl">
            <Stack direction="row" alignItems="center" justifyContent="space-between" spacing={2}>
              <Stack direction="row" alignItems="center" spacing={2}>
                <Box
                  component={RouterLink}
                  to="/tdf"
                  sx={{ display: 'inline-flex', alignItems: 'center' }}
                  aria-label="Ir a TDF"
                >
                  <BrandLogo
                    variant="wordmark"
                    size={42}
                    sx={{
                      height: { xs: 28, sm: 36, md: 42 },
                      filter: (theme) =>
                        theme.palette.mode === 'dark' ? 'brightness(0) invert(1)' : 'none',
                    }}
                  />
                </Box>
                <Stack
                  direction="row"
                  spacing={0.5}
                  sx={{ display: { xs: 'none', md: 'flex' }, flexWrap: 'wrap' }}
                >
                  {PUBLIC_NAV_ITEMS.map((item) => (
                    <Button
                      key={item.to}
                      component={RouterLink}
                      to={item.to}
                      sx={{
                        textTransform: 'none',
                        color: isActiveNavItem(item.to) ? 'text.primary' : 'text.secondary',
                        bgcolor: isActiveNavItem(item.to) ? 'action.selected' : 'transparent',
                        borderColor: isActiveNavItem(item.to) ? 'divider' : 'transparent',
                        borderWidth: 1,
                        borderStyle: 'solid',
                        fontWeight: isActiveNavItem(item.to) ? 700 : 500,
                        '&:hover': {
                          bgcolor: isActiveNavItem(item.to) ? 'action.selected' : 'action.hover',
                        },
                      }}
                    >
                      {item.label}
                    </Button>
                  ))}
                </Stack>
              </Stack>
              <Stack direction="row" alignItems="center" spacing={1.5}>
                {showLoginButton && (
                  <Button
                    variant="contained"
                    component={RouterLink}
                    to={contextualLoginPath}
                    sx={{ textTransform: 'none' }}
                  >
                    Ingresar
                  </Button>
                )}
                <Tooltip title="Más opciones">
                  <IconButton
                    aria-label="Más opciones"
                    onClick={(e) => setMenuAnchor(e.currentTarget)}
                    sx={{ color: 'text.secondary' }}
                  >
                    <MoreVertIcon />
                  </IconButton>
                </Tooltip>
                <Menu
                  anchorEl={menuAnchor}
                  open={open}
                  onClose={() => setMenuAnchor(null)}
                  anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
                  transformOrigin={{ vertical: 'top', horizontal: 'right' }}
                >
                  {PUBLIC_NAV_ITEMS.map((item) => (
                    <MenuItem key={item.to} component={RouterLink} to={item.to} onClick={() => setMenuAnchor(null)}>
                      {item.label}
                    </MenuItem>
                  ))}
                  <MenuItem component={RouterLink} to="/donar" onClick={() => setMenuAnchor(null)}>
                    Donar
                  </MenuItem>
                  <MenuItem component={RouterLink} to="/feedback" onClick={() => setMenuAnchor(null)}>
                    Enviar sugerencia
                  </MenuItem>
                </Menu>
              </Stack>
            </Stack>
          </Container>
        </Box>
      )}
      <Container maxWidth="xl" sx={{ py: { xs: showHeader ? 2 : 3, md: showHeader ? 4 : 5 } }}>
        {children}
      </Container>
      <Box
        sx={{
          borderTop: '1px solid',
          borderColor: 'divider',
          bgcolor: 'background.paper',
          py: 3,
        }}
      >
        <Container maxWidth="xl">
          <Stack
            direction={{ xs: 'column', xl: 'row' }}
            spacing={3}
            alignItems={{ xs: 'flex-start', xl: 'center' }}
            justifyContent="space-between"
          >
            <Stack spacing={1} sx={{ maxWidth: 460 }}>
              <Typography variant="subtitle2" sx={{ letterSpacing: 0.2 }}>
                TDF Records
              </Typography>
              <Typography variant="body2" color="text.secondary">
                Si te atoras, te dejamos una salida clara desde esta página para que sigas avanzando.
              </Typography>
              <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                {footerPrimaryAction.kind === 'external' ? (
                  <Button
                    size="small"
                    variant="contained"
                    component="a"
                    href={footerPrimaryAction.value}
                    target="_blank"
                    rel="noreferrer"
                    sx={{ textTransform: 'none', boxShadow: 'none' }}
                  >
                    {footerPrimaryAction.label}
                  </Button>
                ) : (
                  <Button
                    size="small"
                    variant="contained"
                    component={RouterLink}
                    to={footerPrimaryAction.value}
                    sx={{ textTransform: 'none', boxShadow: 'none' }}
                  >
                    {footerPrimaryAction.label}
                  </Button>
                )}
                {footerSecondaryAction.kind === 'external' ? (
                  <Button
                    size="small"
                    variant="outlined"
                    component="a"
                    href={footerSecondaryAction.value}
                    target="_blank"
                    rel="noreferrer"
                    sx={{ textTransform: 'none' }}
                  >
                    {footerSecondaryAction.label}
                  </Button>
                ) : (
                  <Button
                    size="small"
                    variant="outlined"
                    component={RouterLink}
                    to={footerSecondaryAction.value}
                    sx={{ textTransform: 'none' }}
                  >
                    {footerSecondaryAction.label}
                  </Button>
                )}
              </Stack>
            </Stack>
            <Stack spacing={0.75}>
              <Typography variant="caption" color="text.secondary" sx={{ textTransform: 'uppercase', letterSpacing: 0.35 }}>
                Explorar
              </Typography>
              <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                {PUBLIC_NAV_ITEMS.map((item) => (
                  <Button
                    key={item.to}
                    size="small"
                    component={RouterLink}
                    to={item.to}
                    sx={{
                      textTransform: 'none',
                      color: isActiveNavItem(item.to) ? 'text.primary' : 'text.secondary',
                      bgcolor: isActiveNavItem(item.to) ? 'action.selected' : 'transparent',
                      px: 1.5,
                      fontWeight: isActiveNavItem(item.to) ? 700 : 500,
                      '&:hover': {
                        bgcolor: isActiveNavItem(item.to) ? 'action.selected' : 'action.hover',
                      },
                    }}
                  >
                    {item.label}
                  </Button>
                ))}
                <Button
                  size="small"
                  component={RouterLink}
                  to="/feedback"
                  sx={{ textTransform: 'none', color: 'text.secondary', px: 1.5 }}
                >
                  Sugerencias
                </Button>
                <Button
                  size="small"
                  component={RouterLink}
                  to="/donar"
                  sx={{ textTransform: 'none', color: 'text.secondary', px: 1.5 }}
                >
                  Donar
                </Button>
              </Stack>
            </Stack>
            <Stack spacing={0.75}>
              <Typography variant="caption" color="text.secondary" sx={{ textTransform: 'uppercase', letterSpacing: 0.35 }}>
                Gestión de mensajes
              </Typography>
              <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                <Button
                  size="small"
                  component={RouterLink}
                  to="/whatsapp/consentimiento"
                  sx={{ textTransform: 'none', color: 'text.secondary', px: 1.5 }}
                >
                  Consentimiento WhatsApp
                </Button>
                <Button
                  size="small"
                  component={RouterLink}
                  to="/whatsapp/ok"
                  sx={{ textTransform: 'none', color: 'text.secondary', px: 1.5 }}
                >
                  Confirmación WhatsApp
                </Button>
              </Stack>
            </Stack>
            {showLoginButton && (
              <Stack direction="row" spacing={2} flexWrap="wrap" useFlexGap>
                <Button
                  size="small"
                  component={RouterLink}
                  to={contextualLoginPath}
                  variant="outlined"
                  sx={{ textTransform: 'none' }}
                >
                  Ir a login
                </Button>
              </Stack>
            )}
          </Stack>
        </Container>
      </Box>
    </Box>
  );
}
