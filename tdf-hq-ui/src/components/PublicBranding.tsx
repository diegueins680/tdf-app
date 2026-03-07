import { Box, Button, Container, IconButton, Menu, MenuItem, Stack, Tooltip, Typography } from '@mui/material';
import { Link as RouterLink, useLocation } from 'react-router-dom';
import BrandLogo from './BrandLogo';
import type { ReactNode } from 'react';
import { useMemo, useState } from 'react';
import MoreVertIcon from '@mui/icons-material/MoreVert';
import { STUDIO_WHATSAPP_URL } from '../config/appConfig';
import { buildLoginRedirectPath } from '../utils/loginRouting';

const PUBLIC_NAV_ITEMS = [
  { label: 'Comunidad', to: '/fans' },
  { label: 'Tienda', to: '/marketplace' },
  { label: 'Reservar', to: '/reservar' },
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
  const footerPrimaryAction = useMemo<FooterAction>(() => {
    if (location.pathname.startsWith('/reservar')) {
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
    return { label: 'Ir a la comunidad', kind: 'route', value: '/fans' };
  }, [location.pathname]);
  const footerSecondaryAction = useMemo<FooterAction>(() => {
    if (location.pathname.startsWith('/reservar')) {
      return { label: 'Ingresar y autocompletar', kind: 'route', value: contextualLoginPath };
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
            bgcolor: '#0f1118',
            py: 1.5,
          }}
        >
          <Container maxWidth="xl">
            <Stack direction="row" alignItems="center" justifyContent="space-between" spacing={2}>
              <Stack direction="row" alignItems="center" spacing={2}>
                <Box
                  component={RouterLink}
                  to="/inicio"
                  sx={{ display: 'inline-flex', alignItems: 'center' }}
                  aria-label="Ir al inicio"
                >
                  <BrandLogo
                    variant="wordmark"
                    size={48}
                    sx={{ filter: 'brightness(0) invert(1)' }}
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
                      color="inherit"
                      component={RouterLink}
                      to={item.to}
                      sx={{ textTransform: 'none', color: '#cbd5e1' }}
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
                    color="secondary"
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
                    sx={{ color: '#e2e8f0' }}
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
          bgcolor: '#0f1118',
          py: 2,
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
              <Typography variant="subtitle2" sx={{ color: '#f8fafc', letterSpacing: 0.2 }}>
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
                    color="secondary"
                    component="a"
                    href={footerPrimaryAction.value}
                    target="_blank"
                    rel="noreferrer"
                    sx={{ textTransform: 'none' }}
                  >
                    {footerPrimaryAction.label}
                  </Button>
                ) : (
                  <Button
                    size="small"
                    variant="contained"
                    color="secondary"
                    component={RouterLink}
                    to={footerPrimaryAction.value}
                    sx={{ textTransform: 'none' }}
                  >
                    {footerPrimaryAction.label}
                  </Button>
                )}
                {footerSecondaryAction.kind === 'external' ? (
                  <Button
                    size="small"
                    variant="outlined"
                    color="inherit"
                    component="a"
                    href={footerSecondaryAction.value}
                    target="_blank"
                    rel="noreferrer"
                    sx={{ textTransform: 'none', borderColor: 'rgba(148,163,184,0.35)' }}
                  >
                    {footerSecondaryAction.label}
                  </Button>
                ) : (
                  <Button
                    size="small"
                    variant="outlined"
                    color="inherit"
                    component={RouterLink}
                    to={footerSecondaryAction.value}
                    sx={{ textTransform: 'none', borderColor: 'rgba(148,163,184,0.35)' }}
                  >
                    {footerSecondaryAction.label}
                  </Button>
                )}
              </Stack>
            </Stack>
            <Stack spacing={0.75}>
              <Typography variant="caption" sx={{ color: '#94a3b8', textTransform: 'uppercase', letterSpacing: 0.35 }}>
                Explorar
              </Typography>
              <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                {PUBLIC_NAV_ITEMS.map((item) => (
                  <Button
                    key={item.to}
                    size="small"
                    color="inherit"
                    component={RouterLink}
                    to={item.to}
                    sx={{ textTransform: 'none' }}
                  >
                    {item.label}
                  </Button>
                ))}
                <Button size="small" color="inherit" component={RouterLink} to="/feedback" sx={{ textTransform: 'none' }}>
                  Sugerencias
                </Button>
                <Button size="small" color="inherit" component={RouterLink} to="/donar" sx={{ textTransform: 'none' }}>
                  Donar
                </Button>
              </Stack>
            </Stack>
            <Stack spacing={0.75}>
              <Typography variant="caption" sx={{ color: '#94a3b8', textTransform: 'uppercase', letterSpacing: 0.35 }}>
                Gestión de mensajes
              </Typography>
              <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                <Button
                  size="small"
                  color="inherit"
                  component={RouterLink}
                  to="/whatsapp/consentimiento"
                  sx={{ textTransform: 'none' }}
                >
                  Consentimiento WhatsApp
                </Button>
                <Button
                  size="small"
                  color="inherit"
                  component={RouterLink}
                  to="/whatsapp/ok"
                  sx={{ textTransform: 'none' }}
                >
                  Confirmación WhatsApp
                </Button>
              </Stack>
            </Stack>
            {showLoginButton && (
              <Stack direction="row" spacing={2} flexWrap="wrap" useFlexGap>
                <Button
                  size="small"
                  color="inherit"
                  component={RouterLink}
                  to={contextualLoginPath}
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
