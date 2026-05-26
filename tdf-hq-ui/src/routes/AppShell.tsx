import { useEffect, useState } from 'react';
import { Box, Container, Stack, useMediaQuery, useTheme } from '@mui/material';
import { Navigate, Outlet, useLocation } from 'react-router-dom';

import ApiActivityIndicator from '../components/ApiActivityIndicator';
import ApiStatusChip from '../components/ApiStatusChip';
import ChatKitLauncher from '../components/ChatKitLauncher';
import SidebarNav from '../components/SidebarNav';
import TopBar from '../components/TopBar';
import { useSession } from '../session/SessionContext';
import { canAccessPath } from '../utils/accessControl';
import { buildLoginRedirectPath, pickLandingPath } from '../utils/loginRouting';
import RouteLoadingFallback from './RouteLoadingFallback';

const DESKTOP_NAV_MIN_WIDTH = 1024;

export function Shell() {
  const theme = useTheme();
  const isDesktop = useMediaQuery(theme.breakpoints.up('lg'));
  const { session, loading } = useSession();
  const location = useLocation();
  const [sidebarCollapsed, setSidebarCollapsed] = useState(() => {
    if (typeof window === 'undefined') return false;
    return window.innerWidth < DESKTOP_NAV_MIN_WIDTH;
  });

  useEffect(() => {
    if (isDesktop) {
      try {
        window.localStorage.setItem('sidebar-collapsed', sidebarCollapsed ? '1' : '0');
      } catch {
        // ignore
      }
    }
  }, [sidebarCollapsed, isDesktop]);

  useEffect(() => {
    setSidebarCollapsed(!isDesktop);
  }, [isDesktop]);

  const handleNavigateFromSidebar = () => {
    if (!isDesktop) {
      setSidebarCollapsed(true);
    }
  };

  useEffect(() => {
    if (sidebarCollapsed) return;
    const handler = (event: KeyboardEvent) => {
      if (event.key !== 'Escape') return;
      if (!isDesktop) {
        setSidebarCollapsed(true);
      }
    };
    window.addEventListener('keydown', handler);
    return () => window.removeEventListener('keydown', handler);
  }, [sidebarCollapsed, isDesktop]);

  if (loading) {
    return <RouteLoadingFallback />;
  }

  if (!session) {
    const loginPath = buildLoginRedirectPath(`${location.pathname}${location.search}${location.hash}`);
    return <Navigate to={loginPath} replace />;
  }

  if (!canAccessPath(location.pathname, session.roles ?? [], session.modules)) {
    const landingPath = pickLandingPath(session.roles ?? [], session.modules);
    const fallbackPath =
      landingPath !== location.pathname && canAccessPath(landingPath, session.roles ?? [], session.modules)
        ? landingPath
        : '/inicio';
    return <Navigate to={fallbackPath} replace />;
  }

  const hideFloatingAssistants =
    location.pathname === '/inicio'
    || location.pathname.startsWith('/records')
    || location.pathname.startsWith('/marketplace')
    || location.pathname.startsWith('/operacion/ordenes-marketplace')
    || location.pathname.startsWith('/social/inbox')
    || location.pathname.startsWith('/configuracion/cursos')
    || location.pathname.startsWith('/configuracion/inscripciones-curso')
    || location.pathname.startsWith('/configuracion/usuarios-admin')
    || location.pathname.startsWith('/configuracion/cms')
    || location.pathname.startsWith('/configuracion/roles-permisos')
    || location.pathname.startsWith('/escuela/clases')
    || location.pathname.startsWith('/escuela/profesores')
    || location.pathname.startsWith('/escuela/trial-lessons');

  const handleToggleSidebar = () => setSidebarCollapsed((prev) => !prev);

  return (
    <Box
      sx={{
        display: 'flex',
        minHeight: '100vh',
        height: '100vh',
        bgcolor: 'background.default',
        overflow: 'hidden',
      }}
    >
      <SidebarNav open={!sidebarCollapsed} onNavigate={handleNavigateFromSidebar} />
      <Box
        sx={{
          flexGrow: 1,
          display: 'flex',
          flexDirection: 'column',
          position: 'relative',
          minWidth: 0,
          minHeight: 0,
          maxHeight: '100vh',
          overflow: 'hidden',
        }}
      >
        {!sidebarCollapsed && (
          <Box
            sx={{
              position: 'fixed',
              inset: 0,
              bgcolor: 'rgba(0,0,0,0.45)',
              backdropFilter: 'blur(2px)',
              zIndex: 1100,
              display: { xs: 'block', lg: 'none' },
            }}
            onClick={() => setSidebarCollapsed(true)}
          />
        )}
        <TopBar onToggleSidebar={handleToggleSidebar} sidebarOpen={!sidebarCollapsed} />
        <ApiActivityIndicator />
        <Box
          component="main"
          sx={{
            flexGrow: 1,
            position: 'relative',
            px: { xs: 2, md: 4 },
            py: { xs: 2, md: 4 },
            overflowY: 'auto',
            minHeight: 0,
          }}
        >
          <Container maxWidth="xl" sx={{ pt: { xs: 3, md: 4 }, pb: 6 }}>
            <Outlet />
          </Container>
          {!hideFloatingAssistants && (
            <Box
              sx={(theme) => ({
                position: 'fixed',
                right: { xs: 16, md: 32 },
                bottom: `calc(${theme.spacing(10)} + env(safe-area-inset-bottom, 0px))`,
                zIndex: theme.zIndex.tooltip,
                display: { xs: 'none', md: 'block' },
              })}
            >
              <Stack spacing={1.5} alignItems="flex-end">
                <ApiStatusChip />
                <ChatKitLauncher />
              </Stack>
            </Box>
          )}
        </Box>
      </Box>
    </Box>
  );
}

export function ConfigurationIndexRedirect() {
  const { session } = useSession();
  const roles = session?.roles ?? [];
  const modules = session?.modules;
  const targetPath = [
    '/configuracion/roles-permisos',
    '/configuracion/usuarios-admin',
    '/configuracion/cms',
    '/configuracion/whatsapp-consentimiento',
    '/configuracion/integraciones/calendario',
    '/configuracion/estado',
    '/configuracion/logs',
    '/configuracion/brain',
    '/configuracion/opciones-ux',
    '/configuracion/preferencias',
    '/configuracion/inscripciones-curso',
    '/configuracion/cursos',
  ].find((path) => canAccessPath(path, roles, modules))
    ?? pickLandingPath(roles, modules);

  return <Navigate to={targetPath} replace />;
}
