import { useEffect, useRef, useState } from 'react';
import { Box, Container, Stack, useMediaQuery, useTheme } from '@mui/material';
import { Navigate, Outlet, useLocation } from 'react-router-dom';

import ApiActivityIndicator from '../components/ApiActivityIndicator';
import ApiStatusChip from '../components/ApiStatusChip';
import ChatKitLauncher from '../components/ChatKitLauncher';
import { OfflineBanner } from '../components/OfflineBanner';
import RegistryBreadcrumbs from '../components/RegistryBreadcrumbs';
import RouteErrorBoundary from '../components/RouteErrorBoundary';
import SidebarNav from '../components/SidebarNav';
import TopBar from '../components/TopBar';
import { useSession } from '../session/SessionContext';
import { canAccessPath } from '../utils/accessControl';
import { buildLoginRedirectPath, pickLandingPath } from '../utils/loginRouting';
import RouteLoadingFallback from './RouteLoadingFallback';
import ForbiddenPage from '../pages/ForbiddenPage';
import { evaluatePathAccess } from '../features/featureRegistry';
import { useNavigationPreferences } from '../hooks/useNavigationPreferences';
import { getAnalyticsClient } from '../analytics/posthog';

const DESKTOP_NAV_MIN_WIDTH = 1024;

export function Shell() {
  const theme = useTheme();
  const isDesktop = useMediaQuery(theme.breakpoints.up('lg'));
  const { session, loading } = useSession();
  const location = useLocation();
  const sidebarToggleRef = useRef<HTMLButtonElement | null>(null);
  const recordedPathRef = useRef('');
  const navigationPreferences = useNavigationPreferences(Boolean(session));
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
      if (event.key === 'Escape' && !sidebarCollapsed) {
        setSidebarCollapsed(true);
      }
    };
    window.addEventListener('keydown', handler);
    return () => window.removeEventListener('keydown', handler);
  }, [sidebarCollapsed, isDesktop]);

  useEffect(() => {
    if (isDesktop || sidebarCollapsed) return;
    const sidebar = document.getElementById('app-sidebar');
    if (!sidebar) return;
    const sidebarToggleButton = sidebarToggleRef.current;
    const selector = [
      'a[href]',
      'button:not([disabled])',
      'input:not([disabled])',
      'select:not([disabled])',
      'textarea:not([disabled])',
      '[tabindex]:not([tabindex="-1"])',
    ].join(',');
    const getFocusable = () => Array.from(sidebar.querySelectorAll<HTMLElement>(selector));
    const animationFrame = window.requestAnimationFrame(() => getFocusable()[0]?.focus());
    const trapFocus = (event: KeyboardEvent) => {
      if (event.key !== 'Tab') return;
      const focusable = getFocusable();
      if (focusable.length === 0) return;
      const first = focusable[0];
      const last = focusable[focusable.length - 1];
      if (!first || !last) return;
      if (event.shiftKey && document.activeElement === first) {
        event.preventDefault();
        last.focus();
      } else if (!event.shiftKey && document.activeElement === last) {
        event.preventDefault();
        first.focus();
      }
    };
    window.addEventListener('keydown', trapFocus);
    return () => {
      window.cancelAnimationFrame(animationFrame);
      window.removeEventListener('keydown', trapFocus);
      sidebarToggleButton?.focus();
    };
  }, [isDesktop, sidebarCollapsed]);

  useEffect(() => {
    if (!session || loading) return;
    const decision = evaluatePathAccess(location.pathname, {
      authenticated: true,
      roles: session.roles,
      modules: session.modules,
      featureFlags: session.featureFlags,
    });
    const key = `${location.pathname}:${decision?.feature.id ?? 'unregistered'}:${decision?.state ?? 'unknown'}`;
    if (recordedPathRef.current === key) return;
    recordedPathRef.current = key;
    if (decision?.state === 'allowed' && decision.feature.recentBehavior !== 'none') {
      navigationPreferences.visit.mutate(decision.feature.id);
    } else if (decision && decision.state !== 'allowed') {
      getAnalyticsClient().capture('feature_403_shown', {
        feature_id: decision.feature.id,
        platform: 'web',
        reason: decision.reason,
      });
    } else if (!decision) {
      getAnalyticsClient().capture('feature_destination_unresolved', {
        platform: 'web',
        route_registered: false,
      });
    }
  }, [loading, location.pathname, navigationPreferences.visit, session]);

  if (loading) {
    return <RouteLoadingFallback />;
  }

  if (!session) {
    const loginPath = buildLoginRedirectPath(`${location.pathname}${location.search}${location.hash}`);
    return <Navigate to={loginPath} replace />;
  }

  const accessDecision = evaluatePathAccess(location.pathname, {
    authenticated: true,
    roles: session.roles,
    modules: session.modules,
    featureFlags: session.featureFlags,
  });
  const forbiddenDecision = accessDecision?.state !== 'allowed' ? accessDecision : null;

  const hideFloatingAssistants =
    location.pathname === '/inicio'
    || location.pathname.startsWith('/records')
    || location.pathname.startsWith('/marketplace')
    || location.pathname.startsWith('/operacion/ordenes-marketplace')
    || location.pathname.startsWith('/social/inbox')
    || location.pathname.startsWith('/configuracion/cursos')
    || location.pathname.startsWith('/configuracion/inscripciones-curso')
    || location.pathname.startsWith('/configuracion/usuarios-admin')
    || location.pathname.startsWith('/configuracion/actividad')
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
      <Box
        component="a"
        href="#main-content"
        sx={{
          position: 'fixed',
          top: 8,
          left: 8,
          zIndex: (currentTheme) => currentTheme.zIndex.tooltip + 1,
          px: 2,
          py: 1,
          borderRadius: 1,
          bgcolor: 'background.paper',
          color: 'text.primary',
          transform: 'translateY(-150%)',
          transition: 'transform 0.15s ease',
          '&:focus': { transform: 'translateY(0)' },
        }}
      >
        Saltar al contenido principal
      </Box>
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
            component="button"
            type="button"
            aria-label="Cerrar menú lateral"
            tabIndex={-1}
            sx={{
              position: 'fixed',
              inset: 0,
              bgcolor: 'rgba(0,0,0,0.45)',
              backdropFilter: 'blur(2px)',
              zIndex: 1100,
              display: { xs: 'block', lg: 'none' },
              border: 0,
              p: 0,
              cursor: 'pointer',
            }}
            onClick={() => setSidebarCollapsed(true)}
          />
        )}
        <TopBar
          onToggleSidebar={handleToggleSidebar}
          sidebarOpen={!sidebarCollapsed}
          toggleButtonRef={sidebarToggleRef}
        />
        <OfflineBanner />
        <ApiActivityIndicator />
        <Box
          component="main"
          id="main-content"
          tabIndex={-1}
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
            <RegistryBreadcrumbs />
            {forbiddenDecision ? <ForbiddenPage decision={forbiddenDecision} /> : <RouteErrorBoundary><Outlet /></RouteErrorBoundary>}
          </Container>
          {!forbiddenDecision && !hideFloatingAssistants && (
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
    '/configuracion/actividad',
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
