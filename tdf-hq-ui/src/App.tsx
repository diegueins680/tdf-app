import { useEffect, useState } from 'react';
import { Box, Button, Container } from '@mui/material';
import { Route, Routes, Navigate, Outlet } from 'react-router-dom';
import TopBar from './components/TopBar';
import PartiesPage from './pages/PartiesPage';
import BookingsPage from './pages/BookingsPage';
import KanbanPage from './pages/KanbanPage';
import OrdersPage from './pages/OrdersPage';
import RoomsPage from './pages/RoomsPage';
import LoginPage from './pages/LoginPage';
import UserRoleManagement from './components/UserRoleManagement';
import SystemPage from './pages/SystemPage';
import AboutPage from './pages/AboutPage';
import DocsPage from './pages/DocsPage';
import FanHubPage from './pages/FanHubPage';
import CourseRegistrationsAdminPage from './pages/CourseRegistrationsAdminPage';
import { useSession } from './session/SessionContext';
import SidebarNav from './components/SidebarNav';
import ApiStatusChip from './components/ApiStatusChip';
import RadioWidget from './components/RadioWidget';
import PublicBranding from './components/PublicBranding';
import InscripcionPage from './pages/inscripcion/InscripcionPage';
import LiveSessionIntakePage from './pages/LiveSessionIntakePage';
import CourseProductionLandingPage from './pages/CourseProductionLandingPage';
import LogsPage from './pages/LogsPage';
import SystemStatusPage from './pages/SystemStatusPage';
import AdminUsersPage from './pages/AdminUsersPage';
import TrialsPage from './pages/TrialsPage';
import LiveSessionPublicPage from './pages/LiveSessionPublicPage';
import TeachersPage from './pages/TeachersPage';
import TrialLessonsPage from './pages/TrialLessonsPage';
import ClassesPage from './pages/ClassesPage';
import ReportsPage from './pages/ReportsPage';
import RecordsPublicPage from './pages/RecordsPublicPage';
import PublicProfilePage from './pages/PublicProfilePage';
import SocialPageView from './pages/SocialPage';
import SocialEventsPage from './pages/SocialEventsPage';
import LabelArtistsPage from './pages/LabelArtistsPage';
import LabelProjectsPage from './pages/LabelProjectsPage';
import LabelReleasesPage from './pages/LabelReleasesPage';
import ServiceTypesPage from './pages/ServiceTypesPage';
import InventoryPage from './pages/InventoryPage';
import InventoryScanPage from './pages/InventoryScanPage';
import ReservasEquipoPage from './pages/ReservasEquipoPage';
import MusicMakerPage from './pages/MusicMakerPage';
import AdsInboxPage from './pages/AdsInboxPage';
import CmsAdminPage from './pages/CmsAdminPage';
import FeedbackPage from './pages/FeedbackPage';
import CompaniesPage from './pages/CompaniesPage';
import LeadsPage from './pages/LeadsPage';
import DonationPage from './pages/DonationPage';
import PaymentsPage from './pages/PaymentsPage';
import CalendarSyncPage from './pages/CalendarSyncPage';
import SecurityPage from './pages/SecurityPage';
import MarketplacePage from './pages/MarketplacePage';
import ManualPage from './pages/ManualPage';
import GoogleDriveCallbackPage from './pages/GoogleDriveCallbackPage';
import MarketplaceOrdersPage from './pages/MarketplaceOrdersPage';
import DatafastReturnPage from './pages/DatafastReturnPage';
import MarketplaceOrderTrackingPage from './pages/MarketplaceOrderTrackingPage';
import NotFoundPage from './pages/NotFoundPage';
import TidalAgentPage from './pages/TidalAgentPage';
import LabelAssetsPage from './pages/LabelAssetsPage';
import LabelTracksPage from './pages/LabelTracksPage';
import AdminDiagnosticsPage from './pages/AdminDiagnosticsPage';
import PublicBookingPage from './pages/PublicBookingPage';
import CourseBuilderPage from './pages/CourseBuilderPage';
import UxOptionsPage from './pages/UxOptionsPage';

function Shell() {
  const { session } = useSession();
  const [sidebarCollapsed, setSidebarCollapsed] = useState(() => {
    if (typeof window === 'undefined') return true;
    const saved = window.localStorage.getItem('sidebar-collapsed');
    if (saved !== null) return saved === '1';
    return window.innerWidth < 1024;
  });

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      window.localStorage.setItem('sidebar-collapsed', sidebarCollapsed ? '1' : '0');
    } catch {
      // ignore storage issues
    }
  }, [sidebarCollapsed]);

  if (!session) {
    return <Navigate to="/login" replace />;
  }

  const handleNavigateFromSidebar = () => {
    if (typeof window !== 'undefined' && window.innerWidth < 1024) {
      setSidebarCollapsed(true);
    }
  };

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
              bgcolor: 'rgba(15,17,24,0.72)',
              zIndex: 1100,
              display: { xs: 'block', lg: 'none' },
            }}
            onClick={() => setSidebarCollapsed(true)}
          />
        )}
        <TopBar onToggleSidebar={handleToggleSidebar} />
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
          <Box sx={{ position: 'absolute', left: { xs: 16, md: 32 }, top: 16 }}>
            <Button
              variant="contained"
              size="small"
              onClick={handleToggleSidebar}
              sx={{
                borderRadius: 999,
                px: 3,
                bgcolor: 'rgba(148,163,184,0.2)',
                color: '#f8fafc',
                backdropFilter: 'blur(12px)',
              }}
            >
              {sidebarCollapsed ? 'Abrir menú' : 'Cerrar menú'}
            </Button>
          </Box>
          <Container maxWidth="xl" sx={{ pt: 10, pb: 6 }}>
            <Outlet />
          </Container>
          <Box
            sx={(theme) => ({
              position: 'fixed',
              right: { xs: 16, md: 32 },
              bottom: `calc(${theme.spacing(10)} + env(safe-area-inset-bottom, 0px))`,
              zIndex: theme.zIndex.tooltip,
            })}
          >
            <ApiStatusChip />
          </Box>
        </Box>
      </Box>
    </Box>
  );
}

export default function App() {
  return (
    <>
      <Routes>
        <Route path="/fans" element={<PublicBranding><FanHubPage /></PublicBranding>} />
        <Route path="/mi-artista" element={<PublicBranding><FanHubPage focusArtist /></PublicBranding>} />
        <Route path="/marketplace" element={<PublicBranding><MarketplacePage /></PublicBranding>} />
        <Route path="/marketplace/orden/:orderId" element={<PublicBranding><MarketplaceOrderTrackingPage /></PublicBranding>} />
        <Route path="/marketplace/pago-datafast" element={<PublicBranding><DatafastReturnPage /></PublicBranding>} />
        <Route path="/oauth/google-drive/callback" element={<PublicBranding><GoogleDriveCallbackPage /></PublicBranding>} />
        <Route path="/curso/produccion-musical-dic-2025" element={<PublicBranding><CourseProductionLandingPage /></PublicBranding>} />
        <Route path="/inscripcion/:slug" element={<PublicBranding><InscripcionPage /></PublicBranding>} />
        <Route path="/trials" element={<PublicBranding><TrialsPage /></PublicBranding>} />
        <Route path="/live-sessions/registro" element={<PublicBranding><LiveSessionPublicPage /></PublicBranding>} />
        <Route path="/feedback" element={<PublicBranding><FeedbackPage /></PublicBranding>} />
        <Route path="/records" element={<PublicBranding><RecordsPublicPage /></PublicBranding>} />
        <Route path="/inventario/scan/:token" element={<PublicBranding><InventoryScanPage /></PublicBranding>} />
        <Route path="/donar" element={<PublicBranding><DonationPage /></PublicBranding>} />
        <Route path="/reservar" element={<PublicBranding><PublicBookingPage /></PublicBranding>} />
        <Route path="/login" element={<PublicBranding showHeader={false}><LoginPage /></PublicBranding>} />
        <Route path="/herramientas/creador-musical" element={<PublicBranding><MusicMakerPage /></PublicBranding>} />
        <Route element={<Shell />}>
          <Route path="/" element={<Navigate to="/inicio" replace />} />

          {/* Aliases para rutas históricas */}
          <Route path="/parties" element={<Navigate to="/crm/contactos" replace />} />
          <Route path="/bookings" element={<Navigate to="/estudio/calendario" replace />} />
          <Route path="/pipelines" element={<Navigate to="/estudio/pipelines" replace />} />
          <Route path="/kanban" element={<Navigate to="/estudio/pipelines" replace />} />
          <Route path="/contactos" element={<Navigate to="/crm/contactos" replace />} />
          <Route path="/system" element={<Navigate to="/configuracion/preferencias" replace />} />
          <Route path="/admin/roles" element={<Navigate to="/configuracion/roles-permisos" replace />} />

          <Route path="/inicio" element={<FanHubPage />} />
          <Route path="/perfil/:partyId" element={<PublicProfilePage />} />
          <Route path="/social" element={<SocialPageView />} />
          <Route path="/social/eventos" element={<SocialEventsPage />} />
          <Route path="/docs" element={<DocsPage />} />
          <Route path="/manual" element={<ManualPage />} />
          <Route path="/acerca" element={<AboutPage />} />
          <Route path="/seguridad" element={<SecurityPage />} />
          <Route path="/admin/diagnosticos" element={<AdminDiagnosticsPage />} />
          <Route path="/herramientas/tidal-agent" element={<TidalAgentPage />} />
          <Route path="/crm" element={<Outlet />}>
            <Route path="contactos" element={<PartiesPage />} />
            <Route path="empresas" element={<CompaniesPage />} />
            <Route path="leads" element={<LeadsPage />} />
            <Route index element={<Navigate to="contactos" replace />} />
          </Route>

          <Route path="/estudio" element={<Outlet />}>
            <Route path="calendario" element={<BookingsPage />} />
            <Route path="salas" element={<RoomsPage />} />
            <Route path="ordenes" element={<OrdersPage />} />
            <Route path="servicios" element={<ServiceTypesPage />} />
            <Route path="pipelines" element={<KanbanPage />} />
            <Route path="live-sessions" element={<LiveSessionIntakePage />} />
            <Route path="reportes" element={<ReportsPage />} />
            <Route index element={<Navigate to="calendario" replace />} />
          </Route>

          <Route path="/label" element={<Outlet />}>
            <Route path="artistas" element={<LabelArtistsPage />} />
            <Route path="proyectos" element={<LabelProjectsPage />} />
            <Route path="releases" element={<LabelReleasesPage />} />
            <Route path="assets" element={<LabelAssetsPage />} />
            <Route path="tracks" element={<LabelTracksPage />} />
            <Route index element={<Navigate to="artistas" replace />} />
          </Route>

          <Route path="/escuela" element={<Outlet />}>
            <Route path="profesores" element={<TeachersPage />} />
            <Route path="clases" element={<ClassesPage />} />
            <Route path="trial-lessons" element={<TrialLessonsPage />} />
            <Route path="trial-queue" element={<AdsInboxPage />} />
            <Route index element={<Navigate to="trial-lessons" replace />} />
          </Route>

          <Route path="/finanzas" element={<Outlet />}>
            <Route path="pagos" element={<PaymentsPage />} />
            <Route index element={<Navigate to="pagos" replace />} />
          </Route>

          <Route path="/operacion" element={<Outlet />}>
            <Route path="inventario" element={<InventoryPage />} />
            <Route path="ordenes-marketplace" element={<MarketplaceOrdersPage />} />
            <Route path="reservas-equipo" element={<ReservasEquipoPage />} />
            <Route index element={<Navigate to="inventario" replace />} />
          </Route>

          <Route path="/configuracion" element={<Outlet />}>
            <Route path="inscripciones-curso" element={<CourseRegistrationsAdminPage />} />
            <Route path="cursos" element={<CourseBuilderPage />} />
            <Route path="usuarios-admin" element={<AdminUsersPage />} />
            <Route path="estado" element={<SystemStatusPage />} />
            <Route path="logs" element={<LogsPage />} />
            <Route path="roles-permisos" element={<UserRoleManagement />} />
            <Route path="integraciones/calendario" element={<CalendarSyncPage />} />
            <Route path="cms" element={<CmsAdminPage />} />
            <Route path="opciones-ux" element={<UxOptionsPage />} />
            <Route path="preferencias" element={<SystemPage />} />
            <Route index element={<Navigate to="roles-permisos" replace />} />
          </Route>

          <Route path="*" element={<NotFoundPage />} />
        </Route>
        <Route path="*" element={<PublicBranding><NotFoundPage /></PublicBranding>} />
      </Routes>
      <RadioWidget />
    </>
  );
}
