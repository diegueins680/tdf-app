import { lazy } from 'react';
import { Navigate, Outlet, Route } from 'react-router-dom';

import { ConfigurationIndexRedirect, Shell } from './AppShell';

const AboutPage = lazy(() => import('../pages/AboutPage'));
const AdsInboxPage = lazy(() => import('../pages/AdsInboxPage'));
const AdminDiagnosticsPage = lazy(() => import('../pages/AdminDiagnosticsPage'));
const AdminTokenPage = lazy(() => import('../pages/AdminTokenPage'));
const AdminUsersPage = lazy(() => import('../pages/AdminUsersPage'));
const ArtistPublicPage = lazy(() => import('../pages/ArtistPublicPage'));
const BookingsPage = lazy(() => import('../pages/BookingsPage'));
const BrainAdminPage = lazy(() => import('../pages/BrainAdminPage'));
const CalendarSyncPage = lazy(() => import('../pages/CalendarSyncPage'));
const ChatKitPage = lazy(() => import('../pages/ChatKitPage'));
const ChatPage = lazy(() => import('../pages/ChatPage'));
const ClassesPage = lazy(() => import('../pages/ClassesPage'));
const CmsAdminPage = lazy(() => import('../pages/CmsAdminPage'));
const CompaniesPage = lazy(() => import('../pages/CompaniesPage'));
const CourseBuilderPage = lazy(() => import('../pages/CourseBuilderPage'));
const CourseRegistrationsAdminPage = lazy(() => import('../pages/CourseRegistrationsAdminPage'));
const DocsPage = lazy(() => import('../pages/DocsPage'));
const FanHubPage = lazy(() => import('../pages/FanHubPage'));
const InstagramConnectPage = lazy(() => import('../pages/InstagramConnectPage'));
const InternshipsPage = lazy(() => import('../pages/InternshipsPage'));
const InventoryPage = lazy(() => import('../pages/InventoryPage'));
const KanbanPage = lazy(() => import('../pages/KanbanPage'));
const LabelArtistsPage = lazy(() => import('../pages/LabelArtistsPage'));
const LabelAssetsPage = lazy(() => import('../pages/LabelAssetsPage'));
const LabelProjectsPage = lazy(() => import('../pages/LabelProjectsPage'));
const LabelReleasesPage = lazy(() => import('../pages/LabelReleasesPage'));
const LabelTracksPage = lazy(() => import('../pages/LabelTracksPage'));
const LeadsPage = lazy(() => import('../pages/LeadsPage'));
const LiveSessionIntakePage = lazy(() => import('../pages/LiveSessionIntakePage'));
const LogsPage = lazy(() => import('../pages/LogsPage'));
const ManualPage = lazy(() => import('../pages/ManualPage'));
const MarketplaceOrdersPage = lazy(() => import('../pages/MarketplaceOrdersPage'));
const NotFoundPage = lazy(() => import('../pages/NotFoundPage'));
const OrdersPage = lazy(() => import('../pages/OrdersPage'));
const PartiesPage = lazy(() => import('../pages/PartiesPage'));
const PaymentsPage = lazy(() => import('../pages/PaymentsPage'));
const PublicProfilePage = lazy(() => import('../pages/PublicProfilePage'));
const RecordsPublicPage = lazy(() => import('../pages/RecordsPublicPage'));
const ReportsPage = lazy(() => import('../pages/ReportsPage'));
const ReservasEquipoPage = lazy(() => import('../pages/ReservasEquipoPage'));
const RoomsPage = lazy(() => import('../pages/RoomsPage'));
const SecurityPage = lazy(() => import('../pages/SecurityPage'));
const ServiceTypesPage = lazy(() => import('../pages/ServiceTypesPage'));
const SocialEventsPage = lazy(() => import('../pages/SocialEventsPage'));
const SocialInboxPage = lazy(() => import('../pages/SocialInboxPage'));
const SocialPageView = lazy(() => import('../pages/SocialPage'));
const SystemPage = lazy(() => import('../pages/SystemPage'));
const SystemStatusPage = lazy(() => import('../pages/SystemStatusPage'));
const TeacherPortalPage = lazy(() => import('../pages/TeacherPortalPage'));
const TeachersPage = lazy(() => import('../pages/TeachersPage'));
const TidalAgentPage = lazy(() => import('../pages/TidalAgentPage'));
const TrialLessonsPage = lazy(() => import('../pages/TrialLessonsPage'));
const UserRoleManagement = lazy(() => import('../components/UserRoleManagement'));
const UxOptionsPage = lazy(() => import('../pages/UxOptionsPage'));
const WhatsAppConsentPage = lazy(() => import('../pages/WhatsAppConsentPage'));

export function renderProtectedRoutes() {
  return (
    <>
      <Route element={<Shell />}>
        <Route path="/" element={<Navigate to="/inicio" replace />} />

        <Route path="/parties" element={<Navigate to="/crm/contactos" replace />} />
        <Route path="/bookings" element={<Navigate to="/estudio/calendario" replace />} />
        <Route path="/pipelines" element={<Navigate to="/estudio/pipelines" replace />} />
        <Route path="/kanban" element={<Navigate to="/estudio/pipelines" replace />} />
        <Route path="/contactos" element={<Navigate to="/crm/contactos" replace />} />
        <Route path="/system" element={<Navigate to="/configuracion/preferencias" replace />} />
        <Route path="/admin/roles" element={<Navigate to="/configuracion/roles-permisos" replace />} />

        <Route path="/inicio" element={<FanHubPage />} />
        <Route path="/mi-profesor" element={<TeacherPortalPage />} />
        <Route path="/perfil/:partyId" element={<PublicProfilePage />} />
        <Route path="/social" element={<SocialPageView />} />
        <Route path="/social/instagram" element={<InstagramConnectPage />} />
        <Route path="/social/inbox" element={<SocialInboxPage />} />
        <Route path="/social/eventos" element={<SocialEventsPage />} />
        <Route path="/chat" element={<ChatPage />} />
        <Route path="/docs" element={<DocsPage />} />
        <Route path="/manual" element={<ManualPage />} />
        <Route path="/acerca" element={<AboutPage />} />
        <Route path="/seguridad" element={<SecurityPage />} />
        <Route path="/practicas" element={<InternshipsPage />} />
        <Route path="/admin/diagnosticos" element={<AdminDiagnosticsPage />} />
        <Route path="/herramientas/chatkit" element={<ChatKitPage />} />
        <Route path="/herramientas/tidal-agent" element={<TidalAgentPage />} />
        <Route path="/herramientas/token-admin" element={<AdminTokenPage />} />
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
          <Route path="brain" element={<BrainAdminPage />} />
          <Route path="roles-permisos" element={<UserRoleManagement />} />
          <Route path="integraciones/calendario" element={<CalendarSyncPage />} />
          <Route path="cms" element={<CmsAdminPage />} />
          <Route path="whatsapp-consentimiento" element={<WhatsAppConsentPage />} />
          <Route path="opciones-ux" element={<UxOptionsPage />} />
          <Route path="preferencias" element={<SystemPage />} />
          <Route index element={<ConfigurationIndexRedirect />} />
        </Route>

        <Route path="*" element={<NotFoundPage />} />
      </Route>
    </>
  );
}
