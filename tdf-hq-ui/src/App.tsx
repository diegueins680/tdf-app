import { useState } from 'react';
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
import BasicFeaturePage from './components/BasicFeaturePage';
import AboutPage from './pages/AboutPage';
import DocsPage from './pages/DocsPage';
import FanHubPage from './pages/FanHubPage';
import CourseRegistrationsAdminPage from './pages/CourseRegistrationsAdminPage';
import { useSession } from './session/SessionContext';
import SidebarNav from './components/SidebarNav';
import ApiStatusChip from './components/ApiStatusChip';
import InscripcionPage from './pages/inscripcion/InscripcionPage';
import LiveSessionIntakePage from './pages/LiveSessionIntakePage';
import CourseProductionLandingPage from './pages/CourseProductionLandingPage';
import LogsPage from './pages/LogsPage';
import SystemStatusPage from './pages/SystemStatusPage';
import AdminUsersPage from './pages/AdminUsersPage';
import TrialsPage from './pages/TrialsPage';
import TeachersPage from './pages/TeachersPage';
import TrialLessonsPage from './pages/TrialLessonsPage';
import RecordsPublicPage from './pages/RecordsPublicPage';
import AdsInboxPage from './pages/AdsInboxPage';

function Shell() {
  const { session } = useSession();
  const [sidebarCollapsed, setSidebarCollapsed] = useState(true);

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
          <Box sx={{ position: 'fixed', bottom: 24, right: 32 }}>
            <ApiStatusChip />
          </Box>
        </Box>
      </Box>
    </Box>
  );
}

export default function App() {
  return (
    <Routes>
      <Route path="/fans" element={<FanHubPage />} />
      <Route path="/mi-artista" element={<FanHubPage focusArtist />} />
      <Route path="/curso/produccion-musical-dic-2025" element={<CourseProductionLandingPage />} />
      <Route path="/inscripcion/:slug" element={<InscripcionPage />} />
      <Route path="/trials" element={<TrialsPage />} />
      <Route path="/records" element={<RecordsPublicPage />} />
      <Route path="/login" element={<LoginPage />} />
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
        <Route path="/docs" element={<DocsPage />} />
        <Route path="/acerca" element={<AboutPage />} />
        <Route
          path="/seguridad"
          element={
            <BasicFeaturePage
              title="Seguridad"
              description="Notas de seguridad y pendientes de hardening."
              storageKey="seguridad"
            />
          }
        />

        <Route path="/crm" element={<Outlet />}>
          <Route path="contactos" element={<PartiesPage />} />
          <Route
            path="empresas"
            element={<BasicFeaturePage title="CRM / Empresas" storageKey="crm-empresas" />}
          />
          <Route
            path="leads"
            element={<BasicFeaturePage title="CRM / Leads" storageKey="crm-leads" />}
          />
          <Route index element={<Navigate to="contactos" replace />} />
        </Route>

        <Route path="/estudio" element={<Outlet />}>
          <Route path="calendario" element={<BookingsPage />} />
          <Route path="salas" element={<RoomsPage />} />
          <Route path="ordenes" element={<OrdersPage />} />
          <Route path="pipelines" element={<KanbanPage />} />
          <Route path="live-sessions" element={<LiveSessionIntakePage />} />
          <Route
            path="reportes"
            element={<BasicFeaturePage title="Estudio / Reportes" storageKey="estudio-reportes" />}
          />
          <Route index element={<Navigate to="calendario" replace />} />
        </Route>

        <Route path="/label" element={<Outlet />}>
          <Route path="artistas" element={<BasicFeaturePage title="Label / Artistas" storageKey="label-artistas" />} />
          <Route path="proyectos" element={<BasicFeaturePage title="Label / Proyectos" storageKey="label-proyectos" />} />
          <Route path="releases" element={<BasicFeaturePage title="Label / Releases" storageKey="label-releases" />} />
          <Route path="tracks" element={<BasicFeaturePage title="Label / Tracks" storageKey="label-tracks" />} />
          <Route path="assets" element={<BasicFeaturePage title="Label / Assets" storageKey="label-assets" />} />
          <Route path="metadata" element={<BasicFeaturePage title="Label / Metadata" storageKey="label-metadata" />} />
          <Route path="contratos" element={<BasicFeaturePage title="Label / Contratos" storageKey="label-contratos" />} />
          <Route path="regalias" element={<BasicFeaturePage title="Label / Regalías" storageKey="label-regalias" />} />
          <Route path="marketing" element={<BasicFeaturePage title="Label / Marketing" storageKey="label-marketing" />} />
          <Route index element={<Navigate to="artistas" replace />} />
        </Route>

        <Route path="/eventos" element={<Outlet />}>
          <Route path="agenda" element={<BasicFeaturePage title="Eventos / Agenda" storageKey="eventos-agenda" />} />
          <Route path="fechas-y-tours" element={<BasicFeaturePage title="Eventos / Fechas y tours" storageKey="eventos-fechas" />} />
          <Route path="venues" element={<BasicFeaturePage title="Eventos / Venues" storageKey="eventos-venues" />} />
          <Route path="staff" element={<BasicFeaturePage title="Eventos / Staff" storageKey="eventos-staff" />} />
          <Route path="presupuestos" element={<BasicFeaturePage title="Eventos / Presupuestos" storageKey="eventos-presupuestos" />} />
          <Route path="post-mortem" element={<BasicFeaturePage title="Eventos / Post-mortem" storageKey="eventos-postmortem" />} />
          <Route index element={<Navigate to="agenda" replace />} />
        </Route>

        <Route path="/escuela" element={<Outlet />}>
          <Route path="profesores" element={<TeachersPage />} />
          <Route path="clases" element={<BasicFeaturePage title="Escuela / Clases" storageKey="escuela-clases" />} />
          <Route path="trial-lessons" element={<TrialLessonsPage />} />
          <Route path="trial-queue" element={<AdsInboxPage />} />
          <Route path="programas" element={<BasicFeaturePage title="Escuela / Programas" storageKey="escuela-programas" />} />
          <Route path="cursos" element={<BasicFeaturePage title="Escuela / Cursos" storageKey="escuela-cursos" />} />
          <Route path="cohortes" element={<BasicFeaturePage title="Escuela / Cohortes" storageKey="escuela-cohortes" />} />
          <Route path="estudiantes" element={<BasicFeaturePage title="Escuela / Estudiantes" storageKey="escuela-estudiantes" />} />
          <Route path="inscripciones" element={<BasicFeaturePage title="Escuela / Inscripciones" storageKey="escuela-inscripciones" />} />
          <Route path="pagos" element={<BasicFeaturePage title="Escuela / Pagos" storageKey="escuela-pagos" />} />
          <Route index element={<Navigate to="programas" replace />} />
        </Route>

        <Route path="/finanzas" element={<Outlet />}>
          <Route path="cotizaciones" element={<BasicFeaturePage title="Finanzas / Cotizaciones" storageKey="finanzas-cotizaciones" />} />
          <Route path="facturas" element={<BasicFeaturePage title="Finanzas / Facturas" storageKey="finanzas-facturas" />} />
          <Route path="cobros" element={<BasicFeaturePage title="Finanzas / Cobros" storageKey="finanzas-cobros" />} />
          <Route path="recibos" element={<BasicFeaturePage title="Finanzas / Recibos" storageKey="finanzas-recibos" />} />
          <Route path="regalias" element={<BasicFeaturePage title="Finanzas / Regalías" storageKey="finanzas-regalias" />} />
          <Route index element={<Navigate to="cotizaciones" replace />} />
        </Route>

        <Route path="/bar" element={<Outlet />}>
          <Route path="sell" element={<BasicFeaturePage title="Bar / Sell" storageKey="bar-sell" />} />
          <Route path="register" element={<BasicFeaturePage title="Bar / Register" storageKey="bar-register" />} />
          <Route path="inventory" element={<BasicFeaturePage title="Bar / Inventory" storageKey="bar-inventory" />} />
          <Route path="staff" element={<BasicFeaturePage title="Bar / Staff" storageKey="bar-staff" />} />
          <Route index element={<Navigate to="sell" replace />} />
        </Route>

        <Route path="/operacion" element={<Outlet />}>
          <Route path="inventario" element={<BasicFeaturePage title="Operación / Inventario" storageKey="operacion-inventario" />} />
          <Route path="calendario-domo" element={<BasicFeaturePage title="Operación / Calendario del domo" storageKey="operacion-calendario-domo" />} />
          <Route path="reservas-equipo" element={<BasicFeaturePage title="Operación / Reservas de equipo" storageKey="operacion-reservas-equipo" />} />
          <Route path="mantenimiento" element={<BasicFeaturePage title="Operación / Mantenimiento" storageKey="operacion-mantenimiento" />} />
          <Route path="paquetes" element={<BasicFeaturePage title="Operación / Paquetes" storageKey="operacion-paquetes" />} />
          <Route path="paquetes/resumen" element={<BasicFeaturePage title="Operación / Paquetes - Resumen" storageKey="operacion-paquetes-resumen" />} />
          <Route index element={<Navigate to="inventario" replace />} />
        </Route>

        <Route path="/configuracion" element={<Outlet />}>
          <Route path="inscripciones-curso" element={<CourseRegistrationsAdminPage />} />
          <Route path="usuarios-admin" element={<AdminUsersPage />} />
          <Route path="estado" element={<SystemStatusPage />} />
          <Route path="logs" element={<LogsPage />} />
          <Route path="roles-permisos" element={<UserRoleManagement />} />
          <Route path="impuestos-series" element={<BasicFeaturePage title="Configuración / Impuestos y series" storageKey="configuracion-impuestos" />} />
          <Route path="unidades-negocio" element={<BasicFeaturePage title="Configuración / Unidades de negocio" storageKey="configuracion-unidades" />} />
          <Route path="sedes" element={<BasicFeaturePage title="Configuración / Sedes" storageKey="configuracion-sedes" />} />
          <Route path="marcas" element={<BasicFeaturePage title="Configuración / Marcas" storageKey="configuracion-marcas" />} />
          <Route path="integraciones" element={<BasicFeaturePage title="Configuración / Integraciones" storageKey="configuracion-integraciones" />} />
          <Route path="preferencias" element={<SystemPage />} />
          <Route index element={<Navigate to="roles-permisos" replace />} />
        </Route>

        <Route
          path="/insights"
          element={<BasicFeaturePage title="Insights" description="Notas y KPIs pendientes de instrumentar." storageKey="insights" />}
        />

        <Route path="*" element={<Navigate to="/inicio" replace />} />
      </Route>
      <Route path="*" element={<Navigate to="/login" replace />} />
    </Routes>
  );
}
