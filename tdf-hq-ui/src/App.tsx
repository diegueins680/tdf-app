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
import PlaceholderPage from './components/PlaceholderPage';
import AboutPage from './pages/AboutPage';
import DocsPage from './pages/DocsPage';
import FanHubPage from './pages/FanHubPage';
import { useSession } from './session/SessionContext';
import SidebarNav from './components/SidebarNav';
import ApiStatusChip from './components/ApiStatusChip';
import InscripcionPage from './pages/inscripcion/InscripcionPage';
import LiveSessionIntakePage from './pages/LiveSessionIntakePage';
import CourseProductionLandingPage from './pages/CourseProductionLandingPage';
import LogsPage from './pages/LogsPage';

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
      <Route path="/curso/produccion-musical-dic-2025" element={<CourseProductionLandingPage />} />
      <Route path="/inscripcion/:slug" element={<InscripcionPage />} />
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
        <Route path="/seguridad" element={<PlaceholderPage title="Seguridad" />} />

        <Route path="/crm" element={<Outlet />}>
          <Route path="contactos" element={<PartiesPage />} />
          <Route path="empresas" element={<PlaceholderPage title="CRM / Empresas" />} />
          <Route path="leads" element={<PlaceholderPage title="CRM / Leads" />} />
          <Route index element={<Navigate to="contactos" replace />} />
        </Route>

        <Route path="/estudio" element={<Outlet />}>
          <Route path="calendario" element={<BookingsPage />} />
          <Route path="salas" element={<RoomsPage />} />
          <Route path="ordenes" element={<OrdersPage />} />
          <Route path="pipelines" element={<KanbanPage />} />
          <Route path="live-sessions" element={<LiveSessionIntakePage />} />
          <Route path="reportes" element={<PlaceholderPage title="Estudio / Reportes" />} />
          <Route index element={<Navigate to="calendario" replace />} />
        </Route>

        <Route path="/label" element={<Outlet />}>
          <Route path="artistas" element={<PlaceholderPage title="Label / Artistas" />} />
          <Route path="proyectos" element={<PlaceholderPage title="Label / Proyectos" />} />
          <Route path="releases" element={<PlaceholderPage title="Label / Releases" />} />
          <Route path="tracks" element={<PlaceholderPage title="Label / Tracks" />} />
          <Route path="assets" element={<PlaceholderPage title="Label / Assets" />} />
          <Route path="metadata" element={<PlaceholderPage title="Label / Metadata" />} />
          <Route path="contratos" element={<PlaceholderPage title="Label / Contratos" />} />
          <Route path="regalias" element={<PlaceholderPage title="Label / Regalías" />} />
          <Route path="marketing" element={<PlaceholderPage title="Label / Marketing" />} />
          <Route index element={<Navigate to="artistas" replace />} />
        </Route>

        <Route path="/eventos" element={<Outlet />}>
          <Route path="agenda" element={<PlaceholderPage title="Eventos / Agenda" />} />
          <Route path="fechas-y-tours" element={<PlaceholderPage title="Eventos / Fechas y tours" />} />
          <Route path="venues" element={<PlaceholderPage title="Eventos / Venues" />} />
          <Route path="staff" element={<PlaceholderPage title="Eventos / Staff" />} />
          <Route path="presupuestos" element={<PlaceholderPage title="Eventos / Presupuestos" />} />
          <Route path="post-mortem" element={<PlaceholderPage title="Eventos / Post-mortem" />} />
          <Route index element={<Navigate to="agenda" replace />} />
        </Route>

        <Route path="/escuela" element={<Outlet />}>
          <Route path="profesores" element={<PlaceholderPage title="Escuela / Profesores" />} />
          <Route path="clases" element={<PlaceholderPage title="Escuela / Clases" />} />
          <Route path="trial-lessons" element={<PlaceholderPage title="Escuela / Trial lessons" />} />
          <Route path="trial-queue" element={<PlaceholderPage title="Escuela / Trial queue" />} />
          <Route path="programas" element={<PlaceholderPage title="Escuela / Programas" />} />
          <Route path="cursos" element={<PlaceholderPage title="Escuela / Cursos" />} />
          <Route path="cohortes" element={<PlaceholderPage title="Escuela / Cohortes" />} />
          <Route path="estudiantes" element={<PlaceholderPage title="Escuela / Estudiantes" />} />
          <Route path="inscripciones" element={<PlaceholderPage title="Escuela / Inscripciones" />} />
          <Route path="pagos" element={<PlaceholderPage title="Escuela / Pagos" />} />
          <Route index element={<Navigate to="programas" replace />} />
        </Route>

        <Route path="/finanzas" element={<Outlet />}>
          <Route path="cotizaciones" element={<PlaceholderPage title="Finanzas / Cotizaciones" />} />
          <Route path="facturas" element={<PlaceholderPage title="Finanzas / Facturas" />} />
          <Route path="cobros" element={<PlaceholderPage title="Finanzas / Cobros" />} />
          <Route path="recibos" element={<PlaceholderPage title="Finanzas / Recibos" />} />
          <Route path="regalias" element={<PlaceholderPage title="Finanzas / Regalías" />} />
          <Route index element={<Navigate to="cotizaciones" replace />} />
        </Route>

        <Route path="/bar" element={<Outlet />}>
          <Route path="sell" element={<PlaceholderPage title="Bar / Sell" />} />
          <Route path="register" element={<PlaceholderPage title="Bar / Register" />} />
          <Route path="inventory" element={<PlaceholderPage title="Bar / Inventory" />} />
          <Route path="staff" element={<PlaceholderPage title="Bar / Staff" />} />
          <Route index element={<Navigate to="sell" replace />} />
        </Route>

        <Route path="/operacion" element={<Outlet />}>
          <Route path="inventario" element={<PlaceholderPage title="Operación / Inventario" />} />
          <Route path="calendario-domo" element={<PlaceholderPage title="Operación / Calendario del domo" />} />
          <Route path="reservas-equipo" element={<PlaceholderPage title="Operación / Reservas de equipo" />} />
          <Route path="mantenimiento" element={<PlaceholderPage title="Operación / Mantenimiento" />} />
          <Route path="paquetes" element={<PlaceholderPage title="Operación / Paquetes" />} />
          <Route path="paquetes/resumen" element={<PlaceholderPage title="Operación / Paquetes - Resumen" />} />
          <Route index element={<Navigate to="inventario" replace />} />
        </Route>

        <Route path="/configuracion" element={<Outlet />}>
          <Route path="logs" element={<LogsPage />} />
          <Route path="roles-permisos" element={<UserRoleManagement />} />
          <Route path="impuestos-series" element={<PlaceholderPage title="Configuración / Impuestos y series" />} />
          <Route path="unidades-negocio" element={<PlaceholderPage title="Configuración / Unidades de negocio" />} />
          <Route path="sedes" element={<PlaceholderPage title="Configuración / Sedes" />} />
          <Route path="marcas" element={<PlaceholderPage title="Configuración / Marcas" />} />
          <Route path="integraciones" element={<PlaceholderPage title="Configuración / Integraciones" />} />
          <Route path="preferencias" element={<SystemPage />} />
          <Route index element={<Navigate to="roles-permisos" replace />} />
        </Route>

        <Route path="/insights" element={<PlaceholderPage title="Insights" description="Analítica consolidada de negocio." />} />

        <Route path="*" element={<Navigate to="/inicio" replace />} />
      </Route>
      <Route path="*" element={<Navigate to="/login" replace />} />
    </Routes>
  );
}
