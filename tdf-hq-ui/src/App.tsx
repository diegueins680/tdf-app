import { Container } from '@mui/material';
import { Route, Routes, Navigate, Outlet } from 'react-router-dom';
import TopBar from './components/TopBar';
import PartiesPage from './pages/PartiesPage';
import BookingsPage from './pages/BookingsPage';
import KanbanPage from './pages/KanbanPage';
import LoginPage from './pages/LoginPage';
import UserRoleManagement from './components/UserRoleManagement';
import SystemPage from './pages/SystemPage';
import PlaceholderPage from './components/PlaceholderPage';
import { useSession } from './session/SessionContext';

function Shell() {
  const { session } = useSession();

  if (!session) {
    return <Navigate to="/login" replace />;
  }

  return (
    <>
      <TopBar />
      <Container maxWidth="xl" sx={{ mt: 3, mb: 6 }}>
        <Outlet />
      </Container>
    </>
  );
}

export default function App() {
  return (
    <Routes>
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

        <Route path="/inicio" element={<PlaceholderPage title="Inicio" description="Resumen ejecutivo y accesos rápidos." />} />

        <Route path="/crm" element={<Outlet />}>
          <Route path="contactos" element={<PartiesPage />} />
          <Route path="empresas" element={<PlaceholderPage title="CRM / Empresas" />} />
          <Route path="leads" element={<PlaceholderPage title="CRM / Leads" />} />
          <Route index element={<Navigate to="contactos" replace />} />
        </Route>

        <Route path="/estudio" element={<Outlet />}>
          <Route path="calendario" element={<BookingsPage />} />
          <Route path="salas" element={<PlaceholderPage title="Estudio / Salas" />} />
          <Route path="ordenes" element={<PlaceholderPage title="Estudio / Órdenes" />} />
          <Route path="pipelines" element={<KanbanPage />} />
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
