import { Route, Routes, Navigate, Outlet, useLocation } from 'react-router-dom';
import Layout from './components/Layout';
import Placeholder from './components/Placeholder';
import PartiesPage from './pages/PartiesPage';
import BookingsPage from './pages/BookingsPage';
import CalendarPage from './pages/CalendarPage';
import PipelinesPage from './pages/PipelinesPage';
import SessionsPage from './pages/SessionsPage';
import TeachersPage from './pages/TeachersPage';
import LessonsPage from './pages/LessonsPage';
import TrialLessonsPage from './pages/TrialLessonsPage';
import RoomsPage from './pages/RoomsPage';
import PackagesPage from './pages/PackagesPage';
import InvoicesPage from './pages/InvoicesPage';
import InventoryPage from './pages/InventoryPage';
import BarSellPage from './pages/bar/Sell';
import BarRegisterPage from './pages/bar/Register';
import BarInventoryPage from './pages/bar/Inventory';
import BarStaffPage from './pages/bar/Staff';
import AdminConsolePage from './pages/AdminConsolePage';
import LoginPage from './pages/LoginPage';
import SignupPage from './pages/public/SignupPage';
import TrialRequestPage from './pages/public/TrialRequestPage';
import ForgotPasswordPage from './pages/public/ForgotPasswordPage';
import ResetPasswordPage from './pages/public/ResetPasswordPage';
import ChangePasswordPage from './pages/account/ChangePasswordPage';
import TrialQueuePage from './pages/trials/TrialQueuePage';
import StudentProfilePage from './pages/students/StudentProfilePage';
import PackageListLite from './features/packages/PackageList';
import Payments from './features/payments/Payments';
import ReceiptView from './features/receipts/ReceiptView';
import TeacherLessons from './features/lessons/TeacherLessons';
import StudentLessons from './features/lessons/StudentLessons';
import StudentsByTeacher from './features/students/StudentsByTeacher';
import MetadataPage from './pages/Metadata';
import ErrorBoundary from './components/ErrorBoundary';
import { useAuth } from './auth/AuthProvider';

function RequireAuth() {
  const { isAuthenticated } = useAuth();
  const location = useLocation();

  if (!isAuthenticated) {
    return <Navigate to="/login" state={{ from: location }} replace />;
  }

  return <Outlet />;
}

export default function App() {
  return (
    <ErrorBoundary>
      <Routes>
        <Route path="/calendar" element={<CalendarPage />} />
        <Route path="/login" element={<LoginPage />} />
        <Route path="/signup" element={<SignupPage />} />
        <Route path="/forgot-password" element={<ForgotPasswordPage />} />
        <Route path="/reset-password" element={<ResetPasswordPage />} />
        <Route path="/trial" element={<TrialRequestPage />} />
        <Route element={<RequireAuth />}>
          <Route element={<Layout />}>
            <Route path="/" element={<Navigate to="/inicio" replace />} />
            <Route path="/inicio" element={<Placeholder title="Inicio" description="Selecciona un módulo para comenzar a trabajar." />} />
            <Route path="/account/password" element={<ChangePasswordPage />} />

            <Route path="/crm" element={<Outlet />}>
              <Route index element={<Navigate to="contactos" replace />} />
              <Route path="contactos" element={<PartiesPage />} />
              <Route path="empresas" element={<Placeholder title="CRM · Empresas" />} />
              <Route path="leads" element={<Placeholder title="CRM · Leads" />} />
            </Route>

            <Route path="/estudio" element={<Outlet />}>
              <Route index element={<Navigate to="calendario" replace />} />
              <Route path="calendario" element={<BookingsPage />} />
              <Route path="salas" element={<RoomsPage />} />
              <Route path="ordenes" element={<SessionsPage />} />
              <Route path="pipelines" element={<PipelinesPage />} />
              <Route path="reportes" element={<Placeholder title="Estudio · Reportes" />} />
            </Route>

            <Route path="/label" element={<Outlet />}>
              <Route index element={<Navigate to="artistas" replace />} />
              <Route path="artistas" element={<Placeholder title="Label · Artistas" />} />
              <Route path="proyectos" element={<Placeholder title="Label · Proyectos" />} />
              <Route path="releases" element={<Placeholder title="Label · Releases" />} />
              <Route path="tracks" element={<Placeholder title="Label · Tracks" />} />
              <Route path="assets" element={<Placeholder title="Label · Assets" />} />
              <Route path="metadata" element={<MetadataPage />} />
              <Route path="contratos" element={<Placeholder title="Label · Contratos" />} />
              <Route path="regalias" element={<Placeholder title="Label · Regalías" />} />
              <Route path="marketing" element={<Placeholder title="Label · Marketing" />} />
            </Route>

            <Route path="/eventos" element={<Outlet />}>
              <Route index element={<Navigate to="agenda" replace />} />
              <Route path="agenda" element={<Placeholder title="Eventos · Agenda" />} />
              <Route path="fechas-y-tours" element={<Placeholder title="Eventos · Fechas y tours" />} />
              <Route path="venues" element={<Placeholder title="Eventos · Venues" />} />
              <Route path="staff" element={<Placeholder title="Eventos · Staff" />} />
              <Route path="presupuestos" element={<Placeholder title="Eventos · Presupuestos" />} />
              <Route path="post-mortem" element={<Placeholder title="Eventos · Post-mortem" />} />
            </Route>

            <Route path="/escuela" element={<Outlet />}>
              <Route index element={<Navigate to="clases" replace />} />
              <Route path="profesores" element={<TeachersPage />} />
              <Route path="clases" element={<LessonsPage />} />
              <Route path="trial-lessons" element={<TrialLessonsPage />} />
              <Route path="trial-queue" element={<TrialQueuePage />} />
              <Route path="programas" element={<Placeholder title="Escuela · Programas" />} />
              <Route path="cursos" element={<Placeholder title="Escuela · Cursos" />} />
              <Route path="cohortes" element={<Placeholder title="Escuela · Cohortes" />} />
              <Route path="estudiantes" element={<Placeholder title="Escuela · Estudiantes" description="Lista de estudiantes matriculados. Click en un estudiante para ver su perfil y progreso." />} />
              <Route path="estudiantes/:id" element={<StudentProfilePage />} />
              <Route path="inscripciones" element={<Placeholder title="Escuela · Inscripciones" />} />
              <Route path="pagos" element={<Placeholder title="Escuela · Pagos" />} />
            </Route>

            <Route path="/finanzas" element={<Outlet />}>
              <Route index element={<Navigate to="facturas" replace />} />
              <Route path="cotizaciones" element={<Placeholder title="Finanzas · Cotizaciones" />} />
              <Route path="facturas" element={<InvoicesPage />} />
              <Route path="cobros" element={<Payments />} />
              <Route path="recibos/:receiptId" element={<ReceiptView />} />
              <Route path="regalias" element={<Placeholder title="Finanzas · Regalías" />} />
            </Route>

            <Route path="/bar" element={<Outlet />}>
              <Route index element={<Navigate to="sell" replace />} />
              <Route path="sell" element={<BarSellPage />} />
              <Route path="register" element={<BarRegisterPage />} />
              <Route path="inventory" element={<BarInventoryPage />} />
              <Route path="staff" element={<BarStaffPage />} />
            </Route>

            <Route path="/operacion" element={<Outlet />}>
              <Route index element={<Navigate to="inventario" replace />} />
              <Route path="calendario-domo" element={<CalendarPage />} />
              <Route path="inventario" element={<InventoryPage />} />
              <Route path="reservas-equipo" element={<Placeholder title="Operación · Reservas de equipo" />} />
              <Route path="mantenimiento" element={<Placeholder title="Operación · Mantenimiento" />} />
              <Route path="paquetes" element={<PackagesPage />} />
              <Route path="paquetes/resumen" element={<PackageListLite />} />
            </Route>

            <Route path="/configuracion" element={<Outlet />}>
              <Route index element={<Navigate to="roles-permisos" replace />} />
              <Route path="roles-permisos" element={<AdminConsolePage />} />
              <Route path="impuestos-series" element={<Placeholder title="Configuración · Impuestos y series" />} />
              <Route path="unidades-negocio" element={<Placeholder title="Configuración · Unidades de negocio" />} />
              <Route path="sedes" element={<Placeholder title="Configuración · Sedes" />} />
              <Route path="marcas" element={<Placeholder title="Configuración · Marcas" />} />
              <Route path="integraciones" element={<Placeholder title="Configuración · Integraciones" />} />
              <Route path="preferencias" element={<Placeholder title="Configuración · Preferencias" />} />
            </Route>

            <Route path="/insights" element={<Placeholder title="Insights" description="Panel de indicadores operativos y financieros (en desarrollo)." />} />

            {/* Alias de rutas anteriores para compatibilidad */}
            <Route path="/parties" element={<Navigate to="/crm/contactos" replace />} />
            <Route path="/bookings" element={<Navigate to="/estudio/calendario" replace />} />
            <Route path="/sessions" element={<Navigate to="/estudio/ordenes" replace />} />
            <Route path="/teachers" element={<Navigate to="/escuela/profesores" replace />} />
            <Route path="/lessons" element={<Navigate to="/escuela/clases" replace />} />
            <Route path="/trials" element={<Navigate to="/escuela/trial-lessons" replace />} />
            <Route path="/trials/queue" element={<Navigate to="/escuela/trial-queue" replace />} />
            <Route path="/rooms" element={<Navigate to="/estudio/salas" replace />} />
            <Route path="/pipelines" element={<Navigate to="/estudio/pipelines" replace />} />
            <Route path="/packages" element={<Navigate to="/operacion/paquetes" replace />} />
            <Route path="/invoices" element={<Navigate to="/finanzas/facturas" replace />} />
            <Route path="/inventory" element={<Navigate to="/operacion/inventario" replace />} />
            <Route path="/admin" element={<Navigate to="/configuracion/roles-permisos" replace />} />
            <Route path="/metadata" element={<Navigate to="/label/metadata" replace />} />

            {/* Legacy routes for backward compatibility */}
            <Route path="/students/:id" element={<Navigate to="/escuela/estudiantes/:id" replace />} />
            <Route path="/students/:studentId/lessons" element={<StudentLessons />} />
            
            {/* Feature routes */}
            <Route path="/finance/receipts/:receiptId" element={<ReceiptView />} />
            <Route path="/payments" element={<Payments />} />
            <Route path="/packages/lite" element={<PackageListLite />} />
            <Route path="/teachers/:teacherId/lessons" element={<TeacherLessons />} />
            <Route path="/teachers/:teacherId/students" element={<StudentsByTeacher />} />
            <Route path="*" element={<Navigate to="/inicio" replace />} />
          </Route>
        </Route>
      </Routes>
    </ErrorBoundary>
  );
}
