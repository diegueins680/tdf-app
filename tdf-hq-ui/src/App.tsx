import { Container } from '@mui/material';
import { Route, Routes, Navigate } from 'react-router-dom';
import TopBar from './components/TopBar';
import DashboardPage from './pages/DashboardPage';
import PartiesPage from './pages/PartiesPage';
import BookingsPage from './pages/BookingsPage';
import KanbanPage from './pages/KanbanPage';
import PackagesPage from './pages/PackagesPage';
import InvoicesPage from './pages/InvoicesPage';
import InventoryPage from './pages/InventoryPage';

export default function App() {
  return (
    <>
      <TopBar />
      <Container maxWidth="xl" sx={{ mt: 3, mb: 6 }}>
        <Routes>
          <Route path="/" element={<Navigate to="/dashboard" replace />} />
          <Route path="/dashboard" element={<DashboardPage />} />
          <Route path="/parties" element={<PartiesPage />} />
          <Route path="/bookings" element={<BookingsPage />} />
          <Route path="/kanban" element={<KanbanPage />} />
          <Route path="/packages" element={<PackagesPage />} />
          <Route path="/invoices" element={<InvoicesPage />} />
          <Route path="/inventory" element={<InventoryPage />} />
        </Routes>
      </Container>
    </>
  );
}
