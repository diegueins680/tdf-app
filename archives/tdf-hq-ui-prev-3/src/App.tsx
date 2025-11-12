import { Container } from '@mui/material';
import { Route, Routes, Navigate } from 'react-router-dom';
import TopBar from './components/TopBar';
import PartiesPage from './pages/PartiesPage';
import BookingsPage from './pages/BookingsPage';
import KanbanPage from './pages/KanbanPage';

export default function App() {
  return (
    <>
      <TopBar />
      <Container maxWidth="lg" sx={{ mt: 3, mb: 6 }}>
        <Routes>
          <Route path="/" element={<Navigate to="/parties" replace />} />
          <Route path="/parties" element={<PartiesPage />} />
          <Route path="/bookings" element={<BookingsPage />} />
          <Route path="/kanban" element={<KanbanPage />} />
        </Routes>
      </Container>
    </>
  );
}
