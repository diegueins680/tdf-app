
import { AppBar, Toolbar, Typography, Container, Button, Box } from '@mui/material';
import { Link, Route, Routes, Navigate, useLocation } from 'react-router-dom';
import PartiesPage from './pages/PartiesPage';
import BookingsPage from './pages/BookingsPage';
import PipelinesPage from './pages/PipelinesPage';

function NavButton({ to, children }: any) {
  const loc = useLocation();
  const active = loc.pathname.startsWith(to);
  return (
    <Button color={active ? 'secondary' : 'inherit'} component={Link} to={to}>
      {children}
    </Button>
  );
}

export default function App() {
  return (
    <Box sx={{ display: 'flex', flexDirection: 'column', minHeight: '100%' }}>
      <AppBar position="static">
        <Toolbar sx={{ gap: 2 }}>
          <Typography variant="h6" sx={{ flexGrow: 1 }}>TDF HQ</Typography>
          <NavButton to="/parties">Parties</NavButton>
          <NavButton to="/bookings">Bookings</NavButton>
          <NavButton to="/pipelines">Pipelines</NavButton>
        </Toolbar>
      </AppBar>
      <Container maxWidth="lg" sx={{ mt: 3, mb: 6, flexGrow: 1 }}>
        <Routes>
          <Route path="/" element={<Navigate to="/parties" replace />} />
          <Route path="/parties" element={<PartiesPage />} />
          <Route path="/bookings" element={<BookingsPage />} />
          <Route path="/pipelines" element={<PipelinesPage />} />
        </Routes>
      </Container>
    </Box>
  );
}
