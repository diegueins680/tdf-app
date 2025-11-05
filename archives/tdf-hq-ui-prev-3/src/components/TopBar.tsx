import { AppBar, Toolbar, Typography, Button } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';

export default function TopBar() {
  return (
    <AppBar position="static" color="primary" elevation={1}>
      <Toolbar sx={{ gap: 2 }}>
        <Typography variant="h6" sx={{ flexGrow: 1 }}>TDF HQ</Typography>
        <Button color="inherit" component={RouterLink} to="/parties">Parties</Button>
        <Button color="inherit" component={RouterLink} to="/bookings">Bookings</Button>
        <Button color="inherit" component={RouterLink} to="/kanban">Kanban</Button>
      </Toolbar>
    </AppBar>
  );
}
