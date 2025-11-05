import React from 'react';
import { ThemeProvider, createTheme, CssBaseline, Container, Typography, Box } from '@mui/material';
import UserRoleManagement from './components/UserRoleManagement';

const theme = createTheme({
  palette: {
    mode: 'light',
    primary: {
      main: '#1976d2',
    },
    secondary: {
      main: '#dc004e',
    },
  },
});

function App() {
  return (
    <ThemeProvider theme={theme}>
      <CssBaseline />
      <Container maxWidth="lg">
        <Box py={4}>
          <Typography variant="h3" component="h1" gutterBottom>
            TDF Records - User Role Management
          </Typography>
          <Typography variant="body1" color="text.secondary" paragraph>
            Manage user roles for the TDF Records platform. Users can have multiple roles assigned.
          </Typography>
          <UserRoleManagement />
        </Box>
      </Container>
    </ThemeProvider>
  );
}

export default App;
