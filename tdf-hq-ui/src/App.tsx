import { Container, Typography, Box } from '@mui/material'
import UserRoleManagement from './components/UserRoleManagement'

function App() {
  return (
    <Container maxWidth="lg">
      <Box sx={{ my: 4 }}>
        <Typography variant="h3" component="h1" gutterBottom>
          TDF Records - User Management
        </Typography>
        <UserRoleManagement />
      </Box>
    </Container>
  )
}

export default App
