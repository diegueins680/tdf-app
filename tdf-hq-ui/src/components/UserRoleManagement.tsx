import { useState } from 'react'
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import {
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
  Select,
  MenuItem,
  FormControl,
  CircularProgress,
  Alert,
  Typography,
  Box,
  Chip,
  SelectChangeEvent,
} from '@mui/material'
import { getUsers, updateUserRole, PartyRole } from '../api/client'

const ROLE_OPTIONS: PartyRole['role'][] = [
  'AdminRole',
  'ManagerRole',
  'EngineerRole',
  'TeacherRole',
  'ReceptionRole',
  'AccountingRole',
  'ReadOnlyRole',
  'CustomerRole',
  'ArtistRole',
  'StudentRole',
]

const ROLE_DISPLAY: Record<PartyRole['role'], string> = {
  AdminRole: 'Admin',
  ManagerRole: 'Manager',
  EngineerRole: 'Engineer',
  TeacherRole: 'Teacher',
  ReceptionRole: 'Reception',
  AccountingRole: 'Accounting',
  ReadOnlyRole: 'Read Only',
  CustomerRole: 'Customer',
  ArtistRole: 'Artist',
  StudentRole: 'Student',
}

const ROLE_COLORS: Record<PartyRole['role'], 'primary' | 'secondary' | 'success' | 'error' | 'warning' | 'info' | 'default'> = {
  AdminRole: 'error',
  ManagerRole: 'primary',
  EngineerRole: 'info',
  TeacherRole: 'success',
  ReceptionRole: 'secondary',
  AccountingRole: 'warning',
  ReadOnlyRole: 'default',
  CustomerRole: 'default',
  ArtistRole: 'secondary',
  StudentRole: 'info',
}

export default function UserRoleManagement() {
  const queryClient = useQueryClient()
  const [successMessage, setSuccessMessage] = useState<string | null>(null)
  const [errorMessage, setErrorMessage] = useState<string | null>(null)

  const { data: users, isLoading, error } = useQuery({
    queryKey: ['users'],
    queryFn: getUsers,
  })

  const updateRoleMutation = useMutation({
    mutationFn: ({ userId, role }: { userId: number; role: PartyRole['role'] }) =>
      updateUserRole(userId, role),
    onSuccess: (response) => {
      if (response.urrSuccess) {
        setSuccessMessage(response.urrMessage)
        setErrorMessage(null)
        queryClient.invalidateQueries({ queryKey: ['users'] })
        setTimeout(() => setSuccessMessage(null), 3000)
      } else {
        setErrorMessage(response.urrMessage)
        setSuccessMessage(null)
      }
    },
    onError: (error: Error) => {
      setErrorMessage(`Failed to update role: ${error.message}`)
      setSuccessMessage(null)
    },
  })

  const handleRoleChange = (userId: number, event: SelectChangeEvent<PartyRole['role']>) => {
    const newRole = event.target.value as PartyRole['role']
    updateRoleMutation.mutate({ userId, role: newRole })
  }

  if (isLoading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    )
  }

  if (error) {
    return (
      <Alert severity="error">
        Error loading users: {error instanceof Error ? error.message : 'Unknown error'}
      </Alert>
    )
  }

  return (
    <Box>
      <Typography variant="h5" component="h2" gutterBottom>
        User Role Management
      </Typography>

      {successMessage && (
        <Alert severity="success" sx={{ mb: 2 }}>
          {successMessage}
        </Alert>
      )}

      {errorMessage && (
        <Alert severity="error" sx={{ mb: 2 }}>
          {errorMessage}
        </Alert>
      )}

      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell><strong>User ID</strong></TableCell>
              <TableCell><strong>Name</strong></TableCell>
              <TableCell><strong>Email</strong></TableCell>
              <TableCell><strong>Current Role</strong></TableCell>
              <TableCell><strong>Change Role</strong></TableCell>
              <TableCell><strong>Status</strong></TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {users && users.length > 0 ? (
              users.map((user) => (
                <TableRow key={user.uwpUserId}>
                  <TableCell>{user.uwpUserId}</TableCell>
                  <TableCell>{user.uwpName}</TableCell>
                  <TableCell>{user.uwpEmail || 'N/A'}</TableCell>
                  <TableCell>
                    <Chip
                      label={ROLE_DISPLAY[user.uwpRole]}
                      color={ROLE_COLORS[user.uwpRole]}
                      size="small"
                    />
                  </TableCell>
                  <TableCell>
                    <FormControl size="small" fullWidth>
                      <Select
                        value={user.uwpRole}
                        onChange={(e) => handleRoleChange(user.uwpUserId, e)}
                        disabled={updateRoleMutation.isPending}
                      >
                        {ROLE_OPTIONS.map((role) => (
                          <MenuItem key={role} value={role}>
                            {ROLE_DISPLAY[role]}
                          </MenuItem>
                        ))}
                      </Select>
                    </FormControl>
                  </TableCell>
                  <TableCell>
                    <Chip
                      label={user.uwpIsActive ? 'Active' : 'Inactive'}
                      color={user.uwpIsActive ? 'success' : 'default'}
                      size="small"
                    />
                  </TableCell>
                </TableRow>
              ))
            ) : (
              <TableRow>
                <TableCell colSpan={6} align="center">
                  No users found
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </TableContainer>
    </Box>
  )
}
