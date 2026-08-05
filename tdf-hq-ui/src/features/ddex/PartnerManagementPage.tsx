import React, { useState } from 'react';
import {
  Box,
  Paper,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Typography,
  Button,
  Chip,
  IconButton,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  TextField,
  FormControlLabel,
  Switch,
  Stack,
  Alert,
  CircularProgress,
} from '@mui/material';
import {
  Add as AddIcon,
  Edit as EditIcon,
  Delete as DeleteIcon,
} from '@mui/icons-material';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { DDEX, DdexPartnerDTO, DdexPartnerCreateRequest } from '../../api/ddex';

export const PartnerManagementPage: React.FC = () => {
  const queryClient = useQueryClient();
  const [openDialog, setOpenDialog] = useState(false);
  const [editingPartner, setEditingPartner] = useState<DdexPartnerDTO | null>(null);
  const [formData, setFormData] = useState<DdexPartnerCreateRequest>({
    partnerName: '',
    partnerDpid: null,
    partnerAllowedVersions: ['4.3.2'],
  });

  const { data: partners, isLoading, error } = useQuery({
    queryKey: ['ddex-partners'],
    queryFn: () => DDEX.listPartners(),
  });

  const createMutation = useMutation({
    mutationFn: (data: DdexPartnerCreateRequest) => DDEX.createPartner(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['ddex-partners'] });
      handleCloseDialog();
    },
  });

  const handleOpenDialog = (partner?: DdexPartnerDTO) => {
    if (partner) {
      setEditingPartner(partner);
      setFormData({
        partnerName: partner.ddexPartnerName,
        partnerDpid: partner.ddexPartnerDpid,
        partnerAllowedVersions: partner.ddexPartnerAllowedVersions,
      });
    } else {
      setEditingPartner(null);
      setFormData({
        partnerName: '',
        partnerDpid: null,
        partnerAllowedVersions: ['4.3.2'],
      });
    }
    setOpenDialog(true);
  };

  const handleCloseDialog = () => {
    setOpenDialog(false);
    setEditingPartner(null);
    setFormData({
      partnerName: '',
      partnerDpid: null,
      partnerAllowedVersions: ['4.3.2'],
    });
  };

  const handleSubmit = () => {
    createMutation.mutate(formData);
  };

  if (isLoading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    );
  }

  if (error) {
    return (
      <Box p={3}>
        <Alert severity="error">
          Error loading partners: {(error as Error).message}
        </Alert>
      </Box>
    );
  }

  return (
    <Box p={3}>
      <Stack direction="row" justifyContent="space-between" alignItems="center" mb={3}>
        <Typography variant="h4">Partner Management</Typography>
        <Button
          variant="contained"
          startIcon={<AddIcon />}
          onClick={() => handleOpenDialog()}
        >
          Add Partner
        </Button>
      </Stack>

      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>Name</TableCell>
              <TableCell>DPID</TableCell>
              <TableCell>Allowed Versions</TableCell>
              <TableCell align="right">Actions</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {partners?.map((partner) => (
              <TableRow key={partner.ddexPartnerId} hover>
                <TableCell>{partner.ddexPartnerName}</TableCell>
                <TableCell>
                  <Typography variant="body2" sx={{ fontFamily: 'monospace' }}>
                    {partner.ddexPartnerDpid || '-'}
                  </Typography>
                </TableCell>
                <TableCell>
                  <Stack direction="row" spacing={1}>
                    {partner.ddexPartnerAllowedVersions.map((version) => (
                      <Chip key={version} label={version} size="small" />
                    ))}
                  </Stack>
                </TableCell>
                <TableCell align="right">
                  <IconButton
                    size="small"
                    color="primary"
                    onClick={() => handleOpenDialog(partner)}
                  >
                    <EditIcon />
                  </IconButton>
                  <IconButton
                    size="small"
                    color="error"
                    onClick={() => console.log('Delete', partner.ddexPartnerId)}
                  >
                    <DeleteIcon />
                  </IconButton>
                </TableCell>
              </TableRow>
            ))}
            {(!partners || partners.length === 0) && (
              <TableRow>
                <TableCell colSpan={4} align="center">
                  <Typography color="text.secondary" py={4}>
                    No partners configured
                  </Typography>
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </TableContainer>

      {/* Add/Edit Dialog */}
      <Dialog open={openDialog} onClose={handleCloseDialog} maxWidth="sm" fullWidth>
        <DialogTitle>
          {editingPartner ? 'Edit Partner' : 'Add Partner'}
        </DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            <TextField
              label="Partner Name"
              value={formData.partnerName}
              onChange={(e) => setFormData({ ...formData, partnerName: e.target.value })}
              fullWidth
              required
            />
            <TextField
              label="DPID"
              value={formData.partnerDpid || ''}
              onChange={(e) => setFormData({ ...formData, partnerDpid: e.target.value || null })}
              fullWidth
              placeholder="DPID:XXXXXXXX"
              helperText="DDEX Party Identifier (optional)"
            />
            <TextField
              label="Allowed Versions"
              value={formData.partnerAllowedVersions.join(', ')}
              onChange={(e) => setFormData({
                ...formData,
                partnerAllowedVersions: e.target.value.split(',').map(v => v.trim()).filter(Boolean),
              })}
              fullWidth
              helperText="Comma-separated list (e.g., 4.3.2, 4.2)"
            />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={handleCloseDialog}>Cancel</Button>
          <Button
            variant="contained"
            onClick={handleSubmit}
            disabled={!formData.partnerName || createMutation.isPending}
          >
            {createMutation.isPending ? 'Saving...' : 'Save'}
          </Button>
        </DialogActions>
      </Dialog>

      {createMutation.isError && (
        <Alert severity="error" sx={{ mt: 2 }}>
          Error: {(createMutation.error as Error).message}
        </Alert>
      )}
    </Box>
  );
};
