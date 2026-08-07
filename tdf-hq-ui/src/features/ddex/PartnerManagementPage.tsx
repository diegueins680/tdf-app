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
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  TextField,
  Stack,
  Alert,
  CircularProgress,
} from '@mui/material';
import { Add as AddIcon } from '@mui/icons-material';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { DDEX } from '../../api/ddex';
import type { DdexPartnerCreateRequest } from '../../api/ddex';

const PartnerManagementPage: React.FC = () => {
  const queryClient = useQueryClient();
  const [openDialog, setOpenDialog] = useState(false);
  const [formData, setFormData] = useState<DdexPartnerCreateRequest>({
    partnerName: '',
    partnerDpid: null,
    partnerAllowedVersions: ['4.3'],
  });

  const { data: partners, isLoading, error } = useQuery({
    queryKey: ['ddex-partners'],
    queryFn: () => DDEX.listPartners(),
  });

  const createMutation = useMutation({
    mutationFn: (data: DdexPartnerCreateRequest) => DDEX.createPartner(data),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ['ddex-partners'] });
      handleCloseDialog();
    },
  });

  const handleOpenDialog = () => {
    setFormData({
      partnerName: '',
      partnerDpid: null,
      partnerAllowedVersions: ['4.3'],
    });
    setOpenDialog(true);
  };

  const handleCloseDialog = () => {
    setOpenDialog(false);
    setFormData({
      partnerName: '',
      partnerDpid: null,
      partnerAllowedVersions: ['4.3'],
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
          Error loading partners: {error.message}
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
          onClick={handleOpenDialog}
          sx={{ minHeight: 44 }}
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
              </TableRow>
            ))}
            {(!partners || partners.length === 0) && (
              <TableRow>
                <TableCell colSpan={3} align="center">
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
          Add Partner
        </DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            <TextField
              label="Partner Name"
              value={formData.partnerName}
              onChange={(e) => setFormData({ ...formData, partnerName: e.target.value })}
              fullWidth
              required
              inputProps={{ maxLength: 160 }}
            />
            <TextField
              label="DPID"
              value={formData.partnerDpid || ''}
              onChange={(e) => setFormData({ ...formData, partnerDpid: e.target.value || null })}
              fullWidth
              placeholder="DPID:XXXXXXXX"
              helperText="DDEX Party Identifier (optional)"
              inputProps={{ maxLength: 200 }}
            />
            <TextField
              label="Allowed Versions"
              value={formData.partnerAllowedVersions.join(', ')}
              onChange={(e) => setFormData({
                ...formData,
                partnerAllowedVersions: e.target.value.split(',').map(v => v.trim()).filter(Boolean),
              })}
              fullWidth
              error={formData.partnerAllowedVersions.length === 0 || formData.partnerAllowedVersions.some((version) => !['3.8.2', '4.2', '4.3'].includes(version))}
              helperText="Valores admitidos: 3.8.2, 4.2, 4.3"
            />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={handleCloseDialog} sx={{ minHeight: 44 }}>Cancel</Button>
          <Button
            variant="contained"
            onClick={handleSubmit}
            disabled={!formData.partnerName.trim() || createMutation.isPending || formData.partnerAllowedVersions.length === 0 || formData.partnerAllowedVersions.some((version) => !['3.8.2', '4.2', '4.3'].includes(version))}
            sx={{ minHeight: 44 }}
          >
            {createMutation.isPending ? 'Saving...' : 'Save'}
          </Button>
        </DialogActions>
      </Dialog>

      {createMutation.isError && (
        <Alert severity="error" sx={{ mt: 2 }}>
          Error: {createMutation.error.message}
        </Alert>
      )}
    </Box>
  );
};

export default PartnerManagementPage;
