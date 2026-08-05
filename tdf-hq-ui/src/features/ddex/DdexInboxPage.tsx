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
  TablePagination,
  Chip,
  IconButton,
  Typography,
  TextField,
  MenuItem,
  Select,
  FormControl,
  InputLabel,
  Button,
  Stack,
  Alert,
  CircularProgress,
} from '@mui/material';
import {
  Visibility as ViewIcon,
  Download as DownloadIcon,
  CheckCircle as ValidIcon,
  Error as InvalidIcon,
  HourglassEmpty as PendingIcon,
} from '@mui/icons-material';
import { useQuery } from '@tanstack/react-query';
import { DDEX, getStatusColor } from '../../api/ddex';
import { DdexUploadDropzone } from './DdexUploadDropzone';

export const DdexInboxPage: React.FC = () => {
  const [page, setPage] = useState(0);
  const [rowsPerPage, setRowsPerPage] = useState(25);
  const [statusFilter, setStatusFilter] = useState<string>('');
  const [partnerFilter, setPartnerFilter] = useState<string>('');
  const [showUpload, setShowUpload] = useState(false);

  const { data: documents, isLoading, error, refetch } = useQuery({
    queryKey: ['ddex-documents', statusFilter, partnerFilter],
    queryFn: () => DDEX.listDocuments(statusFilter || undefined, partnerFilter || undefined),
  });

  const handleChangePage = (_event: unknown, newPage: number) => {
    setPage(newPage);
  };

  const handleChangeRowsPerPage = (event: React.ChangeEvent<HTMLInputElement>) => {
    setRowsPerPage(parseInt(event.target.value, 10));
    setPage(0);
  };

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'valid':
      case 'imported':
        return <ValidIcon color="success" />;
      case 'invalid':
      case 'import_failed':
        return <InvalidIcon color="error" />;
      default:
        return <PendingIcon color="action" />;
    }
  };

  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleString();
  };

  const handleUploadComplete = () => {
    setShowUpload(false);
    void refetch();
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
          Error loading DDEX documents: {error.message}
        </Alert>
      </Box>
    );
  }

  const paginatedDocuments = documents?.slice(
    page * rowsPerPage,
    page * rowsPerPage + rowsPerPage
  ) ?? [];

  return (
    <Box p={3}>
      <Stack direction="row" justifyContent="space-between" alignItems="center" mb={3}>
        <Typography variant="h4">DDEX Inbox</Typography>
        <Button variant="contained" onClick={() => setShowUpload(!showUpload)}>
          {showUpload ? 'Cancel' : 'Upload Document'}
        </Button>
      </Stack>

      {showUpload && (
        <Box mb={3}>
          <DdexUploadDropzone onUploadComplete={handleUploadComplete} />
        </Box>
      )}

      <Stack direction="row" spacing={2} mb={3}>
        <FormControl sx={{ minWidth: 200 }}>
          <InputLabel>Status</InputLabel>
          <Select
            value={statusFilter}
            label="Status"
            onChange={(e) => setStatusFilter(e.target.value)}
          >
            <MenuItem value="">All</MenuItem>
            <MenuItem value="received">Received</MenuItem>
            <MenuItem value="validating">Validating</MenuItem>
            <MenuItem value="valid">Valid</MenuItem>
            <MenuItem value="invalid">Invalid</MenuItem>
            <MenuItem value="ready_to_import">Ready to Import</MenuItem>
            <MenuItem value="imported">Imported</MenuItem>
          </Select>
        </FormControl>

        <TextField
          label="Partner"
          value={partnerFilter}
          onChange={(e) => setPartnerFilter(e.target.value)}
          sx={{ minWidth: 200 }}
        />
      </Stack>

      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>Status</TableCell>
              <TableCell>File Name</TableCell>
              <TableCell>Family</TableCell>
              <TableCell>Version</TableCell>
              <TableCell>Message ID</TableCell>
              <TableCell>Sender</TableCell>
              <TableCell>Received</TableCell>
              <TableCell align="right">Actions</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {paginatedDocuments.map((doc) => (
              <TableRow key={doc.ddexDocumentId} hover>
                <TableCell>
                  <Stack direction="row" spacing={1} alignItems="center">
                    {getStatusIcon(doc.ddexDocumentStatus)}
                    <Chip
                      label={doc.ddexDocumentStatus}
                      color={getStatusColor(doc.ddexDocumentStatus)}
                      size="small"
                    />
                  </Stack>
                </TableCell>
                <TableCell>{doc.ddexDocumentFileName}</TableCell>
                <TableCell>
                  <Chip label={doc.ddexDocumentFamily} size="small" variant="outlined" />
                </TableCell>
                <TableCell>{doc.ddexDocumentVersion}</TableCell>
                <TableCell>
                  <Typography variant="body2" sx={{ fontFamily: 'monospace' }}>
                    {doc.ddexDocumentMessageId || '-'}
                  </Typography>
                </TableCell>
                <TableCell>{doc.ddexDocumentSenderId || '-'}</TableCell>
                <TableCell>{formatDate(doc.ddexDocumentCreatedAt)}</TableCell>
                <TableCell align="right">
                  <IconButton
                    size="small"
                    color="primary"
                    href={`/label/ddex/documents/${doc.ddexDocumentId}`}
                  >
                    <ViewIcon />
                  </IconButton>
                  <IconButton
                    size="small"
                    color="default"
                    onClick={() => {
                      // TODO: Implement download
                      console.log('Download', doc.ddexDocumentId);
                    }}
                  >
                    <DownloadIcon />
                  </IconButton>
                </TableCell>
              </TableRow>
            ))}
            {paginatedDocuments.length === 0 && (
              <TableRow>
                <TableCell colSpan={8} align="center">
                  <Typography color="text.secondary" py={4}>
                    No DDEX documents found
                  </Typography>
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
        <TablePagination
          rowsPerPageOptions={[10, 25, 50, 100]}
          component="div"
          count={documents?.length ?? 0}
          rowsPerPage={rowsPerPage}
          page={page}
          onPageChange={handleChangePage}
          onRowsPerPageChange={handleChangeRowsPerPage}
        />
      </TableContainer>
    </Box>
  );
};
