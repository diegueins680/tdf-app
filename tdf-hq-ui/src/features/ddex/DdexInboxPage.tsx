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
  MenuItem,
  Select,
  FormControl,
  InputLabel,
  Stack,
  Alert,
  CircularProgress,
} from '@mui/material';
import {
  Visibility as ViewIcon,
} from '@mui/icons-material';
import { useQuery } from '@tanstack/react-query';
import { DDEX } from '../../api/ddex';

const DdexInboxPage: React.FC = () => {
  const [page, setPage] = useState(0);
  const [rowsPerPage, setRowsPerPage] = useState(25);
  const [statusFilter, setStatusFilter] = useState<string>('');

  const { data: references, isLoading: referencesLoading, error: referencesError } = useQuery({
    queryKey: ['ddex-references', 'es'],
    queryFn: () => DDEX.getReferences('es'),
  });

  const { data: documents, isLoading, error } = useQuery({
    queryKey: ['ddex-documents', statusFilter],
    queryFn: () => DDEX.listDocuments(statusFilter || undefined),
  });

  const handleChangePage = (_event: unknown, newPage: number) => {
    setPage(newPage);
  };

  const handleChangeRowsPerPage = (event: React.ChangeEvent<HTMLInputElement>) => {
    setRowsPerPage(parseInt(event.target.value, 10));
    setPage(0);
  };

  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleString();
  };

  if (isLoading || referencesLoading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    );
  }

  if (error || referencesError) {
    return (
      <Box p={3}>
        <Alert severity="error">
          Error loading DDEX documents: {(error || referencesError)?.message}
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
      <Typography variant="h4" mb={3}>DDEX Inbox</Typography>

      <Stack direction="row" spacing={2} mb={3}>
        <FormControl sx={{ minWidth: 200 }}>
          <InputLabel>Status</InputLabel>
          <Select
            value={statusFilter}
            label="Status"
            onChange={(e) => setStatusFilter(e.target.value)}
          >
            <MenuItem value="">All</MenuItem>
            {references?.ddexReferenceDocumentStates.map((state) => (
              <MenuItem key={state.ddexDocumentStateId} value={state.ddexDocumentStateId}>
                {state.ddexDocumentStateName}
              </MenuItem>
            ))}
          </Select>
        </FormControl>
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
                    <Chip
                      label={doc.ddexDocumentWorkflowStateNameEs}
                      size="small"
                    />
                  </Stack>
                </TableCell>
                <TableCell>{doc.ddexDocumentFileName}</TableCell>
                <TableCell>
                  <Chip label={doc.ddexDocumentStandardCode} size="small" variant="outlined" />
                </TableCell>
                <TableCell>{doc.ddexDocumentVersionCode}</TableCell>
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
                    aria-label={`View DDEX document ${doc.ddexDocumentFileName}`}
                  >
                    <ViewIcon />
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

export default DdexInboxPage;
