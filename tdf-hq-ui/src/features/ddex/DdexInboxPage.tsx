import React, { useMemo, useState } from 'react';
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
  CheckCircle as ValidIcon,
  Error as InvalidIcon,
  HourglassEmpty as PendingIcon,
} from '@mui/icons-material';
import { useQuery } from '@tanstack/react-query';
import { DDEX, getStatusColor } from '../../api/ddex';
import { useSession } from '../../session/SessionContext';
import { accessRequestPath, evaluateFeatureAccess, getFeatureById } from '../featureRegistry';
import { Link as RouterLink } from 'react-router-dom';
import { useTranslation } from 'react-i18next';

const inboxCopy = {
  es: {
    error: 'No se pudieron cargar los documentos DDEX',
    errors: 'errores',
    pending: 'pendientes',
    status: 'Estado',
    partner: 'Partner',
    all: 'Todos',
    fileName: 'Archivo',
    family: 'Familia',
    version: 'Versión',
    messageId: 'ID de mensaje',
    sender: 'Remitente',
    received: 'Recibido',
    actions: 'Acciones',
    empty: 'No se encontraron documentos DDEX',
    view: 'Ver documento DDEX',
    rowsPerPage: 'Filas por página:',
  },
  en: {
    error: 'DDEX documents could not be loaded',
    errors: 'errors',
    pending: 'pending',
    status: 'Status',
    partner: 'Partner',
    all: 'All',
    fileName: 'File name',
    family: 'Family',
    version: 'Version',
    messageId: 'Message ID',
    sender: 'Sender',
    received: 'Received',
    actions: 'Actions',
    empty: 'No DDEX documents found',
    view: 'View DDEX document',
    rowsPerPage: 'Rows per page:',
  },
} as const;

const statusLabels = {
  es: { received: 'Recibido', validating: 'Validando', valid: 'Válido', invalid: 'Inválido', ready_to_import: 'Listo para importar', imported: 'Importado' },
  en: { received: 'Received', validating: 'Validating', valid: 'Valid', invalid: 'Invalid', ready_to_import: 'Ready to import', imported: 'Imported' },
} as const;

const DdexInboxPage: React.FC = () => {
  const [page, setPage] = useState(0);
  const [rowsPerPage, setRowsPerPage] = useState(25);
  const [statusFilter, setStatusFilter] = useState<string>('');
  const [partnerFilter, setPartnerFilter] = useState<string>('');
  const { i18n } = useTranslation();
  const locale: 'es' | 'en' = (i18n.resolvedLanguage ?? i18n.language ?? 'es').toLowerCase().startsWith('en') ? 'en' : 'es';
  const text = inboxCopy[locale];
  const { session } = useSession();
  const inboxFeature = getFeatureById('label.ddex.inbox');
  const importDecision = inboxFeature ? evaluateFeatureAccess(inboxFeature, {
    authenticated: Boolean(session), roles: session?.roles, modules: session?.modules,
  }, 'import') : null;

  const { data: documents, isLoading, error } = useQuery({
    queryKey: ['ddex-documents', statusFilter, partnerFilter],
    queryFn: () => DDEX.listDocuments(statusFilter || undefined, partnerFilter || undefined),
  });
  const { data: summaryDocuments } = useQuery({
    queryKey: ['ddex-documents', 'authorized-summary'],
    queryFn: () => DDEX.listDocuments(),
  });
  const statusCounts = useMemo(() => ({
    errors: (summaryDocuments ?? []).filter((document) => ['invalid', 'import_failed', 'quarantined'].includes(document.ddexDocumentStatus)).length,
    pending: (summaryDocuments ?? []).filter((document) => ['received', 'queued', 'validating', 'mapping_required', 'ready_to_import', 'importing'].includes(document.ddexDocumentStatus)).length,
  }), [summaryDocuments]);

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
    return new Date(dateString).toLocaleString(locale === 'en' ? 'en-US' : 'es-EC');
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
          {text.error}: {error.message}
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
      <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" alignItems={{ xs: 'stretch', sm: 'center' }} mb={2} gap={1}>
        <Typography variant="h4">DDEX / Bandeja</Typography>
        <Button component={RouterLink} to="/label/ddex/partners" variant="outlined" sx={{ minHeight: 44 }}>
          DDEX / Partners
        </Button>
      </Stack>
      <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap mb={2} role="status" aria-label="Estado DDEX autorizado">
        <Chip color="error" variant="outlined" label={`${statusCounts.errors} ${text.errors}`} />
        <Chip variant="outlined" label={`${statusCounts.pending} ${text.pending}`} sx={{ color: '#7a4100', borderColor: '#9a5200' }} />
      </Stack>
      {importDecision?.state === 'locked' && inboxFeature ? (
        <Alert severity="info" sx={{ mb: 2 }} action={(
          <Button component={RouterLink} to={accessRequestPath(inboxFeature, 'import')} sx={{ minHeight: 44 }}>
            Solicitar acceso
          </Button>
        )}>
          Importar DDEX requiere una categoría de acceso adicional. No se muestran datos protegidos.
        </Alert>
      ) : (
        <Alert severity="warning" sx={{ mb: 2 }}>
          La carga, descarga original y confirmación transaccional de importaciones siguen en beta y permanecen deshabilitadas hasta completar almacenamiento privado y rollback.
        </Alert>
      )}

      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} mb={3}>
        <FormControl sx={{ minWidth: { sm: 200 } }}>
          <InputLabel id="ddex-status-filter-label">{text.status}</InputLabel>
          <Select
            id="ddex-status-filter"
            labelId="ddex-status-filter-label"
            value={statusFilter}
            label={text.status}
            onChange={(e) => setStatusFilter(e.target.value)}
          >
            <MenuItem value="">{text.all}</MenuItem>
            {Object.entries(statusLabels[locale]).map(([value, label]) => <MenuItem key={value} value={value}>{label}</MenuItem>)}
          </Select>
        </FormControl>

        <TextField
          label={text.partner}
          value={partnerFilter}
          onChange={(e) => setPartnerFilter(e.target.value)}
          sx={{ minWidth: { sm: 200 } }}
        />
      </Stack>

      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>{text.status}</TableCell>
              <TableCell>{text.fileName}</TableCell>
              <TableCell>{text.family}</TableCell>
              <TableCell>{text.version}</TableCell>
              <TableCell>{text.messageId}</TableCell>
              <TableCell>{text.sender}</TableCell>
              <TableCell>{text.received}</TableCell>
              <TableCell align="right">{text.actions}</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {paginatedDocuments.map((doc) => (
              <TableRow key={doc.ddexDocumentId} hover>
                <TableCell>
                  <Stack direction="row" spacing={1} alignItems="center">
                    {getStatusIcon(doc.ddexDocumentStatus)}
                    <Chip
                      label={statusLabels[locale][doc.ddexDocumentStatus as keyof typeof statusLabels.es] ?? doc.ddexDocumentStatus}
                      color={getStatusColor(doc.ddexDocumentStatus)}
                      variant="outlined"
                      size="small"
                      sx={getStatusColor(doc.ddexDocumentStatus) === 'warning' ? { color: '#7a4100', borderColor: '#9a5200' } : undefined}
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
                    color="primary"
                    component={RouterLink}
                    to={`/label/ddex/documents/${doc.ddexDocumentId}`}
                    aria-label={`${text.view} ${doc.ddexDocumentFileName}`}
                    sx={{ minWidth: 44, minHeight: 44 }}
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
                    {text.empty}
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
          labelRowsPerPage={text.rowsPerPage}
          getItemAriaLabel={(type) => locale === 'en' ? `${type} page` : `Página ${type}`}
          sx={{ '& .MuiIconButton-root': { width: 44, height: 44 } }}
        />
      </TableContainer>
    </Box>
  );
};

export default DdexInboxPage;
