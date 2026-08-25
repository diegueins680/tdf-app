import React from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import {
  Box,
  Paper,
  Tabs,
  Tab,
  Typography,
  Button,
  Stack,
  Chip,
  Alert,
  CircularProgress,
  Card,
  CardContent,
  Grid,
  List,
  ListItem,
  ListItemText,
  Divider,
} from '@mui/material';
import {
  ArrowBack as BackIcon,
  CheckCircle as ValidIcon,
  Error as InvalidIcon,
} from '@mui/icons-material';
import { useQuery } from '@tanstack/react-query';
import { DDEX } from '../../api/ddex';
import { useLocalePreferences } from '../../contexts/LocalePreferencesContext';

interface TabPanelProps {
  children?: React.ReactNode;
  index: number;
  value: number;
}

const TabPanel: React.FC<TabPanelProps> = ({ children, value, index }) => (
  <Box role="tabpanel" hidden={value !== index} py={3}>
    {value === index && children}
  </Box>
);

const DdexDocumentPage: React.FC = () => {
  const { id } = useParams<{ id: string }>();
  const navigate = useNavigate();
  const { locale } = useLocalePreferences();
  const [tabValue, setTabValue] = React.useState(0);

  const documentId = parseInt(id || '0', 10);

  const { data: document, isLoading: docLoading, error: docError } = useQuery({
    queryKey: ['ddex-document', documentId],
    queryFn: () => DDEX.getDocument(documentId),
    enabled: documentId > 0,
  });

  const { data: validationReport, error: validationError } = useQuery({
    queryKey: ['ddex-validation', documentId],
    queryFn: () => DDEX.getValidationReport(documentId),
    enabled: documentId > 0,
    retry: false,
  });

  if (docLoading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    );
  }

  if (docError || !document) {
    return (
      <Box p={3}>
        <Alert severity="error">
          Error loading document: {docError?.message || 'Document not found'}
        </Alert>
        <Button startIcon={<BackIcon />} onClick={() => navigate('/label/ddex')} sx={{ mt: 2 }}>
          Back to Inbox
        </Button>
      </Box>
    );
  }

  const handleTabChange = (_event: React.SyntheticEvent, newValue: number) => {
    setTabValue(newValue);
  };

  return (
    <Box p={3}>
      <Stack direction="row" justifyContent="space-between" alignItems="center" mb={3}>
        <Stack direction="row" spacing={2} alignItems="center">
          <Button startIcon={<BackIcon />} onClick={() => navigate('/label/ddex')}>
            Back
          </Button>
          <Typography variant="h4">{document.ddexDocumentFileName}</Typography>
          <Chip
            label={document.ddexDocumentWorkflowStateNameEs}
          />
        </Stack>
      </Stack>
      <Alert severity="warning" sx={{ mb: 3 }}>
        La descarga original, vista previa e importación permanecen deshabilitadas mientras sus operaciones de almacenamiento y confirmación transaccional estén incompletas.
      </Alert>

      <Paper sx={{ mb: 3 }}>
        <Tabs value={tabValue} onChange={handleTabChange}>
          <Tab label="Summary" />
          <Tab label="Validation" />
        </Tabs>
      </Paper>

      <TabPanel value={tabValue} index={0}>
        <Grid container spacing={3}>
          <Grid item xs={12} md={6}>
            <Card>
              <CardContent>
                <Typography variant="h6" gutterBottom>
                  Document Information
                </Typography>
                <List component="div" dense>
                  <ListItem component="div">
                    <ListItemText
                      primary="File Name"
                      secondary={document.ddexDocumentFileName}
                    />
                  </ListItem>
                  <Divider />
                  <ListItem component="div">
                    <ListItemText
                      primary="SHA-256"
                      secondary={
                        <Typography variant="body2" sx={{ fontFamily: 'monospace', fontSize: '0.75rem' }}>
                          {document.ddexDocumentSha256}
                        </Typography>
                      }
                    />
                  </ListItem>
                  <Divider />
                  <ListItem component="div">
                    <ListItemText
                      primary="Family"
                      secondary={document.ddexDocumentStandardCode}
                    />
                  </ListItem>
                  <Divider />
                  <ListItem component="div">
                    <ListItemText
                      primary="Version"
                      secondary={document.ddexDocumentVersionCode}
                    />
                  </ListItem>
                  <Divider />
                  <ListItem component="div">
                    <ListItemText
                      primary="Received"
                      secondary={new Date(document.ddexDocumentCreatedAt).toLocaleString()}
                    />
                  </ListItem>
                </List>
              </CardContent>
            </Card>
          </Grid>
          <Grid item xs={12} md={6}>
            <Card>
              <CardContent>
                <Typography variant="h6" gutterBottom>
                  Message Header
                </Typography>
                <List component="div" dense>
                  <ListItem component="div">
                    <ListItemText
                      primary="Message ID"
                      secondary={
                        <Typography variant="body2" sx={{ fontFamily: 'monospace' }}>
                          {document.ddexDocumentMessageId || '-'}
                        </Typography>
                      }
                    />
                  </ListItem>
                  <Divider />
                  <ListItem component="div">
                    <ListItemText
                      primary="Sender DPID"
                      secondary={document.ddexDocumentSenderId || '-'}
                    />
                  </ListItem>
                  <Divider />
                  <ListItem component="div">
                    <ListItemText
                      primary="Recipient DPID"
                      secondary={document.ddexDocumentRecipientId || '-'}
                    />
                  </ListItem>
                </List>
              </CardContent>
            </Card>
          </Grid>
        </Grid>
      </TabPanel>

      <TabPanel value={tabValue} index={1}>
        <Card>
          <CardContent>
            {validationError ? (
              <Alert severity="info">
                No existe todavía un reporte de validación publicado para este documento.
              </Alert>
            ) : validationReport ? <><Stack direction="row" spacing={2} alignItems="center" mb={2}>
              {validationReport?.reportIsValid ? (
                <>
                  <ValidIcon color="success" />
                  <Typography variant="h6" color="success.main">
                    Document is valid
                  </Typography>
                </>
              ) : (
                <>
                  <InvalidIcon color="error" />
                  <Typography variant="h6" color="error.main">
                    Document has validation errors
                  </Typography>
                </>
              )}
            </Stack>
            
            {validationReport?.reportIssues && validationReport.reportIssues.length > 0 ? (
              <List>
                {validationReport.reportIssues.map((issue, idx) => (
                  <ListItem key={idx}>
                    <ListItemText
                      primary={
                        <Stack direction="row" spacing={1} alignItems="center">
                          <Chip
                            label={locale.startsWith('en')
                              ? issue.issueSeverityNameEn
                              : issue.issueSeverityNameEs}
                            size="small"
                          />
                          <Chip
                            label={locale.startsWith('en')
                              ? issue.issueLayerNameEn
                              : issue.issueLayerNameEs}
                            size="small"
                            variant="outlined"
                          />
                          <Typography>{issue.issueMessage}</Typography>
                        </Stack>
                      }
                      secondary={
                        issue.issueLine
                          ? `Line ${issue.issueLine}${issue.issueColumn ? `, Column ${issue.issueColumn}` : ''}`
                          : undefined
                      }
                    />
                  </ListItem>
                ))}
              </List>
            ) : (
              <Typography color="text.secondary">
                No validation issues found
              </Typography>
            )}
            </> : null}
          </CardContent>
        </Card>
      </TabPanel>

      <TabPanel value={tabValue} index={2}>
        <Alert severity="info">
          La vista previa tipada de importación aún no está disponible en esta revisión.
        </Alert>
      </TabPanel>

      <TabPanel value={tabValue} index={3}>
        <Card>
          <CardContent>
            <Typography variant="h6" gutterBottom>
              Document History
            </Typography>
            <Typography color="text.secondary">
              History tracking not yet implemented
            </Typography>
          </CardContent>
        </Card>
      </TabPanel>
    </Box>
  );
};

export default DdexDocumentPage;
