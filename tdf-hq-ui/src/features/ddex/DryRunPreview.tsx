import React from 'react';
import {
  Box,
  Typography,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
  Chip,
  Alert,
  Grid,
  Card,
  CardContent,
  List,
  ListItem,
  ListItemText,
  Divider,
} from '@mui/material';
import {
  CheckCircle as CreateIcon,
  Update as UpdateIcon,
  Link as LinkIcon,
  Warning as WarningIcon,
} from '@mui/icons-material';
import { DryRunResult, ImportPlanChange, ImportPlanConflict } from '../../api/ddex';

interface DryRunPreviewProps {
  result: DryRunResult;
}

export const DryRunPreview: React.FC<DryRunPreviewProps> = ({ result }) => {
  const createCount = result.drrChanges.filter(c => c.ipchOperation === 'CreateEntity').length;
  const updateCount = result.drrChanges.filter(c => c.ipchOperation === 'UpdateEntity').length;
  const linkCount = result.drrChanges.filter(c => c.ipchOperation === 'LinkEntity').length;

  const getOperationIcon = (operation: string) => {
    switch (operation) {
      case 'CreateEntity':
        return <CreateIcon color="success" fontSize="small" />;
      case 'UpdateEntity':
        return <UpdateIcon color="primary" fontSize="small" />;
      case 'LinkEntity':
        return <LinkIcon color="info" fontSize="small" />;
      default:
        return null;
    }
  };

  const getOperationColor = (operation: string): 'success' | 'primary' | 'info' | 'default' => {
    switch (operation) {
      case 'CreateEntity':
        return 'success';
      case 'UpdateEntity':
        return 'primary';
      case 'LinkEntity':
        return 'info';
      default:
        return 'default';
    }
  };

  return (
    <Box>
      <Typography variant="h6" gutterBottom>
        Import Preview
      </Typography>

      {/* Summary Cards */}
      <Grid container spacing={2} sx={{ mb: 3 }}>
        <Grid item xs={12} sm={3}>
          <Card>
            <CardContent sx={{ textAlign: 'center' }}>
              <Typography variant="h3" color="success.main">{createCount}</Typography>
              <Typography variant="body2" color="text.secondary">
                Entities to Create
              </Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={12} sm={3}>
          <Card>
            <CardContent sx={{ textAlign: 'center' }}>
              <Typography variant="h3" color="primary.main">{updateCount}</Typography>
              <Typography variant="body2" color="text.secondary">
                Entities to Update
              </Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={12} sm={3}>
          <Card>
            <CardContent sx={{ textAlign: 'center' }}>
              <Typography variant="h3" color="info.main">{linkCount}</Typography>
              <Typography variant="body2" color="text.secondary">
                Links to Create
              </Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={12} sm={3}>
          <Card>
            <CardContent sx={{ textAlign: 'center' }}>
              <Typography variant="h3" color={result.drrConflicts.length > 0 ? 'warning.main' : 'success.main'}>
                {result.drrConflicts.length}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                Conflicts Detected
              </Typography>
            </CardContent>
          </Card>
        </Grid>
      </Grid>

      {/* Validity Status */}
      {result.drrIsValid ? (
        <Alert severity="success" sx={{ mb: 3 }}>
          Import is valid and can proceed.
        </Alert>
      ) : (
        <Alert severity="error" sx={{ mb: 3 }}>
          Import has blocking issues that must be resolved.
        </Alert>
      )}

      {/* Warnings */}
      {result.drrWarnings.length > 0 && (
        <Alert severity="warning" sx={{ mb: 3 }}>
          <Typography variant="subtitle2" gutterBottom>
            Warnings ({result.drrWarnings.length})
          </Typography>
          <List dense>
            {result.drrWarnings.map((warning, idx) => (
              <ListItem key={idx}>
                <ListItemText primary={warning} />
              </ListItem>
            ))}
          </List>
        </Alert>
      )}

      {/* Changes Table */}
      <Typography variant="subtitle1" gutterBottom>
        Proposed Changes
      </Typography>
      <TableContainer component={Paper} sx={{ mb: 3 }}>
        <Table size="small">
          <TableHead>
            <TableRow>
              <TableCell>Operation</TableCell>
              <TableCell>Entity Type</TableCell>
              <TableCell>Reference</TableCell>
              <TableCell>Description</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {result.drrChanges.map((change, idx) => (
              <TableRow key={idx}>
                <TableCell>
                  <Box display="flex" alignItems="center" gap={1}>
                    {getOperationIcon(change.ipchOperation)}
                    <Chip
                      label={change.ipchOperation.replace('Entity', '')}
                      size="small"
                      color={getOperationColor(change.ipchOperation)}
                    />
                  </Box>
                </TableCell>
                <TableCell>{change.ipchEntityType}</TableCell>
                <TableCell>
                  <Typography variant="body2" sx={{ fontFamily: 'monospace' }}>
                    {change.ipchEntityRef}
                  </Typography>
                </TableCell>
                <TableCell>{change.ipchDescription}</TableCell>
              </TableRow>
            ))}
            {result.drrChanges.length === 0 && (
              <TableRow>
                <TableCell colSpan={4} align="center">
                  <Typography color="text.secondary" py={2}>
                    No changes to import
                  </Typography>
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </TableContainer>

      {/* Conflicts Summary */}
      {result.drrConflicts.length > 0 && (
        <>
          <Typography variant="subtitle1" gutterBottom>
            Conflicts ({result.drrConflicts.length})
          </Typography>
          <Alert severity="warning" sx={{ mb: 2 }}>
            These conflicts must be resolved before importing. Click "Next" to resolve them.
          </Alert>
        </>
      )}
    </Box>
  );
};
