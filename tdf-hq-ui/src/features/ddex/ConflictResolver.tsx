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
  FormControl,
  InputLabel,
  Select,
  MenuItem,
  Alert,
  Stack,
  Card,
  CardContent,
} from '@mui/material';
import {
  Warning as WarningIcon,
  Error as ErrorIcon,
} from '@mui/icons-material';
import type { ImportPlanConflict } from '../../api/ddex';

interface ConflictResolverProps {
  conflicts: ImportPlanConflict[];
  resolutions: Map<number, string>;
  onResolutionChange: (conflictId: number, action: string) => void;
}

const resolutionOptions = [
  { value: 'UseExisting', label: 'Use Existing Entity' },
  { value: 'CreateNew', label: 'Create New Entity' },
  { value: 'Merge', label: 'Merge Data' },
  { value: 'Ignore', label: 'Ignore Conflict' },
  { value: 'ManualReview', label: 'Mark for Manual Review' },
];

export const ConflictResolver: React.FC<ConflictResolverProps> = ({
  conflicts,
  resolutions,
  onResolutionChange,
}) => {
  const resolvedCount = resolutions.size;
  const totalCount = conflicts.length;
  const allResolved = resolvedCount === totalCount;

  const getConflictSeverity = (conflictType: string): 'error' | 'warning' => {
    switch (conflictType) {
      case 'DuplicateIsrc':
      case 'DuplicateUpc':
        return 'error';
      case 'DataMismatch':
      case 'MissingReference':
        return 'warning';
      default:
        return 'warning';
    }
  };

  const getConflictColor = (conflictType: string): 'error' | 'warning' | 'info' => {
    switch (conflictType) {
      case 'DuplicateIsrc':
      case 'DuplicateUpc':
        return 'error';
      case 'DuplicateParty':
        return 'warning';
      case 'DataMismatch':
        return 'warning';
      case 'MissingReference':
        return 'info';
      default:
        return 'warning';
    }
  };

  return (
    <Box>
      <Stack direction="row" justifyContent="space-between" alignItems="center" sx={{ mb: 3 }}>
        <Typography variant="h6">
          Resolve Conflicts
        </Typography>
        <Chip
          label={`${resolvedCount} / ${totalCount} resolved`}
          color={allResolved ? 'success' : 'warning'}
        />
      </Stack>

      {!allResolved && (
        <Alert severity="info" sx={{ mb: 3 }}>
          Please select a resolution action for each conflict before proceeding.
        </Alert>
      )}

      {allResolved && (
        <Alert severity="success" sx={{ mb: 3 }}>
          All conflicts have been resolved. You can proceed with the import.
        </Alert>
      )}

      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell width="50">Status</TableCell>
              <TableCell>Conflict Type</TableCell>
              <TableCell>Entity</TableCell>
              <TableCell>Identifier</TableCell>
              <TableCell>Description</TableCell>
              <TableCell width="200">Resolution</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {conflicts.map((conflict) => {
              const isResolved = resolutions.has(conflict.conflictId);
              const selectedAction = resolutions.get(conflict.conflictId) || conflict.ipcSuggestedAction;

              return (
                <TableRow
                  key={conflict.conflictId}
                  sx={{
                    bgcolor: isResolved ? 'success.50' : 'warning.50',
                    '&:hover': { bgcolor: isResolved ? 'success.100' : 'warning.100' },
                  }}
                >
                  <TableCell>
                    {isResolved ? (
                      <Chip label="Resolved" size="small" color="success" />
                    ) : (
                      <Chip label="Pending" size="small" color="warning" />
                    )}
                  </TableCell>
                  <TableCell>
                    <Chip
                      label={conflict.ipcConflictType}
                      size="small"
                      color={getConflictColor(conflict.ipcConflictType)}
                    />
                  </TableCell>
                  <TableCell>{conflict.ipcEntityType}</TableCell>
                  <TableCell>
                    <Typography variant="body2" sx={{ fontFamily: 'monospace' }}>
                      {conflict.ipcIdentifier}
                    </Typography>
                  </TableCell>
                  <TableCell>{conflict.ipcDescription}</TableCell>
                  <TableCell>
                    <FormControl size="small" fullWidth>
                      <InputLabel>Action</InputLabel>
                      <Select
                        value={selectedAction}
                        label="Action"
                        onChange={(e) => onResolutionChange(conflict.conflictId, e.target.value)}
                      >
                        {resolutionOptions.map((option) => (
                          <MenuItem key={option.value} value={option.value}>
                            {option.label}
                          </MenuItem>
                        ))}
                      </Select>
                    </FormControl>
                  </TableCell>
                </TableRow>
              );
            })}
          </TableBody>
        </Table>
      </TableContainer>

      {/* Conflict Details */}
      {conflicts.length > 0 && (
        <Box sx={{ mt: 3 }}>
          <Typography variant="subtitle2" gutterBottom>
            Conflict Details
          </Typography>
          <Stack spacing={2}>
            {conflicts.map((conflict) => (
              <Card key={conflict.conflictId} variant="outlined">
                <CardContent>
                  <Stack direction="row" spacing={2} alignItems="center">
                    {getConflictSeverity(conflict.ipcConflictType) === 'error' ? (
                      <ErrorIcon color="error" />
                    ) : (
                      <WarningIcon color="warning" />
                    )}
                    <Typography variant="subtitle1">
                      {conflict.ipcConflictType}: {conflict.ipcIdentifier}
                    </Typography>
                  </Stack>
                  <Typography variant="body2" color="text.secondary" sx={{ mt: 1 }}>
                    {conflict.ipcDescription}
                  </Typography>
                  {conflict.ipcExistingId && (
                    <Typography variant="body2" sx={{ mt: 1 }}>
                      Existing entity ID: <strong>{conflict.ipcExistingId}</strong>
                    </Typography>
                  )}
                  <Typography variant="body2" sx={{ mt: 1 }}>
                    Suggested action: <Chip label={conflict.ipcSuggestedAction} size="small" />
                  </Typography>
                </CardContent>
              </Card>
            ))}
          </Stack>
        </Box>
      )}
    </Box>
  );
};
