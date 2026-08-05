import React, { useState } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import {
  Box,
  Paper,
  Stepper,
  Step,
  StepLabel,
  Button,
  Typography,
  Alert,
  CircularProgress,
  Stack,
} from '@mui/material';
import {
  ArrowBack as BackIcon,
  ArrowForward as NextIcon,
} from '@mui/icons-material';
import { useQuery, useMutation } from '@tanstack/react-query';
import { DDEX, ImportPlanDTO, DryRunResult } from '../../api/ddex';
import { DryRunPreview } from './DryRunPreview';
import { ConflictResolver } from './ConflictResolver';

const steps = ['Review Changes', 'Resolve Conflicts', 'Confirm Import'];

export const ImportWizardPage: React.FC = () => {
  const { id } = useParams<{ id: string }>();
  const navigate = useNavigate();
  const [activeStep, setActiveStep] = useState(0);
  const [dryRunResult, setDryRunResult] = useState<DryRunResult | null>(null);
  const [resolutions, setResolutions] = useState<Map<number, string>>(new Map());

  const documentId = parseInt(id || '0', 10);

  // Fetch document details
  const { data: document, isLoading: docLoading } = useQuery({
    queryKey: ['ddex-document', documentId],
    queryFn: () => DDEX.getDocument(documentId),
    enabled: documentId > 0,
  });

  // Create import plan mutation
  const createPlanMutation = useMutation({
    mutationFn: () => DDEX.createImportPlan(documentId),
    onSuccess: (plan) => {
      // After creating plan, run dry-run
      dryRunMutation.mutate(plan.importPlanId);
    },
  });

  // Dry-run mutation
  const dryRunMutation = useMutation({
    mutationFn: (planId: number) => DDEX.dryRunImport(planId),
    onSuccess: (result) => {
      setDryRunResult(result);
    },
  });

  // Commit import mutation
  const commitMutation = useMutation({
    mutationFn: (planId: number) => DDEX.commitImportPlan(planId),
    onSuccess: () => {
      navigate(`/label/ddex/documents/${documentId}`);
    },
  });

  const handleStartImport = () => {
    createPlanMutation.mutate();
  };

  const handleNext = () => {
    setActiveStep((prev) => prev + 1);
  };

  const handleBack = () => {
    setActiveStep((prev) => prev - 1);
  };

  const handleResolutionChange = (conflictId: number, action: string) => {
    setResolutions((prev) => {
      const next = new Map(prev);
      next.set(conflictId, action);
      return next;
    });
  };

  const handleCommit = () => {
    if (createPlanMutation.data) {
      commitMutation.mutate(createPlanMutation.data.importPlanId);
    }
  };

  const isLoading = docLoading || createPlanMutation.isPending || dryRunMutation.isPending;
  const hasConflicts = dryRunResult?.drrConflicts && dryRunResult.drrConflicts.length > 0;
  const allConflictsResolved = hasConflicts &&
    dryRunResult!.drrConflicts.every((c) => resolutions.has(c.conflictId));

  const renderStepContent = () => {
    switch (activeStep) {
      case 0:
        return dryRunResult ? (
          <DryRunPreview result={dryRunResult} />
        ) : (
          <Box textAlign="center" py={4}>
            <Button
              variant="contained"
              size="large"
              onClick={handleStartImport}
              disabled={isLoading}
            >
              Start Import Preview
            </Button>
          </Box>
        );

      case 1:
        return dryRunResult && hasConflicts ? (
          <ConflictResolver
            conflicts={dryRunResult.drrConflicts}
            resolutions={resolutions}
            onResolutionChange={handleResolutionChange}
          />
        ) : (
          <Alert severity="success">No conflicts to resolve!</Alert>
        );

      case 2:
        return (
          <Box>
            <Typography variant="h6" gutterBottom>
              Confirm Import
            </Typography>
            <Alert severity="info" sx={{ mb: 2 }}>
              You are about to import this DDEX document into the catalog.
              This action will create new entities and cannot be undone.
            </Alert>
            {dryRunResult && (
              <Box>
                <Typography variant="body2">
                  <strong>Entities to create:</strong> {dryRunResult.drrChanges.filter(c => c.ipchOperation === 'CreateEntity').length}
                </Typography>
                <Typography variant="body2">
                  <strong>Entities to update:</strong> {dryRunResult.drrChanges.filter(c => c.ipchOperation === 'UpdateEntity').length}
                </Typography>
                <Typography variant="body2">
                  <strong>Links to create:</strong> {dryRunResult.drrChanges.filter(c => c.ipchOperation === 'LinkEntity').length}
                </Typography>
              </Box>
            )}
          </Box>
        );

      default:
        return null;
    }
  };

  if (docLoading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    );
  }

  return (
    <Box p={3}>
      <Stack direction="row" spacing={2} alignItems="center" mb={3}>
        <Button startIcon={<BackIcon />} onClick={() => navigate(`/label/ddex/documents/${documentId}`)}>
          Back to Document
        </Button>
        <Typography variant="h4">Import Wizard</Typography>
      </Stack>

      {document && (
        <Typography variant="subtitle1" color="text.secondary" mb={3}>
          Importing: {document.ddexDocumentFileName}
        </Typography>
      )}

      <Paper sx={{ p: 3, mb: 3 }}>
        <Stepper activeStep={activeStep} sx={{ mb: 4 }}>
          {steps.map((label) => (
            <Step key={label}>
              <StepLabel>{label}</StepLabel>
            </Step>
          ))}
        </Stepper>

        {isLoading && (
          <Box display="flex" justifyContent="center" py={4}>
            <CircularProgress />
          </Box>
        )}

        {!isLoading && renderStepContent()}

        <Stack direction="row" spacing={2} justifyContent="flex-end" mt={4}>
          <Button
            disabled={activeStep === 0}
            onClick={handleBack}
          >
            Back
          </Button>

          {activeStep < steps.length - 1 ? (
            <Button
              variant="contained"
              endIcon={<NextIcon />}
              onClick={handleNext}
              disabled={activeStep === 0 && !dryRunResult}
            >
              Next
            </Button>
          ) : (
            <Button
              variant="contained"
              color="primary"
              onClick={handleCommit}
              disabled={commitMutation.isPending || (hasConflicts && !allConflictsResolved)}
            >
              {commitMutation.isPending ? 'Importing...' : 'Confirm Import'}
            </Button>
          )}
        </Stack>
      </Paper>

      {commitMutation.isError && (
        <Alert severity="error">
          Import failed: {(commitMutation.error as Error).message}
        </Alert>
      )}
    </Box>
  );
};
