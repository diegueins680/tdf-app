import { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Checkbox,
  FormControl,
  FormControlLabel,
  InputLabel,
  MenuItem,
  Paper,
  Select,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { MerchReputation } from '../api/merchReputation';

const readText = (record: Record<string, unknown>, key: string) =>
  typeof record[key] === 'string' ? record[key] : '';

export default function MerchReputationModerationPage() {
  const queryClient = useQueryClient();
  const [selectedCase, setSelectedCase] = useState<string | null>(null);
  const [decision, setDecision] = useState('reject_report');
  const [reasonCode, setReasonCode] = useState('legitimate_negative_opinion');
  const [rationale, setRationale] = useState('');
  const [workflowAction, setWorkflowAction] = useState<
    'triage' | 'request_evidence' | 'provisionally_hide' | 'resume_review'
  >('triage');
  const [resolvingAppeal, setResolvingAppeal] = useState<string | null>(null);
  const [appealOutcome, setAppealOutcome] = useState<'upheld' | 'reversed'>('upheld');
  const [appealRationale, setAppealRationale] = useState('');
  const [selectedSuggestion, setSelectedSuggestion] = useState<string | null>(null);
  const [suggestionStatus, setSuggestionStatus] = useState<'testing' | 'approved' | 'rejected' | 'duplicate'>('testing');
  const [suggestionMinimumSample, setSuggestionMinimumSample] = useState(5);
  const [suggestionBiasPassed, setSuggestionBiasPassed] = useState(false);
  const [suggestionUtilityPassed, setSuggestionUtilityPassed] = useState(false);
  const [suggestionReason, setSuggestionReason] = useState('');
  const cases = useQuery({
    queryKey: ['merch-moderation-cases'],
    queryFn: () => MerchReputation.moderationCases(),
    retry: false,
  });
  const suggestions = useQuery({
    queryKey: ['merch-category-suggestions'],
    queryFn: () => MerchReputation.categorySuggestions(),
    retry: false,
  });
  const mutation = useMutation({
    mutationFn: (caseId: string) => MerchReputation.decide(caseId, {
      moderationDecision: decision,
      moderationReasonCode: reasonCode,
      moderationRationale: rationale,
      moderationEvidence: { reviewedInAdminPanel: true },
    }),
    onSuccess: async () => {
      setSelectedCase(null);
      setRationale('');
      await queryClient.invalidateQueries({ queryKey: ['merch-moderation-cases'] });
    },
  });
  const workflowMutation = useMutation({
    mutationFn: (caseId: string) => MerchReputation.transitionCase(caseId,workflowAction,rationale),
    onSuccess: async () => {
      setSelectedCase(null);
      setRationale('');
      await queryClient.invalidateQueries({ queryKey: ['merch-moderation-cases'] });
    },
  });
  const appealMutation = useMutation({
    mutationFn: (appealId: string) => MerchReputation.resolveAppeal(
      appealId,appealOutcome,appealRationale,
    ),
    onSuccess: async () => {
      setResolvingAppeal(null);
      setAppealRationale('');
      await queryClient.invalidateQueries({ queryKey: ['merch-moderation-cases'] });
    },
  });
  const suggestionMutation = useMutation({
    mutationFn: (suggestionId: string) => MerchReputation.decideCategorySuggestion(suggestionId, {
      suggestionStatus,
      suggestionMinimumSample,
      suggestionBiasTest: { passed: suggestionBiasPassed, reviewedInAdminPanel: true },
      suggestionUtilityTest: { passed: suggestionUtilityPassed, reviewedInAdminPanel: true },
      suggestionDecisionReason: suggestionReason,
    }),
    onSuccess: async () => {
      setSelectedSuggestion(null);
      setSuggestionReason('');
      setSuggestionBiasPassed(false);
      setSuggestionUtilityPassed(false);
      await queryClient.invalidateQueries({ queryKey: ['merch-category-suggestions'] });
    },
  });

  return (
    <Box sx={{ maxWidth: 980, mx: 'auto' }}>
      <Stack spacing={2}>
        <Typography component="h1" variant="h4" fontWeight={900}>Moderación de reputación de merch</Typography>
        <Alert severity="warning">
          Conserva la evidencia original. Distingue opinión negativa legítima de abuso y registra una
          justificación concreta. Las consecuencias financieras requieren otro flujo y revisión humana independiente.
        </Alert>
        <Paper component="section" variant="outlined" sx={{ p: 2 }}>
          <Typography component="h2" variant="h6" fontWeight={800}>Gobierno de categorías sugeridas</Typography>
          <Typography variant="body2" color="text.secondary">
            Ninguna sugerencia modifica el puntaje actual. Aprobar exige muestra mínima y evidencia real de sesgo y utilidad.
          </Typography>
          {suggestions.isLoading && <Typography role="status">Cargando sugerencias…</Typography>}
          {suggestions.isError && <Alert severity="error">No se pudieron cargar las sugerencias.</Alert>}
          <Stack spacing={1.5} sx={{ mt: 1.5 }}>
            {suggestions.data?.map((suggestion) => {
              const suggestionId = readText(suggestion,'suggestionId');
              return (
                <Card variant="outlined" key={suggestionId}>
                  <CardContent>
                    <Typography fontWeight={800}>{readText(suggestion,'label')} · {readText(suggestion,'status')}</Typography>
                    <Typography>{readText(suggestion,'definition')}</Typography>
                    {selectedSuggestion === suggestionId ? (
                      <Stack spacing={1.25} sx={{ mt: 1.5 }} component="form" onSubmit={(event) => {
                        event.preventDefault();
                        suggestionMutation.mutate(suggestionId);
                      }}>
                        <FormControl>
                          <InputLabel id={'suggestion-status-' + suggestionId}>Resultado</InputLabel>
                          <Select
                            labelId={'suggestion-status-' + suggestionId}
                            label="Resultado"
                            value={suggestionStatus}
                            onChange={(event) => setSuggestionStatus(event.target.value as typeof suggestionStatus)}
                          >
                            <MenuItem value="testing">Pasar a pruebas</MenuItem>
                            <MenuItem value="approved">Aprobar</MenuItem>
                            <MenuItem value="duplicate">Marcar duplicada</MenuItem>
                            <MenuItem value="rejected">Rechazar</MenuItem>
                          </Select>
                        </FormControl>
                        <TextField
                          type="number"
                          label="Muestra mínima"
                          value={suggestionMinimumSample}
                          onChange={(event) => setSuggestionMinimumSample(Number(event.target.value))}
                          inputProps={{ min: 5 }}
                        />
                        <FormControlLabel control={<Checkbox checked={suggestionBiasPassed}
                          onChange={(event) => setSuggestionBiasPassed(event.target.checked)} />}
                          label="Prueba de sesgo documentada y aprobada" />
                        <FormControlLabel control={<Checkbox checked={suggestionUtilityPassed}
                          onChange={(event) => setSuggestionUtilityPassed(event.target.checked)} />}
                          label="Prueba de utilidad documentada y aprobada" />
                        <TextField
                          label="Motivo y referencia de evidencia"
                          value={suggestionReason}
                          onChange={(event) => setSuggestionReason(event.target.value)}
                          inputProps={{ minLength: 20, maxLength: 3000 }}
                          multiline
                          required
                        />
                        {suggestionMutation.isError && <Alert severity="error">No se guardó la decisión de categoría.</Alert>}
                        <Stack direction="row" gap={1}>
                          <Button type="submit" variant="contained" disabled={suggestionMutation.isPending
                            || suggestionReason.trim().length < 20 || suggestionMinimumSample < 5
                            || (suggestionStatus === 'approved' && (!suggestionBiasPassed || !suggestionUtilityPassed))}>
                            Guardar decisión
                          </Button>
                          <Button onClick={() => setSelectedSuggestion(null)}>Cancelar</Button>
                        </Stack>
                      </Stack>
                    ) : (
                      <Button sx={{ mt: 1 }} onClick={() => setSelectedSuggestion(suggestionId)}>Revisar sugerencia</Button>
                    )}
                  </CardContent>
                </Card>
              );
            })}
          </Stack>
        </Paper>
        {cases.isLoading && <Typography role="status">Cargando casos…</Typography>}
        {cases.isError && (
          <Alert severity="error" action={<Button color="inherit" onClick={() => void cases.refetch()}>Reintentar</Button>}>
            Permisos insuficientes o moderación deshabilitada.
          </Alert>
        )}
        {cases.data?.map((moderationCase) => {
          const caseId = readText(moderationCase, 'caseId');
          const report = typeof moderationCase['report'] === 'object' && moderationCase['report'] !== null
            ? moderationCase['report'] as Record<string, unknown>
            : {};
          const appeals = Array.isArray(moderationCase['appeals'])
            ? moderationCase['appeals'].filter((appeal): appeal is Record<string, unknown> =>
              typeof appeal === 'object' && appeal !== null)
            : [];
          return (
            <Card variant="outlined" key={caseId}>
              <CardContent>
                <Stack spacing={1.5}>
                  <Typography component="h2" variant="h6" fontWeight={800}>
                    {readText(report, 'reason') || 'Reporte'} · {readText(moderationCase, 'state')}
                  </Typography>
                  <Typography>{readText(report, 'details') || 'Sin detalle adicional.'}</Typography>
                  {appeals.map((appeal) => {
                    const appealId = readText(appeal, 'id');
                    const state = readText(appeal, 'state');
                    return (
                      <Paper component="section" variant="outlined" sx={{ p: 2 }} key={appealId}>
                        <Typography fontWeight={800}>Apelación · {state}</Typography>
                        <Typography>{readText(appeal, 'grounds')}</Typography>
                        {resolvingAppeal === appealId ? (
                          <Stack spacing={1.5} component="form" sx={{ mt: 1 }} onSubmit={(event) => {
                            event.preventDefault();
                            appealMutation.mutate(appealId);
                          }}>
                            <FormControl>
                              <InputLabel id={'appeal-outcome-' + appealId}>Resultado</InputLabel>
                              <Select
                                labelId={'appeal-outcome-' + appealId}
                                label="Resultado"
                                value={appealOutcome}
                                onChange={(event) => setAppealOutcome(event.target.value as 'upheld' | 'reversed')}
                              >
                                <MenuItem value="upheld">Confirmar decisión</MenuItem>
                                <MenuItem value="reversed">Revertir y restaurar visibilidad</MenuItem>
                              </Select>
                            </FormControl>
                            <TextField
                              label="Justificación independiente"
                              multiline minRows={3} required
                              value={appealRationale}
                              onChange={(event) => setAppealRationale(event.target.value)}
                              inputProps={{ minLength: 20, maxLength: 3000 }}
                            />
                            {appealMutation.isError && <Alert severity="error">No se resolvió la apelación.</Alert>}
                            <Stack direction="row" gap={1}>
                              <Button type="submit" variant="contained"
                                disabled={appealRationale.trim().length < 20 || appealMutation.isPending}>
                                Registrar resultado
                              </Button>
                              <Button onClick={() => setResolvingAppeal(null)}>Cancelar</Button>
                            </Stack>
                          </Stack>
                        ) : state === 'open' || state === 'awaiting_evidence' ? (
                          <Button sx={{ mt: 1 }} onClick={() => setResolvingAppeal(appealId)}>
                            Revisar apelación
                          </Button>
                        ) : null}
                      </Paper>
                    );
                  })}
                  {selectedCase === caseId ? (
                    <Stack spacing={1.5} component="form" onSubmit={(event) => {
                      event.preventDefault();
                      mutation.mutate(caseId);
                    }}>
                      <FormControl>
                        <InputLabel id={'workflow-action-' + caseId}>Etapa previa</InputLabel>
                        <Select
                          labelId={'workflow-action-' + caseId}
                          label="Etapa previa"
                          value={workflowAction}
                          onChange={(event) => setWorkflowAction(event.target.value as typeof workflowAction)}
                        >
                          <MenuItem value="triage">Asignar y revisar</MenuItem>
                          <MenuItem value="request_evidence">Solicitar información adicional</MenuItem>
                          <MenuItem value="provisionally_hide">Ocultar provisionalmente por seguridad</MenuItem>
                          <MenuItem value="resume_review">Restaurar mientras continúa la revisión</MenuItem>
                        </Select>
                      </FormControl>
                      <FormControl>
                        <InputLabel id={'decision-' + caseId}>Decisión</InputLabel>
                        <Select
                          labelId={'decision-' + caseId}
                          label="Decisión"
                          value={decision}
                          onChange={(event) => setDecision(event.target.value)}
                        >
                          <MenuItem value="reject_report">Rechazar reporte / opinión legítima</MenuItem>
                          <MenuItem value="hide">Ocultar</MenuItem>
                          <MenuItem value="limit">Limitar</MenuItem>
                          <MenuItem value="restore">Restaurar</MenuItem>
                        </Select>
                      </FormControl>
                      <TextField
                        label="Código de motivo"
                        value={reasonCode}
                        onChange={(event) => setReasonCode(event.target.value)}
                      />
                      <TextField
                        label="Justificación auditable"
                        multiline
                        minRows={4}
                        value={rationale}
                        onChange={(event) => setRationale(event.target.value)}
                        inputProps={{ minLength: 20, maxLength: 3000 }}
                        required
                      />
                      <Button
                        type="button"
                        variant="outlined"
                        disabled={workflowMutation.isPending || rationale.trim().length < 20}
                        onClick={() => workflowMutation.mutate(caseId)}
                      >
                        Registrar etapa sin decidir
                      </Button>
                      {workflowMutation.isError && (
                        <Alert severity="error">No se registró la etapa de moderación.</Alert>
                      )}
                      {mutation.isError && <Alert severity="error">No se registró la decisión.</Alert>}
                      <Stack direction="row" gap={1}>
                        <Button type="submit" variant="contained" disabled={rationale.trim().length < 20 || mutation.isPending}>
                          Registrar decisión
                        </Button>
                        <Button onClick={() => setSelectedCase(null)}>Cancelar</Button>
                      </Stack>
                    </Stack>
                  ) : (
                    <Button sx={{ alignSelf: 'flex-start' }} onClick={() => setSelectedCase(caseId)}>
                      Revisar caso
                    </Button>
                  )}
                </Stack>
              </CardContent>
            </Card>
          );
        })}
      </Stack>
    </Box>
  );
}
