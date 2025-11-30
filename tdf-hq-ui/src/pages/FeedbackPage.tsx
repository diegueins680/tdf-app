import { useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Chip,
  Grid,
  MenuItem,
  Paper,
  Stack,
  TextField,
  Typography,
  Checkbox,
  FormControlLabel,
} from '@mui/material';
import UploadFileIcon from '@mui/icons-material/UploadFile';
import { useMutation } from '@tanstack/react-query';
import { submitFeedback } from '../api/feedback';
import { useSession } from '../session/SessionContext';

const categories = [
  { value: 'bug', label: 'Bug' },
  { value: 'idea', label: 'Idea' },
  { value: 'ux', label: 'UX' },
  { value: 'datos', label: 'Datos' },
];

const severities = [
  { value: 'P1', label: 'P1 - Crítico' },
  { value: 'P2', label: 'P2 - Alto' },
  { value: 'P3', label: 'P3 - Medio' },
  { value: 'P4', label: 'P4 - Bajo' },
];

export default function FeedbackPage() {
  const { session } = useSession();
  const [title, setTitle] = useState('');
  const [description, setDescription] = useState('');
  const [category, setCategory] = useState('bug');
  const [severity, setSeverity] = useState('P2');
  const [contactEmail, setContactEmail] = useState(session?.username ?? '');
  const [consent, setConsent] = useState(false);
  const [attachment, setAttachment] = useState<File | null>(null);

  const mutation = useMutation({
    mutationFn: () =>
      submitFeedback({
        title,
        description,
        category,
        severity,
        contactEmail: contactEmail.trim() || undefined,
        consent,
        attachment,
      }),
    onSuccess: () => {
      setTitle('');
      setDescription('');
      setSeverity('P2');
      setCategory('bug');
      setAttachment(null);
      setConsent(false);
    },
  });

  const attachmentLabel = attachment
    ? `${attachment.name} (${Math.round(attachment.size / 1024)} KB)`
    : 'Adjuntar captura o documento (opcional)';

  return (
    <Box>
      <Stack spacing={2} sx={{ mb: 3 }}>
        <Typography variant="h4" fontWeight={800}>
          Sugerencias y bugs
        </Typography>
        <Typography variant="body1" color="text.secondary">
          Cuéntanos qué esperas del sistema y reporta cualquier problema. Las notificaciones se envían a Diego y al
          equipo para priorizar rápido.
        </Typography>
        <Stack direction="row" spacing={1}>
          <Chip label="Bug" size="small" />
          <Chip label="Idea" size="small" />
          <Chip label="UX" size="small" />
          <Chip label="Datos" size="small" />
        </Stack>
      </Stack>

      {mutation.isError && (
        <Alert severity="error">
          {mutation.error instanceof Error ? mutation.error.message : 'No se pudo enviar tu feedback.'}
        </Alert>
      )}
      {mutation.isSuccess && <Alert severity="success">Recibido. ¡Gracias!</Alert>}

      <Paper sx={{ p: 3 }}>
        <Stack spacing={2}>
          <Grid container spacing={2}>
            <Grid item xs={12} md={6}>
              <TextField
                label="Título"
                value={title}
                onChange={(e) => setTitle(e.target.value)}
                required
                fullWidth
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                select
                label="Categoría"
                value={category}
                onChange={(e) => setCategory(e.target.value)}
                fullWidth
              >
                {categories.map((opt) => (
                  <MenuItem key={opt.value} value={opt.value}>
                    {opt.label}
                  </MenuItem>
                ))}
              </TextField>
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                select
                label="Severidad"
                value={severity}
                onChange={(e) => setSeverity(e.target.value)}
                fullWidth
              >
                {severities.map((opt) => (
                  <MenuItem key={opt.value} value={opt.value}>
                    {opt.label}
                  </MenuItem>
                ))}
              </TextField>
            </Grid>
            <Grid item xs={12}>
              <TextField
                label="Descripción"
                value={description}
                onChange={(e) => setDescription(e.target.value)}
                required
                fullWidth
                multiline
                minRows={4}
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField
                label="Correo de contacto"
                value={contactEmail}
                onChange={(e) => setContactEmail(e.target.value)}
                placeholder="Opcional si queremos hacer seguimiento"
                fullWidth
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <Button component="label" startIcon={<UploadFileIcon />} variant="outlined">
                {attachmentLabel}
                <input
                  type="file"
                  hidden
                  onChange={(e) => {
                    const file = e.target.files?.[0];
                    if (file) setAttachment(file);
                  }}
                />
              </Button>
            </Grid>
            <Grid item xs={12}>
              <FormControlLabel
                control={
                  <Checkbox
                    checked={consent}
                    onChange={(e) => setConsent(e.target.checked)}
                  />
                }
                label="Autorizo usar esta información para mejoras internas y seguimiento."
              />
            </Grid>
          </Grid>

          <Stack direction="row" spacing={2} justifyContent="flex-end">
            <Button variant="outlined" onClick={() => {
              setTitle('');
              setDescription('');
              setSeverity('P2');
              setCategory('bug');
              setAttachment(null);
              setConsent(false);
            }}>
              Limpiar
            </Button>
            <Button
              variant="contained"
              onClick={() => mutation.mutate()}
              disabled={mutation.isPending || !title.trim() || !description.trim() || !consent}
            >
              {mutation.isPending ? 'Enviando…' : 'Enviar'}
            </Button>
          </Stack>
        </Stack>
      </Paper>
    </Box>
  );
}
