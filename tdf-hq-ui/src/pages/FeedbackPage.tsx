import { useState } from 'react';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';
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
import { feedbackSchema, emailSchema } from '../lib/schemas';

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

export const contactEmailFromSessionUsername = (username?: string): string =>
  username?.includes('@') ? username : '';

const feedbackFormSchema = feedbackSchema.extend({
  category: z.string(),
  severity: z.string(),
  contactEmail: emailSchema.optional().or(z.literal('')),
});
type FeedbackFormData = z.infer<typeof feedbackFormSchema>;

export default function FeedbackPage() {
  const { session } = useSession();
  const [attachment, setAttachment] = useState<File | null>(null);
  const { register, handleSubmit, reset, getValues, formState: { errors } } = useForm<FeedbackFormData>({
    resolver: zodResolver(feedbackFormSchema),
    defaultValues: {
      title: '',
      description: '',
      category: 'bug',
      severity: 'P2',
      contactEmail: contactEmailFromSessionUsername(session?.username),
      consent: false,
    },
  });

  const mutation = useMutation({
    mutationFn: (payload: {
      title: string;
      description: string;
      category: string;
      severity: string;
      contactEmail?: string;
      consent: boolean;
      attachment: File | null;
    }) => submitFeedback(payload),
    onSuccess: () => {
      const currentEmail = getValues('contactEmail');
      reset({
        title: '',
        description: '',
        category: 'bug',
        severity: 'P2',
        contactEmail: currentEmail,
        consent: false,
      });
      setAttachment(null);
    },
  });

  const onSubmit = handleSubmit((data) => {
    mutation.mutate({
      title: data.title,
      description: data.description,
      category: data.category,
      severity: data.severity,
      contactEmail: data.contactEmail?.trim() || undefined,
      consent: data.consent,
      attachment,
    });
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
                {...register('title')}
                label="Título"
                required
                fullWidth
                error={Boolean(errors.title)}
                helperText={errors.title?.message}
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                select
                label="Categoría"
                fullWidth
                {...register('category')}
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
                fullWidth
                {...register('severity')}
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
                {...register('description')}
                label="Descripción"
                required
                fullWidth
                multiline
                minRows={4}
                error={Boolean(errors.description)}
                helperText={errors.description?.message}
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField
                {...register('contactEmail')}
                label="Correo de contacto"
                placeholder="Opcional si queremos hacer seguimiento"
                fullWidth
                error={Boolean(errors.contactEmail)}
                helperText={errors.contactEmail?.message}
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
                    {...register('consent')}
                  />
                }
                label="Autorizo usar esta información para mejoras internas y seguimiento."
              />
              {errors.consent && (
                <Typography variant="caption" color="error" sx={{ display: 'block', ml: 4 }}>
                  {errors.consent.message}
                </Typography>
              )}
            </Grid>
          </Grid>

          <Stack direction="row" spacing={2} justifyContent="flex-end">
            <Button variant="outlined" onClick={() => {
              const currentEmail = getValues('contactEmail');
              reset({
                title: '',
                description: '',
                category: 'bug',
                severity: 'P2',
                contactEmail: currentEmail,
                consent: false,
              });
              setAttachment(null);
            }}>
              Limpiar
            </Button>
            <Button
              variant="contained"
              onClick={onSubmit}
              disabled={mutation.isPending}
            >
              {mutation.isPending ? 'Enviando…' : 'Enviar'}
            </Button>
          </Stack>
        </Stack>
      </Paper>
    </Box>
  );
}
