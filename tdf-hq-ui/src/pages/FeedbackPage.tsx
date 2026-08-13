import { useEffect, useState } from 'react';
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
import { useMutation, useQuery } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import { submitFeedback } from '../api/feedback';
import { Catalogs, type CatalogItem, type CatalogPage } from '../api/catalogs';
import { useSession } from '../session/SessionContext';
import { feedbackSchema, emailSchema } from '../lib/schemas';

const CATEGORY_CATALOG = 'feedback-categories';
const SEVERITY_CATALOG = 'feedback-severities';

const publishedItems = (page?: CatalogPage): CatalogItem[] =>
  page?.items.filter((item) => item.active && item.workflowState === 'published' && !item.deprecatedAt) ?? [];

const globalDefaultId = (page: CatalogPage | undefined, scopeKind: string): string =>
  page?.defaults.find((entry) =>
    entry.scopeKind === scopeKind && entry.scopeId === 'global' && !entry.localeId)?.entityId ?? '';

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
  const { i18n } = useTranslation();
  const [title, setTitle] = useState('');
  const [description, setDescription] = useState('');
  const [categoryId, setCategoryId] = useState('');
  const [severityId, setSeverityId] = useState('');
  const [contactEmail, setContactEmail] = useState(contactEmailFromSessionUsername(session?.username));
  const [consent, setConsent] = useState(false);
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

  const catalogQuery = useQuery({
    queryKey: ['catalogs', 'feedback-form', i18n.resolvedLanguage ?? i18n.language],
    queryFn: () => Catalogs.listPublicBatch([CATEGORY_CATALOG, SEVERITY_CATALOG], {
      locale: i18n.resolvedLanguage ?? i18n.language,
      page: 1,
      pageSize: 100,
    }),
    staleTime: 1000 * 60 * 10,
  });
  const categoryPage = catalogQuery.data?.catalogs.find((page) => page.catalog.code === CATEGORY_CATALOG);
  const severityPage = catalogQuery.data?.catalogs.find((page) => page.catalog.code === SEVERITY_CATALOG);
  const categories = publishedItems(categoryPage);
  const severities = publishedItems(severityPage);
  const defaultCategoryId = globalDefaultId(categoryPage, 'feedback-category');
  const defaultSeverityId = globalDefaultId(severityPage, 'feedback-severity');
  const catalogsReady = Boolean(
    categories.length
      && severities.length
      && categories.some((item) => item.id === defaultCategoryId)
      && severities.some((item) => item.id === defaultSeverityId),
  );

  useEffect(() => {
    if (!catalogsReady) return;
    if (!categories.some((item) => item.id === categoryId)) setCategoryId(defaultCategoryId);
    if (!severities.some((item) => item.id === severityId)) setSeverityId(defaultSeverityId);
  }, [catalogsReady, categories, categoryId, defaultCategoryId, defaultSeverityId, severities, severityId]);

  const resetForm = () => {
    setTitle('');
    setDescription('');
    setSeverityId(defaultSeverityId);
    setCategoryId(defaultCategoryId);
    setAttachment(null);
    setConsent(false);
  };

  const mutation = useMutation({
    mutationFn: () =>
      submitFeedback({
        title,
        description,
        categoryId,
        severityId,
        contactEmail: contactEmail.trim() || undefined,
        consent,
        attachment,
      }),
    onSuccess: () => {
      resetForm();
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
          {categories.map((category) => <Chip key={category.id} label={category.name} size="small" />)}
        </Stack>
      </Stack>

      {catalogQuery.isError && (
        <Alert severity="error">
          No se pudieron cargar las categorías y severidades publicadas. Intenta nuevamente antes de enviar.
        </Alert>
      )}

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
                value={categoryId}
                onChange={(e) => setCategoryId(e.target.value)}
                disabled={!catalogsReady}
                fullWidth
                {...register('category')}
              >
                {categories.map((opt) => (
                  <MenuItem key={opt.id} value={opt.id}>
                    {opt.name}
                  </MenuItem>
                ))}
              </TextField>
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                select
                label="Severidad"
                value={severityId}
                onChange={(e) => setSeverityId(e.target.value)}
                disabled={!catalogsReady}
                fullWidth
                {...register('severity')}
              >
                {severities.map((opt) => (
                  <MenuItem key={opt.id} value={opt.id}>
                    {opt.name}
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
            <Button variant="outlined" onClick={resetForm}>
              Limpiar
            </Button>
            <Button
              variant="contained"
              onClick={() => mutation.mutate()}
              disabled={mutation.isPending || !catalogsReady || !categoryId || !severityId || !title.trim() || !description.trim() || !consent}
            >
              {mutation.isPending ? 'Enviando…' : 'Enviar'}
            </Button>
          </Stack>
        </Stack>
      </Paper>
    </Box>
  );
}
