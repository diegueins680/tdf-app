import { useEffect, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  ButtonGroup,
  CircularProgress,
  FormControlLabel,
  IconButton,
  Paper,
  Stack,
  Switch,
  TextField,
  Typography,
} from '@mui/material';
import ArrowDownwardIcon from '@mui/icons-material/ArrowDownward';
import ArrowUpwardIcon from '@mui/icons-material/ArrowUpward';
import {
  MerchReputation,
  type MerchNotificationPreferences,
  type MerchReputationPriorities,
} from '../api/merchReputation';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';

type SubjectKind = 'store' | 'product';
const emptyNotifications: MerchNotificationPreferences = {
  reviewInvitation: false,
  reviewReminder: false,
  sellerResponseNotification: false,
  moderationChange: false,
  evidenceRequest: false,
  appealResult: false,
  badgeChange: false,
};

export default function MerchReputationPreferencesPage() {
  const { locale } = useLocalePreferences();
  const isEnglish = locale === 'en';
  const queryClient = useQueryClient();
  const [kind, setKind] = useState<SubjectKind>('store');
  const [dimensions, setDimensions] = useState<MerchReputationPriorities['orderedDimensions']>([]);
  const [notifications, setNotifications] = useState(emptyNotifications);
  const [suggestionLabel, setSuggestionLabel] = useState('');
  const [suggestionDefinition, setSuggestionDefinition] = useState('');
  const query = useQuery({
    queryKey: ['merch-reputation-priorities', kind],
    queryFn: () => MerchReputation.priorities(kind),
    retry: false,
  });
  useEffect(() => setDimensions(query.data?.orderedDimensions ?? []), [query.data]);
  const notificationQuery = useQuery({
    queryKey: ['merch-reputation-notification-preferences'],
    queryFn: MerchReputation.notificationPreferences,
  });
  useEffect(() => setNotifications(notificationQuery.data ?? emptyNotifications), [notificationQuery.data]);
  const mutation = useMutation({
    mutationFn: () => MerchReputation.savePriorities(
      kind,
      dimensions.map((dimension) => dimension.code),
      query.data?.revision ?? 0,
    ),
    onSuccess: async (saved) => {
      queryClient.setQueryData(['merch-reputation-priorities', kind], saved);
      await queryClient.invalidateQueries({ queryKey: ['merch-reputation-priorities', kind] });
    },
  });
  const notificationMutation = useMutation({
    mutationFn: () => MerchReputation.saveNotificationPreferences(notifications),
    onSuccess: (saved) => queryClient.setQueryData(
      ['merch-reputation-notification-preferences'],saved,
    ),
  });
  const suggestionMutation = useMutation({
    mutationFn: () => MerchReputation.suggestCategory(kind,suggestionLabel,suggestionDefinition),
    onSuccess: () => {
      setSuggestionLabel('');
      setSuggestionDefinition('');
    },
  });
  const move = (index: number, delta: -1 | 1) => setDimensions((current) => {
    const destination = index + delta;
    if (destination < 0 || destination >= current.length) return current;
    const next = [...current];
    [next[index], next[destination]] = [next[destination]!, next[index]!];
    return next;
  });

  return (
    <Box sx={{ maxWidth: 760, mx: 'auto', py: 3, px: { xs: 1.5, sm: 0 } }}>
      <Typography variant="h4" component="h1" fontWeight={900} gutterBottom>
        {isEnglish ? 'Merch discovery priorities' : 'Prioridades para descubrir merch'}
      </Typography>
      <Alert severity="info" sx={{ mb: 2 }}>
        {isEnglish
          ? 'This order personalizes discovery. It never changes a store or product public score.'
          : 'Este orden personaliza el descubrimiento. Nunca cambia el puntaje público de una tienda o producto.'}
      </Alert>
      <ButtonGroup aria-label={isEnglish ? 'Reputation subject' : 'Sujeto de reputación'} sx={{ mb: 2 }}>
        <Button variant={kind === 'store' ? 'contained' : 'outlined'} onClick={() => setKind('store')}>
          {isEnglish ? 'Stores' : 'Tiendas'}
        </Button>
        <Button variant={kind === 'product' ? 'contained' : 'outlined'} onClick={() => setKind('product')}>
          {isEnglish ? 'Products' : 'Productos'}
        </Button>
      </ButtonGroup>
      {query.isLoading && <CircularProgress aria-label={isEnglish ? 'Loading priorities' : 'Cargando prioridades'} />}
      {query.isError && <Alert severity="error">{isEnglish ? 'Could not load priorities.' : 'No se pudieron cargar las prioridades.'}</Alert>}
      <Stack component="ol" spacing={1.25} sx={{ listStyle: 'none', p: 0 }} aria-live="polite">
        {dimensions.map((dimension, index) => (
          <Paper component="li" variant="outlined" key={dimension.code} sx={{ p: 2, display: 'flex', gap: 2, alignItems: 'center' }}>
            <Box sx={{ flex: 1 }}>
              <Typography fontWeight={800}>{isEnglish ? dimension.nameEn : dimension.nameEs}</Typography>
              <Typography variant="body2" color="text.secondary">
                {isEnglish ? dimension.definitionEn : dimension.definitionEs}
              </Typography>
            </Box>
            <Stack direction="row">
              <IconButton
                aria-label={(isEnglish ? 'Move up ' : 'Subir ') + (isEnglish ? dimension.nameEn : dimension.nameEs)}
                disabled={index === 0}
                onClick={() => move(index, -1)}
              ><ArrowUpwardIcon /></IconButton>
              <IconButton
                aria-label={(isEnglish ? 'Move down ' : 'Bajar ') + (isEnglish ? dimension.nameEn : dimension.nameEs)}
                disabled={index === dimensions.length - 1}
                onClick={() => move(index, 1)}
              ><ArrowDownwardIcon /></IconButton>
            </Stack>
          </Paper>
        ))}
      </Stack>
      {mutation.isError && <Alert severity="error" sx={{ mt: 2 }}>{isEnglish ? 'Changes were not saved. Reload and retry.' : 'No se guardaron los cambios. Recarga e intenta de nuevo.'}</Alert>}
      {mutation.isSuccess && <Alert severity="success" sx={{ mt: 2 }}>{isEnglish ? 'Priorities saved.' : 'Prioridades guardadas.'}</Alert>}
      <Button
        variant="contained"
        sx={{ mt: 2 }}
        disabled={!dimensions.length || mutation.isPending}
        onClick={() => mutation.mutate()}
      >
        {mutation.isPending ? (isEnglish ? 'Saving…' : 'Guardando…') : (isEnglish ? 'Save order' : 'Guardar orden')}
      </Button>
      <Paper component="section" variant="outlined" sx={{ p: 2, mt: 3 }}>
        <Typography component="h2" variant="h6" fontWeight={800}>
          {isEnglish ? 'Suggest another category' : 'Sugerir otra categoría'}
        </Typography>
        <Typography variant="body2" color="text.secondary" sx={{ mb: 2 }}>
          {isEnglish
            ? 'Suggestions are moderated for duplicates, clarity, sample size, bias and utility. They do not affect today’s score.'
            : 'Las sugerencias se moderan por duplicados, claridad, muestra, sesgo y utilidad. No afectan el puntaje actual.'}
        </Typography>
        <Stack spacing={1.5} component="form" onSubmit={(event) => {
          event.preventDefault();
          suggestionMutation.mutate();
        }}>
          <TextField
            label={isEnglish ? 'Category name' : 'Nombre de categoría'}
            value={suggestionLabel}
            onChange={(event) => setSuggestionLabel(event.target.value)}
            inputProps={{ minLength: 3, maxLength: 80 }}
            required
          />
          <TextField
            label={isEnglish ? 'Clear definition' : 'Definición comprensible'}
            value={suggestionDefinition}
            onChange={(event) => setSuggestionDefinition(event.target.value)}
            inputProps={{ minLength: 20, maxLength: 500 }}
            multiline
            minRows={3}
            required
          />
          {suggestionMutation.isSuccess && <Alert severity="success">
            {isEnglish ? 'Suggestion received for moderation.' : 'Sugerencia recibida para moderación.'}
          </Alert>}
          {suggestionMutation.isError && <Alert severity="error">
            {isEnglish ? 'The suggestion could not be submitted.' : 'No se pudo enviar la sugerencia.'}
          </Alert>}
          <Button
            type="submit"
            variant="outlined"
            disabled={suggestionMutation.isPending || suggestionLabel.trim().length < 3
              || suggestionDefinition.trim().length < 20}
          >
            {isEnglish ? 'Send suggestion' : 'Enviar sugerencia'}
          </Button>
        </Stack>
      </Paper>
      <Paper component="section" variant="outlined" sx={{ p: 2, mt: 3 }}>
        <Typography component="h2" variant="h6" fontWeight={800}>
          {isEnglish ? 'Optional notifications' : 'Notificaciones opcionales'}
        </Typography>
        <Typography variant="body2" color="text.secondary" sx={{ mb: 1 }}>
          {isEnglish ? 'All choices start off. Previews never include review or order details.' : 'Todas empiezan apagadas. Los previews nunca incluyen detalles de evaluación u orden.'}
        </Typography>
        <Stack>
          {([
            ['reviewInvitation', isEnglish ? 'Review invitation after delivery' : 'Invitación después de la entrega'],
            ['reviewReminder', isEnglish ? 'One review reminder' : 'Un recordatorio para evaluar'],
            ['sellerResponseNotification', isEnglish ? 'Seller response' : 'Respuesta del vendedor'],
            ['moderationChange', isEnglish ? 'Moderation status' : 'Cambio de moderación'],
            ['evidenceRequest', isEnglish ? 'Evidence request' : 'Solicitud de evidencia'],
            ['appealResult', isEnglish ? 'Appeal result' : 'Resultado de apelación'],
            ['badgeChange', isEnglish ? 'Badge gained or lost' : 'Insignia obtenida o perdida'],
          ] as const).map(([key,label]) => (
            <FormControlLabel key={key} label={label} control={<Switch
              checked={notifications[key]}
              onChange={(event) => setNotifications((current) => ({ ...current,[key]: event.target.checked }))}
            />} />
          ))}
        </Stack>
        {notificationMutation.isError && <Alert severity="error">{isEnglish ? 'Notification choices were not saved.' : 'No se guardaron las notificaciones.'}</Alert>}
        <Button variant="outlined" sx={{ mt: 1 }} disabled={notificationMutation.isPending || notificationQuery.isLoading}
          onClick={() => notificationMutation.mutate()}>
          {isEnglish ? 'Save notifications' : 'Guardar notificaciones'}
        </Button>
      </Paper>
    </Box>
  );
}
