import { Alert, Box, Paper, Typography } from '@mui/material';
import CategoryPriorityPrototype from '../components/reputation/CategoryPriorityPrototype';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { useSession } from '../session/SessionContext';

export default function ReputationPreferencesPage() {
  const { locale } = useLocalePreferences();
  const { session } = useSession();
  const isEnglish = locale === 'en';
  const enabled = session?.featureFlags?.includes('CONTEXTUAL_REPUTATION_ENABLED') ?? false;

  if (!enabled) {
    return (
      <Box sx={{ maxWidth: 760, mx: 'auto', py: 3 }}>
        <Typography variant="h4" fontWeight={800} gutterBottom>
          {isEnglish ? 'Reputation priorities' : 'Prioridades de reputación'}
        </Typography>
        <Alert severity="info">
          {isEnglish
            ? 'Contextual reputation is not enabled for this account yet.'
            : 'La reputación contextual todavía no está habilitada para esta cuenta.'}
        </Alert>
      </Box>
    );
  }

  return (
    <Box sx={{ maxWidth: 760, mx: 'auto', py: 3 }}>
      <Paper elevation={0} sx={{ p: { xs: 2, sm: 3 }, border: 1, borderColor: 'divider', borderRadius: 3 }}>
        <CategoryPriorityPrototype locale={isEnglish ? 'en' : 'es'} />
      </Paper>
    </Box>
  );
}
