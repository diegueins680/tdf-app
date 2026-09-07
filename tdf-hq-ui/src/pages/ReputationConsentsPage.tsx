import { Alert, Box, Button, Checkbox, FormControlLabel, Paper, Stack, Typography } from '@mui/material';
import { useEffect, useState } from 'react';
import { Reputation, type ReputationConsent, type ReputationConsentKind } from '../api/reputation';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';

const kinds: ReputationConsentKind[] = ['pilot_participation', 'public_visibility', 'public_rankings', 'rating_reminders'];
const labels = {
  es: { pilot_participation: 'Participación en el piloto', public_visibility: 'Visibilidad pública de reputación', public_rankings: 'Rankings públicos', rating_reminders: 'Solicitudes y recordatorios de valoración' },
  en: { pilot_participation: 'Pilot participation', public_visibility: 'Public reputation visibility', public_rankings: 'Public rankings', rating_reminders: 'Rating requests and reminders' },
};

export default function ReputationConsentsPage() {
  const { locale } = useLocalePreferences();
  const language = locale.startsWith('en') ? 'en' : 'es';
  const [items, setItems] = useState<ReputationConsent[]>([]);
  const [error, setError] = useState('');
  const [saving, setSaving] = useState<ReputationConsentKind | null>(null);
  useEffect(() => { void Reputation.getMyConsents().then(setItems).catch(() => setError(language === 'en' ? 'We could not load your consents.' : 'No pudimos cargar tus consentimientos.')); }, [language]);
  const toggle = async (consentKind: ReputationConsentKind, granted: boolean) => {
    setSaving(consentKind); setError('');
    try { setItems(await Reputation.updateMyConsents([{ consentKind, granted }])); }
    catch { setError(language === 'en' ? 'The change could not be saved. Try again.' : 'No se pudo guardar el cambio. Inténtalo de nuevo.'); }
    finally { setSaving(null); }
  };
  return <Box sx={{ maxWidth: 760, mx: 'auto', py: 3 }}><Paper sx={{ p: 3 }}><Stack spacing={2}>
    <Typography variant="h4">{language === 'en' ? 'Contextual reputation consents' : 'Consentimientos de reputación contextual'}</Typography>
    <Typography color="text.secondary">{language === 'en' ? 'Each consent is independent. Turning one off takes effect immediately for new public reads and reminders.' : 'Cada consentimiento es independiente. Al retirarlo, el cambio aplica inmediatamente para nuevas lecturas públicas y recordatorios.'}</Typography>
    {error && <Alert severity="error">{error}</Alert>}
    {kinds.map((kind) => <FormControlLabel key={kind} control={<Checkbox checked={items.find((item) => item.consentKind === kind)?.granted ?? false} disabled={saving === kind} onChange={(event) => void toggle(kind, event.target.checked)} />} label={labels[language][kind]} />)}
    <Button variant="outlined" onClick={() => void Promise.all(items.filter((item) => item.granted).map((item) => toggle(item.consentKind, false)))} disabled={saving !== null}>{language === 'en' ? 'Withdraw all consents' : 'Retirar todos los consentimientos'}</Button>
  </Stack></Paper></Box>;
}
