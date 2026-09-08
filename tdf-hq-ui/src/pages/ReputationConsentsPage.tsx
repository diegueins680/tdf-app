import { Alert, Box, Button, Checkbox, FormControlLabel, Link, Paper, Stack, Typography } from '@mui/material';
import { useEffect, useState } from 'react';
import { Link as RouterLink } from 'react-router-dom';
import { Reputation, type ReputationConsent, type ReputationConsentKind } from '../api/reputation';
import { Directory } from '../api/directory';
import { ApiError } from '../api/client';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { useSession } from '../session/SessionContext';

const consentCopyVersion = 'reputation-consent-v0.1';
const kinds: ReputationConsentKind[] = ['pilot_participation', 'public_visibility', 'public_rankings', 'rating_reminders'];
const copy = {
  es: {
    title: 'Comparte tu reputación contextual',
    introduction: 'TDF muestra resultados agregados de interacciones verificadas. Tus rankings personales y la identidad de quien evalúa no son públicos. Cuando no hay evidencia suficiente mostramos “Reputación en formación”.',
    controls: {
      pilot_participation: 'Acepto participar en el piloto de reputación contextual. Puedes retirarte cuando quieras; TDF detiene nuevas señales, solicitudes y agregaciones del piloto sobre tu perfil.',
      public_visibility: 'Permito mostrar mi reputación agregada, badges verificables y tendencia estadísticamente válida en mi perfil público. No publicamos autores ni posiciones individuales; al retirarlo ocultamos los resultados afectados de inmediato.',
      public_rankings: 'Permito ser considerado para rankings públicos cuando exista muestra suficiente. No hay posiciones exactas en grupos pequeños ni uso de atributos sensibles.',
      rating_reminders: 'Acepto recibir solicitudes y recordatorios de valoración. Puedes desactivarlos cuando quieras; no enviamos mensajes en cola o reintento después del retiro.',
    },
    rights: 'Consulta tus derechos y privacidad, la explicación del cálculo o presenta una apelación. No revelamos la identidad de otros evaluadores.',
    withdrawAll: 'Retirar todos los consentimientos', loadError: 'No pudimos cargar tus consentimientos.', saveError: 'No se pudo guardar el cambio. Inténtalo de nuevo.',
    pilotUnavailable: 'El piloto de reputación contextual todavía no está habilitado. Puedes retirar consentimientos existentes en cualquier momento.',
    ageRequired: 'Antes de aceptar, confirma el control de edad. La declaración no se muestra como identidad verificada.',
    confirmAdult: 'Confirmar que soy mayor de edad', ageSaved: 'Control de edad confirmado. Ya puedes volver a aceptar el consentimiento.',
  },
  en: {
    title: 'Share your contextual reputation',
    introduction: 'TDF displays aggregated results from verified interactions. Your personal rankings and evaluator identities are not public. When there is not enough evidence, we show “Reputation in progress”.',
    controls: {
      pilot_participation: 'I agree to participate in the contextual reputation pilot. You may withdraw at any time; TDF stops new signals, requests, and pilot aggregations about your profile.',
      public_visibility: 'I allow my aggregated reputation, verified badges, and statistically valid trend to appear on my public profile. We do not publish evaluator identities or individual positions; withdrawal hides affected results immediately.',
      public_rankings: 'I allow myself to be considered for public rankings when there is a sufficient sample. Rankings do not show exact positions in small groups or use sensitive attributes.',
      rating_reminders: 'I agree to receive rating requests and reminders. You may turn them off at any time; we do not send queued or retrying messages after withdrawal.',
    },
    rights: 'Review your privacy rights, the calculation explanation, or submit an appeal. We do not reveal other evaluators’ identities.',
    withdrawAll: 'Withdraw all consents', loadError: 'We could not load your consents.', saveError: 'The change could not be saved. Try again.',
    pilotUnavailable: 'The contextual reputation pilot is not enabled yet. You can withdraw existing consents at any time.',
    ageRequired: 'Before accepting, confirm age assurance. This attestation is not displayed as identity verification.',
    confirmAdult: 'Confirm I am an adult', ageSaved: 'Age assurance confirmed. You can now accept consent again.',
  },
};

export default function ReputationConsentsPage() {
  const { locale } = useLocalePreferences();
  const { session } = useSession();
  const language = locale.startsWith('en') ? 'en' : 'es';
  const text = copy[language];
  const [items, setItems] = useState<ReputationConsent[]>([]);
  const [error, setError] = useState('');
  const [notice, setNotice] = useState('');
  const [saving, setSaving] = useState(false);
  const [loaded, setLoaded] = useState(false);
  const [ageRequired, setAgeRequired] = useState(false);
  const [ageSaving, setAgeSaving] = useState(false);
  const pilotEnabled = session?.featureFlags?.includes('CONTEXTUAL_REPUTATION_ENABLED') ?? false;
  useEffect(() => {
    let active = true;
    setLoaded(false);
    void Reputation.getMyConsents()
      .then((nextItems) => { if (active) { setItems(nextItems); setLoaded(true); } })
      .catch(() => { if (active) { setError(text.loadError); setLoaded(true); } });
    return () => { active = false; };
  }, [text.loadError]);

  const save = async (changes: { consentKind: ReputationConsentKind; granted: boolean }[]) => {
    if (!loaded || saving || changes.some(({ granted }) => granted && !pilotEnabled)) return;
    setSaving(true); setError(''); setNotice(''); setAgeRequired(false);
    try {
      setItems(await Reputation.updateMyConsents(changes.map((change) => ({ ...change, consentCopyVersion, consentLocale: language }))));
    } catch (caught) {
      if (caught instanceof ApiError && caught.status === 403 && changes.some(({ granted }) => granted)) setAgeRequired(true);
      else setError(text.saveError);
    } finally { setSaving(false); }
  };
  const confirmAdult = async () => {
    setAgeSaving(true); setError(''); setNotice('');
    try { await Directory.setAgeAssurance(true); setAgeRequired(false); setNotice(text.ageSaved); }
    catch { setError(text.saveError); } finally { setAgeSaving(false); }
  };
  const withdrawAll = () => void save(items.filter(({ granted }) => granted).map(({ consentKind }) => ({ consentKind, granted: false })));

  return <Box sx={{ maxWidth: 760, mx: 'auto', py: 3 }}><Paper sx={{ p: 3 }}><Stack spacing={2}>
    <Typography variant="h4">{text.title}</Typography>
    <Typography color="text.secondary">{text.introduction}</Typography>
    <Typography color="text.secondary">{text.rights} <Link component={RouterLink} to="/privacidad">{language === 'en' ? 'Privacy' : 'Privacidad'}</Link>{' · '}<Link component={RouterLink} to="/reputacion/como-se-calcula">{language === 'en' ? 'Calculation' : 'Cálculo'}</Link>{' · '}<Link component={RouterLink} to="/apelaciones">{language === 'en' ? 'Appeals' : 'Apelaciones'}</Link></Typography>
    {!pilotEnabled && <Alert severity="info">{text.pilotUnavailable}</Alert>}
    {ageRequired && <Alert severity="warning" action={<Button color="inherit" onClick={() => void confirmAdult()} disabled={ageSaving}>{text.confirmAdult}</Button>}>{text.ageRequired}</Alert>}
    {error && <Alert severity="error">{error}</Alert>}
    {notice && <Alert severity="success">{notice}</Alert>}
    {kinds.map((kind) => {
      const isGranted = items.find((item) => item.consentKind === kind)?.granted ?? false;
      return <FormControlLabel key={kind} control={<Checkbox checked={isGranted} disabled={!loaded || saving || (!isGranted && !pilotEnabled)} onChange={(event) => void save([{ consentKind: kind, granted: event.target.checked }])} />} label={<Typography>{text.controls[kind]}</Typography>} />;
    })}
    <Button variant="outlined" onClick={withdrawAll} disabled={!loaded || saving || !items.some(({ granted }) => granted)}>{text.withdrawAll}</Button>
  </Stack></Paper></Box>;
}
