import { Card, CardContent, Link, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';

type ReputationRightsView = 'privacy' | 'appeal' | 'calculation';

const privacyEmail = 'privacidad@tdfrecords.com';

function mailto(subject: string) {
  return `mailto:${privacyEmail}?subject=${encodeURIComponent(subject)}`;
}

export default function PublicReputationRightsPage({ view = 'privacy' }: { view?: ReputationRightsView }) {
  const { locale } = useLocalePreferences();
  const english = locale.toLowerCase().startsWith('en');
  const copy = english ? {
    privacyTitle: 'Privacy and reputation',
    privacyBody: 'Request access, export, correction, or deletion of your reputation data. Individual positions and evaluator identities are not public.',
    withdrawal: 'Withdraw consent',
    withdrawalBody: 'During the pilot, send a withdrawal request for participation, public visibility, rankings, or reminders. We stop the selected processing before any new public reading or reminder.',
    appealTitle: 'Reputation review requests',
    appealBody: 'Ask for a review of a signal, category, badge, or aggregated result. We do not reveal other evaluators’ identities.',
    appealAction: 'Request a reputation review',
    calculationTitle: 'How contextual reputation is calculated',
    calculationBody: 'Public reputation uses only verified interactions, institutional category weights, a minimum sample, and uncertainty safeguards. Personal category priorities create a private compatibility score and never change another person’s public reputation.',
    calculationMore: 'Read the technical calculation and safeguards',
    contact: 'For a request, contact ',
    contactSuffix: '. Include your account email, request type, and relevant context. We will not disclose third-party data.',
    privacyAction: 'Request privacy help',
    appealSubject: 'Contextual reputation — review request',
    withdrawalSubject: 'Contextual reputation — withdraw consent',
  } : {
    privacyTitle: 'Privacidad y reputación',
    privacyBody: 'Solicita acceso, exportación, corrección o eliminación de tus datos de reputación. Las posiciones individuales y la identidad de evaluadores no son públicas.',
    withdrawal: 'Retirar consentimiento',
    withdrawalBody: 'Durante el piloto, solicita retirar por separado tu participación, visibilidad pública, rankings o recordatorios. Detenemos el tratamiento seleccionado antes de una nueva lectura pública o recordatorio.',
    appealTitle: 'Solicitudes de revisión de reputación',
    appealBody: 'Solicita una revisión de una señal, categoría, insignia o resultado agregado. No revelamos la identidad de otros evaluadores.',
    appealAction: 'Solicitar revisión de reputación',
    calculationTitle: 'Cómo se calcula la reputación contextual',
    calculationBody: 'La reputación pública usa únicamente interacciones verificadas, pesos institucionales de categoría, muestra mínima y salvaguardas de incertidumbre. Las prioridades personales crean una puntuación privada de compatibilidad y nunca cambian la reputación pública de otra persona.',
    calculationMore: 'Leer el cálculo técnico y las salvaguardas',
    contact: 'Para iniciar una solicitud, escribe a ',
    contactSuffix: '. Incluye el correo de tu cuenta, el tipo de solicitud y el contexto. No expondremos datos de terceros.',
    privacyAction: 'Solicitar ayuda de privacidad',
    appealSubject: 'Reputación contextual — solicitud de revisión',
    withdrawalSubject: 'Reputación contextual — retirar consentimiento',
  };

  const title = view === 'appeal' ? copy.appealTitle : view === 'calculation' ? copy.calculationTitle : copy.privacyTitle;
  const body = view === 'appeal' ? copy.appealBody : view === 'calculation' ? copy.calculationBody : copy.privacyBody;

  return (
    <Stack spacing={3} sx={{ maxWidth: 760, mx: 'auto', py: 4 }} lang={english ? 'en' : 'es'}>
      <Typography variant="h3" fontWeight={800}>{title}</Typography>
      <Typography color="text.secondary">{body}</Typography>
      <Card><CardContent><Stack spacing={2}>
        {view === 'privacy' && <>
          <Typography id="retirar-consentimientos" variant="h6">{copy.withdrawal}</Typography>
          <Typography>{copy.withdrawalBody}</Typography>
          <Link href={mailto(copy.withdrawalSubject)}>{copy.withdrawal}</Link>
        </>}
        {view === 'appeal' && <Link href={mailto(copy.appealSubject)}>{copy.appealAction}</Link>}
        {view === 'calculation' && <Link component={RouterLink} to="/privacidad">{copy.privacyAction}</Link>}
        {view !== 'calculation' && <Link component={RouterLink} to="/reputacion/como-se-calcula">{copy.calculationMore}</Link>}
        <Typography>{copy.contact}<Link href={`mailto:${privacyEmail}`}>{privacyEmail}</Link>{copy.contactSuffix}</Typography>
      </Stack></CardContent></Card>
    </Stack>
  );
}
