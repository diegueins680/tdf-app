import { useEffect, useRef, useState } from 'react';
import { Alert, Box, Button, Card, CardContent, CircularProgress, Container, Stack, Typography } from '@mui/material';
import { Link as RouterLink, useLocation, useNavigate, useParams } from 'react-router-dom';
import { ApiError } from '../../api/client';
import { SocialEventsAPI, type EventAdminInvitationPreviewDTO } from '../../api/socialEvents';
import { useLocalePreferences } from '../../contexts/LocalePreferencesContext';
import { useSession } from '../../session/SessionContext';
import { formatInEventTimeZone } from '../../utils/dateTime';
import { buildEventAdminInvitationPath, EVENT_ADMIN_INVITATION_TOKEN, readEventAdminInvitationHash } from '../../utils/eventAdminInvitationLink';
import { buildLoginRedirectPath } from '../../utils/loginRouting';

// Same shape the login return target accepts for a public event page.
const PUBLIC_EVENT_ID = /^[1-9]\d{0,17}$/;

/**
 * Tab-scoped copy of the pending token, keyed by event so it can only fire on
 * that event's page. It exists so the token survives the sign-in or
 * account-creation round trip, whose return URL never carries it.
 */
export const eventAdminInvitationStorageKey = (eventId: string) => `tdf-hq-ui/event-admin-invitation-token:${eventId}`;

const readStoredToken = (eventId: string): string | null => {
  try {
    const stored = window.sessionStorage.getItem(eventAdminInvitationStorageKey(eventId)) ?? '';
    return EVENT_ADMIN_INVITATION_TOKEN.test(stored) ? stored : null;
  } catch {
    return null;
  }
};

const storeToken = (eventId: string, token: string) => {
  try {
    window.sessionStorage.setItem(eventAdminInvitationStorageKey(eventId), token);
  } catch {
    // Blocked storage: the token stays in memory and in the address bar fragment.
  }
};

const clearStoredToken = (eventId: string) => {
  try {
    window.sessionStorage.removeItem(eventAdminInvitationStorageKey(eventId));
  } catch {
    // Nothing to clear.
  }
};

// The fragment is authoritative when present; storage only restores the token
// after the login round trip returns to the bare event path.
const resolveToken = (eventId: string, hash: string): string | null => {
  const fromHash = readEventAdminInvitationHash(hash);
  if (!fromHash) return readStoredToken(eventId);
  storeToken(eventId, fromHash);
  return fromHash;
};

type Phase =
  | { kind: 'working' }
  | { kind: 'accepted'; eventId: string }
  | { kind: 'unusable' }
  | { kind: 'organizer' }
  | { kind: 'failed' };

const formatStart = (value: string, locale: string, timezone: string) => {
  const date = new Date(value);
  return Number.isNaN(date.getTime())
    ? value
    : formatInEventTimeZone(date, locale, timezone, { dateStyle: 'full', timeStyle: 'short' });
};

function InvitationPanel({ eventId, token }: { eventId: string; token: string }) {
  const location = useLocation();
  const navigate = useNavigate();
  const { session, loading } = useSession();
  const { locale, timezone } = useLocalePreferences();
  const [phase, setPhase] = useState<Phase>({ kind: 'working' });
  const [preview, setPreview] = useState<EventAdminInvitationPreviewDTO | null>(null);
  const [retry, setRetry] = useState(0);
  // One redemption per account, token and explicit retry. A ref (not effect
  // cleanup) keeps React StrictMode's repeated effect from accepting twice.
  const attempt = useRef<string | null>(null);
  const account = session ? String(session.partyId ?? session.username) : null;
  // Latest navigation context for the async flow, without re-running it.
  const route = useRef({ location, navigate });
  route.current = { location, navigate };
  // A real unmount (leaving the page, another event, a replacement token) must
  // silence late responses; StrictMode's remount turns this back on.
  const mounted = useRef(false);
  useEffect(() => {
    mounted.current = true;
    return () => { mounted.current = false; };
  }, []);

  useEffect(() => {
    if (loading || !account) return;
    const key = `${account}:${token}:${retry}`;
    if (attempt.current === key) return;
    attempt.current = key;
    const current = () => mounted.current && attempt.current === key;
    // A token the server has settled is dead even if nobody is left to see it.
    const forgetSettledToken = () => {
      if (readStoredToken(eventId) === token) clearStoredToken(eventId);
    };
    // A spent or dead token must not linger in the address bar or history entry.
    const finish = () => {
      clearStoredToken(eventId);
      const { location: latest, navigate: go } = route.current;
      if (readEventAdminInvitationHash(latest.hash)) {
        go({ pathname: latest.pathname, search: latest.search }, { replace: true, state: latest.state });
      }
    };
    setPhase({ kind: 'working' });
    setPreview(null);
    void (async () => {
      try {
        const invitation = await SocialEventsAPI.previewEventAdminInvitation(token);
        if (!current()) return;
        if (invitation.eaipEventId !== eventId) {
          // Never accept an invitation for a different event than the page shows.
          clearStoredToken(eventId);
          if (!PUBLIC_EVENT_ID.test(invitation.eaipEventId)) { setPhase({ kind: 'unusable' }); return; }
          route.current.navigate(buildEventAdminInvitationPath(invitation.eaipEventId, token), { replace: true });
          return;
        }
        setPreview(invitation);
        const accepted = await SocialEventsAPI.acceptEventAdminInvitation(token);
        forgetSettledToken();
        if (!current()) return;
        finish();
        setPhase({ kind: 'accepted', eventId: accepted.eaaEventId });
      } catch (error) {
        const status = error instanceof ApiError ? error.status : null;
        if (status === 404 || status === 409) forgetSettledToken();
        if (!current()) return;
        if (status === 404 || status === 409) {
          finish();
          setPhase({ kind: status === 404 ? 'unusable' : 'organizer' });
        } else {
          setPhase({ kind: 'failed' });
        }
      }
    })();
  }, [account, eventId, loading, retry, token]);

  const eventPath = `/eventos/${eventId}`;
  const eventSummary = preview && (
    <Box>
      <Typography variant="h6" component="p">{preview.eaipEventTitle}</Typography>
      <Typography color="text.secondary">{formatStart(preview.eaipEventStart, locale, timezone)}</Typography>
    </Box>
  );

  let body;
  if (loading) {
    body = <Stack direction="row" spacing={1.5} alignItems="center" role="status">
      <CircularProgress size={22} /><Typography>Comprobando tu sesión…</Typography>
    </Stack>;
  } else if (!session) {
    body = <>
      <Typography>
        Te invitaron a administrar este evento en TDF Records. Para aceptar la invitación, inicia sesión o crea tu cuenta.
      </Typography>
      <Typography color="text.secondary">
        Al terminar volverás a esta página y tu acceso como administrador del evento se activará automáticamente.
      </Typography>
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
        <Button component={RouterLink} to={`/login?${new URLSearchParams({ signup: '1', redirect: eventPath }).toString()}`} variant="contained">Crear una cuenta</Button>
        <Button component={RouterLink} to={buildLoginRedirectPath(eventPath)} variant="outlined">Ya tengo cuenta: iniciar sesión</Button>
      </Stack>
      <Typography variant="caption" color="text.secondary">
        El enlace es personal y de un solo uso: funciona para una sola cuenta. No lo reenvíes.
      </Typography>
    </>;
  } else if (phase.kind === 'accepted') {
    body = <>
      <Alert severity="success" role="status">Listo: ya eres administrador de este evento.</Alert>
      {eventSummary}
      <Typography>
        Puedes editar el evento, subir el afiche, gestionar los tipos de entrada, revisar órdenes y entradas, registrar la entrada de asistentes, planificar la logística y emitir entradas de cortesía.
      </Typography>
      <Button component={RouterLink} to={`/social/eventos/${encodeURIComponent(phase.eventId)}`} variant="contained" sx={{ alignSelf: 'flex-start' }}>
        Administrar el evento
      </Button>
    </>;
  } else if (phase.kind === 'unusable') {
    body = <Alert severity="error">
      El enlace es inválido, venció, fue revocado o ya fue usado. Pide a la persona que organiza el evento que genere una nueva invitación.
    </Alert>;
  } else if (phase.kind === 'organizer') {
    body = <>
      <Alert severity="info">
        Tú organizas este evento, así que ya tienes acceso completo. Este enlace es para la persona que invitaste: envíaselo por un canal privado para que lo abra con su propia cuenta.
      </Alert>
      <Button component={RouterLink} to={`/social/eventos/${encodeURIComponent(eventId)}`} variant="contained" sx={{ alignSelf: 'flex-start' }}>
        Administrar el evento
      </Button>
    </>;
  } else if (phase.kind === 'failed') {
    body = <>
      <Alert severity="error">No pudimos activar la invitación en este momento. Revisa tu conexión e inténtalo de nuevo; el enlace sigue disponible.</Alert>
      {eventSummary}
      <Button variant="contained" onClick={() => setRetry((value) => value + 1)} sx={{ alignSelf: 'flex-start' }}>Reintentar</Button>
    </>;
  } else {
    body = <>
      {eventSummary}
      <Stack direction="row" spacing={1.5} alignItems="center" role="status">
        <CircularProgress size={22} />
        <Typography>{preview ? 'Activando tu acceso como administrador…' : 'Verificando la invitación…'}</Typography>
      </Stack>
    </>;
  }

  return (
    <Container maxWidth="md" sx={{ pt: { xs: 2, md: 3 } }}>
      <Card variant="outlined" component="section" aria-labelledby="event-admin-invitation-title">
        <CardContent>
          <Stack spacing={2}>
            <Typography id="event-admin-invitation-title" component="h2" variant="h5">Invitación para administrar este evento</Typography>
            {body}
          </Stack>
        </CardContent>
      </Card>
    </Container>
  );
}

// Holds the token for this event page: it outlives the fragment the flow
// clears, and a fragment arriving later (same tab, no reload) replaces it.
// `key` restarts the flow per event and per token.
function InvitationGate({ eventId }: { eventId: string }) {
  const { hash } = useLocation();
  const [token, setToken] = useState(() => resolveToken(eventId, hash));
  const fromHash = readEventAdminInvitationHash(hash);
  useEffect(() => {
    if (!fromHash) return;
    storeToken(eventId, fromHash);
    setToken(fromHash);
  }, [eventId, fromHash]);
  return token ? <InvitationPanel key={token} eventId={eventId} token={token} /> : null;
}

/**
 * Mounted beside the public event page (`/eventos/:eventId`), independent of
 * whether the event itself loads. Renders nothing unless the URL fragment is
 * `#admin=<token>` or a pending token is stored for this event.
 */
export default function EventAdminInvitationAcceptPanel() {
  const { eventId = '' } = useParams();
  if (!PUBLIC_EVENT_ID.test(eventId)) return null;
  return <InvitationGate key={eventId} eventId={eventId} />;
}
