import { useEffect, useRef, useState } from 'react';
import { Alert, Box, Button, Stack, TextField, Typography } from '@mui/material';
import { useSession } from '../session/SessionContext';
import { acceptTeamInvitation, confirmEmailProof, previewTeamInvitation, requestEmailProof, type invitationPreviewSchema } from '../api/eventCrm';
import type { z } from 'zod';
import LoginPage from './LoginPage';

const roles: Record<string, string> = { coordinator: 'Coordinación', sales: 'Ventas', payment_verifier: 'Verificación de pagos' };
const permissions: Record<string, string> = { 'team.sales': 'Coordinar equipo de ventas', logistics: 'Coordinar logística y responsables', 'crm.all': 'Consultar y gestionar el embudo completo de este evento', 'orders.read': 'Consultar órdenes y sus estados de pago', 'crm.assigned': 'Seguimiento de contactos asignados', 'payments.verify': 'Verificar pagos manuales como delegado del organizador' };
export default function EventTeamInvitationPage() {
  const { session } = useSession();
  // A different account must start a fresh review, including while a previous
  // account's request is still in flight.
  return <InvitationSession key={session?.partyId ?? 'anonymous'} />;
}
function InvitationSession() {
  const { session } = useSession();
  const active = useRef(true);
  useEffect(() => { active.current = true; return () => { active.current = false; }; }, []);
  // Fragment never reaches HTTP access logs; no local/session storage or analytics.
  const [acceptedEventId, setAcceptedEventId] = useState<number | null>(null);
  const [token] = useState(() => window.location.hash.slice(1));
  const [preview, setPreview] = useState<z.infer<typeof invitationPreviewSchema> | null>(null);
  const [proof, setProof] = useState(''); const [error, setError] = useState(''); const [notice, setNotice] = useState(''); const [busy, setBusy] = useState(false); const [accepted, setAccepted] = useState(false);
  const attempt = async (action: () => Promise<void>) => { setBusy(true); setError(''); try { await action(); } catch { if (active.current) setError('No se pudo completar. Comprueba la cuenta, la verificación del correo y la vigencia del enlace.'); } finally { if (active.current) setBusy(false); } };
  return <Box sx={{ maxWidth: 650, mx: 'auto', p: 2, pb: 12 }} className="ph-no-capture ph-sensitive"><Stack spacing={2}>
    <Typography variant="h4" component="h1">Invitación al equipo de un evento</Typography>
    {!session ? <><Alert severity="info">Inicia sesión con tu cuenta invitada. Si aún no tienes cuenta, completa el registro con el correo que confirmó la persona organizadora.</Alert><LoginPage onAuthenticated={() => undefined} /></> : <>
      <Typography>Sesión: {session.displayName}. La invitación sólo funciona con la cuenta o el correo verificado de la destinataria.</Typography>
      {error && <Alert severity="error">{error}</Alert>}{notice && <Alert severity="success">{notice}</Alert>}
      {!accepted && <Button disabled={busy || !/^[A-Za-z0-9_-]{43}$/.test(token)} variant="outlined" onClick={() => void attempt(async () => setPreview(await previewTeamInvitation(token)))}>Revisar evento, rol y permisos</Button>}
      {!accepted && <>
        <Typography>Si te invitaron por correo, verifica su acceso antes de aceptar.</Typography>
        <Button disabled={busy} onClick={() => void attempt(async () => { await requestEmailProof(); setNotice('Código solicitado a tu correo de cuenta. Caduca en 30 minutos.'); })}>Enviar código a mi correo de cuenta</Button>
        <TextField label="Código de verificación del correo" value={proof} onChange={e => setProof(e.target.value)} autoComplete="off" />
        <Button disabled={busy || !proof} onClick={() => void attempt(async () => { await confirmEmailProof(proof); setProof(''); setNotice('Correo verificado. Ya puedes revisar tu invitación.'); })}>Verificar correo</Button>
      </>}
      {preview && !accepted && <>
        <Typography variant="h5" component="h2">{preview.eventTitle}</Typography><Typography>{preview.venue} · {new Date(preview.startTime).toLocaleString('es-EC', { timeZone: 'America/Guayaquil' })}</Typography>
        <Typography>Rol: {roles[preview.role] ?? preview.role} · Vence: {new Date(preview.expiresAt).toLocaleString('es-EC')}</Typography>
        {preview.permissions.map(p => <Typography key={p}>• {permissions[p] ?? p}</Typography>)}
        <Alert severity="info">Este acceso se limita a este evento. Publicación, precios, cobros y devoluciones permanecen reservados al organizador.</Alert>
        <Button variant="contained" disabled={busy} onClick={() => void attempt(async () => { await acceptTeamInvitation(token); if (!active.current) return; setAcceptedEventId(preview.eventId); setAccepted(true); setPreview(null); window.history.replaceState(null, '', window.location.pathname); setNotice('Invitación aceptada. El enlace ya no puede volver a usarse.'); })}>Aceptar este rol y sus permisos</Button>
      </>}
      {acceptedEventId && <Button variant="contained" href={`/social/eventos/${acceptedEventId}/crm`}>Abrir CRM del evento</Button>}
    </>}
  </Stack></Box>;
}
