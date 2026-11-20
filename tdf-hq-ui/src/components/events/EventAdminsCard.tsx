import { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Box, Button, Card, CardContent, Chip, CircularProgress, Dialog, DialogActions, DialogContent, DialogTitle, Divider, Stack, TextField, Typography } from '@mui/material';
import { ApiError } from '../../api/client';
import { SocialEventsAPI, type EventAdminInvitationStatus, type EventAdminRosterDTO } from '../../api/socialEvents';
import { buildEventAdminInvitationLink } from '../../utils/eventAdminInvitationLink';
import { firstNonEmptyString } from '../../utils/stringValues';

const INVITEE_NAME_MAX_LENGTH = 120;

const statusChip: Record<EventAdminInvitationStatus, { label: string; color: 'warning' | 'success' | 'default' }> = {
  pending: { label: 'Pendiente', color: 'warning' },
  accepted: { label: 'Aceptada', color: 'success' },
  revoked: { label: 'Revocada', color: 'default' },
  expired: { label: 'Vencida', color: 'default' },
};

type Confirmation =
  | { kind: 'remove-admin'; id: string; name: string }
  | { kind: 'revoke-invitation'; id: string; name: string };

const statusOf = (error: unknown) => (error instanceof ApiError ? error.status : null);

interface EventAdminsCardProps {
  eventId: string;
  /** Caller access reported by the server; the roster response stays authoritative. */
  access: EventAdminRosterDTO['earAccess'];
  /** Separates cached rosters between accounts on a shared device. */
  accountScope: string;
  formatDate: (value: string) => string;
}

export default function EventAdminsCard({ eventId, access, accountScope, formatDate }: EventAdminsCardProps) {
  const queryClient = useQueryClient();
  const rosterKey = ['social-event-admins', eventId, accountScope];
  const [inviteeName, setInviteeName] = useState('');
  const [issued, setIssued] = useState<{ invitationId: string; name: string; link: string } | null>(null);
  const [copyFeedback, setCopyFeedback] = useState<{ severity: 'success' | 'error'; message: string } | null>(null);
  // The target outlives `confirmOpen` so the dialog keeps its text while closing.
  const [confirmation, setConfirmation] = useState<Confirmation | null>(null);
  const [confirmOpen, setConfirmOpen] = useState(false);
  const rosterQuery = useQuery({
    queryKey: rosterKey,
    queryFn: () => SocialEventsAPI.listEventAdmins(eventId),
    enabled: Boolean(eventId),
    retry: false,
  });
  const roster = rosterQuery.data;
  const isOwner = (roster?.earAccess ?? access) === 'owner';
  const refreshRoster = () => queryClient.invalidateQueries({ queryKey: ['social-event-admins', eventId] });

  const inviteMutation = useMutation({
    mutationFn: (name: string) => SocialEventsAPI.createEventAdminInvitation(eventId, name),
    onSuccess: (result) => {
      setIssued({
        invitationId: result.eaisInvitation.eaiId,
        name: result.eaisInvitation.eaiInviteeName,
        link: buildEventAdminInvitationLink(window.location.origin, eventId, result.eaisToken),
      });
      setInviteeName('');
      setCopyFeedback(null);
      void refreshRoster();
    },
  });
  const confirmMutation = useMutation({
    mutationFn: async (target: Confirmation) => {
      if (target.kind === 'remove-admin') await SocialEventsAPI.removeEventAdmin(eventId, target.id);
      else await SocialEventsAPI.revokeEventAdminInvitation(eventId, target.id);
    },
    onSuccess: (_value, target) => {
      // A revoked link must not stay on screen looking usable.
      if (target.kind === 'revoke-invitation') setIssued((current) => (current?.invitationId === target.id ? null : current));
      setConfirmOpen(false);
    },
    // Also after a failure: a 409 means the roster on screen is stale.
    onSettled: () => { void refreshRoster(); },
  });

  const trimmedName = inviteeName.trim();
  const submitInvitation = () => {
    if (!trimmedName || inviteMutation.isPending) return;
    inviteMutation.mutate(trimmedName);
  };
  const copyLink = async () => {
    if (!issued) return;
    try {
      if (!navigator.clipboard?.writeText) throw new Error('unsupported');
      await navigator.clipboard.writeText(issued.link);
      setCopyFeedback({ severity: 'success', message: 'Enlace copiado. Envíalo por un canal privado.' });
    } catch {
      setCopyFeedback({ severity: 'error', message: 'No se pudo copiar automáticamente. Selecciona el enlace y cópialo manualmente.' });
    }
  };
  const closeConfirmation = () => {
    if (confirmMutation.isPending) return;
    setConfirmOpen(false);
  };
  const inviteError = inviteMutation.error
    ? statusOf(inviteMutation.error) === 409
      ? 'Hay demasiadas invitaciones pendientes. Revoca una antes de generar otra.'
      : statusOf(inviteMutation.error) === 400
        ? `Escribe el nombre de la persona invitada (máximo ${INVITEE_NAME_MAX_LENGTH} caracteres).`
        : 'No se pudo generar la invitación. Inténtalo de nuevo.'
    : null;
  const confirmError = confirmMutation.error
    ? confirmation?.kind === 'revoke-invitation' && statusOf(confirmMutation.error) === 409
      ? 'La invitación ya fue aceptada. Para retirar el acceso, quita a la persona de la lista de administradores.'
      : 'No se pudo completar la acción. Actualiza el evento e inténtalo de nuevo.'
    : null;

  return <Card variant="outlined" component="section" aria-labelledby="event-admins-title">
    <CardContent><Stack spacing={1.5}>
      <Typography id="event-admins-title" component="h2" variant="h6">Administradores del evento</Typography>
      <Typography variant="body2" color="text.secondary">
        {isOwner
          ? 'Las personas que invites podrán editar el evento, subir el afiche, gestionar entradas, ver órdenes, registrar la entrada de asistentes, planificar la logística y emitir cortesías. Solo tú puedes invitar o quitar administradores.'
          : 'Administras este evento por invitación de quien lo organiza. Solo esa persona puede invitar o quitar administradores.'}
      </Typography>
      {rosterQuery.isLoading && <CircularProgress size={22} aria-label="Cargando administradores" />}
      {rosterQuery.isError && <Alert severity="error" action={<Button color="inherit" size="small" onClick={() => { void rosterQuery.refetch(); }}>Reintentar</Button>}>
        No se pudo cargar la lista de administradores.
      </Alert>}
      {roster && (roster.earAdmins.length === 0
        ? <Typography color="text.secondary">Todavía no hay administradores invitados.</Typography>
        : <Stack component="ul" spacing={1} sx={{ listStyle: 'none', p: 0, m: 0 }} aria-label="Administradores">
          {roster.earAdmins.map((admin) => {
            const name = firstNonEmptyString(admin.eamDisplayName?.trim(), `Cuenta ${admin.eamPartyId}`);
            return <Stack component="li" key={admin.eamPartyId} direction={{ xs: 'column', sm: 'row' }} spacing={1} justifyContent="space-between" alignItems={{ sm: 'center' }} sx={{ p: 1.25, border: '1px solid', borderColor: 'divider', borderRadius: 2 }}>
              <Box>
                <Typography fontWeight={700}>{name}</Typography>
                <Typography variant="body2" color="text.secondary">Administra desde {formatDate(admin.eamGrantedAt)}</Typography>
              </Box>
              {isOwner && <Button color="error" aria-label={`Quitar a ${name}`} onClick={() => { confirmMutation.reset(); setConfirmOpen(true); setConfirmation({ kind: 'remove-admin', id: admin.eamPartyId, name }); }}>Quitar</Button>}
            </Stack>;
          })}
        </Stack>)}

      {isOwner && <>
        <Divider />
        <Typography variant="subtitle1" fontWeight={700}>Invitar a un administrador</Typography>
        <Typography variant="body2" color="text.secondary">
          No necesita tener cuenta todavía: al abrir el enlace podrá iniciar sesión o crear una, y quedará como administrador de este evento.
        </Typography>
        {inviteError && <Alert severity="error">{inviteError}</Alert>}
        <Stack component="form" direction={{ xs: 'column', sm: 'row' }} spacing={1} onSubmit={(submit) => { submit.preventDefault(); submitInvitation(); }}>
          <TextField label="Nombre de la persona invitada" value={inviteeName} onChange={(change) => setInviteeName(change.target.value)} inputProps={{ maxLength: INVITEE_NAME_MAX_LENGTH }} disabled={inviteMutation.isPending} sx={{ flex: 1 }} />
          <Button type="submit" variant="contained" disabled={!trimmedName || inviteMutation.isPending}>
            {inviteMutation.isPending ? <CircularProgress size={20} color="inherit" /> : 'Generar enlace de invitación'}
          </Button>
        </Stack>
        {issued && <Stack spacing={1} sx={{ p: 1.5, border: '1px solid', borderColor: 'success.main', borderRadius: 2 }} role="group" aria-label="Enlace de invitación generado">
          <Typography fontWeight={700}>Enlace para {issued.name}</Typography>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
            <TextField label="Enlace de invitación" value={issued.link} fullWidth InputProps={{ readOnly: true }} onFocus={(focus) => focus.target.select()} />
            <Button variant="outlined" onClick={() => { void copyLink(); }}>Copiar enlace</Button>
          </Stack>
          <Alert severity="warning">
            Este enlace se muestra una sola vez: cópialo ahora. Vence en 14 días, funciona para una sola cuenta y conviene enviarlo por un canal privado (por ejemplo, WhatsApp).
          </Alert>
          {copyFeedback && <Alert severity={copyFeedback.severity} role="status">{copyFeedback.message}</Alert>}
        </Stack>}
        {roster && roster.earInvitations.length > 0 && <>
          <Typography variant="subtitle1" fontWeight={700}>Invitaciones</Typography>
          <Stack component="ul" spacing={1} sx={{ listStyle: 'none', p: 0, m: 0 }} aria-label="Invitaciones">
            {roster.earInvitations.map((invitation) => {
              const chip = statusChip[invitation.eaiStatus] ?? { label: invitation.eaiStatus, color: 'default' as const };
              return <Stack component="li" key={invitation.eaiId} direction={{ xs: 'column', sm: 'row' }} spacing={1} justifyContent="space-between" alignItems={{ sm: 'center' }} sx={{ p: 1.25, border: '1px solid', borderColor: 'divider', borderRadius: 2 }}>
                <Box>
                  <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
                    <Typography fontWeight={700}>{invitation.eaiInviteeName}</Typography>
                    <Chip size="small" label={chip.label} color={chip.color} />
                  </Stack>
                  <Typography variant="body2" color="text.secondary">
                    {invitation.eaiStatus === 'accepted' && invitation.eaiAcceptedAt
                      ? `Aceptada el ${formatDate(invitation.eaiAcceptedAt)}`
                      : invitation.eaiStatus === 'pending'
                        ? `Creada el ${formatDate(invitation.eaiCreatedAt)} · vence el ${formatDate(invitation.eaiExpiresAt)}`
                        : `Creada el ${formatDate(invitation.eaiCreatedAt)}`}
                  </Typography>
                </Box>
                {invitation.eaiStatus === 'pending' && <Button color="error" aria-label={`Revocar la invitación de ${invitation.eaiInviteeName}`} onClick={() => { confirmMutation.reset(); setConfirmOpen(true); setConfirmation({ kind: 'revoke-invitation', id: invitation.eaiId, name: invitation.eaiInviteeName }); }}>Revocar</Button>}
              </Stack>;
            })}
          </Stack>
        </>}
      </>}
    </Stack></CardContent>
    <Dialog open={confirmOpen && confirmation !== null} onClose={closeConfirmation} aria-labelledby="event-admin-confirmation-title" fullWidth maxWidth="xs">
      <DialogTitle id="event-admin-confirmation-title">{confirmation?.kind === 'revoke-invitation' ? 'Revocar invitación' : 'Quitar administrador'}</DialogTitle>
      <DialogContent><Stack spacing={1.5}>
        <Typography>
          {confirmation?.kind === 'revoke-invitation'
            ? `El enlace enviado a ${confirmation.name} dejará de funcionar. Podrás generar uno nuevo cuando quieras.`
            : `${confirmation?.name ?? 'Esta persona'} dejará de administrar este evento de inmediato. Su cuenta y sus entradas no se modifican.`}
        </Typography>
        {confirmError && <Alert severity="error">{confirmError}</Alert>}
      </Stack></DialogContent>
      <DialogActions>
        <Button onClick={closeConfirmation} disabled={confirmMutation.isPending}>Cancelar</Button>
        <Button color="error" variant="contained" disabled={confirmMutation.isPending} onClick={() => { if (confirmation) confirmMutation.mutate(confirmation); }}>
          {confirmMutation.isPending ? 'Guardando…' : confirmation?.kind === 'revoke-invitation' ? 'Revocar invitación' : 'Quitar administrador'}
        </Button>
      </DialogActions>
    </Dialog>
  </Card>;
}
