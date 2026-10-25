import { useEffect, useRef } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Box, Button, Checkbox, FormControlLabel, Stack, Typography } from '@mui/material';
import { useLocation } from 'react-router-dom';
import { ApiError } from '../../api/client';
import { EventMemoryInvitations } from '../../api/eventMemoryInvitations';
import { useSession } from '../../session/SessionContext';

export default function EventMemoryInvitationControls({ eventId, organizer }: { eventId: string; organizer: boolean }) {
  const { session } = useSession();
  const { search } = useLocation();
  const client = useQueryClient();
  const scope = session?.partyId;
  const preferenceKey = ['event-memory-preferences', scope];
  const campaignKey = ['event-memory-campaign', scope, eventId];
  const preference = useQuery({ queryKey: preferenceKey, queryFn: EventMemoryInvitations.preferences, enabled: Boolean(scope), retry: false });
  const campaign = useQuery({ queryKey: campaignKey, queryFn: () => EventMemoryInvitations.summary(eventId), enabled: Boolean(scope && organizer), retry: false });
  const savePreference = useMutation({ mutationFn: EventMemoryInvitations.setPreference,
    onSuccess: (value) => { client.setQueryData(preferenceKey, value); void client.invalidateQueries({ queryKey: campaignKey }); } });
  const saveCampaign = useMutation({ mutationFn: (enabled: boolean) => EventMemoryInvitations.setCampaign(eventId, enabled),
    onSuccess: (value) => client.setQueryData(campaignKey, value) });
  const opened = useRef('');
  const invitation = new URLSearchParams(search).get('invitation');
  useEffect(() => {
    if (!scope || !invitation || !/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(invitation)) return;
    const key = `${scope}:${eventId}:${invitation}`;
    if (opened.current === key) return;
    opened.current = key;
    // Attribution failure must never block reading or publishing a memory.
    void EventMemoryInvitations.open(eventId, invitation).catch(() => { opened.current = ''; });
  }, [scope, eventId, invitation]);
  if (!scope || (preference.error instanceof ApiError && preference.error.status === 404)) return null;
  return <Stack spacing={1.5}>
    {preference.data && <>
      <FormControlLabel control={<Checkbox checked={preference.data.inApp} disabled={savePreference.isPending}
        onChange={(_event, checked) => savePreference.mutate(checked)} />} label="Recibir invitaciones a compartir recuerdos dentro de TDF" />
      <Typography variant="caption" color="text.secondary">Una invitación y como máximo un recordatorio por evento, con al menos 7 días entre avisos. Puedes desactivarlos aquí. Esta preferencia no autoriza correo, push ni publicidad.</Typography>
    </>}
    {(savePreference.isError || saveCampaign.isError || (preference.isError && !(preference.error instanceof ApiError && preference.error.status === 404))) &&
      <Alert severity="error">No se pudo guardar o consultar la preferencia. Inténtalo de nuevo; tu selección anterior se conserva.</Alert>}
    {organizer && campaign.data && <Box>
      <Typography variant="h6">Invitaciones a compartir recuerdos</Typography>
      <Typography color="text.secondary">Solo se enviarán a cuentas elegibles que hayan autorizado avisos internos, después de confirmar la finalización del evento.</Typography>
      <FormControlLabel control={<Checkbox checked={campaign.data.enabled} disabled={saveCampaign.isPending}
        onChange={(_event, checked) => saveCampaign.mutate(checked)} />} label="Habilitar invitaciones posteriores a este evento" />
      {campaign.data.dispatcherEnabled === false && <Alert severity="info">Los envíos internos están pausados por la plataforma.</Alert>}
      {!campaign.data.eventFinished && <Alert severity="info">Envío en espera: se requiere estado Finalizado, hora de cierre confirmada hace al menos una hora y un evento de los últimos 30 días.</Alert>}
      <Typography variant="body2">Cuentas relacionadas: {campaign.data.candidates} · Con preferencia activa: {campaign.data.consented}</Typography>
      <Typography variant="body2">Entregadas en el buzón: {campaign.data.invitations} · Recordatorios: {campaign.data.reminders} · Aperturas atribuidas: {campaign.data.opened} · Autores tras la invitación: {campaign.data.published}</Typography>
      <Typography variant="body2">Momentos: {campaign.data.moments} · Autores únicos: {campaign.data.uniqueAuthors}</Typography>
      <Button onClick={() => { void campaign.refetch(); }} disabled={campaign.isFetching}>Actualizar resultados</Button>
    </Box>}
    {organizer && campaign.isError && <Alert severity="info">Los resultados de la campaña no están disponibles para esta cuenta.</Alert>}
  </Stack>;
}
