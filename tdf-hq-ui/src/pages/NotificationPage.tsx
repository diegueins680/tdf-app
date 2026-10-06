import { useEffect, useRef } from 'react';
import { Alert, Button, CircularProgress, Stack, Typography } from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Link, Navigate, useLocation, useParams } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import { Fans } from '../api/fans';
import { notificationFallbackPath, notificationTargetPath, positiveNotificationId } from '../components/notificationTarget';
import { useSession } from '../session/SessionContext';

export default function NotificationPage() {
  const location = useLocation();
  const client = useQueryClient();
  const attempted = useRef('');
  const read = useMutation({ mutationFn: Fans.markNotificationRead,
    onSuccess: () => { void client.invalidateQueries({ queryKey: ['notifications'] }); void client.invalidateQueries({ queryKey: ['notification-count'] }); },
    onError: () => window.dispatchEvent(new Event('tdf-notification-read-error')),
  });
  const { notificationId } = useParams();
  const { session } = useSession();
  const { i18n } = useTranslation();
  const english = i18n.language.startsWith('en');
  const id = positiveNotificationId(Number(notificationId));
  const query = useQuery({ queryKey: ['notifications', session?.partyId, id],
    queryFn: () => Fans.getNotification(Number(id)), enabled: Boolean(id), retry: false });
  const notification = query.isError ? undefined : query.data;
  const markRead = read.mutate;
  const activated = (location.state as { activatedNotificationId?: number } | null)?.activatedNotificationId;
  useEffect(() => {
    const key = `${session?.partyId}:${id}`;
    if (notification && !notification.nIsRead && activated !== notification.nId && attempted.current !== key) {
      attempted.current = key;
      markRead(notification.nId);
    }
  }, [notification, activated, id, session?.partyId, markRead]);
  if (query.isPending && id) return <CircularProgress />;
  const destination = notification && notificationTargetPath(notification);
  if (destination) return <Navigate to={destination} replace />;
  return <Stack spacing={2}>
    <Typography component="h1" variant="h4">{english ? 'Notification' : 'Notificación'}</Typography>
    {notification && <><Typography component="h2" variant="h6">{notification.nTitle}</Typography><Typography>{notification.nBody}</Typography></>}
    <Alert severity="info">{english
      ? 'A specific destination is unavailable for this notification. It may no longer be available to your account, or the original notification did not retain its reference.'
      : 'No hay un destino específico disponible para esta notificación. Puede que ya no esté disponible para tu cuenta o que la notificación original no haya conservado su referencia.'}</Alert>
    <Button component={Link} to={notification ? notificationFallbackPath(notification) : '/inicio'}>{english ? 'Continue' : 'Continuar'}</Button>
  </Stack>;
}
