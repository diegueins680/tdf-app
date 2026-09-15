import { lazy, Suspense } from 'react';
import { Typography } from '@mui/material';
import { useParams, useSearchParams } from 'react-router-dom';
import { useTranslation } from 'react-i18next';

const EventTaskPage = lazy(() => import('./EventTaskPage'));
const SocialEventDetailPage = lazy(() => import('./SocialEventDetailPage'));

// Dispatch before importing/mounting the legacy overview and its unrelated readers.
export default function SocialEventWorkspacePage() {
  const { eventId = '' } = useParams();
  const [search] = useSearchParams();
  const { t } = useTranslation();
  const selected = search.getAll('tarea');
  return <Suspense fallback={<Typography role="status">{t('eventTask.loading')}</Typography>}>
    {selected.length === 0 ? <SocialEventDetailPage />
      : <EventTaskPage eventId={eventId} activityId={selected.length === 1 ? selected[0]! : null} />}
  </Suspense>;
}
