import { useState } from 'react';
import { Box, Skeleton, Typography } from '@mui/material';
import { useTranslation } from 'react-i18next';
import { isProviderPlaceholder, recordThumbnailCandidates, type ThumbnailResource } from './resolveRecordThumbnail';

interface Props {
  resource: ThumbnailResource;
  /** Only for catalog overview artwork, never for a specific video's playback card. */
  fallbackResources?: ThumbnailResource[];
  title: string;
  compact?: boolean;
}

export default function RecordThumbnail(props: Props) {
  // Catalog overview cards can use alternate media belonging to the same item.
  // At most three resources / nine distinct requests; each URL is validated
  // against its own provider identity before it enters this bounded queue.
  const resources = [props.resource, ...(props.fallbackResources ?? []).filter(resource =>
    (resource.providerCode !== props.resource.providerCode || resource.externalCode !== props.resource.externalCode)
    && recordThumbnailCandidates(resource).length > 0).slice(0, 2)];
  const sources = [...new Set(resources.flatMap(recordThumbnailCandidates))];
  const unavailable = [props.resource, ...(props.fallbackResources ?? [])]
    .every(resource => resource.availability === 'unavailable');
  // Reset only when the resource/metadata changes, never on an ordinary rerender.
  return <ThumbnailAttempt key={JSON.stringify([resources, sources])} {...props} sources={sources} unavailable={unavailable} />;
}

function ThumbnailAttempt({ title, compact, sources, unavailable }: Props & { sources: string[]; unavailable: boolean }) {
  const { t } = useTranslation();
  const [index, setIndex] = useState(0);
  const [loaded, setLoaded] = useState(false);
  const src = sources[index];
  const label = unavailable
    ? t('records.videoUnavailable', 'Video no disponible en la fuente')
    : t('records.thumbnailUnavailable', 'Miniatura no disponible');
  const failed = () => { setLoaded(false); setIndex(index + 1); };
  return (
    <Box sx={{ position: 'relative', width: '100%', height: '100%', minHeight: compact ? 40 : 120, bgcolor: '#182235', color: '#fff' }}>
      {src ? <>
        {!loaded && <Skeleton variant="rectangular" aria-label={t('records.loadingThumbnail', 'Cargando miniatura')}
          sx={{ position: 'absolute', inset: 0, height: '100%' }} />}
        <Box component="img" key={src} src={src} alt={title} loading="lazy" decoding="async"
          onError={failed}
          onLoad={(event) => {
            if (isProviderPlaceholder(src, event.currentTarget.naturalWidth, event.currentTarget.naturalHeight)) failed();
            else setLoaded(true);
          }}
          sx={{ display: 'block', width: '100%', height: '100%', objectFit: 'cover', opacity: loaded ? 1 : 0 }} />
      </> : <Box role="img" aria-label={label}
        sx={{ height: '100%', minHeight: 'inherit', display: 'flex', alignItems: 'center', justifyContent: 'center', p: compact ? 0 : 2 }}>
        {compact ? <span aria-hidden="true">—</span> : <Typography align="center" variant="body2">{label}</Typography>}
      </Box>}
    </Box>
  );
}
