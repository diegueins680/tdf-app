import { useEffect, useRef, useState } from 'react';
import { Alert, Button, Checkbox, FormControlLabel, MenuItem, Stack, TextField, Typography } from '@mui/material';
import { useMutation, useQuery } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import { Catalogs } from '../api/catalogs';
import { submitFeedback } from '../api/feedback';
import { useSession } from '../session/SessionContext';
import { useMobileTelemetry } from './telemetry';
import type { MobilePlatform } from './distribution';

export default function MobileFeedbackForm({ platform, request = false }: { platform: MobilePlatform; request?: boolean }) {
  const { t, i18n } = useTranslation();
  const { session } = useSession();
  const track = useMobileTelemetry(request ? 'app_enrollment' : 'app_feedback');
  const [email, setEmail] = useState(session?.username?.includes('@') ? session.username : '');
  const [description, setDescription] = useState('');
  const [kind, setKind] = useState('bug');
  const [consent, setConsent] = useState(false);
  const [attachment, setAttachment] = useState<File | null>(null);
  const [fileError, setFileError] = useState(false);
  const opened = useRef(false);
  const fileInput = useRef<HTMLInputElement>(null);
  useEffect(() => {
    if (!opened.current && !request) { opened.current = true; track('mobile_feedback_opened', { platform }); }
  }, [platform, request, track]);
  const catalogs = useQuery({
    queryKey: ['catalogs', 'mobile-feedback', i18n.language],
    queryFn: () => Catalogs.listPublicBatch(['feedback-categories', 'feedback-severities'], { locale: i18n.language, page: 1, pageSize: 100 }),
  });
  const defaultId = (code: string, scope: string) => {
    const page = catalogs.data?.catalogs.find(p => p.catalog.code === code);
    const preferred = scope === 'feedback-category' ? page?.items.find(i => i.code === kind && i.active && i.workflowState === 'published' && !i.deprecatedAt)?.id : undefined;
    const id = preferred ?? page?.defaults.find(d => d.scopeKind === scope && d.scopeId === 'global' && !d.localeId)?.entityId;
    return page?.items.find(item => item.id === id && item.active && item.workflowState === 'published' && !item.deprecatedAt)?.id;
  };
  const categoryId = defaultId('feedback-categories', 'feedback-category');
  const severityId = defaultId('feedback-severities', 'feedback-severity');
  const mutation = useMutation({
    mutationFn: () => submitFeedback({
      title: `${t(request ? 'app.requestTitle' : 'app.feedbackTitle')} — ${platform}`,
      description: request ? `mobile_testing_access_request\nplatform: ${platform}\nlocale: ${i18n.language}\nVoluntary request; admission pending.`
        : `${description.trim()}\n\n[TDF Mobile]\nkind: ${kind}\nplatform: ${platform}\nlocale: ${i18n.language}\nsurface: app_feedback`,
      categoryId: categoryId!, severityId: severityId!, consent,
      contactEmail: request ? email.trim() : undefined, attachment: request ? undefined : attachment,
    }),
    onSuccess: () => {
      track(request ? 'mobile_testing_request_submitted' : 'mobile_feedback_submitted', { platform, ...(request ? {} : { feedback_kind: kind }) });
      setDescription(''); setAttachment(null); setConsent(false);
    },
  });
  if (mutation.isSuccess) return <Alert severity="success" role="status">{t(request ? 'app.requested' : 'app.received')}</Alert>;
  return <Stack component="form" spacing={2} onSubmit={e => { e.preventDefault(); if (consent && categoryId && severityId && !mutation.isPending) mutation.mutate(); }}>
    <Typography>{t(request ? 'app.requestHelp' : 'app.feedbackIntro')}</Typography>
    {request ? <TextField required type="email" autoComplete="email" label={t('app.email')} value={email} inputProps={{ maxLength: 254 }} onChange={e => setEmail(e.target.value)} /> : <>
      <TextField select label={t('app.kind')} value={kind} onChange={e => setKind(e.target.value)}>
        {['bug', 'ux', 'idea', 'general'].map(k => <MenuItem key={k} value={k}>{t(`app.${k}`)}</MenuItem>)}
      </TextField>
      <TextField required multiline minRows={4} label={t('app.description')} helperText={t('app.descriptionHelp')} inputProps={{ maxLength: 4000 }} value={description} onChange={e => setDescription(e.target.value)} />
      <Button variant="outlined" onClick={() => fileInput.current?.click()} sx={{ minHeight: 44 }}>{t('app.attachment')}</Button>
      <input ref={fileInput} aria-label={t('app.attachment')} type="file" accept="image/png,image/jpeg" hidden onChange={e => {
          const file = e.target.files?.[0]; const valid = !file || (['image/png', 'image/jpeg'].includes(file.type) && file.size <= 5 * 1024 * 1024);
          setFileError(!valid); setAttachment(valid ? file ?? null : null);
        }} />
      {attachment && <Typography>{attachment.name}</Typography>}
      {fileError && <Alert severity="error">{t('app.attachmentInvalid')}</Alert>}
      <Typography variant="body2">{t('app.metadata')}</Typography>
    </>}
    <FormControlLabel control={<Checkbox checked={consent} onChange={e => setConsent(e.target.checked)} />} label={t(request ? 'app.consent' : 'app.feedbackConsent')} />
    {request && <Typography variant="body2">{t('app.enrollment')}</Typography>}
    {(catalogs.isError || (catalogs.isSuccess && (!categoryId || !severityId))) && <Alert severity="error">{t('app.catalogError')} <Button onClick={() => void catalogs.refetch()}>{t('app.retry')}</Button></Alert>}
    {mutation.isError && <Alert severity="error">{t('app.error')}</Alert>}
    <Button variant="contained" type="submit" sx={{ minHeight: 44 }} disabled={mutation.isPending || !consent || !categoryId || !severityId || (request ? !email.trim() : !description.trim()) || fileError}>{t(mutation.isPending ? 'app.sending' : 'app.send')}</Button>
  </Stack>;
}
