import { useEffect, useRef, useState } from 'react';
import { Alert, Button, Checkbox, FormControlLabel, Stack, Typography } from '@mui/material';
import { useMutation, useQuery } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import { Link as RouterLink } from 'react-router-dom';
import { isAccountDeletionFormEnabled } from '../config/accountDeletionRollout';
import { ApiError } from '../api/client';
import { Catalogs } from '../api/catalogs';
import { requestAccountDeletion } from '../api/accountDeletion';
import { loadSessionSnapshot } from '../api/session';
import { useSession } from '../session/SessionContext';
import { LegalDisclosure } from '../components/legal/LegalDisclosure';
import { buildLoginRedirectPath } from '../utils/loginRouting';

function DeletionForm({ partyId, username, onAuthenticationLost }: { partyId: number; username: string; onAuthenticationLost: () => void }) {
  const { t, i18n } = useTranslation();
  const [confirmed, setConfirmed] = useState(false);
  const sending = useRef(false);
  const catalogs = useQuery({
    queryKey: ['catalogs', 'account-deletion', i18n.language],
    queryFn: () => Catalogs.listPublicBatch(['feedback-categories', 'feedback-severities'], { locale: i18n.language, page: 1, pageSize: 100 }),
    retry: false,
  });
  const published = (catalog: string, codes: string[]) => {
    const items = catalogs.data?.catalogs.find(page => page.catalog.code === catalog)?.items;
    return codes.map(code => items?.find(item => item.code === code && item.active && item.workflowState === 'published' && !item.deprecatedAt)?.id).find(Boolean);
  };
  const categoryId = published('feedback-categories', ['permissions', 'question', 'suggestion', 'idea']);
  const severityId = published('feedback-severities', ['p4', 'p3']);
  const mutation = useMutation({
    mutationFn: () => requestAccountDeletion({ partyId, categoryId: categoryId!, severityId: severityId!, locale: i18n.language }),
    onError: error => {
      if (error instanceof ApiError && error.status === 401) onAuthenticationLost();
    },
    onSettled: () => { sending.current = false; },
  });
  if (mutation.isSuccess) return <Alert severity="success" role="status">{t('accountDeletion.received')}</Alert>;
  return <Stack component="form" spacing={2} onSubmit={event => {
    event.preventDefault();
    if (!confirmed || !categoryId || !severityId || sending.current) return;
    sending.current = true;
    mutation.mutate();
  }}>
    <Typography sx={{ overflowWrap: 'anywhere' }}>{t('accountDeletion.account', { username })}</Typography>
    <FormControlLabel control={<Checkbox checked={confirmed} onChange={event => setConfirmed(event.target.checked)} />} label={t('accountDeletion.confirm')} />
    {(catalogs.isError || (catalogs.isSuccess && (!categoryId || !severityId))) && <Alert severity="error">{t('accountDeletion.unavailable')} <Button onClick={() => void catalogs.refetch()}>{t('accountDeletion.retry')}</Button></Alert>}
    {mutation.isError && <Alert severity="error">{t('accountDeletion.error')}</Alert>}
    <Button type="submit" color="error" variant="contained" disableRipple sx={theme => ({ bgcolor: 'error.dark', color: 'common.white', transition: 'none', '&:hover': { bgcolor: 'error.dark' }, '&.Mui-disabled': { bgcolor: theme.palette.mode === 'dark' ? 'grey.800' : 'grey.300', color: 'text.primary' } })} disabled={!confirmed || !categoryId || !severityId || mutation.isPending}>{t(mutation.isPending ? 'accountDeletion.sending' : 'accountDeletion.submit')}</Button>
  </Stack>;
}

export default function AccountDeletionPage() {
  const { t, i18n } = useTranslation();
  const { session, loading, logout } = useSession();
  const formEnabled = isAccountDeletionFormEnabled();
  const [reauthenticationRequired, setReauthenticationRequired] = useState(false);
  const identity = useQuery({
    queryKey: ['account-deletion-session', session?.partyId],
    queryFn: loadSessionSnapshot,
    enabled: formEnabled && !!session && !loading,
    retry: false,
    staleTime: 0,
    gcTime: 0,
  });
  useEffect(() => {
    const robots = document.createElement('meta'); robots.name = 'robots'; robots.content = 'noindex,nofollow'; document.head.appendChild(robots);
    return () => robots.remove();
  }, []);
  const authenticated = !reauthenticationRequired && !!session && !!identity.data && identity.data.partyId === session.partyId;
  return <Stack spacing={3} sx={{ maxWidth: 760, mx: 'auto', '& .MuiButtonBase-root': { minHeight: 44 }, '& .Mui-focusVisible': { outline: '3px solid currentColor', outlineOffset: 3 } }}>
    <Stack direction="row" spacing={1} justifyContent="flex-end" aria-label={t('app.language')}>
      <Button aria-pressed={i18n.language.startsWith('es')} onClick={() => void i18n.changeLanguage('es')}>Español</Button>
      <Button aria-pressed={i18n.language.startsWith('en')} onClick={() => void i18n.changeLanguage('en')}>English</Button>
    </Stack>
    <Typography component="h1" variant="h3">{t('accountDeletion.title')}</Typography>
    <Typography>{t(formEnabled ? 'accountDeletion.intro' : 'accountDeletion.emailIntro')}</Typography>
    <LegalDisclosure id="account-deletion-terms" language={i18n.language.startsWith('en') ? 'en' : 'es'} headingLevel={2} title={t('accountDeletion.termsTitle')}>
      <Typography>{t('accountDeletion.timing')}</Typography>
      <Typography>{t('accountDeletion.retention')}</Typography>
    </LegalDisclosure>
    {!formEnabled ? <Stack spacing={2}>
      <Typography>{t('accountDeletion.emailHelp')}</Typography>
      <Button component="a" href="mailto:info@tdfrecords.net?subject=TDF%20account%20deletion" variant="contained">{t('accountDeletion.emailRequest')}</Button>
    </Stack> : loading || (session && identity.isPending) ? <Typography role="status">{t('accountDeletion.loading')}</Typography> : authenticated
      ? <DeletionForm key={identity.data!.partyId} partyId={identity.data!.partyId} username={identity.data!.username} onAuthenticationLost={() => setReauthenticationRequired(true)} />
      : <Stack spacing={2}>
        {identity.isError && <Alert severity="error">{t('accountDeletion.unavailable')} <Button onClick={() => void identity.refetch()}>{t('accountDeletion.retry')}</Button></Alert>}
        <Typography>{t('accountDeletion.loginHelp')}</Typography>
        <Button variant="contained" component={RouterLink} to={buildLoginRedirectPath('/cuenta/eliminar')} onClick={() => { if (session) logout(); }}>{t('accountDeletion.login')}</Button>
      </Stack>}
    <Button component="a" href="/mobile-app/privacy.html">{t('accountDeletion.privacy')}</Button>
  </Stack>;
}
