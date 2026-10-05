import { useEffect, useState } from 'react';
import { Alert, Button, Chip, Paper, Stack, ToggleButton, ToggleButtonGroup, Typography } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import { MOBILE_CANONICAL_URL, availableChannel, channelLabel, detectPlatform, validateDistribution, type MobilePlatform } from '../mobile/distribution';
import MobileFeedbackForm from '../mobile/MobileFeedbackForm';
import { useMobileTelemetry } from '../mobile/telemetry';

export default function MobileAppPage() {
  const { t, i18n } = useTranslation();
  const detected = detectPlatform(navigator.userAgent, navigator.maxTouchPoints);
  const [platform, setPlatform] = useState<MobilePlatform>(detected === 'ios' ? 'ios' : 'android');
  const [form, setForm] = useState<'request' | 'feedback' | null>(new URLSearchParams(window.location.search).get('feedback') === '1' ? 'feedback' : null);
  const [now, setNow] = useState(Date.now);
  const track = useMobileTelemetry('app_landing');
  const distribution = useQuery({
    queryKey: ['mobile-distribution'],
    queryFn: async () => {
      const response = await fetch('/mobile-distribution.json', { cache: 'no-store' });
      if (!response.ok) throw new Error('Distribution unavailable');
      return validateDistribution(await response.json());
    }, staleTime: 60_000, refetchInterval: 60_000, retry: false,
  });
  useEffect(() => {
    const previous = document.title;
    document.title = 'TDF Mobile | TDF Records';
    const canonical = document.createElement('link'); canonical.rel = 'canonical'; canonical.href = MOBILE_CANONICAL_URL; document.head.appendChild(canonical);
    const interval = window.setInterval(() => setNow(Date.now()), 30_000);
    return () => { document.title = previous; canonical.remove(); clearInterval(interval); };
  }, []);
  const channel = distribution.data?.[platform];
  const active = channel && availableChannel(channel, now);
  const beta = channel?.status !== 'public' && channel?.status !== 'store_preorder';
  return <Stack spacing={3} sx={{ maxWidth: 760, mx: 'auto', '& .MuiButtonBase-root': { minHeight: 44 }, '& .Mui-focusVisible': { outline: '3px solid currentColor', outlineOffset: 3 } }}>
    <Stack direction="row" spacing={1} justifyContent="flex-end" aria-label={t('app.language')}>
      <Button aria-pressed={i18n.language === 'es'} onClick={() => void i18n.changeLanguage('es')}>Español</Button>
      <Button aria-pressed={i18n.language === 'en'} onClick={() => void i18n.changeLanguage('en')}>English</Button>
    </Stack>
    <Typography component="h1" variant="h3" fontWeight={800}>{t('app.title')}</Typography>
    <Typography variant="h5" component="p">{t('app.invite')}</Typography>
    <Typography>{t('app.intro')}</Typography>
    {beta && <Typography>{t('app.beta')}</Typography>}
    <ToggleButtonGroup exclusive value={platform} aria-label={t('app.choose')} onChange={(_, value: MobilePlatform | null) => {
      if (value) { setPlatform(value); setForm(null); track('mobile_platform_selected', { platform: value }); }
    }}>
      <ToggleButton value="android">{t('app.android')}</ToggleButton>
      <ToggleButton value="ios">{t('app.ios')}</ToggleButton>
    </ToggleButtonGroup>
    <Paper variant="outlined" sx={{ p: { xs: 2, sm: 3 } }}><Stack spacing={2}>
      <Typography component="h2" variant="h5">{t(`app.${platform}`)}</Typography>
      {distribution.isPending ? <Typography role="status" aria-live="polite">{t('app.loading')}</Typography> : active ? <>
        <Chip sx={{ alignSelf: 'flex-start' }} label={t(channel.status === 'store_preorder' ? 'app.preorder' : beta ? 'app.betaStatus' : 'app.publicStatus')} />
        {channel.status === 'closed_testing' && channel.admission === 'approval_required' && <Alert severity="info">{t('app.stepClosed')}</Alert>}
        {channel.enrollmentUrl && <Button component="a" href={channel.enrollmentUrl} referrerPolicy="no-referrer" onClick={() => track('mobile_testing_join_clicked', { platform, distribution_status: channel.status, destination: 'tester_group' })}>{t('app.group')}</Button>}
        {channel.status === 'closed_testing' && channel.admission === 'approval_required' && !channel.enrollmentUrl
          ? <Button variant="contained" onClick={() => { setForm('request'); track('mobile_testing_interest_clicked', { platform, distribution_status: channel.status }); }}>{t('app.request')}</Button>
          : <Button component="a" variant="contained" href={channel.url} referrerPolicy="no-referrer" onClick={event => {
            if (!availableChannel(channel)) { event.preventDefault(); setForm('request'); return; }
            track(beta ? 'mobile_testing_join_clicked' : 'mobile_store_clicked', { platform, distribution_status: channel.status, destination: beta ? 'testing' : 'store' });
          }}>{t(channelLabel(channel, platform))}</Button>}
      </> : <>
        <Alert severity="info">{t(channel?.capacity === 'full' ? 'app.full' : 'app.unavailable')}</Alert>
        <Button variant="contained" onClick={() => { setForm('request'); track('mobile_testing_interest_clicked', { platform, distribution_status: channel?.status ?? 'unavailable' }); }}>{t('app.request')}</Button>
      </>}
      {distribution.isError && <Button onClick={() => void distribution.refetch()}>{t('app.retry')}</Button>}
    </Stack></Paper>
    <Typography component="h2" variant="h5">{t('app.steps')}</Typography>
    <Typography>{t(channel?.status === 'store_preorder' ? 'app.stepPreorder' : channel?.status === 'public' ? (platform === 'ios' ? 'app.stepIosPublic' : 'app.stepAndroidPublic') : platform === 'ios' ? 'app.stepIos' : 'app.stepAndroid')}</Typography>
    <Typography>{t('app.stepUse')}</Typography>
    <Button variant="outlined" onClick={() => setForm('feedback')}>{t('app.openFeedback')}</Button>
    {form && <Paper variant="outlined" sx={{ p: 2 }}><Stack spacing={2}>
      <Typography component="h2" variant="h5">{t(form === 'request' ? 'app.request' : 'app.feedback')}</Typography>
      <MobileFeedbackForm key={`${form}-${platform}`} platform={platform} request={form === 'request'} />
    </Stack></Paper>}
    <Typography variant="body2" color="text.secondary">{t('app.optout')}</Typography>
    <Button component="a" href="/mobile-app/privacy.html">{t('app.privacy')}</Button>
    <Typography variant="body2">{t('app.share')}</Typography>
  </Stack>;
}
