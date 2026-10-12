import { useEffect, useRef, useState } from 'react';
import { Alert, Button, Chip, Paper, Stack, ToggleButton, ToggleButtonGroup, Typography } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import { useLocation } from 'react-router-dom';
import QRCode from 'qrcode';
import { captureGrowthAttribution } from '../analytics/growthAttribution';
import { MOBILE_CANONICAL_URL, availableChannel, channelLabel, detectPlatform, validateDistribution, type DistributionChannel, type MobilePlatform } from '../mobile/distribution';
import MobileFeedbackForm from '../mobile/MobileFeedbackForm';
import { smartLink, useMobileTelemetry } from '../mobile/telemetry';

// Official store artwork (public/badges), in the page language. Apple's badge is only for App Store listings, so
// TestFlight keeps a text button; a pre-order keeps its text label because both badges say "download".
const storeBadge = (platform: MobilePlatform, channel: DistributionChannel, language: string): { src: string; height: number } | null => {
  if (!channel.url || channel.status === 'store_preorder') return null;
  let host = '';
  try { host = new URL(channel.url).hostname; } catch { return null; }
  // Only es and en translate this page; every other locale falls back to English copy.
  const locale = language.toLowerCase().startsWith('es') ? 'es' : 'en';
  if (platform === 'android' && host === 'play.google.com') return { src: `/badges/google-play-badge-${locale}.png`, height: 60 };
  if (platform === 'ios' && channel.status === 'public' && host === 'apps.apple.com') return { src: `/badges/app-store-badge-${locale}.svg`, height: 44 };
  return null;
};

export default function MobileAppPage() {
  const { t, i18n } = useTranslation();
  const { search, pathname } = useLocation();
  const detected = detectPlatform(navigator.userAgent, navigator.maxTouchPoints);
  const [platform, setPlatform] = useState<MobilePlatform>(detected === 'ios' ? 'ios' : 'android');
  const [form, setForm] = useState<'request' | 'feedback' | null>(new URLSearchParams(search).get('feedback') === '1' ? 'feedback' : null);
  const [now, setNow] = useState(Date.now);
  const [qr, setQr] = useState<string | null>(null);
  const [copied, setCopied] = useState<'done' | 'failed' | null>(null);
  const viewed = useRef(false);
  const track = useMobileTelemetry('app_landing');
  // One canonical link to share: the smart link keeps only the allowlisted campaign tags of this visit.
  const shareUrl = smartLink(search, 'share');
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
  // Campaign tags on the landing are kept first-party so later funnel events (join, feedback) stay attributed
  // after the visitor navigates within TDF. Only utm labels and referral codes are stored; never the full query.
  useEffect(() => { captureGrowthAttribution({ search, pathname }); }, [search, pathname]);
  useEffect(() => { if (new URLSearchParams(search).get('feedback') === '1') setForm('feedback'); }, [search]);
  const channel = distribution.data?.[platform];
  // Exposure is reported once per visit, after the manifest answered, with the state the visitor actually saw.
  useEffect(() => {
    if (viewed.current || distribution.isPending) return;
    viewed.current = true;
    track('mobile_landing_viewed', { platform, distribution_status: distribution.data?.[platform]?.status ?? 'unknown', detected_platform: detected });
  }, [distribution.isPending, distribution.data, platform, detected, track]);
  // Desktop visitors scan the smart link with the phone they will install on; phones already are the phone.
  const showQr = detected === 'desktop';
  const qrUrl = smartLink(search, 'qr');
  useEffect(() => {
    if (!showQr) return;
    let cancelled = false;
    QRCode.toString(qrUrl, { type: 'svg', margin: 1, errorCorrectionLevel: 'M' })
      .then(svg => { if (!cancelled) setQr(`data:image/svg+xml;utf8,${encodeURIComponent(svg)}`); })
      .catch(() => { if (!cancelled) setQr(null); });
    return () => { cancelled = true; };
  }, [showQr, qrUrl]);
  const copyLink = async () => {
    try {
      if (!navigator.clipboard?.writeText) throw new Error('Clipboard unavailable');
      await navigator.clipboard.writeText(shareUrl);
      setCopied('done');
      track('mobile_share_link_copied', { platform });
    } catch { setCopied('failed'); }
  };
  const active = channel && availableChannel(channel, now);
  const beta = channel?.status !== 'public' && channel?.status !== 'store_preorder';
  const badge = channel ? storeBadge(platform, channel, i18n.language) : null;
  const badgeContent = (label: string) => badge
    ? <img src={badge.src} alt={label} height={badge.height} style={{ display: 'block', height: badge.height, width: 'auto' }} />
    : label;
  const badgeSx = badge ? { alignSelf: 'flex-start', p: 0, minWidth: 0, bgcolor: 'transparent', boxShadow: 'none', '&:hover': { bgcolor: 'transparent', boxShadow: 'none' } } : undefined;
  return <Stack spacing={3} sx={{ maxWidth: 760, mx: 'auto', '& .MuiButtonBase-root': { minHeight: 44 }, '& .Mui-focusVisible': { outline: '3px solid currentColor', outlineOffset: 3 } }}>
    <Stack direction="row" spacing={1} justifyContent="flex-end" aria-label={t('app.language')}>
      <Button aria-pressed={i18n.language === 'es'} onClick={() => void i18n.changeLanguage('es')}>Español</Button>
      <Button aria-pressed={i18n.language === 'en'} onClick={() => void i18n.changeLanguage('en')}>English</Button>
    </Stack>
    <Typography component="h1" variant="h3" fontWeight={800}>{t('app.title')}</Typography>
    <Typography variant="h5" component="p">{t('app.invite')}</Typography>
    <Typography>{t('app.intro')}</Typography>
    {beta && <Typography>{t('app.beta')}</Typography>}
    <ToggleButtonGroup sx={{ '& .MuiToggleButton-root': { color: 'text.primary' } }} exclusive value={platform} aria-label={t('app.choose')} onChange={(_, value: MobilePlatform | null) => {
      if (value) { setPlatform(value); setForm(null); track('mobile_platform_selected', { platform: value }); }
    }}>
      <ToggleButton value="android">{t('app.android')}</ToggleButton>
      <ToggleButton value="ios">{t('app.ios')}</ToggleButton>
    </ToggleButtonGroup>
    <Paper variant="outlined" sx={{ p: { xs: 2, sm: 3 } }}><Stack spacing={2}>
      <Typography component="h2" variant="h5">{t(`app.${platform}`)}</Typography>
      {distribution.isPending ? <Typography role="status" aria-live="polite">{t('app.loading')}</Typography> : active ? <>
        <Chip sx={{ alignSelf: 'flex-start' }} label={t(channel.status === 'store_preorder' ? 'app.preorder' : beta ? 'app.betaStatus' : 'app.publicStatus')} />
        {channel.status === 'closed_testing' && channel.admission === 'approval_required' && <Alert severity="info">{t(channel.enrollmentUrl ? 'app.stepClosed' : 'app.stepClosedEmail')}</Alert>}
        {channel.enrollmentUrl && <Button component="a" href={channel.enrollmentUrl} referrerPolicy="no-referrer" onClick={() => track('mobile_testing_join_clicked', { platform, distribution_status: channel.status, destination: 'tester_group' })}>{t('app.group')}</Button>}
        {channel.status === 'closed_testing' && channel.admission === 'approval_required' && !channel.enrollmentUrl
          ? <>
            <Button variant="contained" onClick={() => { setForm('request'); track('mobile_testing_interest_clicked', { platform, distribution_status: channel.status }); }}>{t('app.request')}</Button>
            {badge && <Typography variant="body2">{t('app.alreadyAdmitted')}</Typography>}
            <Button component="a" variant={badge ? 'text' : 'outlined'} sx={badgeSx} href={channel.url} referrerPolicy="no-referrer" onClick={event => {
              if (!availableChannel(channel)) { event.preventDefault(); setForm('request'); return; }
              track('mobile_testing_join_clicked', { platform, distribution_status: channel.status, destination: 'testing' });
            }}>{badgeContent(t('app.alreadyAdmitted'))}</Button>
          </>
          : <Button component="a" variant={badge ? 'text' : 'contained'} sx={badgeSx} href={channel.url} referrerPolicy="no-referrer" onClick={event => {
            if (!availableChannel(channel)) { event.preventDefault(); setForm('request'); return; }
            track(beta ? 'mobile_testing_join_clicked' : 'mobile_store_clicked', { platform, distribution_status: channel.status, destination: beta ? 'testing' : 'store' });
          }}>{badgeContent(t(channelLabel(channel, platform)))}</Button>}
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
    <Paper variant="outlined" component="section" aria-labelledby="mobile-share-heading" sx={{ p: 2 }}><Stack spacing={1.5}>
      <Typography id="mobile-share-heading" component="h2" variant="h6">{t('app.share')}</Typography>
      <Typography variant="body2" color="text.secondary">{t('app.shareHint')}</Typography>
      <Stack direction="row" useFlexGap sx={{ flexWrap: 'wrap', alignItems: 'center' }} spacing={1}>
        <Typography component="code" sx={{ fontFamily: 'monospace', overflowWrap: 'anywhere' }}>{MOBILE_CANONICAL_URL}</Typography>
        <Button variant="outlined" onClick={() => void copyLink()}>{t('app.copyLink')}</Button>
      </Stack>
      <Typography aria-live="polite" variant="body2" sx={{ minHeight: '1.5em' }}>{copied === 'done' ? t('app.copied') : copied === 'failed' ? t('app.copyFailed') : ''}</Typography>
      {showQr && qr && <Stack spacing={1} alignItems="flex-start">
        <Typography variant="body2">{t('app.qrHint')}</Typography>
        <img src={qr} alt={t('app.qrAlt')} width={160} height={160} style={{ display: 'block', background: '#fff', padding: 4, border: '1px solid #ccc' }} />
      </Stack>}
    </Stack></Paper>
  </Stack>;
}
