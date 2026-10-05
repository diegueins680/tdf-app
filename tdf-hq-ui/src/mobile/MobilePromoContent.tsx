import { useEffect, useRef, useState } from 'react';
import { Button, Paper, Stack, Typography } from '@mui/material';
import { Link, useLocation } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import { appLink, useMobileTelemetry } from './telemetry';
import { detectPlatform } from './distribution';

export const DISMISS_KEY = 'tdf:mobile-promo-dismissed:v1';
export const DISMISS_MS = 30 * 24 * 60 * 60 * 1000;
export function promoDismissed(now = Date.now()): boolean {
  try { const until = Number(localStorage.getItem(DISMISS_KEY)); return Number.isFinite(until) && until > now; } catch { return true; }
}
export default function MobilePromo({ surface, banner = false, compact = false }: { surface: string; banner?: boolean; compact?: boolean }) {
  const { t } = useTranslation();
  const { search } = useLocation();
  const track = useMobileTelemetry(surface);
  const [dismissed, setDismissed] = useState(() => banner && promoDismissed());
  const ref = useRef<HTMLDivElement>(null);
  const measured = useRef(false);
  const trackRef = useRef(track);
  trackRef.current = track;
  const visible = !dismissed && (!banner || detectPlatform(navigator.userAgent, navigator.maxTouchPoints) !== 'desktop');
  useEffect(() => {
    measured.current = false;
    if (!visible || !ref.current || !('IntersectionObserver' in window)) return;
    const observer = new IntersectionObserver(([entry]) => {
      if (entry?.isIntersecting && !measured.current) {
        measured.current = true;
        trackRef.current('mobile_promo_viewed');
        observer.disconnect();
      }
    }, { threshold: 0.5 });
    observer.observe(ref.current);
    return () => observer.disconnect();
  }, [visible, surface]);
  if (!visible) return null;
  return <Paper ref={ref} component="aside" aria-label={t('app.title')} variant="outlined" sx={{ p: compact ? 0 : 2, my: 2, border: compact ? 0 : undefined, minWidth: 0 }}>
    <Stack spacing={1}>
      {!compact && <Typography fontWeight={700}>{t('app.invite')}</Typography>}
      {!banner && !compact && <Typography color="text.secondary">{t('app.promo')}</Typography>}
      <Stack direction="row" useFlexGap sx={{ flexWrap: 'wrap' }} spacing={1}>
        <Button component={Link} to={appLink(search, surface)} onClick={() => track('mobile_testing_interest_clicked')} sx={{ minHeight: 44 }}>{t(compact ? 'app.menu' : 'app.discover')}</Button>
        {banner && <Button sx={{ minHeight: 44 }} onClick={() => {
          setDismissed(true);
          try { localStorage.setItem(DISMISS_KEY, String(Date.now() + DISMISS_MS)); } catch { /* in-memory dismiss still works */ }
          track('mobile_promo_dismissed');
        }}>{t('app.dismiss')}</Button>}
      </Stack>
    </Stack>
  </Paper>;
}
