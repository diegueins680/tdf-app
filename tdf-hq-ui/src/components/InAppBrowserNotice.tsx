import { useState } from 'react';
import { Alert, AlertTitle, Button, Stack, Typography } from '@mui/material';
import OpenInNewIcon from '@mui/icons-material/OpenInNew';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import { useTranslation } from 'react-i18next';
import { buildOpenInSystemBrowserHref, detectInAppBrowser } from '../utils/inAppBrowser';
import type { InAppBrowserHost } from '../utils/inAppBrowser';

const HOST_LABEL: Record<InAppBrowserHost, string | null> = {
  instagram: 'Instagram',
  facebook: 'Facebook',
  messenger: 'Messenger',
  tiktok: 'TikTok',
  line: 'LINE',
  snapchat: 'Snapchat',
  other: null,
};

export function useInAppBrowser(): InAppBrowserHost | null {
  const [host] = useState(() => (typeof navigator === 'undefined' ? null : detectInAppBrowser(navigator.userAgent)));
  return host;
}

/**
 * Shown on the login/signup card when the page runs inside a social app's
 * embedded browser, where Google Sign-In cannot use passkeys and often fails.
 * Offers the system browser (one tap on iOS/Android) or copying the link,
 * and reminds the person that email + password works here.
 */
export function InAppBrowserNotice({ host, context }: { host: InAppBrowserHost; context: 'login' | 'signup' }) {
  const { t } = useTranslation();
  const [copied, setCopied] = useState<'ok' | 'failed' | null>(null);
  const currentUrl = typeof window === 'undefined' ? '' : window.location.href;
  const openHref = currentUrl ? buildOpenInSystemBrowserHref(currentUrl, navigator.userAgent, navigator.maxTouchPoints) : null;
  const platformIsIos = openHref?.startsWith('x-safari-') ?? false;
  const appName = HOST_LABEL[host] ?? t('authEntry.inAppBrowserGenericApp');

  const copyLink = async () => {
    try {
      await navigator.clipboard.writeText(currentUrl);
      setCopied('ok');
    } catch {
      setCopied('failed');
    }
  };

  return (
    <Alert severity="info" sx={{ width: '100%', '& .MuiAlert-message': { width: '100%' } }} data-testid="in-app-browser-notice">
      <AlertTitle>{t('authEntry.inAppBrowserTitle', { app: appName })}</AlertTitle>
      <Typography variant="body2">
        {t(context === 'signup' ? 'authEntry.inAppBrowserBodySignup' : 'authEntry.inAppBrowserBodyLogin')}
      </Typography>
      <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap sx={{ mt: 1.5 }}>
        {openHref && (
          <Button
            variant="contained"
            size="small"
            startIcon={<OpenInNewIcon />}
            href={openHref}
            rel="noreferrer"
          >
            {t(platformIsIos ? 'authEntry.inAppBrowserOpenSafari' : 'authEntry.inAppBrowserOpenChrome')}
          </Button>
        )}
        <Button variant="outlined" size="small" startIcon={<ContentCopyIcon />} onClick={() => { void copyLink(); }}>
          {t('authEntry.inAppBrowserCopyLink')}
        </Button>
      </Stack>
      <Typography variant="caption" role="status" sx={{ display: 'block', mt: 0.75 }}>
        {copied === 'ok' ? t('authEntry.inAppBrowserCopied') : copied === 'failed' ? t('authEntry.inAppBrowserCopyFailed') : t('authEntry.inAppBrowserMenuHint')}
      </Typography>
    </Alert>
  );
}
