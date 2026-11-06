import { useState } from 'react';
import Link, { type LinkProps } from '@mui/material/Link';
import Alert from '@mui/material/Alert';
import { downloadStorageUrl, storageFileIdFromUrl } from '../api/contentStorage';

export default function PrivateAttachmentLink({ href, fileName, children, ...props }:
  LinkProps & { href: string; fileName: string }) {
  const [failed, setFailed] = useState(false);
  const [busy, setBusy] = useState(false);
  const managed = storageFileIdFromUrl(href) !== null;
  return <>
    <Link {...props} href={href} aria-busy={busy} onClick={managed ? (event) => {
      event.preventDefault();
      if (busy) return;
      setBusy(true); setFailed(false);
      void downloadStorageUrl(href, fileName).catch(() => setFailed(true)).finally(() => setBusy(false));
    } : undefined}>{children}</Link>
    {failed && <Alert severity="error">No pudimos abrir el adjunto. Comprueba tu sesión y vuelve a intentar.</Alert>}
  </>;
}
