// CompleteMultipartUpload can return an XML error after sending HTTP 200.
// Never treat an ETag header alone as proof that an object was committed.
export const parseMusicMultipartCompletion = (body: string, headerEtag?: string | null): string => {
  const document = new DOMParser().parseFromString(body, 'application/xml');
  if (document.querySelector('parsererror')) {
    throw new Error('El almacenamiento devolvió una confirmación XML inválida. Reintenta la finalización.');
  }
  const root = document.documentElement;
  if (root.localName === 'Error') {
    const code = Array.from(root.children).find((element) => element.localName === 'Code')?.textContent?.trim();
    const safeCode = code && /^[A-Za-z0-9]{1,64}$/.test(code) ? ` (${code})` : '';
    throw new Error(`El almacenamiento no completó la carga${safeCode}. Reintenta o revisa las partes cargadas.`);
  }
  if (root.localName !== 'CompleteMultipartUploadResult') {
    throw new Error('El almacenamiento no confirmó la finalización multipart. Reintenta la finalización.');
  }
  const etags = Array.from(root.children).filter((element) => element.localName === 'ETag');
  const etag = etags[0]?.textContent?.trim();
  if (etags.length !== 1 || !etag) {
    throw new Error('El almacenamiento no devolvió un ETag final inequívoco.');
  }
  const header = headerEtag?.trim();
  if (header && header !== etag) {
    throw new Error('El ETag de la confirmación no coincide con la cabecera. Reintenta la finalización.');
  }
  return etag;
};
