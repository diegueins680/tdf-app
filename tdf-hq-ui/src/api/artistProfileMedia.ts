import { getAuthSessionEpoch } from '../session/authEvents';
import { prepareArtistImageUrl } from '../utils/artistImage';

function verifyImage(url: string): Promise<void> {
  return new Promise((resolve, reject) => {
    const image = new Image();
    const finish = (valid: boolean) => {
      window.clearTimeout(timeout);
      image.onload = null;
      image.onerror = null;
      if (valid) resolve();
      else reject(new Error('No pudimos cargar la portada. Comprueba que el enlace sea público o vuelve a subir la imagen.'));
    };
    const timeout = window.setTimeout(() => finish(false), 15000);
    image.onload = () => finish(image.naturalWidth > 0 && image.naturalHeight > 0);
    image.onerror = () => finish(false);
    image.src = url;
  });
}

/** Validate before any write, retaining the account that started the request. */
export async function saveArtistProfileMedia<Payload extends { apuHeroImageUrl?: string | null }, Result>(
  payload: Payload, write: (prepared: Payload) => Promise<Result>,
): Promise<Result> {
  const epoch = getAuthSessionEpoch();
  if (!Object.prototype.hasOwnProperty.call(payload, 'apuHeroImageUrl')) return write(payload);
  const imageUrl = prepareArtistImageUrl(payload.apuHeroImageUrl);
  if (imageUrl) await verifyImage(imageUrl);
  if (epoch !== getAuthSessionEpoch()) throw new Error('La sesión cambió. Vuelve a guardar desde tu cuenta actual.');
  return write({ ...payload, apuHeroImageUrl: imageUrl });
}
