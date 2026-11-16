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

export interface ArtistProfileMediaOptions {
  /** The cover already saved on the profile, when editing an existing one. */
  persistedCover?: string | null;
  /** A dependent write that must only happen once the cover is accepted. */
  beforeWrite?: () => Promise<unknown>;
}

/**
 * Validate before any write, retaining the account that started the request.
 * A cover that is unchanged from the saved profile is kept as it is, so an
 * older embedded upload does not block edits to other fields; only a newly
 * supplied cover has to be a hosted image that loads.
 */
export async function saveArtistProfileMedia<Payload extends { apuHeroImageUrl?: string | null }, Result>(
  payload: Payload, write: (prepared: Payload) => Promise<Result>, options: ArtistProfileMediaOptions = {},
): Promise<Result> {
  const epoch = getAuthSessionEpoch();
  let prepared = payload;
  if (Object.prototype.hasOwnProperty.call(payload, 'apuHeroImageUrl')) {
    const supplied = payload.apuHeroImageUrl?.trim() ?? '';
    const unchanged = supplied !== '' && supplied === (options.persistedCover?.trim() ?? '');
    if (!unchanged) {
      const imageUrl = prepareArtistImageUrl(payload.apuHeroImageUrl);
      if (imageUrl) await verifyImage(imageUrl);
      prepared = { ...payload, apuHeroImageUrl: imageUrl };
    }
  }
  if (epoch !== getAuthSessionEpoch()) throw new Error('La sesión cambió. Vuelve a guardar desde tu cuenta actual.');
  if (options.beforeWrite) {
    await options.beforeWrite();
    if (epoch !== getAuthSessionEpoch()) throw new Error('La sesión cambió. Vuelve a guardar desde tu cuenta actual.');
  }
  return write(prepared);
}
