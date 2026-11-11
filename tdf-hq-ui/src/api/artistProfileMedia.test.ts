import { jest } from '@jest/globals';
import { fireEvent } from '@testing-library/react';
import { saveArtistProfileMedia } from './artistProfileMedia';
import { advanceAuthSessionEpoch } from '../session/authEvents';

let image: HTMLImageElement;
beforeEach(() => {
  image = document.createElement('img');
  Object.defineProperties(image, { naturalWidth: { value: 640 }, naturalHeight: { value: 480 } });
  jest.spyOn(window, 'Image').mockImplementation(() => image);
});
afterEach(() => { jest.restoreAllMocks(); jest.useRealTimers(); });

it('blocks HTML page links and failed images before writing any profile fields', async () => {
  const write = jest.fn(async (payload: object) => payload);
  await expect(saveArtistProfileMedia({ apuHeroImageUrl: 'https://open.spotify.com/artist/abc' }, write)).rejects.toThrow('Sube una imagen');
  const saving = saveArtistProfileMedia({ apuHeroImageUrl: 'https://cdn.example/missing.jpg' }, write);
  fireEvent.error(image);
  await expect(saving).rejects.toThrow('No pudimos cargar');
  expect(write).not.toHaveBeenCalled();
});

it('persists a public Drive image URL only after the image decodes', async () => {
  const write = jest.fn(async (payload: object) => payload);
  const saving = saveArtistProfileMedia({ apuHeroImageUrl: 'https://drive.google.com/file/d/cover123/view?resourcekey=key' }, write);
  expect(write).not.toHaveBeenCalled();
  expect(image.src).toBe('https://drive.google.com/thumbnail?id=cover123&sz=w1600&resourcekey=key');
  fireEvent.load(image);
  await saving;
  expect(write).toHaveBeenCalledWith({ apuHeroImageUrl: image.src });
});

it('rejects a stale save after switching accounts during image validation', async () => {
  const write = jest.fn(async (payload: object) => payload);
  const saving = saveArtistProfileMedia({ apuHeroImageUrl: 'https://cdn.example/cover.jpg' }, write);
  advanceAuthSessionEpoch();
  fireEvent.load(image);
  await expect(saving).rejects.toThrow('La sesión cambió');
  expect(write).not.toHaveBeenCalled();
});

it('releases the pending save when an image never loads and supports removing the cover', async () => {
  jest.useFakeTimers();
  const write = jest.fn(async (payload: object) => payload);
  const saving = saveArtistProfileMedia({ apuHeroImageUrl: 'https://cdn.example/slow.jpg' }, write);
  const failure = expect(saving).rejects.toThrow('No pudimos cargar');
  await jest.advanceTimersByTimeAsync(15000);
  await failure;
  expect(write).not.toHaveBeenCalled();
  await saveArtistProfileMedia({ apuHeroImageUrl: '' }, write);
  expect(write).toHaveBeenCalledWith({ apuHeroImageUrl: null });
});
