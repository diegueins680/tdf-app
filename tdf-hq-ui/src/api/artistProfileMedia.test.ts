import { jest } from '@jest/globals';
import { fireEvent } from '@testing-library/react';
import { saveArtistProfileMedia } from './artistProfileMedia';
import { advanceAuthSessionEpoch, getAuthSessionEpoch } from '../auth/authEvents';

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
  const saving = saveArtistProfileMedia({ apuHeroImageUrl: 'https://cdn.example/cover.jpg' }, write, { sessionEpoch: getAuthSessionEpoch });
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

it('keeps an unchanged legacy embedded cover so other profile fields can still be saved', async () => {
  const write = jest.fn(async (payload: object) => payload);
  const legacy = 'data:image/png;base64,iVBORw0KGgo=';
  await saveArtistProfileMedia({ apuBio: 'Nueva bio', apuHeroImageUrl: legacy }, write, { persistedCover: legacy });
  expect(write).toHaveBeenCalledWith({ apuBio: 'Nueva bio', apuHeroImageUrl: legacy });
  // The same embedded image is still refused as a newly supplied cover.
  await expect(saveArtistProfileMedia({ apuHeroImageUrl: legacy }, write)).rejects.toThrow('Sube una imagen');
  await expect(saveArtistProfileMedia({ apuHeroImageUrl: legacy }, write, { persistedCover: 'https://cdn.example/old.jpg' }))
    .rejects.toThrow('Sube una imagen');
  expect(write).toHaveBeenCalledTimes(1);
});

it('runs a dependent write only after the cover is accepted', async () => {
  const order: string[] = [];
  const write = jest.fn(async (payload: object) => { order.push('profile'); return payload; });
  const beforeWrite = jest.fn(async () => { order.push('name'); });
  await expect(saveArtistProfileMedia({ apuHeroImageUrl: 'https://open.spotify.com/artist/abc' }, write, { beforeWrite }))
    .rejects.toThrow('Sube una imagen');
  const failing = saveArtistProfileMedia({ apuHeroImageUrl: 'https://cdn.example/missing.jpg' }, write, { beforeWrite });
  fireEvent.error(image);
  await expect(failing).rejects.toThrow('No pudimos cargar');
  expect(beforeWrite).not.toHaveBeenCalled();
  expect(write).not.toHaveBeenCalled();
  const saving = saveArtistProfileMedia({ apuHeroImageUrl: 'https://cdn.example/cover.jpg' }, write, { beforeWrite });
  fireEvent.load(image);
  await saving;
  expect(order).toEqual(['name', 'profile']);
});
