import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

jest.unstable_mockModule('../../api/client', () => ({ API_BASE_URL: 'https://api.tdfrecords.net' }));

const { default: DirectoryPreviewImage } = await import('./DirectoryPreviewImage');

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

describe('DirectoryPreviewImage', () => {
  let container: HTMLDivElement;
  let root: Root;

  beforeEach(() => {
    container = document.createElement('div');
    document.body.appendChild(container);
    root = createRoot(container);
  });

  afterEach(async () => {
    await act(async () => root.unmount());
    container.remove();
  });

  const renderImage = async (props: Partial<Parameters<typeof DirectoryPreviewImage>[0]> = {}) => {
    await act(async () => {
      root.render(
        <DirectoryPreviewImage
          kind="profile"
          imageUrl={null}
          alt="Foto de Perfil"
          fallbackAlt="Imagen de referencia de Perfil"
          {...props}
        />,
      );
    });
    return container.querySelector('img')!;
  };

  it.each([
    ['TDF Records', 'https://www.tdfrecords.net/tdf-app-icon-1024.png'],
    ['Domo del Pululahua', 'https://www.tdfrecords.net/assets/tdf-ui/domo-pululahua-hero-cozy.jpg'],
  ])('shows the canonical image for %s instead of the placeholder', async (title, imageUrl) => {
    const image = await renderImage({ kind: title === 'TDF Records' ? 'profile' : 'venue', imageUrl, alt: `Foto de ${title}` });
    expect(image.getAttribute('src')).toBe(imageUrl);
    expect(image.getAttribute('alt')).toBe(`Foto de ${title}`);
    expect(image.dataset['previewSource']).toBe('media');
  });

  it('resolves API-served media against the API origin', async () => {
    const image = await renderImage({ imageUrl: '/assets/serve/directory/profiles/bassist.webp' });
    expect(image.getAttribute('src')).toBe('https://api.tdfrecords.net/assets/serve/directory/profiles/bassist.webp');
  });

  it('uses the kind placeholder only when no valid media exists', async () => {
    const image = await renderImage({ kind: 'event', imageUrl: null });
    expect(image.getAttribute('src')).toBe(`${window.location.origin}/event-fallback.svg`);
    expect(image.getAttribute('alt')).toBe('Imagen de referencia de Perfil');
    expect(image.dataset['previewSource']).toBe('placeholder');
  });

  it('rejects unsafe media URLs', async () => {
    const image = await renderImage({ imageUrl: 'javascript:alert(1)' });
    expect(image.dataset['previewSource']).toBe('placeholder');
  });

  it('falls back to the placeholder when the image fails to load', async () => {
    const image = await renderImage({ imageUrl: 'https://cdn.example.test/missing.jpg' });
    await act(async () => { image.dispatchEvent(new Event('error')); });
    const replaced = container.querySelector('img')!;
    expect(replaced.getAttribute('src')).toBe(`${window.location.origin}/artist-fallback.svg`);
    expect(replaced.dataset['previewSource']).toBe('placeholder');
  });

  it('reserves layout space and loads lazily by default', async () => {
    const image = await renderImage({ imageUrl: 'https://cdn.example.test/a.jpg', width: 440, height: 440 });
    expect(image.getAttribute('loading')).toBe('lazy');
    expect(image.getAttribute('decoding')).toBe('async');
    expect(image.getAttribute('width')).toBe('440');
    expect(image.getAttribute('height')).toBe('440');
  });
});
