import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

jest.unstable_mockModule('../../api/client', () => ({ API_BASE_URL: 'https://api.tdfrecords.net' }));

const { default: ExpandablePreviewImage } = await import('./ExpandablePreviewImage');

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

const poster = 'https://api.tdfrecords.net/assets/serve/social-events/events/220/poster.jpg';

describe('ExpandablePreviewImage', () => {
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

  const renderImage = async (imageUrl: string | null) => {
    await act(async () => {
      root.render(
        <ExpandablePreviewImage
          kind="event"
          imageUrl={imageUrl}
          alt="Afiche de Quito en lo Alto"
          fallbackAlt="Imagen de referencia de Quito en lo Alto"
          expandLabel="Ver el afiche completo"
          closeLabel="Cerrar"
        />,
      );
    });
  };

  const viewer = () => document.body.querySelector<HTMLElement>('[role="dialog"]');

  it('opens the whole image, uncropped, when the preview is activated', async () => {
    await renderImage(poster);
    const trigger = container.querySelector<HTMLButtonElement>('button[aria-label="Ver el afiche completo"]')!;
    expect(trigger).not.toBeNull();
    expect(viewer()).toBeNull();

    await act(async () => trigger.click());

    const full = viewer()!.querySelector('img')!;
    expect(full.getAttribute('src')).toBe(poster);
    expect(full.getAttribute('alt')).toBe('Afiche de Quito en lo Alto');
    expect(viewer()!.getAttribute('aria-label')).toBe('Afiche de Quito en lo Alto');
  });

  it('closes from the close control', async () => {
    await renderImage(poster);
    await act(async () => container.querySelector<HTMLButtonElement>('button')!.click());
    const close = viewer()!.querySelector<HTMLButtonElement>('button[aria-label="Cerrar"]')!;

    await act(async () => close.click());

    // jsdom never ends the exit transition, so the closed viewer is the faded one.
    expect(viewer()!.closest('.MuiDialog-container')!.getAttribute('style')).toContain('opacity: 0');
  });

  it('offers no viewer for the placeholder', async () => {
    await renderImage(null);
    expect(container.querySelector('button')).toBeNull();
    expect(container.querySelector('img')!.getAttribute('data-preview-source')).toBe('placeholder');
  });
});
