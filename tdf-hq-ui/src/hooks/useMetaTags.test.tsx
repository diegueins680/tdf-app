import { act } from 'react';
import { createRoot } from 'react-dom/client';

import { useMetaTags } from './useMetaTags';

function Harness() {
  useMetaTags({
    title: 'Synthetic directory profile',
    description: 'Public SEO fixture',
    canonical: 'https://tdf-app.pages.dev/directorio/synthetic-profile',
    ogImage: 'https://tdf-app.pages.dev/artist-fallback.svg',
    structuredData: { '@context': 'https://schema.org', '@type': 'Person', name: 'Synthetic directory profile' },
  });
  return null;
}

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

describe('useMetaTags directory SEO', () => {
  it('emits canonical, Open Graph, social preview, robots, and JSON-LD metadata', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    try {
      await act(async () => root.render(<Harness />));
      expect(document.querySelector<HTMLLinkElement>('link[rel="canonical"]')?.href).toBe('https://tdf-app.pages.dev/directorio/synthetic-profile');
      expect(document.querySelector<HTMLMetaElement>('meta[property="og:url"]')?.content).toBe('https://tdf-app.pages.dev/directorio/synthetic-profile');
      expect(document.querySelector<HTMLMetaElement>('meta[name="twitter:card"]')?.content).toBe('summary_large_image');
      expect(document.querySelector<HTMLMetaElement>('meta[name="robots"]')?.content).toBe('index,follow');
      expect(document.querySelector('script[type="application/ld+json"]')?.textContent).toContain('Synthetic directory profile');
    } finally {
      await act(async () => root.unmount());
      container.remove();
    }
  });
});
