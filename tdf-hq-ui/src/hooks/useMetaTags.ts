import { useEffect } from 'react';

interface MetaTags {
  title: string;
  description?: string;
  ogImage?: string;
  ogType?: string;
  canonical?: string;
  structuredData?: Record<string, unknown>;
  robots?: string;
}

export function useMetaTags({ title, description, ogImage, ogType = 'website', canonical, structuredData, robots = 'index,follow' }: MetaTags) {
  useEffect(() => {
    // Set document title
    document.title = `${title} · TDF Records`;

    // Helper to set or create meta tag
    const setMeta = (name: string, content: string, isProperty = false) => {
      const attr = isProperty ? 'property' : 'name';
      let tag = document.querySelector(`meta[${attr}="${name}"]`);
      if (!tag) {
        tag = document.createElement('meta');
        tag.setAttribute(attr, name);
        document.head.appendChild(tag);
      }
      tag.setAttribute('content', content);
    };

    if (description) setMeta('description', description);
    setMeta('og:title', `${title} · TDF Records`, true);
    if (description) setMeta('og:description', description, true);
    setMeta('og:type', ogType, true);
    if (ogImage) setMeta('og:image', ogImage, true);
    setMeta('twitter:card', ogImage ? 'summary_large_image' : 'summary');
    setMeta('twitter:title', `${title} · TDF Records`);
    if (description) setMeta('twitter:description', description);
    if (ogImage) setMeta('twitter:image', ogImage);
    setMeta('robots', robots);
    if (canonical) {
      setMeta('og:url', canonical, true);
      let link = document.querySelector<HTMLLinkElement>('link[rel="canonical"]');
      if (!link) {
        link = document.createElement('link');
        link.rel = 'canonical';
        document.head.appendChild(link);
      }
      link.href = canonical;
    }
    if (structuredData) {
      let script = document.querySelector<HTMLScriptElement>('script[data-tdf-structured-data="true"]');
      if (!script) {
        script = document.createElement('script');
        script.type = 'application/ld+json';
        script.dataset['tdfStructuredData'] = 'true';
        document.head.appendChild(script);
      }
      script.textContent = JSON.stringify(structuredData);
    }

    // Cleanup on unmount
    return () => {
      document.title = 'TDF Records';
      document.querySelector('script[data-tdf-structured-data="true"]')?.remove();
    };
  }, [title, description, ogImage, ogType, canonical, structuredData, robots]);
}
