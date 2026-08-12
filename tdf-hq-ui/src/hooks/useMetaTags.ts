import { useEffect } from 'react';

interface MetaTags {
  title: string;
  description?: string;
  ogImage?: string;
  ogType?: string;
  canonical?: string;
}

export function useMetaTags({ title, description, ogImage, ogType = 'website', canonical }: MetaTags) {
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
    if (canonical) {
      let link = document.querySelector('link[rel="canonical"]') as HTMLLinkElement | null;
      if (!link) {
        link = document.createElement('link');
        link.rel = 'canonical';
        document.head.appendChild(link);
      }
      link.href = canonical;
    }

    // Cleanup on unmount
    return () => {
      document.title = 'TDF Records';
    };
  }, [title, description, ogImage, ogType, canonical]);
}
