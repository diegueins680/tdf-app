import { useLayoutEffect, useRef } from 'react';
import { NavigationType, useLocation, useNavigationType } from 'react-router-dom';

interface Position { x: number; y: number }
const STORAGE_KEY = 'tdf.navigation.scroll.v1';
const MAX_ENTRIES = 100;

function readPositions(): Record<string, Position> {
  try {
    const parsed: unknown = JSON.parse(sessionStorage.getItem(STORAGE_KEY) ?? '{}');
    if (!parsed || typeof parsed !== 'object') return {};
    return Object.fromEntries(Object.entries(parsed).filter((entry): entry is [string, Position] => {
      const value = entry[1] as Position | null;
      return Boolean(value && Number.isFinite(value.x) && Number.isFinite(value.y) && value.y >= 0);
    }).slice(-MAX_ENTRIES));
  } catch { return {}; }
}

/** Public routes scroll the document; AppShell routes scroll their persistent main. */
function scrollContainer(): HTMLElement {
  const main = document.getElementById('main-content');
  return main?.hasAttribute('data-navigation-scroll-container')
    ? main : document.scrollingElement as HTMLElement ?? document.documentElement;
}

export default function NavigationScrollRestoration() {
  const location = useLocation();
  const navigationType = useNavigationType();
  const previous = useRef<typeof location | null>(null);
  const positions = useRef<Record<string, Position>>(readPositions());

  useLayoutEffect(() => {
    const old = previous.current;
    previous.current = location;
    const initial = old === null;
    const changedPage = old !== null && old.pathname !== location.pathname;
    const saved = positions.current[location.key];
    // Query/filter replacements are in-page actions; history entries retain their own positions.
    const restore = navigationType === NavigationType.Pop ? saved : undefined;
    const reset = !initial && changedPage && navigationType !== NavigationType.Pop;
    // Native same-document fragment navigation can retain the router history key.
    // A newly requested anchor takes precedence over that key's saved position.
    const changedAnchor = old !== null && !changedPage && old.hash !== location.hash;
    const anchorHash = changedAnchor || (!restore && (initial || changedPage)) ? location.hash : '';
    const target = restore ?? (reset ? { x: 0, y: 0 } : undefined);
    let settled = false;
    let interrupted = false;
    let frame = 0;
    let observed: HTMLElement | null = null;
    const persist = () => {
      // History updates synchronously; React may retain the outgoing screen
      // during a transition. Its later layout/scroll events belong to neither
      // the new entry nor a new position on the page the visitor just left.
      if (window.location.pathname !== location.pathname) return;
      if (!settled && (target || anchorHash)) return;
      const element = scrollContainer();
      positions.current[location.key] = { x: element.scrollLeft, y: element.scrollTop };
    };
    const flush = () => {
      positions.current = Object.fromEntries(Object.entries(positions.current).slice(-MAX_ENTRIES));
      try { sessionStorage.setItem(STORAGE_KEY, JSON.stringify(positions.current)); } catch { /* optional */ }
    };
    const attempt = () => {
      if (interrupted) return;
      const element = scrollContainer();
      if (observed !== element) {
        observed = element;
        resize.observe(element);
        for (const child of element.children) resize.observe(child);
      }
      if (document.querySelector('[data-route-loading], [data-scroll-restoration-pending]')) return;
      if (anchorHash) {
        let id: string;
        try { id = decodeURIComponent(anchorHash.slice(1)); } catch { settled = true; return; }
        const anchor = document.getElementById(id);
        if (!anchor) return;
        anchor.scrollIntoView({ block: 'start', behavior: 'instant' });
        settled = true;
      } else if (target) {
        element.scrollTo({ left: target.x, top: target.y, behavior: 'instant' });
        settled = Math.abs(element.scrollTop - target.y) <= 2;
      } else {
        settled = true; // Initial document refresh retains browser-native restoration.
      }
      if (settled) persist();
    };
    const schedule = () => { cancelAnimationFrame(frame); frame = requestAnimationFrame(attempt); };
    const onScroll = () => {
      // Browser anchoring/native history can run after our layout callback,
      // notably for nested containers in WebKit. Retain the intended target
      // until actual user input hands control back to the visitor.
      if (!interrupted && (target || anchorHash)) schedule();
      else persist();
    };
    const resize = new ResizeObserver(schedule);
    const mutation = new MutationObserver(schedule);
    mutation.observe(document.body, { childList: true, subtree: true });
    const interrupt = () => { interrupted = true; settled = true; resize.disconnect(); mutation.disconnect(); };
    document.addEventListener('wheel', interrupt, { passive: true });
    document.addEventListener('touchstart', interrupt, { passive: true });
    document.addEventListener('pointerdown', interrupt, { passive: true });
    document.addEventListener('click', interrupt, true);
    document.addEventListener('keydown', interrupt);
    document.addEventListener('scroll', onScroll, true);
    const save = () => { persist(); flush(); };
    document.addEventListener('click', persist, true);
    window.addEventListener('pagehide', save);
    // Own history restoration only after mount; keep the browser's initial refresh position.
    const nativeRestoration = history.scrollRestoration;
    history.scrollRestoration = 'manual';
    schedule();
    return () => {
      // The outgoing DOM may already be removed. Keep the last observed position.
      flush();
      cancelAnimationFrame(frame);
      resize.disconnect();
      mutation.disconnect();
      document.removeEventListener('wheel', interrupt);
      document.removeEventListener('touchstart', interrupt);
      document.removeEventListener('pointerdown', interrupt);
      document.removeEventListener('click', interrupt, true);
      document.removeEventListener('keydown', interrupt);
      document.removeEventListener('scroll', onScroll, true);
      document.removeEventListener('click', persist, true);
      window.removeEventListener('pagehide', save);
      history.scrollRestoration = nativeRestoration;
    };
  }, [location, navigationType]);
  return null;
}
