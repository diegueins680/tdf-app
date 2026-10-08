import { useEffect, type RefObject } from 'react';

/**
 * Fixed bars docked to the bottom of the viewport stack instead of covering
 * each other or the page:
 *
 *   page sticky CTAs (course enrollment, mobile cart)  bottom: BOTTOM_DOCK_OFFSET
 *   radio mini bar                                     bottom: ABOVE_GLOBAL_PLAYER
 *   global music player                                bottom: 0
 *
 * Each docked bar publishes its measured height as a CSS variable while it is
 * visible, and the document body reserves the combined height so the last
 * fields and buttons of a page can always scroll clear of the bars.
 */
export const GLOBAL_PLAYER_HEIGHT_VAR = '--tdf-global-player-height';
export const RADIO_BAR_HEIGHT_VAR = '--tdf-radio-bar-height';
export const ABOVE_GLOBAL_PLAYER = `var(${GLOBAL_PLAYER_HEIGHT_VAR}, 0px)`;
export const BOTTOM_DOCK_OFFSET = `calc(var(${GLOBAL_PLAYER_HEIGHT_VAR}, 0px) + var(${RADIO_BAR_HEIGHT_VAR}, 0px))`;

const activeBars = new Set<string>();
let bodyPaddingBeforeDock: string | null = null;

function reserveBody(cssVar: string) {
  if (activeBars.size === 0) {
    bodyPaddingBeforeDock = document.body.style.paddingBottom;
    document.body.style.paddingBottom = BOTTOM_DOCK_OFFSET;
  }
  activeBars.add(cssVar);
}

function releaseBody(cssVar: string) {
  activeBars.delete(cssVar);
  if (activeBars.size === 0) {
    document.body.style.paddingBottom = bodyPaddingBeforeDock ?? '';
    bodyPaddingBeforeDock = null;
  }
}

/** Publish a visible docked bar's height under `cssVar` and reserve it on <body>. */
export function useDockedBarHeight(cssVar: string, ref: RefObject<HTMLElement | null>, active: boolean): void {
  useEffect(() => {
    if (!active || typeof document === 'undefined') return undefined;
    const node = ref.current;
    if (!node) return undefined;
    const root = document.documentElement;
    const publish = () => {
      root.style.setProperty(cssVar, `${Math.ceil(node.getBoundingClientRect().height)}px`);
    };
    publish();
    reserveBody(cssVar);
    const observer = typeof ResizeObserver === 'undefined' ? null : new ResizeObserver(publish);
    observer?.observe(node);
    window.addEventListener('resize', publish);
    return () => {
      observer?.disconnect();
      window.removeEventListener('resize', publish);
      root.style.removeProperty(cssVar);
      releaseBody(cssVar);
    };
  }, [active, cssVar, ref]);
}
