// Runs inside the browser via Locator.evaluate; intentionally has no closure imports.
export function inspectReviewPaint(element) {
  const ancestors = [];
  for (let node = element; node instanceof Element; node = node.parentElement) ancestors.push(node);
  const nodes = [...ancestors, ...element.querySelectorAll('button')];
  const styles = nodes.map(node => {
    const style = getComputedStyle(node);
    return { tag: node.tagName, role: node.getAttribute('role'), className: node.className,
      opacity: style.opacity, color: style.color, backgroundColor: style.backgroundColor,
      visibility: style.visibility, display: style.display,
      transition: style.transition, animation: style.animation };
  });
  const animations = [...new Set([
    ...element.getAnimations({ subtree: true }), ...ancestors.flatMap(node => node.getAnimations()),
  ])].map(animation => ({ playState: animation.playState, pending: animation.pending,
    endTime: animation.effect?.getComputedTiming().endTime ?? null }));
  // Paused intermediate motion is not ready either; never paper over a stuck fade.
  const ready = element.isConnected && element.getClientRects().length > 0
    && styles.every(style => style.opacity === '1' && style.visibility === 'visible' && style.display !== 'none')
    && animations.every(animation => !animation.pending && animation.playState !== 'running'
      && animation.playState !== 'paused');
  return { ready, styles, animations };
}
