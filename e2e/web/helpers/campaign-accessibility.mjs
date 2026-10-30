// Observations for failed campaign audits, not a paint-readiness predicate.
// In particular, MUI's requestAnimationFrame scrolling is not a WAAPI animation.
export function inspectCampaignTabs() {
  const rect = (r) => ({ x: r.x, y: r.y, width: r.width, height: r.height });
  const tabs = Array.from(document.querySelectorAll('[role="tablist"] [role="tab"]')).map((tab) => {
    const bounds = tab.getBoundingClientRect();
    let left = Math.max(0, bounds.left);
    let top = Math.max(0, bounds.top);
    let right = Math.min(innerWidth, bounds.right);
    let bottom = Math.min(innerHeight, bounds.bottom);
    const ancestors = [];
    for (let element = tab; element; element = element.parentElement) {
      const style = getComputedStyle(element);
      const box = element.getBoundingClientRect();
      const clipsX = /^(hidden|clip|scroll|auto)$/.test(style.overflowX);
      const clipsY = /^(hidden|clip|scroll|auto)$/.test(style.overflowY);
      if (element !== tab) {
        if (clipsX) {
          left = Math.max(left, box.left + element.clientLeft);
          right = Math.min(right, box.left + element.clientLeft + element.clientWidth);
        }
        if (clipsY) {
          top = Math.max(top, box.top + element.clientTop);
          bottom = Math.min(bottom, box.top + element.clientTop + element.clientHeight);
        }
      }
      ancestors.push({
        tag: element.tagName, role: element.getAttribute('role'), className: element.getAttribute('class'),
        bounds: rect(box), color: style.color, backgroundColor: style.backgroundColor,
        opacity: style.opacity, visibility: style.visibility, display: style.display,
        overflowX: style.overflowX, overflowY: style.overflowY,
        scrollLeft: element.scrollLeft, scrollTop: element.scrollTop,
        animations: element.getAnimations().map((animation) => ({
          playState: animation.playState, currentTime: animation.currentTime,
        })),
      });
    }
    return {
      text: tab.textContent, selected: tab.getAttribute('aria-selected'), bounds: rect(bounds),
      clipIntersection: { x: left, y: top, width: Math.max(0, right - left), height: Math.max(0, bottom - top) },
      ancestors,
    };
  });
  return {
    sampledAt: performance.now(), viewport: { width: innerWidth, height: innerHeight, devicePixelRatio },
    scrollX, scrollY, fonts: document.fonts.status, tabs,
  };
}

export async function recordCampaignAccessibilityFailure(page, testInfo, tab, violations, before) {
  if (violations.length === 0) return;
  const after = await page.evaluate(inspectCampaignTabs);
  await testInfo.attach(`campaign-accessibility-${tab}`, {
    body: JSON.stringify({ tab, violations, before, after }, null, 2),
    contentType: 'application/json',
  });
}
